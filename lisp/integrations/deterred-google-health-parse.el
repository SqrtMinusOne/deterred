;;; deterred-google-health-parse.el --- Parse Google Health CSVs -*- lexical-binding: t -*-

;; Copyright (C) 2026 Korytov Pavel
;; Author: Korytov Pavel <thexcloud@gmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Read selected Takeout CSV files in Emacs, without accessing a database.
;; Output contains scalar database rows, stage-replacement authority, explicit
;; deletion IDs, and warnings.  Missing daily metric families remain absent.
;; CSV buffers are released after each file; irrelevant streams are not read.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defconst deterred-google-health-parse--session-columns
  '(id kind activity_type name start_timestamp end_timestamp start_utc_offset
    end_utc_offset local_date data_source recording_method algorithm_version
    source_updated_at minutes_asleep minutes_awake sleep_score
    restlessness_normalized hr_below_resting_hr steps distance_m energy_kcal
    altitude_m cardio_load heart_rate_avg_bpm heart_rate_max_bpm)
  "Columns supplied for every session snapshot.")

(defconst deterred-google-health-parse--sum-metrics
  '(("Steps" "steps" steps t)
    ("Active Energy Burned" "Kilocalories" active_energy_kcal nil)
    ("Distance" "distance" distance_m nil)
    ("Floors" "floors" floors t)
    ("Active Zone Minutes" "total minutes" active_zone_minutes nil))
  "Folder, source column, destination column and integer flag.")

(defconst deterred-google-health-parse--daily-metrics
  '(("Daily Resting Heart Rate"
     ("beats per minute" resting_heart_rate_bpm 0 nil))
    ("Daily Heart Rate Variability"
     ("average heart rate variability milliseconds" hrv_rmssd_ms 0 nil)
     ("deep sleep root mean square of successive differences milliseconds" hrv_deep_rmssd_ms 0 nil)
     ("non rem heart rate beats per minute" non_rem_heart_rate_bpm 0 nil))
    ("Daily Respiratory Rate" ("breaths per minute" respiratory_rate 0 nil))
    ("Daily Oxygen Saturation"
     ("average percentage" spo2_percent 0 100)
     ("lower bound percentage" spo2_lower_percent 0 100)
     ("upper bound percentage" spo2_upper_percent 0 100))
    ("Daily Readiness"
     ("score" readiness_score 0 100)
     ("sleep readiness" readiness_sleep nil nil)
     ("heart rate variability readiness" readiness_hrv nil nil)
     ("resting heart rate readiness" readiness_resting_heart_rate nil nil))
    ("Daily Sleep Breathing Quality"
     ("breathing quality sleep score" sleep_breathing_quality nil nil)))
  "Daily folder fields: source, destination, minimum and maximum.")

(cl-defstruct (deterred-google-health-parse--state
               (:constructor deterred-google-health-parse--state-create))
  health physical zone
  (sessions (make-hash-table :test #'equal))
  (stages (make-hash-table :test #'equal))
  (daily (make-hash-table :test #'equal))
  (versions (make-hash-table :test #'eq))
  (warning-set (make-hash-table :test #'equal))
  warnings seen-files)

(defun deterred-google-health-parse--text (value)
  "Trim VALUE, returning nil for an exported missing value."
  (when value
    (let ((trimmed (string-trim value)))
      (unless (member (downcase trimmed) '("" "null" "nan" "none"))
        trimmed))))

(defun deterred-google-health-parse--warn (state message)
  "Add MESSAGE once to STATE's warnings, preserving encounter order."
  (unless (gethash message (deterred-google-health-parse--state-warning-set state))
    (puthash message t (deterred-google-health-parse--state-warning-set state))
    (push message (deterred-google-health-parse--state-warnings state))))

(defun deterred-google-health-parse--files (directory &optional prefix)
  "Return sorted CSV files from DIRECTORY, optionally matching PREFIX."
  (when (file-directory-p directory)
    (directory-files directory t
                     (concat "\\`" (if prefix (regexp-quote prefix) ".*")
                             (when prefix ".*") "\\.csv\\'"))))

(defun deterred-google-health-parse--physical-files (state folder)
  "Return selected CSV files in physical activity FOLDER of STATE."
  (deterred-google-health-parse--files
   (expand-file-name folder (deterred-google-health-parse--state-physical state))))

(defun deterred-google-health-parse--csv-row ()
  "Read one CSV record at point, preserving quoted embedded line endings.
Handle doubled quotes, empty final fields, CRLF and LF record separators."
  (unless (eobp)
    (let (fields done)
      (while (not done)
        (push
         (if (eq (char-after) ?\")
             (progn
               (forward-char)
               (let ((start (point)) pieces closed)
                 (while (not closed)
                   (unless (search-forward "\"" nil t)
                     (error "Unterminated quoted CSV field"))
                   (push (buffer-substring-no-properties start (1- (point))) pieces)
                   (if (eq (char-after) ?\")
                       (progn (push "\"" pieces) (forward-char) (setq start (point)))
                     (setq closed t)))
                 (apply #'concat (nreverse pieces))))
           (let ((start (point)))
             (skip-chars-forward "^,\r\n")
             (buffer-substring-no-properties start (point))))
         fields)
        (cond
         ((eobp) (setq done t))
         ((eq (char-after) ?,) (forward-char))
         ((eq (char-after) ?\n) (forward-char) (setq done t))
         ((eq (char-after) ?\r)
          (forward-char)
          (when (eq (char-after) ?\n) (forward-char))
          (setq done t))
         (t (error "Unexpected character after quoted CSV field"))))
      (nreverse fields))))

(defun deterred-google-health-parse--rows (state files required function)
  "Call FUNCTION with each CSV row and its context in FILES.
Rows are string-keyed alists.  Validate REQUIRED column names and row widths.
STATE tracks whether any supported files were seen."
  (dolist (file (sort (copy-sequence files) #'string<))
    (setf (deterred-google-health-parse--state-seen-files state) t)
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8-unix))
        (insert-file-contents file))
      (when (memq 'eight-bit (find-charset-region (point-min) (point-max)))
        (error "Cannot read %s: invalid UTF-8" file))
      (goto-char (point-min))
      (when (eq (char-after) #xfeff) (forward-char))
      (let* ((header-start (point))
             (headers (deterred-google-health-parse--csv-row))
             (line (count-lines header-start (point)))
             (missing (cl-set-difference required headers :test #'equal)))
        (unless (= (length headers) (length (delete-dups (copy-sequence headers))))
          (error "%s: duplicate CSV columns" (file-name-nondirectory file)))
        (when missing
          (error "%s: missing columns: %s" (file-name-nondirectory file)
                 (string-join (sort missing #'string<) ", ")))
        (while (not (eobp))
          ;; DictReader-style blank lines do not constitute observations.
          (if (memq (char-after) '(?\n ?\r))
              (progn
                (when (eq (char-after) ?\r) (forward-char))
                (when (eq (char-after) ?\n) (forward-char))
                (cl-incf line))
            (let* ((start (point))
                   (values (condition-case error-data
                               (deterred-google-health-parse--csv-row)
                             (error
                              (error "%s:%d: malformed CSV: %s"
                                     (file-name-nondirectory file) (1+ line)
                                     (error-message-string error-data)))))
                   (context (format "%s:%d" (file-name-nondirectory file)
                                    (cl-incf line (count-lines start (point))))))
              (unless (= (length values) (length headers))
                (error "%s: malformed CSV row" context))
              (funcall function (cl-mapcar #'cons headers values) context))))))))

(defun deterred-google-health-parse--zone-valid-p (zone)
  "Return non-nil if ZONE names an installed IANA timezone.
Emacs otherwise silently accepts unknown zone names as UTC."
  (and (stringp zone)
       (not (string-empty-p zone))
       (not (file-name-absolute-p zone))
       (not (member ".." (split-string zone "/")))
       (cl-some (lambda (directory)
                  (when directory
                    (let ((file (expand-file-name zone directory)))
                      (and (file-regular-p file)
                           (condition-case nil
                               (with-temp-buffer
                                 (insert-file-contents-literally file nil 0 4)
                                 (equal (buffer-string) "TZif"))
                             (file-error nil))))))
                (list (getenv "TZDIR") "/usr/share/zoneinfo"
                      "/usr/share/lib/zoneinfo" "/usr/lib/zoneinfo"
                      "/etc/zoneinfo"))))

(defun deterred-google-health-parse--date-parts (value context)
  "Validate ISO calendar date VALUE, returning (YEAR MONTH DAY).
Use CONTEXT to report malformed or normalized calendar dates."
  (unless (and (stringp value)
               (string-match "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)\\'" value))
    (error "%s: invalid summary date" context))
  (let* ((year (string-to-number (match-string 1 value)))
         (month (string-to-number (match-string 2 value)))
         (day (string-to-number (match-string 3 value)))
         (time (encode-time 0 0 0 day month year t)))
    (unless (and (> year 0) (equal value (format-time-string "%Y-%m-%d" time t)))
      (error "%s: invalid summary date" context))
    (list year month day)))

(defun deterred-google-health-parse--instant (value context)
  "Parse explicitly zoned ISO timestamp VALUE into Unix seconds.
Fractions are preserved for sample identity; callers truncate session seconds.
Report malformed timestamps with CONTEXT, without using the machine zone."
  (unless (and
           (stringp value)
           (string-match
            (concat "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)"
                    "[Tt ]\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)"
                    "\\(?:[.,]\\([0-9]+\\)\\)?"
                    "\\(Z\\|[+-][0-9]\\{2\\}:?[0-9]\\{2\\}\\)\\'") value))
    (error "%s: invalid timestamp %S" context value))
  (let* ((year (string-to-number (match-string 1 value)))
         (month (string-to-number (match-string 2 value)))
         (day (string-to-number (match-string 3 value)))
         (hour (string-to-number (match-string 4 value)))
         (minute (string-to-number (match-string 5 value)))
         (second (string-to-number (match-string 6 value)))
         (fraction (match-string 7 value))
         (zone (match-string 8 value))
         (zone-digits (replace-regexp-in-string ":" "" zone))
         (zone-hour (if (equal zone "Z") 0 (string-to-number (substring zone-digits 1 3))))
         (zone-minute (if (equal zone "Z") 0 (string-to-number (substring zone-digits 3 5))))
         (offset (* (if (string-prefix-p "-" zone) -1 1)
                    (+ (* zone-hour 3600) (* zone-minute 60))))
         (time (encode-time second minute hour day month year t))
         (decoded (decode-time time t)))
    (unless (and (> year 0) (< hour 24) (< minute 60) (< second 60)
                 (< zone-hour 24) (< zone-minute 60)
                 (= day (decoded-time-day decoded))
                 (= month (decoded-time-month decoded))
                 (= year (decoded-time-year decoded)))
      (error "%s: invalid timestamp %S" context value))
    (+ (- (float-time time) offset)
       (if fraction (string-to-number (concat "0." fraction)) 0))))

(cl-defun deterred-google-health-parse--number
    (state row key context &key integer positive maximum (minimum 0))
  "Read numeric KEY from ROW, logging unusable values in STATE with CONTEXT.
INTEGER, POSITIVE, MINIMUM and MAXIMUM constrain accepted values."
  (let ((raw (deterred-google-health-parse--text (alist-get key row nil nil #'equal))))
    (when raw
      (let* ((numeric (string-match-p
                       "\\`[+-]?\\(?:[0-9]+\\(?:\\.[0-9]*\\)?\\|\\.[0-9]+\\)\\(?:[eE][+-]?[0-9]+\\)?\\'" raw))
             (value (and numeric (string-to-number raw)))
             (valid (and value (or (integerp value) (< (abs value) 1.0e+INF))
                         (or (null minimum) (>= value minimum))
                         (or (null maximum) (<= value maximum))
                         (or (not positive) (> value 0))
                         (or (not integer) (= value (truncate value))))))
        (if valid
            (if integer (truncate value) (float value))
          (deterred-google-health-parse--warn
           state (format "%s: unusable %s; stored as NULL" context key))
          nil)))))

(defun deterred-google-health-parse--first-number (state row keys context &rest options)
  "Read the first usable numeric value among KEYS in ROW.
Pass STATE, CONTEXT and OPTIONS to the scalar reader."
  (catch 'value
    (dolist (key keys)
      (let ((value (apply #'deterred-google-health-parse--number
                          state row key context options)))
        (when value (throw 'value value))))))

(defun deterred-google-health-parse--updated (state value context)
  "Read optional revision timestamp VALUE, logging invalid data in STATE."
  (when (deterred-google-health-parse--text value)
    (condition-case nil
        (truncate (deterred-google-health-parse--instant value context))
      (error
       (deterred-google-health-parse--warn
        state (format "%s: unusable update timestamp; stored as NULL" context))
       nil))))

(defun deterred-google-health-parse--offset (state value timestamp context)
  "Read recorded UTC offset VALUE, or derive it for TIMESTAMP in STATE's zone."
  (if (null (deterred-google-health-parse--text value))
      (car (current-time-zone timestamp (deterred-google-health-parse--state-zone state)))
    (unless (string-match
             "\\`\\([+-]\\)\\([0-9]\\{1,2\\}\\):\\([0-9]\\{1,2\\}\\)\\(?::\\([0-9]\\{1,2\\}\\)\\)?\\'" value)
      (error "%s: invalid UTC offset %S" context value))
    (let ((sign (if (equal (match-string 1 value) "+") 1 -1))
          (hour (string-to-number (match-string 2 value)))
          (minute (string-to-number (match-string 3 value)))
          (second (string-to-number (or (match-string 4 value) "0"))))
      (unless (and (< hour 24) (< minute 60) (< second 60))
        (error "%s: invalid UTC offset %S" context value))
      (* sign (+ (* hour 3600) (* minute 60) second)))))

(defun deterred-google-health-parse--interval (state row start end context &optional start-offset end-offset)
  "Parse ROW's START/END timestamps and optional recorded offset columns.
STATE provides the fallback timezone and CONTEXT identifies malformed data."
  (let ((begin (truncate (deterred-google-health-parse--instant
                          (alist-get start row nil nil #'equal) context)))
        (finish (truncate (deterred-google-health-parse--instant
                           (alist-get end row nil nil #'equal) context))))
    (unless (> finish begin) (error "%s: interval end must be after start" context))
    (dolist (field (list start-offset end-offset))
      (when (and field (null (deterred-google-health-parse--text
                             (alist-get field row nil nil #'equal))))
        (error "%s: missing recorded UTC offset %s" context field)))
    `((start_timestamp . ,begin) (end_timestamp . ,finish)
      (start_utc_offset . ,(deterred-google-health-parse--offset
                           state (alist-get start-offset row nil nil #'equal) begin context))
      (end_utc_offset . ,(deterred-google-health-parse--offset
                         state (alist-get end-offset row nil nil #'equal) finish context)))))

(cl-defun deterred-google-health-parse--session
    (state kind row context start end &key id-key start-offset end-offset (source-key "data source"))
  "Construct a full scalar session snapshot for KIND from ROW in STATE.
START, END, ID-KEY, START-OFFSET, END-OFFSET and SOURCE-KEY name CSV fields.
CONTEXT identifies invalid records."
  (let* ((record (mapcar (lambda (column) (cons column nil))
                         deterred-google-health-parse--session-columns))
         (source (deterred-google-health-parse--text (alist-get source-key row nil nil #'equal)))
         (identifier (deterred-google-health-parse--text (alist-get id-key row nil nil #'equal))))
    (when (and id-key (null identifier)) (error "%s: missing %s" context id-key))
    (dolist (pair (deterred-google-health-parse--interval
                   state row start end context start-offset end-offset))
      (setf (alist-get (car pair) record) (cdr pair)))
    (unless identifier
      (setq identifier
            (secure-hash 'sha256
                         (encode-coding-string
                          (string-join (list (or source "") kind
                                             (alist-get start row nil nil #'equal)
                                             (alist-get end row nil nil #'equal)) "\0")
                          'utf-8-unix))))
    (setf (alist-get 'id record) (concat kind ":" identifier)
          (alist-get 'kind record) kind
          (alist-get 'data_source record) source
          (alist-get 'local_date record)
          (format-time-string "%Y-%m-%d"
                              (alist-get (if (equal kind "sleep") 'end_timestamp 'start_timestamp) record)
                              (alist-get (if (equal kind "sleep") 'end_utc_offset 'start_utc_offset) record)))
    record))

(defun deterred-google-health-parse--versions (state collection)
  "Get revision metadata for COLLECTION from STATE."
  (or (gethash collection (deterred-google-health-parse--state-versions state))
      (puthash collection (make-hash-table :test #'equal)
               (deterred-google-health-parse--state-versions state))))

(defun deterred-google-health-parse--remember (state collection record version context)
  "Keep the latest RECORD by VERSION in COLLECTION, using STATE's metadata.
Conflicting records with equal revisions fail with CONTEXT."
  (let* ((id (alist-get 'id record))
         (old (gethash id collection))
         (versions (deterred-google-health-parse--versions state collection))
         (previous (gethash id versions))
         (equal-version (equal version previous)))
    (when (and old equal-version (not (equal old record)))
      (error "%s: conflicting records for %s" context id))
    (when (or (not old) (and version (or (null previous) (> version previous))))
      (puthash id record collection)
      (puthash id version versions))))

(defun deterred-google-health-parse--sleep (state)
  "Load sleep, score and stage CSVs into STATE; return authoritative sleep IDs."
  (let* ((health (deterred-google-health-parse--state-health state))
         (sessions (deterred-google-health-parse--state-sessions state))
         (stages (deterred-google-health-parse--state-stages state))
         (stage-files (deterred-google-health-parse--files health "UserSleepStages_"))
         (partitions (mapcar (lambda (file)
                               (substring (file-name-nondirectory file) (length "UserSleepStages_")))
                             stage-files))
         (authoritative (make-hash-table :test #'equal))
         (scores (make-hash-table :test #'equal)))
    (dolist (file (sort (deterred-google-health-parse--files health "UserSleeps_") #'string<))
      (deterred-google-health-parse--rows
       state (list file) '("sleep_id" "sleep_start" "sleep_end" "start_utc_offset" "end_utc_offset")
       (lambda (row context)
         (let ((record (deterred-google-health-parse--session
                        state "sleep" row context "sleep_start" "sleep_end"
                        :id-key "sleep_id" :start-offset "start_utc_offset"
                        :end-offset "end_utc_offset" :source-key "data_source")))
           (setf (alist-get 'name record) "Sleep"
                 (alist-get 'algorithm_version record)
                 (deterred-google-health-parse--text (alist-get "algorithm_version" row nil nil #'equal))
                 (alist-get 'source_updated_at record)
                 (deterred-google-health-parse--updated
                  state (alist-get "sleep_last_updated" row nil nil #'equal) context))
           (dolist (column '(minutes_asleep minutes_awake))
             (setf (alist-get column record)
                   (deterred-google-health-parse--number state row (symbol-name column) context)))
           (deterred-google-health-parse--remember
            state sessions record (alist-get 'source_updated_at record) context)
           (when (member (substring (file-name-nondirectory file) (length "UserSleeps_")) partitions)
             (puthash (alist-get 'id record) t authoritative))))))
    (deterred-google-health-parse--rows
     state (deterred-google-health-parse--files health "UserSleepScores_") '("sleep_id")
     (lambda (row context)
       (let ((record `((id . ,(concat "sleep:" (alist-get "sleep_id" row nil nil #'equal)))
                       (sleep_score . ,(deterred-google-health-parse--number state row "overall_score" context :maximum 100))
                       (restlessness_normalized . ,(deterred-google-health-parse--number state row "restlessness_normalized" context))
                       (hr_below_resting_hr . ,(deterred-google-health-parse--number state row "hr_below_resting_hr" context :maximum 1)))))
         (deterred-google-health-parse--remember
          state scores record (deterred-google-health-parse--updated
                               state (alist-get "sleep_score_last_updated" row nil nil #'equal) context) context))))
    (maphash (lambda (id score)
               (let ((session (gethash id sessions)))
                 (if session
                     (dolist (pair score) (setf (alist-get (car pair) session) (cdr pair)))
                   (deterred-google-health-parse--warn state (format "Sleep score without an imported sleep: %s; omitted" id)))))
             scores)
    (deterred-google-health-parse--rows
     state stage-files '("sleep_id" "sleep_stage_id" "sleep_stage_type" "sleep_stage_start"
                         "sleep_stage_end" "start_utc_offset" "end_utc_offset")
     (lambda (row context)
       (let* ((session-id (concat "sleep:" (alist-get "sleep_id" row nil nil #'equal)))
              (stage (deterred-google-health-parse--text (alist-get "sleep_stage_type" row nil nil #'equal)))
              (id (deterred-google-health-parse--text (alist-get "sleep_stage_id" row nil nil #'equal))))
         (unless (gethash session-id sessions)
           (error "%s: stage references missing sleep %s" context session-id))
         (unless (and stage id) (error "%s: missing sleep stage identity or type" context))
         (puthash session-id t authoritative)
         (let ((record (append
                        (deterred-google-health-parse--interval
                         state row "sleep_stage_start" "sleep_stage_end" context "start_utc_offset" "end_utc_offset")
                        `((id . ,(concat "sleep_stage:" id)) (session_id . ,session-id) (stage . ,stage)
                          (algorithm_version . ,(deterred-google-health-parse--text
                                                 (alist-get "algorithm_version" row nil nil #'equal)))))))
           (deterred-google-health-parse--remember
            state stages record (deterred-google-health-parse--updated
                                 state (alist-get "sleep_stage_last_updated" row nil nil #'equal) context) context)))))
    (sort (hash-table-keys authoritative) #'string<)))

(defun deterred-google-health-parse--exercises (state)
  "Load exercises into STATE and return IDs with authoritative deletions."
  (let ((sessions (deterred-google-health-parse--state-sessions state))
        (deleted (make-hash-table :test #'equal))
        (aliases '(("outdoor walk" . "walk") ("walking" . "walk") ("walk" . "walk")
                   ("bike" . "bike") ("cycling" . "bike") ("outdoor bike" . "bike")
                   ("run" . "run") ("running" . "run") ("outdoor run" . "run")))
        deleted-ids)
    (deterred-google-health-parse--rows
     state (deterred-google-health-parse--files
            (deterred-google-health-parse--state-health state) "UserExercises_")
     '("exercise_id" "exercise_start" "exercise_end" "utc_offset")
     (lambda (row context)
       (let ((version (deterred-google-health-parse--updated
                       state (alist-get "exercise_last_updated" row nil nil #'equal) context)))
         (if (not (member (deterred-google-health-parse--text
                           (alist-get "deletion_reason" row nil nil #'equal)) '(nil "UNSPECIFIED")))
             (let ((id (deterred-google-health-parse--text (alist-get "exercise_id" row nil nil #'equal))))
               (unless id (error "%s: missing exercise_id" context))
               (deterred-google-health-parse--remember state deleted `((id . ,(concat "exercise:" id))) version context))
           (let* ((record (deterred-google-health-parse--session
                           state "exercise" row context "exercise_start" "exercise_end"
                           :id-key "exercise_id" :start-offset "utc_offset" :end-offset "utc_offset"))
                  (name (or (deterred-google-health-parse--text (alist-get "activity_name" row nil nil #'equal)) "Activity"))
                  (normalized (string-trim (replace-regexp-in-string "[^a-z0-9]+" "_" (downcase name)) "_+" "_+")))
             (setf (alist-get 'name record) name
                   (alist-get 'activity_type record) (or (alist-get (downcase name) aliases nil nil #'equal)
                                                        (unless (string-empty-p normalized) normalized) "activity")
                   (alist-get 'recording_method record) (deterred-google-health-parse--text
                                                        (alist-get "log_type" row nil nil #'equal))
                   (alist-get 'source_updated_at record) version)
             (dolist (spec '((steps ("summary_steps" "manually_logged_total_steps" "tracker_total_steps") t 1 nil)
                             (distance_m ("summary_distance_mm" "manually_logged_total_distance_mm" "tracker_total_distance_mm") nil 0.001 nil)
                             (energy_kcal ("summary_calories_kcal" "manually_logged_total_calories" "tracker_total_calories") nil 1 nil)
                             (altitude_m ("tracker_total_altitude_mm") nil 0.001 nil)
                             (cardio_load ("summary_cardio_load" "tracker_cardio_load") nil 1 nil)
                             (heart_rate_avg_bpm ("summary_avg_heart_rate" "tracker_avg_heart_rate") nil 1 t)
                             (heart_rate_max_bpm ("tracker_peak_heart_rate") nil 1 t)))
               (let ((value (deterred-google-health-parse--first-number
                             state row (nth 1 spec) context :integer (nth 2 spec) :positive (nth 4 spec))))
                 (setf (alist-get (car spec) record) (and value (* value (nth 3 spec))))))
             (deterred-google-health-parse--remember state sessions record version context))))))
    (dolist (id (sort (hash-table-keys deleted) #'string<))
      (let ((deletion-version (gethash id (deterred-google-health-parse--versions state deleted)))
            (active-version (gethash id (deterred-google-health-parse--versions state sessions))))
        (when (and (gethash id sessions) (or (null deletion-version) (null active-version)))
          (error "Ambiguous deleted and active revisions for %s: missing update time" id))
        (unless (and (gethash id sessions) (< deletion-version active-version))
          (remhash id sessions)
          (push id deleted-ids)
          (deterred-google-health-parse--warn state (format "Explicitly deleted exercise: %s" id)))))
    (nreverse deleted-ids)))

(defun deterred-google-health-parse--other-sessions (state)
  "Load sedentary and mindfulness intervals into STATE."
  (dolist (spec '(("Sedentary Period" . "sedentary") ("Mindfulness Session" . "meditation")))
    (deterred-google-health-parse--rows
     state (deterred-google-health-parse--physical-files state (car spec)) '("start time" "end time")
     (lambda (row context)
       (let ((record (deterred-google-health-parse--session
                      state (cdr spec) row context "start time" "end time")))
         (if (equal (cdr spec) "meditation")
             (let ((external-id (deterred-google-health-parse--text (alist-get "metadata_external_id" row nil nil #'equal))))
               (when external-id
                 (setf (alist-get 'id record) (concat "meditation:" (or (alist-get 'data_source record) "") ":" external-id)))
               (setf (alist-get 'name record) (or (deterred-google-health-parse--text
                                                 (alist-get "metadata_session_name" row nil nil #'equal)) "Meditation")
                     (alist-get 'activity_type record) (deterred-google-health-parse--text
                                                        (alist-get "metadata_type" row nil nil #'equal))
                     (alist-get 'source_updated_at record) (deterred-google-health-parse--updated
                                                            state (alist-get "metadata_update_time" row nil nil #'equal) context)
                     (alist-get 'heart_rate_avg_bpm record) (deterred-google-health-parse--number
                                                            state row "heart_rate_summary_average_bpm" context :positive t)))
           (setf (alist-get 'name record) "Sedentary"))
         (deterred-google-health-parse--remember
          state (deterred-google-health-parse--state-sessions state) record (alist-get 'source_updated_at record) context))))))

(defun deterred-google-health-parse--day (state date)
  "Get DATE's sparse daily record in STATE."
  (or (gethash date (deterred-google-health-parse--state-daily state))
      (puthash date `((date . ,date) (timezone . ,(deterred-google-health-parse--state-zone state)))
               (deterred-google-health-parse--state-daily state))))

(defun deterred-google-health-parse--samples (state folder columns &optional integer)
  "Read incremental COLUMNS in FOLDER into a timestamp hash table.
STATE receives warnings.  INTEGER constrains all supplied columns."
  (let ((samples (make-hash-table :test #'equal))
        (conflicts (make-hash-table :test #'equal)))
    (deterred-google-health-parse--rows
     state (deterred-google-health-parse--physical-files state folder) (cons "timestamp" columns)
     (lambda (row context)
       (let* ((timestamp (deterred-google-health-parse--instant (alist-get "timestamp" row nil nil #'equal) context))
              (values (mapcar (lambda (column) (deterred-google-health-parse--number
                                              state row column context :integer integer)) columns))
              (value (unless (memq nil values) (apply #'+ values)))
              (old (gethash timestamp samples 'absent)))
         (when (and (not (eq old 'absent)) (not (equal old value)))
           (puthash timestamp t conflicts)
           (deterred-google-health-parse--warn
            state (format "%s: conflicting observations at %s; stored as NULL"
                          folder (alist-get "timestamp" row nil nil #'equal))))
         (puthash timestamp (unless (gethash timestamp conflicts) value) samples))))
    samples))

(defun deterred-google-health-parse--aggregate (state samples column &optional coverage)
  "Aggregate SAMPLES into daily COLUMN in STATE.
With COVERAGE, count distinct valid minutes and warn for partial energy days."
  (let ((days (make-hash-table :test #'equal))
        (zone (deterred-google-health-parse--state-zone state)))
    (maphash
     (lambda (timestamp value)
       (let* ((date (format-time-string "%Y-%m-%d" timestamp zone))
              (entry (or (gethash date days)
                         (puthash date (vector 0 nil (and coverage (make-hash-table :test #'eql))) days))))
         (if value
             (progn (cl-incf (aref entry 0) value)
                    (when coverage (puthash (floor timestamp 60) t (aref entry 2))))
           (aset entry 1 t)))) samples)
    (maphash
     (lambda (date entry)
       (let ((record (deterred-google-health-parse--day state date)))
         (setf (alist-get column record) (unless (aref entry 1) (aref entry 0)))
         (when coverage
           (let* ((count (hash-table-count (aref entry 2)))
                  (parts (deterred-google-health-parse--date-parts date date))
                  (start (encode-time 0 0 0 (nth 2 parts) (nth 1 parts) (car parts) zone))
                  (end (encode-time 0 0 0 (1+ (nth 2 parts)) (nth 1 parts) (car parts) zone))
                  (expected (round (/ (float-time (time-subtract end start)) 60))))
             (setf (alist-get 'energy_sample_minutes record) count)
             (unless (= count expected)
               (deterred-google-health-parse--warn
                state (format "%s: partial total-energy coverage (%d/%d minutes); daily totals are replacement snapshots"
                              date count expected)))))
         ;; `alist-get' can add a new head, so save the sparse row again.
         (puthash date record (deterred-google-health-parse--state-daily state)))) days)))

(defun deterred-google-health-parse--daily-data (state)
  "Read per-day sensor summaries and aggregate sample streams into STATE."
  (dolist (spec deterred-google-health-parse--sum-metrics)
    (deterred-google-health-parse--aggregate
     state (deterred-google-health-parse--samples state (nth 0 spec) (list (nth 1 spec)) (nth 3 spec)) (nth 2 spec)))
  (deterred-google-health-parse--aggregate
   state (deterred-google-health-parse--samples state "Active Minutes" '("light" "moderate" "very")) 'active_minutes)
  (let ((energy (deterred-google-health-parse--samples state "Calories" '("calories")))
        (precise (deterred-google-health-parse--samples state "Calories In Heart Rate Zone" '("kcal"))))
    (maphash (lambda (timestamp value)
               (when (or value (eq (gethash timestamp energy 'absent) 'absent))
                 (puthash timestamp value energy))) precise)
    (deterred-google-health-parse--aggregate state energy 'energy_kcal t))
  (let ((conflicts (make-hash-table :test #'equal)))
    (dolist (spec deterred-google-health-parse--daily-metrics)
      (let ((folder (car spec)) (fields (cdr spec)))
        (deterred-google-health-parse--rows
         state (deterred-google-health-parse--physical-files state folder)
         (cons "timestamp" (mapcar #'car fields))
         (lambda (row context)
           (let* ((timestamp (alist-get "timestamp" row nil nil #'equal))
                  (date (substring timestamp 0 (min 10 (length timestamp))))
                  (_ (deterred-google-health-parse--date-parts date context))
                  (record (deterred-google-health-parse--day state date)))
             ;; An exported daily summary's UTC midnight is its date label.
             (when (> (length timestamp) 10)
               (deterred-google-health-parse--instant timestamp context))
             (dolist (field fields)
               (let* ((source (nth 0 field)) (column (nth 1 field))
                      (value (if (null (nth 2 field))
                                 (deterred-google-health-parse--text (alist-get source row nil nil #'equal))
                               (deterred-google-health-parse--number state row source context :positive t :maximum (nth 3 field))))
                      (key (cons date column)))
                 (when (member value '("TYPE_UNSPECIFIED" "UNSPECIFIED" "NOT_AVAILABLE")) (setq value nil))
                 (when (and (assq column record) (not (equal (alist-get column record) value)))
                   (puthash key t conflicts)
                   (deterred-google-health-parse--warn state (format "%s: conflicting %s; stored as NULL" date column)))
                 (setf (alist-get column record) (unless (gethash key conflicts) value))))
             (puthash date record (deterred-google-health-parse--state-daily state)))))))))

(defun deterred-google-health-parse--record-less-p (left right columns)
  "Compare LEFT and RIGHT records lexicographically using COLUMNS."
  (catch 'result
    (dolist (column columns)
      (let ((a (alist-get column left)) (b (alist-get column right)))
        (unless (equal a b)
          (throw 'result (if (numberp a) (< a b) (string< a b))))))
    nil))

;;;###autoload
(defun deterred-google-health-parse-directory (directory &optional timezone)
  "Read selected Google Health Takeout CSV files in DIRECTORY.
TIMEZONE is an installed IANA name, defaulting to Europe/Moscow.  Return an
alist with sessions, sleep_stages, sparse daily rows, stage_session_ids,
deleted_session_ids and warnings.  Keys are symbols; SQL NULL values are nil.
No database, external process or network connection is used."
  (setq directory (expand-file-name directory)
        timezone (or timezone "Europe/Moscow"))
  (unless (file-directory-p directory) (error "Export directory does not exist: %s" directory))
  (unless (deterred-google-health-parse--zone-valid-p timezone)
    (error "Unknown reporting timezone: %s" timezone))
  (let* ((state (deterred-google-health-parse--state-create
                 :health (expand-file-name "Health Fitness Data_GoogleData" directory)
                 :physical (expand-file-name "Physical Activity_GoogleData" directory)
                 :zone timezone))
         (stage-ids (deterred-google-health-parse--sleep state))
         (deleted-ids (deterred-google-health-parse--exercises state)))
    (deterred-google-health-parse--other-sessions state)
    (deterred-google-health-parse--daily-data state)
    (unless (deterred-google-health-parse--state-seen-files state)
      (error "No supported Google Health CSV files found; select the Google Health directory"))
    `((sessions . ,(sort (hash-table-values (deterred-google-health-parse--state-sessions state))
                        (lambda (a b) (deterred-google-health-parse--record-less-p a b '(start_timestamp id)))))
      (sleep_stages . ,(sort (hash-table-values (deterred-google-health-parse--state-stages state))
                            (lambda (a b) (deterred-google-health-parse--record-less-p a b '(session_id start_timestamp id)))))
      (daily . ,(mapcar (lambda (date) (gethash date (deterred-google-health-parse--state-daily state)))
                       (sort (hash-table-keys (deterred-google-health-parse--state-daily state)) #'string<)))
      (stage_session_ids . ,stage-ids) (deleted_session_ids . ,deleted-ids)
      (warnings . ,(nreverse (deterred-google-health-parse--state-warnings state))))))

(provide 'deterred-google-health-parse)
;;; deterred-google-health-parse.el ends here

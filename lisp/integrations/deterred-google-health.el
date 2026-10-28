;;; deterred-google-health.el --- Google Health Takeout source -*- lexical-binding: t -*-

;; Copyright (C) 2026 Korytov Pavel

;; Author: Korytov Pavel <thexcloud@gmail.com>
;; Homepage: https://github.com/SqrtMinusOne/deterred.el

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Import the Google Health directory from an extracted Takeout archive.
;; Selected CSV files are parsed in Emacs Lisp before the database transaction.
;; Daily measurements are snapshots, and omitted metric families are preserved.
;; Both sleep algorithms are stored, with the effective SQL view selecting v2
;; for overlapping sleep sessions.  No routes or raw sensor streams are stored.

;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'deterred-db)
(require 'deterred-source)
(require 'deterred-google-health-parse)

(defcustom deterred-google-health-timezone "Europe/Moscow"
  "IANA timezone used to aggregate Google Health measurement samples.
Exported session offsets and dates of existing daily summaries are preserved.
Use the same timezone when importing overlapping exports."
  :group 'deterred-sources
  :type 'string)

(defconst deterred-google-health--import-log-buffer
  "*DETERRED Google Health Import*"
  "Buffer containing the most recent import counts and diagnostics.")

(defun deterred-google-health--read-directory ()
  "Read the Google Health directory from an extracted Takeout."
  (read-directory-name "Google Health directory: " nil nil t))

(defun deterred-google-health--parse-path (path)
  "Parse Google Health CSV files in PATH without accessing the database."
  (deterred-google-health-parse-directory
   (expand-file-name path) deterred-google-health-timezone))

(defun deterred-google-health--upsert-rows (db table rows key)
  "Upsert ROWS into TABLE in DB, using KEY as the conflict key.
Only supplied columns are updated, so absent daily metric families survive.
Values are bound parameters, including explicit nil values for SQL NULL."
  (let ((queries (make-hash-table :test #'equal)))
    (dolist (row rows)
      (let* ((columns (mapcar #'car row))
             (query
              (or (gethash columns queries)
                  (puthash
                   columns
                   (format
                    "INSERT INTO %s (%s) VALUES (%s)
ON CONFLICT (%s) DO UPDATE SET %s"
                    table
                    (mapconcat #'symbol-name columns ", ")
                    (mapconcat (lambda (_) "?") columns ", ")
                    key
                    (mapconcat
                     (lambda (column)
                       (format "%s = excluded.%s" column column))
                     (remq key columns) ", "))
                   queries))))
        (sqlite-execute db query (mapcar #'cdr row))))))

(defun deterred-google-health--import-data (data db)
  "Apply parsed DATA to DB in one transaction and return imported row counts.
Stage collections listed in `stage_session_ids' replace their previous rows.
If a stage file was absent, the parser omits those IDs and existing stages
remain intact.  Parse or database failures must not leave partial imports."
  (let ((sessions (alist-get 'sessions data))
        (stages (alist-get 'sleep_stages data))
        (daily (alist-get 'daily data))
        (deleted-session-ids (alist-get 'deleted_session_ids data))
        (stage-session-ids (alist-get 'stage_session_ids data))
        (session-map (make-hash-table :test #'equal))
        (stage-parents (make-hash-table :test #'equal)))
    (dolist (session sessions)
      (puthash (alist-get 'id session) session session-map))
    (dolist (id stage-session-ids)
      (unless (equal (alist-get 'kind (gethash id session-map)) "sleep")
        (error "Stage replacement requires an imported sleep session: %s" id))
      (puthash id t stage-parents))
    (dolist (stage stages)
      (let* ((id (alist-get 'session_id stage))
             (session (gethash id session-map)))
        (unless (gethash id stage-parents)
          (error "Stage has no imported parent collection: %s" id))
        (unless (and (>= (alist-get 'start_timestamp stage)
                         (alist-get 'start_timestamp session))
                     (<= (alist-get 'end_timestamp stage)
                         (alist-get 'end_timestamp session)))
          (error "Sleep stage lies outside its parent session: %s"
                 (alist-get 'id stage)))))
    (with-sqlite-transaction db
      (dolist (id deleted-session-ids)
        (let ((kind (caar (sqlite-select
                          db "SELECT kind FROM google_health_session WHERE id = ?"
                          (list id)))))
          (unless (and (string-prefix-p "exercise:" id)
                       (not (gethash id session-map))
                       (or (null kind) (equal kind "exercise")))
            (error "Invalid deleted exercise ID: %s" id))
          (sqlite-execute db "DELETE FROM google_health_session WHERE id = ?"
                          (list id))))
      (dolist (row daily)
        (let ((old-zone (caar (sqlite-select
                              db "SELECT timezone FROM google_health_daily
                                  WHERE date = ?"
                              (list (alist-get 'date row))))))
          (when (and old-zone (not (equal old-zone (alist-get 'timezone row))))
            (error "Google Health date %s was imported in %s, not %s"
                   (alist-get 'date row) old-zone (alist-get 'timezone row)))))
      (deterred-google-health--upsert-rows db 'google_health_session sessions 'id)
      (dolist (id stage-session-ids)
        (sqlite-execute db "DELETE FROM google_health_sleep_stage WHERE session_id = ?"
                        (list id)))
      (deterred-google-health--upsert-rows db 'google_health_sleep_stage stages 'id)
      (dolist (session sessions)
        (when (and (equal (alist-get 'kind session) "sleep")
                   (not (gethash (alist-get 'id session) stage-parents))
                   (sqlite-select
                    db "SELECT id FROM google_health_sleep_stage
                        WHERE session_id = ?
                          AND (start_timestamp < ? OR end_timestamp > ?) LIMIT 1"
                    (list (alist-get 'id session)
                          (alist-get 'start_timestamp session)
                          (alist-get 'end_timestamp session))))
          (error "Updated sleep %s requires its stage export to preserve valid intervals"
                 (alist-get 'id session))))
      (deterred-google-health--upsert-rows db 'google_health_daily daily 'date)
      (when (or sessions deleted-session-ids)
        (deterred-db-mark-updated db 'google_health_session))
      (when stage-session-ids
        (deterred-db-mark-updated db 'google_health_sleep_stage))
      (when daily (deterred-db-mark-updated db 'google_health_daily)))
    `((sessions . ,(length sessions))
      (deleted_sessions . ,(length deleted-session-ids))
      (sleep_stages . ,(length stages))
      (daily . ,(length daily)))))

;;;###autoload
(defun deterred-google-health-load-directory (directory &optional db)
  "Import selected Google Health CSVs from DIRECTORY into DB.
Interactively, use DETERRED's configured database.  The optional DB argument
allows isolated imports.  Return imported row counts, including updated rows."
  (interactive (list (deterred-google-health--read-directory)))
  (message "Parsing Google Health Takeout...")
  (let* ((data (deterred-google-health--parse-path directory))
         (counts (deterred-google-health--import-data
                  data (or db (deterred-db--init))))
         (warnings (alist-get 'warnings data)))
    (with-current-buffer (get-buffer-create deterred-google-health--import-log-buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Imported %d sessions, %d sleep stages and %d daily rows.\nDeleted exercises: %d.\n"
                        (alist-get 'sessions counts)
                        (alist-get 'sleep_stages counts)
                        (alist-get 'daily counts)
                        (alist-get 'deleted_sessions counts)))
        (dolist (warning warnings)
          (insert "\n" warning "\n"))
        (goto-char (point-min))
        (special-mode)))
    (message "Google Health: %d sessions, %d stages, %d daily rows; %d warnings%s"
             (alist-get 'sessions counts) (alist-get 'sleep_stages counts)
             (alist-get 'daily counts) (length warnings)
             (if warnings (concat " (see " deterred-google-health--import-log-buffer ")") ""))
    counts))

;;;###autoload
(defclass deterred-google-health (deterred-source)
  ((name :initform "Google Health")
   (warn-days :initform 30)
   (directory :initarg :directory :initform nil :type (or null string)))
  "Google Health Takeout source with an optional default import directory.")

(cl-defmethod deterred-source-actions ((source deterred-google-health) &optional callback)
  "Offer a Takeout import action for SOURCE, then run CALLBACK."
  (deterred-source--actions-pick
   (list (list "Load Takeout directory"
               (lambda ()
                 (deterred-google-health-load-directory
                  (or (oref source directory) (deterred-google-health--read-directory))))
               nil))
   callback))

(cl-defmethod deterred-source-sync ((source deterred-google-health) &optional callback)
  "Import the configured Takeout directory for SOURCE, then run CALLBACK."
  (deterred-google-health-load-directory
   (or (oref source directory) (deterred-google-health--read-directory)))
  (when callback (funcall callback)))

(defun deterred-google-health--date-timestamp (date timezone &optional end)
  "Convert DATE in TIMEZONE to its start timestamp, or last second if END."
  (let ((parts (mapcar #'string-to-number (split-string date "-"))))
    (- (time-convert
        (encode-time 0 0 0 (+ (nth 2 parts) (if end 1 0))
                     (nth 1 parts) (nth 0 parts) timezone)
        'integer)
       (if end 1 0))))

(cl-defmethod deterred-source-range ((_source deterred-google-health) &optional db)
  "Return the complete availability range of sessions and daily rows in DB."
  (let* ((db (or db (deterred-db--init)))
         (bounds (car (sqlite-select
                       db "SELECT MIN(start_timestamp), MAX(end_timestamp)
                           FROM google_health_session_effective")))
         (starts (delq nil (list (car bounds))))
         (ends (delq nil (list (cadr bounds)))))
    (dolist (range (sqlite-select
                   db "SELECT MIN(date), MAX(date), timezone
                       FROM google_health_daily GROUP BY timezone"))
      (push (deterred-google-health--date-timestamp (nth 0 range) (nth 2 range)) starts)
      (push (deterred-google-health--date-timestamp (nth 1 range) (nth 2 range) t) ends))
    (cons (when starts (apply #'min starts)) (when ends (apply #'max ends)))))

(cl-defmethod deterred-source-range-summary
  ((_source deterred-google-health) start end &optional db)
  "Summarize sessions by local date and daily measurements in DB.
START and END select the calendar dates shown in DETERRED's date picker.
Sleep belongs to its waking date rather than its starting UTC date."
  (let* ((db (or db (deterred-db--init)))
         (dates (list (format-time-string "%F" start) (format-time-string "%F" end)))
         (sessions (deterred-db-select-alist
                    db "SELECT kind, COUNT(*) AS count,
                               SUM(minutes_asleep) AS asleep,
                               SUM(end_timestamp - start_timestamp) / 3600.0 AS hours
                        FROM google_health_session_effective
                        WHERE local_date BETWEEN ? AND ? GROUP BY kind" dates))
         (daily (car (deterred-db-select-alist
                      db "SELECT COUNT(*) AS days, SUM(steps) AS steps,
                                 SUM(energy_kcal) AS energy,
                                 SUM(active_energy_kcal) AS active_energy
                          FROM google_health_daily WHERE date BETWEEN ? AND ?" dates)))
         (sleep (cl-find "sleep" sessions :key (lambda (row) (alist-get 'kind row))
                         :test #'equal))
         (exercise (cl-find "exercise" sessions :key (lambda (row) (alist-get 'kind row))
                            :test #'equal))
         (parts nil))
    (dolist (row sessions)
      (push (if (equal (alist-get 'kind row) "sleep")
                (format "Sleep: %d sessions%s"
                        (alist-get 'count row)
                        (if-let* ((minutes (alist-get 'asleep row)))
                            (format ", %.1f h asleep" (/ minutes 60.0)) ""))
              (format "%s: %d sessions, %.1f h"
                      (capitalize (alist-get 'kind row))
                      (alist-get 'count row) (alist-get 'hours row)))
            parts))
    (dolist (metric '((steps "Steps: %.0f")
                      (energy "Total energy: %.0f kcal")
                      (active_energy "Active energy: %.0f kcal")))
      (when-let* ((value (alist-get (car metric) daily)))
        (push (format (cadr metric) value) parts)))
    (when (and (null parts) (> (alist-get 'days daily) 0))
      (push (format "Health measurements: %d days" (alist-get 'days daily)) parts))
    (when (or sessions (> (alist-get 'days daily) 0))
      (cl-flet ((measurement (value decimals)
                  (if (numberp value) (format (if decimals "%.1f" "%.0f") value) "—")))
        (let ((summary
               (format "%s steps, %s (%s) kcal energy (act.), %s h sleep, %s h exercise."
                       (measurement (alist-get 'steps daily) nil)
                       (measurement (alist-get 'energy daily) nil)
                       (measurement (alist-get 'active_energy daily) nil)
                       (measurement (when-let* ((minutes (alist-get 'asleep sleep)))
                                      (/ minutes 60.0)) t)
                       (measurement (alist-get 'hours exercise) t))))
          `((:short-description . ,summary)
            (:long-description . ,(string-join (nreverse parts) "\n"))))))))

(cl-defmethod deterred-source-events
  ((_source deterred-google-health) start end &optional _params db)
  "Return effective session intervals overlapping START through END from DB."
  (sqlite-select
   (or db (deterred-db--init))
   "SELECT start_timestamp, end_timestamp,
           CASE WHEN kind = 'exercise' THEN COALESCE(activity_type, 'exercise')
                ELSE kind END
    FROM google_health_session_effective
    WHERE end_timestamp > ? AND start_timestamp <= ?
    ORDER BY start_timestamp, id"
   (list start end)))

(declare-function deterred-dashboard-google-health "deterred-dashboard-google-health")

(cl-defmethod deterred-source-default-dashboard ((_source deterred-google-health))
  "Return the Google Health dashboard."
  (require 'deterred-dashboard-google-health)
  (deterred-dashboard-google-health))

(provide 'deterred-google-health)
;;; deterred-google-health.el ends here

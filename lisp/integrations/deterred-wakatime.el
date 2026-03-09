;;; deterred-wakatime.el --- WakaTime integration for DETERRED. -*- lexical-binding: t -*-

;; Copyright (C) 2025 Korytov Pavel

;; Author: Korytov Pavel <thexcloud@gmail.com>
;; Maintainer: Korytov Pavel <thexcloud@gmail.com>
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

;; WakaTime integration for DETERRED.
;;
;; This provides can integrate either via WakaTime dump
;; (`deterred-wakatime-load-json') or via the API
;; (`deterred-wakatime-api-load').  The former way is preferreable
;; because the API doesn't seem to be particularly stable, and it only
;; gives the last 7 days anyway.

;;; Code:
(require 'deterred-db)
(require 'deterred-source)
(require 'deterred-utils)
(require 'request)
(require 'iso8601)
(require 'cl-lib)
(require 'uuidgen)
(require 'org-duration)

(defconst deterred-wakatime-key-mappings
  '((branches total_seconds name)
    (categories total_seconds name)
    (editors total_seconds name)
    (entities total_seconds name type)
    (grand_total total_seconds)
    (languages total_seconds name)
    (machines total_seconds name)
    (operating_systems total_seconds name))
  "A tree of keys to parse in the wakatime dump.")

(defconst deterred-wakatime-uuid-namespace
  "e6e12255-6b5c-4fed-9e51-c74fc6570ca8")

(defcustom deterred-wakatime-api-key nil
  "Api key for WakaTime."
  :group 'deterred-sources
  :type 'string)

(defcustom deterred-wakatime-api-endpoint "https://wakatime.com/api/v1/"
  "Api key for WakaTime."
  :group 'deterred-sources
  :type 'string)

(defcustom deterred-wakatime-api-range 7
  "How many days in the past to include in API export.

If nil, infer the number of missing days from local data and download
that many days plus one overlap day."
  :group 'deterred-sources
  :type '(choice
          (const :tag "Auto infer from local data" nil)
          (number :tag "Fixed number of days")))

(defcustom deterred-wakatime-wakapi-compatibility-mode nil
  "Whether to adapt summary parsing for Wakapi compatibility API.

When non-nil, summary date extraction prefers `range.start' because
Wakapi currently does not always provide stable daily values in
`range.date'."
  :group 'deterred-sources
  :type 'boolean)

(defcustom deterred-wakatime-process-project-name #'identity
  "A function to process project name."
  :group 'deterred-sources
  :type 'function)

(defun deterred-wakatime--api-normalize-date-string (value)
  "Normalize VALUE to YYYY-MM-DD."
  (when (stringp value)
    (if (string-match-p (rx bos (= 4 digit) "-" (= 2 digit) "-" (= 2 digit)) value)
        (substring value 0 10)
      (condition-case nil
          (format-time-string "%Y-%m-%d" (iso8601-parse value))
        (error nil)))))

(defun deterred-wakatime--api-date-string (day-in-project)
  "Extract YYYY-MM-DD date string from DAY-IN-PROJECT summary item."
  (let* ((range (alist-get 'range day-in-project))
         (preferred (if deterred-wakatime-wakapi-compatibility-mode
                        (or (alist-get 'start range)
                            (alist-get 'date range))
                      (or (alist-get 'date range)
                          (alist-get 'start range))))
         (date-string (deterred-wakatime--api-normalize-date-string preferred)))
    (unless date-string
      (error "Unable to derive date from summary range: %S" range))
    date-string))

(defun deterred-wakatime--api-compute-range-days (&optional db)
  "Compute range days for API sync.

If `deterred-wakatime-api-range' is non-nil, return it.
If it is nil, infer missing days from the latest local WakaTime entry
and add one overlap day."
  (if deterred-wakatime-api-range
      deterred-wakatime-api-range
    (let* ((db (or db (deterred-db--init)))
           (latest-ts
            (condition-case nil
                (caar (sqlite-select db "SELECT MAX(timestamp) FROM wakatime_entities"))
              (error nil)))
           (missing-days
            (if latest-ts
                (max 0
                     (- (time-to-days (current-time))
                        (time-to-days (seconds-to-time latest-ts))))
              ;; Initial sync without prior data.
              7)))
      (1+ missing-days))))

(defun deterred-wakatime--process-project (day-in-project db)
  "Get project ID from DAY-IN-PROJECT.

DAY-IN-PROJECT is a structure from the WakaTime dump, containing the
aggregate of the project activity in a given day.  DB is a SQLite
object.

Return the project ID."
  (let* ((name (funcall deterred-wakatime-process-project-name
                        (alist-get 'name day-in-project)))
         (id (uuidgen-3 deterred-wakatime-uuid-namespace name)))
    (sqlite-execute db "INSERT INTO wakatime_projects (id, name) VALUES (?, ?)
                     ON CONFLICT (id) DO NOTHING"
                    (list id name))
    id))

(defun deterred-wakatime--process-day-in-project (db date-string day-in-project)
  "Process data from one day in project.

DAY-IN-PROJECT can be retrieved from:
- the Wakatime dump in days->projects
- the summary endpoint with the project parameter.

DATE-STRING must be in the YYYY-MM-DD format.

DB is the sqlite database object."
  (let ((date (iso8601-parse-date date-string))
        (project-id (deterred-wakatime--process-project
                     day-in-project db)))
    (setf
     (decoded-time-second date) 0
     (decoded-time-minute date) 0
     (decoded-time-hour date) 0)
    (dolist (key-mapping deterred-wakatime-key-mappings)
      (let ((key-values (alist-get (car key-mapping) day-in-project)))
        (when (eq (car key-mapping) 'grand_total)
          (setq key-values (list key-values)))
        (deterred-db-insert-unsafe
         db
         :table-name (intern (format "wakatime_%s" (car key-mapping)))
         :values (cl-map
                  'list
                  (lambda (datum)
                    (append
                     `((project_id . ,project-id)
                       (timestamp . ,(time-convert
                                      (encode-time date)
                                      'integer)))
                     datum
                     nil))
                  key-values)
         :attrs (append '(project_id timestamp) (cdr key-mapping))
         :conflict-action 'do-update
         :conflict-attrs `(project_id
                           timestamp
                           ,@(unless (eq (car key-mapping) 'grand_total)
                               '(name))))))))

(defun deterred-wakatime--get-project-path (filename project-name &optional project)
  "Convert FILENAME to a local path under PROJECT-NAME.

Or, if PROJECT is non-nil, get project path instead.

FILENAME and PROJECT-NAME are string."
  (let ((project-found project) items-in-project)
    (dolist (item (file-name-split filename))
      (let ((cand-name (funcall deterred-wakatime-process-project-name item)))
        (when project-found
          (push item items-in-project))
        (when (equal cand-name project-name)
          (if project
              (setq project-found nil)
            (setq project-found t)))))
    (setq items-in-project (nreverse items-in-project))
    (when items-in-project
      (apply #'file-name-concat "/" items-in-project))))

(defun deterred-wakatime--postprocess-entities (db)
  "Update project_path in the wakatime_entities table.

DB is a SQLite connection object."
  (let* ((projects
          (deterred-db-select-alist
           db "SELECT id, name FROM wakatime_projects"))
         (entities-to-process
          (deterred-db-select-alist
           db "SELECT project_id, timestamp, total_seconds, name
FROM wakatime_entities WHERE project_path IS NULL"))
         (project-name-by-id (make-hash-table :test #'equal))
         (project-path-by-file-name (make-hash-table :test #'equal))
         entities-update-data
         projects-update-data)
    ;; Populate `project-name-by-id'
    (cl-loop for project in projects
             do (puthash (alist-get 'id project)
                         (alist-get 'name project)
                         project-name-by-id))
    ;; Populate `projects-update-data'
    (cl-loop for i from 0
             with total-entities = (seq-length entities-to-process)
             for entity in entities-to-process
             for name = (alist-get 'name entity)
             for project-name = (gethash (alist-get 'project_id entity)
                                         project-name-by-id)
             for project-path = (gethash (alist-get 'name entity)
                                         project-path-by-file-name)
             unless project-path
             do (progn
                  (setq project-path (deterred-wakatime--get-project-path
                                      name
                                      project-name))
                  (when project-path
                    (puthash name
                             project-path
                             project-path-by-file-name)))
             when project-path
             do (push `(,@entity
                        (project_path . ,project-path))
                      entities-update-data)
             when (= (% i 100))
             do (message "Postprocessing %d/%d wakatime entities" i total-entities))
    (when entities-update-data
      (deterred-db-insert-unsafe
       db
       :table-name 'wakatime_entities
       :values entities-update-data
       :conflict-attrs '(project_id timestamp name)
       :conflict-action 'do-update))))

(defun deterred-wakatime--postprocess-projects (db)
  "Update project_root in wakatime_projects.

DB is the SQLite connection object."
  (let ((data (deterred-db-select-alist
               db "WITH latest_timestamps AS (
  SELECT
    project_id,
    MAX(timestamp) AS max_timestamp
  FROM wakatime_entities
  GROUP BY project_id
),
ranked_entities AS (
  SELECT
    e.project_id,
    wp.name project_name,
    e.timestamp,
    e.total_seconds,
    e.name,
    e.type,
    e.project_path,
    ROW_NUMBER() OVER (PARTITION BY e.project_id ORDER BY e.name) AS rn
  FROM wakatime_entities e
  INNER JOIN latest_timestamps lt
    ON e.project_id = lt.project_id
    AND e.timestamp = lt.max_timestamp
  INNER JOIN wakatime_projects wp ON wp.id = e.project_id
  WHERE e.type = 'file'
)
SELECT
  project_id,
  project_name,
  timestamp,
  total_seconds,
  name,
  type,
  project_path
FROM ranked_entities
WHERE rn = 1")))
    (deterred-db-insert-unsafe
     db
     :table-name 'wakatime_projects
     :values (seq-filter
              (lambda (datum) (alist-get 'project_root datum))
              (mapcar (lambda (datum)
                        `((id . ,(alist-get 'project_id datum))
                          (name . ,(alist-get 'project_name datum))
                          (project_root
                           . ,(deterred-wakatime--get-project-path
                               (alist-get 'name datum)
                               (alist-get 'project_name datum)
                               t))))
                      data))
     :conflict-attrs '(id)
     :conflict-action 'do-update)))

(defun deterred-wakatime-purge ()
  "Clear all WakaTime data from the DETERRED database."
  (interactive)
  (when (y-or-n-p "Are you sure you want to purge WakaTime data from DETERRED?")
    (let ((db (deterred-db--init)))
      (with-sqlite-transaction db
        (dolist (item deterred-wakatime-key-mappings)
          (sqlite-execute db (format "DELETE FROM wakatime_%s" (car item))))
        (sqlite-execute db "DELETE FROM wakatime_projects")
        (deterred-db-mark-updated-batch
         db
         (append
          (mapcar (lambda (f) (format "wakatime_%s" (car f)))
                  deterred-wakatime-key-mappings)
          (list "wakatime_projects")
          nil))))))

(defun deterred-wakatime-load-json (file)
  "Load Wakatime export FILE into DETERRED."
  (interactive
   (list
    (read-file-name "JSON file: " nil nil nil nil
                    (lambda (f)
                      (or (file-directory-p f)
                          (string-match-p (rx ".json" eos) f))))))
  (let ((data (json-read-file file))
        (db (deterred-db--init)))
    (with-sqlite-transaction db
      (cl-mapc
       (lambda (day)
         (let ((date-string (alist-get 'date day)))
           (cl-mapc
            (lambda (day-in-project)
              (deterred-wakatime--process-day-in-project
               db date-string day-in-project))
            (alist-get 'projects day))
           (message "Processed: %s" (alist-get 'date day))))
       (alist-get 'days data))
      (deterred-wakatime--postprocess-entities db)
      (deterred-wakatime--postprocess-projects db)
      (deterred-db-mark-updated-batch
       db
       (append
        (mapcar (lambda (f) (format "wakatime_%s" (car f)))
                deterred-wakatime-key-mappings)
        (list "wakatime_projects")
        nil)))))

(defun deterred-wakatime--api-get-summary
    (callback &optional project-name range-days)
  "Invoke WakaTime's summary endpoint.  Feed the results into CALLBACK.

If PROJECT-NAME is non-nil, filter by it.
If RANGE-DAYS is non-nil, use it as the lookback window."
  (let* ((range-days (or range-days (deterred-wakatime--api-compute-range-days)))
         (params `(("api_key" . ,deterred-wakatime-api-key)
                   ("end" . ,(format-time-string "%Y-%m-%d"))
                   ("start" . ,(format-time-string
                                "%Y-%m-%d"
                                (- (time-convert nil 'integer)
                                   (* 60 60 24 range-days)))))))
    (when project-name
      (setf (alist-get "project" params nil nil #'equal)
            project-name))
    (request (concat deterred-wakatime-api-endpoint "users/current/summaries")
      :parser 'json-read
      :params `(,@params)
      :encoding 'utf-8
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (funcall callback data)))
      :error #'deterred-utils-on-request-error)))

(defun deterred-wakatime--api-get-summary-recursive
    (project-names callback &optional data range-days)
  "Invoke Wakatime's summary endpoint for all PROJECT-NAMES.

Call CALLBACK with an alist with project names as keys and responses
as values.

DATA is the recursive parameter.
RANGE-DAYS is the API lookback window."
  (if (seq-empty-p project-names)
      (funcall callback data)
    (message "Fetching %s..." (car project-names))
    (deterred-wakatime--api-get-summary
     (lambda (project-data)
       (push (cons (car project-names) project-data)
             data)
       (deterred-wakatime--api-get-summary-recursive
        (cdr project-names) callback data range-days))
     (car project-names)
     range-days)))

(defun deterred-wakatime-api-load (&optional callback)
  "Load data from the WakaTime API into DETERRED.

Call CALLBACK on success.

This requires `deterred-wakatime-api-key' to be set.

If you aren't a premium user, WakaTime will only return the last 14
days, so either you run this at least every 14 days or use
`deterred-wakatime-load-json' with a WakaTime dump file.

If you are, you probably can set `deterred-wakatime-api-range' to some
large value and download everything, but I haven't tried this."
  (interactive)
  (let ((range-days (deterred-wakatime--api-compute-range-days)))
    (message "Fetching WakaTime API data for %s day(s)" range-days)
    (deterred-wakatime--api-get-summary
     (lambda (data)
       (let ((project-names (thread-last
                              (alist-get 'data data)
                              (mapcar (lambda (day)
                                        (mapcar
                                         (lambda (project)
                                           (alist-get 'name project))
                                         (alist-get 'projects day))))
                              (flatten-list)
                              (seq-uniq))))
         (message "Fetched %s projects from WakaTime"
                  (seq-length project-names))
         (deterred-wakatime--api-get-summary-recursive
          project-names
          (lambda (all-projects-data)
            (let ((db (deterred-db--init)))
              (with-sqlite-transaction db
                (pcase-dolist (`(,project-name . ,project-data) all-projects-data)
                  (cl-mapc
                   (lambda (day-in-project)
                     (setf (alist-get 'name day-in-project) project-name)
                     (deterred-wakatime--process-day-in-project
                      db
                      (deterred-wakatime--api-date-string day-in-project)
                      day-in-project))
                   (alist-get 'data project-data)))
                (deterred-wakatime--postprocess-entities db)
                (deterred-wakatime--postprocess-projects db)
                (deterred-db-mark-updated-batch
                 db
                 (append
                  (mapcar (lambda (f) (format "wakatime_%s" (car f)))
                          deterred-wakatime-key-mappings)
                  (list "wakatime_projects")
                  nil))))
            (if callback
                (funcall callback)
              (message "Done fetching %s projects from WakaTime"
                       (seq-length project-names))))
          nil
          range-days)))
     nil
     range-days)))

;;;###autoload
(defclass deterred-wakatime (deterred-source)
  ((name :initform "Wakatime"))
  "DETERRED source for wakatime.")

(cl-defmethod deterred-source-range ((_source deterred-wakatime) &optional db)
  "Get the data availability range for Mastodon.

DB is the sqlite database object.

Return a cons cell, with car as the start timestamp, and cdr as the
end timestamp."
  (let* ((db (or db (deterred-db--init)))
         (data (sqlite-select
                db "SELECT MIN(timestamp), MAX(timestamp)
                    FROM wakatime_entities")))
    (cons (caar data) (cadar data))))

(cl-defmethod deterred-source-actions ((_source deterred-wakatime) &optional callback)
  "Run an action for the wakatime source.

Run CALLBACK when done."
  (deterred-source--actions-pick
   '(("Load Wakatime JSON" deterred-wakatime-load-json nil))
   callback))

(cl-defmethod deterred-source-sync ((_source deterred-wakatime) &optional callback)
  "Sync WakaTime with DETERRED.

Call CALLBACK when done."
  (unless deterred-wakatime-api-key
    (user-error "Wakatime API key not set!"))
  (deterred-wakatime-api-load callback))

(cl-defmethod deterred-source-range-summary
  ((_source deterred-wakatime) start end &optional db)
  "Make WakaTime summary for [START, END].

DB is the sqlite database object."
  (let* ((db (or db (deterred-db--init)))
         (total-data
          (deterred-db-select-alist
           db "SELECT wp.name, sum(wgt.total_seconds) / 60 total
               FROM wakatime_projects wp
               INNER JOIN wakatime_grand_total wgt ON wgt.project_id = wp.id
               WHERE wgt.timestamp BETWEEN ? AND ?
               GROUP BY wp.id, wp.name
               ORDER BY total DESC"
           (list start end)))
         (editor-data
          (deterred-db-select-alist
           db "SELECT we.name editor, sum(we.total_seconds) / 60 total
               FROM wakatime_editors we
               WHERE we.timestamp BETWEEN ? AND ?
               GROUP BY we.name
               ORDER BY total DESC"
           (list start end)))
         (languages-data
          (deterred-db-select-alist
           db "SELECT wl.name lang, wp.name project, sum(wl.total_seconds) / 60 total
               FROM wakatime_languages wl
               INNER JOIN wakatime_projects wp ON wp.id = wl.project_id
               WHERE wl.timestamp BETWEEN ? AND ?
               GROUP BY wl.name
               ORDER BY total DESC"
           (list start end)))
         (total-minutes
          (apply #'+ (mapcar (lambda (d) (alist-get 'total d)) total-data)))
         (first-project-percentile (if (> total-minutes 0)
                                       (/ (alist-get 'total (car total-data))
                                          total-minutes)
                                     0)))
    (when (> total-minutes 0)
      (let ((first-project-duration
             (org-duration-from-minutes
              (alist-get 'total (car total-data))))
            (other-projects-duration
             (org-duration-from-minutes
              (- total-minutes
                 (alist-get 'total (car total-data)))))
            (all-projects-duration
             (org-duration-from-minutes total-minutes)))
        `((:short-description
           . ,(deterred-format
               (when (> first-project-percentile 0.5)
                 (f first-project-duration " in " (f-acc "total-data[0]->'name") "; "
                    other-projects-duration " in " (f-num
                                                    (1- (length total-data)))
                    " other projects"))
               (when (<= first-project-percentile 0.5)
                 (f all-projects-duration " in " (f-num (length total-data))
                    " projects"))))
          (:long-description
           . ,(deterred-format
               "Projects: \n"
               (f-mapconcat
                (f "- " (org-duration-from-minutes (alist-get 'total iter))
                   " in " (f-acc "iter->'name"))
                total-data
                "\n"))))))))

(provide 'deterred-wakatime)
;;; deterred-wakatime.el ends here

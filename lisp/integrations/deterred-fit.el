;;; deterred-fit.el --- FIT file integration for DETERRED -*- lexical-binding: t -*-

;; Copyright (C) 2026 Korytov Pavel

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

;; FIT file integration for DETERRED.

;;; Code:
(require 'json)
(require 'org-duration)
(require 'subr-x)
(require 'uuidgen)

(require 'deterred-db)
(require 'deterred-source)

(defcustom deterred-fit-python (or (executable-find "python3")
                                   (executable-find "python"))
  "Python executable used to parse FIT files."
  :group 'deterred-sources
  :type 'string)

(defconst deterred-fit-uuid-namespace
  "ebae5b10-bfc1-470e-9e10-5c6a04860fe3")

(defconst deterred-fit--parser-script
  (expand-file-name
   (concat (unless load-file-name "../") "../python/deterred-fit-parse.py")
   (file-name-directory (or load-file-name buffer-file-name default-directory)))
  "Path to the helper script that parses FIT files.")

(defun deterred-fit--read-directory ()
  "Prompt for a directory with FIT files."
  (read-directory-name "FIT directory: " nil nil t))

(defun deterred-fit--id (file-id-key)
  "Create a stable UUID from FILE-ID-KEY."
  (uuidgen-3 deterred-fit-uuid-namespace file-id-key))

(defun deterred-fit--parse-path (path)
  "Parse FIT files from PATH and return a list of activity alists."
  (unless deterred-fit-python
    (user-error "No Python executable found for FIT parsing"))
  (unless (file-exists-p deterred-fit--parser-script)
    (user-error "FIT parser script not found: %s" deterred-fit--parser-script))
  (with-temp-buffer
    (let ((status (call-process deterred-fit-python nil t nil
                                deterred-fit--parser-script
                                (expand-file-name path))))
      (unless (zerop status)
        (error "FIT parser failed: %s" (string-trim (buffer-string))))
      (goto-char (point-min))
      (let ((json-object-type 'alist)
            (json-array-type 'list)
            (json-key-type 'symbol)
            (json-null nil)
            (json-false nil))
        (json-read)))))

(defun deterred-fit--normalize-item (item)
  "Convert ITEM from JSON into a DB row alist."
  `((id . ,(deterred-fit--id (alist-get 'file_id_key item)))
    (software . ,(alist-get 'software item))
    (sport_name . ,(alist-get 'sport_name item))
    (version . ,(alist-get 'version item))
    (part_number . ,(alist-get 'part_number item))
    (start_timestamp . ,(alist-get 'start_timestamp item))
    (end_timestamp . ,(alist-get 'end_timestamp item))
    (start_lat . ,(alist-get 'start_lat item))
    (end_lat . ,(alist-get 'end_lat item))
    (distance . ,(alist-get 'distance item))
    (average_speed . ,(alist-get 'average_speed item))))

(defun deterred-fit-load-directory (directory)
  "Load FIT activities from DIRECTORY into DETERRED."
  (interactive (list (deterred-fit--read-directory)))
  (let* ((db (deterred-db--init))
         (items (mapcar #'deterred-fit--normalize-item
                        (deterred-fit--parse-path directory))))
    (when items
      (with-sqlite-transaction db
        (deterred-db-insert-unsafe
         db
         :table-name 'fit_activity
         :values items
         :conflict-action 'do-update
         :conflict-attrs '(id))
        (deterred-db-mark-updated db 'fit_activity)))
    items))

;;;###autoload
(defclass deterred-fit (deterred-source)
  ((name :initform "FIT")
   (warn-days :initform 14))
  "DETERRED source for FIT activity files.")

(cl-defmethod deterred-source-actions ((_source deterred-fit) &optional callback)
  "Run a manual action for the FIT source.

Run CALLBACK when done."
  (deterred-source--actions-pick
   '(("Load FIT directory" deterred-fit-load-directory nil))
   callback))

(cl-defmethod deterred-source-sync ((_source deterred-fit) &optional callback)
  "Sync FIT activities with DETERRED.

Call CALLBACK when done."
  (deterred-fit-load-directory (deterred-fit--read-directory))
  (when callback
    (funcall callback)))

(cl-defmethod deterred-source-range ((_source deterred-fit) &optional db)
  "Get the data availability range for FIT activities.

DB is the sqlite database object."
  (let* ((db (or db (deterred-db--init)))
         (data (sqlite-select
                db "SELECT MIN(start_timestamp), MAX(end_timestamp)
                    FROM fit_activity")))
    (cons (caar data) (cadar data))))

(cl-defmethod deterred-source-range-summary
  ((_source deterred-fit) start end &optional db)
  "Make a summary for FIT activities in [START, END].

DB is the sqlite database object."
  (let* ((db (or db (deterred-db--init)))
         (summary
          (car (deterred-db-select-alist
                db "SELECT COUNT(*) activity_count,
                           COALESCE(SUM(distance), 0) total_distance
                    FROM fit_activity
                    WHERE start_timestamp BETWEEN ? AND ?"
                (list start end))))
         (sports
          (deterred-db-select-alist
           db "SELECT sport_name, COUNT(*) activity_count
               FROM fit_activity
               WHERE start_timestamp BETWEEN ? AND ?
               GROUP BY sport_name
               ORDER BY activity_count DESC"
           (list start end)))
         (activity-count (alist-get 'activity_count summary))
         (total-distance (alist-get 'total_distance summary)))
    (when (> activity-count 0)
      `((:short-description
         . ,(format "%d activities, %.2f km"
                    activity-count (/ total-distance 1000.0)))
        (:long-description
         . ,(concat
             (format "Total distance: %.2f km\n" (/ total-distance 1000.0))
             (mapconcat
              (lambda (item)
                (format "- %s: %d"
                        (or (alist-get 'sport_name item) "(unknown)")
                        (alist-get 'activity_count item)))
              sports
              "\n")))))))

(cl-defmethod deterred-source-events
  ((_source deterred-fit) start end &optional _params db)
  "Return FIT activity intervals for [START, END].

The third event field is the sport name, so default grouping is by
sport.

DB is the sqlite database object."
  (let ((db (or db (deterred-db--init))))
    (sqlite-select
     db
     "SELECT start_timestamp, end_timestamp, sport_name
FROM fit_activity
WHERE end_timestamp >= ?
  AND start_timestamp <= ?
ORDER BY start_timestamp"
     (list start end))))

(declare-function deterred-dashboard-fit "deterred-dashboard-fit")

(cl-defmethod deterred-source-default-dashboard ((_source deterred-fit))
  "Return the default dashboard for FIT activities."
  (deterred-dashboard-fit))

(provide 'deterred-fit)
;;; deterred-fit.el ends here

;;; deterred-wakatime.el --- TODO -*- lexical-binding: t -*-

;; Copyright (C) 2024 Korytov Pavel

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

;; TODO

;;; Code:
(require 'deterred-db)
(require 'cl-lib)

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

(defun deterred-wakatime--process-project (day-in-project db)
  "Get project ID from DAY-IN-PROJECT.

DAY-IN-PROJECT is a structure from the WakaTime dump, containing the
aggregate of the project activity in a given day.  DB is a SQLite
object.

Return the project ID."
  (let* ((name (alist-get 'name day-in-project))
         (id (uuidgen-3 deterred-wakatime-uuid-namespace name)))
    (sqlite-execute db "INSERT INTO wakatime_projects (id, name) VALUES (?, ?)
                     ON CONFLICT (id) DO NOTHING"
                    (list id name))
    id))

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
         (let ((date (iso8601-parse-date (alist-get 'date day))))
           (setf
            (decoded-time-second date) 0
            (decoded-time-minute date) 0
            (decoded-time-hour date) 0)
           (cl-mapc
            (lambda (day-in-project)
              (let ((project-id (deterred-wakatime--process-project
                                 day-in-project db)))
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
                     :conflict-action 'do-nothing)))))
            (alist-get 'projects day))
           (message "Processed: %s" (alist-get 'date day))))
       (alist-get 'days data))
      (deterred-db--mark-update-batch
       (append
        (mapcar (lambda (f) (format "wakatime_%s" (car f)))
                deterred-wakatime-key-mappings)
        (list "wakatime_projects")
        nil)
       db))))

(provide 'deterred-wakatime)
;;; deterred-wakatime.el ends here

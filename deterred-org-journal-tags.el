;;; deterred-org-journal-tags.el --- TODO -*- lexical-binding: t -*-

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
(require 'deterred-locations)
(require 'cl-lib)
(require 'org-journal-tags)

(defconst deterred-org-journal-tags-uuid-namespace
  "97cdf2aa-0f6f-4c03-8071-73483503db49")

(defun deterred-org-journal-tags--parse-timestamp (timestamp header)
  "Try to parse org-journal timestamp from TIMESTAMP and HEADER.

TIMESTAMP is a UNIX timestamp with the date of the record.  HEADER is
the header of the entry, which should be parseable by
`parse-time-string'."
  (let* ((date (decode-time timestamp))
         (time (parse-time-string header)))
    (setf (decoded-time-second date) (decoded-time-second time)
          (decoded-time-minute date) (decoded-time-minute time)
          (decoded-time-hour date) (decoded-time-hour time))
    (let ((final-timestamp (time-convert (encode-time date) 'integer)))
      (unless (string-empty-p org-journal-time-format-post-midnight)
        (let ((time-pm
               (string-trim
                (format-time-string org-journal-time-format-post-midnight
                                    final-timestamp))))
          (when (string-match-p (regexp-quote time-pm) header)
            (setq final-timestamp (+ final-timestamp (* 24 60 60))))))
      final-timestamp)))

(defun deterred-org-journal-tags---parse-ref (db ref)
  "Parse `org-journal-tag-reference' REF into a DETERRED record.

DB is the sqlite database object, used to determine the timezones."
  (let* ((local-timestamp (deterred-org-journal-tags--parse-timestamp
                           (org-journal-tag-reference-date ref)
                           (org-journal-tag-reference-time ref)))
         (local-offset (car (current-time-zone)))
         (target-offset (deterred-locations-offset-at
                         local-timestamp nil db))
         (timestamp (- local-timestamp (- target-offset local-offset)))
         (id (uuidgen-3 deterred-org-journal-tags-uuid-namespace
                        (number-to-string timestamp)))
         (size (- (org-journal-tag-reference-ref-end ref)
                  (org-journal-tag-reference-ref-start ref))))
    `((id . ,id)
      (timestamp . ,timestamp)
      (size . ,size))))

(defun deterred-org-journal-tags--list-records (db)
  "List all journal records in `org-journal-tags'.

DB is the sqlite database object, used to determine the timezones."
  (let ((all-refs (org-journal-tags-query)))
    (mapcar (lambda (ref)
              (deterred-org-journal-tags---parse-ref db ref))
            all-refs)))

(defun deterred-org-journal-tags--list-tags-and-records (db)
  "List all tags in `org-journal-tags'.

DB is the sqlite database object, used to determine the timezones."
  (let (tags records)
    (cl-loop
     for tag-name being the hash-keys of (alist-get :tags org-journal-tags-db)
     using (hash-values tag-value)
     for tag-id = (uuidgen-3 deterred-org-journal-tags-uuid-namespace tag-name)
     do (unless (string-empty-p tag-name)
          (push `((id . ,(uuidgen-3 deterred-org-journal-tags-uuid-namespace tag-name))
                  (name . ,tag-name))
                tags)
          (cl-loop for refs being the hash-values of (org-journal-tag-dates tag-value)
                   do (cl-loop
                       for ref in refs
                       for parsed-ref = (deterred-org-journal-tags---parse-ref db ref)
                       do (push `((tag_id . ,tag-id)
                                  (record_id . ,(alist-get 'id parsed-ref)))
                                records)))))
    (list tags records)))

(defun deterred-org-journal-tags-load ()
  "Load `org-journal-tags' into DETERRED."
  (interactive)
  (org-journal-tags-db-ensure)
  (let* ((db (deterred-db--init))
         (records (deterred-org-journal-tags--list-records db))
         (tags-and-records (deterred-org-journal-tags--list-tags-and-records db)))
    (with-sqlite-transaction db
      (deterred-db-insert-unsafe
       db :table-name 'org_journal_record
       :values records
       :conflict-action 'do-update
       :conflict-attrs '(id))
      (deterred-db-insert-unsafe
       db :table-name 'org_journal_tag
       :values (car tags-and-records)
       :conflict-action 'do-update
       :conflict-attrs '(id))
      (deterred-db-insert-unsafe
       db :table-name 'org_journal_record_tag
       :values (nth 1 tags-and-records)
       :conflict-action 'do-nothing
       :conflict-attrs '(record_id tag_id))
      (deterred-db--mark-update-batch
       db
       '(org_journal_record org_journal_tag org_journal_record_tag)))))

(defclass deterred-org-journal-tags (deterred-source)
  ((name :initform "Org Journal"))
  "DETERRED source for org-journal-tags.")

(cl-defmethod deterred-source-range ((_source deterred-org-journal-tags) &optional db)
  "Get the data availability range for Mastodon.

DB is the sqlite database object.

Return a cons cell, with car as the start timestamp, and cdr as the
end timestamp."
  (let* ((db (or db (deterred-db--init)))
         (data (sqlite-select
                db "SELECT MIN(timestamp), MAX(timestamp)
                    FROM org_journal_record")))
    (cons (caar data) (cadar data))))

(cl-defmethod deterred-source-sync ((_source deterred-org-journal-tags)
                                    &optional callback)
  "Sync DETERRED with org-journal-tags.

Call CALLBACK when done."
  (deterred-org-journal-tags-load)
  (when callback
    (funcall callback)))

(defun deterred-org-journal-tags--render-refs (refs)
  (dolist (ref refs)
    (magit-insert-section (deterred-org-journal-item t)
      (insert
       (propertize
        (org-journal-tag-reference-time ref)
        'face 'deterred-faces-section-heading-4))
      (magit-insert-heading)
      (insert (org-journal-tags--extract-ref ref) "\n"))))

(cl-defmethod deterred-source-day-summary
  ((_source deterred-org-journal-tags) timestamp &optional _db)
  (let ((refs (org-journal-tags-query
               :start-date timestamp
               :end-date (+ (* 60 60 24) timestamp (- 1)))))
    (when refs
      `((:short-description
         . ,(deterred-format (f-num (seq-length refs)) " "
                             (if (= (seq-length refs) 1) "record" "records ")))
        (:long-description-fn
         . ,(lambda (&rest _) (deterred-org-journal-tags--render-refs refs)))))))

(provide 'deterred-org-journal-tags)
;;; deterred-org-journal-tags.el ends here

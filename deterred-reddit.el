;;; deterred-reddit.el --- TODO -*- lexical-binding: t -*-

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

;; TODO

;;; Code:
(require 'deterred-db)
(require 'deterred-utils)
(require 'deterred-source)

(defun deterred-reddit-load-dump (folder)
  "Load Reddit dump in FOLDER into DETERRED."
  (interactive
   (list
    (expand-file-name
     (read-file-name "Dump folder: " nil nil nil nil #'file-directory-p))))
  (let ((comments-file (concat folder "comments.csv"))
        (posts-file (concat folder "posts.csv"))
        (db (deterred-db--init)))
    (unless (file-exists-p comments-file)
      (user-error "\"comments.csv\" not found in folder"))
    (unless (file-exists-p posts-file)
      (user-error "\"posts.csv\" not found in folder"))
    (with-sqlite-transaction db
      (let* ((comments-data (cl-mapcar
                             (lambda (comment)
                               `((id . ,(alist-get 'id comment))
                                 (url . ,(alist-get 'permalink comment))
                                 (timestamp . ,(time-convert
                                                (encode-time
                                                 (parse-time-string
                                                  (alist-get 'date comment)))
                                                #'integer))
                                 (subreddit . ,(alist-get 'subreddit comment))
                                 (body . ,(alist-get 'body comment))))
                             (deterred-utils-csv-to-alist comments-file)))
             (posts-data (cl-mapcar
                          (lambda (comment)
                            `((id . ,(alist-get 'id comment))
                              (url . ,(alist-get 'permalink comment))
                              (timestamp . ,(time-convert
                                             (encode-time
                                              (parse-time-string
                                               (alist-get 'date comment)))
                                             #'integer))
                              (subreddit . ,(alist-get 'subreddit comment))
                              (body . ,(alist-get 'body comment))
                              (title . ,(alist-get 'title comment)) ))
                          (deterred-utils-read-csv-with-python posts-file))))
        (deterred-db-insert-unsafe
         db :table-name 'reddit_post
         :values posts-data
         :conflict-action 'do-update
         :conflict-attrs '(id))
        (deterred-db-insert-unsafe
         db :table-name 'reddit_comment
         :values comments-data
         :conflict-action 'do-update
         :conflict-attrs '(id))
        (deterred-db--mark-update-batch
         db '(reddit_post reddit_comment))))))

(defclass deterred-reddit (deterred-source)
  ((name :initform "Reddit"))
  "DETERRED source for reddit.")

(cl-defmethod deterred-source-range ((_source deterred-reddit) &optional db)
  "Get the data availability range for Mastodon.

DB is the sqlite database object.

Return a cons cell, with car as the start timestamp, and cdr as the
end timestamp."
  (let* ((db (or db (deterred-db--init)))
         (data (sqlite-select
                db "SELECT MIN(timestamp), MAX(timestamp)
                    FROM reddit_comment")))
    (cons (caar data) (cadar data))))

(cl-defmethod deterred-source-actions ((_source deterred-reddit) &optional callback)
  "Run an action for the reddit source.

Run CALLBACK when done."
  (deterred-source--actions-pick
   '(("Load Reddit dump folder" deterred-reddit-load-dump nil))
   callback))

(provide 'deterred-reddit)
;;; deterred-reddit.el ends here

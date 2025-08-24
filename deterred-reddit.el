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
(require 'deterred-format)
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
        (deterred-db-mark-updated-batch
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

(defun deterred-reddit--render-data (posts comments)
  (dolist (post (append posts comments))
    (magit-insert-section (deterred-reddit-post t)
      (insert
       (deterred-format
        (propertize
         (f
          (format-time-string deterred-dispatcher-time-format
                              (alist-get 'timestamp post))
          ": "
          (if-let (title (alist-get 'title post))
              (f "\"" title "\"")
            "comment")
          " on r/" (alist-get 'subreddit post))
         'face 'deterred-faces-section-heading-4)))
      (magit-insert-heading)
      (when-let ((body (alist-get 'body post)))
        (insert
         body "\n"
         (deterred-format
          (f-button "[Open]" (lambda (&rest _)
                               (browse-url (alist-get 'url post)))))
         "\n\n")))))

(cl-defmethod deterred-source-day-summary
  ((_source deterred-reddit) timestamp &optional db)
  (let* ((db (or db (deterred-db--init)))
         (posts (deterred-db-select-alist
                 db "SELECT * FROM reddit_post
                     WHERE timestamp BETWEEN ? AND ?"
                 (list timestamp (+ (* 60 60 24) timestamp))))
         (comments
          (deterred-db-select-alist
           db "SELECT * FROM reddit_comment
                     WHERE timestamp BETWEEN ? AND ?"
           (list timestamp (+ (* 60 60 24) timestamp))))
         (comment-subreddits
          (seq-uniq
           (mapcar (lambda (c) (format "r/%s" (alist-get 'subreddit c)))
                   comments))))
    (when (or posts comments)
      `((:short-description
         . ,(deterred-format
             (when posts
               (f (f-num (seq-length posts)) " posts"))
             (when comments
               (when posts
                 " and ")
               (f (f-num (seq-length comments)) " comments"
                  (if (> (seq-length comment-subreddits) 2)
                      (f " on " (seq-length comment-subreddits) "subreddits")
                    (f " on " (f-join comment-subreddits ", ")))))))
        (:long-description-fn
         . ,(lambda (&rest _) (deterred-reddit--render-data posts comments)))))))

(provide 'deterred-reddit)
;;; deterred-reddit.el ends here

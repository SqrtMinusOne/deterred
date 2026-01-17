;;; deterred-social.el --- Social media sources for DETERRED -*- lexical-binding: t -*-

;; Copyright (C) 2024-2025 Korytov Pavel

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

;; Combined social media sources: Reddit and Mastodon.

;;; Code:
(require 'cl-lib)
(require 'magit-section)
(require 'iso8601)
(require 'request)
(require 'uuidgen)

(require 'deterred-db)
(require 'deterred-utils)
(require 'deterred-format)
(require 'deterred-source)

;;; Mastodon

(defconst deterred-social-mastodon-uuid-namespace
  "6c4ea183-e81a-4e9d-bffc-11ed5aacb130")

(defun deterred-social-mastodon--process-posts (data &optional stop-id)
  "Add Mastodon post DATA into the DETERRED database.

If STOP-ID is encountered, stop."
  (let* ((db (deterred-db--init))
         (mention-accounts (make-hash-table :test 'equal))
         posts post-mentions max-id)
    (unless (seq-empty-p data)
      (with-sqlite-transaction db
        (cl-block post-loop
          (cl-mapc
           (lambda (datum)
             (let* ((server (url-host (url-generic-parse-url (alist-get 'uri datum))))
                    (post-id (uuidgen-3
                              deterred-social-mastodon-uuid-namespace (alist-get 'uri datum)))
                    (post
                     `((id . ,post-id)
                       (timestamp . ,(time-convert
                                      (encode-time (iso8601-parse
                                                    (alist-get 'created_at datum)))
                                      'integer))
                       (uri . ,(alist-get 'uri datum))
                       (server . ,server)
                       (replies_count . ,(alist-get 'replies_count datum))
                       (reblogs_count . ,(alist-get 'reblogs_count datum))
                       (favourites_count . ,(alist-get 'favourites_count datum))
                       (content . ,(alist-get 'content datum))
                       (application . ,(alist-get 'name (alist-get 'application datum)))
                       (is_reply . ,(if (alist-get 'in_reply_to_id datum) 1 0)))))
               (setq max-id (alist-get 'id datum))
               (when (and stop-id (equal max-id stop-id))
                 (cl-return-from post-loop))
               (push post posts)
               (cl-mapc
                (lambda (mention)
                  (let ((account-name (alist-get 'acct mention)))
                    (unless (string-match-p "@" account-name)
                      (setq account-name (concat account-name "@" server)))
                    (let* ((split-name (split-string account-name "@"))
                           (account-username (car split-name))
                           (account-server (cadr split-name))
                           (id (uuidgen-3
                                deterred-social-mastodon-uuid-namespace
                                account-name)))
                      (puthash account-name
                               `((id . ,id)
                                 (username . ,account-username)
                                 (server . ,account-server))
                               mention-accounts)
                      (push `((post_id . ,post-id)
                              (account_id . ,id))
                            post-mentions))))
                (alist-get 'mentions datum))))
           data))
        (when posts
          (deterred-db-insert-unsafe
           db :table-name 'mastodon_post
           :values posts
           :conflict-action 'do-update
           :conflict-attrs '(id))
          (deterred-db-insert-unsafe
           db :table-name 'mastodon_account
           :values (hash-table-values mention-accounts)
           :conflict-action 'do-nothing)
          (deterred-db-insert-unsafe
           db :table-name 'mastodon_post_mention
           :values post-mentions
           :conflict-action 'do-nothing)
          (deterred-db-mark-updated-batch
           db
           '(mastodon_post mastodon_account mastodon_post_mention)))))
    max-id))

(defun deterred-social-mastodon-sync (server account-id &optional max-id stop-id callback)
  "Load Mastodon posts into DETERRED.

SERVER is the server URL, ACCOUNT-ID is the identifier (not the
handle) of the poster.  MAX-ID is used for pagination.

STOP-ID is used to fetch up to a particular ID.

Call CALLBACK when done."
  (interactive
   (list
    (read-string "Server: " "https://mastodon.bsd.cafe")
    (read-string "Account ID: ")
    (read-string "Max ID (optional): ")))
  (let ((url (format "%s/api/v1/accounts/%s/statuses" server account-id))
        (max-id (if (string-empty-p max-id) nil max-id)))
    (request url
      :params `((exclude_reblogs . true)
                (exclude_replies . false)
                (limit . 40)
                ,@(when max-id
                    `((max_id . ,max-id))))
      :parser 'json-read
      :encoding 'utf-8
      :success
      (cl-function
       (lambda (&key data response &allow-other-keys)
         (setq max-id (deterred-social-mastodon--process-posts data stop-id))
         (let ((rate-limit-remaining (string-to-number
                                      (request-response-header
                                       response "x-ratelimit-remaining")))
               (rate-limit-reset (request-response-header
                                  response "x-ratelimit-reset")))
           (cond ((seq-empty-p data)
                  (message "Fininshed fetching posts.")
                  (when callback (funcall callback)))
                 ((eq rate-limit-remaining 0)
                  (message "Hit rate limit at max-id %s. Continue at %s"
                           max-id rate-limit-reset)
                  (when callback (funcall callback)))
                 ((and stop-id (equal stop-id max-id))
                  (message "Found stop-id, finished fetching posts.")
                  (when callback (funcall callback)))
                 (t
                  (message "Fetching posts, currently at %s, %s rate limit remaining"
                           max-id rate-limit-remaining)
                  (deterred-social-mastodon-sync server account-id max-id stop-id
                                                 callback))))))
      :error #'deterred-utils-on-request-error)))

;;; Reddit

(defun deterred-social-reddit-load-dump (folder)
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
                                                'integer))
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
                                             'integer))
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

;;; Rendering

(defun deterred-social--render-post (post)
  "Render a single social media POST (Reddit or Mastodon)."
  (let ((source (alist-get 'source post)))
    (magit-insert-section (deterred-social-post post t)
      (insert
       (propertize
        (pcase source
          ('mastodon
           (format "%s on %s via %s"
                   (format-time-string deterred-dispatcher-date-time-format
                                       (alist-get 'timestamp post))
                   (alist-get 'server post)
                   (alist-get 'application post)))
          ('reddit
           (deterred-format
            (f (format-time-string deterred-dispatcher-date-time-format
                                   (alist-get 'timestamp post))
               ": "
               (if-let (title (alist-get 'title post))
                   (f "\"" title "\"")
                 "comment")
               " on r/" (alist-get 'subreddit post)))))
        'face 'deterred-faces-section-heading-4))
      (magit-insert-heading)
      (pcase source
        ('mastodon
         (insert
          (string-trim
           (with-temp-buffer
             (shr-insert-document
              (with-temp-buffer
                (insert (alist-get 'content post))
                (libxml-parse-html-region)))
             (buffer-string)))
          "\n"))
        ('reddit
         (when-let ((body (alist-get 'body post)))
           (insert
            body "\n"
            (deterred-format
             (f-button "[Open]" (lambda (&rest _)
                                  (browse-url (alist-get 'url post)))))
            "\n\n")))))))

(defun deterred-social--render-data (mastodon-posts reddit-posts reddit-comments)
  "Render MASTODON-POSTS, REDDIT-POSTS and REDDIT-COMMENTS sorted by timestamp."
  (let ((all-posts
         (sort
          (append
           (mapcar (lambda (p) (cons '(source . mastodon) p)) mastodon-posts)
           (mapcar (lambda (p) (cons '(source . reddit) p)) reddit-posts)
           (mapcar (lambda (p) (cons '(source . reddit) p)) reddit-comments))
          (lambda (a b)
            (< (alist-get 'timestamp a) (alist-get 'timestamp b))))))
    (dolist (post all-posts)
      (deterred-social--render-post post))))

;;; Combined source

;;;###autoload
(defclass deterred-social (deterred-source)
  ((name :initform "Social")
   (mastodon-server :initarg :mastodon-server :initform nil)
   (mastodon-account-id :initarg :mastodon-account-id :initform nil))
  "DETERRED source for social media (Reddit and Mastodon).")

(cl-defmethod deterred-source-range ((_source deterred-social) &optional db)
  "Get the data availability range for social media.

DB is the sqlite database object.

Return a cons cell, with car as the start timestamp, and cdr as the
end timestamp."
  (let* ((db (or db (deterred-db--init)))
         (data (sqlite-select
                db "SELECT MIN(min_ts), MAX(max_ts) FROM (
                      SELECT MIN(timestamp) as min_ts, MAX(timestamp) as max_ts
                      FROM reddit_comment
                      UNION ALL
                      SELECT MIN(timestamp) as min_ts, MAX(timestamp) as max_ts
                      FROM mastodon_post
                    )")))
    (cons (caar data) (cadar data))))

(cl-defmethod deterred-source-range-detail ((_source deterred-social) &optional db)
  "Get the data availability range for each social platform.

DB is the sqlite database object.

Return a list of alists with :name, :start, :end for each platform."
  (let* ((db (or db (deterred-db--init)))
         (reddit-range (car (sqlite-select
                             db "SELECT MIN(timestamp), MAX(timestamp)
                                 FROM reddit_comment")))
         (mastodon-range (car (sqlite-select
                               db "SELECT MIN(timestamp), MAX(timestamp)
                                   FROM mastodon_post")))
         (result nil))
    (when (car mastodon-range)
      (push `((:name . "mastodon")
              (:start . ,(car mastodon-range))
              (:end . ,(cadr mastodon-range)))
            result))
    (when (car reddit-range)
      (push `((:name . "reddit")
              (:start . ,(car reddit-range))
              (:end . ,(cadr reddit-range)))
            result))
    result))

(cl-defmethod deterred-source-sync ((source deterred-social) &optional callback)
  "Sync DETERRED with Mastodon.

Call CALLBACK when done.

SOURCE is an instance of `deterred-social'."
  (let* ((db (deterred-db--init))
         (server (oref source mastodon-server))
         (account-id (oref source mastodon-account-id)))
    (if (and server account-id)
        (let* ((max-url (caar (sqlite-select
                               db (format "SELECT max(uri) FROM mastodon_post
                                           WHERE uri LIKE '%s%%'" server))))
               (stop-id (when max-url
                          (save-match-data
                            (string-match (rx "/statuses/" (group (* num))) max-url)
                            (match-string 1 max-url)))))
          (deterred-social-mastodon-sync server account-id nil stop-id callback))
      (message "Mastodon server/account-id not configured")
      (when callback (funcall callback)))))

(cl-defmethod deterred-source-actions ((_source deterred-social) &optional callback)
  "Run an action for the social source.

Run CALLBACK when done."
  (deterred-source--actions-pick
   '(("Load Reddit dump folder" deterred-social-reddit-load-dump nil))
   callback))

(cl-defmethod deterred-source-range-summary
  ((_source deterred-social) start end &optional db)
  "Make social media summary for [START, END].

DB is the sqlite database object."
  (let* ((db (or db (deterred-db--init)))
         ;; Reddit data
         (reddit-posts (deterred-db-select-alist
                        db "SELECT * FROM reddit_post
                            WHERE timestamp BETWEEN ? AND ?
                            ORDER BY timestamp ASC"
                        (list start end)))
         (reddit-comments
          (deterred-db-select-alist
           db "SELECT * FROM reddit_comment
               WHERE timestamp BETWEEN ? AND ?
               ORDER BY timestamp ASC"
           (list start end)))
         (reddit-comment-subreddits
          (seq-uniq
           (mapcar (lambda (c) (format "r/%s" (alist-get 'subreddit c)))
                   reddit-comments)))
         ;; Mastodon data
         (mastodon-posts (deterred-db-select-alist
                          db "SELECT * FROM mastodon_post
                              WHERE timestamp BETWEEN ? AND ? AND is_reply = 0"
                          (list start end)))
         (mastodon-unique-servers
          (seq-uniq (mapcar (lambda (post) (alist-get 'server post)) mastodon-posts)))
         ;; Build descriptions
         (has-reddit (or reddit-posts reddit-comments))
         (has-mastodon mastodon-posts))
    (when (or has-reddit has-mastodon)
      `((:short-description
         . ,(deterred-format
             ;; Reddit description
             (when has-reddit
               (f
                (when reddit-posts
                  (f (f-num (seq-length reddit-posts)) " reddit posts"
                     (when reddit-comments "; ")))
                (when reddit-comments
                  (f (f-num (seq-length reddit-comments)) " reddit comments"
                     (if (> (seq-length reddit-comment-subreddits) 2)
                         (f " on " (f-num (seq-length reddit-comment-subreddits)) " subreddits")
                       (f " on " (f-join reddit-comment-subreddits ", ")))))))
             ;; Separator
             (when (and has-reddit has-mastodon) "; ")
             ;; Mastodon description
             (when has-mastodon
               (f
                (f (f-num (seq-length mastodon-posts)) " mastodon posts"
                   (when (= (seq-length mastodon-unique-servers) 1)
                     (f " on " (car mastodon-unique-servers))))))))
        (:long-description-fn
         . ,(lambda (&rest _)
              (deterred-social--render-data
               mastodon-posts reddit-posts reddit-comments)))))))

(provide 'deterred-social)
;;; deterred-social.el ends here

;;; deterred-mastodon.el --- TODO -*- lexical-binding: t -*-

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

(defconst deterred-mastodon-uuid-namespace
  "6c4ea183-e81a-4e9d-bffc-11ed5aacb130")

(defun deterred-mastodon--process-posts (data)
  "Add Mastodon post DATA into the DETERRED database."
  (let* ((db (deterred-db--init))
         (mention-accounts (make-hash-table :test 'equal))
         posts post-mentions max-id)
    (unless (seq-empty-p data)
      (with-sqlite-transaction db
        (cl-mapc
         (lambda (datum)
           (let* ((server (url-host (url-generic-parse-url (alist-get 'uri datum))))
                  (post-id (uuidgen-3
                            deterred-mastodon-uuid-namespace (alist-get 'uri datum)))
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
             (push post posts)
             (setq max-id (alist-get 'id datum))
             (cl-mapc
              (lambda (mention)
                (let ((account-name (alist-get 'acct mention)))
                  (unless (string-match-p "@" account-name)
                    (setq account-name (concat account-name "@" server)))
                  (let* ((split-name (split-string account-name "@"))
                         (account-username (car split-name))
                         (account-server (cadr split-name))
                         (id (uuidgen-3
                              deterred-mastodon-uuid-namespace
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
         data)
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
        (deterred-db--mark-update-batch
         db
         '(mastodon_post mastodon_account mastodon_post_mention))))
    max-id))

(defun deterred-mastodon-sync (server account-id &optional max-id)
  "Load Mastodon posts into DETERRED.

SERVER is the server URL, ACCOUNT-ID is the identifier (not the
handle) of the poster.  MAX-ID is used for pagination."
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
         (setq max-id (deterred-mastodon--process-posts data))
         (let ((rate-limit-remaining (string-to-number
                                      (request-response-header
                                       response "x-ratelimit-remaining")))
               (rate-limit-reset (request-response-header
                                  response "x-ratelimit-reset")))
           (cond ((seq-empty-p data)
                  (message "Fininshed fetching posts."))
                 ((eq rate-limit-remaining 0)
                  (message "Hit rate limit at max-id %s. Continue at %s"
                           max-id rate-limit-reset))
                 (t
                  (message "Fetching posts, currently at %s, %s rate limit remaining"
                           max-id rate-limit-remaining)
                  (deterred-mastodon-sync server account-id max-id))))))
      :error (cl-function
              (lambda (&key error-thrown &allow-other-keys)
                (message "Error!: %S" error-thrown))))))

(provide 'deterred-mastodon)
;;; deterred-mastodon.el ends here

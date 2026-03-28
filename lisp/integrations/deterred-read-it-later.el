;;; deterred-read-it-later.el --- TODO -*- lexical-binding: t -*-

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
(require 'deterred-source)
(require 'deterred-db)
(require 'deterred-utils)
(require 'magit-section)
(require 'iso8601)
(require 'url-parse)
(require 'uuidgen)
(require 'request)

(defcustom deterred-read-it-later-readeck-url "http://localhost:8000/"
  "URL of the Readeck instance."
  :group 'deterred-sources
  :type 'string)

(defcustom deterred-read-it-later-readeck-token nil
  "Token for the Readeck instance.

Has to have the \"Bookmarks: Read Only\" role."
  :group 'deterred-sources
  :type 'string)

(defcustom detered-read-it-later-wallabag-url "http://localhost:8000"
  "URL for the Wallabag instance."
  :group 'deterred-sources
  :type 'string)

(defcustom deterred-read-it-later-wallabag-client-id nil
  "Client ID for wallabag."
  :group 'deterred-sources
  :type 'string)

(defcustom deterred-read-it-later-wallabag-client-secret nil
  "Client secret for wallabag."
  :group 'deterred-sources
  :type 'string)

(defcustom deterred-read-it-later-wallabag-username nil
  "Username for wallabag."
  :group 'deterred-sources
  :type 'string)

(defcustom deterred-read-it-later-wallabag-password nil
  "Password for wallabag."
  :group 'deterred-sources
  :type 'string)

(defconst deterred-read-it-later-uuid-namespace
  "44556cd0-f370-45a5-ac3a-bf8017a70bb0")

(defun deterred-read-it-later--id (id provider)
  "Get an unique UUID for ID and PROVIDER."
  (when (numberp id)
    (setq id (number-to-string id)))
  (uuidgen-3 deterred-read-it-later-uuid-namespace
             (concat id provider)))

(defun deterred-read-it-later--host (url)
  "Get host from URL."
  (let* ((parsed (url-generic-parse-url url))
         (host (string-replace "www." "" (url-host parsed))))
    host))

(defun deterred-read-it-later--readeck-list (callback &optional results page
                                                      start-timestamp)
  "List all read articles from Readeck.

Call CALLBACK with the results.  PAGE and RESULTS are the recursive
parameters.  START-TIMESTAMP is the optional start timestamp for
incremental sync.

The results are a list of alists suitable for
`deterred-read-it-later--store', which see for the structure."
  (unless page
    (setq page 0))
  (request (concat deterred-read-it-later-readeck-url
                   "api/bookmarks")
    :parser 'json-read
    :params `(("limit" . 30)
              ("offset" . ,(* page 30))
              ("sort" . "created")
              ("is_archived" . "true")
              ,@(when start-timestamp
                  `(("range_start" . ,start-timestamp))))
    :headers `(("Authorization"
                . ,(concat "Bearer " deterred-read-it-later-readeck-token)))
    :success
    (cl-function
     (lambda (&key data response &allow-other-keys)
       (let ((total-pages (string-to-number
                           (request-response-header
                            response "total-pages"))))
         (message "Parsing Readeck: %s/%s" page total-pages)
         (setq results
               (append
                results
                (cl-mapcar
                 (lambda (datum)
                   (let ((created-at (time-convert
                                      (encode-time
                                       (iso8601-parse (alist-get 'created datum)))
                                      'integer)))
                     (setq deterred-read-it-later--readeck-sync-timestamp
                           (max (or deterred-read-it-later--readeck-sync-timestamp 0)
                                created-at))
                     `((id . ,(deterred-read-it-later--id
                               (alist-get 'id datum)
                               "readeck"))
                       (href . ,(concat deterred-read-it-later-readeck-url
                                        "bookmarks/"
                                        (alist-get 'id datum)))
                       (url . ,(alist-get 'url datum))
                       (title . ,(alist-get 'title datum))
                       (host . ,(deterred-read-it-later--host
                                 (alist-get 'url datum)))
                       (created_at . ,created-at)
                       (read_at
                        . ,(time-convert
                            (encode-time
                             (iso8601-parse (alist-get 'updated datum)))
                            'integer))
                       (provider . "readeck"))))
                 data)))
         (if (>= total-pages page)
             (deterred-read-it-later--readeck-list callback results (1+ page)
                                                   start-timestamp)
           (setq deterred-read-it-later--readeck-sync-timestamp nil)
           (funcall callback results)))))
    :error #'deterred-utils-on-request-error))

(defun deterred-read-it-later--wallabag-authorize (callback)
  "Authorize in the Wallabag instance.

Call CALLBACK with the access token."
  (request (concat deterred-read-it-later-wallabag-url "oauth/v2/token")
    :type "POST"
    :parser 'json-read
    :headers '(("Content-Type" . "application/json"))
    :data (json-encode
           `(("grant_type" . "password")
             ("client_id" . ,deterred-read-it-later-wallabag-client-id)
             ("client_secret" . ,deterred-read-it-later-wallabag-client-secret)
             ("username" . ,deterred-read-it-later-wallabag-username)
             ("password" . ,deterred-read-it-later-wallabag-password)))
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (funcall callback (alist-get 'access_token data))))
    :error #'deterred-utils-on-request-error))

(defun deterred-read-it-later--wallabag-list (callback &optional token page results)
  "List all read articles from Wallabag.

Call CALLBACK with the results.  PAGE and RESULTS are the recursive
parameters; TOKEN is retrieved on the first pass.

The results are a list of alists suitable for
`deterred-read-it-later--store', which see for the structure."
  (if (not token)
      (deterred-read-it-later--wallabag-authorize
       (lambda (token)
         (deterred-read-it-later--wallabag-list callback token 1)))
    (request (concat deterred-read-it-later-wallabag-url "api/entries")
      :parser 'json-read
      :params `(("archive" . 1)
                ("sort" . "created")
                ("order" . "desc")
                ("page" . ,page)
                ("perPage" . 30)
                ("detail" . "metadata"))
      :headers `(("Authorization"
                  . ,(concat "Bearer " token)))
      :success
      (cl-function
       (lambda (&key data &allow-other-keys)
         (let ((total-pages (alist-get 'pages data)))
           (message "Parsing Wallabag: %s/%s pages" page total-pages)
           (setq results
                 (append results
                         (cl-mapcar
                          (lambda (datum)
                            `((id . ,(deterred-read-it-later--id
                                      (alist-get 'id datum)
                                      "wallabag"))
                              (href . ,(concat deterred-read-it-later-readeck-url
                                               "view/"
                                               (number-to-string
                                                (alist-get 'id datum))))
                              (url . ,(alist-get 'url datum))
                              (title . ,(alist-get 'title datum))
                              (host . ,(deterred-read-it-later--host
                                        (alist-get 'url datum)))
                              (created_at
                               . ,(time-convert
                                   (encode-time
                                    (iso8601-parse (alist-get 'created_at datum)))
                                   'integer))
                              (read_at
                               . ,(time-convert
                                   (encode-time
                                    (iso8601-parse (alist-get 'archived_at datum)))
                                   'integer))
                              (provider . "wallabag")))
                          (alist-get 'items (alist-get '_embedded data)))))
           (if (< page total-pages)
               (deterred-read-it-later--wallabag-list
                callback token (1+ page) results)
             (funcall callback results)))))
      :error #'deterred-utils-on-request-error)))

(defun deterred-read-it-later--store (results)
  "Store RESULTS in the database.

RESULTS should be a list of alists with the following keys:
- id: unique article identifier (UUID)
- href: link to the article in the read-it-later app
- url: original article URL
- title: article title
- host: hostname extracted from the URL
- created_at: article creation timestamp (UNIX epoch)
- read_at: article read timestamp (UNIX epoch)
- provider: read-it-later provider name (e.g., \"readeck\", \"wallabag\")

This function first inserts unique hosts into read_it_later_host table
\(with language as NULL\), then inserts the articles into
read_it_later_article table."
  (let ((db (deterred-db--init)))
    (with-sqlite-transaction db
      ;; First, insert unique hosts into read_it_later_host table
      (let ((hosts (seq-uniq (mapcar (lambda (result)
                                       `((host . ,(alist-get 'host result))
                                         (language . nil)))
                                     results))))
        (deterred-db-insert-unsafe
         db
         :table-name 'read_it_later_host
         :values hosts
         :conflict-action 'do-nothing))
      ;; Then insert articles
      (deterred-db-insert-unsafe
       db
       :table-name 'read_it_later_article
       :values results
       :conflict-action 'do-nothing)
      (deterred-db-mark-updated-batch
       db '(read_it_later_article read_it_later_host)))))

(defun deterred-read-it-later--get-start-timestamp (provider)
  "Get last timestamp saved for PROVIDER."
  (let ((db (deterred-db--init)))
    (caar (deterred-db-select-template
           db
           "SELECT max(created_at) FROM read_it_later_article
WHERE provider = :provider"
           `((:provider . ,provider))))))

(defvar deterred-read-it-later--readeck-sync-timestamp nil
  "Timestamp used to resume sync it case it fails.")

(defun deterred-read-it-later-readeck-sync (&optional callback)
  "Sync Readeck with DETERRED.

Call CALLBACK when done."
  (interactive)
  (deterred-utils-assert-var-set deterred-read-it-later-readeck-url)
  (deterred-utils-assert-var-set deterred-read-it-later-readeck-token)
  (deterred-read-it-later--readeck-list
   (lambda (data)
     (deterred-read-it-later--store data)
     (when callback (funcall callback)))
   nil nil
   deterred-read-it-later--readeck-sync-timestamp))

(defun deterred-read-it-later-wallabag-sync (&optional callback)
  "Sync Wallabag with DETERRED.

Call CALLBACK when done."
  (interactive)
  (deterred-utils-assert-var-set deterred-read-it-later-wallabag-username)
  (deterred-utils-assert-var-set deterred-read-it-later-wallabag-password)
  (deterred-utils-assert-var-set deterred-read-it-later-wallabag-client-id)
  (deterred-utils-assert-var-set deterred-read-it-later-wallabag-client-secret)
  (deterred-read-it-later--wallabag-list
   (lambda (data)
     (deterred-read-it-later--store data)
     (when callback (funcall callback)))))

;;;###autoload
(defclass deterred-read-it-later (deterred-source)
  ((name :initform "Read It Later")
   (sources :initarg :sources :initform '(readeck)))
  "DETERRED source for read-it-later apps.")

(cl-defmethod deterred-source-range ((_source deterred-source) &optional db)
  "Get the data availability range for Mastodon.

DB is the sqlite database object.

Return a cons cell, with car as the start timestamp, and cdr as the
end timestamp."
  (let* ((db (or db (deterred-db--init)))
         (data (sqlite-select
                db "SELECT MIN(created_at), MAX(created_at)
                    FROM read_it_later_article")))
    (cons (caar data) (cadar data))))

(cl-defmethod deterred-source-sync ((source deterred-read-it-later)
                                    &optional callback sources recursed)
  "Sync DETERRED with read-it-later apps.

Call CALLBACK when done.  SOURCES and RECURSED are the recursive
parameters.

SOURCE is the instance of `deterred-read-it-later'."
  (unless recursed
    (setq sources (oref source sources)))
  (if (not sources)
      (when callback (funcall callback))
    (pcase (car sources)
      ('wallabag (deterred-read-it-later-wallabag-sync
                  (lambda ()
                    (deterred-source-sync source callback (cdr sources) t))))
      ('readeck (deterred-read-it-later-readeck-sync
                 (lambda ()
                   (deterred-source-sync source callback (cdr sources) t)))))))

(cl-defmethod deterred-source-day-summary
  ((_source deterred-read-it-later) timestamp &optional db)
  (let* ((db (or db (deterred-db--init)))
         (articles (deterred-db-select-alist
                    db "SELECT * FROM read_it_later_article
                        WHERE read_at BETWEEN ? AND ?"
                    (list timestamp (+ (* 60 60 24) timestamp)))))
    (when articles
      `((:short-description
         . ,(deterred-format
             (f-num (seq-length articles))
             " articles read"))
        (:long-description
         . ,(deterred-format
             (f-mapconcat
              (f (format-time-string deterred-dispatcher-time-format
                                     (f-acc "iter->'timestamp"))
                 ": \"" (f-button
                         (f-acc "iter->'title")
                         (lambda (&rest _)
                           (browse-url (alist-get iter 'href))))
                 "\" on " (f-button
                           (f-acc "iter->'host")
                           (lambda (&rest _)
                             (browse-url (alist-get iter 'url)))))
              articles)))))))

(defun deterred-read-it-later--render-articles-by-host (articles-by-host)
  "Render read-it-later ARTICLES-BY-HOST grouped by host.

ARTICLES-BY-HOST is a list of (host . articles-list) pairs."
  (dolist (host-group (seq-sort-by
                       (lambda (group) (length (cdr group)))
                       #'>
                       articles-by-host))
    (let ((host (car host-group))
          (articles (cdr host-group)))
      (magit-insert-section (deterred-read-it-later-host t t)
        (insert
         (propertize
          (format "%s (%d article%s)"
                  host
                  (length articles)
                  (if (= (length articles) 1) "" "s"))
          'face 'deterred-faces-section-heading-3))
        (magit-insert-heading)
        (dolist (article articles)
          (magit-insert-section (deterred-read-it-later-article t t)
            (insert
             (propertize
              (alist-get 'title article)
              'face 'deterred-faces-section-heading-4
              'mouse-face 'highlight
              'help-echo "Click to open article"
              'keymap (let ((map (make-sparse-keymap)))
                        (define-key map [mouse-1]
                                    (lambda ()
                                      (interactive)
                                      (browse-url (alist-get 'href article))))
                        (define-key map (kbd "RET")
                                    (lambda ()
                                      (interactive)
                                      (browse-url (alist-get 'href article))))
                        map)))
            (magit-insert-heading)))))))

(cl-defmethod deterred-source-range-summary
  ((_source deterred-read-it-later) start end &optional db)
  "Make read-it-later summary for [START, END].

DB is the sqlite database object."
  (let* ((db (or db (deterred-db--init)))
         (articles
          (deterred-db-select-alist
           db "SELECT * FROM read_it_later_article
               WHERE read_at BETWEEN ? AND ?
               ORDER BY host, read_at DESC"
           (list start end)))
         (articles-by-host (seq-group-by (lambda (art) (alist-get 'host art)) articles))
         (host-count (length articles-by-host)))
    (when articles
      `((:short-description
         . ,(format "%d host%s (%d article%s)"
                    host-count
                    (if (= host-count 1) "" "s")
                    (length articles)
                    (if (= (length articles) 1) "" "s")))
        (:long-description-fn
         . ,(lambda (&rest _)
              (deterred-read-it-later--render-articles-by-host articles-by-host)))))))

(cl-defmethod deterred-source-events
  ((_source deterred-read-it-later) start end &optional params db)
  "Return read-it-later article events for [START, END].

PARAMS may contain the same filters as the read-it-later dashboard,
namely `:start-date', `:end-date', `:hosts' and `:providers'.  The
third event field is the hostname, so default grouping is by host.

DB is the sqlite database object."
  (let ((db (or db (deterred-db--init))))
    (deterred-db-select-template
     db
     "SELECT rila.read_at, null, rila.host
FROM read_it_later_article rila
WHERE rila.read_at BETWEEN :start AND :end
  [[AND rila.read_at >= :start-date]]
  [[AND rila.read_at <= :end-date]]
  [[AND rila.host IN :hosts]]
  [[AND rila.provider IN :providers]]
ORDER BY rila.read_at"
     (append params `((:start . ,start) (:end . ,end))))))

(declare-function deterred-dashboard-read-it-later "deterred-dashboard-read-it-later")

(cl-defmethod deterred-source-default-dashboard ((_source deterred-read-it-later))
  "Return the default dashboard for read-it-later."
  (deterred-dashboard-read-it-later))

(provide 'deterred-read-it-later)
;;; deterred-read-it-later.el ends here

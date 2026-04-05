;;; deterred-messengers.el --- TODO -*- lexical-binding: t -*-

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
(require 'cl-lib)
(require 'uuidgen)
(require 'dom)
(require 'iso8601)

(require 'deterred-db)
(require 'deterred-source)
(require 'deterred-utils)
(require 'deterred-messengers-telega-export)

;; Telega functions (optional dependency)
(declare-function telega-user-get "telega-user")
(declare-function telega-user-title "telega-user")
(declare-function telega-chat-get "telega-chat")
(declare-function telega-chat-title "telega-chat")
(declare-function telega--searchChatMessages "telega-tdlib")
(declare-function telega--getChats "telega-tdlib")
(defvar telega--me-id)

(defconst deterred-messengers-uuid-namespace
  "d9536b80-3213-4321-b37f-ebf1b558a530")

(defconst deterred-messengers-messenger-list
  '("telegram" "vk" "discord")
  "List of supported messenger names.")

(defcustom deterred-messengers-my-id nil
  "The user's UUID."
  :type 'string
  :group 'deterred)

(defcustom deterred-messengers-vk-timezone-offset -10800
  "Timezone offset in seconds to convert VK timestamps to UTC.

VK exports times in MSK (UTC+3), so the default is -10800 seconds (-3 hours)
to convert to UTC."
  :type 'integer
  :group 'deterred)

(defun deterred-messengers--id (messenger &rest ids)
  "Convert MESSENGER IDS into a unique UUID.

MESSENGER is a string like \"telegram\" or \"vk\".
This is used to maintain cross-messenger uniqueness of the
identifiers."
  (uuidgen-3 deterred-messengers-uuid-namespace
             (string-join (append
                           (list messenger)
                           (mapcar (lambda (id)
                                     (if (numberp id)
                                         (number-to-string id)
                                       id))
                                   ids)
                           nil))))

(defun deterred-messengers--load-users-by-messenger (db messenger)
  "Load existing users from DB for MESSENGER.

Returns a hash table mapping messenger_id to user UUID."
  (let ((users (make-hash-table :test #'equal))
        (column (intern (concat messenger "_id")))
        (rows (deterred-db-select-alist
               db
               (format "SELECT id, %s_id FROM messenger_user WHERE %s_id IS NOT NULL"
                       messenger messenger))))
    (dolist (row rows)
      (puthash (alist-get column row) (alist-get 'id row) users))
    users))

(defun deterred-messengers--load-chats-by-messenger (db messenger)
  "Load existing chats from DB for MESSENGER.

Returns a hash table mapping messenger_id to chat UUID."
  (let ((chats (make-hash-table :test #'equal))
        (column (intern (concat messenger "_id")))
        (rows (deterred-db-select-alist
               db
               (format "SELECT id, %s_id FROM messenger_chat WHERE %s_id IS NOT NULL"
                       messenger messenger))))
    (dolist (row rows)
      (puthash (alist-get column row) (alist-get 'id row) chats))
    chats))

(defun deterred-messengers--get-or-create-user (user-cache messenger messenger-id name)
  "Get or create a user UUID for MESSENGER-ID.

USER-CACHE is a hash table from
`deterred-messengers--load-users-by-messenger'.  If the user doesn't
exist, creates a new random UUID and adds it to the cache.

MESSENGER is a string.

Returns an alist suitable for database insertion with keys:
- id - the UUID (random v4 for new users, existing UUID for known
   users)
- <messenger>_id - the messenger-specific ID (e.g., telegram_id,
  vk_id)
- name - the user's display NAME."
  (let ((uuid (gethash messenger-id user-cache)))
    (unless uuid
      (setq uuid (uuidgen-4))
      (puthash messenger-id uuid user-cache))
    `((id . ,uuid)
      (,(intern (concat messenger "_id")) . ,messenger-id)
      (name . ,name))))

(defun deterred-messengers--get-or-create-chat
    (chat-cache messenger messenger-id name type &optional target-user-id)
  "Get or create a chat UUID for MESSENGER-ID.

CHAT-CACHE is a hash table from
`deterred-messengers--load-chats-by-messenger'.  If the chat doesn't
exist, creates a new random UUID and adds it to the cache.

MESSENGER is a string.

Returns an alist suitable for database insertion with keys:
- id - the UUID (random v4 for new chats, existing UUID for known
  chats)
- <messenger>_id - the messenger-specific ID (e.g., telegram_id, vk_id)
- name - the chat's display NAME
- type - either \"personal_chat\" or \"group\"
- target_user_id - (optional) for personal chats, the UUID of the other user.

NAME and TARGET-USER-ID are just added into the resuling alist."
  (let ((uuid (gethash messenger-id chat-cache)))
    (unless uuid
      (setq uuid (uuidgen-4))
      (puthash messenger-id uuid chat-cache))
    `((id . ,uuid)
      (,(intern (concat messenger "_id")) . ,messenger-id)
      (name . ,name)
      (type . ,type)
      ,@(when target-user-id
          `((target_user_id . ,target-user-id))))))

(defun deterred-messengers--telegram-process-chat
    (user-cache chat-cache chat my-telegram-id)
  "Process Telegram CHAT and return data for insertion.

CHAT is an object from Telegram's JSON dump.  USER-CACHE and
CHAT-CACHE are hash tables from
`deterred-messengers--load-users-by-messenger' and
`deterred-messengers--load-chats-by-messenger'.  MY-TELEGRAM-ID is the
Telegram ID of the user who made the dump.

Returns an alist with keys:
- users - hash table of user alists (keyed by user UUID)
- chats - hash table of chat alists (keyed by chat UUID)
- messages - list of message alists for database insertion

Note: We create the chat after processing messages because for
personal chats we need to determine the target_user_id and the proper
chat name (the other person's name) from the messages."
  (let* ((type (pcase (alist-get 'type chat)
                 ("personal_chat" "personal_chat")
                 (_ "group")))
         (telegram-chat-id (alist-get 'id chat))
         (chat-name (alist-get 'name chat))
         (found-users (make-hash-table :test #'equal))
         (found-chats (make-hash-table :test #'equal))
         messages
         target-user-id)

    ;; Process all messages
    (cl-mapc
     (lambda (message)
       (when (equal (alist-get 'type message) "message")
         (let* ((sender-telegram-id
                 (string-to-number
                  (string-replace "user" "" (alist-get 'from_id message))))
                (sender-name (alist-get 'from message))
                (user-data (deterred-messengers--get-or-create-user
                            user-cache "telegram" sender-telegram-id sender-name))
                (sender-uuid (alist-get 'id user-data))
                (timestamp (string-to-number (alist-get 'date_unixtime message)))
                (msg-id (deterred-messengers--id
                         "telegram" telegram-chat-id (alist-get 'id message)))
                (telegram-msg-id (alist-get 'id message))
                (content (if (stringp (alist-get 'text message))
                             (alist-get 'text message)
                           (mapconcat
                            (lambda (val)
                              (if (stringp val) val (alist-get 'text val)))
                            (alist-get 'text message))))
                (is-attachment (if (null (alist-get 'file message)) 0 1))
                (is-outgoing (equal my-telegram-id sender-telegram-id)))

           ;; Store user
           (puthash sender-uuid user-data found-users)

           ;; For personal chats, use the other person's name and set target user
           (when (and (equal type "personal_chat") (not is-outgoing))
             (setq chat-name sender-name)
             (setq target-user-id sender-uuid))

           ;; Add message (chat_id will be set later)
           (push `((id . ,msg-id)
                   (telegram_id . ,telegram-msg-id)
                   (sender_id . ,sender-uuid)
                   (chat_id . ,nil)
                   (content . ,content)
                   (timestamp . ,timestamp)
                   (is_attachment . ,is-attachment)
                   (messenger . "telegram"))
                 messages))))
     (alist-get 'messages chat))

    (when (> (seq-length messages) 0)
      ;; Create chat (now that we have target-user-id and chat-name for personal chats)
      (let* ((chat-data (deterred-messengers--get-or-create-chat
                         chat-cache "telegram" telegram-chat-id
                         chat-name type target-user-id))
             (chat-uuid (alist-get 'id chat-data)))
        (puthash chat-uuid chat-data found-chats)

        ;; Update message chat_ids
        (setq messages
              (mapcar (lambda (msg)
                        (setf (alist-get 'chat_id msg) chat-uuid)
                        msg)
                      messages))
        `((users . ,found-users)
          (chats . ,found-chats)
          (messages . ,messages))))))

(defun deterred-messengers-load-telegram-json (file)
  "Load Telegram export FILE into DETERRED."
  (interactive
   (list
    (read-file-name "JSON file: " nil nil nil nil
                    (lambda (f)
                      (or (file-directory-p f)
                          (string-match-p (rx ".json" eos) f))))))
  (let* ((data (json-read-file file))
         (my-telegram-id (alist-get 'user_id (alist-get 'personal_information data)))
         (db (deterred-db--init))
         (user-cache (deterred-messengers--load-users-by-messenger db "telegram"))
         (chat-cache (deterred-messengers--load-chats-by-messenger db "telegram"))
         (all-users-ht (make-hash-table :test #'equal))
         (all-chats-ht (make-hash-table :test #'equal))
         all-messages)
    (with-sqlite-transaction db
      (let ((total (seq-length (alist-get 'list (alist-get 'chats data)))))
        (cl-loop for chat across (alist-get 'list (alist-get 'chats data))
                 for i from 0
                 for result = (deterred-messengers--telegram-process-chat
                               user-cache chat-cache chat my-telegram-id)
                 when result
                 do (deterred-utils-merge-hashes all-users-ht (alist-get 'users result))
                 and do (deterred-utils-merge-hashes all-chats-ht (alist-get 'chats result))
                 and do (push (alist-get 'messages result) all-messages)
                 do (message "Processed %s/%s chats" i total)))

      (let ((users (hash-table-values all-users-ht))
            (chats (hash-table-values all-chats-ht))
            (messages (apply #'append all-messages)))
        (when users
          (deterred-db-insert-unsafe
           db :table-name 'messenger_user
           :values users
           :conflict-action 'do-update
           :conflict-attrs '(id)))
        (when chats
          (deterred-db-insert-unsafe
           db :table-name 'messenger_chat
           :values chats
           :attrs '(id telegram_id name type target_user_id vk_id)
           :conflict-action 'do-update
           :conflict-attrs '(id)))
        (when messages
          (deterred-db-insert-unsafe
           db :table-name 'messenger_message
           :values messages
           :conflict-action 'do-update
           :conflict-attrs '(id))))

      (deterred-db-mark-updated-batch
       db
       '(messenger_chat messenger_user messenger_message)))))

(defun deterred-messengers--parse-vk-date (date-str)
  "Parse VK DATE-STR and return (is-edited . timestamp).

Expected format: '8:17:38 pm on 10 May 2020' or with ' (edited)' suffix.
Applies `deterred-messengers-vk-timezone-offset' to convert to UTC."
  (let ((is-edited nil))
    ;; Check for edited marker
    (when (string-suffix-p " (edited)" date-str)
      (setq is-edited t)
      (setq date-str (substring date-str 0 -9)))

    ;; Parse format: "8:17:38 pm on 10 May 2020"
    (if (string-match (rx (group (+ digit)) ":"
                          (group (+ digit)) ":"
                          (group (+ digit)) " "
                          (group (| "am" "pm")) " on "
                          (group (+ digit)) " "
                          (group (+ alpha)) " "
                          (group (+ digit)))
                      date-str)
        (let* ((hour (string-to-number (match-string 1 date-str)))
               (minute (string-to-number (match-string 2 date-str)))
               (second (string-to-number (match-string 3 date-str)))
               (ampm (match-string 4 date-str))
               (day (string-to-number (match-string 5 date-str)))
               (month-str (match-string 6 date-str))
               (year (string-to-number (match-string 7 date-str)))
               ;; Convert 12-hour to 24-hour format
               (hour-24 (cond
                         ((and (equal ampm "am") (= hour 12)) 0)
                         ((and (equal ampm "pm") (/= hour 12)) (+ hour 12))
                         (t hour)))
               (timestamp (+ (floor (float-time
                                     (date-to-time
                                      (format "%d %s %d %02d:%02d:%02d"
                                              day month-str year
                                              hour-24 minute second))))
                             deterred-messengers-vk-timezone-offset)))
          (cons is-edited timestamp))
      (cons nil nil))))

(defun deterred-messengers--parse-vk-file (file-path)
  "Parse VK HTML export FILE-PATH.

Returns an alist with keys:
  chat_name - name extracted from page header
  messages - list of message alists with keys: author, vk_id, content, timestamp, is_edited
           vk_id is the numeric VK user ID extracted from the author link, or nil for \"You\""
  (let ((dom (with-temp-buffer
               (insert-file-contents file-path)
               (libxml-parse-html-region (point-min) (point-max))))
        messages
        chat-name)
    (when-let* ((body (dom-by-tag dom 'body))
                (page-content (dom-by-class body "page_content page_block")))
      ;; Extract chat name from page header (the last ui_crumb)
      (when-let* ((h2 (dom-by-tag page-content 'h2))
                  (header-inner (dom-by-class h2 "_header_inner"))
                  (ui-crumbs (dom-by-class header-inner "ui_crumb")))
        (let ((crumbs-list (if (listp (car ui-crumbs)) ui-crumbs (list ui-crumbs))))
          (setq chat-name (string-trim (dom-texts (car (last crumbs-list)))))))

      (setq my/test (dom-by-class page-content "wrap_page_content"))

      ;; Parse messages
      (when-let ((wrap-content (dom-by-class page-content "wrap_page_content"))
                 (items (dom-by-class wrap-content (rx bos "item" eos))))
        (dolist (item (if (listp (car items)) items (list items)))
          (when-let* ((item-main-list (dom-by-class item "item__main"))
                      (item-main (car item-main-list))
                      (message-div-list (dom-by-class item-main "message"))
                      (message-div (car message-div-list))
                      (message-header-list (dom-by-class message-div "message__header"))
                      (message-header (car message-header-list))
                      (header-text (dom-texts message-header)))
            ;; Parse header: "Author, at TIME" or "<a href...>Author</a>, at TIME"
            (when (string-match (rx (group (+? any)) ", at " (group (+ any))) header-text)
              (let* ((author (string-trim (match-string 1 header-text)))
                     (date-str (match-string 2 header-text))
                     (date-info (deterred-messengers--parse-vk-date date-str))
                     (is-edited (car date-info))
                     (timestamp (cdr date-info))
                     ;; Extract VK ID from author link if present
                     (author-link (dom-by-tag message-header 'a))
                     (vk-id (when author-link
                              (let ((href (dom-attr author-link 'href)))
                                (when (and href (string-match (rx "vk.com/id" (group (+ digit))) href))
                                  (match-string 1 href)))))
                     ;; Find the content div - it's a direct child div with no class
                     (content-div (seq-find
                                   (lambda (el)
                                     (and (listp el)
                                          (eq (dom-tag el) 'div)
                                          (or (null (dom-attr el 'class))
                                              (equal (dom-attr el 'class) ""))))
                                   (dom-children message-div)))
                     (message-text (when content-div
                                     (string-trim (dom-texts content-div)))))
                (when timestamp
                  (push `((author . ,author)
                          (vk_id . ,vk-id)
                          (content . ,(or message-text ""))
                          (timestamp . ,timestamp)
                          (is_edited . ,(if is-edited 1 0)))
                        messages))))))))
    `((chat_name . ,chat-name)
      (messages . ,(nreverse messages)))))

(defun deterred-messengers--parse-vk-directory (directory)
  "Parse all VK HTML files in DIRECTORY.

Returns an alist with keys:
- vk_id - the directory name
- chat_name - name extracted from page headers (should be same across all files)
- messages - list of message alists from `deterred-messengers--parse-vk-file'."
  (let* ((vk-id (file-name-nondirectory directory))
         (html-files (directory-files directory t (rx ".html" eos)))
         all-messages
         chat-name)
    (dolist (file html-files)
      (let ((file-data (deterred-messengers--parse-vk-file file)))
        (unless chat-name
          (setq chat-name (alist-get 'chat_name file-data)))
        (dolist (msg (alist-get 'messages file-data))
          (push msg all-messages))))
    `((vk_id . ,vk-id)
      (chat_name . ,chat-name)
      (messages . ,(nreverse all-messages)))))

(defun deterred-messengers--vk-process-directory (user-cache chat-cache directory)
  "Process VK export DIRECTORY and return data for insertion.

USER-CACHE and CHAT-CACHE are hash tables from
`deterred-messengers--load-users-by-messenger' and
`deterred-messengers--load-chats-by-messenger'.

Returns an alist with keys:
- users - hash table of user alists (keyed by user UUID)
- chats - hash table of chat alists (keyed by chat UUID)
- messages - list of message alists for database insertion

Note: We create the chat after processing messages because for personal
chats we need to determine the target_user_id from the messages."
  (let* ((parsed (deterred-messengers--parse-vk-directory directory))
         (vk-id (alist-get 'vk_id parsed))
         (chat-name (alist-get 'chat_name parsed))
         (messages (alist-get 'messages parsed))
         (found-users (make-hash-table :test #'equal))
         (found-chats (make-hash-table :test #'equal))
         (my-name "You")
         (my-user-data (deterred-messengers--get-or-create-user
                        user-cache "vk" my-name my-name))
         (my-uuid (alist-get 'id my-user-data))
         is-group
         target-user-id
         processed-messages)

    (when (and chat-name messages)
      ;; Store my user
      (puthash my-uuid my-user-data found-users)

      ;; Determine chat type: if any message author equals chat-name, it's personal
      (setq is-group t)
      (dolist (msg messages)
        (when (equal (alist-get 'author msg) chat-name)
          (setq is-group nil)))

      ;; Process messages
      (dolist (msg messages)
        (let* ((author (alist-get 'author msg))
               (is-outgoing (equal author my-name))
               (msg-vk-id (alist-get 'vk_id msg))
               (sender-id (if is-outgoing
                              my-uuid
                            ;; For VK, use the message's vk_id from the author link
                            (let ((user-data (deterred-messengers--get-or-create-user
                                              user-cache "vk" msg-vk-id author)))
                              (puthash (alist-get 'id user-data) user-data found-users)
                              (unless is-group
                                (setq target-user-id (alist-get 'id user-data)))
                              (alist-get 'id user-data))))
               (msg-id (deterred-messengers--id
                        "vk" vk-id (alist-get 'timestamp msg)))
               (content (alist-get 'content msg))
               (timestamp (alist-get 'timestamp msg)))

          ;; Add message (chat_id will be set later)
          (push `((id . ,msg-id)
                  (sender_id . ,sender-id)
                  (chat_id . ,nil)
                  (content . ,content)
                  (timestamp . ,timestamp)
                  (is_attachment . 0)
                  (messenger . "vk"))
                processed-messages)))

      ;; Create chat (now that we have target-user-id for personal chats)
      (let* ((chat-data (deterred-messengers--get-or-create-chat
                         chat-cache "vk" vk-id
                         chat-name
                         (if is-group "group" "personal_chat")
                         target-user-id))
             (chat-uuid (alist-get 'id chat-data)))

        ;; Store chat
        (puthash chat-uuid chat-data found-chats)

        ;; Update message chat_ids
        (setq processed-messages
              (mapcar (lambda (msg)
                        (setf (alist-get 'chat_id msg) chat-uuid)
                        msg)
                      processed-messages))

        `((users . ,found-users)
          (chats . ,found-chats)
          (messages . ,processed-messages))))))

(defun deterred-messengers-load-vk-html (directory)
  "Load VK export DIRECTORY into DETERRED.

DIRECTORY should contain the 'messages' folder from a VK export."
  (interactive "DVK export directory: ")
  (let* ((messages-dir (expand-file-name "messages" directory))
         (db (deterred-db--init))
         (user-cache (deterred-messengers--load-users-by-messenger db "vk"))
         (chat-cache (deterred-messengers--load-chats-by-messenger db "vk"))
         (subdirs (seq-filter
                   #'file-directory-p
                   (directory-files messages-dir t "^[^.]")))
         (all-users-ht (make-hash-table :test #'equal))
         (all-chats-ht (make-hash-table :test #'equal))
         all-messages)
    (unless (file-directory-p messages-dir)
      (user-error "Directory %s does not contain a 'messages' folder" directory))

    (with-sqlite-transaction db
      (let ((total (length subdirs)))
        (cl-loop for subdir in subdirs
                 for i from 1
                 for result = (deterred-messengers--vk-process-directory
                               user-cache chat-cache subdir)
                 when result
                 do (deterred-utils-merge-hashes all-users-ht (alist-get 'users result))
                 and do (deterred-utils-merge-hashes all-chats-ht (alist-get 'chats result))
                 and do (push (alist-get 'messages result) all-messages)
                 do (message "Processed %s/%s chats" i total)))

      ;; Insert all data
      (let ((users (hash-table-values all-users-ht))
            (chats (hash-table-values all-chats-ht))
            (messages (apply #'append all-messages)))
        (when users
          (deterred-db-insert-unsafe
           db :table-name 'messenger_user
           :values users
           :conflict-action 'do-update
           :conflict-attrs '(id)))
        (when chats
          (deterred-db-insert-unsafe
           db :table-name 'messenger_chat
           :values chats
           :conflict-action 'do-update
           :conflict-attrs '(id)))
        (when messages
          (deterred-db-insert-unsafe
           db :table-name 'messenger_message
           :values messages
           :conflict-action 'do-update
           :conflict-attrs '(id))))
      (deterred-db-mark-updated-batch
       db
       '(messenger_chat messenger_user messenger_message)))))

;;;###autoload
(defclass deterred-messengers (deterred-source)
  ((name :initform "Messengers"))
  "DETERRED source for messengers.")

(cl-defmethod deterred-source-range ((_source deterred-messengers) &optional db)
  "Get the data availability range for Mastodon.

DB is the sqlite database object.

Return a cons cell, with car as the start timestamp, and cdr as the
end timestamp."
  (let* ((db (or db (deterred-db--init)))
         (data (sqlite-select
                db "SELECT MIN(timestamp), MAX(timestamp)
                    FROM messenger_message")))
    (cons (caar data) (cadar data))))

(cl-defmethod deterred-source-range-detail ((_source deterred-messengers) &optional db)
  "Get the data availability range for each messenger.

DB is the sqlite database object.

Return a list of alists with :name, :start, :end for each messenger."
  (let* ((db (or db (deterred-db--init)))
         (data (deterred-db-select-alist
                db "SELECT messenger, MIN(timestamp) as start, MAX(timestamp) as end
                    FROM messenger_message
                    GROUP BY messenger
                    ORDER BY messenger")))
    (mapcar (lambda (row)
              `((:name . ,(alist-get 'messenger row))
                (:start . ,(alist-get 'start row))
                (:end . ,(alist-get 'end row))))
            data)))

(declare-function
 deterred-messengers-categories-process-all-chats "deterred-messengers-categories")
(declare-function deterred-messengers-chains-compute "deterred-messengers-chains")
(declare-function
 deterred-messengers-chains-compute-hostname "deterred-messengers-chains")

(cl-defmethod deterred-source-actions ((_source deterred-messengers) &optional callback)
  "Run an action for the messengers source.

Run CALLBACK when done."
  (deterred-source--actions-pick
   '(("Load Telegram JSON" deterred-messengers-load-telegram-json nil)
     ("Load VK HTML" deterred-messengers-load-vk-html nil)
     ("Load Telega" deterred-messengers-load-telega nil)
     ("Load Discord" deterred-messengers-load-discord)
     ("Process categories" deterred-messengers-categories-process-all-chats nil)
     ("Compute chains" deterred-messengers-chains-compute nil)
     ("Compute chain hostnames" deterred-messengers-chains-compute-hostname nil)
     ("Clear duplicates" deterred-messengers-delete-duplicates nil))
   callback))

(cl-defmethod deterred-source-range-summary
  ((_source deterred-messengers) start end &optional db)
  "Make messengers summary for [START, END].

DB is the sqlite database object."
  (let* ((db (or db (deterred-db--init)))
         (day-data
          (deterred-db-select-alist
           db "SELECT
                 mc.id,
                 mc.name,
                 mc.\"type\",
                 sum(CASE WHEN mm.sender_id != ? THEN 1 ELSE 0 END) received,
                 sum(CASE WHEN mm.sender_id = ? THEN 1 ELSE 0 END) sent
              FROM messenger_message mm
              INNER JOIN messenger_chat mc ON mc.id = mm.chat_id
              LEFT JOIN messenger_user mu ON mu.id = mc.target_user_id
              WHERE mm.\"timestamp\" BETWEEN ? AND ?
              GROUP BY mc.id, mc.name, mc.\"type\"
              HAVING sum(CASE WHEN mm.sender_id = ? THEN 1 ELSE 0 END) > 0 OR MC.\"type\" = \"personal_chat\"
              ORDER BY mc.\"type\", sent DESC"
           (list deterred-messengers-my-id deterred-messengers-my-id
                 start end
                 deterred-messengers-my-id)))
         (msg-by-type
          (seq-reduce (lambda (acc datum)
                        (let ((type (intern (alist-get 'type datum))))
                          (unless (alist-get type acc)
                            (setf (alist-get type acc) (cons 0 0)))
                          (setf (car (alist-get type acc))
                                (+ (alist-get 'received datum)
                                   (car (alist-get type acc))))
                          (setf (cdr (alist-get type acc))
                                (+ (alist-get 'sent datum)
                                   (cdr (alist-get type acc)) 0)))
                        acc)
                      day-data nil))
         (data-by-type (seq-group-by
                        (lambda (datum) (intern (alist-get 'type datum)))
                        day-data)))
    (when day-data
      `((:short-description
         . ,(format "%s/%s in groups; %s/%s in personal"
                    (or (car (alist-get 'group msg-by-type)) 0)
                    (or (cdr (alist-get 'group msg-by-type)) 0)
                    (or (car (alist-get 'personal_chat msg-by-type)) 0)
                    (or (cdr (alist-get 'personal_chat msg-by-type)) 0)))
        (:long-description
         . ,(deterred-format
             "Personal chats (received/sent):\n"
             (f-mapconcat
              (f "- " (f-acc "iter->'name") ": " (f-num (f-acc "iter->'received"))
                 "/" (f-num (f-acc "iter->'sent")))
              (f-acc "data-by-type->'personal_chat"))
             "\n\n"
             "Group chats (received/sent):\n"
             (f-mapconcat
              (f "- " (f-acc "iter->'name") ": " (f-num (f-acc "iter->'received"))
                 "/" (f-num (f-acc "iter->'sent")))
              (f-acc "data-by-type->'group"))))))))

(defun deterred-messengers--merge-messenger-ids (db table-name id1 id2)
  "Merge messenger IDs from ID2 into ID1 in TABLE-NAME.

DB is the sqlite database object.
TABLE-NAME is either \"messenger_user\" or \"messenger_chat\".
ID1 is the UUID to keep.
ID2 is the UUID to merge from.

For each messenger in `deterred-messengers-messenger-list', if ID2
has a messenger_id and ID1 doesn't, copy it to ID1.  The messenger_id
is first removed from ID2 to avoid UNIQUE constraint violations."
  (let* ((columns (mapcar (lambda (m) (intern (concat m "_id")))
                          deterred-messengers-messenger-list))
         (select-cols (mapconcat (lambda (col) (symbol-name col))
                                 columns ", "))
         (query (format "SELECT %s FROM %s WHERE id = ?" select-cols table-name))
         (data1 (car (deterred-db-select-alist db query (list id1))))
         (data2 (car (deterred-db-select-alist db query (list id2)))))
    (dolist (col columns)
      (when (and (alist-get col data2)
                 (not (alist-get col data1)))
        ;; First NULL out the messenger_id on ID2 to avoid UNIQUE constraint violation
        (sqlite-execute db
                        (format "UPDATE %s SET %s = NULL WHERE id = ?"
                                table-name (symbol-name col))
                        (list id2))
        ;; Then set it on ID1
        (sqlite-execute db
                        (format "UPDATE %s SET %s = ? WHERE id = ?"
                                table-name (symbol-name col))
                        (list (alist-get col data2) id1))))))

(defun deterred-messengers--get-messenger-columns ()
  "Get list of messenger ID column names as symbols."
  (mapcar (lambda (m) (intern (concat m "_id")))
          deterred-messengers-messenger-list))

(defun deterred-messengers--format-messenger-list (data)
  "Format messenger list from DATA alist for display.

DATA should have keys like telegram_id, vk_id, etc.
Returns a string like \"telegram, vk\"."
  (let ((messengers (delq nil
                          (mapcar (lambda (messenger)
                                    (when (alist-get (intern (concat messenger "_id")) data)
                                      messenger))
                                  deterred-messengers-messenger-list))))
    (string-join messengers ", ")))

(defun deterred-messengers-merge-chats (chat1-id chat2-id &optional db)
  "Merge CHAT2-ID into CHAT1-ID.

All messages from CHAT2-ID will be moved to CHAT1-ID, and CHAT2-ID
will be deleted.  Messenger IDs from chat2 will be copied to chat1
if chat1 doesn't have them.

DB is an optional sqlite database object.  If provided, the merge
happens within the existing transaction.  Otherwise, a new database
connection and transaction are created.

This function can be called programmatically with chat UUIDs, or
interactively where it will prompt for chat selection."
  (interactive
   (let* ((db (deterred-db--init))
          (columns (concat "id, name, type, "
                           (mapconcat #'symbol-name
                                      (deterred-messengers--get-messenger-columns)
                                      ", ")))
          (chats (deterred-db-select-alist
                  db (format "SELECT %s FROM messenger_chat" columns)))
          (format-chat (lambda (chat)
                         (let ((name (alist-get 'name chat))
                               (type (alist-get 'type chat))
                               (messengers (deterred-messengers--format-messenger-list chat)))
                           (format "%s [%s] (%s)" name type messengers))))
          (chat-alist (mapcar (lambda (chat)
                                (cons (funcall format-chat chat)
                                      (alist-get 'id chat)))
                              chats))
          (chat1-name (completing-read "Keep this chat: " chat-alist nil t))
          (chat1-id (alist-get chat1-name chat-alist nil nil #'equal))
          (remaining-alist (seq-remove (lambda (pair) (equal (cdr pair) chat1-id)) chat-alist))
          (chat2-name (completing-read "Merge this chat (will be deleted): " remaining-alist nil t))
          (chat2-id (alist-get chat2-name remaining-alist nil nil #'equal)))
     (list chat1-id chat2-id)))

  (let ((db (or db (deterred-db--init)))
        (needs-transaction (null db)))
    (if needs-transaction
        (with-sqlite-transaction db
          (deterred-messengers--merge-chats-impl db chat1-id chat2-id))
      (deterred-messengers--merge-chats-impl db chat1-id chat2-id))))

(defun deterred-messengers--merge-chats-impl (db chat1-id chat2-id)
  "Implementation of chat merging.  DB must be provided.

CHAT1-ID is kept, CHAT2-ID is deleted."
  ;; Merge messenger IDs from chat2 into chat1
  (let* ((chat2-data (car (deterred-db-select-alist
                           db "SELECT name FROM messenger_chat WHERE id = ?"
                           (list chat2-id))))
         (chat1-data (car (deterred-db-select-alist
                           db "SELECT name FROM messenger_chat WHERE id = ?"
                           (list chat1-id)))))

    (deterred-messengers--merge-messenger-ids db "messenger_chat" chat1-id chat2-id)

    (message "Merging chat '%s' into '%s'..."
             (alist-get 'name chat2-data)
             (alist-get 'name chat1-data)))

  ;; Move all messages from chat2 to chat1
  (sqlite-execute db "UPDATE messenger_message SET chat_id = ? WHERE chat_id = ?"
                  (list chat1-id chat2-id))

  ;; Delete chat2
  (sqlite-execute db "DELETE FROM messenger_chat WHERE id = ?"
                  (list chat2-id))

  (deterred-db-mark-updated-batch
   db
   '(messenger_chat messenger_message))

  (message "Successfully merged chats"))

(defun deterred-messengers--merge-duplicate-personal-chats (db)
  "Find and merge duplicate personal chats in DB.

Duplicate personal chats are those with the same target_user_id.
The first chat encountered for each target is kept, others are
merged into it.

DB must be a sqlite database object within an active transaction."
  (let ((all-personal-chats (deterred-db-select-alist
                             db "SELECT id, target_user_id
FROM messenger_chat
WHERE type = 'personal_chat' AND target_user_id IS NOT NULL
ORDER BY target_user_id"))
        (processed-targets (make-hash-table :test #'equal)))
    (dolist (chat all-personal-chats)
      (let* ((target-id (alist-get 'target_user_id chat))
             (chat-id (alist-get 'id chat))
             (existing-chat-id (gethash target-id processed-targets)))
        (if existing-chat-id
            ;; This is a duplicate, merge into existing
            (deterred-messengers--merge-chats-impl db existing-chat-id chat-id)
          ;; First chat with this target, remember it
          (puthash target-id chat-id processed-targets))))))

(defun deterred-messengers-merge-users (user1-id user2-id)
  "Merge USER2-ID into USER1-ID.

All data from USER2-ID will be moved to USER1-ID, and USER2-ID
will be deleted.  Personal chats with the same target will be merged.

This function can be called programmatically with user UUIDs, or
interactively where it will prompt for user selection."
  (interactive
   (let* ((db (deterred-db--init))
          (columns (concat "id, name, "
                           (mapconcat (lambda (col) (symbol-name col))
                                      (deterred-messengers--get-messenger-columns)
                                      ", ")))
          (users (deterred-db-select-alist
                  db (format "SELECT %s FROM messenger_user" columns)))
          (format-user (lambda (user)
                         (let ((name (alist-get 'name user))
                               (messengers (deterred-messengers--format-messenger-list user)))
                           (format "%s (%s)" name messengers))))
          (user-alist (mapcar (lambda (user)
                                (cons (funcall format-user user)
                                      (alist-get 'id user)))
                              users))
          (user1-name (completing-read "Keep this user: " user-alist nil t))
          (user1-id (alist-get user1-name user-alist nil nil #'equal))
          (remaining-alist (seq-remove (lambda (pair) (equal (cdr pair) user1-id)) user-alist))
          (user2-name (completing-read "Merge this user (will be deleted): " remaining-alist nil t))
          (user2-id (alist-get user2-name remaining-alist nil nil #'equal)))
     (list user1-id user2-id)))

  (let ((db (deterred-db--init)))
    (with-sqlite-transaction db
      ;; Merge messenger IDs from user2 into user1
      (let* ((user2-data (car (deterred-db-select-alist
                               db "SELECT name FROM messenger_user WHERE id = ?"
                               (list user2-id))))
             (user1-data (car (deterred-db-select-alist
                               db "SELECT name FROM messenger_user WHERE id = ?"
                               (list user1-id)))))

        (deterred-messengers--merge-messenger-ids db "messenger_user" user1-id user2-id)

        (message "Merging '%s' into '%s'..."
                 (alist-get 'name user2-data)
                 (alist-get 'name user1-data)))

      ;; Update all references from user2 to user1
      (sqlite-execute db "UPDATE messenger_message SET sender_id = ? WHERE sender_id = ?"
                      (list user1-id user2-id))
      (sqlite-execute db "UPDATE messenger_chat SET target_user_id = ? WHERE target_user_id = ?"
                      (list user1-id user2-id))
      ;; Find and merge duplicate personal chats
      (deterred-messengers--merge-duplicate-personal-chats db)

      ;; Delete user2
      (sqlite-execute db "DELETE FROM messenger_user WHERE id = ?"
                      (list user2-id))

      (deterred-db-mark-updated-batch
       db
       '(messenger_chat messenger_user messenger_message))

      (message "Successfully merged users"))))

(defun deterred-messengers--telega-get-message-text (msg)
  "Extract text content from telega MSG.

Returns the message text or empty string if no text content."
  (let* ((content (plist-get msg :content))
         (text-obj (or (plist-get content :text)
                       (plist-get content :caption))))
    (if (and text-obj (plist-get text-obj :text))
        ;; text-obj is a formattedText object, extract :text field
        (plist-get text-obj :text)
      ;; Fallback to empty string
      "")))

(defun deterred-messengers--telega-get-sender-info (msg)
  "Get sender ID and name from telega MSG.

Returns (sender-id . sender-name)."
  (let* ((sender (plist-get msg :sender_id))
         (sender-type (plist-get sender :@type)))
    (pcase sender-type
      ("messageSenderUser"
       (let* ((user-id (plist-get sender :user_id))
              (user (telega-user-get user-id)))
         (cons user-id
               (or (telega-user-title user 'full-name t)
                   (format "User %d" user-id)))))
      ("messageSenderChat"
       (let* ((chat-id (plist-get sender :chat_id))
              (chat (telega-chat-get chat-id)))
         (cons chat-id
               (or (telega-chat-title chat)
                   (format "Chat %d" chat-id)))))
      (_ (cons 0 "Unknown")))))

(defun deterred-messengers--telega-process-messages (chat user-cache chat-cache start-timestamp)
  "Process messages from telega CHAT starting from START-TIMESTAMP.

USER-CACHE and CHAT-CACHE are hash tables from
`deterred-messengers--load-users-by-messenger' and
`deterred-messengers--load-chats-by-messenger'.

Returns an alist with keys:
- users - hash table of user alists (keyed by user UUID)
- chats - hash table of chat alists (keyed by chat UUID)
- messages - list of message alists for database insertion."
  (let* ((chat-type-obj (plist-get chat :type))
         (chat-type (plist-get chat-type-obj :@type))
         (telegram-chat-id (plist-get chat :id))
         (type (pcase chat-type
                 ("chatTypePrivate" "personal_chat")
                 ("chatTypeSecret" "personal_chat")
                 (_ "group")))
         (found-users (make-hash-table :test #'equal))
         (found-chats (make-hash-table :test #'equal))
         messages
         target-user-id
         chat-title
         (my-user-id (plist-get telega--me-id :user_id)))

    ;; For personal chats, get the other user from chat structure
    (if (equal type "personal_chat")
        (let* ((other-user-id (plist-get chat-type-obj :user_id))
               (other-user (when other-user-id (telega-user-get other-user-id)))
               (other-user-name (when other-user
                                  (telega-user-title other-user 'full-name t))))
          (when other-user-id
            (let ((user-data (deterred-messengers--get-or-create-user
                             user-cache "telegram" other-user-id
                             (or other-user-name (format "User %d" other-user-id)))))
              (setq target-user-id (alist-get 'id user-data))
              (setq chat-title (or other-user-name (format "User %d" other-user-id)))
              (puthash target-user-id user-data found-users))))
      ;; For group chats, use the chat title
      (setq chat-title (telega-chat-title chat)))

    ;; Get messages synchronously
    (let ((msgs nil)
          (from-msg-id 0))
      (while (progn
               (let ((result (telega--searchChatMessages
                                 chat
                                 '(:@type "searchMessagesFilterEmpty")
                                 from-msg-id
                                 0
                               :limit 100)))
                 (when result
                   (let ((batch-msgs (append (plist-get result :messages) nil)))
                     (when batch-msgs
                       (setq from-msg-id (plist-get (car (last batch-msgs)) :id))
                       (setq msgs (append msgs batch-msgs))
                       ;; Check if oldest message is still after start-timestamp
                       (> (plist-get (car (last batch-msgs)) :date) start-timestamp)))))))

      ;; Filter messages by timestamp and process them
      (dolist (msg (seq-filter (lambda (m) (>= (plist-get m :date) start-timestamp)) msgs))
        (let* ((sender-info (deterred-messengers--telega-get-sender-info msg))
               (sender-telegram-id (car sender-info))
               (sender-name (cdr sender-info))
               (user-data (deterred-messengers--get-or-create-user
                           user-cache "telegram" sender-telegram-id sender-name))
               (sender-uuid (alist-get 'id user-data))
               (timestamp (plist-get msg :date))
               (msg-id (deterred-messengers--id
                        "telegram" telegram-chat-id (plist-get msg :id)))
               (telegram-msg-id (plist-get msg :id))
               (content (deterred-messengers--telega-get-message-text msg))
               (is-attachment (if (plist-get (plist-get msg :content) :@type) 1 0))
               (is-outgoing (equal my-user-id sender-telegram-id)))

          ;; Store user
          (puthash sender-uuid user-data found-users)

          ;; Add message
          (push `((id . ,msg-id)
                  (telegram_id . ,telegram-msg-id)
                  (sender_id . ,sender-uuid)
                  (chat_id . ,nil)
                  (content . ,content)
                  (timestamp . ,timestamp)
                  (is_attachment . ,is-attachment)
                  (messenger . "telegram"))
                messages))))

    (when (> (seq-length messages) 0)
      ;; Create chat
      (let* ((chat-data (deterred-messengers--get-or-create-chat
                         chat-cache "telegram" telegram-chat-id
                         chat-title type target-user-id))
             (chat-uuid (alist-get 'id chat-data)))
        (puthash chat-uuid chat-data found-chats)

        ;; Update message chat_ids
        (setq messages
              (mapcar (lambda (msg)
                        (setf (alist-get 'chat_id msg) chat-uuid)
                        msg)
                      messages))
        `((users . ,found-users)
          (chats . ,found-chats)
          (messages . ,messages))))))

(defun deterred-messengers--telega-parse-data (&optional start-date)
  "Parse telega data and return it in deterred-messengers format.

START-DATE is a UNIX timestamp.  If nil, parses all messages.

Returns an alist with keys:
- users - list of user alists
- chats - list of chat alists
- messages - list of message alists."
  (unless telega--me-id
    (user-error "Telega is not initialized.  Please start telega first"))

  (let* ((db (deterred-db--init))
         (start-timestamp (or start-date 0))
         (user-cache (deterred-messengers--load-users-by-messenger db "telegram"))
         (chat-cache (deterred-messengers--load-chats-by-messenger db "telegram"))
         (all-users-ht (make-hash-table :test #'equal))
         (all-chats-ht (make-hash-table :test #'equal))
         all-messages
         (chats (telega--getChats '(:@type "chatListMain"))))

    (message "Processing %d chats from telega..." (length chats))

    (cl-loop for chat in chats
             for i from 1
             for result = (deterred-messengers--telega-process-messages
                           chat user-cache chat-cache start-timestamp)
             when result
             do (progn
                  (deterred-utils-merge-hashes all-users-ht (alist-get 'users result))
                  (deterred-utils-merge-hashes all-chats-ht (alist-get 'chats result))
                  (push (alist-get 'messages result) all-messages))
             do (when (= (% i 10) 0)
                  (message "Processed %d/%d chats..." i (length chats))))

    `((users . ,(hash-table-values all-users-ht))
      (chats . ,(hash-table-values all-chats-ht))
      (messages . ,(apply #'append all-messages)))))

(defun deterred-messengers-load-telega ()
  "Load telega data into DETERRED.

Imports messages starting from the last imported date minus 1 day,
or all messages if this is the first import."
  (interactive)
  (unless (featurep 'telega)
    (user-error "Telega is not loaded.  Please load telega.el first"))

  (let* ((db (deterred-db--init))
         ;; Get last imported timestamp for telegram, minus 1 day for safety
         (last-timestamp (caar (sqlite-select
                                db "SELECT MAX(timestamp) FROM messenger_message
                                   WHERE messenger = 'telegram'")))
         (start-date (if last-timestamp
                         (- last-timestamp (* 24 60 60))  ; 1 day before last import
                       nil)))  ; nil means import all

    (message "Parsing telega data%s..."
             (if start-date
                 (format " from %s" (format-time-string "%Y-%m-%d" start-date))
               " (all messages)"))

    (let ((data (deterred-messengers--telega-parse-data start-date)))
      (with-sqlite-transaction db
        (let ((users (alist-get 'users data))
              (chats (alist-get 'chats data))
              (messages (alist-get 'messages data)))

          (message "Importing %d users, %d chats, %d messages..."
                   (length users) (length chats) (length messages))

          (when users
            (deterred-db-insert-unsafe
             db :table-name 'messenger_user
             :values users
             :conflict-action 'do-update
             :conflict-attrs '(id)))
          (when chats
            (deterred-db-insert-unsafe
             db :table-name 'messenger_chat
             :values chats
             :attrs '(id telegram_id name type target_user_id vk_id)
             :conflict-action 'do-update
             :conflict-attrs '(id)))
          (when messages
            (deterred-db-insert-unsafe
             db :table-name 'messenger_message
             :values messages
             :conflict-action 'do-update
             :conflict-attrs '(id)))

          (deterred-db-mark-updated-batch
           db
           '(messenger_chat messenger_user messenger_message))

          (message "Successfully imported telega data: %d messages" (length messages)))))))

(defun deterred-messengers-discord--parse-name (index-name)
  "Extract name and type from Discord chat INDEX-NAME.

INDEX-NAME is a value from the Discord messages dump index file, which
is a dict mapping chat ids to their names.

For group chats, this returns:
- name - server name or \"Unknown group chat\"
- subgroup-name - channel name, which can be \"Unknown Channel\" if
  the user has left the server.
- type - `group'.

And for personal chats:
- name - other user's name, which is unfortunately not their readable
  name but a username.
- type - `personal_chat'."
  (save-match-data
    ;; <channel-name> in <server-name>, return server-name
    (or (when (string-match
               (rx bos (group (* nonl)) " in " (group (* nonl))) index-name)
          `((name . ,(match-string 2 index-name))
            (subgroup-name . ,(match-string 1 index-name))
            (type . group)))
        ;; Direct message with <user-name>, return user-name
        (when (string-match (rx bos "Direct message with " (group (* nonl)))
                            index-name)
          `((name . ,(match-string 1 index-name))
            (type . personal_chat)))
        (when (or (equal "Unknown channel" index-name)
                  (equal "None" index-name) )
          `((name . "Unknown group chat")
            (type . group)))
        `((name . ,index-name)
          (type . group)))))

(defun deterred-messengers-discord--parse-my-id (directory index)
  "Extract the discord ID of the user who has made the dump.

Return a number.

DIRECTORY is the dump folder, INDEX is the contents of the index
file.  This works by finding a common participant ID in two direct
message chats.  I'm not sure if it can work for any Discord dump, but
it works for me."
  (cl-block search-loop
    (let (prev-participants)
      (dolist (item index)
        (let ((channel-data
               (json-read-file
                (concat directory (format "c%s/channel.json" (car item))))))
          (when (and (equal (alist-get 'type channel-data) "DM")
                     (> (seq-length (alist-get 'recipients channel-data)) 1))
            (if prev-participants
                (let ((me (seq-intersection
                           (alist-get 'recipients channel-data) prev-participants)))
                  (unless (= 1 (seq-length me))
                    (error "Can't find me! Intersection: %s" me))
                  (cl-return-from search-loop (string-to-number (car me))))
              (setq prev-participants (alist-get 'recipients channel-data)))))))))

(defun deterred-messengers-discord--parse-folder (directory)
  "Parse discord dump DIRECTORY.

Return an alist with the following keys:
- users
- chats
- messages
ready to be inserted into the database."
  (let* ((index (json-read-file (concat directory "index.json")))
         (my-id (deterred-messengers-discord--parse-my-id directory index))
         (db (deterred-db--init))
         (user-cache (deterred-messengers--load-users-by-messenger db "discord"))
         (chat-cache (deterred-messengers--load-chats-by-messenger db "discord"))
         user-data chat-data messages-data
         (chat-discord-id-by-name (make-hash-table :test #'equal))
         (me (car (deterred-db-select-alist
                   db "SELECT * FROM messenger_user WHERE id = ?"
                   (list deterred-messengers-my-id)))))
    ;; Insert myself into `user-data'
    (puthash my-id deterred-messengers-my-id user-cache)
    (unless me
      (setq me `((id . ,deterred-messengers-my-id)
                 (name . "Me"))))
    (setf (alist-get 'discord_id me) my-id)
    (push me user-data)

    ;; Processing chats one by one
    (dolist (index-value index)
      (pcase-let ((`(,key . ,name) index-value))
        (let* ((chat-id (string-to-number (symbol-name key)))
               (chat-directory (concat directory (format "c%s/" key)))
               (channel-dump (json-read-file (concat chat-directory "channel.json")))
               (messages-dump (json-read-file (concat chat-directory "messages.json")))
               (name-data (deterred-messengers-discord--parse-name name))
               other-user-id found-server)
          ;; If it's a personal chat, determine the counterpart
          (when (eq (alist-get 'type name-data) 'personal_chat)
            (let ((other-ids (seq-difference
                              (mapcar #'string-to-number
                                      (alist-get 'recipients channel-dump))
                              (list my-id))))
              (unless (= (seq-length other-ids) 1)
                (error "Something weird with %s: %d participants"
                       chat-directory (seq-length other-ids)))
              (let ((user
                     (deterred-messengers--get-or-create-user
                      user-cache "discord" (car other-ids)
                      (alist-get 'name name-data))))
                (push user user-data)
                (setq other-user-id (alist-get 'id user)))))
          ;; If it's a server that's been seen before, use the
          ;; previous found chat-id
          (if-let ((prev-id
                    (gethash (alist-get 'name name-data) chat-discord-id-by-name)))
              (setq chat-id prev-id
                    found-server t)
            (puthash (alist-get 'name name-data) chat-id chat-discord-id-by-name))
          (let ((chat (deterred-messengers--get-or-create-chat
                       chat-cache "discord" chat-id (alist-get 'name name-data)
                       (alist-get 'type name-data) other-user-id)))
            (unless found-server
              (push chat chat-data))
            (push
             (mapcar
              (lambda (m)
                `((id . ,(deterred-messengers--id "discord" (alist-get 'ID m)))
                  (timestamp . ,(time-convert
                                 (encode-time
                                  (iso8601-parse
                                   (string-replace " " "T" (alist-get 'Timestamp m))))
                                 'integer))
                  (sender_id . ,deterred-messengers-my-id)
                  (messenger . "discord")
                  (chat_id . ,(alist-get 'id chat))
                  (content . ,(alist-get 'Contents m))
                  (discord_id . ,(alist-get 'ID m))
                  (is_attachment . 0)))
              messages-dump)
             messages-data)))))
    `((users . ,user-data)
      (chats . ,chat-data)
      (messages . ,(apply #'append messages-data)))))

(defun deterred-messengers-load-discord (directory)
  "Load Discord export DIRECTORY into DETERRED.

The directory has to have an index.json file, mapping chat ids to
names, and folders like c<chat-id>.  Each folder has to have the
following files:
- channel.json
- messages.json"
  (interactive "DDiscord export directory: ")
  (let ((data (deterred-messengers-discord--parse-folder directory))
        (db (deterred-db--init)))
    (with-sqlite-transaction db
      (deterred-db-insert-unsafe
       db
       :table-name 'messenger_user
       :values (alist-get 'users data)
       :conflict-action 'do-update
       :conflict-attrs '(id))
      (deterred-db-insert-unsafe
       db
       :table-name 'messenger_chat
       :values (alist-get 'chats data)
       :conflict-action 'do-update
       :conflict-attrs '(id))
      (deterred-db-insert-unsafe
       db
       :table-name 'messenger_message
       :values (alist-get 'messages data)
       :conflict-action 'do-update
       :conflict-attrs '(id)))))

(defun deterred-messengers-delete-duplicates ()
  "Delete duplicate messages from DETERRED."
  (interactive)
  (let ((db (deterred-db--init)))
    (let ((count
           (sqlite-execute
            db
            "DELETE FROM messenger_message
WHERE rowid NOT IN (
    SELECT MIN(rowid)
    FROM messenger_message
    WHERE content != ''
    GROUP BY content, timestamp, chat_id
)"
            )))
      (message "%s duplicates deleted" count))))

(cl-defmethod deterred-source-events
  ((_source deterred-messengers) start end &optional params db)
  "Return events for messages.

See the generic method description for START, END and DB.  PARAMS are
the same as in `deterred-dashboard-messengers'."
  (let ((db (or db (deterred-db--init))))
    (deterred-db-select-template
     db
     "SELECT mm.timestamp, null, mc.name
FROM messenger_message mm
INNER JOIN messenger_chat mc ON mc.id = mm.chat_id
WHERE mm.timestamp BETWEEN :start AND :end
    [[AND date(mm.timestamp, 'unixepoch') >= date(:start-date, 'unixepoch')]]
    [[AND date(mm.timestamp, 'unixepoch') <= date(:end-date, 'unixepoch')]]
    [[AND mc.type IN :chat-type]]
    [[AND mc.name IN :chat-name]]
    [[AND mm.messenger IN :messenger]]
    [[AND mm.category IN :category]]"
     (deterred-utils-merge-alists (list params `((:start . ,start) (:end . ,end)))))))

(declare-function deterred-dashboard-messengers "deterred-dashboard-messengers")

(cl-defmethod deterred-source-default-dashboard ((_source deterred-messengers))
  "Return the default dashboard for the messengers source."
  (deterred-dashboard-messengers))

(provide 'deterred-messengers)
;;; deterred-messengers.el ends here

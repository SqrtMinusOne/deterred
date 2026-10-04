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
(require 'json)
(require 'generator)

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

;; Rocket functions (optional dependency)
(declare-function rocket-core-get "rocket-core" (object key &optional default))
(declare-function rocket-core-time "rocket-core" (value))
(declare-function rocket-core-request "rocket-core"
                  (server-id method &optional params success error))
(declare-function rocket-server-running-p "rocket-server" ())
(declare-function rocket-server-states "rocket-core" ())
(declare-function rocket-server-state-id "rocket-core" (state))
(declare-function rocket-server-state-name "rocket-core" (state))
(declare-function rocket-server-state-user-id "rocket-core" (state))
(declare-function rocket-server-state-connection-state "rocket-core" (state))
(declare-function rocket-user-get "rocket-core" (server-id user-id))
(declare-function rocket-user-name "rocket-core" (user))
(declare-function rocket-user-username "rocket-core" (user))

;; MAX functions (optional dependency)
(declare-function max-core-get "max-core" (object key &optional default))
(declare-function max-core-time "max-core" (value))
(declare-function max-core-request-sync "max-core" (method &optional params timeout))
(declare-function max-connected-p "max-core" ())
(declare-function max-account-state-get "max-core" (&optional account-id))
(declare-function max-account-state-connection-state "max-core" (state))
(declare-function max-server-running-p "max-server" ())

(defconst deterred-messengers-uuid-namespace
  "d9536b80-3213-4321-b37f-ebf1b558a530")

(defconst deterred-messengers-messenger-list
  '("telegram" "vk" "discord" "max" "rocket")
  "List of supported messenger names.")

(defcustom deterred-messengers-my-id nil
  "The user's UUID."
  :type 'string
  :group 'deterred)

(defcustom deterred-messengers-discord-non-work nil
  "Discord chat names to include in the Social Media dashboard.

Names are matched exactly against `messenger_chat.name'.  An empty
list excludes all Discord messages from that dashboard."
  :type '(repeat (string :tag "Chat name"))
  :group 'deterred)

(defcustom deterred-messengers-vk-timezone-offset -10800
  "Timezone offset in seconds to convert VK timestamps to UTC.

VK exports times in MSK (UTC+3), so the default is -10800 seconds (-3 hours)
to convert to UTC."
  :type 'integer
  :group 'deterred)

(defcustom deterred-messengers-request-interval 1
  "Minimum seconds between MAX or Rocket.Chat import requests."
  :type 'number
  :group 'deterred)

(defcustom deterred-messengers-rate-limit-retries 5
  "Maximum retries of a rate-limited MAX or Rocket.Chat import request."
  :type 'integer
  :group 'deterred)

(defcustom deterred-messengers-rate-limit-wait 60
  "Seconds to wait when a rate limit supplies no positive retry delay."
  :type 'number
  :group 'deterred)

(defcustom deterred-messengers-rocket-start-dates nil
  "Earliest Rocket.Chat import dates by configured workspace ID.
Each entry maps a workspace symbol or string to a YYYY-MM-DD date.
Messages at or after local midnight on that date can be imported in
either direction.  A missing entry or nil date imposes no date limit.

The limit also applies to resumed imports and historical backfills.
It does not delete previously imported messages.  Moving the date earlier
does not automatically backfill history excluded by a previous import."
  :type '(alist :key-type (choice (symbol :tag "Workspace symbol")
                                  (string :tag "Workspace string"))
                :value-type (choice (const :tag "All history" nil)
                                    (string :tag "Start date (YYYY-MM-DD)")))
  :group 'deterred)

(defcustom deterred-messengers-rocket-excluded-rooms nil
  "Rocket.Chat rooms to skip, grouped by configured workspace ID.
Each entry maps a workspace symbol or string to a list of room IDs or
room names.  Match IDs and room/subscription names exactly and case
sensitively, without regular expressions.  IDs remain valid after renaming.

Excluded rooms make no history requests and do not count toward import
progress.  Existing messages and saved pagination positions are retained.
The setting is captured when an import starts; cancel and restart a running
import to apply changes."
  :type '(alist :key-type (choice (symbol :tag "Workspace symbol")
                                  (string :tag "Workspace string"))
                :value-type (repeat (string :tag "Room ID or name")))
  :group 'deterred)

(defcustom deterred-messengers-max-reconnect-timeout 30
  "Seconds to wait for MAX's automatic reconnect during an import.
This does not start or authenticate a stopped client.  Set to zero to fail
immediately when MAX is reconnecting."
  :type 'number
  :group 'deterred)

(defvar deterred-messengers--import-last-request (make-hash-table :test #'eq)
  "Times of the last import requests, keyed by messenger symbol.")

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

(defun deterred-messengers--import-key (&rest ids)
  "Encode IDS as an unambiguous composite remote identifier."
  (json-serialize (vconcat ids)))

(defun deterred-messengers--import-wait (seconds)
  "Wait SECONDS while allowing client processes to handle output.
The user can interrupt the wait with `keyboard-quit'."
  (let ((deadline (+ (float-time) seconds)))
    (while (< (float-time) deadline)
      (accept-process-output nil (max 0 (min 1 (- deadline (float-time))))))))

(defun deterred-messengers--rate-limit-delay (payload)
  "Return the retry delay in seconds supplied by error PAYLOAD."
  (let ((milliseconds (plist-get (plist-get payload :data) :retry_after_ms))
        (description (plist-get payload :message)))
    (cond
     ((and (numberp milliseconds) (> milliseconds 0)) (/ milliseconds 1000.0))
     ((and (stringp description)
           (string-match "\\b\\([0-9]+\\(?:\\.[0-9]+\\)?\\) seconds?\\b" description)
           (> (string-to-number (match-string 1 description)) 0))
      (string-to-number (match-string 1 description)))
     (t (max 1 deterred-messengers-rate-limit-wait)))))

(defun deterred-messengers--import-request (messenger request)
  "Call REQUEST for MESSENGER with pacing and bounded rate-limit retries.
MESSENGER is `max' or `rocket'.  REQUEST is a function of no arguments.
Other errors propagate immediately."
  (let ((retries 0) result done)
    (while (not done)
      (let ((delay (- deterred-messengers-request-interval
                      (- (float-time)
                         (gethash messenger deterred-messengers--import-last-request 0)))))
        (when (> delay 0)
          (deterred-messengers--import-wait delay)))
      (puthash messenger (float-time) deterred-messengers--import-last-request)
      (condition-case err
          (setq result (funcall request) done t)
        (error
         (let* ((payload (cadr err))
                (code (and (listp payload) (plist-get payload :code)))
                (limited (equal code (if (eq messenger 'rocket)
                                         "http-429" "rate-limited"))))
           (unless (and limited (< retries deterred-messengers-rate-limit-retries))
             (signal (car err) (cdr err)))
           (cl-incf retries)
           (let ((delay (deterred-messengers--rate-limit-delay payload)))
             (message "%s rate limit: waiting %s seconds (retry %d/%d)..."
                      messenger delay retries deterred-messengers-rate-limit-retries)
             (deterred-messengers--import-wait delay))))))
    result))

(defun deterred-messengers--import-cutoff (db messenger chat-uuid &optional remote-chat-id)
  "Return the incremental import cutoff for MESSENGER and CHAT-UUID in DB.
Revisit one day before the latest stored message, or all history for a
new chat.  For Rocket, REMOTE-CHAT-ID limits this to the original room,
since multiple rooms share one workspace chat."
  (let ((latest
         (caar (if (and (equal messenger "rocket") remote-chat-id)
                   (sqlite-select
                    db "SELECT MAX(timestamp) FROM messenger_message
                        WHERE messenger = 'rocket' AND rocket_room_id = ?"
                    (list remote-chat-id))
                 (when chat-uuid
                   (sqlite-select
                    db "SELECT MAX(timestamp) FROM messenger_message
                        WHERE messenger = ? AND chat_id = ?"
                    (list messenger chat-uuid)))))))
    (if latest (max 0 (- latest 86400)) 0)))

(defun deterred-messengers--import-state (db messenger remote-chat-id chat-uuid)
  "Return the saved cursor and cutoff for a chat import in DB.
MESSENGER and REMOTE-CHAT-ID identify the remote chat.  CHAT-UUID is used
to calculate a fresh cutoff when there is no unfinished import."
  (or (car (deterred-db-select-alist
            db "SELECT cutoff, cursor FROM messenger_import_progress
                WHERE messenger = ? AND remote_chat_id = ?"
            (list messenger remote-chat-id)))
      `((cutoff . ,(deterred-messengers--import-cutoff
                    db messenger chat-uuid remote-chat-id))
        (cursor . nil))))

(defun deterred-messengers--rocket-move-room (db remote-room-id chat-id)
  "Move existing REMOTE-ROOM-ID messages to CHAT-ID in DB.
Keep chains belonging entirely to the moved room.  Invalidate chains
shared with other rooms, whose derived time spans can no longer be reused.
The caller must have inserted the target chat within a transaction."
  (dolist (row (sqlite-select
                db "SELECT DISTINCT chain_id FROM messenger_message
                    WHERE messenger = 'rocket' AND rocket_room_id = ?
                      AND chat_id != ? AND chain_id IS NOT NULL"
                (list remote-room-id chat-id)))
    (let ((chain-id (car row)))
      (if (sqlite-select
           db "SELECT 1 FROM messenger_message WHERE chain_id = ?
               AND (messenger IS NOT 'rocket' OR rocket_room_id IS NOT ?) LIMIT 1"
           (list chain-id remote-room-id))
          (progn
            (sqlite-execute db "UPDATE messenger_message SET chain_id = NULL
                                WHERE chain_id = ?" (list chain-id))
            (sqlite-execute db "DELETE FROM messenger_message_chain WHERE id = ?"
                            (list chain-id)))
        (sqlite-execute db "UPDATE messenger_message_chain SET chat_id = ? WHERE id = ?"
                        (list chat-id chain-id)))))
  (sqlite-execute
   db "UPDATE messenger_message SET chat_id = ?
       WHERE messenger = 'rocket' AND rocket_room_id = ? AND chat_id != ?"
   (list chat-id remote-room-id chat-id)))

(defun deterred-messengers--store-chat
    (db messenger users chat messages &optional cutoff cursor remote-chat-id)
  "Store CHAT data from MESSENGER in DB atomically.
USERS and MESSAGES are insertion alists.  Update only supplied columns,
preserving other messenger IDs and derived message metadata.  Return the
number of messages.  With CUTOFF, also save the next page CURSOR, or clear
the pending import when CURSOR is nil.  REMOTE-CHAT-ID overrides the
checkpoint key for a Rocket room stored under a shared workspace chat."
  (with-sqlite-transaction db
    (sqlite-execute
     db "INSERT INTO messenger_user (id, name) VALUES (?, 'Me')
         ON CONFLICT (id) DO NOTHING"
     (list deterred-messengers-my-id))
    ;; Users may have different fields, notably the canonical self record.
    (dolist (user users)
      (deterred-db-insert-unsafe
       db :table-name 'messenger_user :values (list user)
       :conflict-action 'do-update :conflict-attrs '(id)))
    (deterred-db-insert-unsafe
     db :table-name 'messenger_chat :values (list chat)
     :conflict-action 'do-update :conflict-attrs '(id))
    (when (and (equal messenger "rocket") remote-chat-id)
      (deterred-messengers--rocket-move-room db remote-chat-id (alist-get 'id chat)))
    (let ((attrs `(id ,(intern (concat messenger "_id"))
                     ,@(when (equal messenger "rocket") '(rocket_room_id)) sender_id chat_id
                     content timestamp is_attachment messenger)))
      (dolist (batch (seq-partition messages 500))
        (deterred-db-insert-unsafe
         db :table-name 'messenger_message :values batch :attrs attrs
         :conflict-action 'do-update :conflict-attrs '(id))))
    (when cutoff
      (let ((remote-id (or remote-chat-id
                           (alist-get (intern (concat messenger "_id")) chat))))
        (if cursor
            (sqlite-execute
             db "INSERT INTO messenger_import_progress
                 (messenger, remote_chat_id, cutoff, cursor) VALUES (?, ?, ?, ?)
                 ON CONFLICT (messenger, remote_chat_id)
                 DO UPDATE SET cutoff = excluded.cutoff, cursor = excluded.cursor"
             (list messenger remote-id cutoff cursor))
          (sqlite-execute
           db "DELETE FROM messenger_import_progress
               WHERE messenger = ? AND remote_chat_id = ?"
           (list messenger remote-id)))))
    (deterred-db-mark-updated-batch
     db (append '(messenger_user messenger_chat messenger_message)
                (when (equal messenger "rocket") '(messenger_message_chain)))))
  (length messages))

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
     ("Load Max" deterred-messengers-load-max nil)
     ("Load Rocket" deterred-messengers-load-rocket t)
     ("Rocket import status" deterred-messengers-rocket-import-status nil)
     ("Cancel Rocket import" deterred-messengers-cancel-rocket-import nil)
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

(defun deterred-messengers--max-await-connection ()
  "Wait for an existing MAX reconnect, or signal an actionable error.
Allow client status events to run while waiting.  Never start the sidecar
or retry disconnected, logged-out, or failed authentication states."
  (unless (and (featurep 'max-core) (featurep 'max-server)
               (max-server-running-p))
    (user-error "MAX is not connected; start max.el first"))
  (let ((deadline (+ (float-time)
                     (max 0 deterred-messengers-max-reconnect-timeout)))
        waiting)
    (while (and (max-server-running-p) (not (max-connected-p))
                (fboundp 'max-account-state-get)
                (fboundp 'max-account-state-connection-state)
                (when-let ((state (max-account-state-get)))
                  (eq (max-account-state-connection-state state) 'reconnecting)))
      (when (>= (float-time) deadline)
        (user-error "MAX reconnect timed out; saved import progress can be resumed"))
      (unless waiting
        (setq waiting t)
        (message "MAX is reconnecting; waiting to resume the import..."))
      (deterred-messengers--import-wait
       (min 0.25 (max 0 (- deadline (float-time))))))
    (unless (and (max-server-running-p) (max-connected-p))
      (user-error "MAX is disconnected; reconnect max.el and rerun the import"))))

(defun deterred-messengers--max-request (method &optional params)
  "Read MAX METHOD with PARAMS, waiting for an automatic reconnect."
  (deterred-messengers--max-await-connection)
  (deterred-messengers--import-request
   'max
   (lambda ()
     ;; Pacing and rate-limit waits also process connection status events.
     (deterred-messengers--max-await-connection)
     (max-core-request-sync method params))))

(defun deterred-messengers--max-user
    (user-cache profiles self-id user-id &optional fallback-name)
  "Return a DETERRED user row for MAX USER-ID.

USER-CACHE maps MAX IDs to DETERRED UUIDs.  PROFILES contains normalized
MAX profiles, SELF-ID identifies the account, and FALLBACK-NAME is used
when the profile has no display name."
  (if (equal user-id self-id)
      `((id . ,deterred-messengers-my-id) (max_id . ,self-id))
    (let ((profile (gethash user-id profiles)))
      (deterred-messengers--get-or-create-user
       user-cache "max" user-id
       (or (max-core-get profile :name)
           (max-core-get profile :username)
           fallback-name
           (format "MAX user %s" user-id))))))

(defun deterred-messengers--max-import-chat
    (db chat user-cache chat-cache profiles self-id)
  "Import all message directions from MAX CHAT into DB.

USER-CACHE and CHAT-CACHE map MAX IDs to DETERRED UUIDs.  PROFILES
contains normalized MAX profiles and SELF-ID is the current account.
Save each fetched page together with its continuation cursor.  After
an interruption, resume at that cursor using the original cutoff.
Return the number of imported messages."
  (let* ((chat-id (max-core-get chat :id))
         (name (or (max-core-get chat :displayName)
                   (max-core-get chat :name)
                   (format "MAX chat %s" chat-id)))
         (type (if (equal (max-core-get chat :type) "direct")
                   "personal_chat" "group"))
         (chat-row (deterred-messengers--get-or-create-chat
                    chat-cache "max" chat-id name type))
         (chat-uuid (alist-get 'id chat-row))
         (state (deterred-messengers--import-state db "max" chat-id chat-uuid))
         (cutoff (alist-get 'cutoff state))
         (before (alist-get 'cursor state))
         (before-time
          (when before
            (caar (sqlite-select
                   db "SELECT timestamp FROM messenger_message
                       WHERE messenger = 'max' AND chat_id = ? AND max_id = ?"
                   (list chat-uuid before)))))
         (seen-cursors (make-hash-table :test #'equal))
         (seen-messages (make-hash-table :test #'equal))
         (count 0)
         finished)
    (unless (and (stringp chat-id) (not (string-empty-p chat-id)))
      (error "MAX returned a chat without an ID"))
    (when before
      (puthash before t seen-cursors))
    (while (not finished)
      (let* ((result (deterred-messengers--max-request
                      "history.get"
                      (append (list :chatId chat-id :limit 100)
                              (when before (list :before before)))))
             (payload (max-core-get result :messages 'missing))
             (page (if (or (listp payload) (vectorp payload))
                       (append payload nil)
                     (error "MAX returned invalid history in %s" name)))
             (has-more (max-core-get result :hasMore 'missing))
             (users (make-hash-table :test #'equal))
             messages)
        (unless (memq has-more '(t nil :false false json-false :null))
          (error "MAX returned an invalid history continuation flag in %s" name))
        (dolist (profile (append (max-core-get result :users) nil))
          (puthash (max-core-get profile :id) profile profiles))
        (puthash deterred-messengers-my-id
                 (deterred-messengers--max-user
                  user-cache profiles self-id self-id)
                 users)
        (when (equal type "personal_chat")
          (let ((peers (seq-remove
                        (lambda (id) (equal id self-id))
                        (append (max-core-get chat :participantIds) nil))))
            (when (= (length peers) 1)
              (let* ((user (deterred-messengers--max-user
                            user-cache profiles self-id (car peers) name))
                     (uuid (alist-get 'id user)))
                (puthash uuid user users)
                (setf (alist-get 'target_user_id chat-row) uuid)))))
        (dolist (msg page)
          (let* ((message-id (max-core-get msg :id))
                 (timestamp (floor (max-core-time
                                    (max-core-get msg :timestamp)))))
            (unless (and (stringp message-id)
                         (not (string-empty-p message-id))
                         (> timestamp 0))
              (error "MAX returned a message without an ID or timestamp in %s" name))
            (when (and (>= timestamp cutoff)
                       (not (gethash message-id seen-messages)))
              (let* ((sender-id
                      (or (max-core-get msg :userId)
                          (deterred-messengers--import-key
                           "system" chat-id)))
                     (user (deterred-messengers--max-user
                            user-cache profiles self-id sender-id
                            (or (max-core-get msg :username) name)))
                     (sender-uuid (alist-get 'id user)))
                (puthash sender-uuid user users)
                (push `((id . ,(deterred-messengers--id
                                "max" (deterred-messengers--import-key
                                       chat-id message-id)))
                        (max_id . ,message-id)
                        (sender_id . ,sender-uuid)
                        (chat_id . ,chat-uuid)
                        (content . ,(or (max-core-get msg :text) ""))
                        (timestamp . ,timestamp)
                        (is_attachment . ,(if (seq-empty-p
                                               (max-core-get msg :attachments))
                                              0 1))
                        (messenger . "max"))
                      messages)))
            (puthash message-id t seen-messages)))
        (if (null page)
            (if (memq has-more '(nil :false false json-false :null))
                (setq finished t)
              (error "MAX returned an empty history page with more data in %s" name))
          (let* ((oldest (car page))
                 (cursor (max-core-get oldest :id))
                 (oldest-time (floor (max-core-time
                                      (max-core-get oldest :timestamp)))))
            (when (or (gethash cursor seen-cursors)
                      (and before-time (> oldest-time before-time)))
              (error "MAX history did not advance in %s" name))
            (setq finished
                  (or (< oldest-time cutoff)
                      (memq has-more '(nil :false false json-false :null))))
            (unless finished
              (puthash cursor t seen-cursors)
              (setq before cursor before-time oldest-time))))
        (cl-incf count
                 (deterred-messengers--store-chat
                  db "max" (hash-table-values users) chat-row
                  (nreverse messages) cutoff (unless finished before)))))
    count))

(defun deterred-messengers-load-max ()
  "Import all available MAX chats and message directions into DETERRED.

The first import downloads full history.  Subsequent imports revisit
the last imported day separately for each chat.  Interrupted imports
resume after their last saved page.  MAX must already be connected;
this command does not start or authenticate the client."
  (interactive)
  (unless (and (stringp deterred-messengers-my-id)
               (not (string-empty-p (string-trim deterred-messengers-my-id))))
    (user-error "Set deterred-messengers-my-id before importing MAX"))
  (let* ((snapshot (deterred-messengers--max-request "sync.initial"))
         (account (max-core-get snapshot :account))
         (self-id (max-core-get account :id))
         (chats (append (max-core-get snapshot :chats) nil))
         (profiles (make-hash-table :test #'equal)))
    (unless (and (stringp self-id) (not (string-empty-p self-id)))
      (user-error "MAX did not return the connected account ID"))
    (dolist (profile (append (max-core-get snapshot :users) nil))
      (puthash (max-core-get profile :id) profile profiles))
    (puthash self-id account profiles)
    (let* ((db (deterred-db--init))
           (user-cache (deterred-messengers--load-users-by-messenger db "max"))
           (chat-cache (deterred-messengers--load-chats-by-messenger db "max"))
           (count 0))
      (puthash self-id deterred-messengers-my-id user-cache)
      (cl-loop for chat in chats
               for index from 1
               do (progn
                    (message "Importing MAX chat %d/%d..." index (length chats))
                    (cl-incf count
                             (deterred-messengers--max-import-chat
                              db chat user-cache chat-cache profiles self-id))))
      (message "Successfully imported MAX data: %d messages" count))))

(defun deterred-messengers--rocket-start-timestamp (server)
  "Return SERVER's configured local-midnight import timestamp, or zero.
Reject malformed dates and impossible calendar dates before importing."
  (let* ((entry (seq-find
                 (lambda (item)
                   (equal (format "%s" (car item)) (format "%s" server)))
                 deterred-messengers-rocket-start-dates))
         (date (cdr entry)))
    (if (null date)
        0
      (save-match-data
        (unless (and (stringp date)
                     (string-match
                      "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)\\'"
                      date))
          (user-error "Invalid Rocket.Chat start date for %s: %S; expected YYYY-MM-DD"
                      server date))
        (let* ((year (string-to-number (match-string 1 date)))
               (month (string-to-number (match-string 2 date)))
               (day (string-to-number (match-string 3 date)))
               (time (condition-case nil
                         (encode-time 0 0 0 day month year)
                       (error nil)))
               (decoded (and time (decode-time time))))
          ;; encode-time accepts February 30 by rolling into March.  Require
          ;; the decoded calendar fields to match rather than normalizing it.
          (unless (and (> year 0) decoded
                       (= year (nth 5 decoded))
                       (= month (nth 4 decoded))
                       (= day (nth 3 decoded)))
            (user-error "Invalid Rocket.Chat calendar date for %s: %S" server date))
          (time-convert time 'integer))))))

(cl-defstruct (deterred-messengers--rocket-job
               (:constructor deterred-messengers--make-rocket-job))
  "A background Rocket import and its current progress."
  iterator timer request token self-id callback error resume-at
  (status 'running) (count 0) (retries 0) progress)

(defvar deterred-messengers--rocket-import-job nil
  "Current or most recent background Rocket import.")

(defun deterred-messengers--rocket-active-p (job)
  "Return non-nil if JOB is the current active Rocket import."
  (and job (eq job deterred-messengers--rocket-import-job)
       (memq (deterred-messengers--rocket-job-status job)
             '(running waiting requesting))))

(defun deterred-messengers--rocket-progress (job)
  "Describe the remaining chats in each workspace of JOB."
  (mapconcat
   (lambda (progress)
     (let ((server (plist-get progress :server))
           (total (plist-get progress :total))
           (completed (plist-get progress :completed))
           (chat (plist-get progress :chat)))
       (if total
           (format "%s: %d/%d chats complete, %d remaining%s"
                   server completed total (- total completed)
                   (if chat (format " (%s)" chat) ""))
         (format "%s: waiting for chat list" server))))
   (deterred-messengers--rocket-job-progress job) "; "))

(defun deterred-messengers-rocket-import-status ()
  "Show background Rocket import progress for every selected server."
  (interactive)
  (let ((job deterred-messengers--rocket-import-job))
    (unless job (user-error "No Rocket import has been started"))
    (message "Rocket import %s%s%s; %d messages saved. %s"
             (deterred-messengers--rocket-job-status job)
             (if-let ((deadline (deterred-messengers--rocket-job-resume-at job)))
                 (format " (resumes in %ds)" (ceiling (max 0 (- deadline (float-time)))))
               "")
             (if-let ((err (deterred-messengers--rocket-job-error job)))
                 (concat ": " (error-message-string err)) "")
             (deterred-messengers--rocket-job-count job)
             (deterred-messengers--rocket-progress job))))

(defun deterred-messengers--rocket-finish (job status &optional err)
  "Finish JOB with STATUS and optional error condition ERR."
  (when (deterred-messengers--rocket-active-p job)
    (when (deterred-messengers--rocket-job-timer job)
      (cancel-timer (deterred-messengers--rocket-job-timer job)))
    ;; Invalidate callbacks before closing the iterator or notifying the UI.
    (setf (deterred-messengers--rocket-job-status job) status
          (deterred-messengers--rocket-job-error job) err
          (deterred-messengers--rocket-job-timer job) nil
          (deterred-messengers--rocket-job-token job) nil
          (deterred-messengers--rocket-job-resume-at job) nil)
    (iter-close (deterred-messengers--rocket-job-iterator job))
    (setf (deterred-messengers--rocket-job-iterator job) nil
          (deterred-messengers--rocket-job-request job) nil)
    (message "Rocket import %s: %d messages saved%s. %s"
             status (deterred-messengers--rocket-job-count job)
             (if err (concat "; " (error-message-string err)) "")
             (deterred-messengers--rocket-progress job))
    (when-let ((callback (deterred-messengers--rocket-job-callback job)))
      (setf (deterred-messengers--rocket-job-callback job) nil)
      (funcall callback))))

(defun deterred-messengers-cancel-rocket-import ()
  "Cancel the background Rocket import, retaining all saved pages."
  (interactive)
  (unless (deterred-messengers--rocket-active-p deterred-messengers--rocket-import-job)
    (user-error "No Rocket import is running"))
  (deterred-messengers--rocket-finish deterred-messengers--rocket-import-job 'cancelled))

(defun deterred-messengers--rocket-step (job function args)
  "Run FUNCTION with JOB and ARGS, catching background failures."
  (when (deterred-messengers--rocket-active-p job)
    (setf (deterred-messengers--rocket-job-timer job) nil
          (deterred-messengers--rocket-job-resume-at job) nil
          (deterred-messengers--rocket-job-status job) 'running)
    (let ((deterred-messengers-my-id (deterred-messengers--rocket-job-self-id job)))
      (condition-case err
          (apply function job args)
        (quit (deterred-messengers--rocket-finish job 'cancelled))
        (error (deterred-messengers--rocket-finish job 'failed err))))))

(defun deterred-messengers--rocket-schedule (job delay function &rest args)
  "Run FUNCTION with JOB and ARGS after DELAY, returning to Emacs meanwhile."
  (when (deterred-messengers--rocket-active-p job)
    (setf (deterred-messengers--rocket-job-status job) (if (> delay 0) 'waiting 'running)
          (deterred-messengers--rocket-job-resume-at job)
          (when (> delay 0) (+ (float-time) delay))
          (deterred-messengers--rocket-job-timer job)
          (run-at-time delay nil #'deterred-messengers--rocket-step job function args))))

(defun deterred-messengers--rocket-request-failed (job payload)
  "Retry JOB's current request after a rate limit in PAYLOAD, or fail."
  (if (and (equal (plist-get payload :code) "http-429")
           (< (deterred-messengers--rocket-job-retries job)
              deterred-messengers-rate-limit-retries))
      (let ((delay (deterred-messengers--rate-limit-delay payload)))
        (cl-incf (deterred-messengers--rocket-job-retries job))
        (message "Rocket.Chat %s rate limit: resuming in %s seconds (retry %d/%d). %s"
                 (car (deterred-messengers--rocket-job-request job)) delay
                 (deterred-messengers--rocket-job-retries job)
                 deterred-messengers-rate-limit-retries
                 (deterred-messengers--rocket-progress job))
        (deterred-messengers--rocket-schedule
         job delay #'deterred-messengers--rocket-send))
    (deterred-messengers--rocket-finish job 'failed (list 'rocket-server-error payload))))

(defun deterred-messengers--rocket-send (job)
  "Send JOB's current request asynchronously, respecting request pacing."
  (let ((delay (- deterred-messengers-request-interval
                  (- (float-time)
                     (gethash 'rocket deterred-messengers--import-last-request 0)))))
    (if (> delay 0)
        (deterred-messengers--rocket-schedule job delay #'deterred-messengers--rocket-send)
      ;; Rocket's request API would otherwise start a stopped sidecar itself.
      (unless (rocket-server-running-p)
        (user-error "Rocket stopped during import; reconnect and rerun the import"))
      (let ((token (make-symbol "rocket-request")))
        (setf (deterred-messengers--rocket-job-token job) token
              (deterred-messengers--rocket-job-status job) 'requesting)
        (puthash 'rocket (float-time) deterred-messengers--import-last-request)
        (apply
         #'rocket-core-request
         (append
          (deterred-messengers--rocket-job-request job)
          (list
           (lambda (result)
             (when (and (deterred-messengers--rocket-active-p job)
                        (eq token (deterred-messengers--rocket-job-token job)))
               (setf (deterred-messengers--rocket-job-token job) nil)
               (deterred-messengers--rocket-schedule
                job 0 #'deterred-messengers--rocket-advance result)))
           (lambda (payload)
             (when (and (deterred-messengers--rocket-active-p job)
                        (eq token (deterred-messengers--rocket-job-token job)))
               (setf (deterred-messengers--rocket-job-token job) nil)
               (deterred-messengers--rocket-schedule
                job 0 #'deterred-messengers--rocket-request-failed payload))))))))))

(defun deterred-messengers--rocket-advance (job &optional result)
  "Give RESULT to JOB's iterator and schedule its next request."
  (condition-case nil
      (progn
        (setf (deterred-messengers--rocket-job-request job)
              (iter-next (deterred-messengers--rocket-job-iterator job) result)
              (deterred-messengers--rocket-job-retries job) 0)
        (deterred-messengers--rocket-schedule job 0 #'deterred-messengers--rocket-send))
    (iter-end-of-sequence (deterred-messengers--rocket-finish job 'complete))))

(iter-defun deterred-messengers--rocket-request (server method &optional params)
  "Yield a Rocket request for SERVER's METHOD with PARAMS to the job driver."
  (iter-yield (list server method params)))

(iter-defun deterred-messengers--rocket-inventory (server)
  "Return fresh (ROOM-ID ROOM SUBSCRIPTION) records for SERVER."
  (let* ((snapshot (iter-yield-from
                    (deterred-messengers--rocket-request server "sync.initial")))
         (rooms (make-hash-table :test #'equal))
         order)
    (dolist (kind '(:subscriptions :rooms))
      (let* ((response (rocket-core-get snapshot kind))
             (items (rocket-core-get response :update :missing)))
        (unless (or (listp items) (vectorp items))
          (error "Rocket returned an invalid %s inventory" kind))
        (seq-doseq (item items)
          (let* ((id (rocket-core-get item
                                      (if (eq kind :rooms) :_id :rid)))
                 (entry (and id (gethash id rooms))))
            (unless (and (stringp id) (not (string-empty-p id)))
              (error "Rocket returned a room without an ID"))
            (unless entry
              (setq entry (list id nil nil))
              (puthash id entry rooms)
              (push id order))
            (setf (nth (if (eq kind :rooms) 1 2) entry) item)))))
    ;; A hidden subscription may be absent from rooms.get; retain it without
    ;; admitting old rooms from Rocket's normalized cache.
    (mapcar (lambda (id) (gethash id rooms)) (nreverse order))))

(defun deterred-messengers--rocket-chat-data
    (server own-id entry user-cache chat-cache &optional server-name)
  "Convert SERVER room ENTRY into (USERS CHAT) for database insertion.
OWN-ID is the authenticated user.  USER-CACHE and CHAT-CACHE map scoped
Rocket identifiers to existing DETERRED UUIDs.  Groups share one chat named
SERVER-NAME, falling back to SERVER; personal chats retain room identities."
  (pcase-let* ((`(,room-id ,room ,subscription) entry)
               (room-type (or (rocket-core-get room :t)
                              (rocket-core-get subscription :t)))
               (members (delete-dups
                         (append (or (rocket-core-get room :uids)
                                     (rocket-core-get subscription :uids)) nil)))
               (others (remove own-id members))
               (member-count (or (rocket-core-get room :usersCount)
                                 (rocket-core-get subscription :usersCount)))
               ;; Rocket uses "d" for both personal and multi-user DMs.
               ;; Hidden subscriptions may omit membership entirely.
               (group-p (or (not (equal room-type "d"))
                            (> (length members) 2)
                            (> (length others) 1)
                            (and (numberp member-count) (> member-count 2))
                            (not (or members
                                     (and (numberp member-count)
                                          (> member-count 0)
                                          (<= member-count 2))))))
               (name (or (rocket-core-get subscription :fname)
                         (rocket-core-get room :fname)
                         (rocket-core-get subscription :name)
                         (rocket-core-get room :name)
                         room-id))
               (target nil)
               (users nil))
    (when (and (not group-p) (= (length others) 1))
      (let* ((other-id (car others))
             (profile (rocket-user-get server other-id))
             (user (deterred-messengers--get-or-create-user
                    user-cache "rocket"
                    (deterred-messengers--import-key
                     (format "%s" server) other-id)
                    (or (and profile (rocket-user-name profile))
                        (and profile (rocket-user-username profile))
                        (rocket-core-get subscription :fname)
                        (rocket-core-get subscription :name)
                        other-id))))
        (setq target (alist-get 'id user))
        (unless (equal target deterred-messengers-my-id)
          (push user users))))
    (when (and (not group-p) members (null others))
      (setq target deterred-messengers-my-id))
    (list users
          (deterred-messengers--get-or-create-chat
           chat-cache "rocket"
           (if group-p
               (deterred-messengers--import-key (format "%s" server))
             (deterred-messengers--import-key (format "%s" server) room-id))
           (if group-p (or server-name (format "%s" server)) name)
           (if group-p "group" "personal_chat") target))))

(iter-defun deterred-messengers--rocket-import-chat
    (db server own-id room-id users chat &optional user-cache start-timestamp job)
  "Import SERVER's messages in ROOM-ID into DB for CHAT.
OWN-ID is the authenticated user ID.  USERS and CHAT are insertion alists.
USER-CACHE maps scoped Rocket user identifiers to existing DETERRED UUIDs.
START-TIMESTAMP is the earliest allowed Unix timestamp, defaulting to the
date configured for SERVER in `deterred-messengers-rocket-start-dates'.
Search includes thread replies, both directions in personal chats, and only
sent messages in groups.  Save each page and its next offset together,
retaining the original cutoff until the chat is complete.  Resume
interrupted imports at their saved offset.
Update JOB's saved message count after each page and return the room count."
  (let* ((chat-id (alist-get 'id chat))
         (room-key (deterred-messengers--import-key (format "%s" server) room-id))
         (personal-p (equal (alist-get 'type chat) "personal_chat"))
         (user-cache (or user-cache
                         (deterred-messengers--load-users-by-messenger db "rocket")))
         (sender-users (make-hash-table :test #'equal))
         ;; If membership becomes available later, revisit received history
         ;; that was previously excluded by the conservative sent-only policy.
         (state (if (or (and personal-p
                             (not (equal (caar (sqlite-select
                                                db "SELECT type FROM messenger_chat WHERE id = ?"
                                                (list chat-id))) "personal_chat")))
                        (sqlite-select
                         db "SELECT 1 FROM messenger_message m
                             JOIN messenger_chat c ON c.id = m.chat_id
                             WHERE m.messenger = 'rocket' AND m.rocket_room_id = ?
                               AND c.type != ? LIMIT 1"
                         (list room-key (alist-get 'type chat))))
                    '((cutoff . 0) (cursor . nil))
                  (deterred-messengers--import-state
                   db "rocket" room-key chat-id)))
         (cutoff (max (alist-get 'cutoff state)
                      (or start-timestamp
                          (deterred-messengers--rocket-start-timestamp server))))
         (cursor (alist-get 'cursor state))
         (offset (if cursor (string-to-number cursor) 0))
         (seen (make-hash-table :test #'equal))
         (count 0)
         done)
    (dolist (user users)
      (puthash (alist-get 'rocket_id user) user sender-users))
    (when (and cursor
               (not (string-match-p "\\`[0-9]+\\'" cursor)))
      (error "Invalid saved Rocket history offset for room %s" room-id))
    (while (not done)
      (let* ((response
              (iter-yield-from (deterred-messengers--rocket-request
               server "message.search"
               `((room-id . ,room-id)
                 ;; The server consumes only "desc" from "descend", leaving
                 ;; "end" as a text query.  An order-only query returns no
                 ;; results, so use a date before Rocket existed for all DMs.
                 (text . ,(if personal-p "after:01/01/1970 order:desc"
                            "from:me order:desc"))
                 (offset . ,offset)
                 (count . 100)))))
             (page (rocket-core-get response :messages :missing))
             (total (rocket-core-get response :total))
             (new-count 0)
             (page-users (copy-sequence users))
             messages older)
        (unless (or (listp page) (vectorp page))
          (error "Rocket returned invalid message search results"))
        (seq-doseq (message page)
          (let* ((id (rocket-core-get message :_id))
                 (timestamp (rocket-core-time (rocket-core-get message :ts)))
                 (sender (rocket-core-get (rocket-core-get message :u) :_id)))
            (unless (and (stringp id) (not (string-empty-p id))
                         (> timestamp 0))
              (error "Rocket returned a message without an ID or timestamp"))
            (when (< timestamp cutoff)
              (setq older t))
            (unless (gethash id seen)
              (puthash id t seen)
              (cl-incf new-count)
              ;; Keep groups sent-only even if the server ignores from:me.
              (when (and (or personal-p (equal sender own-id))
                         (equal (rocket-core-get message :rid) room-id)
                         (>= timestamp cutoff))
                (unless (and (stringp sender) (not (string-empty-p sender)))
                  (error "Rocket returned a message without a sender ID"))
                (let* ((key (deterred-messengers--import-key
                             (format "%s" server) id))
                       (sender-id
                        (if (equal sender own-id)
                            deterred-messengers-my-id
                          (let* ((user-key (deterred-messengers--import-key
                                            (format "%s" server) sender))
                                 (user
                                  (or (gethash user-key sender-users)
                                      (let ((profile (rocket-user-get server sender))
                                            (author (rocket-core-get message :u)))
                                        (deterred-messengers--get-or-create-user
                                         user-cache "rocket" user-key
                                         (or (and profile (rocket-user-name profile))
                                             (rocket-core-get author :name)
                                             (and profile (rocket-user-username profile))
                                             (rocket-core-get author :username)
                                             sender))))))
                            (puthash user-key user sender-users)
                            (cl-pushnew user page-users :test #'equal)
                            (alist-get 'id user)))))
                  (push
                   `((id . ,(deterred-messengers--id "rocket" key))
                     (rocket_id . ,key)
                     (rocket_room_id . ,room-key)
                     (sender_id . ,sender-id)
                     (chat_id . ,chat-id)
                     (content . ,(or (rocket-core-get message :msg) ""))
                     (timestamp . ,(floor timestamp))
                     (is_attachment
                      . ,(if (or (not (seq-empty-p
                                       (rocket-core-get message :attachments)))
                                 (not (seq-empty-p
                                       (rocket-core-get message :files)))
                                 (rocket-core-get message :file))
                             1 0))
                     (messenger . "rocket"))
                   messages))))))
        (when (and (> (length page) 0) (= new-count 0))
          (error "Rocket message pagination made no progress in room %s" room-id))
        (cl-incf offset (length page))
        ;; A server can cap pages below the requested size.  Without a total,
        ;; continue until an empty page rather than truncating the import.
        (setq done (or (zerop (length page)) older
                       (and (numberp total) (>= offset total))))
        ;; Persist the unchanged cutoff alongside every page.  Otherwise the
        ;; newly stored latest message would hide unfinished older history on
        ;; restart.  Even a fully filtered page advances its remote offset.
        (let ((stored (deterred-messengers--store-chat
                       db "rocket" page-users chat (nreverse messages) cutoff
                       (unless done (number-to-string offset)) room-key)))
          (cl-incf count stored)
          (when job (cl-incf (deterred-messengers--rocket-job-count job) stored)))))
    count))

(defun deterred-messengers--rocket-room-excluded-p (server entry exclusions)
  "Return non-nil if SERVER's room ENTRY matches configured EXCLUSIONS."
  (let ((rooms (cdr (seq-find
                    (lambda (item)
                      (equal (format "%s" (car item)) (format "%s" server)))
                    exclusions))))
    (seq-some (lambda (value) (and (stringp value) (member value rooms)))
              (list (car entry)
                    (rocket-core-get (nth 1 entry) :name)
                    (rocket-core-get (nth 2 entry) :name)))))

(iter-defun deterred-messengers--rocket-import-run
    (job states start-times db excluded-rooms)
  "Import STATES into DB for JOB using START-TIMES and EXCLUDED-ROOMS."
  (let ((user-cache (deterred-messengers--load-users-by-messenger db "rocket"))
        (chat-cache (deterred-messengers--load-chats-by-messenger db "rocket")))
    (cl-loop
     for state in states
     for progress in (deterred-messengers--rocket-job-progress job)
     do
     (let ((server (rocket-server-state-id state))
           (server-name (rocket-server-state-name state))
           (own-id (rocket-server-state-user-id state)))
       (message "Rocket.Chat %s: fetching chat list..." server)
       (let ((entries (seq-remove
                       (lambda (entry)
                         (deterred-messengers--rocket-room-excluded-p
                          server entry excluded-rooms))
                       (iter-yield-from (deterred-messengers--rocket-inventory server)))))
         (setf (plist-get progress :total) (length entries))
         (dolist (entry entries)
           (pcase-let ((`(,users ,chat)
                        (deterred-messengers--rocket-chat-data
                         server own-id entry user-cache chat-cache server-name)))
             (setf (plist-get progress :chat)
                   (or (rocket-core-get (nth 2 entry) :name)
                       (rocket-core-get (nth 1 entry) :name) (car entry)))
             (message "Importing Rocket.Chat %s: chat %d/%d (%d remaining after this): %s..."
                      server (1+ (plist-get progress :completed)) (length entries)
                      (- (length entries) (plist-get progress :completed) 1)
                      (plist-get progress :chat))
             (iter-yield-from
               (deterred-messengers--rocket-import-chat
                db server own-id (car entry) users chat user-cache
                (alist-get server start-times nil nil #'equal) job))
             (cl-incf (plist-get progress :completed))
             (setf (plist-get progress :chat) nil))))))))

(defun deterred-messengers-load-rocket (&optional callback)
  "Start a background Rocket.Chat import from authenticated workspaces.
Include both directions in personal chats and only sent messages in groups.
Group messages share one chat per workspace; personal chats remain separate.
Use running rocket.el sessions without starting or logging into a client.
The first import retrieves all available history; subsequent imports refresh
each room from its last stored message minus one day, including replies.
Interrupted imports resume at their last saved page.  All imports respect
`deterred-messengers-rocket-start-dates' and
`deterred-messengers-rocket-excluded-rooms'.
Return the job immediately.  CALLBACK, when non-nil, is called without
arguments when the job completes, fails, or is cancelled.  Use
`deterred-messengers-rocket-import-status' to inspect progress and
`deterred-messengers-cancel-rocket-import' to stop the job."
  (interactive)
  (when (deterred-messengers--rocket-active-p deterred-messengers--rocket-import-job)
    (user-error "A Rocket import is already running; check its status or cancel it"))
  (unless (and (stringp deterred-messengers-my-id)
               (not (string-empty-p (string-trim deterred-messengers-my-id))))
    (user-error "Set deterred-messengers-my-id before importing Rocket.Chat"))
  (unless (and (featurep 'rocket-core)
               (fboundp 'rocket-server-running-p)
               (rocket-server-running-p))
    (user-error "Rocket is not running; start rocket.el before importing"))
  (let ((states
         (seq-filter
          (lambda (state)
            (and (stringp (rocket-server-state-user-id state))
                 (not (string-empty-p (rocket-server-state-user-id state)))
                 (memq (rocket-server-state-connection-state state)
                       '(authenticated connecting connected reconnecting ready))))
          (rocket-server-states))))
    (unless states
      (user-error "Rocket has no authenticated workspaces"))
    (let* ((start-times
            ;; Validate every selected workspace before any RPC or DB access.
            (mapcar (lambda (state)
                      (let ((server (rocket-server-state-id state)))
                        (cons server
                              (deterred-messengers--rocket-start-timestamp server))))
                    states))
           (db (deterred-db--init))
           (job (deterred-messengers--make-rocket-job
                 :self-id deterred-messengers-my-id :callback callback
                 :progress (mapcar (lambda (state)
                                     (list :server (rocket-server-state-id state)
                                           :total nil :completed 0 :chat nil))
                                   states))))
      (setf (deterred-messengers--rocket-job-iterator job)
            (deterred-messengers--rocket-import-run
             job states start-times db
             (copy-tree deterred-messengers-rocket-excluded-rooms)))
      (setq deterred-messengers--rocket-import-job job)
      (deterred-messengers--rocket-schedule job 0 #'deterred-messengers--rocket-advance)
      (message "Rocket.Chat import started in the background (%d servers)" (length states))
      job)))

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

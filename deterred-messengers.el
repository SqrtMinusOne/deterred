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
(require 'deterred-db)
(require 'deterred-source)
(require 'cl-lib)

(defconst deterred-messengers-uuid-namespace
  "d9536b80-3213-4321-b37f-ebf1b558a530")

(defun deterred-telegram--id (&rest ids)
  "Convert Telegram IDS into a unique UUID.

This is used to maintain cross-messenger uniqueness of the
idenitifers."
  (uuidgen-3 deterred-messengers-uuid-namespace
             (string-join (append
                           (list "telegram")
                           (mapcar (lambda (id)
                                     (if (numberp id)
                                         (number-to-string id)
                                       id))
                                   ids)
                           nil))))

(defun deterred-messengers--telegram-process-chat (db chat my-telegram-id)
  "Insert Telegram CHAT into DETERRED.

CHAT is an object from Telegram's JSON dump.  DB is a sqlite database
object, MY-TELEGRAM-ID is the Telegram ID of the user who made the
dump."
  (let* ((type (pcase (alist-get 'type chat)
                 ("personal_chat" "personal_chat")
                 (_ "group")))
         (chat-id (deterred-telegram--id (alist-get 'id chat)))
         (chat-object
          `((id . ,chat-id)
            (telegram_id . ,(alist-get 'id chat))
            (name . ,(alist-get 'name chat))
            (type . ,type)))
         (found-users (make-hash-table :test #'equal))
         messages)
    (cl-mapc
     (lambda (message)
       (when (equal (alist-get 'type message) "message")
         (let* ((sender-telegram-id
                 (string-to-number
                  (string-replace "user" "" (alist-get 'from_id message))))
                (sender-name (alist-get 'from message))
                (sender-id (deterred-telegram--id sender-telegram-id))
                (timestamp (string-to-number (alist-get 'date_unixtime message)))
                (id (deterred-telegram--id chat-id (alist-get 'id message)))
                (telegram-id (alist-get 'id message))
                (content (if (stringp (alist-get 'text message))
                             (alist-get 'text message)
                           (mapconcat
                            (lambda (val)
                              (if (stringp val) val (alist-get 'text val)))
                            (alist-get 'text message))))
                (is-attachment (if (null (alist-get 'file message)) 0 1)))
           (push `((id . ,id)
                   (telegram_id . ,telegram-id)
                   (sender_id . ,sender-id)
                   (chat_id . ,chat-id)
                   (content . ,content)
                   (timestamp . ,timestamp)
                   (is_attachment . ,is-attachment))
                 messages)
           (puthash
            sender-id
            `((id . ,sender-id)
              (name . ,sender-name)
              (telegram_id . ,sender-telegram-id))
            found-users)
           (when (and (equal type "personal_chat")
                      (not (equal my-telegram-id sender-telegram-id)))
             (setf (alist-get 'name chat-object) sender-name)))))
     (alist-get 'messages chat))
    (when (and (> (seq-length messages) 0)
               (> (hash-table-count found-users) 0))
      (when (equal type "personal_chat")
        (let ((target-user-id (deterred-telegram--id (alist-get 'id chat))))
          (when (gethash target-user-id found-users)
            (setf (alist-get 'target_user_id chat-object)
                  target-user-id))))
      (deterred-db-insert-unsafe
       db :table-name 'messenger_user
       :values (hash-table-values found-users)
       :conflict-action 'do-update
       :conflict-attrs '(id))
      (deterred-db-insert-unsafe
       db :table-name 'messenger_chat
       :values (list chat-object)
       :conflict-action 'do-update
       :conflict-attrs '(id))
      (deterred-db-insert-unsafe
       db :table-name 'messenger_message
       :values messages
       :conflict-action 'do-update
       :conflict-attrs '(id)))))

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
         (db (deterred-db--init)))
    (with-sqlite-transaction db
      (let ((total (seq-length (alist-get 'list (alist-get 'chats data)))))
        (cl-loop for chat across (alist-get 'list (alist-get 'chats data))
                 for i from 0
                 do (deterred-messengers--telegram-process-chat db chat my-telegram-id)
                 do (message "Processed %s/%s chats" i total)))
      (deterred-db--mark-update-batch
       db
       '(messenger_chat messenger_user messenger_message)))))

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

(cl-defmethod deterred-source-actions ((_source deterred-messengers) &optional callback)
  "Run an action for the messengers source.

Run CALLBACK when done."
  (deterred-source--actions-pick
   '(("Load Telegram JSON" deterred-messengers-load-telegram-json nil))
   callback))

(provide 'deterred-messengers)
;;; deterred-messengers.el ends here

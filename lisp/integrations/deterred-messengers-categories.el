;;; deterred-messengers-categories.el --- Messenger chain extraction for DETERRED -*- lexical-binding: t -*-

;; Copyright (C) 2026 Korytov Pavel

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
(require 'llm)

(require 'deterred-db)
(require 'deterred-format)
(require 'deterred-messengers)

(defcustom deterred-messengers-categories-alist
  '(("education" . "Messages related to our learning activities")
    ("teaching" . "Messages related to conducting and managing the education process")
    ("work" . "Messages clearly related to professional work, mostly programming or technical topics. Don't choose if in doubt.")
    ("personal" . "Anything else not clearly related to another category and various personal matters"))
  "Alist of categories for classifying ambigous message sequences.

;; This must include all categories in ambigous messenger_chat chats,
i.e., ones with the category field like a|b|c."
  :type '(alist :key-type string :value-type string)
  :group 'deterred-sources)

(defcustom deterred-messengers-categories-llm-provider nil
  "An LLM provider instance for categorising ambigous chats."
  :group 'deterred-sources)

(defun deterred-messengers-categories--assign-by-chat (db)
  "Assign categories by the category column for unambigous chats.

DB is the SQLite database object."
  (sqlite-execute
   db
   "UPDATE messenger_message
SET category = (SELECT category FROM messenger_chat mc WHERE chat_id = mc.id)
WHERE chat_id IN (SELECT id FROM messenger_chat WHERE category IS NOT NULL AND category NOT LIKE \"%|%\")
AND chain_id is not null"))

(defun deterred-messengers-categories--fetch-sequences (db all chat-id)
  "Fetch all message sequences for CHAT-ID.

DB is the SQLite object.  If ALL is non-nil, fetch all messages;
otherwise, fetch only ones with unset category.

A sequence is a list of messages with at most
`deterred-messengers-chains-message-gap' gap between them and in which
I had participated.

The return value is a list of cons cells:
- sequence_id
- messages: a list of lists
  - sender name or \"Me\"
  - content"
  (let ((data (deterred-db-select-template-alist
               db
               "SELECT
  m.content,
  m.timestamp,
  mmc.sequence_id,
  mu.name sender,
  mu.id sender_id
FROM messenger_message m
INNER JOIN messenger_message_chain mmc ON mmc.id = m.chain_id
INNER JOIN messenger_user mu ON mu.id = m.sender_id
WHERE m.chat_id = :chat_id AND CASE when :all = 1 THEN 1 ELSE m.category IS NULL END
ORDER BY m.timestamp DESC"
               `((:all . ,(if all 1 0))
                 (:chat_id . ,chat-id)))))
    (thread-last
      data
      (seq-group-by
       (lambda (m) (alist-get 'sequence_id m)))
      (mapcar
       (lambda (sequence)
         (cons (car sequence)
               (mapcar
                (lambda (m)
                  (list (if (equal (alist-get 'sender_id m) deterred-messengers-my-id)
                            "Me"
                          (alist-get 'sender m))
                        (alist-get 'content m)))
                (seq-sort-by
                 (lambda (m) (alist-get 'timestamp m))
                 #'<
                 (cdr sequence)))))))))

(defun deterred-messengers-categories--format-prompt (sequence categories)
  "Format prompt to classify SEQUENCE by CATEGORIES.

SEQUENCE is a list of lists like:
- sender name
- message contents.

CATEGORIES is a list of strings, all of which must be keys
`deterred-messengers-categories-alist'."
  (deterred-format
   "Task: Classify the following message sequence into one of the categories.\n"
   "If unclear, prefer the first categories in the list.\n"
   "Output just the category name. No explanation, questions, just the category.\n"
   (f-mapconcat
    (lambda (category)
      (if-let ((description
                (alist-get category deterred-messengers-categories-alist
                           nil nil #'equal)))
          (concat "- " category ": " description)
        (user-error
         "Unknown category %s, configure `deterred-messengers-categories-alist'"
         category)))
    categories
    "\n")
   "\n"
   "Messages:\n"
   (f-mapconcat
    (lambda (msg)
      (concat
       (nth 0 msg)
       ": "
       (replace-regexp-in-string "[^[:print:]\n]" "" (nth 1 msg))))
    sequence)
   "\n"
   "Category:"))

(defun deterred-messengers-categories--debug-prompt (chat-id db all max-prompts)
  "Debug prompt generation for categorising an ambigous chat.

CHAT-ID is the chat id, DB is the SQLite connection object.  If ALL is
nil, return only uncategories messages.  Create a buffer with
MAX-PROMPTS messages sequences."
  (interactive
   (let* ((db (deterred-db--init))
          (chats (mapcar
                  (lambda (c)
                    (cons (alist-get 'name c) (alist-get 'id c)))
                  (deterred-db-select-alist
                   db "SELECT id, name FROM messenger_chat WHERE category LIKE \"%|%\""))))
     (list
      (alist-get (completing-read "Chat: " chats) chats nil nil #'equal)
      db
      (not (y-or-n-p "Exclude messages with configured categories?"))
      (read-number "Max prompts: " 10))))
  (let* ((chat (car (deterred-db-select-alist
                     db "SELECT * FROM messenger_chat WHERE id = ?" (list chat-id))))
         (category-string (alist-get 'category chat)))
    (unless (and (stringp category-string)
                 (string-match-p (rx  "|") category-string))
      (user-error "Please set a |-separated category for chat %s" chat-id))
    (let* ((categories (string-split category-string "|" t))
           (sequences (deterred-messengers-categories--fetch-sequences
                       db all chat-id))
           (buffer (generate-new-buffer "*prompt-debug*")))
      (with-current-buffer buffer
        (cl-loop for i from 0 to max-prompts
                 for (sequence-id . sequence) in sequences
                 for prompt = (deterred-messengers-categories--format-prompt
                               sequence categories)
                 do (insert "Sequence: " sequence-id "\n"
                            prompt
                            "\n---\n"))
        (goto-char (point-min))
        (special-mode))
      (display-buffer buffer))))

(defun deterred-messengers-categories--query-category (prompt callback)
  "Query an LLM with PROMPT category.

Call CALLBACK with the result or nil, if unsuccessful."
  (llm-chat-async deterred-messengers-categories-llm-provider
                  (llm-make-chat-prompt prompt :reasoning 'none :max-tokens 50)
                  (lambda (response)
                    (let ((cand (string-trim response)))
                      (if (alist-get cand deterred-messengers-categories-alist
                                     nil nil #'equal)
                          (funcall callback cand)
                        (funcall callback nil))))
                  (lambda (err msg)
                    (message "LLM generation error: %s %s" msg err))))

(defun deterred-messengers-categories--process-chat-recursively
    (sequences categories start-time i total chat-name retry callback)
  "Recursively process message SEQUENCES to assign them CATEGORIES.

START-TIME is the processing start time, I is the current message
index of TOTAL.  SEQUENCES gets `cdr'-ed on each iteration.  RETRY is
the retry count, if it's more than 3, stop retrying to process the
current sequence.  CHAT-NAME is the chat name.

Call CALLBACK when done."
  (if (seq-empty-p sequences)
      (progn
        (message "Processing %s done" chat-name)
        (funcall callback))
    (let* ((sequence-id (caar sequences))
           (sequence (cdar sequences))
           (prompt (deterred-messengers-categories--format-prompt
                    sequence categories))
           (eta (/
                 (*
                  (/ (float (- (time-convert nil 'integer) start-time)) i) (- total i))
                 60)))
      (message "Processing %s: %s/%s (ETA: %s)" chat-name i total
               (condition-case err
                   (org-duration-from-minutes eta)
                 (error "?")))
      (deterred-messengers-categories--query-category
       prompt
       (lambda (category)
         (if (null category)
             (if (> retry 3)
                 (progn
                   (message "Can't determine category for %s" sequence-id)
                   (deterred-messengers-categories--process-chat-recursively
                    (cdr sequences) categories start-time (1+ i) total chat-name
                    0 callback))
               (deterred-messengers-categories--process-chat-recursively
                sequences categories start-time i total chat-name
                (1+ retry) callback))
           (let ((db (deterred-db--init)))
             (sqlite-execute
              db "UPDATE messenger_message
SET category = ?
WHERE chain_id IN (SELECT id FROM messenger_message_chain WHERE sequence_id = ?)"
              (list category sequence-id)))
           (deterred-messengers-categories--process-chat-recursively
            (cdr sequences) categories start-time (1+ i) total chat-name
            0 callback)))))))

(defun deterred-messengers-categories-process-chat (chat-id db all &optional callback)
  "Assign categories to messages in CHAT-ID.

CHAT-ID is the chat id, DB is the SQLite connection object.  If ALL is
nil, process only uncategoried messages.  Call optional CALLBACK when
done."
  (interactive
   (let* ((db (deterred-db--init))
          (chats (mapcar
                  (lambda (c)
                    (cons (alist-get 'name c) (alist-get 'id c)))
                  (deterred-db-select-alist
                   db "SELECT id, name FROM messenger_chat WHERE category LIKE \"%|%\""))))
     (list
      (alist-get (completing-read "Chat: " chats) chats nil nil #'equal)
      db
      (not (y-or-n-p "Exclude messages with configured categories?")))))
  (let* ((chat (car (deterred-db-select-alist
                     db "SELECT * FROM messenger_chat WHERE id = ?" (list chat-id))))
         (category-string (alist-get 'category chat)))
    (unless (and (stringp category-string)
                 (string-match-p (rx  "|") category-string))
      (user-error "Please set a |-separated category for chat %s" chat-id))
    (let* ((categories (string-split category-string "|" t))
           (sequences
            (deterred-messengers-categories--fetch-sequences
             db all chat-id)))
      (deterred-messengers-categories--process-chat-recursively
       sequences categories (time-convert nil 'integer) 0 (seq-length sequences)
       (alist-get 'name chat) 0
       (lambda ()
         (when callback
           (funcall callback)))))))

(defun deterred-messengers-categories-process-all-chats (&optional chats recurse)
  "Process all chat categories in DETERRED.

CHATS is a recursive parameter.  RECURSE is t when recursion is happening."
  (interactive)
  (let ((db (deterred-db--init)))
    (when (and (not chats) (not recurse))
      (setq chats (deterred-db-select-alist
                   db "SELECT id FROM messenger_chat WHERE category LIKE \"%|%\"")))
    (unless recurse
      (message "Processing unambigous chats...")
      (deterred-messengers-categories--assign-by-chat db))
    (when chats
      (deterred-messengers-categories-process-chat
       (alist-get 'id (car chats)) db nil
       (lambda ()
         (deterred-messengers-categories-process-all-chats (cdr chats) t))))))

(provide 'deterred-messengers-categories)
;;; deterred-messengers-categories.el ends here

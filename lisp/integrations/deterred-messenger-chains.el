;;; deterred-messenger-chains.el --- Messenger chain extraction for DETERRED -*- lexical-binding: t -*-

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

;; Extract message chains from messenger data to estimate time spent
;; on messengers.
;;
;; A message chain is a burst of conversation activity.  Chains are
;; extracted per-chat, then intertwined across chats at individual
;; message granularity to prevent temporal overlap.

;;; Code:
(require 'cl-lib)
(require 'uuidgen)
(require 'deterred-db)
(require 'deterred-messengers)
(require 'deterred-utils)

(defcustom deterred-messenger-chains-message-gap 300
  "Maximum gap in seconds between consecutive messages in the same chain.
Default is 300 (5 minutes)."
  :type 'integer
  :group 'deterred)

(defcustom deterred-messenger-chains-lookback 300
  "Seconds to look back before my first message.

For personal chats started by the other person, time prepended before
my first message.  For group chats, time prepended before my first
message.  Default is 300 (5 minutes)."
  :type 'integer
  :group 'deterred)

(defcustom deterred-messenger-chains-typing-speed 5
  "Characters per second for typing time estimation.

Used to estimate how long before my first message I started typing,
for personal chat chains that I started.  Default is 5 chars/sec."
  :type 'number
  :group 'deterred)

(defun deterred-messenger-chains--get-chat-messages (db chat-id)
  "Fetch all messages for CHAT-ID from DB, ordered by timestamp.

DB is a SQLite connection object.

CHAT-ID is a messenger chat UUID string from the `messenger_chat'
table.

Return a list of message alists ordered by ascending `timestamp'.
Each alist contains these keys:

- id
- sender_id
- content
- timestamp
- is_attachment"
  (deterred-db-select-alist
   db
   "SELECT id, sender_id, content, timestamp, is_attachment
    FROM messenger_message
    WHERE chat_id = ?
    ORDER BY timestamp ASC"
   (list chat-id)))

(defun deterred-messenger-chains--extract-raw-chains (messages gap)
  "Split MESSAGES into raw chains separated by gaps larger than GAP seconds.

MESSAGES must be a list of message alists as returned by
`deterred-messenger-chains--get-chat-messages', already sorted by
ascending `timestamp'.

GAP is an integer number of seconds.  Whenever the difference between
two consecutive message timestamps is greater than GAP, a new chain is
started.

Return a list of raw chains.  Each raw chain is a list of the original
message alists, preserving order.  The return shape is therefore:

  ((MSG-1 MSG-2 ...)
   (MSG-N ...))

where each MSG is an alist from MESSAGES (as returned by
`deterred-messenger-chains--get-chat-messages').  Return nil when
MESSAGES is nil.

These raw chains are temporary candidate conversation bursts.  Later
processing applies personal-chat or group-chat heuristics to each raw
chain separately before preserving the retained entries as distinct raw
sequences."
  (when messages
    (let ((chains nil)
          (current-chain (list (car messages)))
          (prev-ts (alist-get 'timestamp (car messages))))
      (dolist (msg (cdr messages))
        (let ((ts (alist-get 'timestamp msg)))
          (if (> (- ts prev-ts) gap)
              (progn
                (push (nreverse current-chain) chains)
                (setq current-chain (list msg)))
            (push msg current-chain))
          (setq prev-ts ts)))
      (push (nreverse current-chain) chains)
      (nreverse chains))))

(defun deterred-messenger-chains--make-sequence-id (chat-id chain)
  "Return a stable sequence id for raw CHAIN in CHAT-ID.

CHAT-ID is the chat UUID string.

CHAIN is a raw chain as returned by
`deterred-messenger-chains--extract-raw-chains'."
  (uuidgen-3
   deterred-messengers-uuid-namespace
   (concat "messenger-sequence:"
           chat-id
           ":"
           (alist-get 'id (car chain)))))

(defun deterred-messenger-chains--msgs-to-entries
    (messages chat-id sequence-id adjusted-start)
  "Convert MESSAGES to normalize entries with CHAT-ID and SEQUENCE-ID.

MESSAGES is a non-empty list of message alists in ascending timestamp
order, as produced by `deterred-messenger-chains--extract-raw-chains'.

CHAT-ID is the chat UUID string shared by all messages in MESSAGES.
SEQUENCE-ID is the UUID string of the raw chain from which MESSAGES was
derived.

ADJUSTED-START is an integer UNIX timestamp to use as the start of the
first returned entry.  All later entries start at their own message
timestamp.

Return a list of normalized entries suitable for
`deterred-utils-normalize-by-timeout'.  Each entry has the form:

  (START END (CHAT-ID SEQUENCE-ID MSG-ID))

where START and END are integer UNIX timestamps, CHAT-ID is the chat
UUID string, SEQUENCE-ID is the raw-chain UUID string, and MSG-ID is
the UUID string of the message represented by that entry.

The first entry spans from ADJUSTED-START to the timestamp of the
first message.  Every later entry initially has the form
`(TIMESTAMP nil (CHAT-ID SEQUENCE-ID MSG-ID))', letting the
normalization step fill in the effective end time."
  (let ((entries nil))
    (dolist (msg messages)
      (let ((ts (alist-get 'timestamp msg))
            (msg-id (alist-get 'id msg)))
        (push (list ts nil (list chat-id sequence-id msg-id)) entries)))
    (setq entries (nreverse entries))
    ;; Adjust first entry's start
    (when entries
      (let ((first-entry (car entries)))
        (setf (nth 0 first-entry) adjusted-start)
        (setf (nth 1 first-entry) (alist-get 'timestamp (car messages)))))
    entries))

(defun deterred-messenger-chains--process-personal-chain
    (chain my-id chat-id sequence-id)
  "Process a raw CHAIN under personal chat rules.

CHAIN is one raw chain as returned by
`deterred-messenger-chains--extract-raw-chains': a list of message
alists from one personal chat, ordered by ascending timestamp.

MY-ID is the user's UUID string.  CHAT-ID is the chat UUID string.
SEQUENCE-ID is the UUID string assigned to CHAIN.

If the first message in CHAIN is mine, keep the full chain and prepend
estimated typing time before the first message.  If somebody else
started the chain, keep only the part from
`deterred-messenger-chains-lookback' seconds before my first message
onward.

Return a list of normalized entries in the format produced by
`deterred-messenger-chains--msgs-to-entries', or nil when CHAIN
contains no message sent by MY-ID."
  (let ((has-my-message (cl-some (lambda (msg)
                                   (equal (alist-get 'sender_id msg) my-id))
                                 chain)))
    (when has-my-message
      (let* ((first-msg (car chain))
             (first-sender (alist-get 'sender_id first-msg)))
        (if (equal first-sender my-id)
            ;; I started: all messages, prepend typing time
            (let* ((content (alist-get 'content first-msg))
                   (content-len (if (stringp content) (length content) 0))
                   (typing-time (if (> deterred-messenger-chains-typing-speed 0)
                                    (ceiling (/ (float content-len)
                                                deterred-messenger-chains-typing-speed))
                                  0))
                   (adjusted-start (- (alist-get 'timestamp first-msg) typing-time)))
              (deterred-messenger-chains--msgs-to-entries
               chain chat-id sequence-id adjusted-start))
          ;; Other started: filter to lookback before my first message
          (let* ((my-first-msg (cl-find-if
                                (lambda (msg)
                                  (equal (alist-get 'sender_id msg) my-id))
                                chain))
                 (my-first-ts (alist-get 'timestamp my-first-msg))
                 (threshold (- my-first-ts deterred-messenger-chains-lookback))
                 (filtered (seq-filter
                            (lambda (msg)
                              (>= (alist-get 'timestamp msg) threshold))
                            chain)))
            (when filtered
              (deterred-messenger-chains--msgs-to-entries
               filtered chat-id sequence-id threshold))))))))

(defun deterred-messenger-chains--process-group-chain
    (chain my-id chat-id sequence-id)
  "Process a raw CHAIN under group chat rules.

CHAIN is one raw chain as returned by
`deterred-messenger-chains--extract-raw-chains': a list of message
alists from one group chat, ordered by ascending timestamp.

MY-ID is the user's UUID string.  CHAT-ID is the chat UUID string.
SEQUENCE-ID is the UUID string assigned to CHAIN.

Keep only the part of CHAIN that starts
`deterred-messenger-chains-lookback' seconds before my first message
and ends at my last message in the chain.

Return a list of normalized entries in the format produced by
`deterred-messenger-chains--msgs-to-entries', or nil when CHAIN
contains no message sent by MY-ID."
  (let ((my-messages (seq-filter
                      (lambda (msg)
                        (equal (alist-get 'sender_id msg) my-id))
                      chain)))
    (when my-messages
      (let* ((my-first-ts (alist-get 'timestamp (car my-messages)))
             (my-last-ts (alist-get 'timestamp (car (last my-messages))))
             (threshold (- my-first-ts deterred-messenger-chains-lookback))
             (filtered (seq-filter
                        (lambda (msg)
                          (let ((ts (alist-get 'timestamp msg)))
                            (and (>= ts threshold)
                                 (<= ts my-last-ts))))
                        chain)))
        (when filtered
          (deterred-messenger-chains--msgs-to-entries
           filtered chat-id sequence-id threshold))))))

(defun deterred-messenger-chains--process-chat (db chat)
  "Process one CHAT and return raw-chain entries for intertwining.

CHAT is an alist selected from `messenger_chat' with at least:
- id
- type

DB is the SQLite connection object.

Fetch all messages for CHAT, split them into raw per-chat chains, and
apply either personal-chat or group-chat filtering rules.

Each retained raw chain gets its own `sequence_id'.  The raw chain
grouping is preserved in the return value so later normalization can
trim overlap without losing the original per-conversation boundaries.

Return a list of normalized-entry lists, one list per retained raw
chain.  Each entry has the form:

  (START END (CHAT-ID SEQUENCE-ID MSG-ID))

Return nil when CHAT produces no retained raw chains."
  (let* ((chat-id (alist-get 'id chat))
         (chat-type (alist-get 'type chat))
         (messages (deterred-messenger-chains--get-chat-messages db chat-id))
         (raw-chains (deterred-messenger-chains--extract-raw-chains
                      messages
                      deterred-messenger-chains-message-gap))
         (my-id deterred-messengers-my-id)
         (process-fn (if (equal chat-type "personal_chat")
                       #'deterred-messenger-chains--process-personal-chain
                       #'deterred-messenger-chains--process-group-chain))
         result)
    (dolist (chain raw-chains)
      (let ((sequence-id (deterred-messenger-chains--make-sequence-id
                          chat-id chain)))
        (when-let* ((entries (funcall process-fn
                                      chain my-id chat-id sequence-id)))
          (push entries result))))
    (nreverse result)))

(defun deterred-messenger-chains--save (db all-chains)
  "Delete existing chains and save ALL-CHAINS to DB.

DB is the SQLite connection object.

ALL-CHAINS must be the output of
`deterred-utils-normalize-by-timeout'.  Its shape is:

  (((START END (CHAT-ID SEQUENCE-ID MSG-ID ...))
    ...)
   ...)

Each outer list element corresponds to one raw conversation sequence
after normalization.  Each inner entry describes a contiguous interval
assigned to one chat, where START and END are integer UNIX timestamps,
CHAT-ID is a chat UUID string, SEQUENCE-ID identifies the original raw
chain, and MSG-ID values are the UUID strings of messages covered by
that interval.

This function clears existing `messenger_message_chain' rows, inserts
new chain records, and updates `messenger_message.chain_id' for the
messages referenced by ALL-CHAINS."
  ;; Clear existing
  (message "Clearing existing chain assignments...")
  (sqlite-execute db "UPDATE messenger_message SET chain_id = NULL")
  (sqlite-execute db "DELETE FROM messenger_message_chain")
  ;; Collect chains and messages
  (message "Collecting chain data...")
  (let (chain-values
        msg-updates)
    (dolist (chat-chain all-chains)
      (dolist (entry chat-chain)
        (let* ((start (nth 0 entry))
               (end (nth 1 entry))
               (data (nth 2 entry))
               (chat-id (nth 0 data))
               (sequence-id (nth 1 data))
               (msg-ids (cddr data))
               (id (uuidgen-3 deterred-messengers-uuid-namespace
                              (concat sequence-id (number-to-string start)))))
          (push `((id . ,id)
                  (chat_id . ,chat-id)
                  (sequence_id . ,sequence-id)
                  (timestamp_start . ,start)
                  (timestamp_end . ,end))
                chain-values)
          (push (cons id msg-ids) msg-updates))))
    ;; Insert chains
    (when chain-values
      (message "Inserting %d chains..." (length chain-values))
      (deterred-db-insert-unsafe
       db :table-name 'messenger_message_chain
       :values (nreverse chain-values)
       :conflict-action 'do-update
       :conflict-attrs '(id)))
    ;; Assign chain_id to messages by collected IDs
    (let ((total (length msg-updates))
          (i 0))
      (dolist (update msg-updates)
        (let ((chain-id (car update))
              (msg-ids (cdr update)))
          (when msg-ids
            (sqlite-execute
             db
             (format "UPDATE messenger_message SET chain_id = ? WHERE id IN (%s)"
                     (mapconcat (lambda (_) "?") msg-ids ","))
             (cons chain-id msg-ids))))
        (cl-incf i)
        (when (= (% i 500) 0)
          (message "Assigning messages to chains... %d/%d" i total))))))

(defun deterred-messenger-chains-compute (&optional db)
  "Compute messenger chains and store them in DB.

DB is an optional SQLite connection object.  When nil, initialize one
with `deterred-db--init'.

Fetch all chats from `messenger_chat', derive raw per-chat chains from
their messages, apply personal-chat or group-chat filtering rules, and
then pass the resulting normalized entries to
`deterred-utils-normalize-by-timeout' to remove overlap across chats.

The raw chains are preserved as separate sequences.  Each raw chain
gets its own `sequence_id', and normalization trims overlap while
keeping those sequence boundaries intact.

The normalization input is a list shaped like:

  (((START END (CHAT-ID SEQUENCE-ID MSG-ID)) ...)
   ...)

and the normalized result is a list of normalized raw sequences shaped
like:

  (((START END (CHAT-ID SEQUENCE-ID MSG-ID ...)) ...)
   ...)

Store the final chains in the database via
`deterred-messenger-chains--save' and mark the affected tables as
updated.  This function is called for side effects and returns the
result of the final `message' call."
  (interactive)
  (deterred-utils-assert-var-set deterred-messengers-my-id)
  (let* ((db (or db (deterred-db--init)))
         (chats (deterred-db-select-alist
                 db "SELECT id, type FROM messenger_chat"))
         (all-sequences nil)
         (total (length chats)))
    ;; Step 1: Per-chat processing
    (cl-loop for chat in chats
             for i from 1
             for chat-sequences = (deterred-messenger-chains--process-chat db chat)
             when chat-sequences
             do (setq all-sequences (nconc all-sequences chat-sequences))
             when (= (% i 25) 0)
             do (message "Processed %d/%d chats for chains..." i total))
    (message "Extracted %d raw chains, intertwining..." (length all-sequences))
    ;; Step 2: Intertwine
    (let ((normalized (deterred-utils-normalize-by-timeout
                       all-sequences
                       deterred-messenger-chains-message-gap
                       (lambda (a b)
                         (append (list (nth 0 a) (nth 1 a))
                                 (cddr a)
                                 (cddr b))))))
      (message "Saving...")
      ;; Step 3: Save
      (with-sqlite-transaction db
        (deterred-messenger-chains--save db normalized)
        (deterred-db-mark-updated-batch
         db '(messenger_message_chain messenger_message)))
      (let ((total-chains (apply #'+ (mapcar #'length normalized))))
        (message "Saved %d chains across %d raw sequences"
                 total-chains (length normalized))))))

(defun deterred-messenger-chains-compute-hostname (&optional db)
  "Update the hostname attribute in messages.

DB is an optional SQLite connection object.  When nil, initialize one
with `deterred-db--init'.

First mark all `messenger_message' rows whose timestamps fall inside
the overall ActivityWatch not-afk range as \"<mobile>\" and clear the
rest.  Then join messages against `activitywatch_notafk_period' and
replace \"<mobile>\" with the concrete `hostname' of each matching
period.

This function updates the database in place and returns the result of
the final `message' call."
  (interactive)
  (let* ((db (or db (deterred-db--init)))
         (borders
          (car
           (deterred-db-select-alist
            db
            "SELECT
               min(notafk_start_timestamp) start,
               max(notafk_end_timestamp) end
             FROM activitywatch_notafk_period"))))
    (with-sqlite-transaction db
      ;; Step 1: Set hostname based on whether timestamp is in tracked range
      (message "Marking messages...")
      (sqlite-execute
       db
       "UPDATE messenger_message SET hostname = CASE
          WHEN timestamp >= ? AND timestamp <= ? THEN '<mobile>'
          ELSE null
        END"
       (list
        (alist-get 'start borders)
        (alist-get 'end borders)))
      ;; Step 2: Build temp table with matches via join
      (message "Joining messages with notafk periods...")
      (sqlite-execute db "DROP TABLE IF EXISTS matched_hostname")
      (sqlite-execute
       db
       "CREATE TEMP TABLE matched_hostname AS
        SELECT m.id, a.hostname
        FROM messenger_message m
        JOIN activitywatch_notafk_period a
          ON m.timestamp >= a.notafk_start_timestamp
         AND m.timestamp <= a.notafk_end_timestamp")
      (sqlite-execute
       db
       "CREATE INDEX temp.idx_matched_id ON matched_hostname (id)")
      ;; Step 3: Overwrite matched messages with actual hostname
      (message "Updating matched messages...")
      (sqlite-execute
       db
       "UPDATE messenger_message
        SET hostname = (SELECT h.hostname FROM matched_hostname h
                        WHERE h.id = messenger_message.id)
        WHERE id IN (SELECT id FROM matched_hostname)")
      ;; Cleanup
      (sqlite-execute db "DROP TABLE IF EXISTS matched_hostname")
      (message "Hostname assignment complete."))))

(provide 'deterred-messenger-chains)
;;; deterred-messenger-chains.el ends here

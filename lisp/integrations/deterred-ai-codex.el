;;; deterred-ai-codex.el --- Streaming Codex usage parser  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Korytov Pavel

;; Author: Korytov Pavel <thexcloud@gmail.com>
;; Maintainer: Korytov Pavel <thexcloud@gmail.com>
;; Homepage: https://github.com/SqrtMinusOne/deterred.el

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; This module parses Codex rollout JSONL without performing database or
;; pricing operations.  `deterred-ai-codex-parse-jsonl' supports both a
;; complete parse and an append-only parse resumed from a JSON-serializable
;; state and byte offset.

;;; Code:

(require 'cl-lib)
(require 'iso8601)
(require 'json)
(require 'seq)
(require 'subr-x)

(defconst deterred-ai-codex-parser-version "2"
  "Version of the persisted Codex parser state and output semantics.")

(defconst deterred-ai-codex-api-version 1
  "Version of the in-process Codex parser API.")

(defvar deterred-ai-codex-progress-callback nil
  "Optional callback for progress within one Codex rollout.

The arguments are path, absolute read offset, total file bytes, and
the number of complete lines parsed.")

(defvar deterred-ai-codex-corpus-progress-callback nil
  "Optional callback for progress across a Codex rollout corpus.

The arguments are completed files, total files, current path,
cumulative bytes read, and total corpus bytes.")

(defconst deterred-ai-codex--read-chunk-size (* 1024 1024)
  "Number of bytes read from a rollout at a time.")

(defconst deterred-ai-codex--long-context-threshold 272000
  "Raw input-token threshold for GPT-5.6 Sol long-context pricing.")

(define-error 'deterred-ai-codex-parse-error
  "Invalid Codex rollout JSONL")

(defun deterred-ai-codex--error (path line-no byte-offset format-string &rest args)
  "Signal a parser error for PATH at LINE-NO and BYTE-OFFSET.

FORMAT-STRING and ARGS describe the problem."
  (signal 'deterred-ai-codex-parse-error
          (list (format "%s:%d (byte %d): %s"
                        path line-no byte-offset
                        (apply #'format format-string args)))))

(defun deterred-ai-codex--value (key alist)
  "Return KEY from ALIST, treating JSON null as nil.

Also accept string keys so state decoded with either JSON API can be used."
  (let ((value (or (alist-get key alist)
                   (alist-get (symbol-name key) alist nil nil #'equal))))
    (unless (eq value :json-null)
      value)))

(defun deterred-ai-codex--state-value (key state)
  "Return KEY from persisted STATE."
  (deterred-ai-codex--value key state))

(defun deterred-ai-codex--non-empty-string (value)
  "Return trimmed VALUE when it is a non-empty string."
  (when (stringp value)
    (let ((trimmed (string-trim value)))
      (unless (string-empty-p trimmed)
        trimmed))))

(defun deterred-ai-codex--alist-object-p (value)
  "Return non-nil when VALUE can be treated as a JSON object alist."
  (and (consp value) (consp (car value))))

(defun deterred-ai-codex--timestamp-time (timestamp path line-no byte-offset)
  "Parse TIMESTAMP from PATH at LINE-NO and BYTE-OFFSET."
  (unless (stringp timestamp)
    (deterred-ai-codex--error
     path line-no byte-offset "record has no string timestamp"))
  (condition-case err
      (encode-time (iso8601-parse timestamp))
    (error
     (deterred-ai-codex--error
      path line-no byte-offset "invalid timestamp %S: %s"
      timestamp (error-message-string err)))))

(defun deterred-ai-codex--timestamp-seconds (timestamp path line-no byte-offset)
  "Return epoch seconds for TIMESTAMP, with PATH location for errors.

LINE-NO and BYTE-OFFSET identify the source record."
  (truncate
   (float-time
    (deterred-ai-codex--timestamp-time timestamp path line-no byte-offset))))

(defun deterred-ai-codex--usage-date
    (timestamp timezone path line-no byte-offset)
  "Return the date for TIMESTAMP in TIMEZONE.

PATH, LINE-NO, and BYTE-OFFSET identify the source record for errors."
  (condition-case err
      (format-time-string
       "%Y-%m-%d"
       (deterred-ai-codex--timestamp-time
        timestamp path line-no byte-offset)
       (deterred-ai-codex--non-empty-string timezone))
    (error
     (deterred-ai-codex--error
      path line-no byte-offset "invalid timezone %S: %s"
      timezone (error-message-string err)))))

(defun deterred-ai-codex--token-value
    (key usage default path line-no byte-offset)
  "Read a non-negative integer token KEY from USAGE.

Use DEFAULT when the key is absent.  PATH, LINE-NO, and BYTE-OFFSET
identify the source record for errors."
  (let ((value (deterred-ai-codex--value key usage)))
    (cond
     ((null value) default)
     ((and (integerp value) (>= value 0)) value)
     (t
      (deterred-ai-codex--error
       path line-no byte-offset "%s must be a non-negative integer, got %S"
       key value)))))

(defun deterred-ai-codex--normalize-usage
    (usage path line-no byte-offset)
  "Normalize and validate Codex token USAGE.

PATH, LINE-NO, and BYTE-OFFSET identify the source record for errors."
  (when (deterred-ai-codex--alist-object-p usage)
    (let* ((input (deterred-ai-codex--token-value
                   'input_tokens usage 0 path line-no byte-offset))
           (cached (deterred-ai-codex--token-value
                    'cached_input_tokens usage 0 path line-no byte-offset))
           (output (deterred-ai-codex--token-value
                    'output_tokens usage 0 path line-no byte-offset))
           (reasoning (deterred-ai-codex--token-value
                       'reasoning_output_tokens usage 0
                       path line-no byte-offset))
           (reported-total (deterred-ai-codex--value 'total_tokens usage))
           (total (if (null reported-total)
                      (+ input output)
                    (deterred-ai-codex--token-value
                     'total_tokens usage 0 path line-no byte-offset))))
      (when (> cached input)
        (deterred-ai-codex--error
         path line-no byte-offset
         "cached input tokens %d exceed input tokens %d" cached input))
      (when (> reasoning output)
        (deterred-ai-codex--error
         path line-no byte-offset
         "reasoning output tokens %d exceed output tokens %d"
         reasoning output))
      (unless (= total (+ input output))
        (deterred-ai-codex--error
         path line-no byte-offset
         "total tokens %d do not equal input plus output (%d)"
         total (+ input output)))
      `((input-tokens . ,input)
        (cached-input-tokens . ,cached)
        (output-tokens . ,output)
        (reasoning-output-tokens . ,reasoning)
        (total-tokens . ,total)))))

(defun deterred-ai-codex--total-only-last-usage-p (usage)
  "Return non-nil for Codex's non-request total-only USAGE marker.

Around context compaction Codex may write a `last_token_usage' with every
component zero but a nonzero `total_tokens'.  It is not an API request and
cannot be reconciled as one; the accompanying cumulative total remains
authoritative."
  (and (deterred-ai-codex--alist-object-p usage)
       (seq-every-p
        (lambda (key)
          (zerop (or (deterred-ai-codex--value key usage) 0)))
        '(input_tokens cached_input_tokens output_tokens
          reasoning_output_tokens))
       (> (or (deterred-ai-codex--value 'total_tokens usage) 0) 0)))

(defconst deterred-ai-codex--usage-keys
  '(input-tokens cached-input-tokens output-tokens
    reasoning-output-tokens total-tokens)
  "Canonical token usage keys.")

(defun deterred-ai-codex--usage-zero-p (usage)
  "Return non-nil when every component in USAGE is zero."
  (seq-every-p (lambda (key) (zerop (or (alist-get key usage) 0)))
               deterred-ai-codex--usage-keys))

(defun deterred-ai-codex--usage-equal-p (left right)
  "Return non-nil when LEFT and RIGHT have equal usage components."
  (seq-every-p
   (lambda (key)
     (= (or (alist-get key left) 0)
        (or (alist-get key right) 0)))
   deterred-ai-codex--usage-keys))

(defun deterred-ai-codex--usage-add (left right)
  "Add canonical usage components in LEFT and RIGHT."
  (mapcar
   (lambda (key)
     (cons key (+ (or (alist-get key left) 0)
                  (or (alist-get key right) 0))))
   deterred-ai-codex--usage-keys))

(defun deterred-ai-codex--usage-reset-p (current previous last)
  "Return non-nil when CURRENT restarts the PREVIOUS cumulative counters.

Codex can reset these counters when resuming a rollout.  Require LAST
to equal the entire new total so the reset accounts for exactly one
request.  Unexplained decreases still fail normal delta validation."
  (and previous last
       (deterred-ai-codex--usage-equal-p current last)
       (seq-some
        (lambda (key)
          (< (or (alist-get key current) 0)
             (or (alist-get key previous) 0)))
        deterred-ai-codex--usage-keys)))

(defun deterred-ai-codex--usage-delta
    (current previous path line-no byte-offset)
  "Subtract PREVIOUS cumulative usage from CURRENT.

Signal a contextual error on any component decrease."
  (let (result)
    (dolist (key deterred-ai-codex--usage-keys)
      (let* ((current-value (or (alist-get key current) 0))
             (previous-value (or (alist-get key previous) 0))
             (delta (- current-value previous-value)))
        (when (< delta 0)
          (deterred-ai-codex--error
           path line-no byte-offset
           "cumulative %s decreased from %d to %d"
           key previous-value current-value))
        (push (cons key delta) result)))
    (setq result (nreverse result))
    ;; Subsets must also be subsets on an individual request.
    (when (> (alist-get 'cached-input-tokens result)
             (alist-get 'input-tokens result))
      (deterred-ai-codex--error
       path line-no byte-offset
       "request cached input exceeds request input"))
    (when (> (alist-get 'reasoning-output-tokens result)
             (alist-get 'output-tokens result))
      (deterred-ai-codex--error
       path line-no byte-offset
       "request reasoning output exceeds request output"))
    (unless (= (alist-get 'total-tokens result)
               (+ (alist-get 'input-tokens result)
                  (alist-get 'output-tokens result)))
      (deterred-ai-codex--error
       path line-no byte-offset
       "request total does not equal request input plus output"))
    result))

(defun deterred-ai-codex--extract-model (payload)
  "Extract a model name from PAYLOAD."
  (when (deterred-ai-codex--alist-object-p payload)
    (or (deterred-ai-codex--non-empty-string
         (deterred-ai-codex--value 'model payload))
        (when-let* ((settings
                     (deterred-ai-codex--value 'thread_settings payload)))
          (deterred-ai-codex--non-empty-string
           (deterred-ai-codex--value 'model settings)))
        (when-let* ((info (deterred-ai-codex--value 'info payload)))
          (or (deterred-ai-codex--non-empty-string
               (deterred-ai-codex--value 'model info))
              (deterred-ai-codex--non-empty-string
               (deterred-ai-codex--value 'model_name info)))))))

(defun deterred-ai-codex--session-source (payload)
  "Return source metadata from the first session PAYLOAD.

The return value is (SUBAGENT PARENT-ID REPLAYED).  REPLAYED means the
rollout contains copied parent history and must wait for an activation
boundary before emitting usage."
  (let* ((thread-source
          (deterred-ai-codex--value 'thread_source payload))
         (source (deterred-ai-codex--value 'source payload))
         (subagent-source
          (and (deterred-ai-codex--alist-object-p source)
               (deterred-ai-codex--value 'subagent source)))
         (thread-spawn
          (and (deterred-ai-codex--alist-object-p subagent-source)
               (deterred-ai-codex--value
                'thread_spawn subagent-source)))
         (spawn
          (and (deterred-ai-codex--alist-object-p subagent-source)
               (or thread-spawn subagent-source)))
         (subagent (or (equal thread-source "subagent")
                       (deterred-ai-codex--alist-object-p subagent-source)))
         (forked-from-id
          (or (and (deterred-ai-codex--alist-object-p spawn)
                   (deterred-ai-codex--non-empty-string
                    (deterred-ai-codex--value
                     'forked_from_id spawn)))
              (deterred-ai-codex--non-empty-string
               (deterred-ai-codex--value 'forked_from_id payload))))
         (parent
          (or (deterred-ai-codex--non-empty-string
               (deterred-ai-codex--value 'parent_thread_id payload))
              (and (deterred-ai-codex--alist-object-p spawn)
                   (deterred-ai-codex--non-empty-string
                    (deterred-ai-codex--value
                     'parent_thread_id spawn)))
              forked-from-id))
         (replayed
          (and subagent
               (or (deterred-ai-codex--alist-object-p thread-spawn)
                   forked-from-id))))
    (list subagent parent replayed)))

(defun deterred-ai-codex--dimension-string (date model tier)
  "Build a stable dimension string from DATE, MODEL, and TIER."
  (prin1-to-string (list date model tier)))

(defun deterred-ai-codex--turn-record (turn-id turn-dimensions)
  "Find TURN-ID in persisted TURN-DIMENSIONS records."
  (seq-find
   (lambda (record)
     (equal turn-id (deterred-ai-codex--value 'turn-id record)))
   turn-dimensions))

(defun deterred-ai-codex--message-id
    (rollout-id turn-id dimension turn-dimensions)
  "Return a stable message ID and update TURN-DIMENSIONS.

ROLLOUT-ID and TURN-ID form the plain turn key.  The first DIMENSION
uses that key; only later date/model/tier splits receive a suffix."
  (let* ((turn-key (format "codex:%s:%s" rollout-id turn-id))
         (record (deterred-ai-codex--turn-record
                  turn-id turn-dimensions))
         (dimensions (and record
                          (deterred-ai-codex--value 'dimensions record)))
         (position (cl-position dimension dimensions :test #'equal)))
    (unless record
      (setq record (list (cons 'turn-id turn-id)
                         (cons 'dimensions nil)))
      (push record turn-dimensions)
      (setq dimensions nil))
    (unless position
      (setf (alist-get 'dimensions record)
            (append dimensions (list dimension)))
      (setq position (length dimensions)))
    (cons (if (zerop position)
              turn-key
            (format "%s:dim:%s"
                    turn-key
                    (substring (secure-hash 'sha256 dimension) 0 16)))
          turn-dimensions)))

(defun deterred-ai-codex--entry-less-p (left right)
  "Return non-nil when usage entry LEFT should sort before RIGHT."
  (let ((left-ts (alist-get 'timestamp left))
        (right-ts (alist-get 'timestamp right)))
    (if (= left-ts right-ts)
        (string< (alist-get 'message-id left)
                 (alist-get 'message-id right))
      (< left-ts right-ts))))

(defun deterred-ai-codex--content-line-count (content)
  "Count logical lines in patch CONTENT."
  (if (or (not (stringp content)) (string-empty-p content))
      0
    (+ (cl-count ?\n content)
       (if (string-suffix-p "\n" content) 0 1))))

(defun deterred-ai-codex--diff-line-counts (diff)
  "Return (ADDED . REMOVED) line counts from unified DIFF."
  (let ((added 0)
        (removed 0))
    (when (stringp diff)
      (dolist (line (split-string diff "\n"))
        (cond
         ((and (string-prefix-p "+" line)
               (not (string-match-p
                     "\\`+++\\(?:[ \t]\\|\\'\\)" line)))
          (cl-incf added))
         ((and (string-prefix-p "-" line)
               (not (string-match-p
                     "\\`---\\(?:[ \t]\\|\\'\\)" line)))
          (cl-incf removed)))))
    (cons added removed)))

(defun deterred-ai-codex--json-record (bytes path line-no byte-offset)
  "Decode complete unibyte JSON record BYTES from PATH.

LINE-NO and BYTE-OFFSET identify the source record for errors."
  (let ((text (decode-coding-string bytes 'utf-8-unix t)))
    (when (string-suffix-p "\r" text)
      (setq text (substring text 0 -1)))
    (unless (string-empty-p (string-trim text))
      (condition-case err
          (let ((json-object-type 'alist)
                (json-array-type 'list)
                (json-key-type 'symbol)
                (json-false nil)
                (json-null :json-null))
            (json-read-from-string text))
        (error
         (deterred-ai-codex--error
          path line-no byte-offset "malformed JSON: %s"
          (error-message-string err)))))))

(defun deterred-ai-codex--default-data-dir ()
  "Return the configured Codex data directory without requiring AI integration."
  (expand-file-name
   (or (and (boundp 'deterred-ai-codex-data-dir)
            (symbol-value 'deterred-ai-codex-data-dir))
       (getenv "CODEX_HOME")
       "~/.codex/")))

(defun deterred-ai-codex-collect-jsonl-files (&optional data-dir)
  "Discover Codex rollout JSONL files below DATA-DIR.

Both the `sessions' and `archived_sessions' trees are searched
recursively.  Return source-description alists sorted by absolute path."
  (let ((root (expand-file-name
               (or data-dir (deterred-ai-codex--default-data-dir))))
        (seen-relative-paths (make-hash-table :test #'equal))
        result)
    ;; Active sessions win while an identical archived copy also exists.
    (dolist (kind '("sessions" "archived_sessions"))
      (let ((directory (expand-file-name kind root)))
        (when (file-directory-p directory)
          (dolist (path (directory-files-recursively directory "\\.jsonl\\'"))
            (let ((relative-path (file-relative-name path directory)))
              (unless (gethash relative-path seen-relative-paths)
                (puthash relative-path t seen-relative-paths)
                (push `((jsonl-path . ,path)
                        (root-kind . ,kind)
                        (relative-path . ,relative-path)
                        (source-file-key . ,(concat kind ":" relative-path)))
                      result)))))))
    (seq-sort-by (lambda (item) (alist-get 'jsonl-path item))
                 #'string< result)))

;;;###autoload
(defun deterred-ai-codex-parse-jsonl
    (jsonl-path &optional prior-state byte-offset allow-incomplete-source)
  "Parse Codex rollout JSONL-PATH.

PRIOR-STATE is the `:state' returned by an earlier call.  BYTE-OFFSET
must be that call's `:parsed-offset'; only complete appended lines are
then read.  With nil PRIOR-STATE and offset zero, perform a full parse.

Return a plist containing `:entries', flat `:files', `:state',
`:parsed-offset', and `:source'.  Entries from an incremental call are
additive deltas keyed by stable message IDs.  The state contains only
JSON-serializable alists, lists, strings, numbers, booleans, and nil.

An incomplete final line is retained for the next call.  A fully parsed
replayed subagent without its first trigger marker is rejected unless
ALLOW-INCOMPLETE-SOURCE is non-nil.  Direct auxiliary subagents such as
Codex guardians have no copied history or trigger marker and are active
from their first record.

When `deterred-ai-codex-progress-callback' is non-nil, call it at the
initial offset and after each input chunk, including chunks ending
inside a very large JSON record."
  (let* ((path (expand-file-name jsonl-path))
         (file-size (file-attribute-size (file-attributes path)))
         (offset (or byte-offset 0))
         (full-parse (and (null prior-state) (zerop offset)))
         (state-version
          (and prior-state
               (deterred-ai-codex--state-value
                'parser-version prior-state))))
    (when (or (< offset 0) (> offset file-size))
      (deterred-ai-codex--error
       path 0 offset "offset is outside the file (size %d)" file-size))
    (when (and (> offset 0) (null prior-state))
      (deterred-ai-codex--error
       path 0 offset "a nonzero offset requires prior parser state"))
    (when (and state-version
               (not (equal state-version
                           deterred-ai-codex-parser-version)))
      (deterred-ai-codex--error
       path 0 offset "parser state version %S is not supported (expected %S)"
       state-version deterred-ai-codex-parser-version))
    ;; A cached offset must point just after a complete line.
    (when (> offset 0)
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally path nil (1- offset) offset)
        (unless (and (= (buffer-size) 1)
                     (= (char-after (point-min)) ?\n))
          (deterred-ai-codex--error
           path 0 offset "offset is not at a JSONL line boundary"))))
    (let* ((fallback-id (file-name-base path))
           (rollout-id
            (or (deterred-ai-codex--state-value 'rollout-id prior-state)
                fallback-id))
           (source-key
            (or (deterred-ai-codex--state-value 'source-key prior-state)
                (concat "codex:" rollout-id)))
           (parent-session-id
            (deterred-ai-codex--state-value
             'parent-session-id prior-state))
           (subagent
            (and (deterred-ai-codex--state-value 'subagent prior-state) t))
           (session-meta-seen
            (and (deterred-ai-codex--state-value
                  'session-meta-seen prior-state) t))
           ;; Before the first metadata record, treating the source as a root
           ;; lets metadata-free legacy rollouts remain parseable.  A first
           ;; subagent metadata record switches this off before replay usage.
           (active
            (if prior-state
                (and (deterred-ai-codex--state-value 'active prior-state) t)
              t))
           (activation-seen
            (and (deterred-ai-codex--state-value
                  'activation-seen prior-state) t))
           (previous-total
            (deterred-ai-codex--state-value 'previous-total prior-state))
           (current-turn-id
            (deterred-ai-codex--state-value 'current-turn-id prior-state))
           (current-turn-timestamp
            (deterred-ai-codex--state-value
             'current-turn-timestamp prior-state))
           (current-model
            (deterred-ai-codex--state-value 'current-model prior-state))
           (current-cwd
            (deterred-ai-codex--state-value 'current-cwd prior-state))
           (current-timezone
            (if prior-state
                (deterred-ai-codex--state-value
                 'current-timezone prior-state)
              (and (boundp 'deterred-ai-time-zone)
                   (symbol-value 'deterred-ai-time-zone))))
           (service-tier
            (or (deterred-ai-codex--state-value
                 'service-tier prior-state)
                "default"))
           (session-version
            (deterred-ai-codex--state-value 'session-version prior-state))
           (last-timestamp
            (deterred-ai-codex--state-value 'last-timestamp prior-state))
           (line-no
            (or (deterred-ai-codex--state-value 'line-number prior-state) 0))
           (turn-dimensions
            (copy-tree
             (or (deterred-ai-codex--state-value
                  'turn-dimensions prior-state)
                 nil)))
           (groups (make-hash-table :test #'equal))
           (file-groups (make-hash-table :test #'equal))
           (parsed-offset offset)
           (record-byte-offset offset))
      (cl-labels
          ((turn-has-dimension-p (turn-id)
             (and (deterred-ai-codex--turn-record
                   turn-id turn-dimensions) t))
           (require-turn (event-name event-turn-id)
             (let ((turn-id
                    (or (deterred-ai-codex--non-empty-string event-turn-id)
                        (deterred-ai-codex--non-empty-string current-turn-id))))
               (unless turn-id
                 (deterred-ai-codex--error
                  path line-no record-byte-offset
                  "%s is not associated with an explicit turn ID"
                  event-name))
               turn-id))
           (ensure-group (turn-id timestamp data-quality)
             (let* ((timestamp
                     (or timestamp current-turn-timestamp last-timestamp))
                    (_ (unless timestamp
                         (deterred-ai-codex--error
                          path line-no record-byte-offset
                          "turn %s has no timestamp" turn-id)))
                    (date (deterred-ai-codex--usage-date
                           timestamp current-timezone path line-no
                           record-byte-offset))
                    (model (or current-model "unknown-codex"))
                    (tier (or service-tier "default"))
                    (dimension (deterred-ai-codex--dimension-string
                                date model tier))
                    (id-result (deterred-ai-codex--message-id
                                rollout-id turn-id dimension turn-dimensions))
                    (message-id (car id-result))
                    (turn-key (format "codex:%s:%s" rollout-id turn-id))
                    (epoch (deterred-ai-codex--timestamp-seconds
                            timestamp path line-no record-byte-offset))
                    entry)
               (setq turn-dimensions (cdr id-result))
               (setq entry (gethash message-id groups))
               (unless entry
                 (setq entry
                       (list
                        (cons 'timestamp epoch)
                        (cons 'end-timestamp epoch)
                        (cons 'session-id rollout-id)
                        (cons 'parent-session-id parent-session-id)
                        (cons 'model-name model)
                        (cons 'message-id message-id)
                        (cons 'request-id nil)
                        (cons 'cwd current-cwd)
                        (cons 'version session-version)
                        (cons 'input-tokens 0)
                        (cons 'output-tokens 0)
                        (cons 'cache-creation-input-tokens 0)
                        (cons 'cache-read-input-tokens 0)
                        (cons 'reasoning-output-tokens 0)
                        (cons 'total-tokens 0)
                        (cons 'tiered-input-tokens 0)
                        (cons 'tiered-output-tokens 0)
                        (cons 'tiered-cache-creation-input-tokens 0)
                        (cons 'tiered-cache-read-input-tokens 0)
                        (cons 'tiered-reasoning-output-tokens 0)
                        (cons 'tiered-request-count 0)
                        (cons 'pricing-basis-complete t)
                        (cons 'files nil)
                        (cons 'provider "codex")
                        (cons 'record-kind "codex-turn-model")
                        (cons 'data-quality data-quality)
                        (cons 'source-key source-key)
                        (cons 'parser-version
                              deterred-ai-codex-parser-version)
                        (cons 'usage-date date)
                        (cons 'turn-key turn-key)
                        (cons 'request-count 0)
                        (cons 'message-count 0)
                        (cons 'service-tier tier)))
                 (puthash message-id entry groups))
               (when (> (alist-get 'timestamp entry) epoch)
                 (setf (alist-get 'timestamp entry) epoch))
               (when (< (alist-get 'end-timestamp entry) epoch)
                 (setf (alist-get 'end-timestamp entry) epoch))
               (when current-cwd
                 (setf (alist-get 'cwd entry) current-cwd))
               (when session-version
                 (setf (alist-get 'version entry) session-version))
               (when (equal data-quality "partial")
                 (setf (alist-get 'data-quality entry) "partial"))
               entry))
           (flush-empty-current-turn ()
             (when (and active current-turn-id
                        (not (turn-has-dimension-p current-turn-id)))
               (ensure-group current-turn-id current-turn-timestamp "observed")))
           (switch-turn (turn-id timestamp)
             (when (and active current-turn-id
                        (not (equal current-turn-id turn-id)))
               (flush-empty-current-turn))
             (setq current-turn-id turn-id
                   current-turn-timestamp timestamp))
           (add-usage (entry usage partial)
             (let* ((raw-input (alist-get 'input-tokens usage))
                    (cached (alist-get 'cached-input-tokens usage))
                    (uncached (- raw-input cached))
                    (output (alist-get 'output-tokens usage))
                    (reasoning
                     (alist-get 'reasoning-output-tokens usage))
                    (total (alist-get 'total-tokens usage))
                    (tiered
                     (and (equal current-model "gpt-5.6-sol")
                          (> raw-input
                             deterred-ai-codex--long-context-threshold))))
               (cl-incf (alist-get 'input-tokens entry) uncached)
               (cl-incf (alist-get 'cache-read-input-tokens entry) cached)
               (cl-incf (alist-get 'output-tokens entry) output)
               (cl-incf (alist-get 'reasoning-output-tokens entry) reasoning)
               (cl-incf (alist-get 'total-tokens entry) total)
               (cl-incf (alist-get 'request-count entry))
               (when tiered
                 (cl-incf (alist-get 'tiered-input-tokens entry) uncached)
                 (cl-incf (alist-get
                           'tiered-cache-read-input-tokens entry) cached)
                 (cl-incf (alist-get 'tiered-output-tokens entry) output)
                 (cl-incf (alist-get
                           'tiered-reasoning-output-tokens entry) reasoning)
                 (cl-incf (alist-get 'tiered-request-count entry)))
               (when partial
                 (setf (alist-get 'data-quality entry) "partial"))))
           (add-file-change (turn-id timestamp source-path change)
             (let* ((move-path
                     (deterred-ai-codex--non-empty-string
                      (deterred-ai-codex--value 'move_path change)))
                    (final-path (or move-path source-path))
                    (operation
                     (or (deterred-ai-codex--non-empty-string
                          (deterred-ai-codex--value 'type change))
                         "update"))
                    (counts
                     (if (equal operation "update")
                         (deterred-ai-codex--diff-line-counts
                          (deterred-ai-codex--value
                           'unified_diff change))
                       (let ((lines
                              (deterred-ai-codex--content-line-count
                               (deterred-ai-codex--value
                                'content change))))
                         (cond
                          ((equal operation "add") (cons lines 0))
                          ((equal operation "delete") (cons 0 lines))
                          (t (cons 0 0))))))
                    (entry (ensure-group turn-id timestamp "observed"))
                    (message-id (alist-get 'message-id entry))
                    (turn-key (alist-get 'turn-key entry))
                    (key (cons turn-key final-path))
                    (file (gethash key file-groups)))
               (unless file
                 (setq file
                       (list
                        (cons 'file-path final-path)
                        (cons 'previous-file-path
                              (and move-path source-path))
                        (cons 'message-id message-id)
                        (cons 'turn-key turn-key)
                        (cons 'tool-name "apply_patch")
                        (cons 'touch-count 0)
                        (cons 'add-count 0)
                        (cons 'update-count 0)
                        (cons 'delete-count 0)
                        (cons 'move-count 0)
                        (cons 'lines-added 0)
                        (cons 'lines-removed 0)))
                 (puthash key file file-groups))
               (cl-incf (alist-get 'touch-count file))
               (cl-incf (alist-get 'lines-added file) (car counts))
               (cl-incf (alist-get 'lines-removed file) (cdr counts))
               (pcase operation
                 ("add" (cl-incf (alist-get 'add-count file)))
                 ("delete" (cl-incf (alist-get 'delete-count file)))
                 (_ (cl-incf (alist-get 'update-count file))))
               (when move-path
                 (cl-incf (alist-get 'move-count file))
                 (setf (alist-get 'previous-file-path file) source-path))))
           (process-record (record timestamp)
             (when record
               (unless (deterred-ai-codex--alist-object-p record)
                 (deterred-ai-codex--error
                  path line-no record-byte-offset
                  "top-level JSON value is not an object"))
               (let ((type (deterred-ai-codex--value 'type record))
                     (payload (deterred-ai-codex--value 'payload record)))
                 (setq last-timestamp timestamp)
                 (cond
                  ((equal type "session_meta")
                   ;; Copied subagent history contains another session_meta;
                   ;; only the physical rollout's first metadata is identity.
                   (unless session-meta-seen
                     (setq session-meta-seen t)
                     (when (deterred-ai-codex--alist-object-p payload)
                       (when-let* ((id (deterred-ai-codex--non-empty-string
                                       (deterred-ai-codex--value
                                        'id payload))))
                         (setq rollout-id id
                               source-key (concat "codex:" id)))
                       (pcase-let ((`(,is-subagent ,parent ,replayed)
                                    (deterred-ai-codex--session-source
                                     payload)))
                         (setq subagent is-subagent
                               parent-session-id parent
                               ;; thread_spawn/forked rollouts begin with
                               ;; copied parent history.  Direct auxiliary
                               ;; subagents (for example guardians) do not.
                               active (not replayed)
                               activation-seen (not replayed)))
                       (when-let* ((cwd (deterred-ai-codex--non-empty-string
                                        (deterred-ai-codex--value
                                         'cwd payload))))
                         (setq current-cwd cwd))
                       (when-let* ((version
                                    (deterred-ai-codex--non-empty-string
                                     (deterred-ai-codex--value
                                      'cli_version payload))))
                         (setq session-version version))
                       (when-let* ((model
                                    (deterred-ai-codex--extract-model payload)))
                         (setq current-model model)))))
                  ((equal type "turn_context")
                   (when (deterred-ai-codex--alist-object-p payload)
                     (when-let* ((turn-id
                                  (deterred-ai-codex--non-empty-string
                                   (deterred-ai-codex--value
                                    'turn_id payload))))
                       (switch-turn turn-id timestamp))
                     (when-let* ((cwd (deterred-ai-codex--non-empty-string
                                      (deterred-ai-codex--value
                                       'cwd payload))))
                       (setq current-cwd cwd))
                     (when-let* ((timezone
                                  (deterred-ai-codex--non-empty-string
                                   (deterred-ai-codex--value
                                    'timezone payload))))
                       (setq current-timezone timezone))
                     (when-let* ((model
                                  (deterred-ai-codex--extract-model payload)))
                       (setq current-model model))
                     (when-let* ((tier
                                  (deterred-ai-codex--non-empty-string
                                   (deterred-ai-codex--value
                                    'service_tier payload))))
                       (setq service-tier tier))))
                  ((equal type "inter_agent_communication_metadata")
                   (when (and subagent
                              (deterred-ai-codex--alist-object-p payload)
                              (eq (deterred-ai-codex--value
                                   'trigger_turn payload) t)
                              (not activation-seen))
                     (setq active t activation-seen t)
                     ;; The copied task_started/turn_context is now the local
                     ;; turn.  Defer creating its zero row until usage, a
                     ;; patch, the next turn, or EOF so settings immediately
                     ;; following activation cannot create a phantom split.
                     (require-turn "subagent activation" nil)))
                  ((equal type "event_msg")
                   (when (deterred-ai-codex--alist-object-p payload)
                     (pcase (deterred-ai-codex--value 'type payload)
                       ("task_started"
                       (let ((turn-id
                               (deterred-ai-codex--non-empty-string
                                (deterred-ai-codex--value
                                 'turn_id payload))))
                          (unless turn-id
                            (deterred-ai-codex--error
                             path line-no record-byte-offset
                             "task_started has no turn ID"))
                          (switch-turn turn-id timestamp)))
                       ((or "task_complete" "turn_aborted")
                        (when active
                          (let ((turn-id
                                 (require-turn
                                  (deterred-ai-codex--value 'type payload)
                                  (deterred-ai-codex--value
                                   'turn_id payload))))
                            ;; This also creates the explicit zero-usage row
                            ;; for a completed or aborted turn.
                            (ensure-group turn-id timestamp "observed"))))
                       ("thread_settings_applied"
                        (when-let* ((settings
                                    (deterred-ai-codex--value
                                     'thread_settings payload)))
                          (when-let* ((model
                                      (deterred-ai-codex--extract-model
                                       settings)))
                            (setq current-model model))
                          (when-let* ((tier
                                      (deterred-ai-codex--non-empty-string
                                       (deterred-ai-codex--value
                                        'service_tier settings))))
                            (setq service-tier tier))
                          (when-let* ((cwd
                                      (deterred-ai-codex--non-empty-string
                                       (deterred-ai-codex--value
                                        'cwd settings))))
                            (setq current-cwd cwd))))
                       ("token_count"
                       (let* ((info (deterred-ai-codex--value
                                      'info payload))
                               (raw-last
                                (and (deterred-ai-codex--alist-object-p info)
                                     (deterred-ai-codex--value
                                      'last_token_usage info)))
                               (last
                                (and raw-last
                                     (not (deterred-ai-codex--total-only-last-usage-p
                                           raw-last))
                                     (deterred-ai-codex--normalize-usage
                                      raw-last
                                      path line-no record-byte-offset)))
                               (total
                                (and (deterred-ai-codex--alist-object-p info)
                                     (deterred-ai-codex--normalize-usage
                                      (deterred-ai-codex--value
                                       'total_token_usage info)
                                      path line-no record-byte-offset)))
                               delta partial)
                          (cond
                           (total
                            (when (deterred-ai-codex--usage-reset-p
                                   total previous-total last)
                              (setq previous-total nil))
                            (setq delta
                                  (deterred-ai-codex--usage-delta
                                   total previous-total path line-no
                                   record-byte-offset))
                            (setq previous-total total)
                            ;; Unchanged snapshots often repeat a stale last
                            ;; request.  They are state snapshots, not calls.
                            (unless (deterred-ai-codex--usage-zero-p delta)
                              (when (and active last
                                         (not (deterred-ai-codex--usage-equal-p
                                               last delta)))
                                (deterred-ai-codex--error
                                 path line-no record-byte-offset
                                 "last_token_usage does not match the cumulative delta"))))
                           (last
                            (setq delta last partial t
                                  ;; Advance an inferred cumulative baseline
                                  ;; so a later total snapshot cannot count
                                  ;; the same last-only request again.
                                  previous-total
                                  (deterred-ai-codex--usage-add
                                   previous-total last))))
                          ;; Preserve cumulative baseline before the first
                          ;; local subagent activation, but emit no replay rows.
                          (when (and active delta
                                     (not (deterred-ai-codex--usage-zero-p
                                           delta)))
                            (when-let* ((model
                                        (deterred-ai-codex--extract-model
                                         payload)))
                              (setq current-model model))
                            (let* ((turn-id
                                    (require-turn "token_count" nil))
                                   (entry
                                    (ensure-group
                                     turn-id timestamp
                                     (if partial "partial" "observed"))))
                              (add-usage entry delta partial)))))
                       ("patch_apply_end"
                        (when (and active
                                   (eq (deterred-ai-codex--value
                                        'success payload) t))
                          (let* ((turn-id
                                  (require-turn
                                   "patch_apply_end"
                                   (deterred-ai-codex--value
                                    'turn_id payload)))
                                 (changes
                                  (deterred-ai-codex--value
                                   'changes payload)))
                            (when (deterred-ai-codex--alist-object-p changes)
                              (dolist (change-cell changes)
                                (let ((source-path
                                       (if (symbolp (car change-cell))
                                           (symbol-name (car change-cell))
                                         (car change-cell)))
                                      (change (cdr change-cell)))
                                  (when (and (stringp source-path)
                                             (deterred-ai-codex--alist-object-p
                                              change))
                                    (add-file-change
                                     turn-id timestamp source-path
                                     change))))))))))))))))
        ;; Read and process only newline-terminated records while bounding
        ;; buffer memory.  Offsets are byte offsets because this is unibyte.
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (when deterred-ai-codex-progress-callback
            (funcall deterred-ai-codex-progress-callback
                     path offset file-size line-no))
          (let ((read-offset offset))
            (while (< read-offset file-size)
              (goto-char (point-max))
              (let ((end (min file-size
                              (+ read-offset
                                 deterred-ai-codex--read-chunk-size))))
                (insert-file-contents-literally path nil read-offset end)
                (setq read-offset end))
              (goto-char (point-min))
              (while (search-forward "\n" nil t)
                (let* ((line-end (1- (point)))
                       (bytes (buffer-substring-no-properties
                               (point-min) line-end))
                       (consumed (- (point) (point-min))))
                  (cl-incf line-no)
                  (setq record-byte-offset parsed-offset)
                  (let ((record
                         (deterred-ai-codex--json-record
                          bytes path line-no record-byte-offset)))
                    (process-record
                     record
                     (and record
                          (deterred-ai-codex--value
                           'timestamp record))))
                  (cl-incf parsed-offset consumed)
                  (delete-region (point-min) (point))
                  (goto-char (point-min))))
              (when deterred-ai-codex-progress-callback
                (funcall deterred-ai-codex-progress-callback
                         path read-offset file-size line-no)))))
        ;; Include an explicit row for a final no-usage turn.  Its stable
        ;; dimension in state ensures an appended call updates the same row.
        (flush-empty-current-turn)
        (when (and full-parse subagent (not activation-seen)
                   (not allow-incomplete-source))
          (deterred-ai-codex--error
           path line-no parsed-offset
           "replayed subagent rollout has no trigger_turn activation boundary"))
        (let ((entries (hash-table-values groups))
              (files (hash-table-values file-groups)))
          (setq entries (sort entries #'deterred-ai-codex--entry-less-p))
          (setq files
                (sort files
                      (lambda (left right)
                        (let ((left-turn (alist-get 'turn-key left))
                              (right-turn (alist-get 'turn-key right)))
                          (if (equal left-turn right-turn)
                              (string< (alist-get 'file-path left)
                                       (alist-get 'file-path right))
                            (string< left-turn right-turn))))))
          ;; Embed files as a compatibility convenience.  `:files' is the
          ;; canonical turn/path aggregate and should be preferred by callers.
          (dolist (file files)
            (when-let* ((entry (gethash (alist-get 'message-id file) groups)))
              (push (copy-tree file) (alist-get 'files entry))))
          (list
           :entries entries
           :files files
           :state
           `((parser-version . ,deterred-ai-codex-parser-version)
             (rollout-id . ,rollout-id)
             (source-key . ,source-key)
             (parent-session-id . ,parent-session-id)
             (subagent . ,subagent)
             (session-meta-seen . ,session-meta-seen)
             (active . ,active)
             (activation-seen . ,activation-seen)
             (previous-total . ,previous-total)
             (current-turn-id . ,current-turn-id)
             (current-turn-timestamp . ,current-turn-timestamp)
             (current-model . ,current-model)
             (current-cwd . ,current-cwd)
             (current-timezone . ,current-timezone)
             (service-tier . ,service-tier)
             (session-version . ,session-version)
             (last-timestamp . ,last-timestamp)
             (line-number . ,line-no)
             (turn-dimensions . ,turn-dimensions))
           :parsed-offset parsed-offset
           :source
           `((jsonl-path . ,path)
             (rollout-id . ,rollout-id)
             (source-key . ,source-key)
             (parent-session-id . ,parent-session-id)
             (subagent . ,subagent)
             (complete-bytes . ,parsed-offset)
             (file-size . ,file-size))))))))

;;;###autoload
(defun deterred-ai-codex-parse-all (&optional data-dir)
  "Parse all Codex rollout JSONL files below DATA-DIR.

Return a plist with sorted `:entries', flat `:files', and per-rollout
`:sources'.  Parsing stops at the first invalid complete source; callers
can therefore stage the result atomically before replacing database data.

When `deterred-ai-codex-corpus-progress-callback' is non-nil, call it
with completed file count, total file count, current path, cumulative
bytes read, and total bytes.  The first callback reports zero progress
and a nil current path."
  (let* ((descriptions (deterred-ai-codex-collect-jsonl-files data-dir))
         (total-files (length descriptions))
         (total-bytes
          (if deterred-ai-codex-corpus-progress-callback
              (cl-loop
               for description in descriptions
               sum (file-attribute-size
                    (file-attributes
                     (alist-get 'jsonl-path description))))
            0))
         (completed-files 0)
         (completed-bytes 0)
         entries files sources)
    (when deterred-ai-codex-corpus-progress-callback
      (funcall deterred-ai-codex-corpus-progress-callback
               0 total-files nil 0 total-bytes))
    (dolist (description descriptions)
      (let* ((path (alist-get 'jsonl-path description))
             (file-size
              (file-attribute-size (file-attributes path)))
             (parsed
              (let ((deterred-ai-codex-progress-callback
                     (when deterred-ai-codex-corpus-progress-callback
                       (lambda (_path read-offset _file-size _line-number)
                         (funcall deterred-ai-codex-corpus-progress-callback
                                  completed-files total-files path
                                  (+ completed-bytes read-offset)
                                  total-bytes)))))
                (deterred-ai-codex-parse-jsonl path))))
        (setq entries (nconc entries (plist-get parsed :entries))
              files (nconc files (plist-get parsed :files)))
        (push (append description
                      (plist-get parsed :source)
                      `((parsed-offset . ,(plist-get parsed :parsed-offset))
                        (state . ,(plist-get parsed :state))))
              sources)
        (cl-incf completed-files)
        (cl-incf completed-bytes file-size)
        (when deterred-ai-codex-corpus-progress-callback
          (funcall deterred-ai-codex-corpus-progress-callback
                   completed-files total-files path completed-bytes
                   total-bytes))))
    (setq entries (sort entries #'deterred-ai-codex--entry-less-p))
    (list :entries entries
          :files files
          :sources (nreverse sources))))

(provide 'deterred-ai-codex)
;;; deterred-ai-codex.el ends here

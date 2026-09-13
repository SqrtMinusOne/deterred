;;; deterred-ai.el --- AI usage tracking for DETERRED -*- lexical-binding: t -*-

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

;; This file has been mostly AI-generated.

;;; Code:
(require 'deterred-db)
(require 'deterred-backup)
(require 'deterred-source)
(require 'deterred-format)
(require 'deterred-utils)
(require 'request)
(require 'iso8601)
(require 'subr-x)
(require 'cl-lib)
(require 'seq)
(require 'json)
(require 'deterred-ai-codex)

(defconst deterred-ai--codex-api-version 1
  "Codex parser API version required for granular progress reporting.")

;; Keep these declarations here as well as in `deterred-ai-codex.el'.  This
;; lets a newly evaluated integration safely call an older parser already
;; loaded from stale package bytecode; the old parser ignores the bindings.
(defvar deterred-ai-codex-progress-callback nil)
(defvar deterred-ai-codex-corpus-progress-callback nil)

(defconst deterred-ai--pricing-litellm-url
  "https://raw.githubusercontent.com/BerriAI/litellm/main/model_prices_and_context_window.json")

(defcustom deterred-ai-pricing-location "~/.deterred/ai-pricing.json"
  "Path to file with DETERRED pricing cache."
  :group 'deterred
  :type 'file)

(defcustom deterred-ai-claude-data-dir (or (getenv "CLAUDE_CONFIG_DIR")
                                           (expand-file-name "~/.claude/"))
  "Path to Claude data dir."
  :group 'deterred
  :type 'file)

(defcustom deterred-ai-codex-data-dir (or (getenv "CODEX_HOME")
                                          (expand-file-name "~/.codex/"))
  "Path to Codex data dir."
  :group 'deterred
  :type 'file)

(defcustom deterred-ai-model-name-map
  '(("codex-auto-review" . "gpt-5.5")
    ("kimi-for-coding" . "moonshot/kimi-k2.5")
    ("anthropic/claude-4.6-opus-20260205" . "claude-opus-4-6-20260205")
    ("anthropic/claude-4.5-haiku-20251001" . "claude-haiku-4-5-20251001"))
  "Alist mapping model names to LiteLLM-compatible names."
  :group 'deterred
  :type '(alist :key-type string :value-type string))

(defcustom deterred-ai-model-prefixes-to-remove '("anthropic/")
  "Prefixes to remove from model names stored in the DETERRED database.

Use `deterred-ai-remove-model-prefixes' to apply this setting to
existing rows."
  :group 'deterred
  :type '(repeat string))

(defcustom deterred-ai-time-zone nil
  "Time zone used to assign AI usage to a calendar date.

Nil means the system time zone.  The computed date is persisted so
syncing a database to a machine in another time zone does not move
historical usage between days."
  :group 'deterred
  :type '(choice (const :tag "System time zone" nil) string))

(defconst deterred-ai-windsurf-copilot-provider "windsurf-copilot"
  "Provider name used for Windsurf Copilot accepted completions.")

(defconst deterred-ai--parser-version deterred-ai-codex-parser-version
  "Version of the AI transcript parser and its persisted state.")

(defconst deterred-ai--gpt-5.6-long-context-threshold 272000
  "Input-token threshold above which GPT-5.6 uses long-context prices.")

(defvar deterred-ai--pricing-data nil)

(defvar deterred-ai--pricing-source-hash nil)

(defvar deterred-ai--pricing-revision-id nil)

(defconst deterred-ai--parse-progress-step 25
  "How often to report session parsing progress.")

(defun deterred-ai--time-zone-cache-key ()
  "Return a stable identity for the date-assignment time zone."
  (if deterred-ai-time-zone
      (concat "configured:" deterred-ai-time-zone)
    (let ((localtime "/etc/localtime"))
      (format "system:%s:%s"
              (or (getenv "TZ") "")
              (if (file-readable-p localtime)
                  (with-temp-buffer
                    (set-buffer-multibyte nil)
                    (insert-file-contents-literally localtime)
                    (secure-hash 'sha256 (current-buffer)))
                (prin1-to-string (current-time-zone)))))))

(defun deterred-ai--cache-parser-version ()
  "Return the parser/cache semantic version for the current settings."
  (format "%s|date-zone=%s"
          deterred-ai--parser-version (deterred-ai--time-zone-cache-key)))

(defun deterred-ai--ensure-pricing (callback &optional refresh)
  "Make sure that LLM pricing data from LiteLLM is fetched.

The data is stored in `deterred-ai--pricing-data'.

Call CALLBACK if the data has been fetched successfully or read from
archive.  When REFRESH is nil, prefer the on-disk snapshot and avoid a
network request."
  (cond
   ((and deterred-ai--pricing-data (not refresh))
    (funcall callback))
   ((and (not refresh) (file-exists-p deterred-ai-pricing-location))
    (deterred-ai--store-pricing
     (json-read-file (expand-file-name deterred-ai-pricing-location)))
    (funcall callback))
   (t
    (message "deterred-ai: fetching pricing data")
    (request deterred-ai--pricing-litellm-url
      :parser 'json-read
      :encoding 'utf-8
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (with-temp-file deterred-ai-pricing-location
                    (insert (json-encode data)))
                  (deterred-ai--store-pricing data)
                  (funcall callback)))
      :error (cl-function
              (lambda (&key response &allow-other-keys)
                (if (file-exists-p deterred-ai-pricing-location)
                    (progn
                      (deterred-ai--store-pricing
                       (json-read-file deterred-ai-pricing-location))
                      (message "Couldn't fetch LiteLLM pricing data; using archive.")
                      (funcall callback))
                  (deterred-utils-on-request-error :response response))))))))

(defun deterred-ai-refresh-pricing (&optional callback)
  "Refresh the pricing snapshot, then call CALLBACK when non-nil."
  (interactive)
  (deterred-ai--ensure-pricing
   (lambda ()
     (message "deterred-ai: refreshed pricing revision %s"
              (or deterred-ai--pricing-source-hash "unknown"))
     (when callback (funcall callback)))
   t))

(defun deterred-ai--gpt-5.6-sol-pricing ()
  "Return the audited GPT-5.6 Sol pricing record."
  '((input_cost_per_token . 0.000005)
    (cache_read_input_token_cost . 0.0000005)
    (output_cost_per_token . 0.00003)
    (cache_creation_input_token_cost . 0.00000625)
    (deterred_tiered_input_cost_per_token . 0.00001)
    (deterred_tiered_cache_read_input_token_cost . 0.000001)
    (deterred_tiered_output_cost_per_token . 0.000045)
    (deterred_tiered_cache_creation_input_token_cost . 0.0000125)
    (deterred_tiered_threshold . 272000)
    (deterred_tiered_whole_request . t)
    ;; API Priority processing for GPT-5.6 is billed at twice Standard.
    (deterred_priority_multiplier . 2.0)))

(defun deterred-ai--record-pricing-revision ()
  "Record and cache the current pricing revision in the database."
  (when deterred-ai--pricing-source-hash
    (condition-case err
        (let* ((db (deterred-db--init))
               (revision-id deterred-ai--pricing-source-hash)
               (existing
                (sqlite-select
                 db
                 "SELECT 1 FROM meta_ai_pricing_revision
                  WHERE pricing_revision_id = ?"
                 (list revision-id))))
          (unless existing
            (sqlite-execute
             db
             "INSERT INTO meta_ai_pricing_revision
              (pricing_revision_id, provider, source_hash, rules_json, created_at)
              VALUES (?, 'global', ?, ?, unixepoch())"
             (list revision-id deterred-ai--pricing-source-hash
                   (json-encode
                    `((parser_version . ,deterred-ai--parser-version)
                      (gpt_5_6_long_context_threshold
                       . ,deterred-ai--gpt-5.6-long-context-threshold)
                      (gpt_5_6_sol
                       . ,(deterred-ai--gpt-5.6-sol-pricing)))))))
          (setq deterred-ai--pricing-revision-id revision-id))
      (error
       ;; Loading pricing is also useful before the database is initialized.
       (message "deterred-ai: couldn't record pricing revision: %s" err)))))

(defun deterred-ai--store-pricing (data)
  "Store LLM pricing data.

DATA is a response from LiteLLM."
  ;; The local GPT-5.6 rule is part of the effective price book, so include it
  ;; in the revision hash instead of hashing only the upstream snapshot.
  (setq deterred-ai--pricing-source-hash
        (secure-hash
         'sha256
         (encode-coding-string
          (concat (json-encode data) "\0"
                  (json-encode (deterred-ai--gpt-5.6-sol-pricing)))
          'utf-8)))
  (setq deterred-ai--pricing-data (make-hash-table :test #'equal))
  (cl-loop for (k . v) in data
           do (puthash (symbol-name k) v deterred-ai--pricing-data))
  (puthash "gpt-5.6-sol"
           (deterred-ai--gpt-5.6-sol-pricing)
           deterred-ai--pricing-data)
  (deterred-ai--record-pricing-revision))

(defun deterred-ai--get-pricing-datum (model)
  "Get raw LiteLLM pricing data for MODEL.

`deterred-ai--ensure-pricing' has to be called before this."
  (unless deterred-ai--pricing-data
    (error "LLM pricing data has not been fetched"))
  (gethash model deterred-ai--pricing-data))

(defun deterred-ai--json-value (key alist)
  "Get value for KEY from ALIST, returning nil for `:json-null'."
  (let ((val (alist-get key alist)))
    (if (eq val :json-null) nil val)))

(defconst deterred-ai--claude-file-tool-names '("Edit" "Write" "NotebookEdit")
  "Tool names that modify files.")

(defun deterred-ai--claude-collect-jsonl-files (projects-dir)
  "Collect all JSONL files from PROJECTS-DIR.

Returns a list of alists with keys `jsonl-path' and `project-dir'."
  (let (result)
    (when (file-directory-p projects-dir)
      (dolist (project-dir (directory-files projects-dir t "\\`[^.]"))
        (when (file-directory-p project-dir)
          (dolist (entry (directory-files project-dir t))
            (let ((name (file-name-nondirectory entry)))
              (cond
               ;; Main session file: <session-id>.jsonl
               ((string-suffix-p ".jsonl" name)
                (push (list (cons 'jsonl-path entry)
                            (cons 'project-dir
                                  (file-name-nondirectory project-dir)))
                      result))
               ;; Session directory with subagents
               ((file-directory-p entry)
                (let ((subagents-dir (expand-file-name "subagents" entry)))
                  (when (file-directory-p subagents-dir)
                    (dolist (sa (directory-files subagents-dir t "\\.jsonl\\'"))
                      (push (list (cons 'jsonl-path sa)
                                  (cons 'project-dir
                                        (file-name-nondirectory project-dir)))
                            result)))))))))))
    (nreverse result)))

(defun deterred-ai--report-parse-progress (kind parsed total)
  "Report parsing progress for KIND with PARSED sessions out of TOTAL."
  (when (or (zerop parsed)
            (= parsed total)
            (zerop (% parsed deterred-ai--parse-progress-step)))
    (message "deterred-ai: parsed %d/%d %s sessions"
             parsed total kind)))

(defun deterred-ai--call-with-codex-progress (kind function)
  "Call FUNCTION with a byte-weighted progress callback for KIND sessions.

The callback accepts completed files, total files, current path,
cumulative bytes, and total bytes.  Return the value of FUNCTION."
  (let ((last-reported-files -1)
        reporter
        numeric
        finished)
    (unwind-protect
        (prog1
            (funcall
             function
             (lambda (done total path bytes total-bytes)
               (unless reporter
                 (setq numeric (> total-bytes 0)
                       reporter
                       (if numeric
                           (make-progress-reporter
                            (format "deterred-ai: parsing %s sessions..." kind)
                            0 total-bytes 0 0.0 0.2)
                         (make-progress-reporter
                          (format "deterred-ai: parsing %s sessions..." kind)))))
               (unless (= done last-reported-files)
                 (setq last-reported-files done)
                 (deterred-ai--report-parse-progress kind done total))
               (progress-reporter-update
                reporter (and numeric bytes)
                (format " (%d/%d files%s)"
                        done total
                        (if path
                            (format ", %s" (file-name-nondirectory path))
                          "")))))
          (setq finished t))
      (when reporter
        (if finished
            (progress-reporter-done reporter)
          (message "deterred-ai: parsing %s sessions stopped" kind))))))

(defun deterred-ai--codex-progress-supported-p ()
  "Return non-nil when the loaded Codex parser supports progress callbacks."
  (and (boundp 'deterred-ai-codex-api-version)
       (= (symbol-value 'deterred-ai-codex-api-version)
          deterred-ai--codex-api-version)))

(defun deterred-ai--require-codex-api ()
  "Reject a stale loaded Codex parser before it can rebuild stored data."
  (unless (deterred-ai--codex-progress-supported-p)
    (user-error
     (concat "Loaded Codex parser is stale; run M-x straight-rebuild-package "
             "for deterred, then restart Emacs"))))

(defun deterred-ai--codex-parse-all-with-progress
    (data-dir progress-callback)
  "Parse Codex DATA-DIR while dynamically binding PROGRESS-CALLBACK."
  (let ((deterred-ai-codex-corpus-progress-callback progress-callback))
    ;; Keep the parser's original public arity so an older implementation
    ;; already loaded from stale package bytecode cannot break this call.
    (deterred-ai-codex-parse-all data-dir)))

(defun deterred-ai--codex-parse-jsonl-with-progress
    (path progress-callback &optional prior-state byte-offset
          allow-incomplete-source)
  "Parse Codex PATH while dynamically binding PROGRESS-CALLBACK.

PRIOR-STATE, BYTE-OFFSET, and ALLOW-INCOMPLETE-SOURCE are forwarded to
the parser using its backward-compatible public arity."
  (let ((deterred-ai-codex-progress-callback progress-callback))
    (deterred-ai-codex-parse-jsonl
     path prior-state byte-offset allow-incomplete-source)))

(defconst deterred-ai--claude-usage-keys
  '(input_tokens output_tokens cache_creation_input_tokens
    cache_read_input_tokens)
  "Claude usage fields that must progress monotonically per message.")

(defun deterred-ai--timestamp-seconds (timestamp)
  "Convert ISO TIMESTAMP to integer Unix seconds."
  (truncate (float-time (encode-time (iso8601-parse timestamp)))))

(defun deterred-ai--usage-date (timestamp)
  "Return the configured calendar date for Unix TIMESTAMP."
  (format-time-string "%Y-%m-%d" (seconds-to-time timestamp)
                      deterred-ai-time-zone))

(defun deterred-ai--claude-usage (message)
  "Extract normalized usage from Claude MESSAGE."
  (let ((usage (alist-get 'usage message)))
    (mapcar (lambda (key)
              (cons key (or (deterred-ai--json-value key usage) 0)))
            deterred-ai--claude-usage-keys)))

(defun deterred-ai--claude-usage-monotonic-p (older newer)
  "Return non-nil when every usage component in NEWER is >= OLDER."
  (seq-every-p
   (lambda (key)
     (<= (or (alist-get key older) 0)
         (or (alist-get key newer) 0)))
   deterred-ai--claude-usage-keys))

(defun deterred-ai--claude-result-line-counts (tool-result)
  "Return (ADDED . REMOVED) line counts from TOOL-RESULT."
  (let ((patches (deterred-ai--json-value 'structuredPatch tool-result))
        (added 0)
        (removed 0))
    (when (vectorp patches)
      (cl-loop for patch across patches
               do (when-let* ((lines (alist-get 'lines patch))
                              (_ (vectorp lines)))
                    (cl-loop for line across lines
                             do (cond
                                 ((string-prefix-p "+" line) (cl-incf added))
                                 ((string-prefix-p "-" line) (cl-incf removed)))))))
    (cons added removed)))

(defun deterred-ai--claude-tool-result-ids (record)
  "Return tool-use IDs referenced by a Claude user RECORD."
  (let ((content (alist-get 'content (alist-get 'message record)))
        result)
    (when (vectorp content)
      (cl-loop for block across content
               when (equal (alist-get 'type block) "tool_result")
               do (when-let* ((id (deterred-ai--json-value 'tool_use_id block)))
                    (push id result))))
    (nreverse result)))

(defun deterred-ai--claude-file-merge (files file-path tool-name
                                             lines-added lines-removed)
  "Merge a Claude file touch into FILES and return FILES.

FILE-PATH identifies the touched file.  TOOL-NAME may be nil for a
result linked only through legacy UUID metadata.  LINES-ADDED and
LINES-REMOVED are accumulated change counts."
  (when file-path
    (let ((file (gethash file-path files)))
      (unless file
        (setq file `((file-path . ,file-path)
                     (lines-added . 0)
                     (lines-removed . 0)
                     (touch-count . 0)
                     (add-count . 0)
                     (update-count . 0)
                     (delete-count . 0)
                     (move-count . 0)
                     (previous-file-path . nil)))
        (puthash file-path file files))
      (when (or lines-added lines-removed)
        (cl-incf (alist-get 'touch-count file) 1)
        (cl-incf (alist-get 'lines-added file) (or lines-added 0))
        (cl-incf (alist-get 'lines-removed file) (or lines-removed 0))
        (if (equal tool-name "Write")
            (cl-incf (alist-get 'add-count file) 1)
          (cl-incf (alist-get 'update-count file) 1)))))
  files)

(defun deterred-ai--claude-parse-corpus (files)
  "Parse Claude FILES as one corpus and return unique final messages."
  (let ((candidates (make-hash-table :test #'equal))
        (uuid-to-message (make-hash-table :test #'equal))
        (tool-uses (make-hash-table :test #'equal))
        (tool-results (make-hash-table :test #'equal))
        (sequence 0)
        (parsed 0)
        (total (length files)))
    (deterred-ai--report-parse-progress "Claude" parsed total)
    (dolist (file-info files)
      (let* ((path (alist-get 'jsonl-path file-info))
             (source-key (file-relative-name
                          path (expand-file-name "projects"
                                                 deterred-ai-claude-data-dir)))
             (line-no 0))
        (with-temp-buffer
          (insert-file-contents path)
          (goto-char (point-min))
          (while (not (eobp))
            (cl-incf line-no)
            (let* ((line-end (line-end-position))
                   (complete-line (eq (char-after line-end) ?\n))
                   (line (buffer-substring-no-properties
                          (line-beginning-position) line-end))
                   (record
                    (condition-case err
                        (and (not (string-empty-p line))
                             (json-read-from-string line))
                      (error
                       (if (and (not complete-line) (= line-end (point-max)))
                           nil
                         (error "Malformed Claude JSONL %s:%d: %s"
                                path line-no err))))))
              (when record
                (cl-incf sequence)
                (pcase (alist-get 'type record)
                  ("assistant"
                   (when-let* ((message (alist-get 'message record))
                               (message-id (deterred-ai--json-value 'id message))
                               (timestamp-string (alist-get 'timestamp record)))
                     (let* ((timestamp
                             (deterred-ai--timestamp-seconds timestamp-string))
                            (candidate
                             `((sequence . ,sequence)
                               (timestamp . ,timestamp)
                               (timestamp-string . ,timestamp-string)
                               (session-id . ,(alist-get 'sessionId record))
                               (model-name . ,(alist-get 'model message))
                               (message-id . ,message-id)
                               (request-id . ,(deterred-ai--json-value
                                               'requestId record))
                               (cwd . ,(deterred-ai--json-value 'cwd record))
                               (version . ,(deterred-ai--json-value 'version record))
                               (source-key . ,source-key)
                               (usage . ,(deterred-ai--claude-usage message)))))
                       (push candidate (gethash message-id candidates))
                       (puthash message-id
                                (gethash message-id candidates)
                                candidates)
                       (when-let* ((uuid (deterred-ai--json-value 'uuid record)))
                         (puthash uuid message-id uuid-to-message))
                       (let ((content (alist-get 'content message)))
                         (when (vectorp content)
                           (cl-loop for block across content
                                    when (and
                                          (equal (alist-get 'type block) "tool_use")
                                          (member (alist-get 'name block)
                                                  deterred-ai--claude-file-tool-names))
                                    do (when-let* ((tool-id
                                                   (deterred-ai--json-value 'id block)))
                                         (let ((input (alist-get 'input block)))
                                           (puthash
                                            tool-id
                                            `((message-id . ,message-id)
                                              (file-path
                                               . ,(deterred-ai--json-value
                                                   'file_path input))
                                              (tool-name . ,(alist-get 'name block)))
                                            tool-uses)))))))))
                  ("user"
                   (when-let* ((tool-result (alist-get 'toolUseResult record))
                               (_ (consp tool-result)))
                     (let* ((ids (deterred-ai--claude-tool-result-ids record))
                            (source-uuid
                             (deterred-ai--json-value
                              'sourceToolAssistantUUID record))
                            (counts
                             (deterred-ai--claude-result-line-counts tool-result))
                            (result
                             `((tool-ids . ,ids)
                               (source-uuid . ,source-uuid)
                               (file-path . ,(deterred-ai--json-value
                                              'filePath tool-result))
                               (lines-added . ,(car counts))
                               (lines-removed . ,(cdr counts))))
                            ;; Replayed main/subagent/compaction histories
                            ;; contain copies with the same record UUID.
                            (result-key
                             (or (deterred-ai--json-value 'uuid record)
                                 (format "%S\0%s\0%s\0%s\0%d\0%d"
                                         ids source-uuid
                                         (alist-get 'file-path result)
                                         (alist-get 'timestamp record)
                                         (car counts) (cdr counts)))))
                       (puthash result-key result tool-results))))))
            (forward-line 1))))
      (cl-incf parsed)
      (deterred-ai--report-parse-progress "Claude" parsed total)))
    (let ((entries (make-hash-table :test #'equal)))
      (maphash
       (lambda (message-id values)
         (let* ((ordered
                (sort values
                       (lambda (a b)
                         ;; Claude replays the same message in multiple JSONL
                         ;; files.  Epoch seconds are too coarse: snapshots
                         ;; several milliseconds apart can otherwise be
                         ;; interleaved as 1, 227, 1, 227 and look decreasing.
                         (let ((ta (alist-get 'timestamp-string a))
                               (tb (alist-get 'timestamp-string b)))
                           (if (equal ta tb)
                               (< (alist-get 'sequence a)
                                  (alist-get 'sequence b))
                             (string< ta tb))))))
                (previous nil))
           (dolist (candidate ordered)
             (let ((usage (alist-get 'usage candidate)))
               (when (and previous
                          (not (deterred-ai--claude-usage-monotonic-p
                                previous usage)))
                 (error "Claude usage decreased for message %s" message-id))
               (setq previous usage)))
           (let* ((first (car ordered))
                  (last (car (last ordered)))
                  (usage (alist-get 'usage last))
                  (input (alist-get 'input_tokens usage))
                  (output (alist-get 'output_tokens usage))
                  (cache-create (alist-get 'cache_creation_input_tokens usage))
                  (cache-read (alist-get 'cache_read_input_tokens usage))
                  (turn-key (format "claude:%s" message-id))
                  (files (make-hash-table :test #'equal)))
             (maphash
              (lambda (_tool-id tool)
                (when (equal (alist-get 'message-id tool) message-id)
                  (deterred-ai--claude-file-merge
                   files (alist-get 'file-path tool) (alist-get 'tool-name tool)
                   nil nil)))
              tool-uses)
             (puthash
              message-id
              `((timestamp . ,(alist-get 'timestamp first))
                (end-timestamp . ,(alist-get 'timestamp last))
                (session-id . ,(alist-get 'session-id last))
                (model-name . ,(alist-get 'model-name last))
                (message-id . ,message-id)
                (turn-key . ,turn-key)
                (request-id . ,(alist-get 'request-id last))
                (cwd . ,(alist-get 'cwd last))
                (version . ,(alist-get 'version last))
                (provider . "claude")
                (record-kind . "claude-message")
                (data-quality . "observed")
                (source-key . ,(alist-get 'source-key last))
                (parser-version . ,deterred-ai--parser-version)
                (usage-date . ,(deterred-ai--usage-date
                                (alist-get 'timestamp last)))
                (parent-session-id . nil)
                (request-count . 1)
                (message-count . 1)
                (service-tier . "default")
                (input-tokens . ,input)
                (output-tokens . ,output)
                (reasoning-output-tokens . 0)
                (cache-creation-input-tokens . ,cache-create)
                (cache-read-input-tokens . ,cache-read)
                (tiered-input-tokens . 0)
                (tiered-output-tokens . 0)
                (tiered-cache-creation-input-tokens . 0)
                (tiered-cache-read-input-tokens . 0)
                (total-tokens . ,(+ input output cache-create cache-read))
                (files . ,files))
              entries))))
       candidates)
      (maphash
       (lambda (_result-key result)
         (let* ((tool-id
                 (seq-find (lambda (id) (gethash id tool-uses))
                           (alist-get 'tool-ids result)))
                (tool (and tool-id (gethash tool-id tool-uses)))
                (message-id
                 (or (alist-get 'message-id tool)
                     (gethash (alist-get 'source-uuid result) uuid-to-message)))
                (entry (and message-id (gethash message-id entries))))
           (when entry
             (let* ((files (alist-get 'files entry))
                    (path (or (alist-get 'file-path result)
                              (alist-get 'file-path tool))))
               (deterred-ai--claude-file-merge
                files path (alist-get 'tool-name tool)
                (alist-get 'lines-added result)
                (alist-get 'lines-removed result))))))
       tool-results)
      (let (result)
        (maphash
         (lambda (_message-id entry)
           (let (file-list)
             (maphash
              (lambda (_path file)
                (when (zerop (alist-get 'touch-count file))
                  (setf (alist-get 'touch-count file) 1))
                (push (cons (cons 'turn-key (alist-get 'turn-key entry)) file)
                      file-list))
              (alist-get 'files entry))
             (setf (alist-get 'files entry) file-list))
           (push entry result))
         entries)
        (seq-sort-by (lambda (entry) (alist-get 'timestamp entry)) #'< result)))))

(defun deterred-ai--component-cost (tokens tiered-tokens base-price tiered-price)
  "Price TOKENS when TIERED-TOKENS use TIERED-PRICE.

BASE-PRICE applies outside the tier.  The split is supplied by the
parser because some providers price the whole request above a
threshold, while older price records describe a marginal tier."
  (let* ((tokens (or tokens 0))
         (tiered (min tokens (max 0 (or tiered-tokens 0))))
         (base (- tokens tiered)))
    (+ (* base (or base-price 0))
       (* tiered (or tiered-price base-price 0)))))

(defun deterred-ai--service-tier-multiplier (model tier pricing)
  "Return the API price multiplier for MODEL, TIER, and PRICING.

Return nil when the tier cannot be priced without guessing."
  (cond
   ((member (or tier "default") '("default" "standard")) 1.0)
   ((and (equal model "gpt-5.6-sol")
         (equal tier "priority"))
    (deterred-ai--json-value 'deterred_priority_multiplier pricing))
   (t nil)))

(defun deterred-ai--populate-legacy-tiered-basis (entry)
  "Populate marginal tier fields in legacy ENTRY.

New Codex entries already contain a per-request pricing basis.  This
fallback exists for one-request Claude rows and preserves the old
LiteLLM marginal-tier behavior without reparsing them during repricing."
  (unless (alist-get 'pricing-basis-complete entry)
    (let ((threshold 200000))
      (dolist (pair '((input-tokens . tiered-input-tokens)
                      (output-tokens . tiered-output-tokens)
                      (cache-creation-input-tokens
                       . tiered-cache-creation-input-tokens)
                      (cache-read-input-tokens . tiered-cache-read-input-tokens)))
        (let* ((total (or (alist-get (car pair) entry) 0))
               (tiered (max 0 (- total threshold))))
          (setf (alist-get (cdr pair) entry) tiered)))))
  entry)

(defun deterred-ai--calculate-cost (entry)
  "Compute USD cost for a parsed ENTRY.

Uses LiteLLM pricing data.  `deterred-ai--ensure-pricing' must be
called before this.  Returns cost as a float in dollars."
  (let* ((model-name (alist-get 'model-name entry))
         (mapped-name (or (cdr (assoc model-name deterred-ai-model-name-map))
                          model-name))
         (pricing (condition-case nil
                      (deterred-ai--get-pricing-datum mapped-name)
                    (error nil)))
         (tier-multiplier
          (and pricing
               (deterred-ai--service-tier-multiplier
                mapped-name (alist-get 'service-tier entry) pricing))))
    (if (or (null pricing) (null tier-multiplier))
        nil
      (deterred-ai--populate-legacy-tiered-basis entry)
      (let ((input-price (or (deterred-ai--json-value 'input_cost_per_token pricing) 0))
            (input-tiered
             (or (deterred-ai--json-value
                  'deterred_tiered_input_cost_per_token pricing)
                 (deterred-ai--json-value
                  'input_cost_per_token_above_200k_tokens pricing)))
            (output-price (or (deterred-ai--json-value 'output_cost_per_token pricing) 0))
            (output-tiered
             (or (deterred-ai--json-value
                  'deterred_tiered_output_cost_per_token pricing)
                 (deterred-ai--json-value
                  'output_cost_per_token_above_200k_tokens pricing)))
            (cache-create-price (or (deterred-ai--json-value
                                     'cache_creation_input_token_cost pricing) 0))
            (cache-create-tiered
             (or (deterred-ai--json-value
                  'deterred_tiered_cache_creation_input_token_cost pricing)
                 (deterred-ai--json-value
                  'cache_creation_input_token_cost_above_200k_tokens pricing)))
            (cache-read-price (or (deterred-ai--json-value
                                   'cache_read_input_token_cost pricing) 0))
            (cache-read-tiered
             (or (deterred-ai--json-value
                  'deterred_tiered_cache_read_input_token_cost pricing)
                 (deterred-ai--json-value
                  'cache_read_input_token_cost_above_200k_tokens pricing))))
        (float
         (* tier-multiplier
            (+ (deterred-ai--component-cost
                (alist-get 'input-tokens entry)
                (alist-get 'tiered-input-tokens entry)
                input-price input-tiered)
               (deterred-ai--component-cost
                (alist-get 'output-tokens entry)
                (alist-get 'tiered-output-tokens entry)
                output-price output-tiered)
               (deterred-ai--component-cost
                (alist-get 'cache-creation-input-tokens entry)
                (alist-get 'tiered-cache-creation-input-tokens entry)
                cache-create-price cache-create-tiered)
               (deterred-ai--component-cost
                (alist-get 'cache-read-input-tokens entry)
                (alist-get 'tiered-cache-read-input-tokens entry)
                cache-read-price cache-read-tiered))))))))

(defun deterred-ai--match-project-id (path id-by-path)
  "Find wakatime project ID for PATH using ID-BY-PATH hash table.

Walks up directory components until a match is found."
  (when path
    (let* ((parts (file-name-split (expand-file-name path)))
           (i (seq-length parts)))
      (cl-block search
        (while (> i 0)
          (when-let* ((cand (apply #'file-name-concat "/" (seq-take parts i)))
                      (project-id (gethash cand id-by-path)))
            (cl-return-from search project-id))
          (setq i (1- i)))))))

(defun deterred-ai--project-id-by-path (&optional db)
  "Return a hash table mapping project roots to WakaTime project IDs.

DB is the sqlite database object."
  (let* ((db (or db (deterred-db--init)))
         (projects (deterred-db-select-alist
                    db "SELECT id, project_root FROM wakatime_projects
WHERE project_root IS NOT NULL AND name != 'Unknown Project'"))
         (id-by-path (make-hash-table :test 'equal)))
    (dolist (item projects)
      (puthash (alist-get 'project_root item)
               (alist-get 'id item) id-by-path))
    id-by-path))

(defun deterred-ai--match-project-ids (entries)
  "Match wakatime project IDs for ENTRIES.

Queries the database for project roots and matches `cwd' and file
paths against them.  Mutates ENTRIES in place."
  (let ((id-by-path (deterred-ai--project-id-by-path)))
    (dolist (entry entries)
      (nconc entry
             (list (cons 'project-id
                         (deterred-ai--match-project-id
                          (alist-get 'cwd entry) id-by-path))))
      (dolist (file (alist-get 'files entry))
        (nconc file
               (list (cons 'project-id
                           (deterred-ai--match-project-id
                            (alist-get 'file-path file) id-by-path)))))))
  entries)

(defun deterred-ai--alist-set! (alist key value)
  "Destructively set KEY to VALUE in ALIST, including when KEY is absent."
  (if-let* ((cell (assq key alist)))
      (setcdr cell value)
    ;; `(setf (alist-get KEY ALIST) VALUE)' only changes the local ALIST
    ;; variable when KEY is absent; appending keeps the caller's list head.
    (nconc alist (list (cons key value))))
  value)

(defun deterred-ai--postprocess (entries)
  "Post-process parsed ENTRIES with cost, project ID, and hostname.

Adds `usd-cost', `hostname', and `project-id' to each entry and its
files.  `deterred-ai--ensure-pricing' must be called before this."
  (message "deterred-ai: postprocessing %d entries" (length entries))
  (let ((hostname (system-name)))
    (dolist (entry entries)
      (let ((cost (deterred-ai--calculate-cost entry)))
        (deterred-ai--alist-set! entry 'usd-cost (or cost 0.0))
        (deterred-ai--alist-set!
         entry 'pricing-status
         (cond
          ((zerop (or (alist-get 'total-tokens entry) 0))
           "not-applicable")
          (cost "priced")
          (t "unknown")))
        (deterred-ai--alist-set!
         entry 'pricing-revision-id deterred-ai--pricing-revision-id)
        (deterred-ai--alist-set!
         entry 'hostname (or (alist-get 'hostname entry) hostname))
        (deterred-ai--alist-set!
         entry 'provider (or (alist-get 'provider entry) "unknown"))
        (deterred-ai--alist-set!
         entry 'record-kind
         (or (alist-get 'record-kind entry) "legacy-token-event"))
        (deterred-ai--alist-set!
         entry 'data-quality
         (or (alist-get 'data-quality entry) "observed"))
        (deterred-ai--alist-set!
         entry 'parser-version
         (or (alist-get 'parser-version entry) deterred-ai--parser-version))
        (deterred-ai--alist-set!
         entry 'usage-date
         (or (alist-get 'usage-date entry)
             (deterred-ai--usage-date (alist-get 'timestamp entry)))))))
  (deterred-ai--match-project-ids entries)
  entries)

(defun deterred-ai--format-tokens (n)
  "Format token count N for human display."
  (cond
   ((>= n 1000000) (format "%.1fM" (/ n 1000000.0)))
   ((>= n 1000) (format "%.1fk" (/ n 1000.0)))
   (t (number-to-string n))))

(cl-defun deterred-ai--store
    (entries &key (conflict-action 'do-nothing) db (transaction t))
  "Store parsed and postprocessed ENTRIES into the database.

Converts entry alists to DB-compatible format and inserts them using
CONFLICT-ACTION.  DB overrides the default connection; TRANSACTION
controls whether this function opens one.  Never deletes existing data."
  (let ((db (or db (deterred-db--init)))
        item-values
        file-values)
    (dolist (entry entries)
      (push `((message_id  . ,(alist-get 'message-id entry))
              (timestamp   . ,(alist-get 'timestamp entry))
              (session_id  . ,(alist-get 'session-id entry))
              (model_name  . ,(alist-get 'model-name entry))
              (total_tokens . ,(alist-get 'total-tokens entry))
              (usd_cost    . ,(round (* (alist-get 'usd-cost entry) 1000000)))
              (is_stats    . ,(or (alist-get 'is-stats entry) 0))
              (hostname    . ,(alist-get 'hostname entry))
              (cwd         . ,(alist-get 'cwd entry))
              (project_id  . ,(alist-get 'project-id entry))
              (request_id  . ,(alist-get 'request-id entry))
              (version     . ,(alist-get 'version entry))
              (input_tokens . ,(alist-get 'input-tokens entry))
              (output_tokens . ,(alist-get 'output-tokens entry))
              (cache_creation_input_tokens
               . ,(alist-get 'cache-creation-input-tokens entry))
              (cache_read_input_tokens
               . ,(alist-get 'cache-read-input-tokens entry))
              (provider . ,(alist-get 'provider entry))
              (record_kind . ,(alist-get 'record-kind entry))
              (data_quality . ,(alist-get 'data-quality entry))
              (source_key . ,(alist-get 'source-key entry))
              (parser_version . ,(alist-get 'parser-version entry))
              (usage_date . ,(alist-get 'usage-date entry))
              (end_timestamp . ,(or (alist-get 'end-timestamp entry)
                                    (alist-get 'timestamp entry)))
              (turn_key . ,(or (alist-get 'turn-key entry)
                               (alist-get 'message-id entry)))
              (parent_session_id . ,(alist-get 'parent-session-id entry))
              (request_count . ,(alist-get 'request-count entry))
              (message_count . ,(alist-get 'message-count entry))
              (service_tier . ,(or (alist-get 'service-tier entry) "default"))
              (reasoning_output_tokens
               . ,(or (alist-get 'reasoning-output-tokens entry) 0))
              (tiered_input_tokens
               . ,(or (alist-get 'tiered-input-tokens entry) 0))
              (tiered_output_tokens
               . ,(or (alist-get 'tiered-output-tokens entry) 0))
              (tiered_cache_creation_input_tokens
               . ,(or (alist-get 'tiered-cache-creation-input-tokens entry) 0))
              (tiered_cache_read_input_tokens
               . ,(or (alist-get 'tiered-cache-read-input-tokens entry) 0))
              (pricing_revision_id . ,(alist-get 'pricing-revision-id entry))
              (pricing_status . ,(or (alist-get 'pricing-status entry) "unknown")))
            item-values)
      (dolist (file (alist-get 'files entry))
        (when (alist-get 'file-path file)
          (push `((message_id  . ,(alist-get 'message-id entry))
                  (project_id  . ,(alist-get 'project-id file))
                  (file_path   . ,(alist-get 'file-path file))
                  (lines_added . ,(alist-get 'lines-added file))
                  (lines_removed . ,(alist-get 'lines-removed file))
                  (turn_key . ,(or (alist-get 'turn-key file)
                                   (alist-get 'turn-key entry)
                                   (alist-get 'message-id entry)))
                  (touch_count . ,(or (alist-get 'touch-count file) 1))
                  (add_count . ,(or (alist-get 'add-count file) 0))
                  (update_count . ,(or (alist-get 'update-count file) 0))
                  (delete_count . ,(or (alist-get 'delete-count file) 0))
                  (move_count . ,(or (alist-get 'move-count file) 0))
                  (previous_file_path . ,(alist-get 'previous-file-path file)))
                file-values))))
    (when item-values
      (cl-labels
          ((insert-values
            ()
        (deterred-db-insert-unsafe
         db :table-name 'ai_usage_item
         :values item-values
         :conflict-action conflict-action
         :conflict-attrs '(message_id))
        (when file-values
          (deterred-db-insert-unsafe
           db :table-name 'ai_usage_file
           :values file-values
           :conflict-action conflict-action
           :conflict-attrs '(message_id file_path)))
        (deterred-db-mark-updated db 'ai_usage_item)
             (deterred-db-mark-updated db 'ai_usage_file)))
        (if transaction
            (with-sqlite-transaction db (insert-values))
          (insert-values))))
    (message "deterred-ai: stored %d items, %d files"
             (length item-values) (length file-values))))

(defun deterred-ai--file-bytes (path start end)
  "Return literal bytes from PATH between START and END."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path nil start end)
    (buffer-string)))

(defun deterred-ai--complete-file-offset (path size)
  "Return byte offset after the last complete line in PATH of SIZE."
  (if (zerop size)
      0
    ;; In-progress response records can be much larger than 64 KiB.  Walk
    ;; backwards until a newline is found so an unusually long incomplete
    ;; tail does not make an otherwise unchanged rollout reparse forever.
    (let ((end size)
          complete-offset)
      (while (and (> end 0) (null complete-offset))
        (let* ((start (max 0 (- end 65536)))
               (chunk (deterred-ai--file-bytes path start end))
               (newline (cl-position ?\n chunk :from-end t)))
          (if newline
              (setq complete-offset (+ start newline 1))
            (setq end start))))
      (or complete-offset 0))))

(defun deterred-ai--offset-boundary-hash (path offset)
  "Hash the bytes immediately preceding OFFSET in PATH."
  (secure-hash
   'sha256
   (deterred-ai--file-bytes path (max 0 (- offset 4096)) offset)))

(defun deterred-ai--file-fingerprint (path source-key)
  "Return an import fingerprint alist for PATH and SOURCE-KEY."
  (let* ((attrs (file-attributes path 'integer))
         (identifier (file-attribute-file-identifier attrs))
         (mtime (time-convert (file-attribute-modification-time attrs)
                              1000000000))
         (size (file-attribute-size attrs))
         (offset (deterred-ai--complete-file-offset path size)))
    `((source-key . ,source-key)
      (source-path . ,(expand-file-name path))
      ;; `file-attribute-file-identifier' returns (INODE DEVICE).
      (device-id . ,(cadr identifier))
      (inode . ,(car identifier))
      (size-bytes . ,size)
      (mtime-ns . ,(car mtime))
      (parsed-byte-offset . ,offset)
      (offset-boundary-hash . ,(deterred-ai--offset-boundary-hash path offset)))))

(defun deterred-ai--cache-row (db provider source-key)
  "Return cached import metadata from DB for PROVIDER and SOURCE-KEY."
  (car (deterred-db-select-alist
        db
        "SELECT * FROM meta_ai_import_file
         WHERE provider = ? AND hostname = ? AND source_key = ?"
        (list provider (system-name) source-key))))

(defun deterred-ai--cache-row-by-path (db provider path)
  "Return cached import metadata from DB for PROVIDER and absolute PATH."
  (car (deterred-db-select-alist
        db
        "SELECT * FROM meta_ai_import_file
         WHERE provider = ? AND hostname = ? AND source_path = ?"
        (list provider (system-name) (expand-file-name path)))))

(defun deterred-ai--fingerprint-equal-p (fingerprint cached)
  "Return non-nil when FINGERPRINT and CACHED describe the same file."
  (and cached
       (equal (alist-get 'parser_version cached)
              (deterred-ai--cache-parser-version))
       (= (alist-get 'device-id fingerprint) (alist-get 'device_id cached))
       (= (alist-get 'inode fingerprint) (alist-get 'inode cached))
       (= (alist-get 'size-bytes fingerprint) (alist-get 'size_bytes cached))
       (= (alist-get 'mtime-ns fingerprint) (alist-get 'mtime_ns cached))
       (equal (alist-get 'offset-boundary-hash fingerprint)
              (alist-get 'offset_boundary_hash cached))))

(defun deterred-ai--manifest-fingerprint (fingerprints)
  "Return a stable hash for FINGERPRINTS."
  (secure-hash
   'sha256
   (mapconcat
    (lambda (fp)
      (format "%s\0%s\0%s\0%s\0%s"
              (alist-get 'source-key fp)
              (alist-get 'device-id fp)
              (alist-get 'inode fp)
              (alist-get 'size-bytes fp)
              (alist-get 'mtime-ns fp)))
    (sort (copy-sequence fingerprints)
          (lambda (a b)
            (string-lessp (alist-get 'source-key a)
                          (alist-get 'source-key b))))
    "\n")))

(defun deterred-ai--cache-manifest-current-p (db provider fingerprints record-kind)
  "Return non-nil if PROVIDER FINGERPRINTS are current in DB.

RECORD-KIND identifies the authoritative aggregate being checked."
  (let* ((hostname (system-name))
         (cached-count
          (caar (sqlite-select
                 db
                 "SELECT COUNT(*) FROM meta_ai_import_file
                  WHERE provider = ? AND hostname = ?"
                 (list provider hostname))))
         (authority
          (sqlite-select
           db
           "SELECT 1 FROM meta_ai_authoritative_source
            WHERE hostname = ? AND provider = ? AND record_kind = ?
              AND parser_version = ?"
           (list hostname provider record-kind
                 (deterred-ai--cache-parser-version)))))
    (and authority
         (= cached-count (length fingerprints))
         (seq-every-p
          (lambda (fp)
            (deterred-ai--fingerprint-equal-p
             fp (deterred-ai--cache-row db provider
                                        (alist-get 'source-key fp))))
          fingerprints))))

(defun deterred-ai--cache-upsert (db provider fingerprint)
  "Store one PROVIDER FINGERPRINT and parser state in DB."
  (let* ((path (alist-get 'source-path fingerprint))
         (session-id (alist-get 'session-id fingerprint))
         (parser-state (alist-get 'parser-state fingerprint))
         (offset (or (alist-get 'parsed-offset fingerprint)
                     (alist-get 'parsed-byte-offset fingerprint)))
         (parsed-line-number
          (alist-get 'parsed-line-number fingerprint))
         (boundary (deterred-ai--offset-boundary-hash path offset)))
    (sqlite-execute
     db
     "INSERT INTO meta_ai_import_file
      (provider, hostname, source_key, source_path, session_id,
       device_id, inode, size_bytes, mtime_ns, parsed_byte_offset,
       parsed_line_number, offset_boundary_hash, parser_version,
       parser_state_json, last_success_at, last_error_at, last_error)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, unixepoch(), NULL, NULL)
      ON CONFLICT(provider, hostname, source_key) DO UPDATE SET
       source_path = excluded.source_path,
       session_id = excluded.session_id,
       device_id = excluded.device_id,
       inode = excluded.inode,
       size_bytes = excluded.size_bytes,
       mtime_ns = excluded.mtime_ns,
       parsed_byte_offset = excluded.parsed_byte_offset,
       parsed_line_number = excluded.parsed_line_number,
       offset_boundary_hash = excluded.offset_boundary_hash,
       parser_version = excluded.parser_version,
       parser_state_json = excluded.parser_state_json,
       last_success_at = excluded.last_success_at,
       last_error_at = NULL,
       last_error = NULL"
     (list provider (system-name) (alist-get 'source-key fingerprint) path
           session-id (alist-get 'device-id fingerprint)
           (alist-get 'inode fingerprint) (alist-get 'size-bytes fingerprint)
           (alist-get 'mtime-ns fingerprint) offset
           (or parsed-line-number 0) boundary
           (deterred-ai--cache-parser-version)
           (cond ((null parser-state) nil)
                 ((stringp parser-state) parser-state)
                 (t (json-encode parser-state)))))))

(defun deterred-ai--entry-file-count (entries)
  "Return the number of file rows in ENTRIES."
  (cl-loop for entry in entries sum (length (alist-get 'files entry))))

(defun deterred-ai--mark-authoritative
    (db provider record-kind source-count usage-count file-count fingerprint)
  "Store authority metadata for PROVIDER RECORD-KIND in DB.

SOURCE-COUNT, USAGE-COUNT, FILE-COUNT, and FINGERPRINT describe the
successfully imported slice for the current host."
  (sqlite-execute
   db
   "INSERT INTO meta_ai_authoritative_source
    (hostname, provider, record_kind, parser_version, completed_at,
     source_count, usage_row_count, file_row_count, source_fingerprint)
    VALUES (?, ?, ?, ?, unixepoch(), ?, ?, ?, ?)
    ON CONFLICT(hostname, provider, record_kind) DO UPDATE SET
     parser_version = excluded.parser_version,
     completed_at = excluded.completed_at,
     source_count = excluded.source_count,
     usage_row_count = excluded.usage_row_count,
     file_row_count = excluded.file_row_count,
     source_fingerprint = excluded.source_fingerprint"
   (list (system-name) provider record-kind
         (deterred-ai--cache-parser-version) source-count usage-count
         file-count fingerprint)))

(defun deterred-ai--replace-provider-slice
    (entries provider record-kind fingerprints)
  "Atomically replace current-host PROVIDER RECORD-KIND with ENTRIES."
  (let* ((db (deterred-db--init))
         (hostname (system-name))
         (source-fingerprint
          (deterred-ai--manifest-fingerprint fingerprints))
         (superseded-kinds
          (if (equal provider "codex")
              '("codex-turn-model" "legacy-token-event")
            (list record-kind)))
         (kind-placeholders
          (string-join (make-list (length superseded-kinds) "?") ", "))
         (where (format "hostname = ? AND provider = ? AND record_kind IN (%s)"
                        kind-placeholders))
         (params (append (list hostname provider) superseded-kinds))
         (file-count (deterred-ai--entry-file-count entries)))
    (with-sqlite-transaction db
      (sqlite-execute
       db
       (format "DELETE FROM ai_usage_file
                WHERE message_id IN
                  (SELECT message_id FROM ai_usage_item WHERE %s)" where)
       params)
      (sqlite-execute db (format "DELETE FROM ai_usage_item WHERE %s" where) params)
      (deterred-ai--store entries :conflict-action 'do-update
                          :db db :transaction nil)
      (sqlite-execute
       db "DELETE FROM meta_ai_import_file WHERE provider = ? AND hostname = ?"
       (list provider hostname))
      (dolist (fingerprint fingerprints)
        (deterred-ai--cache-upsert db provider fingerprint))
      (deterred-ai--mark-authoritative
       db provider record-kind (length fingerprints) (length entries)
       file-count source-fingerprint))
    (message "deterred-ai: replaced %s with %d usage and %d file rows"
             provider (length entries) file-count)))

(defun deterred-ai--assert-existing-ids-retained (db provider record-kind entries)
  "Signal when ENTRIES would lose a DB ID for PROVIDER RECORD-KIND."
  (let ((new-ids (make-hash-table :test #'equal)))
    (dolist (entry entries)
      (puthash (alist-get 'message-id entry) t new-ids))
    (dolist (row (deterred-db-select-alist
                  db
                  "SELECT message_id FROM ai_usage_item
                   WHERE hostname = ? AND provider = ? AND record_kind = ?"
                  (list (system-name) provider record-kind)))
      (unless (gethash (alist-get 'message_id row) new-ids)
        (error "Refusing to rebuild %s: raw data lacks stored ID %s"
               provider (alist-get 'message_id row))))))

(defun deterred-ai--load-claude-incremental (&optional callback force)
  "Load Claude data only when its corpus changed.

When FORCE is non-nil, rebuild even if fingerprints are unchanged.
Call CALLBACK when non-nil."
  (let* ((db (deterred-db--init))
         (projects-dir (expand-file-name "projects" deterred-ai-claude-data-dir))
         (files (deterred-ai--claude-collect-jsonl-files projects-dir))
         (fingerprints
          (mapcar
           (lambda (file)
             (deterred-ai--file-fingerprint
              (alist-get 'jsonl-path file)
              (file-relative-name (alist-get 'jsonl-path file) projects-dir)))
           files)))
    (cond
     ((null files)
      (message "deterred-ai: no Claude JSONL files found; preserving stored data")
      (when callback (funcall callback)))
     ((and (not force)
           (deterred-ai--cache-manifest-current-p
            db "claude" fingerprints "claude-message"))
      (message "deterred-ai: Claude corpus unchanged (%d files); skipped parsing"
               (length files))
      (when callback (funcall callback)))
     (t
      (deterred-ai--ensure-pricing
       (lambda ()
         (let ((entries (deterred-ai--postprocess
                         (deterred-ai--claude-parse-corpus files))))
           (deterred-ai--assert-existing-ids-retained
            db "claude" "claude-message" entries)
           (deterred-ai--replace-provider-slice
            entries "claude" "claude-message" fingerprints)
           (when callback (funcall callback)))))))))

(defun deterred-ai--codex-cache-status (cached fingerprint)
  "Classify CACHED Codex metadata against FINGERPRINT."
  (cond
   ((null cached) 'new)
   ((not (equal (alist-get 'parser_version cached)
                (deterred-ai--cache-parser-version)))
    'reparse)
   ((deterred-ai--fingerprint-equal-p fingerprint cached) 'unchanged)
   ((and (= (alist-get 'device-id fingerprint) (alist-get 'device_id cached))
         (= (alist-get 'inode fingerprint) (alist-get 'inode cached))
         (> (alist-get 'size-bytes fingerprint) (alist-get 'size_bytes cached))
         (let ((offset (alist-get 'parsed_byte_offset cached)))
           (and offset
                (<= offset (alist-get 'size-bytes fingerprint))
                (equal (deterred-ai--offset-boundary-hash
                        (alist-get 'source-path fingerprint) offset)
                       (alist-get 'offset_boundary_hash cached)))))
    'append)
   (t 'reparse)))

(defun deterred-ai--codex-source-fingerprint (source)
  "Build a cache fingerprint from parsed Codex SOURCE metadata."
  (let* ((path (alist-get 'jsonl-path source))
         (fingerprint
          (deterred-ai--file-fingerprint path (alist-get 'source-key source)))
         (state (alist-get 'state source)))
    (append
     fingerprint
     `((session-id . ,(alist-get 'rollout-id source))
       (parser-state . ,state)
       (parsed-offset . ,(alist-get 'parsed-offset source))
       (parsed-line-number . ,(or (alist-get 'line-number state) 0))))))

(defun deterred-ai--codex-full-rebuild (&optional callback)
  "Parse and atomically rebuild all available current-host Codex data.

Call CALLBACK when non-nil."
  (deterred-ai--ensure-pricing
   (lambda ()
     (let* ((parsed
             (deterred-ai--call-with-codex-progress
              "Codex"
              (lambda (progress-callback)
                (deterred-ai--codex-parse-all-with-progress
                 deterred-ai-codex-data-dir progress-callback))))
            (entries
             (progn
               (message "deterred-ai: postprocessing parsed Codex usage")
               (deterred-ai--postprocess (plist-get parsed :entries))))
            (fingerprints
             (mapcar #'deterred-ai--codex-source-fingerprint
                     (plist-get parsed :sources))))
       (message "deterred-ai: validating %d parsed Codex usage rows"
                (length entries))
       ;; Once canonical rows exist, a rebuild must not silently discard a
       ;; rollout which is no longer present on this machine.  This is what
       ;; preserves imported history from unavailable hosts and partial local
       ;; archives while still allowing the one-time legacy conversion.
       (deterred-ai--assert-existing-ids-retained
        (deterred-db--init) "codex" "codex-turn-model" entries)
       (message "deterred-ai: replacing current-host Codex database rows")
       (deterred-ai--replace-provider-slice
        entries "codex" "codex-turn-model" fingerprints)
       (when callback (funcall callback))))))

(defun deterred-ai--row-number (row key)
  "Return numeric KEY in database ROW, treating NULL as zero."
  (or (alist-get key row) 0))

(defun deterred-ai--merge-additive-entry (db entry)
  "Merge additive Codex ENTRY with its existing row in DB."
  (let ((row (car (deterred-db-select-alist
                   db "SELECT * FROM ai_usage_item WHERE message_id = ?"
                   (list (alist-get 'message-id entry))))))
    (if (null row)
        entry
      (let ((merged (copy-tree entry)))
        (dolist (mapping '((input-tokens . input_tokens)
                           (output-tokens . output_tokens)
                           (cache-creation-input-tokens
                            . cache_creation_input_tokens)
                           (cache-read-input-tokens . cache_read_input_tokens)
                           (reasoning-output-tokens . reasoning_output_tokens)
                           (total-tokens . total_tokens)
                           (tiered-input-tokens . tiered_input_tokens)
                           (tiered-output-tokens . tiered_output_tokens)
                           (tiered-cache-creation-input-tokens
                            . tiered_cache_creation_input_tokens)
                           (tiered-cache-read-input-tokens
                            . tiered_cache_read_input_tokens)
                           (request-count . request_count)))
          (setf (alist-get (car mapping) merged)
                (+ (or (alist-get (car mapping) merged) 0)
                   (deterred-ai--row-number row (cdr mapping)))))
        (setf (alist-get 'timestamp merged)
              (min (alist-get 'timestamp merged) (alist-get 'timestamp row)))
        (setf (alist-get 'end-timestamp merged)
              (max (or (alist-get 'end-timestamp merged) 0)
                   (or (alist-get 'end_timestamp row) 0)))
        (setf (alist-get 'message-count merged)
              (max (or (alist-get 'message-count merged) 0)
                   (deterred-ai--row-number row 'message_count)))
        (setf (alist-get 'data-quality merged)
              (if (or (equal (alist-get 'data-quality merged) "partial")
                      (equal (alist-get 'data_quality row) "partial"))
                  "partial"
                "observed"))
        (dolist (file (alist-get 'files merged))
          (when-let* ((old
                       (car
                        (deterred-db-select-alist
                         db
                         "SELECT * FROM ai_usage_file
                          WHERE message_id = ? AND file_path = ?"
                         (list (alist-get 'message-id merged)
                               (alist-get 'file-path file))))))
            (dolist (mapping '((lines-added . lines_added)
                               (lines-removed . lines_removed)
                               (touch-count . touch_count)
                               (add-count . add_count)
                               (update-count . update_count)
                               (delete-count . delete_count)
                               (move-count . move_count)))
              (setf (alist-get (car mapping) file)
                    (+ (or (alist-get (car mapping) file) 0)
                       (deterred-ai--row-number old (cdr mapping)))))))
        ;; Reprice the complete aggregate.  Summing the old and new costs
        ;; would mislabel a row if the pricing snapshot changed between
        ;; append-only imports.
        (let ((cost (deterred-ai--calculate-cost merged)))
          (setf (alist-get 'usd-cost merged) (or cost 0.0))
          (setf (alist-get 'pricing-status merged)
                (cond
                 ((zerop (or (alist-get 'total-tokens merged) 0))
                  "not-applicable")
                 (cost "priced")
                 (t "unknown")))
          (setf (alist-get 'pricing-revision-id merged)
                deterred-ai--pricing-revision-id))
        merged))))

(defun deterred-ai--delete-codex-source (db source-key)
  "Delete current-host Codex rows belonging to SOURCE-KEY from DB."
  (let ((params (list (system-name) source-key)))
    (sqlite-execute
     db
     "DELETE FROM ai_usage_file
      WHERE message_id IN
       (SELECT message_id FROM ai_usage_item
        WHERE hostname = ? AND provider = 'codex' AND source_key = ?)"
     params)
    (sqlite-execute
     db
     "DELETE FROM ai_usage_item
      WHERE hostname = ? AND provider = 'codex' AND source_key = ?"
     params)))

(defun deterred-ai--mark-codex-authoritative (db fingerprints)
  "Update Codex authority metadata in DB for FINGERPRINTS."
  (let* ((hostname (system-name))
         (usage-count
          (caar (sqlite-select
                 db
                 "SELECT COUNT(*) FROM ai_usage_item
                  WHERE hostname = ? AND provider = 'codex'
                    AND record_kind = 'codex-turn-model'"
                 (list hostname))))
         (file-count
          (caar (sqlite-select
                 db
                 "SELECT COUNT(*) FROM ai_usage_file
                  WHERE message_id IN
                   (SELECT message_id FROM ai_usage_item
                    WHERE hostname = ? AND provider = 'codex'
                      AND record_kind = 'codex-turn-model')"
                 (list hostname))))
         (source-count
          (caar (sqlite-select
                 db
                 "SELECT COUNT(*) FROM meta_ai_import_file
                  WHERE hostname = ? AND provider = 'codex'"
                 (list hostname))))
         (manifest (deterred-ai--manifest-fingerprint fingerprints)))
    (deterred-ai--mark-authoritative
     db "codex" "codex-turn-model" source-count usage-count file-count
     manifest)))

(defun deterred-ai--load-codex-incremental (&optional callback force)
  "Incrementally load Codex JSONL, or rebuild when FORCE is non-nil.

Call CALLBACK when non-nil."
  (deterred-ai--require-codex-api)
  (message "deterred-ai: preparing database (first-run migrations may take a while)")
  (redisplay)
  (let* ((db (deterred-db--init))
         (descriptions
          (progn
            (message "deterred-ai: scanning Codex session files")
            (deterred-ai-codex-collect-jsonl-files
             deterred-ai-codex-data-dir)))
         (authority
          (sqlite-select
           db
           "SELECT 1 FROM meta_ai_authoritative_source
           WHERE hostname = ? AND provider = 'codex'
              AND record_kind = 'codex-turn-model'
              AND parser_version = ?"
           (list (system-name) (deterred-ai--cache-parser-version)))))
    (message "deterred-ai: found %d Codex session files; checking cache"
             (length descriptions))
    (cond
     ((null descriptions)
      (message "deterred-ai: no Codex JSONL files found; preserving stored data")
      (when callback (funcall callback)))
     ((or force (null authority))
      (deterred-ai--codex-full-rebuild callback))
     (t
      (let (work)
        (dolist (description descriptions)
          (let* ((path (alist-get 'jsonl-path description))
                 (cached (deterred-ai--cache-row-by-path db "codex" path))
                 (source-key
                  (or (alist-get 'source_key cached)
                      (alist-get 'source-file-key description)))
                 (fingerprint (deterred-ai--file-fingerprint path source-key))
                 (status (deterred-ai--codex-cache-status cached fingerprint)))
            (unless (eq status 'unchanged)
              (push (list description cached status) work))))
        (if (null work)
            (progn
              (message "deterred-ai: Codex sessions unchanged (%d files); skipped parsing"
                       (length descriptions))
              (when callback (funcall callback)))
          (deterred-ai--ensure-pricing
           (lambda ()
             (let* ((total-work-files (length work))
                    (total-work-bytes
                     (cl-loop
                      for item in work
                      for description = (car item)
                      for cached = (cadr item)
                      for status = (caddr item)
                      for size = (file-attribute-size
                                  (file-attributes
                                   (alist-get 'jsonl-path description)))
                      for offset = (if (eq status 'append)
                                       (or (alist-get
                                            'parsed_byte_offset cached)
                                           0)
                                     0)
                      sum (- size offset)))
                    (completed-work-files 0)
                    (completed-work-bytes 0)
                    staged)
               ;; Parse all changed sources before opening a write transaction.
               (deterred-ai--call-with-codex-progress
                "changed Codex"
                (lambda (progress-callback)
                  (funcall progress-callback
                           0 total-work-files nil 0 total-work-bytes)
                  (dolist (item work)
                    (pcase-let* ((`(,description ,cached ,status) item)
                                 (path (alist-get 'jsonl-path description))
                                 (file-size
                                  (file-attribute-size
                                   (file-attributes path)))
                                 (initial-offset
                                  (if (eq status 'append)
                                      (or (alist-get
                                           'parsed_byte_offset cached)
                                          0)
                                    0))
                                 (work-size (- file-size initial-offset)))
                      (cl-labels
                          ((report-current-file
                            (_path read-offset _file-size _line-number)
                            (funcall progress-callback
                                     completed-work-files total-work-files path
                                     (+ completed-work-bytes
                                        (- read-offset initial-offset))
                                     total-work-bytes)))
                        (let* ((state
                                (and (eq status 'append)
                                     (alist-get 'parser_state_json cached)
                                     (let ((json-object-type 'alist)
                                           (json-array-type 'list)
                                           (json-key-type 'symbol))
                                       (json-read-from-string
                                        (alist-get
                                         'parser_state_json cached)))))
                               (initial-parsed
                                (if (eq status 'append)
                                    (deterred-ai--codex-parse-jsonl-with-progress
                                     path #'report-current-file state
                                     initial-offset nil)
                                  (deterred-ai--codex-parse-jsonl-with-progress
                                   path #'report-current-file)))
                               (identity-changed
                                (and (eq status 'append)
                                     (not (equal
                                           (alist-get 'source_key cached)
                                           (alist-get
                                            'source-key
                                            (plist-get
                                             initial-parsed :source))))))
                               (effective-status
                                (if identity-changed 'reparse status))
                               ;; A rollout can first be cached while its
                               ;; session_meta line is incomplete.  Once that
                               ;; line completes, discard the fallback identity
                               ;; delta and rebuild under the real rollout ID.
                               (parsed
                                (if identity-changed
                                    (progn
                                      (message
                                       "deterred-ai: rollout identity appeared; reparsing %s"
                                       (file-name-nondirectory path))
                                      (deterred-ai-codex-parse-jsonl path))
                                  initial-parsed))
                               (entries
                                (deterred-ai--postprocess
                                 (plist-get parsed :entries)))
                               (source
                                (append
                                 description
                                 (plist-get parsed :source)
                                 `((parsed-offset
                                    . ,(plist-get parsed :parsed-offset))
                                   (state . ,(plist-get parsed :state)))))
                               (fingerprint
                                (deterred-ai--codex-source-fingerprint source)))
                          (push (list effective-status entries fingerprint cached)
                                staged)))
                      (cl-incf completed-work-files)
                      (cl-incf completed-work-bytes work-size)
                      (funcall progress-callback
                               completed-work-files total-work-files path
                               completed-work-bytes total-work-bytes)))
                  staged))
               (message "deterred-ai: writing %d changed Codex sessions"
                        (length work))
               (with-sqlite-transaction db
                 (dolist (stage staged)
                   (pcase-let ((`(,status ,entries ,fingerprint ,cached) stage))
                     (if (eq status 'append)
                         (deterred-ai--store
                          (mapcar (lambda (entry)
                                    (deterred-ai--merge-additive-entry db entry))
                                  entries)
                          :conflict-action 'do-update :db db :transaction nil)
                       (let ((old-source-key (alist-get 'source_key cached))
                             (new-source-key
                              (alist-get 'source-key fingerprint)))
                         (when old-source-key
                           (deterred-ai--delete-codex-source db old-source-key)
                           (unless (equal old-source-key new-source-key)
                             (sqlite-execute
                              db
                              "DELETE FROM meta_ai_import_file
                               WHERE provider = 'codex' AND hostname = ?
                                 AND source_key = ?"
                              (list (system-name) old-source-key))))
                         (unless (equal old-source-key new-source-key)
                           (deterred-ai--delete-codex-source db new-source-key))
                         (deterred-ai--store
                          entries :conflict-action 'do-update
                          :db db :transaction nil)))
                     (deterred-ai--cache-upsert db "codex" fingerprint)))
                 ;; Rebuild the current manifest from disk after successful parsing.
                 (let ((updated-fingerprints nil))
                   (dolist (description descriptions)
                     (let* ((path (alist-get 'jsonl-path description))
                            (cached (deterred-ai--cache-row-by-path db "codex" path))
                            (source-key
                             (or (alist-get 'source_key cached)
                                 (alist-get 'source-file-key description))))
                       (push (deterred-ai--file-fingerprint path source-key)
                             updated-fingerprints)))
                   (deterred-ai--mark-codex-authoritative
                    db updated-fingerprints)))
               (message "deterred-ai: incrementally parsed %d/%d Codex files"
                        (length work) (length descriptions))
               (when callback (funcall callback)))))))))))

(defun deterred-ai--backfill-table-project-ids
    (db table-name path-attr id-by-path &optional extra-where)
  "Backfill `project_id' in TABLE-NAME by matching PATH-ATTR to WakaTime roots.

DB is the sqlite database object.  ID-BY-PATH is a hash table as
returned by `deterred-ai--project-id-by-path'.  EXTRA-WHERE is
appended to the WHERE clause."
  (let* ((table-str (symbol-name table-name))
         (path-str (symbol-name path-attr))
         (path-key (intern path-str))
         (rows (deterred-db-select-alist
                db
                (format "SELECT rowid AS row_id, %s
                         FROM %s
                         WHERE project_id IS NULL
                           AND %s IS NOT NULL%s"
                        path-str table-str path-str
                        (if extra-where
                            (concat "\n AND " extra-where)
                          ""))))
         (updated 0))
    (dolist (row rows)
      (when-let* ((project-id
                   (deterred-ai--match-project-id
                    (alist-get path-key row) id-by-path)))
        (deterred-db-execute-trace
         db
         (format "UPDATE %s
                  SET project_id = ?
                  WHERE rowid = ?"
                 table-str)
         (list project-id (alist-get 'row_id row)))
        (cl-incf updated)))
    updated))

(defun deterred-ai-backfill-project-ids (&optional callback)
  "Backfill missing AI `project_id' values using `wakatime_projects'.

This updates path-based AI tables, including accepted completions.
If CALLBACK is non-nil, call it when done."
  (interactive)
  (let* ((db (deterred-db--init))
         (id-by-path (deterred-ai--project-id-by-path db))
         (item-updated 0)
         (file-updated 0)
         (completion-updated 0))
    (with-sqlite-transaction db
      (setq item-updated
            (deterred-ai--backfill-table-project-ids
             db 'ai_usage_item 'cwd id-by-path "is_stats = 0"))
      (setq file-updated
            (deterred-ai--backfill-table-project-ids
             db 'ai_usage_file 'file_path id-by-path))
      (setq completion-updated
            (deterred-ai--backfill-table-project-ids
             db 'ai_accepted_completions 'filename id-by-path))
      (when (> item-updated 0)
        (deterred-db-mark-updated db 'ai_usage_item))
      (when (> file-updated 0)
        (deterred-db-mark-updated db 'ai_usage_file))
      (when (> completion-updated 0)
        (deterred-db-mark-updated db 'ai_accepted_completions)))
    (message
     "deterred-ai: backfilled project IDs: %d items, %d files, %d completions"
     item-updated file-updated completion-updated)
    (when callback
      (funcall callback))))

;;;###autoload
(defun deterred-ai-windsurf-copilot-accept-completion-hook (info)
  "Store a Windsurf Copilot accepted completion from hook INFO."
  (let* ((accepted (or (plist-get info :accepted) ""))
         (buffer (plist-get info :buffer))
         (filename (or (plist-get info :file)
                       (when (buffer-live-p buffer)
                         (buffer-file-name buffer)))))
    (when filename
      (let ((db (deterred-db--init))
            (timestamp (time-convert nil 'integer)))
        (sqlite-execute
         db
         "INSERT INTO ai_accepted_completions
          (timestamp, hostname, filename, length, provider, project_id)
          VALUES (?, ?, ?, ?, ?, ?)
          ON CONFLICT (hostname, timestamp) DO NOTHING"
         (list timestamp
               (system-name)
               (expand-file-name filename)
               (length accepted)
               deterred-ai-windsurf-copilot-provider
               nil))))))

(defun deterred-ai-load (&optional callback)
  "Load AI usage data from Claude Code and Codex JSONL files into DETERRED.

If CALLBACK is non-nil, call it when done."
  (interactive)
  (deterred-ai--load-claude-incremental
   (lambda ()
     (deterred-ai--load-codex-incremental callback))))

(defun deterred-ai-load-claude (&optional callback)
  "Load AI usage data from Claude Code JSONL files into DETERRED.

If CALLBACK is non-nil, call it when done."
  (interactive)
  (deterred-ai--load-claude-incremental callback))

(defun deterred-ai-load-codex (&optional callback)
  "Load AI usage data from Codex JSONL files into DETERRED.

If CALLBACK is non-nil, call it when done."
  (interactive)
  (deterred-ai--load-codex-incremental callback))

(defun deterred-ai-rebuild-claude (&optional callback)
  "Reparse and atomically rebuild available local Claude JSONL data.

Stored aggregate stats, other hostnames, and rows whose raw IDs are no
longer available are preserved.  Call CALLBACK when non-nil."
  (interactive)
  (deterred-ai--load-claude-incremental callback t))

(defun deterred-ai-rebuild-codex (&optional callback)
  "Reparse and atomically rebuild available local Codex JSONL data.

Other hostnames are never touched.  Once canonical Codex data exists,
the rebuild refuses to lose IDs whose raw rollout is unavailable.  Call
CALLBACK when non-nil."
  (interactive)
  (deterred-ai--load-codex-incremental callback t))

(defun deterred-ai-remove-model-prefixes (&optional callback)
  "Remove configured prefixes from stored AI model names.

The prefixes in `deterred-ai-model-prefixes-to-remove' are applied in
order to `ai_usage_item.model_name'.  Return the number of updated rows.
Call CALLBACK when non-nil."
  (interactive)
  (let ((db (deterred-db--init))
        (prefixes (delete-dups
                   (copy-sequence deterred-ai-model-prefixes-to-remove)))
        (updated 0))
    (dolist (prefix prefixes)
      (unless (and (stringp prefix) (not (string-empty-p prefix)))
        (user-error "AI model prefixes must be non-empty strings: %S" prefix)))
    (with-sqlite-transaction db
      (dolist (prefix prefixes)
        (sqlite-execute
         db
         "UPDATE ai_usage_item
          SET model_name = substr(model_name, ?)
          WHERE substr(model_name, 1, ?) = ?"
         (list (1+ (length prefix)) (length prefix) prefix))
        (cl-incf updated (caar (sqlite-select db "SELECT changes()"))))
      (when (> updated 0)
        (deterred-db-mark-updated db 'ai_usage_item)))
    (message "deterred-ai: removed model prefixes from %d items" updated)
    (when callback
      (funcall callback))
    updated))

(defun deterred-ai-compact-database ()
  "Back up and compact the DETERRED database explicitly.

Logical AI row compaction is performed by migration 35.  This command
checkpoints the WAL, creates a unique consistent pre-compaction copy,
runs VACUUM to reclaim the freed pages, and asks SQLite to optimize its
indexes."
  (interactive)
  (let* ((db (deterred-db--init))
         (checkpoint (car (sqlite-select db "PRAGMA wal_checkpoint(FULL)")))
         (busy (car checkpoint))
         (backup-dir (expand-file-name deterred-backups-location))
         (database-name (file-name-nondirectory
                         (expand-file-name deterred-db-location)))
         (backup-path
          (make-temp-name
           (expand-file-name
            (format "%s.%s.pre-compact.%s."
                    database-name (system-name)
                    (format-time-string "%Y%m%dT%H%M%S"))
            backup-dir))))
    (unless (and (integerp busy) (zerop busy))
      (error "Couldn't checkpoint DETERRED WAL safely: %S" checkpoint))
    (mkdir backup-dir t)
    ;; VACUUM INTO uses SQLite's own snapshot machinery, so this backup is
    ;; consistent and is created even if a normal daily rotation already ran.
    (sqlite-execute db "VACUUM INTO ?" (list backup-path))
    (unless (file-exists-p backup-path)
      (error "SQLite did not create pre-compaction backup %s" backup-path))
    (message "deterred-ai: compacting database (backup: %s)" backup-path)
    (sqlite-execute db "VACUUM")
    (sqlite-execute db "PRAGMA optimize")
    (message "deterred-ai: database compaction complete; backup: %s"
             backup-path)))

(defconst deterred-ai--pricing-recalc-batch-size 500
  "Batch size for recalculating stored AI pricing.")

(defun deterred-ai--recalculate-stored-pricing ()
  "Recalculate pricing for observed rows with stored pricing bases."
  (let* ((db (deterred-db--init))
         (rows (deterred-db-select-alist
                db "SELECT message_id,
                           model_name,
                           service_tier,
                           total_tokens,
                           input_tokens,
                           output_tokens,
                           cache_creation_input_tokens,
                           cache_read_input_tokens,
                           tiered_input_tokens,
                           tiered_output_tokens,
                           tiered_cache_creation_input_tokens,
                           tiered_cache_read_input_tokens
                    FROM ai_usage_item
                    WHERE record_kind IN ('codex-turn-model', 'claude-message')
                      AND data_quality IN ('observed', 'partial')"))
         (total (length rows))
         (batch-size deterred-ai--pricing-recalc-batch-size)
         (offset 0))
    (while (< offset total)
      (let ((batch (seq-subseq rows offset (min total (+ offset batch-size)))))
        (with-sqlite-transaction db
          (dolist (row batch)
            (let ((entry
                   (list
                    (cons 'model-name (alist-get 'model_name row))
                    (cons 'service-tier (alist-get 'service_tier row))
                    (cons 'total-tokens (alist-get 'total_tokens row))
                    (cons 'input-tokens (alist-get 'input_tokens row))
                    (cons 'output-tokens (alist-get 'output_tokens row))
                    (cons 'cache-creation-input-tokens
                          (alist-get 'cache_creation_input_tokens row))
                    (cons 'cache-read-input-tokens
                          (alist-get 'cache_read_input_tokens row))
                    (cons 'tiered-input-tokens
                          (alist-get 'tiered_input_tokens row))
                    (cons 'tiered-output-tokens
                          (alist-get 'tiered_output_tokens row))
                    (cons 'tiered-cache-creation-input-tokens
                          (alist-get 'tiered_cache_creation_input_tokens row))
                    (cons 'tiered-cache-read-input-tokens
                          (alist-get 'tiered_cache_read_input_tokens row))
                    (cons 'pricing-basis-complete t))))
              (let ((cost (deterred-ai--calculate-cost entry)))
              (deterred-db-execute-trace
               db
               "UPDATE ai_usage_item
                SET usd_cost = ?, pricing_status = ?, pricing_revision_id = ?
                WHERE message_id = ?"
               (list (if cost (round (* cost 1000000)) 0)
                     (cond
                      ((zerop (or (alist-get 'total_tokens row) 0))
                       "not-applicable")
                      (cost "priced")
                      (t "unknown"))
                     deterred-ai--pricing-revision-id
                     (alist-get 'message_id row))))))))
      (setq offset (+ offset batch-size)))
    (deterred-db-mark-updated db 'ai_usage_item)
    (message "deterred-ai: recalculated pricing for %d items" total)))

(defun deterred-ai-recalculate-pricing (&optional callback)
  "Recalculate observed AI usage pricing without reparsing transcripts.

Call CALLBACK when non-nil."
  (interactive)
  (deterred-ai--ensure-pricing
   (lambda ()
     (deterred-ai--recalculate-stored-pricing)
     (when callback (funcall callback)))))

(defun deterred-ai--claude-parse-stats (stats-path)
  "Parse a Claude Code stats-cache JSON file at STATS-PATH.

Returns one aggregate entry per date and model.  The estimated number
of messages is kept in `message-count' rather than expanded into rows.
Uses `dailyActivity' for message counts, `dailyModelTokens' for
per-model token totals, and `modelUsage' for token-type ratios."
  (let* ((json-object-type 'alist)
         (json-array-type 'vector)
         (json-key-type 'symbol)
         (data (json-read-file (expand-file-name stats-path)))
         (hostname (system-name))
         ;; Build date → messageCount lookup
         (msg-counts (make-hash-table :test 'equal))
         ;; Build per-model token-type ratios from modelUsage
         (model-ratios (make-hash-table :test 'equal))
         result)
    ;; Parse dailyActivity into msg-counts
    (cl-loop for entry across (alist-get 'dailyActivity data)
             do (puthash (alist-get 'date entry)
                         (alist-get 'messageCount entry)
                         msg-counts))
    ;; Parse modelUsage into ratios
    (cl-loop
     for (model-sym . usage) in (alist-get 'modelUsage data)
     do (let* ((model (symbol-name model-sym))
               (inp (or (alist-get 'inputTokens usage) 0))
               (out (or (alist-get 'outputTokens usage) 0))
               (cr (or (alist-get 'cacheReadInputTokens usage) 0))
               (cc (or (alist-get 'cacheCreationInputTokens usage) 0))
               (total (+ inp out cr cc)))
          (puthash model
                   (if (zerop total)
                       '((input . 0.25) (output . 0.25)
                         (cache-read . 0.25) (cache-create . 0.25))
                     `((input . ,(/ (float inp) total))
                       (output . ,(/ (float out) total))
                       (cache-read . ,(/ (float cr) total))
                       (cache-create . ,(/ (float cc) total))))
                   model-ratios)))
    ;; Iterate dailyModelTokens
    (cl-loop
     for day-entry across (alist-get 'dailyModelTokens data)
     do (let* ((date-str (alist-get 'date day-entry))
               (timestamp (truncate
                           (float-time
                            (encode-time
                             (decoded-time-set-defaults
                              (iso8601-parse-date date-str))))))
               (tokens-by-model (alist-get 'tokensByModel day-entry))
               (day-msg-count (or (gethash date-str msg-counts) 1))
               ;; Sum all tokens for this day to distribute messages
               (day-total-tokens
                (cl-loop for (_m . toks) in tokens-by-model sum toks))
               ;; Distribute messages proportionally, track remainder
               (allocated 0)
               (model-allocations nil))
          ;; First pass: allocate messages proportionally
          (cl-loop
           for (model-sym . model-tokens) in tokens-by-model
           do (let* ((model (symbol-name model-sym))
                     (frac (if (zerop day-total-tokens) 1.0
                             (/ (float model-tokens) day-total-tokens)))
                     (n (max 1 (round (* day-msg-count frac)))))
                (push (list model model-tokens n) model-allocations)
                (setq allocated (+ allocated n))))
          ;; Adjust: add/remove from largest allocation to match total
          (when (and model-allocations (/= allocated day-msg-count))
            (let ((biggest (car (seq-sort-by #'cl-caddr #'> model-allocations))))
              (setf (cl-caddr biggest)
                    (max 1 (+ (cl-caddr biggest) (- day-msg-count allocated))))))
          ;; Create fake entries
          (dolist (alloc model-allocations)
            (let* ((model (car alloc))
                   (model-tokens (cadr alloc))
                   (n-msgs (cl-caddr alloc))
                   ;; Preserve the former per-message estimator exactly, but
                   ;; store its sums in one row.  This keeps pricing thresholds
                   ;; per estimated request instead of applying them once to a
                   ;; whole day.
                   (tokens-per-msg (/ model-tokens n-msgs))
                   (ratios (or (gethash model model-ratios)
                               '((input . 0.25) (output . 0.25)
                                 (cache-read . 0.25) (cache-create . 0.25))))
                   (input-per-msg
                    (round (* tokens-per-msg (alist-get 'input ratios))))
                   (output-per-msg
                    (round (* tokens-per-msg (alist-get 'output ratios))))
                   (cache-create-per-msg
                    (round (* tokens-per-msg
                              (alist-get 'cache-create ratios))))
                   (cache-read-per-msg
                    (round (* tokens-per-msg
                              (alist-get 'cache-read ratios)))))
              (let ((message-id (format "stats:%s:%s:%s"
                                        hostname date-str model)))
                (push
                 `((message-id . ,message-id)
                  (timestamp . ,timestamp)
                  (end-timestamp . ,timestamp)
                  (session-id . "fake_stats")
                  (model-name . ,model)
                  (total-tokens . ,(* n-msgs tokens-per-msg))
                  (input-tokens . ,(* n-msgs input-per-msg))
                  (output-tokens . ,(* n-msgs output-per-msg))
                  (cache-creation-input-tokens
                   . ,(* n-msgs cache-create-per-msg))
                  (cache-read-input-tokens
                   . ,(* n-msgs cache-read-per-msg))
                  (reasoning-output-tokens . 0)
                  (tiered-input-tokens
                   . ,(* n-msgs (max 0 (- input-per-msg 200000))))
                  (tiered-output-tokens
                   . ,(* n-msgs (max 0 (- output-per-msg 200000))))
                  (tiered-cache-creation-input-tokens
                   . ,(* n-msgs (max 0 (- cache-create-per-msg 200000))))
                  (tiered-cache-read-input-tokens
                   . ,(* n-msgs (max 0 (- cache-read-per-msg 200000))))
                  (pricing-basis-complete . t)
                  (hostname . ,hostname)
                  (provider . "claude")
                  (record-kind . "claude-stats")
                  (data-quality . "estimated")
                  (source-key . ,(expand-file-name stats-path))
                  (parser-version . ,deterred-ai--parser-version)
                  (usage-date . ,date-str)
                  (turn-key . nil)
                  (parent-session-id . nil)
                  (request-count . nil)
                  (message-count . ,n-msgs)
                  (service-tier . "default")
                  (is-stats . 1)
                  (cwd . nil)
                  (project-id . nil)
                  (request-id . nil)
                  (version . nil)
                  (files . nil))
                 result))))))
    (nreverse result)))

(cl-defun deterred-ai--claude-store-stats
    (entries &key (conflict-action 'do-update))
  "Store aggregate stats ENTRIES, skipping dates with real data.

Queries the database for dates that already have is_stats = 0
entries and filters ENTRIES to exclude those dates.  CONFLICT-ACTION
controls how an existing aggregate row is handled."
  (let* ((db (deterred-db--init))
         (real-dates
          (mapcar (lambda (row) (alist-get 'day row))
                  (deterred-db-select-alist
                   db "SELECT DISTINCT COALESCE(usage_date,
                                                date(timestamp, 'unixepoch')) day
FROM ai_usage_item
WHERE is_stats = 0 AND hostname = ? AND provider = 'claude'"
                   (list (system-name)))))
         (real-dates-set (make-hash-table :test 'equal))
         filtered)
    (dolist (d real-dates)
      (puthash d t real-dates-set))
    ;; Filter out entries whose date matches real data
    (dolist (entry entries)
      (let ((date-str (alist-get 'usage-date entry)))
        (unless (gethash date-str real-dates-set)
          (push entry filtered))))
    (setq filtered (nreverse filtered))
    (when filtered
      (deterred-ai--store (deterred-ai--postprocess filtered)
                          :conflict-action conflict-action))
    (message "deterred-ai: stats: %d entries parsed, %d skipped (real data), %d stored"
             (length entries)
             (- (length entries) (length filtered))
             (length filtered))))

(defun deterred-ai--claude-load-stats ()
  "Load AI usage data from a Claude Code stats-cache JSON file.

Prompts for the file path, parses it, and stores fake entries for
dates that have no real JSONL data."
  (interactive)
  (let ((stats-path (read-file-name
                     "Stats cache JSON: "
                     deterred-ai-claude-data-dir
                     (expand-file-name "stats-cache.json"
                                       deterred-ai-claude-data-dir)
                     t)))
    (deterred-ai--ensure-pricing
     (lambda ()
       (let ((entries (deterred-ai--claude-parse-stats stats-path)))
         (deterred-ai--claude-store-stats entries))))))

;;;###autoload
(defclass deterred-ai-usage (deterred-source)
  ((name :initform "AI Usage")
   (warn-days :initform 1))
  "DETERRED source for AI usage data (Claude Code and Codex).")

(cl-defmethod deterred-source-actions ((_source deterred-ai-usage) &optional callback)
  "Run an action for the AI usage source.

Run CALLBACK when done."
  (deterred-source--actions-pick
   '(("Load Claude JSONL" deterred-ai-load-claude t)
     ("Load Codex JSONL" deterred-ai-load-codex t)
     ("Rebuild Claude JSONL" deterred-ai-rebuild-claude t)
     ("Rebuild Codex JSONL" deterred-ai-rebuild-codex t)
     ("Refresh pricing snapshot" deterred-ai-refresh-pricing t)
     ("Recalculate pricing" deterred-ai-recalculate-pricing t)
     ("Backfill project IDs" deterred-ai-backfill-project-ids t)
     ("Load Claude stats cache JSON" deterred-ai--claude-load-stats nil)
     ("Back up and compact database" deterred-ai-compact-database nil))
   callback))

(cl-defmethod deterred-source-sync ((_source deterred-ai-usage) &optional callback)
  "Sync AI usage data from Claude Code and Codex JSONL files.

Call CALLBACK when done."
  (deterred-ai-load callback))

(cl-defmethod deterred-source-range ((_source deterred-ai-usage) &optional db)
  "Get the data availability range for AI Usage.

DB is the sqlite database object."
  (let* ((db (or db (deterred-db--init)))
         (data (sqlite-select
                db "SELECT MIN(timestamp), MAX(timestamp)
                    FROM ai_usage_item WHERE is_stats = 0")))
    (cons (caar data) (cadar data))))

(cl-defmethod deterred-source-range-summary
  ((_source deterred-ai-usage) start end &optional db)
  "Make AI Usage summary for [START, END].

DB is the sqlite database object."
  (let* ((db (or db (deterred-db--init)))
         (totals
          (car (deterred-db-select-alist
                db "SELECT
                           COUNT(DISTINCT CASE
                             WHEN turn_key IS NOT NULL THEN turn_key END)
                           + COALESCE(SUM(CASE
                               WHEN turn_key IS NULL
                               THEN COALESCE(message_count, 1)
                               ELSE 0 END), 0) turn_count,
                           COALESCE(SUM(request_count), 0) request_count,
                           COALESCE(SUM(total_tokens), 0) total_tokens,
                           COALESCE(SUM(usd_cost), 0) total_cost
                    FROM ai_usage_item
                    WHERE timestamp BETWEEN ? AND ?
                      AND is_stats = 0"
                (list start end))))
         (turn-count (alist-get 'turn_count totals))
         (request-count (alist-get 'request_count totals))
         (total-cost (alist-get 'total_cost totals))
         (model-data
          (deterred-db-select-alist
           db "SELECT model_name,
                      COUNT(DISTINCT CASE
                        WHEN turn_key IS NOT NULL THEN turn_key END)
                      + COALESCE(SUM(CASE
                          WHEN turn_key IS NULL
                          THEN COALESCE(message_count, 1)
                          ELSE 0 END), 0) turn_count,
                      COALESCE(SUM(request_count), 0) request_count,
                      SUM(total_tokens) total_tokens,
                      SUM(usd_cost) total_cost
               FROM ai_usage_item
               WHERE timestamp BETWEEN ? AND ?
                 AND is_stats = 0
               GROUP BY model_name
               ORDER BY total_cost DESC"
           (list start end))))
    (when (> turn-count 0)
      `((:short-description
         . ,(format "$%.2f, %s turns, %s requests, %s tokens"
                    (/ total-cost 1000000.0)
                    turn-count
                    request-count
                    (deterred-ai--format-tokens
                     (alist-get 'total_tokens totals))))
        (:long-description
         . ,(deterred-format
             "Per model:\n"
             (f-mapconcat
              (f "- " (f-acc "iter->'model_name")
                 ": $" (format "%.2f"
                               (/ (float (alist-get 'total_cost iter))
                                  1000000.0))
                 ", " (f-num (alist-get 'turn_count iter)) " turns"
                 ", " (f-num (alist-get 'request_count iter)) " requests"
                 ", " (deterred-ai--format-tokens
                       (alist-get 'total_tokens iter)) " tokens")
              model-data)))))))

(cl-defmethod deterred-source-events
  ((_source deterred-ai-usage) start end &optional params db)
  "Return AI usage message events for [START, END].

PARAMS may contain the same filters as the AI dashboard, namely
`:start-date', `:end-date', `:hostname', `:model' and `:projects'.
The third event field is the project name, so default grouping is by
project.

DB is the sqlite database object."
  (let ((db (or db (deterred-db--init))))
    (deterred-db-select-template
     db
     "SELECT ai.timestamp, null, COALESCE(wp.name, '(No project)')
FROM ai_usage_item ai
LEFT JOIN wakatime_projects wp ON wp.id = ai.project_id
WHERE ai.is_stats = 0
  AND ai.timestamp BETWEEN :start AND :end
  [[AND ai.timestamp >= :start-date]]
  [[AND ai.timestamp <= :end-date]]
  [[AND ai.hostname IN :hostname]]
  [[AND ai.model_name IN :model]]
  [[AND ai.project_id IN :projects]]
ORDER BY ai.timestamp"
     (append params `((:start . ,start) (:end . ,end))))))

(declare-function deterred-dashboard-ai "deterred-dashboard-ai")

(cl-defmethod deterred-source-default-dashboard ((_source deterred-ai-usage))
  "Return the default dashboard for AI usage."
  (deterred-dashboard-ai))

(provide 'deterred-ai)
;;; deterred-ai.el ends here

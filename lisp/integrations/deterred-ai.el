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

;; TOOD

;;; Code:
(require 'deterred-db)
(require 'deterred-source)
(require 'deterred-format)
(require 'deterred-utils)
(require 'request)
(require 'iso8601)
(require 'subr-x)
(require 'cl-lib)

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
  '(("kimi-for-coding" . "moonshot/kimi-k2.5"))
  "Alist mapping model names to LiteLLM-compatible names."
  :group 'deterred
  :type '(alist :key-type string :value-type string))

(defconst deterred-ai-windsurf-copilot-provider "windsurf-copilot"
  "Provider name used for Windsurf Copilot accepted completions.")

(defconst deterred-ai--tiered-threshold 200000
  "Token threshold for tiered pricing (200k).")

(defvar deterred-ai--pricing-data nil)

(defvar deterred-ai--pricing-updated nil)

(defun deterred-ai--ensure-pricing (callback)
  "Make sure that LLM pricing data from LiteLLM is fetched.

The data is stored in `deterred-ai--pricing-data'.
`deterred-ai--pricing-updated' is set to t if the data has been
fetched from the Internet.

Call CALLBACK if the data has been fetched successfully or read from
archive."
  (if deterred-ai--pricing-updated
      (funcall callback)
    (request deterred-ai--pricing-litellm-url
      :parser 'json-read
      :encoding 'utf-8
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (with-temp-file deterred-ai-pricing-location
                    (insert (json-encode data)))
                  (setq deterred-ai--pricing-updated t)
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
                  (deterred-utils-on-request-error :response response)))))))

(defun deterred-ai--store-pricing (data)
  "Store LLM pricing data.

DATA is a response from LiteLLM."
  (setq deterred-ai--pricing-data (make-hash-table :test #'equal))
  (cl-loop for (k . v) in data
           do (puthash (symbol-name k) v deterred-ai--pricing-data)))

(defun deterred-ai--get-pricing-datum (model)
  "Get raw LiteLLM pricing data for MODEL.

`deterred-ai--ensure-pricing' has to be called before this."
  (unless deterred-ai--pricing-data
    (error "LLM pricing data has not been fetched"))
  (or
   (gethash model deterred-ai--pricing-data)
   (progn
     (let ((matching-keys
            (cl-loop for k being the hash-keys of deterred-ai--pricing-data
                     if (string-match-p (rx bos (literal model)) k)
                     collect k)))
       (if (> (seq-length matching-keys) 1)
           (error "More than 1 match for %s: %s" model matching-keys)
         (puthash model
                  (gethash (car matching-keys) deterred-ai--pricing-data)
                  deterred-ai--pricing-data)))
     (gethash model deterred-ai--pricing-data))
   (error "No matches for %s" model)))

(defun deterred-ai--json-value (key alist)
  "Get value for KEY from ALIST, returning nil for `:json-null'."
  (let ((val (alist-get key alist)))
    (if (eq val :json-null) nil val)))

(defconst deterred-ai--claude-file-tool-names '("Edit" "Write" "NotebookEdit")
  "Tool names that modify files.")

(defun deterred-ai--claude-process-assistant-record (record messages uuid-to-msg-id)
  "Extract data from an assistant RECORD into MESSAGES hash table.

MESSAGES is keyed by message.id.  UUID-TO-MSG-ID maps record uuid to
message.id for linking user records."
  (when-let* ((message (alist-get 'message record))
              (msg-id (deterred-ai--json-value 'id message)))
    (let ((uuid (alist-get 'uuid record))
          (usage (alist-get 'usage message)))
      (puthash uuid msg-id uuid-to-msg-id)
      (unless (gethash msg-id messages)
        (puthash msg-id
                 (list
                  (cons 'timestamp
                        (truncate
                         (float-time
                          (encode-time
                           (iso8601-parse (alist-get 'timestamp record))))))
                  (cons 'session-id (alist-get 'sessionId record))
                  (cons 'model-name (alist-get 'model message))
                  (cons 'message-id msg-id)
                  (cons 'request-id (deterred-ai--json-value 'requestId record))
                  (cons 'cwd (deterred-ai--json-value 'cwd record))
                  (cons 'version (deterred-ai--json-value 'version record))
                  (cons 'input-tokens
                        (deterred-ai--json-value 'input_tokens usage))
                  (cons 'output-tokens
                        (deterred-ai--json-value 'output_tokens usage))
                  (cons 'cache-creation-input-tokens
                        (deterred-ai--json-value 'cache_creation_input_tokens usage))
                  (cons 'cache-read-input-tokens
                        (deterred-ai--json-value 'cache_read_input_tokens usage))
                  (cons 'total-tokens
                        (+ (or (deterred-ai--json-value 'input_tokens usage) 0)
                           (or (deterred-ai--json-value 'output_tokens usage) 0)
                           (or (deterred-ai--json-value 'cache_creation_input_tokens usage) 0)
                           (or (deterred-ai--json-value 'cache_read_input_tokens usage) 0)))
                  (cons 'files nil))
                 messages))
      ;; Scan content for file-modifying tool_use blocks
      (when-let* ((content (alist-get 'content message))
                  (_ (vectorp content)))
        (cl-loop for block across content
                 when (and (equal (alist-get 'type block) "tool_use")
                           (member (alist-get 'name block)
                                   deterred-ai--claude-file-tool-names))
                 do (let* ((input (alist-get 'input block))
                           (file-path (deterred-ai--json-value 'file_path input))
                           (tool-name (alist-get 'name block))
                           (entry (gethash msg-id messages))
                           (existing-files (alist-get 'files entry)))
                      (unless (cl-find file-path existing-files
                                       :key (lambda (f) (alist-get 'file-path f))
                                       :test #'equal)
                        (setf (alist-get 'files entry)
                              (append existing-files
                                      (list
                                       (list
                                        (cons 'file-path file-path)
                                        (cons 'tool-name tool-name)
                                        (cons 'lines-added nil)
                                        (cons 'lines-removed nil))))))))))))

(defun deterred-ai--claude-parse-jsonl (jsonl-path)
  "Parse a Claude Code JSONL session file at JSONL-PATH.

Returns a list of alists, one per unique assistant message, with
token usage and file modification data."
  (let ((messages (make-hash-table :test #'equal))
        (uuid-to-msg-id (make-hash-table :test #'equal))
        (user-file-data (make-hash-table :test #'equal)))
    ;; Pass 1: parse all records
    (with-temp-buffer
      (insert-file-contents jsonl-path)
      (goto-char (point-min))
      (while (not (eobp))
        (when-let* ((line (buffer-substring-no-properties
                           (line-beginning-position) (line-end-position)))
                    (record (condition-case nil
                                (json-read-from-string line)
                              (error nil)))
                    (type (alist-get 'type record)))
          (cond
           ((equal type "assistant")
            (deterred-ai--claude-process-assistant-record
             record messages uuid-to-msg-id))
           ((equal type "user")
            (when-let* ((tool-result (alist-get 'toolUseResult record))
                        (_ (consp tool-result))
                        (source-uuid (alist-get 'sourceToolAssistantUUID record))
                        (file-path (deterred-ai--json-value 'filePath tool-result)))
              (let ((patches (deterred-ai--json-value 'structuredPatch tool-result))
                    (lines-added 0)
                    (lines-removed 0))
                (when (vectorp patches)
                  (cl-loop
                   for patch across patches
                   do (when-let* ((lines (alist-get 'lines patch))
                                  (_ (vectorp lines)))
                        (cl-loop
                         for ln across lines
                         do (cond
                             ((string-prefix-p "+" ln)
                              (cl-incf lines-added))
                             ((string-prefix-p "-" ln)
                              (cl-incf lines-removed)))))))
                (puthash source-uuid
                         (list (cons 'file-path file-path)
                               (cons 'lines-added lines-added)
                               (cons 'lines-removed lines-removed))
                         user-file-data))))))
        (forward-line 1)))
    ;; Pass 2: merge user file data into messages
    (maphash
     (lambda (uuid file-data)
       (when-let* ((msg-id (gethash uuid uuid-to-msg-id))
                   (entry (gethash msg-id messages)))
         (let ((files (alist-get 'files entry))
               (fp (alist-get 'file-path file-data)))
           (let ((file-entry (cl-find fp files
                                      :key (lambda (f) (alist-get 'file-path f))
                                      :test #'equal)))
             (if file-entry
                 (progn
                   (setf (alist-get 'lines-added file-entry)
                         (alist-get 'lines-added file-data))
                   (setf (alist-get 'lines-removed file-entry)
                         (alist-get 'lines-removed file-data)))
               (setf (alist-get 'files entry)
                     (append files (list file-data))))))))
     user-file-data)
    ;; Collect results, filtering out entries without message-id
    (let (result)
      (maphash
       (lambda (_k v)
         (when (alist-get 'message-id v)
           (push v result)))
       messages)
      (seq-sort-by (lambda (e) (alist-get 'timestamp e)) #'< result))))

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

(defun deterred-ai--claude-parse-all ()
  "Parse all Claude Code JSONL session files.

Scans `deterred-ai-claude-data-dir'/projects/ for all JSONL files
including subagent sessions.

Returns a list of alists sorted by timestamp."
  (let* ((projects-dir (expand-file-name "projects" deterred-ai-claude-data-dir))
         (files (deterred-ai--claude-collect-jsonl-files projects-dir))
         result)
    (dolist (file-info files)
      (let ((jsonl-path (alist-get 'jsonl-path file-info))
            (project-dir (alist-get 'project-dir file-info)))
        (condition-case err
            (dolist (entry (deterred-ai--claude-parse-jsonl jsonl-path))
              (push (cons (cons 'project-dir project-dir) entry) result))
          (error
           (message "deterred-ai: error parsing %s: %s" jsonl-path err)))))
    (seq-sort-by (lambda (e) (alist-get 'timestamp e)) #'< result)))

(defun deterred-ai--codex-non-empty-string (value)
  "Return VALUE when it is a non-empty string, otherwise nil."
  (when (stringp value)
    (let ((trimmed (string-trim value)))
      (unless (string-empty-p trimmed)
        trimmed))))

(defun deterred-ai--codex-normalize-usage (usage)
  "Normalize raw Codex token USAGE alist."
  (when (consp usage)
    (let* ((input (or (deterred-ai--json-value 'input_tokens usage) 0))
           (cached (or (deterred-ai--json-value 'cached_input_tokens usage)
                       (deterred-ai--json-value 'cache_read_input_tokens usage)
                       0))
           (output (or (deterred-ai--json-value 'output_tokens usage) 0))
           (reasoning (or (deterred-ai--json-value 'reasoning_output_tokens usage) 0))
           (total (or (deterred-ai--json-value 'total_tokens usage)
                      (+ input output))))
      (list
       (cons 'input_tokens input)
       (cons 'cached_input_tokens cached)
       (cons 'output_tokens output)
       (cons 'reasoning_output_tokens reasoning)
       (cons 'total_tokens total)))))

(defun deterred-ai--codex-subtract-usage (current previous)
  "Build usage delta from CURRENT cumulative usage and PREVIOUS usage."
  (list
   (cons 'input_tokens
         (max (- (or (alist-get 'input_tokens current) 0)
                 (or (alist-get 'input_tokens previous) 0))
              0))
   (cons 'cached_input_tokens
         (max (- (or (alist-get 'cached_input_tokens current) 0)
                 (or (alist-get 'cached_input_tokens previous) 0))
              0))
   (cons 'output_tokens
         (max (- (or (alist-get 'output_tokens current) 0)
                 (or (alist-get 'output_tokens previous) 0))
              0))
   (cons 'reasoning_output_tokens
         (max (- (or (alist-get 'reasoning_output_tokens current) 0)
                 (or (alist-get 'reasoning_output_tokens previous) 0))
              0))
   (cons 'total_tokens
         (max (- (or (alist-get 'total_tokens current) 0)
                 (or (alist-get 'total_tokens previous) 0))
              0))))

(defun deterred-ai--codex-extract-model (payload)
  "Extract model name from Codex PAYLOAD alist."
  (when (consp payload)
    (or (deterred-ai--codex-non-empty-string
         (deterred-ai--json-value 'model payload))
        (when-let* ((info (alist-get 'info payload))
                    (_ (consp info)))
          (or (deterred-ai--codex-non-empty-string
               (deterred-ai--json-value 'model info))
              (deterred-ai--codex-non-empty-string
               (deterred-ai--json-value 'model_name info))
              (when-let* ((metadata (alist-get 'metadata info))
                          (_ (consp metadata)))
                (deterred-ai--codex-non-empty-string
                 (deterred-ai--json-value 'model metadata)))))
        (when-let* ((metadata (alist-get 'metadata payload))
                    (_ (consp metadata)))
          (deterred-ai--codex-non-empty-string
           (deterred-ai--json-value 'model metadata))))))

(defun deterred-ai--codex-build-message-id (session-id timestamp line-no)
  "Build deterministic Codex message ID from SESSION-ID, TIMESTAMP and LINE-NO."
  (format "codex:%s:%s:%d" session-id timestamp line-no))

(defun deterred-ai--codex-parse-jsonl (jsonl-path sessions-dir)
  "Parse a Codex JSONL file at JSONL-PATH under SESSIONS-DIR."
  (let* ((relative-path (file-relative-name jsonl-path sessions-dir))
         (session-id (string-remove-suffix ".jsonl" relative-path))
         (result nil)
         (current-model nil)
         (current-cwd nil)
         (session-version nil)
         (previous-totals nil)
         (line-no 0))
    (with-temp-buffer
      (insert-file-contents jsonl-path)
      (goto-char (point-min))
      (while (not (eobp))
        (cl-incf line-no)
        (when-let* ((line (buffer-substring-no-properties
                           (line-beginning-position) (line-end-position)))
                    (record (condition-case nil
                                (json-read-from-string line)
                              (error nil)))
                    (type (alist-get 'type record)))
          (cond
           ((equal type "session_meta")
            (when-let* ((payload (alist-get 'payload record))
                        (_ (consp payload)))
              (when-let* ((cwd (deterred-ai--codex-non-empty-string
                                (deterred-ai--json-value 'cwd payload))))
                (setq current-cwd cwd))
              (when-let* ((cli-version (deterred-ai--codex-non-empty-string
                                        (deterred-ai--json-value 'cli_version payload))))
                (setq session-version cli-version))
              (when-let* ((model (deterred-ai--codex-extract-model payload)))
                (setq current-model model))))
           ((equal type "turn_context")
            (when-let* ((payload (alist-get 'payload record))
                        (_ (consp payload)))
              (when-let* ((cwd (deterred-ai--codex-non-empty-string
                                (deterred-ai--json-value 'cwd payload))))
                (setq current-cwd cwd))
              (when-let* ((model (deterred-ai--codex-extract-model payload)))
                (setq current-model model))))
           ((equal type "event_msg")
            (when-let* ((payload (alist-get 'payload record))
                        (_ (consp payload))
                        (payload-type (deterred-ai--json-value 'type payload))
                        (_ (equal payload-type "token_count"))
                        (timestamp-str (deterred-ai--json-value 'timestamp record))
                        (_ (stringp timestamp-str))
                        (timestamp (condition-case nil
                                       (truncate
                                        (float-time
                                         (encode-time
                                          (iso8601-parse timestamp-str))))
                                     (error nil))))
              (let* ((info (alist-get 'info payload))
                     (last-usage
                      (deterred-ai--codex-normalize-usage
                       (and (consp info)
                            (alist-get 'last_token_usage info))))
                     (total-usage
                      (deterred-ai--codex-normalize-usage
                       (and (consp info)
                            (alist-get 'total_token_usage info))))
                     (raw-usage
                      (or last-usage
                          (and total-usage
                               (deterred-ai--codex-subtract-usage
                                total-usage previous-totals)))))
                (when total-usage
                  (setq previous-totals total-usage))
                (when raw-usage
                  (let* ((raw-input (or (alist-get 'input_tokens raw-usage) 0))
                         ;; Codex reports cached tokens as a subset of input tokens.
                         (cached (min (or (alist-get 'cached_input_tokens raw-usage) 0)
                                      raw-input))
                         (input (max (- raw-input cached) 0))
                         (output (or (alist-get 'output_tokens raw-usage) 0))
                         (reasoning (or (alist-get 'reasoning_output_tokens raw-usage) 0))
                         (total (or (alist-get 'total_tokens raw-usage) 0)))
                    (unless (and (zerop input)
                                 (zerop cached)
                                 (zerop output)
                                 (zerop reasoning)
                                 (zerop total))
                      (when-let* ((model (deterred-ai--codex-extract-model payload)))
                        (setq current-model model))
                      (let ((model-name (or current-model "unknown-codex")))
                        (push
                         (list
                          (cons 'timestamp timestamp)
                          (cons 'session-id session-id)
                          (cons 'model-name model-name)
                          (cons 'message-id
                                (deterred-ai--codex-build-message-id
                                 session-id timestamp-str line-no))
                          (cons 'request-id nil)
                          (cons 'cwd current-cwd)
                          (cons 'version session-version)
                          (cons 'input-tokens input)
                          (cons 'output-tokens output)
                          (cons 'cache-creation-input-tokens 0)
                          (cons 'cache-read-input-tokens cached)
                          (cons 'total-tokens total)
                          (cons 'files nil))
                         result))))))))))
        (forward-line 1)))
    (nreverse result)))

(defun deterred-ai--codex-collect-jsonl-files (sessions-dir)
  "Collect all Codex JSONL files in SESSIONS-DIR."
  (if (file-directory-p sessions-dir)
      (directory-files-recursively sessions-dir "\\.jsonl\\'")
    nil))

(defun deterred-ai--codex-parse-all ()
  "Parse all Codex JSONL session files."
  (let* ((sessions-dir (expand-file-name "sessions" deterred-ai-codex-data-dir))
         (files (deterred-ai--codex-collect-jsonl-files sessions-dir))
         result)
    (dolist (jsonl-path files)
      (condition-case err
          (setq result
                (append result
                        (deterred-ai--codex-parse-jsonl jsonl-path sessions-dir)))
        (error
         (message "deterred-ai: error parsing %s: %s" jsonl-path err))))
    (seq-sort-by (lambda (e) (alist-get 'timestamp e)) #'< result)))

(defun deterred-ai--parse-all ()
  "Parse all configured AI usage JSONL files."
  (seq-sort-by
   (lambda (e) (alist-get 'timestamp e))
   #'<
   (append (deterred-ai--claude-parse-all)
           (deterred-ai--codex-parse-all))))

(defun deterred-ai--tiered-cost (tokens base-price tiered-price)
  "Calculate cost for TOKENS with BASE-PRICE and optional TIERED-PRICE.

If TOKENS exceeds `deterred-ai--tiered-threshold' and TIERED-PRICE is
non-nil, split the cost at the threshold."
  (cond
   ((or (null tokens) (zerop tokens)) 0)
   ((and tiered-price (> tokens deterred-ai--tiered-threshold))
    (+ (* deterred-ai--tiered-threshold base-price)
       (* (- tokens deterred-ai--tiered-threshold) tiered-price)))
   (t (* tokens base-price))))

(defun deterred-ai--calculate-cost (entry)
  "Compute USD cost for a parsed ENTRY.

Uses LiteLLM pricing data.  `deterred-ai--ensure-pricing' must be
called before this.  Returns cost as a float in dollars."
  (let* ((model-name (alist-get 'model-name entry))
         (mapped-name (or (cdr (assoc model-name deterred-ai-model-name-map))
                          model-name))
         (pricing (condition-case nil
                      (deterred-ai--get-pricing-datum mapped-name)
                    (error nil))))
    (if (null pricing)
        0.0
      (let ((input-price (or (deterred-ai--json-value 'input_cost_per_token pricing) 0))
            (input-tiered (deterred-ai--json-value
                           'input_cost_per_token_above_200k_tokens pricing))
            (output-price (or (deterred-ai--json-value 'output_cost_per_token pricing) 0))
            (output-tiered (deterred-ai--json-value
                            'output_cost_per_token_above_200k_tokens pricing))
            (cache-create-price (or (deterred-ai--json-value
                                     'cache_creation_input_token_cost pricing) 0))
            (cache-create-tiered (deterred-ai--json-value
                                  'cache_creation_input_token_cost_above_200k_tokens pricing))
            (cache-read-price (or (deterred-ai--json-value
                                   'cache_read_input_token_cost pricing) 0))
            (cache-read-tiered (deterred-ai--json-value
                                'cache_read_input_token_cost_above_200k_tokens pricing)))
        (float
         (+ (deterred-ai--tiered-cost
             (alist-get 'input-tokens entry) input-price input-tiered)
            (deterred-ai--tiered-cost
             (alist-get 'output-tokens entry) output-price output-tiered)
            (deterred-ai--tiered-cost
             (alist-get 'cache-creation-input-tokens entry)
             cache-create-price cache-create-tiered)
            (deterred-ai--tiered-cost
             (alist-get 'cache-read-input-tokens entry)
             cache-read-price cache-read-tiered)))))))

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

(defun deterred-ai--postprocess (entries)
  "Post-process parsed ENTRIES with cost, project ID, and hostname.

Adds `usd-cost', `hostname', and `project-id' to each entry and its
files.  `deterred-ai--ensure-pricing' must be called before this."
  (let ((hostname (system-name)))
    (dolist (entry entries)
      (nconc entry (list (cons 'usd-cost (deterred-ai--calculate-cost entry))
                         (cons 'hostname hostname)))))
  (deterred-ai--match-project-ids entries)
  entries)

(defun deterred-ai--format-tokens (n)
  "Format token count N for human display."
  (cond
   ((>= n 1000000) (format "%.1fM" (/ n 1000000.0)))
   ((>= n 1000) (format "%.1fk" (/ n 1000.0)))
   (t (number-to-string n))))

(cl-defun deterred-ai--store (entries &key (conflict-action 'do-nothing))
  "Store parsed and postprocessed ENTRIES into the database.

Converts entry alists to DB-compatible format and inserts them using
CONFLICT-ACTION.  Never deletes existing data."
  (let ((db (deterred-db--init))
        item-values
        file-values)
    (dolist (entry entries)
      (push `((message_id  . ,(alist-get 'message-id entry))
              (timestamp   . ,(alist-get 'timestamp entry))
              (session_id  . ,(alist-get 'session-id entry))
              (model_name  . ,(alist-get 'model-name entry))
              (total_tokens . ,(alist-get 'total-tokens entry))
              (usd_cost    . ,(round (* (alist-get 'usd-cost entry) 1000000)))
              (is_stats    . 0)
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
               . ,(alist-get 'cache-read-input-tokens entry)))
            item-values)
      (dolist (file (alist-get 'files entry))
        (push `((message_id  . ,(alist-get 'message-id entry))
                (project_id  . ,(alist-get 'project-id file))
                (file_path   . ,(alist-get 'file-path file))
                (lines_added . ,(alist-get 'lines-added file))
                (lines_removed . ,(alist-get 'lines-removed file)))
              file-values)))
    (when item-values
      (with-sqlite-transaction db
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
    (message "deterred-ai: stored %d items, %d files"
             (length item-values) (length file-values))))

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
  (deterred-ai--ensure-pricing
   (lambda ()
     (let ((entries (deterred-ai--postprocess (deterred-ai--parse-all))))
       (deterred-ai--store entries)
       (deterred-ai-backfill-project-ids callback)))))

(defun deterred-ai-load-claude (&optional callback)
  "Load AI usage data from Claude Code JSONL files into DETERRED.

If CALLBACK is non-nil, call it when done."
  (interactive)
  (deterred-ai--ensure-pricing
   (lambda ()
     (let ((entries (deterred-ai--postprocess (deterred-ai--claude-parse-all))))
       (deterred-ai--store entries)
       (deterred-ai-backfill-project-ids callback)))))

(defun deterred-ai-load-codex (&optional callback)
  "Load AI usage data from Codex JSONL files into DETERRED.

If CALLBACK is non-nil, call it when done."
  (interactive)
  (deterred-ai--ensure-pricing
   (lambda ()
     (let ((entries (deterred-ai--postprocess (deterred-ai--codex-parse-all))))
       (deterred-ai--store entries)
       (deterred-ai-backfill-project-ids callback)))))

(defconst deterred-ai--pricing-recalc-batch-size 500
  "Batch size for recalculating stored AI pricing.")

(defun deterred-ai--recalculate-stored-pricing ()
  "Recalculate stored USD pricing for all AI usage rows."
  (let* ((db (deterred-db--init))
         (rows (deterred-db-select-alist
                db "SELECT message_id,
                           model_name,
                           input_tokens,
                           output_tokens,
                           cache_creation_input_tokens,
                           cache_read_input_tokens
                    FROM ai_usage_item"))
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
                    (cons 'input-tokens (alist-get 'input_tokens row))
                    (cons 'output-tokens (alist-get 'output_tokens row))
                    (cons 'cache-creation-input-tokens
                          (alist-get 'cache_creation_input_tokens row))
                    (cons 'cache-read-input-tokens
                          (alist-get 'cache_read_input_tokens row)))))
              (deterred-db-execute-trace
               db
               "UPDATE ai_usage_item
                SET usd_cost = ?
                WHERE message_id = ?"
               (list (round (* (deterred-ai--calculate-cost entry) 1000000))
                     (alist-get 'message_id row)))))))
      (setq offset (+ offset batch-size)))
    (deterred-db-mark-updated db 'ai_usage_item)
    (message "deterred-ai: recalculated pricing for %d items" total)))

(defun deterred-ai-recalculate-pricing (&optional callback)
  "Recalculate AI usage pricing and repair stored Codex token buckets.

This reparses real Claude Code and Codex JSONL data with the current
logic, updates matching stored rows, and then recalculates USD cost for
all stored AI usage entries."
  (interactive)
  (deterred-ai--ensure-pricing
   (lambda ()
     (let ((entries (deterred-ai--postprocess (deterred-ai--parse-all))))
       (deterred-ai--store entries :conflict-action 'do-update)
       (deterred-ai--recalculate-stored-pricing)
       (when callback (funcall callback))))))

(defun deterred-ai--claude-parse-stats (stats-path)
  "Parse a Claude Code stats-cache JSON file at STATS-PATH.

Returns a list of fake entry alists, one per estimated message.
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
                   (tokens-per-msg (/ model-tokens n-msgs))
                   (ratios (or (gethash model model-ratios)
                               '((input . 0.25) (output . 0.25)
                                 (cache-read . 0.25) (cache-create . 0.25)))))
              (dotimes (i n-msgs)
                (push
                 (list
                  (cons 'message-id
                        (format "fake_%s_%s_%d" date-str model (1+ i)))
                  (cons 'timestamp timestamp)
                  (cons 'session-id "fake_stats")
                  (cons 'model-name model)
                  (cons 'total-tokens tokens-per-msg)
                  (cons 'input-tokens
                        (round (* tokens-per-msg
                                  (alist-get 'input ratios))))
                  (cons 'output-tokens
                        (round (* tokens-per-msg
                                  (alist-get 'output ratios))))
                  (cons 'cache-creation-input-tokens
                        (round (* tokens-per-msg
                                  (alist-get 'cache-create ratios))))
                  (cons 'cache-read-input-tokens
                        (round (* tokens-per-msg
                                  (alist-get 'cache-read ratios))))
                  (cons 'hostname hostname)
                  (cons 'cwd nil)
                  (cons 'project-id nil)
                  (cons 'request-id nil)
                  (cons 'version nil)
                  (cons 'files nil))
                 result))))))
    (nreverse result)))

(cl-defun deterred-ai--claude-store-stats
    (entries &key (conflict-action 'do-nothing))
  "Store fake stats ENTRIES, skipping dates with real data.

Queries the database for dates that already have is_stats = 0
entries, filters ENTRIES to exclude those dates, calculates cost,
and inserts with is_stats = 1 using CONFLICT-ACTION."
  (let* ((db (deterred-db--init))
         (real-dates
          (mapcar (lambda (row) (alist-get 'day row))
                  (deterred-db-select-alist
                   db "SELECT DISTINCT date(timestamp, 'unixepoch') day
FROM ai_usage_item WHERE is_stats = 0")))
         (real-dates-set (make-hash-table :test 'equal))
         (filtered nil)
         item-values)
    (dolist (d real-dates)
      (puthash d t real-dates-set))
    ;; Filter out entries whose date matches real data
    (dolist (entry entries)
      (let* ((ts (alist-get 'timestamp entry))
             (date-str (format-time-string "%Y-%m-%d" (seconds-to-time ts) t)))
        (unless (gethash date-str real-dates-set)
          (push entry filtered))))
    (setq filtered (nreverse filtered))
    ;; Calculate cost and build DB values
    (dolist (entry filtered)
      (let ((cost (deterred-ai--calculate-cost entry)))
        (push `((message_id  . ,(alist-get 'message-id entry))
                (timestamp   . ,(alist-get 'timestamp entry))
                (session_id  . ,(alist-get 'session-id entry))
                (model_name  . ,(alist-get 'model-name entry))
                (total_tokens . ,(alist-get 'total-tokens entry))
                (usd_cost    . ,(round (* cost 1000000)))
                (is_stats    . 1)
                (hostname    . ,(alist-get 'hostname entry))
                (cwd         . nil)
                (project_id  . nil)
                (request_id  . nil)
                (version     . nil)
                (input_tokens . ,(alist-get 'input-tokens entry))
                (output_tokens . ,(alist-get 'output-tokens entry))
                (cache_creation_input_tokens
                 . ,(alist-get 'cache-creation-input-tokens entry))
                (cache_read_input_tokens
                 . ,(alist-get 'cache-read-input-tokens entry)))
              item-values)))
    (when item-values
      (with-sqlite-transaction db
        (deterred-db-insert-unsafe
         db :table-name 'ai_usage_item
         :values item-values
         :conflict-action conflict-action
         :conflict-attrs '(message_id))
        (deterred-db-mark-updated db 'ai_usage_item)))
    (message "deterred-ai: stats: %d entries parsed, %d skipped (real data), %d stored"
             (length entries)
             (- (length entries) (length filtered))
             (length item-values))))

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
   '(("Load Claude JSONL" deterred-ai-load-claude nil)
     ("Load Codex JSONL" deterred-ai-load-codex nil)
     ("Recalculate pricing" deterred-ai-recalculate-pricing nil)
     ("Backfill project IDs" deterred-ai-backfill-project-ids nil)
     ("Load Claude stats cache JSON" deterred-ai--claude-load-stats nil))
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
                db "SELECT COUNT(*) msg_count,
                           COALESCE(SUM(total_tokens), 0) total_tokens,
                           COALESCE(SUM(usd_cost), 0) total_cost
                    FROM ai_usage_item
                    WHERE timestamp BETWEEN ? AND ?
                      AND is_stats = 0"
                (list start end))))
         (msg-count (alist-get 'msg_count totals))
         (total-cost (alist-get 'total_cost totals))
         (model-data
          (deterred-db-select-alist
           db "SELECT model_name,
                      COUNT(*) msg_count,
                      SUM(total_tokens) total_tokens,
                      SUM(usd_cost) total_cost
               FROM ai_usage_item
               WHERE timestamp BETWEEN ? AND ?
                 AND is_stats = 0
               GROUP BY model_name
               ORDER BY total_cost DESC"
           (list start end))))
    (when (> msg-count 0)
      `((:short-description
         . ,(format "$%.2f, %s messages, %s tokens"
                    (/ total-cost 1000000.0)
                    msg-count
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
                 ", " (f-num (alist-get 'msg_count iter)) " msgs"
                 ", " (deterred-ai--format-tokens
                       (alist-get 'total_tokens iter)) " tokens")
              model-data)))))))

(provide 'deterred-ai)
;;; deterred-ai.el ends here

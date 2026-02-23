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

(defcustom deterred-ai-model-name-map
  '(("kimi-for-coding" . "moonshot/kimi-k2.5"))
  "Alist mapping model names to LiteLLM-compatible names."
  :group 'deterred
  :type '(alist :key-type string :value-type string))

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

(defconst deterred-ai--file-tool-names '("Edit" "Write" "NotebookEdit")
  "Tool names that modify files.")

(defun deterred-ai--process-assistant-record (record messages uuid-to-msg-id)
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
                                   deterred-ai--file-tool-names))
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

(defun deterred-ai--parse-jsonl (jsonl-path)
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
            (deterred-ai--process-assistant-record
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

(defun deterred-ai--collect-jsonl-files (projects-dir)
  "Collect all JSONL files from PROJECTS-DIR.

Returns a list of alists with keys `jsonl-path' and `project-dir'."
  (let (result)
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
                          result))))))))))
    (nreverse result)))

(defun deterred-ai--parse-all ()
  "Parse all Claude Code JSONL session files.

Scans `deterred-ai-claude-data-dir'/projects/ for all JSONL files
including subagent sessions.

Returns a list of alists sorted by timestamp."
  (let* ((projects-dir (expand-file-name "projects" deterred-ai-claude-data-dir))
         (files (deterred-ai--collect-jsonl-files projects-dir))
         result)
    (dolist (file-info files)
      (let ((jsonl-path (alist-get 'jsonl-path file-info))
            (project-dir (alist-get 'project-dir file-info)))
        (condition-case err
            (dolist (entry (deterred-ai--parse-jsonl jsonl-path))
              (push (cons (cons 'project-dir project-dir) entry) result))
          (error
           (message "deterred-ai: error parsing %s: %s" jsonl-path err)))))
    (seq-sort-by (lambda (e) (alist-get 'timestamp e)) #'< result)))


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
    (let* ((parts (file-name-split path))
           (i (seq-length parts)))
      (cl-block search
        (while (> i 0)
          (when-let* ((cand (apply #'file-name-concat "/" (seq-take parts i)))
                      (project-id (gethash cand id-by-path)))
            (cl-return-from search project-id))
          (setq i (1- i)))))))

(defun deterred-ai--match-project-ids (entries)
  "Match wakatime project IDs for ENTRIES.

Queries the database for project roots and matches `cwd' and file
paths against them.  Mutates ENTRIES in place."
  (let* ((db (deterred-db--init))
         (projects (deterred-db-select-alist
                    db "SELECT id, project_root FROM wakatime_projects
WHERE project_root IS NOT NULL AND name != 'Unknown Project'"))
         (id-by-path (make-hash-table :test 'equal)))
    (dolist (item projects)
      (puthash (alist-get 'project_root item)
               (alist-get 'id item) id-by-path))
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

(defun deterred-ai--store (entries)
  "Store parsed and postprocessed ENTRIES into the database.

Converts entry alists to DB-compatible format and inserts with
DO NOTHING on conflict.  Never deletes existing data."
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
         :conflict-action 'do-nothing
         :conflict-attrs '(message_id))
        (when file-values
          (deterred-db-insert-unsafe
           db :table-name 'ai_usage_file
           :values file-values
           :conflict-action 'do-nothing
           :conflict-attrs '(message_id file_path)))
        (deterred-db-mark-updated db 'ai_usage_item)
        (deterred-db-mark-updated db 'ai_usage_file)))
    (message "deterred-ai: stored %d items, %d files"
             (length item-values) (length file-values))))

(defun deterred-ai-load (&optional callback)
  "Load AI usage data from Claude Code JSONL files into DETERRED.

If CALLBACK is non-nil, call it when done."
  (interactive)
  (deterred-ai--ensure-pricing
   (lambda ()
     (let ((entries (deterred-ai--postprocess (deterred-ai--parse-all))))
       (deterred-ai--store entries)
       (when callback (funcall callback))))))

(defun deterred-ai--parse-stats (stats-path)
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

(defun deterred-ai--store-stats (entries)
  "Store fake stats ENTRIES, skipping dates with real data.

Queries the database for dates that already have is_stats = 0
entries, filters ENTRIES to exclude those dates, calculates cost,
and inserts with is_stats = 1."
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
         :conflict-action 'do-nothing
         :conflict-attrs '(message_id))
        (deterred-db-mark-updated db 'ai_usage_item)))
    (message "deterred-ai: stats: %d entries parsed, %d skipped (real data), %d stored"
             (length entries)
             (- (length entries) (length filtered))
             (length item-values))))

(defun deterred-ai-load-stats ()
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
       (let ((entries (deterred-ai--parse-stats stats-path)))
         (deterred-ai--store-stats entries))))))

;;;###autoload
(defclass deterred-ai-usage (deterred-source)
  ((name :initform "AI Usage")
   (warn-days :initform 1))
  "DETERRED source for AI usage data (Claude Code).")

(cl-defmethod deterred-source-actions ((_source deterred-ai-usage) &optional callback)
  "Run an action for the AI usage source.

Run CALLBACK when done."
  (deterred-source--actions-pick
   '(("Load stats cache JSON" deterred-ai-load-stats nil))
   callback))

(cl-defmethod deterred-source-sync ((_source deterred-ai-usage) &optional callback)
  "Sync AI usage data from Claude Code JSONL files.

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

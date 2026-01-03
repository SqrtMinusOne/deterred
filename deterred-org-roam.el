;;; deterred-org-journal-tags.el --- TODO -*- lexical-binding: t -*-

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
(require 'cl-lib)
(require 'org)
(require 'uuidgen)

(require 'deterred-db)
(require 'deterred-format)
(require 'deterred-source)

(defvar org-roam-directory nil)

(defconst deterred-org-roam-uuid-namespace
  "7104dca2-3f42-45fb-903c-4909c0abcd28")

(defun deterred-org-roam--get-file-creation-date (file)
  "Get creation date of FILE from filename or git.

FILE should be an absolute path to an org-roam file.  If the file is
named using the default pattern,
e.g. \"20211031093734-supervised_ml.org\", it will be used, otherwise
the function will try to use git.

Returns a UNIX timestamp."
  (let* ((file (expand-file-name file))
         (basename (file-name-nondirectory file))
         (timestamp-regex (rx bol
                              (group (= 4 digit)) ; year
                              (group (= 2 digit)) ; month
                              (group (= 2 digit)) ; day
                              (group (= 2 digit)) ; hour
                              (group (= 2 digit)) ; minute
                              (group (= 2 digit)) ; second
                              "-")))
    (if (string-match timestamp-regex basename)
        ;; Parse timestamp from filename
        (let ((year (string-to-number (match-string 1 basename)))
              (month (string-to-number (match-string 2 basename)))
              (day (string-to-number (match-string 3 basename)))
              (hour (string-to-number (match-string 4 basename)))
              (minute (string-to-number (match-string 5 basename)))
              (second (string-to-number (match-string 6 basename))))
          (truncate (float-time (encode-time second minute hour day month year))))
      ;; Fallback to git (works even for deleted files)
      (let ((git-timestamp (string-trim
                            (shell-command-to-string
                             (format "git log --all --follow --format=%%at --reverse -- %s | head -1"
                                     (shell-quote-argument file))))))
        (if (string-empty-p git-timestamp)
            (error "Cannot determine creation date for file: %s" file)
          (string-to-number git-timestamp))))))

(defun deterred-org-roam--get-file-content (file)
  "Get content of FILE from file system or git history.

FILE should be an absolute path to an org-roam file.

Returns the file content as a string."
  (let ((file (expand-file-name file)))
    (if (file-exists-p file)
        ;; Read from file system
        (with-temp-buffer
          (insert-file-contents file)
          (buffer-string))
      ;; Read from git history
      (let* ((default-directory (file-name-directory file))
             (repo-root (string-trim
                        (shell-command-to-string
                         "git rev-parse --show-toplevel")))
             (rel-path (file-relative-name file repo-root))
             (commit (string-trim
                     (shell-command-to-string
                      (format "git rev-list -1 --all -- %s"
                              (shell-quote-argument file))))))
        (if (string-empty-p commit)
            (error "File not found in git history: %s" file)
          (shell-command-to-string
           (format "git show %s~1:%s"
                   commit
                   (shell-quote-argument rel-path))))))))

(defun deterred-org-roam--extract-title (content)
  "Extract title and ID from CONTENT using `org-mode' parsing.

CONTENT should be the content of an org-roam file.

Returns an alist with:
- title: the title string, or nil if not found
- id: the ID string, or nil if not found"
  (with-temp-buffer
    (insert content)
    (let (org-mode-hook)
      (org-mode))
    (let* ((title-list (cdr (assoc "TITLE" (org-collect-keywords '("title")))))
           (title (if title-list
                      (org-link-display-format (string-join title-list " "))
                    "Unknown Title"))
           (id (org-entry-get (point-min) "ID")))
      `((title . ,title)
        (id . ,id)))))

(defun deterred-org-roam--extract-tags (content)
  "Extract file tags from CONTENT using `org-mode' parsing.

CONTENT should be the content of an org-roam file.

Returns a list of tag strings, or nil if no tags found."
  (with-temp-buffer
    (insert content)
    (let (org-mode-hook)
      (org-mode))
    (mapcar #'substring-no-properties
            org-file-tags)))

(defun deterred-org-roam--get-file-tag-history (file)
  "Get tag history for FILE from git.

FILE should be an absolute path to an org-roam file.

Returns a list of alists, where each alist contains:
- tag: the tag name
- created: Unix timestamp when the tag first created
- deleted: Unix timestamp when the tag deleted, or nil if still present

The list is sorted by appearance time."
  (let* ((file (expand-file-name file))
         (default-directory (file-name-directory file))
         (repo-root (string-trim
                     (shell-command-to-string
                      "git rev-parse --show-toplevel")))
         (rel-path (file-relative-name file repo-root))
         (commits (split-string
                   (string-trim
                    (shell-command-to-string
                     (format "git log --all --follow --format=%%H%%x09%%at --reverse -- %s"
                             (shell-quote-argument file))))
                   "\n" t))
         (tag-states (make-hash-table :test #'equal))
         (current-tags nil)
         (result nil))
    (unless commits
      (error "No git history found for file: %s" file))
    ;; Process each commit
    (dolist (commit-line commits)
      (let* ((parts (split-string commit-line "\t"))
             (commit-hash (car parts))
             (timestamp (string-to-number (cadr parts)))
             (content (shell-command-to-string
                       (format "git show %s:%s"
                               commit-hash
                               (shell-quote-argument rel-path))))
             (tags (deterred-org-roam--extract-tags content)))
        ;; Check for new tags
        (dolist (tag tags)
          (unless (gethash tag tag-states)
            (puthash tag `((created . ,timestamp)
                           (deleted . ,nil))
                     tag-states)))
        ;; Check for removed tags
        (dolist (old-tag current-tags)
          (unless (member old-tag tags)
            (let ((state (gethash old-tag tag-states)))
              (unless (alist-get 'deleted state)
                (setf (alist-get 'deleted state) timestamp)))))
        (setq current-tags tags)))
    ;; Convert hash table to list
    (maphash (lambda (tag state)
               (push `((tag . ,tag)
                       (created . ,(alist-get 'created state))
                       (deleted . ,(alist-get 'deleted state)))
                     result))
             tag-states)
    ;; Sort by appearance time
    (sort result (lambda (a b)
                   (< (alist-get 'created a)
                      (alist-get 'created b))))))

(defun deterred-org-roam--get-all-files ()
  "Get all files that ever existed in `org-roam-directory'.

Returns a list of alists, where each alist contains:
- file: absolute path to the file
- created: Unix timestamp when the file was created
- modified: list of Unix timestamps for all modifications (including creation)
- deleted: Unix timestamp when the file was deleted, or nil if still exists

The list includes both current and deleted files."
  (let* ((default-directory org-roam-directory)
         (repo-root (string-trim
                     (shell-command-to-string
                      "git rev-parse --show-toplevel")))
         ;; Get git log with file operations (add, modify, delete)
         (log-output (shell-command-to-string
                      "git -c core.quotePath=false log --all --reverse --pretty=format:%H%x09%at --name-status --diff-filter=AMD -- \"*.org\""))
         (lines (split-string log-output "\n" t))
         (files (make-hash-table :test #'equal))
         (current-commit nil)
         (current-timestamp nil)
         (result nil))
    ;; Parse git log output
    (dolist (line lines)
      (if (string-match (rx bol
                            (group (= 40 xdigit))
                            "\t"
                            (group (1+ digit)))
                        line)
          ;; Commit line
          (setq current-commit (match-string 1 line)
                current-timestamp (string-to-number (match-string 2 line)))
        ;; File operation line
        (when (string-match (rx bol
                                (group (any "AMD"))
                                (1+ space)
                                (group (1+ nonl)))
                            line)
          (let* ((operation (match-string 1 line))
                 (rel-file (match-string 2 line))
                 (abs-file (expand-file-name rel-file repo-root))
                 (file-data (gethash abs-file files)))
            (cond
             ((string-match-p (rx bos ".#")
                              (file-relative-name abs-file org-roam-directory))
              t)
             ((string= operation "A")
              ;; File added
              (unless file-data
                (puthash abs-file `((created . ,current-timestamp)
                                    (modified . (,current-timestamp))
                                    (deleted . ,nil))
                         files)))
             ((string= operation "M")
              ;; File modified
              (when file-data
                (push current-timestamp (alist-get 'modified file-data))))
             ((string= operation "D")
              ;; File deleted
              (when file-data
                (setf (alist-get 'deleted file-data) current-timestamp))))))))
    ;; Convert hash table to list and sort modification times
    (maphash (lambda (abs-file file-data)
               (let ((modified-times (alist-get 'modified file-data)))
                 (push `((file . ,abs-file)
                         (created . ,(alist-get 'created file-data))
                         (modified . ,(sort (delete-dups modified-times) #'>))
                         (deleted . ,(alist-get 'deleted file-data)))
                       result)))
             files)
    result))

(defun deterred-org-roam-sync--get-data (db)
  "Get data to sync `org-roam' with DETERRED.

DB is a sqlite database connection instance.

The function calls git O(N) times, where N is the number of new or
created `org-roam' nodes.  The first call will take a lot of time.

The return value is a list of 4 elements.  The first 3 are lists of
alists:
- node-data
  - id - either org-roam id or a UUIDv3 based on filename, in the rare
    cases org-roam hasn't set an ID.
  - filename - filename relative to `org-roam-directory'.
  - title
  - timestamp - UNIX timestamp with creation date
  - timestamp_deleted - UNIX timestamp with deletion date
- tag-data
  - node_id
  - tag
  - timestamp
  - timestamp_deleted
- modification-data - timestamps at which files were modified.
  - node_id
  - timestamp

The last element is a list of processed filenames."
  (let* ((source
          (deterred-db-select-alist
           db "SELECT orn.filename, MAX(ornm.timestamp) last_modified
               FROM org_roam_node orn
               INNER JOIN org_roam_node_modification ornm ON ornm.node_id = orn.id
               GROUP BY orn.filename"))
         (last-modified-source-by-filename (make-hash-table :test #'equal))
         (target (deterred-org-roam--get-all-files))
         (i 0) (total (seq-length target))
         node-data tag-data modification-data keep-filenames)
    (dolist (item source)
      (puthash (alist-get 'filename item) (alist-get 'last_modified item)
               last-modified-source-by-filename))
    (dolist (item target)
      (let* ((file-rel (file-relative-name (alist-get 'file item) org-roam-directory))
             (last-modified-target (car (alist-get 'modified item)))
             (last-modified-source
              (gethash file-rel last-modified-source-by-filename 0)))
        (when (= (% i 20) 0)
          (message "Processing %s/%s org-roam nodes" i total))
        (cl-incf i)
        (push file-rel keep-filenames)
        (when (> last-modified-target last-modified-source)
          (let* ((content (deterred-org-roam--get-file-content
                           (alist-get 'file item)))
                 (node-datum (deterred-org-roam--extract-title content))
                 (tag-history (deterred-org-roam--get-file-tag-history
                               (alist-get 'file item)))
                 (id (or (alist-get 'id node-datum)
                         ;; XXX Somehow some nodes don't have an ID
                         (uuidgen-3 deterred-org-roam-uuid-namespace file-rel))))
            (push `((id . ,id)
                    (filename . ,file-rel)
                    (title . ,(alist-get 'title node-datum))
                    (timestamp . ,(alist-get 'created item))
                    (timestamp_deleted . ,(alist-get 'deleted item)))
                  node-data)
            (dolist (timestamp (alist-get 'modified item))
              (push `((node_id . ,id)
                      (timestamp . ,timestamp))
                    modification-data))
            (dolist (tag-datum tag-history)
              (push `((node_id . ,id)
                      (tag . ,(alist-get 'tag tag-datum))
                      (timestamp . ,(alist-get 'created tag-datum))
                      (timestamp_deleted . ,(alist-get 'deleted tag-datum)))
                    tag-data))))))
    (list node-data tag-data modification-data keep-filenames)))

(defun deterred-org-roam-sync--apply-data (db node-data tag-data modification-data
                                              keep-filenames)
  "Insert `org-roam' data into the DETERRED database.

DB is an instance of sqlite connection.  For NODE-DATA, TAG-DATA,
MODIFICATION-DATA, KEEP-FILENAMES see
`deterred-org-roam-sync--get-data'."
  (let* ((extra-ids
          (mapcar
           (lambda (elem) (alist-get 'id elem))
           (deterred-db-select-alist
            db (format "SELECT id FROM org_roam_NODE where filename NOT IN (%s)"
                       (string-join
                        (mapcar (lambda (filename) (format "\"%s\"" filename))
                                keep-filenames)
                        ", ")))))
         (possibly-renamed-file-ids
          (mapcar
           (lambda (elem) (alist-get 'id elem))
           (deterred-db-select-alist
            db (format "SELECT id FROM org_roam_NODE where filename IN (%s)"
                       (string-join
                        (mapcar (lambda (item)
                                  (format "\"%s\"" (alist-get 'filename item)))
                                node-data)
                        ", ")))))
         (ids-to-delete
          (string-join
           (mapcar
            (lambda (id) (format "\"%s\"" id))
            (delete-dups
             (append extra-ids
                     possibly-renamed-file-ids
                     (mapcar (lambda (elem) (alist-get 'id elem)) node-data))))
           ", ")))
    (when ids-to-delete
      (sqlite-execute
       db (format "DELETE FROM org_roam_node_tag WHERE node_id IN (%s)" ids-to-delete))
      (sqlite-execute
       db (format "DELETE FROM org_roam_node_modification WHERE node_id IN (%s)"
                  ids-to-delete))
      (sqlite-execute db (format "DELETE FROM org_roam_node WHERE id IN (%s)"
                                 ids-to-delete)))
    (deterred-db-insert-unsafe
     db
     :table-name 'org_roam_node
     :values node-data)
    (deterred-db-insert-unsafe
     db
     :table-name 'org_roam_node_modification
     :values modification-data)
    (deterred-db-insert-unsafe
     db
     :table-name 'org_roam_node_tag
     :values tag-data)))

(defun deterred-org-roam-sync ()
  "Sync `org-roam' with DETERRED."
  (interactive)
  (pcase-let* ((db (deterred-db--init))
               (`(,node-data ,tag-data ,modification-data ,keep-filenames)
                (deterred-org-roam-sync--get-data db)))
    (with-sqlite-transaction db
      (deterred-org-roam-sync--apply-data
       db node-data tag-data modification-data keep-filenames))))

;;;###autoload
(defclass deterred-org-roam (deterred-source)
  ((name :initform "Org Roam"))
  "DETERRED source for org-roam.")

(cl-defmethod deterred-source-range ((_source deterred-org-roam) &optional db)
  "Get the data availability range for org-roam.

DB is the sqlite database object.

Return a cons cell, with car as the start timestamp, and cdr as the
end timestamp."
  (let* ((db (or db (deterred-db--init)))
         (data (sqlite-select
                db "SELECT MIN(timestamp), MAX(timestamp)
                    FROM org_roam_node_modification")))
    (cons (caar data) (cadar data))))

(cl-defmethod deterred-source-sync ((_source deterred-org-roam)
                                    &optional callback)
  "Sync DETERRED with org-roam.

Call CALLBACK when done."
  (deterred-org-roam-sync)
  (when callback (funcall callback)))

(defun deterred-org-roam--render-nodes (nodes)
  "Render org-roam NODES as a formatted list."
  (deterred-format
   (f-mapconcat
    (f "- " (f-button
             (f-acc "iter->'title")
             (lambda (&rest _)
               (find-file (expand-file-name
                          (alist-get 'filename iter)
                          org-roam-directory)))))
    nodes)))

(cl-defmethod deterred-source-range-summary
  ((_source deterred-org-roam) start end &optional db)
  "Make org-roam summary for [START, END].

DB is the sqlite database object."
  (let* ((db (or db (deterred-db--init)))
         (added-nodes
          (deterred-db-select-alist
           db "SELECT * FROM org_roam_node
               WHERE timestamp BETWEEN ? AND ?
                 AND (timestamp_deleted IS NULL OR timestamp_deleted > ?)
               ORDER BY timestamp DESC"
           (list start end end)))
         (modified-nodes
          (deterred-db-select-alist
           db "SELECT DISTINCT orn.* FROM org_roam_node orn
               INNER JOIN org_roam_node_modification ornm ON ornm.node_id = orn.id
               WHERE ornm.timestamp BETWEEN ? AND ?
                 AND orn.timestamp < ?
                 AND (orn.timestamp_deleted IS NULL OR orn.timestamp_deleted > ?)
               ORDER BY orn.timestamp DESC"
           (list start end start end)))
         (deleted-nodes
          (deterred-db-select-alist
           db "SELECT * FROM org_roam_node
               WHERE timestamp_deleted BETWEEN ? AND ?
               ORDER BY timestamp_deleted DESC"
           (list start end)))
         (added-count (length added-nodes))
         (modified-count (length modified-nodes))
         (deleted-count (length deleted-nodes))
         (total-count (+ added-count modified-count deleted-count)))
    (when (> total-count 0)
      `((:short-description
         . ,(string-join
             (delq nil
                   (list
                    (when (> added-count 0)
                      (format "%d added" added-count))
                    (when (> modified-count 0)
                      (format "%d modified" modified-count))
                    (when (> deleted-count 0)
                      (format "%d deleted" deleted-count))))
             ", "))
        (:long-description-fn
         . ,(lambda (&rest _)
              (when (> added-count 0)
                (magit-insert-section (deterred-org-roam-added t t)
                  (insert
                   (propertize
                    (format "Added nodes (%d)" added-count)
                    'face 'deterred-faces-section-heading-3))
                  (magit-insert-heading)
                  (insert (deterred-org-roam--render-nodes added-nodes))
                  (insert "\n")))
              (when (> modified-count 0)
                (magit-insert-section (deterred-org-roam-modified t t)
                  (insert
                   (propertize
                    (format "Modified nodes (%d)" modified-count)
                    'face 'deterred-faces-section-heading-3))
                  (magit-insert-heading)
                  (insert (deterred-org-roam--render-nodes modified-nodes))
                  (insert "\n")))
              (when (> deleted-count 0)
                (magit-insert-section (deterred-org-roam-deleted t t)
                  (insert
                   (propertize
                    (format "Deleted nodes (%d)" deleted-count)
                    'face 'deterred-faces-section-heading-3))
                  (magit-insert-heading)
                  (insert (deterred-org-roam--render-nodes deleted-nodes))
                  (insert "\n")))))))))

(provide 'deterred-org-roam)
;;; deterred-org-roam.el ends here

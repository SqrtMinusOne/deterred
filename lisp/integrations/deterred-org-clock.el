;;; deterred-org-clock.el --- Org Clock integration for DETERRED -*- lexical-binding: t -*-

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

;; Org Clock integration for DETERRED.
;;
;; This imports clocked Org headlines from `deterred-org-clock-files'
;; into `org_headline' and `org_clock_item'.

;;; Code:
(require 'cl-lib)
(require 'rx)
(require 'org)
(require 'org-duration)
(require 'org-clock-agg)
(require 'org-ql)
(require 'deterred-db)
(require 'deterred-format)
(require 'deterred-source)
(require 'deterred-utils)

(defconst deterred-org-clock-uuid-namespace
  "b2d5dde1-0779-4bae-96c3-081c54bf0a56")

(defcustom deterred-org-clock-files #'org-agenda-files
  "A list of org files to process or a function that returns it."
  :group 'deterred-sources
  :type '(choice (repeat file) function))

(defcustom deterred-org-clock-store-full-file-path nil
  "Whether to store file paths relative to the common Org root.

When nil, only the file basename is stored.  When non-nil, paths are
stored relative to the top-most directory in
`deterred-org-clock-files'."
  :group 'deterred-sources
  :type 'boolean)

(defconst deterred-org-clock-max-rendered-headlines 100
  "Maximum number of individual Org Clock entries to render.")

(defconst deterred-org-clock-summary-width-padding 6
  "Extra width to reserve when truncating Org Clock summary lines.")

(defun deterred-org-clock--parse-get-parent-headings (elem)
  "Get all headings up to ELEM.

ELEM is an instance of org mode headline."
  (cons (org-element-property :raw-value elem)
        (when-let ((parent (org-element-property :parent elem)))
          (deterred-org-clock--parse-get-parent-headings parent))))

(defun deterred-org-clock--parse-get-path (elem)
  "Get the \"/\"-separated path to the org heading ELEM from the top."
  (string-join
   (mapcar
    (lambda (heading)
      (string-replace "/" "|" (or heading "")))
    (nreverse
     (deterred-org-clock--parse-get-parent-headings elem)))
   "/"))

(defun deterred-org-clock--parse-timestamp (elem property)
  "Parse org timestamp stored in PROPERTY in ELEM.

ELEM is an `org-mode' heading element.

Return a UNIX timestamp or nil."
  (when-let ((timestamp (org-element-property property elem)))
    (time-convert (org-timestamp-to-time timestamp) 'integer)))

(defconst deterred-org-clock--entered-on-regexp
  (rx bol "/Entered on/" (* (or space ":"))
      (group (: (or "<" "[") (* (or alnum space "-" ":")) (or ">" "]"))))
  "A regex to extract the \"Entered on\" timestamp.")

(defun deterred-org-clock--parse-entered-on (elem)
  "Find the \"Entered on\" timestamp in ELEM.

ELEM is an `org-mode' heading element.  The timestamp has to be
stored, e.g., as follows:

/Entered on/ [2022-10-06 Thu 16:43].

The function has to be run in the same buffer as ELEM.

Return a UNIX timestamp or nil."
  (when (org-element-property :contents-begin elem)
    (save-excursion
      (goto-char (org-element-property :contents-begin elem))
      (save-match-data
        (when (re-search-forward deterred-org-clock--entered-on-regexp
                                 (org-element-property :contents-end elem)
                                 t)
          (time-convert
           (encode-time
            (org-parse-time-string
             (substring-no-properties (match-string 1))))
           'integer))))))

(defun deterred-org-clock--id (elem)
  "Get ID for ELEM."
  (deterred-utils--id
   deterred-org-clock-uuid-namespace
   (list
    (alist-get 'title elem)
    (alist-get 'deadline elem)
    (alist-get 'scheduled elem)
    (alist-get 'created elem)
    (alist-get 'started elem)
    (alist-get 'file_name elem))))

(defun deterred-org-clock--clock-get (clock key)
  "Get KEY from CLOCK, handling both alists and plists."
  (or (alist-get key clock)
      (plist-get clock key)))

(defun deterred-org-clock--parse-headline (elem)
  "Parse a headline for org-clock.

ELEM is the headline element as returned by `org-element-map'.

Return an alist with the following keys (keys match the table
structure):
- title
- deadline - a UNIX timestamp
- closed - a UNIX timestamp
- scheduled - a UNIX timestamp
- headline_path - a \"/\"-separated path from the top to the headline
- clocks - a list of (as returned by `org-clock-agg--parse-clocks':
  - `:start' - a UNIX timestamp
- `:end' - a UNIX timestamp
- `:duration' - number of seconds
- category
- tags."
  (let* ((title (org-element-property :raw-value elem))
         (clocks (when (org-element-property :contents-begin elem)
                   (org-clock-agg--parse-clocks elem))))
    (when (and title clocks)
      (save-excursion
        (goto-char (org-element-property :begin elem))
        (let* ((tags-val (org-ql--tags-at (point)))
               (category (org-get-category))
               (deadline (deterred-org-clock--parse-timestamp elem :deadline))
               (closed (deterred-org-clock--parse-timestamp elem :closed))
               (scheduled (deterred-org-clock--parse-timestamp elem :scheduled))
               (created (deterred-org-clock--parse-entered-on elem))
               (headline_path (deterred-org-clock--parse-get-path elem))
               (tags (string-join
                      (seq-filter
                       #'stringp
                       (append (unless (eq (car tags-val) 'org-ql-nil)
                                 (car tags-val))
                               (unless (eq (cdr tags-val) 'org-ql-nil)
                                 (cdr tags-val))))
                      ":"))
               (started (alist-get :start (car clocks))))
          (deterred-utils-make-alist
           title headline_path tags category deadline scheduled closed
           clocks created started))))))

(defun deterred-org-clock--common-parent-directory (files)
  "Return the top-most common parent directory of FILES.

FILES must be a list of absolute file paths.  Return \"/\" if there is
no deeper common directory."
  (let* ((dirs (mapcar #'file-name-directory files))
         (parts-list
          (mapcar (lambda (dir)
                    (split-string (directory-file-name dir) "/" t))
                  dirs))
         common-parts)
    (while (let ((part (nth (length common-parts) (car parts-list))))
             (and part
                  (cl-every
                   (lambda (parts)
                     (equal (nth (length common-parts) parts) part))
                   (cdr parts-list))))
      (push (nth (length common-parts) (car parts-list)) common-parts))
    (if common-parts
        (concat "/" (mapconcat #'identity (nreverse common-parts) "/") "/")
      "/")))

(defun deterred-org-clock--get-files ()
  "Return a list of (FILE-PATH . FILE-NAME) for org-clock integration.

FILE-PATH values are absolute file paths.  FILE-NAME values are plain
file names when `deterred-org-clock-store-full-file-path' is nil,
otherwise file paths relative to the top-most common directory."
  (let* ((raw-files (if (functionp deterred-org-clock-files)
                        (funcall deterred-org-clock-files)
                      deterred-org-clock-files))
         (files (mapcar #'expand-file-name raw-files))
         (common-dir (when deterred-org-clock-store-full-file-path
                       (and files
                            (deterred-org-clock--common-parent-directory files)))))
    (mapcar
     (lambda (file)
       (cons file
             (if deterred-org-clock-store-full-file-path
                 (file-relative-name file common-dir)
               (file-name-nondirectory (directory-file-name file)))))
     files)))

(defun deterred-org-clock--postprocess-headlines (headlines)
  "Merge duplicates in HEADLINES."
  (mapcar
   (lambda (group)
     (if (length> (cdr group) 1)
         (let* ((headline (cadr group))
                (clocks (seq-uniq
                         (cl-mapcan
                          (lambda (h) (alist-get 'clocks h))
                          (copy-tree (cdr group)))
                         (lambda (c1 c2)
                           (=
                            (alist-get :start c1)
                            (alist-get :start c2))))))
           (setf (alist-get 'clocks headline) clocks)
           headline)
       (cadr group)))
   (seq-group-by
    (lambda (headline) (alist-get 'id headline))
    headlines)))

(defun deterred-org-clock--parse-headlines ()
  "Parse headlines for org-clock-agg integration.

Return a cons cell, whose car is as returned by
`org-clock-agg--parse-headline' but with the following additional
fields:
- file_path
- file_name
- id
and without clocks; and whose cdr is a list of alists with the keys:
- headline_id
- start_timestamp - a UNIX timestamp
- end_timestamp - a UNIX timestamp.

This is suitable for insertion into the database."
  (let ((files (deterred-org-clock--get-files))
        raw-headlines headlines clocks
        (clocks-hash (make-hash-table :test #'equal)))
    (dolist (f files)
      (pcase-let ((`(,file-path . ,file-name) f))
        (message "Parsing %s" file-name)
        (with-temp-buffer
          (insert-file-contents file-path)
          (let (org-mode-hook)
            (org-mode))
          (org-element-map (org-element-parse-buffer) 'headline
            (lambda (elem)
              (let ((parsed (deterred-org-clock--parse-headline elem)))
                (when parsed
                  (setf (alist-get 'file_path parsed) file-path
                        (alist-get 'file_name parsed) file-name
                        (alist-get 'id parsed) (deterred-org-clock--id parsed))
                  (push parsed raw-headlines))))))))
    (dolist (headline (deterred-org-clock--postprocess-headlines raw-headlines))
      (let ((id (alist-get 'id headline)))
        (dolist (clock (alist-get 'clocks headline))
          (let* ((start (deterred-org-clock--clock-get clock :start))
                 (end (deterred-org-clock--clock-get clock :end))
                 (clock-key (format "%s:%s:%s" id start end)))
            (unless (gethash clock-key clocks-hash)
              (push `((headline_id . ,id)
                      (start_timestamp . ,start)
                      (end_timestamp . ,end))
                    clocks)
              (puthash clock-key 0 clocks-hash))))
        (push (assoc-delete-all 'clocks headline) headlines)))
    (cons headlines clocks)))

(defun deterred-org-clock--headline-label (headline)
  "Format a label for HEADLINE summary data."
  (let* ((file-name (or (alist-get 'file_name headline) ""))
         (max-width (or (ignore-errors (window-max-chars-per-line))
                        (ignore-errors (window-width))
                        120))
         (title-width (max 20 (- max-width
                                 deterred-org-clock-summary-width-padding
                                 (string-width file-name)
                                 2)))
         (title (truncate-string-to-width
                 (or (alist-get 'title headline) "")
                 title-width nil nil t)))
    (format "%s: %s" file-name title)))

(defun deterred-org-clock--format-duration-hh-mm (minutes)
  "Format MINUTES as HH:mm."
  (let* ((total-minutes (max 0 (round minutes)))
         (hours (/ total-minutes 60))
         (mins (% total-minutes 60)))
    (format "%02d:%02d" hours mins)))

(defun deterred-org-clock--reversed-headline-path (headline)
  "Return reversed parent path for HEADLINE."
  (let* ((tokens (split-string (or (alist-get 'headline_path headline) "") "/" t))
         (parents (reverse (butlast tokens))))
    (string-join parents "/")))

(defun deterred-org-clock--summary-line (headline)
  "Format one summary line for HEADLINE."
  (let* ((duration (deterred-org-clock--format-duration-hh-mm
                    (alist-get 'duration_minutes headline)))
         (file-name (or (alist-get 'file_name headline) ""))
         (title (or (alist-get 'title headline) ""))
         (path (deterred-org-clock--reversed-headline-path headline))
         (prefix (format "%s in %s: " duration file-name))
         (max-width (or (ignore-errors (window-max-chars-per-line))
                        (ignore-errors (window-width))
                        120))
         (tail-width (max 20 (- max-width
                                deterred-org-clock-summary-width-padding
                                (string-width prefix))))
         (tail (string-trim
                (if (string-empty-p path)
                    title
                  (format "%s - %s" title path)))))
    (concat prefix (truncate-string-to-width tail tail-width nil nil t))))

(defun deterred-org-clock--render-headlines (headlines)
  "Render HEADLINES in chronological order."
  (dolist (headline headlines)
    (insert (deterred-org-clock--summary-line headline) "\n")))

(defun deterred-org-clock-load ()
  "Load Org Clock data into DETERRED."
  (interactive)
  (let ((files (deterred-org-clock--get-files)))
    (unless files
      (user-error "No org files set for `deterred-org-clock'"))
    (pcase-let ((`(,headlines . ,clocks) (deterred-org-clock--parse-headlines)))
      (let ((db (deterred-db--init)))
        (with-sqlite-transaction db
          ;; This source is derived from the current Org files, so for
          ;; now, a full refresh is simpler and keeps deletions in
          ;; sync.
          (sqlite-execute db "DELETE FROM org_clock_item")
          (sqlite-execute db "DELETE FROM org_headline")
          (deterred-db-insert-unsafe
           db
           :table-name 'org_headline
           :values headlines)
          (deterred-db-insert-unsafe
           db
           :table-name 'org_clock_item
           :values clocks)
          (deterred-db-mark-updated-batch db '(org_headline org_clock_item)))))))

(defun deterred-org-clock--summary-headlines (db start end)
  "Return grouped Org Clock summary data for [START, END].

DB is the SQLite connection object.  END is treated as inclusive."
  (let ((end-exclusive (1+ end)))
    (deterred-db-select-alist
     db "SELECT oh.id,
                oh.title,
                oh.file_name,
                oh.headline_path,
                oh.tags,
                oh.category,
                SUM(MAX(0, MIN(oci.end_timestamp, ?) - MAX(oci.start_timestamp, ?))) / 60.0 total
         FROM org_clock_item oci
         INNER JOIN org_headline oh ON oh.id = oci.headline_id
         WHERE oci.end_timestamp > ?
           AND oci.start_timestamp < ?
         GROUP BY oh.id, oh.title, oh.file_name, oh.headline_path, oh.tags, oh.category
         ORDER BY total DESC"
     (list end-exclusive start start end-exclusive))))

(defun deterred-org-clock--summary-rendered-headlines (db start end)
  "Return grouped Org Clock headlines for rendering in [START, END].

DB is the SQLite connection object.  END is treated as inclusive."
  (let ((end-exclusive (1+ end)))
    (deterred-db-select-alist
     db "SELECT oh.id,
                oh.title,
                oh.file_name,
                oh.headline_path,
                MIN(MAX(oci.start_timestamp, ?)) earliest_start_timestamp,
                SUM(MIN(oci.end_timestamp, ?) - MAX(oci.start_timestamp, ?)) / 60.0 duration_minutes
         FROM org_clock_item oci
         INNER JOIN org_headline oh ON oh.id = oci.headline_id
         WHERE oci.end_timestamp > ?
           AND oci.start_timestamp < ?
         GROUP BY oh.id, oh.title, oh.file_name, oh.headline_path
         ORDER BY earliest_start_timestamp ASC
         LIMIT ?"
     (list start end-exclusive start start end-exclusive
           deterred-org-clock-max-rendered-headlines))))

;;;###autoload
(defclass deterred-org-clock (deterred-source)
  ((name :initform "Org Clock"))
  "DETERRED source for Org Clock.")

(cl-defmethod deterred-source-range ((_source deterred-org-clock) &optional db)
  "Get the data availability range for Org Clock.

DB is the sqlite database object."
  (let* ((db (or db (deterred-db--init)))
         (data (sqlite-select
                db "SELECT MIN(start_timestamp), MAX(end_timestamp)
                    FROM org_clock_item")))
    (cons (caar data) (cadar data))))

(cl-defmethod deterred-source-sync ((_source deterred-org-clock) &optional callback)
  "Sync DETERRED with Org Clock.

Call CALLBACK when done."
  (deterred-org-clock-load)
  (when callback
    (funcall callback)))

(cl-defmethod deterred-source-range-summary
  ((_source deterred-org-clock) start end &optional db)
  "Make Org Clock summary for [START, END].

DB is the sqlite database object."
  (let* ((db (or db (deterred-db--init)))
         (headline-data (deterred-org-clock--summary-headlines db start end))
         (rendered-headlines
          (deterred-org-clock--summary-rendered-headlines db start end))
         (headline-count (length headline-data))
         (total-minutes (if headline-data
                            (apply #'+ (mapcar (lambda (d) (alist-get 'total d))
                                               headline-data))
                          0))
         (first-headline (car headline-data))
         (first-headline-minutes (alist-get 'total first-headline))
         (first-headline-share (if (> total-minutes 0)
                                   (/ first-headline-minutes total-minutes)
                                 0)))
    (when (> total-minutes 0)
      `((:short-description
         . ,(cond
             ((= headline-count 1)
              (deterred-format
               (org-duration-from-minutes first-headline-minutes)
               " in "
               (deterred-org-clock--headline-label first-headline)))
             ((> first-headline-share 0.5)
              (deterred-format
               (org-duration-from-minutes first-headline-minutes)
               " in "
               (deterred-org-clock--headline-label first-headline)
               "; "
               (org-duration-from-minutes (- total-minutes first-headline-minutes))
               " in "
               (f-num (1- headline-count))
               " other headlines"))
             (t
              (deterred-format
               (org-duration-from-minutes total-minutes)
               " in "
               (f-num headline-count)
               " headlines"))))
        (:long-description-fn
         . ,(lambda (&rest _)
              (deterred-org-clock--render-headlines rendered-headlines)))))))

(provide 'deterred-org-clock)
;;; deterred-org-clock.el ends here

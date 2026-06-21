;;; deterred-grid.el --- Displaying grids for DETERRED -*- lexical-binding: t -*-

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

;; Some functions to work with grids, namely:
;; - `deterred-grid-show' - display a list of alists with `vtable'
;; - `deterred-grid-print-with-org' - display a list of alists like an
;;   org table.

;;; Code:
(require 'org)
(require 'deterred-format)
(require 'vtable)

(declare-function evil-define-key* "evil-core")

(define-derived-mode deterred-grid-mode special-mode "DETERRED Grid"
  :group 'deterred
  (setq-local buffer-read-only t))

(defvar deterred-grid-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") (lambda ()
                                (interactive)
                                (quit-window t)))
    (when (fboundp #'evil-define-key*)
      (evil-define-key* '(normal motion) map
        "q" (lambda ()
              (interactive)
              (quit-window t))))
    map)
  "A keymap for `deterred-grid-mode'.")

(defun deterred-grid-show (data &rest vtable-args)
  "Display a list of alists using `vtable'.

DATA is a list of alists, VTABLE-ARGS is passed to `make-vtable'."
  (let ((buffer (generate-new-buffer "*DETERRED-grid*")))
    (with-current-buffer buffer
      (deterred-grid-mode)
      (let* ((inhibit-read-only t)
             (columns (mapcar #'car (car data)))
             (objects (mapcar (lambda (datum)
                                (mapcar (lambda (column)
                                          (alist-get column datum))
                                        columns))
                              data)))
        (apply #'make-vtable
               :objects objects
               :columns (mapcar (lambda (column)
                                   (let ((name (symbol-name column)))
                                     (list :name name
                                           :min-width (max 1 (string-width name)))))
                                 columns)
               :use-header-line nil
               vtable-args)))
    (switch-to-buffer-other-window buffer)))

(defun deterred-grid--value-to-string (value)
  "Convert VALUE to a string for grid output."
  (cond
   ((null value) "")
   ((numberp value) (number-to-string value))
   ((stringp value) value)
   ((symbolp value) (symbol-name value))
   (t (prin1-to-string value))))

(defun deterred-grid--csv-escape (value)
  "Escape VALUE for a CSV field."
  (let ((string (deterred-grid--value-to-string value)))
    (if (string-match-p (rx (any ",\"\n\r")) string)
        (concat "\"" (string-replace "\"" "\"\"" string) "\"")
      string)))

(defun deterred-grid-to-csv (data)
  "Return DATA as a CSV string.

DATA is a list of alists.  The first row defines the column order."
  (when data
    (let ((columns (mapcar #'car (car data))))
      (concat
       (mapconcat (lambda (column)
                    (deterred-grid--csv-escape (symbol-name column)))
                  columns ",")
       "\n"
       (mapconcat
        (lambda (row)
          (mapconcat (lambda (column)
                       (deterred-grid--csv-escape (alist-get column row)))
                     columns ","))
        data "\n")
       "\n"))))

(defun deterred-grid-save-csv (data file)
  "Save DATA as CSV to FILE."
  (with-temp-file file
    (insert (or (deterred-grid-to-csv data) "")))
  (message "Saved CSV to %s" file))

(defun deterred-grid--org-table-escape (string)
  "Escape STRING for use in `org-mode' tables."
  (string-replace "|" "¦" string))

(cl-defun deterred-grid-print-with-org (data &key grid-button column-names max-rows
                                             max-column-width)
  "Display DATA with an `org-mode' table.

If GRID-BUTTON is non-nil, show a button to view DATA with
`deterred-grid-show'.

If COLUMN-NAMES is passed, use that instead of alist keys for the
column names.

If MAX-ROWS is passed, truncate the table row count.  If
MAX-COLUMN-WIDTH is passed, truncate the column names with
`truncate-string-to-width'."
  (if (null data)
      (deterred-format (f-ace "(no data)" 'deterred-faces-info) "\n")
    (let ((columns (mapcar #'car (car data))))
      (with-temp-buffer
        (let (org-mode-hook)
          (org-mode))
        (insert "| "
                (mapconcat
                 (lambda (col)
                   (let ((name (or (alist-get col column-names)
                                   (symbol-name col))))
                     (when max-column-width
                       (setq name (truncate-string-to-width name max-column-width
                                                            nil nil t)))
                     name))
                 columns " | ")
                " |\n")
        (insert "|--\n")
        (mapc (lambda (datum)
                (insert "| "
                        (mapconcat
                         (lambda (col)
                           (let* ((item (alist-get col datum))
                                  (value (deterred-grid--org-table-escape
                                          (cond
                                           ((numberp item) (number-to-string item))
                                           ((stringp item) item)
                                           (t (prin1-to-string item))))))
                             (when max-column-width
                               (setq value
                                     (truncate-string-to-width value max-column-width
                                                               nil nil t)))
                             value))
                         columns
                         " | ")
                        " |\n"))
              (if max-rows (take max-rows data) data))
        (goto-char (point-min))
        (org-table-align)
        (goto-char (point-max))
        (when grid-button
          (insert (deterred-format
                   (f-button "[View table]"
                             (lambda (&rest _)
                               (deterred-grid-show data))))))
        (string-trim (buffer-string))))))

(provide 'deterred-grid)
;;; deterred-grid.el ends here

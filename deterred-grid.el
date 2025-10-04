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
(require 'vtable)

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
               :columns (mapcar #'symbol-name columns)
               :use-header-line nil
               vtable-args)))
    (switch-to-buffer-other-window buffer)))

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
                      (mapconcat (lambda (col)
                                   (let ((item (alist-get col datum)))
                                     (cond
                                      ((numberp item) (number-to-string item))
                                      ((stringp item) item)
                                      (t (prin1-to-string item)))))
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
      (string-trim (buffer-string)))))

(provide 'deterred-grid)
;;; deterred-grid.el ends here

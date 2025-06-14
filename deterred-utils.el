;;; deterred-utils.el --- TODO -*- lexical-binding: t -*-

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
(require 'pcsv)

(defmacro deterred-utils-assert-var-set (var-name)
  "Signal error is VAR-NAME is nil."
  `(unless ,var-name
     (user-error ,(format "%s not set!" var-name))))

(defun deterred-utils-csv-to-alist (file)
  "Read a CSV FILE into alist with `pcsv'."
  (let* ((data (pcsv-parse-file file))
         (header (mapcar #'intern (car data))))
    (cl-loop for row in (cdr data)
             collect (cl-loop for key in header
                              for value in row
                              collect (cons key value)))))

(defun deterred-utils-read-csv-with-python (file)
  "Read a CSV FILE into alist with python.

This works better than `pcsv' for Reddit dump."
  (json-parse-string
   (shell-command-to-string
    (format "cat %s | python -c 'import csv, json, sys; print(json.dumps([dict(r) for r in csv.DictReader(sys.stdin)]))'"
            (shell-quote-argument file)))
   :object-type 'alist))

(provide 'deterred-utils)
;;; deterred-utils.el ends here

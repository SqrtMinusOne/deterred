;;; deterred-org.el --- TODO -*- lexical-binding: t -*-

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

;; TODO

;;; Code:
(require 'rx)
(require 'org)
(require 'deterred-utils)

(defun deterred-org--parse-get-parent-headings (elem)
  "Get all headings up to ELEM.

ELEM is an instance of org mode headline."
  (cons (org-element-property :raw-value elem)
        (when-let ((parent (org-element-property :parent elem)))
          (deterred-org--parse-get-parent-headings parent))))

(defun deterred-org--parse-get-path (elem)
  "Get the \"/\"-separated path to the org heading ELEM from the top."
  (string-join
   (mapcar
    (lambda (heading)
      (string-replace "/" "|" (or heading "")))
    (nreverse
     (deterred-org--parse-get-parent-headings elem)))
   "/"))

(defun deterred-org--parse-timestamp (elem property)
  "Parse org timestamp stored in PROPERTY in ELEM.

ELEM is an `org-mode' heading element.

Return a UNIX timestamp or nil."
  (when-let ((timestamp (org-element-property property elem)))
    (time-convert (org-timestamp-to-time timestamp) 'integer)))

(defconst deterred-org--entered-on-regexp
  (rx bol "/Entered on/" (* (or space ":"))
      (group (: (or "<" "[") (* (or alnum space "-" ":")) (or ">" "]"))))
  "A regex to extract the \"Entered on\" timestamp.")

(defun deterred-org--parse-entered-on (elem)
  "Find the \"Entered on\" timestamp in ELEM.

ELEM is an `org-mode' heading element.  The timestamp has to be
stored, e.g., as follows:

/Entered on/ [2022-10-06 Thu 16:43].

The function has to be run in the same buffer as ELEM.

Return a UNIX timestamp or nil."
  (save-excursion
    (goto-char (org-element-property :contents-begin elem))
    (save-match-data
      (when (re-search-forward deterred-org--entered-on-regexp
                               (org-element-property :contents-end elem)
                               t)
        (time-convert
         (encode-time
          (org-parse-time-string
           (substring-no-properties (match-string 1))))
         'integer)))))

(defun deterred-org--parse-buffer ()
  "Parse an `org-mode' buffer."
  (let (res)
    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (elem)
        (when-let ((todo-keyword (org-element-property :todo-keyword elem))
                   (title (org-element-property :raw-value elem)))
          (let ((todo-keyword (substring-no-properties todo-keyword))
                (deadline (deterred-org--parse-timestamp elem :deadline))
                (closed (deterred-org--parse-timestamp elem :closed))
                (scheduled (deterred-org--parse-timestamp elem :scheduled))
                (created (deterred-org--parse-entered-on elem))
                (path (deterred-org--parse-get-path elem)))
            (push
             (deterred-utils-make-alist title todo-keyword deadline scheduled path
                                        closed created)
             res)))))
    (nreverse res)))

(provide 'deterred-org)
;;; deterred-org.el ends here

;;; deterred-format.el --- TODO -*- lexical-binding: t -*-

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
(require 'button)
(require 'outline)
(require 'browse-url)

(defconst deterred-format--alist-nest-symbol "->")
(defconst deterred-format--plist-nest-symbol ".")
(defconst deterred-format--elt-symbols (cons "[" "]"))
(defcustom deterred-format-image-open-command #'browse-url-xdg-open
  "A command to open images."
  :type 'function
  :group 'deterred)

(defun deterred-format--parse-accessor-expr (accessor)
  "Parse an ACCESSOR expression."
  (let ((regexp (rx (| (literal deterred-format--alist-nest-symbol)
                       (literal deterred-format--plist-nest-symbol)
                       (:
                        (literal (car deterred-format--elt-symbols)) (group (+ num))
                        (literal (cdr deterred-format--elt-symbols))))))
        (index 0)
        (res (list nil))
        expr)
    (save-match-data
      (while-let ((match-index (string-match regexp accessor index)))
        (unless (eql index match-index)
          (push (substring accessor index match-index) res))
        (let ((match (match-string 0 accessor)))
          (cond ((equal match deterred-format--alist-nest-symbol)
                 (push 'alist-get res))
                ((equal match deterred-format--plist-nest-symbol)
                 (push 'plist-get res))
                ((and (equal (char-to-string
                              (seq-elt match 0))
                             (car deterred-format--elt-symbols))
                      (equal (char-to-string
                              (seq-elt match (1- (seq-length match))))
                             (cdr deterred-format--elt-symbols)))
                 (push 'seq-elt res)
                 (push (string-to-number (match-string 1 accessor)) res))
                (t (error "Accessor parsing error: %s" match))))
        (setq index (match-end 0)))
      (unless (eql index (seq-length accessor))
        (push (substring accessor index) res)))
    (setq res (nreverse res))
    (cl-loop for (key raw-value) on res by #'cddr
             for value = (cond ((numberp raw-value) raw-value)
                               ((listp raw-value)
                                (error "Can't process %s as value" raw-value))
                               (t (read raw-value)))
             if (null key) do (setq expr value)
             if (eq key 'alist-get)
             do (setq expr `(alist-get ,value ,expr
                                       ,@(when (stringp value)
                                           '(nil nil #'equal))))
             if (eq key 'plist-get)
             do (setq expr `(plist-get ,expr ,value
                                       ,@(when (stringp value)
                                           '(#'equal))))
             if (eq key 'seq-elt)
             do (setq expr `(seq-elt ,expr ,value)))
    expr))

(defmacro deterred-format-accessor (accessor)
  "Convert ACCESSOR into an elisp expression.

TODO doc better.

A drill-down string a specifier-separated list of keys.  The available
specifiers are as follows:
- -> for `alist-get'.  E.g.:
  ->a   for (alist-get a ...)
  ->'a  for (alist-get 'a ...)
  ->:a  for (alist-get :a ...)
  ->\"a\" for (alist-get \"a\" ... nil nil #'equal)
- . - for `plist-get', the same principle as for alists.
- [<number>] for `seq-elt', which works with all sequences.

E.g. a->b.c[0]."
  (deterred-format--parse-accessor-expr accessor))

(defconst deterred-format-alias-alist
  `((f-acc . deterred-format-accessor)
    (f-num . number-to-string)
    (f-join . string-join)))

(defun deterred-format--process-expr-item (item)
  (let* ((item-car (car-safe item))
         (item-alias (alist-get item-car deterred-format-alias-alist)))
    (cond ((eq item-car 'f)
           `(concat ,@(deterred-format--process-expr (cdr item))))
          ((eq item-car 'f-mapconcat)
           `(mapconcat
             ,(if (eq (car-safe (nth 1 item)) 'lambda)
                  (nth 1 item)
                `(lambda (iter) ,(deterred-format--process-expr-item
                                  (nth 1 item))))
             ,(deterred-format--process-expr-item (nth 2 item))
             ,(or (nth 3 item) "\n")))
          ((eq item-car 'f-img)
           `(let ((img (create-image ,(deterred-format--process-expr-item
                                       (nth 1 item))
                                     nil nil ,@(cddr item))))
              (apply
               #'propertize
               (if (image-type-available-p (image-property img :type))
                   (propertize "[IMG]" 'display img)
                 "[IMG]")
               (button--properties
                (lambda (&rest _)
                  (funcall deterred-format-image-open-command
                           ,(deterred-format--process-expr-item
                             (nth 1 item))))
                nil nil))))
          ((eq item-car 'f-button)
           `(apply #'propertize
                   ,(deterred-format--process-expr-item (nth 1 item))
                   'face 'button
                   (button--properties
                    ,(nth 2 item)
                    nil nil)))
          (item-alias `(,item-alias ,@(deterred-format--process-expr (cdr item))))
          ((listp item) (deterred-format--process-expr item))
          (t item))))

(defun deterred-format--process-expr (expr)
  (mapcar #'deterred-format--process-expr-item expr))

(defmacro deterred-format (&rest expr)
  `(concat
    ,@(deterred-format--process-expr expr)))

(provide 'deterred-format)
;;; deterred-format.el ends here

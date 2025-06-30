;;; deterred-template.el --- TODO -*- lexical-binding: t -*-

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
(require 'widget)
(require 'outline)

(defconst deterred-template--alist-nest-symbol "->")
(defconst deterred-template--plist-nest-symbol ".")
(defconst deterred-template--elt-symbols (cons "[" "]"))
(defconst deterred-template--list-separator ";")
(defconst deterred-template--quote-escape-symbol "@@@")

(defun deterred-template--escape (string)
  (replace-regexp-in-string (rx "\\\"") deterred-template--quote-escape-symbol string))

(defun deterred-template--unescape (string)
  (replace-regexp-in-string (regexp-quote deterred-template--quote-escape-symbol)
                            "\"" string))

(defun deterred-template--process-value (value)
  "If VALUE is a number, return it.

Otherwise, try to `read' it."
  (cond ((numberp value) value)
        ((listp value) (error "Can't process %s as value" value))
        (t (read (deterred-template--unescape value)))))

(defun deterred-template--parse-accessor-expr (accessor)
  "Parse ACCESSOR.

Accessor is a string that traverses an object."
  (let ((regexp (rx (| (literal deterred-template--alist-nest-symbol)
                       (literal deterred-template--plist-nest-symbol)
                       (:
                        (literal (car deterred-template--elt-symbols)) (group (+ num))
                        (literal (cdr deterred-template--elt-symbols))))))
        (index 0)
        (res (list nil))
        expr)
    (save-match-data
      (while-let ((match-index (string-match regexp accessor index)))
        (unless (eql index match-index)
          (push (substring accessor index match-index) res))
        (let ((match (match-string 0 accessor)))
          (cond ((equal match deterred-template--alist-nest-symbol)
                 (push 'alist-get res))
                ((equal match deterred-template--plist-nest-symbol)
                 (push 'plist-get res))
                ((and (equal (char-to-string
                              (seq-elt match 0))
                             (car deterred-template--elt-symbols))
                      (equal (char-to-string
                              (seq-elt match (1- (seq-length match))))
                             (cdr deterred-template--elt-symbols)))
                 (push 'seq-elt res)
                 (push (string-to-number (match-string 1 accessor)) res))
                (t (error "Accessor parsing error: %s" match))))
        (setq index (match-end 0)))
      (unless (eql index (seq-length accessor))
        (push (substring accessor index) res)))
    (setq res (nreverse res))
    (cl-loop for (key raw-value) on res by #'cddr
             for value = (deterred-template--process-value raw-value)
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

(defun deterred-template--parse-accessor (accessor)
  (if (string-match-p (rx bos "(" (* nonl) ")" eos) accessor)
      (read accessor)
    (deterred-template--parse-accessor-expr accessor)))

(defun deterred-template--parse-children (elem)
  (let (child-res)
    (mapc (lambda (child)
            (setq child-res
                  (deterred-template--parse-tree child child-res)))
          (cddr elem))
    child-res))

(defun deterred-template--parse-tree (elem &optional res)
  (cond
   ;; Simple text
   ((stringp elem) (push (deterred-template--unescape elem) res))
   ;; Just skip these tags
   ((member (car elem) '(html body p))
    (mapc (lambda (child) (setq res (deterred-template--parse-tree child res)))
          (cddr elem)))
   ;; <eval>(expr)</eval>
   ((eq (car elem) 'eval)
    (unless (stringp (caddr elem))
      (error "<eval> tags mustn't have any children"))
    (push `(eval ,(deterred-template--process-value (caddr elem))) res))
   ;; <var value="accessor" />
   ;; <var value="accessor" convert="number-to-string" />
   ;; accessor: a->b->c (for alists)
   ;; accessor: a.b.c (for plists)
   ((eq (car elem) 'var)
    (let ((value (alist-get 'value (cadr elem))))
      (unless value
        (error "<var> must have the \"value\" attribute"))
      (push `(var ((accessor . ,(deterred-template--parse-accessor
                                 (deterred-template--unescape value)))
                   (convert . ,(when-let (f (alist-get 'convert (cadr elem)))
                                 (intern f)))))
            res)))
   ;; <let var1="accessor1" var2="accessor2">...</let>
   ((eq (car elem) 'let)
    (push `(let ,(cl-loop
                  for (k . v) in (cadr elem)
                  collect (cons k (deterred-template--parse-accessor
                                   (deterred-template--unescape v))))
             ,(deterred-template--parse-children elem))
          res))
   ;; <mapconcat iter="accessor" var="iter">...</mapcar>
   ;; <mapconcat iter="accessor" var="iter" separator="\n" >...</mapcar>
   ((eq (car elem) 'mapconcat)
    (let ((iter (alist-get 'iter (cadr elem))))
      (unless iter
        (error "<mapconcat> must have the \"iter\" tag"))
      (push `(mapconcat
              ((accessor . ,(deterred-template--parse-accessor
                             (deterred-template--unescape iter)))
               (var . ,(intern (or (alist-get 'var (cadr elem)) "iter")))
               (separator . ,(or (alist-get 'separator (cadr elem)) "\n")))
              ,(deterred-template--parse-children elem))
            res)))
   ;; <propertize key1="value1; value2" >...</propertize>
   ((eq (car elem) 'propertize)
    (push `(propertize
            ,(cl-loop for (k . v) in (cadr elem)
                      collect
                      (cons k (mapcar #'deterred-template--process-value
                                      (string-split
                                       v deterred-template--list-separator))))
            ,(deterred-template--parse-children elem))
          res))
   ;; <b>, <i>, <u>
   ((member (car elem) '(b i u))
    (push `(propertize
            ((face . ,(pcase (car elem)
                        ('b ''bold)
                        ('i ''italic)
                        ('u ''underline))))
            ,(deterred-template--parse-children elem))
          res))
   ;; <br />
   ((eq (car elem) 'br)
    (push '(br) res))
   ;; <trim>...</trim>
   ((eq (car elem) 'trim)
    (push `(trim nil ,(deterred-template--parse-children elem)) res))
   ;; <button click="(expr)>...</button>"
   ((member (car elem) '(button))
    (let ((click (alist-get 'click (cadr elem))))
      (unless click
        (error "<button> must have the \"click\" attribute"))
      (push `(button
              ((notify . ,(deterred-template--process-value click)))
              ,(deterred-template--parse-children elem))
            res)))
   (t (error "Unknown tag: %s" (car elem))))
  res)

(defun deterred-template--reverse-tree (tree)
  (nreverse
   (mapcar (lambda (elem)
             (when (and (not (stringp elem))
                        (listp (nth 2 elem))
                        (nth 2 elem))
               (setf (nth 2 elem)
                     (deterred-template--reverse-tree (nth 2 elem))))
             elem)
           tree)))

(defun deterred-template--parse (template)
  (let (tree)
    (with-temp-buffer
      (insert (deterred-template--escape template))
      (setq tree (libxml-parse-html-region)))
    (deterred-template--reverse-tree
     (deterred-template--parse-tree tree))))

(defun deterred-template--tree-to-commands (tree)
  `(concat
    ,@(mapcar
       (lambda (elem)
         (if (stringp elem) elem
           (pcase (car elem)
             ('eval (cadr elem))
             ('var (if-let (convert (alist-get 'convert (cadr elem)))
                       `(,convert ,(alist-get 'accessor (cadr elem)))
                     (alist-get 'accessor (cadr elem))))
             ('let `(let (,@(mapcar
                             (lambda (elem)
                               `(,(car elem) ,(cdr elem)))
                             (cadr elem)))
                      ,(deterred-template--tree-to-commands (caddr elem))))
             ('mapconcat
              `(mapconcat
                (lambda (,(alist-get 'var (cadr elem)))
                  ,(deterred-template--tree-to-commands (caddr elem)))
                ,(alist-get 'accessor (cadr elem))
                ,(alist-get 'separator (cadr elem))))
             ('propertize
              `(propertize ,(deterred-template--tree-to-commands (caddr elem))
                           ,@(mapcan
                              (lambda (item)
                                `(',(car item) ,(cdr item)))
                              (cadr elem))))
             ('br "\n")
             ('trim `(string-trim ,(deterred-template--tree-to-commands (caddr elem))))
             ;; ('button
             ;;  `(widget-create
             ;;    'push-button
             ;;    :notify (lambda ()
             ;;              ,(alist-get 'notify (cadr elem)))
             ;;    ,(deterred-template--tree-to-commands (caddr elem))))
             (_ (error "Unknown element in syntax tree: %s" (car elem))))))
       tree)))

(defvar-local deterred-template-playground-template
    "A line of text
<eval>(number-to-string (+ 2 3))</eval><br>

<let v=\"1\" vv=\"\\\"test\\\"\">
<var value=\"v\" convert=\"number-to-string\" />-<var value=\"vv\" />
</let>

<let list=\"'(1 2 3 4 5)\">
<var value=\"list\" convert=\"prin1-to-string\" />
<mapconcat iter=\"list\" var=\"iter\">
<trim><b><var value=\"iter\" convert=\"number-to-string\" /></b></trim>
</mapconcat>
</let>
")

(defvar-local deterred-template-playground-html-tree nil)
(defvar-local deterred-template-playground-tree nil)
(defvar-local deterred-template-playground-commands nil)
(defvar-local deterred-template-playground-render nil)

(defvar deterred-template-playground-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'widget-button-press)
    (define-key map (kbd "q") (lambda ()
                                (interactive)
                                (quit-window t)))
    (when (fboundp #'evil-define-key*)
      (evil-define-key* '(normal motion) map
        (kbd "<RET>") #'widget-button-press
        "q" (lambda ()
              (interactive)
              (quit-window t))))
    map)
  "A keymap for `deterred-template-playground'.")

(define-derived-mode deterred-template-playground-mode fundamental-mode "DETERRED Template"
  :group 'deterred
  (outline-minor-mode 1))

(defun deterred-template--playground-render-controls ()
  (remove-overlays)
  (insert (propertize "* Template" 'face 'outline-1) "\n")
  (widget-create
   'text
   :value deterred-template-playground-template
   :format "%v"
   :size 80
   :notify (lambda (widget &rest _)
             (setq deterred-template-playground-template (widget-value widget))
             nil))
  (insert "\n\n")
  (widget-setup)
  (widget-create 'push-button
                 :notify (lambda (&rest _)
                           (deterred-template-playground-refresh))
                 "Refresh")
  (insert "\n\n"))

(defun deterred-template--print-between (start-line end-line value)
  (save-excursion
    (search-forward start-line)
    (next-line)
    (beginning-of-line)
    (save-excursion
      (let ((start (point)))
        (save-excursion
          (if end-line
              (progn
                (search-forward end-line)
                (previous-line)
                (beginning-of-line)
                (delete-region start (point)))
            (delete-region start (point-max))))
        (insert value)))))

(defun deterred-template-playground-refresh ()
  (interactive)
  (unless (derived-mode-p 'deterred-template-playground-mode)
    (user-error "Not in `deterred-template-playground-mode'"))
  (setq deterred-template-playground-html-tree
        (condition-case-unless-debug err
            (let ((tmp deterred-template-playground-template))
              (with-temp-buffer
                (insert (deterred-template--escape tmp))
                (libxml-parse-html-region)))
          (error (error-message-string err))))
  (setq deterred-template-playground-tree
        (condition-case-unless-debug err
            (deterred-template--parse deterred-template-playground-template)
          (error (error-message-string err))))
  (setq deterred-template-playground-commands
        (condition-case-unless-debug err
            (deterred-template--tree-to-commands
             deterred-template-playground-tree)
          (error (error-message-string err))))
  (setq deterred-template-playground-render
        (condition-case-unless-debug err
            (eval deterred-template-playground-commands)
          (error (error-message-string err))))
  (save-excursion
    (outline-show-all)
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (deterred-template--print-between
       "* HTML tree" "* Syntax tree"
       (concat
        (with-output-to-string
          (pp deterred-template-playground-html-tree))
        "\n\n"))
      (deterred-template--print-between
       "* Syntax tree" "* Command tree"
       (concat
        (with-output-to-string
          (pp deterred-template-playground-tree))
        "\n\n"))
      (deterred-template--print-between
       "* Command tree" "* Render"
       (concat
        (with-output-to-string
          (pp deterred-template-playground-commands))
        "\n\n"))
      (deterred-template--print-between
       "* Render" nil
       deterred-template-playground-render))))

(defun deterred-template--playground-render ()
  (let ((inhibit-read-only t))
    (erase-buffer)
    (unless (derived-mode-p #'deterred-template-playground-mode)
      (deterred-template-playground-mode))
    (deterred-template--playground-render-controls)
    (insert (propertize "* HTML tree" 'face 'outline-1) "\n")
    (insert "TODO\n\n")
    (insert (propertize "* Syntax tree" 'face 'outline-1) "\n")
    (insert "TODO\n\n")
    (insert (propertize "* Command tree" 'face 'outline-1) "\n")
    (insert "TODO\n\n")
    (insert (propertize "* Render" 'face 'outline-1) "\n")
    (insert "TODO\n\n")
    (deterred-template-playground-refresh)))

(defun deterred-template-playground ()
  (interactive)
  (let ((buffer (generate-new-buffer "*deterred-template-playground*")))
    (switch-to-buffer-other-window buffer)
    (with-current-buffer buffer
      (deterred-template--playground-render)
      (goto-char (point-min)))))

(provide 'deterred-template)
;;; deterred-template.el ends here

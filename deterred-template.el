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
(require 'button)
(require 'outline)

(defconst deterred-template--alist-nest-symbol "->")
(defconst deterred-template--plist-nest-symbol ".")
(defconst deterred-template--elt-symbols (cons "[" "]"))
(defconst deterred-template--list-separator ";")
(defconst deterred-template--quote-escape-symbol "@@@")
(defconst deterred-template--function-start "f--")

(defun deterred-template--escape (string)
  "Replace escaped quotes in STRING.

This is necessary because `libxml-parse-html-region' misses them in
attribute lists, e.g. <div value=\"\\\"item\\\"\" /> (which also
breaks helpful, by the way, but not the built-in `describe-function'."
  (replace-regexp-in-string (rx "\\\"") deterred-template--quote-escape-symbol string))

(defun deterred-template--unescape (string)
  "Restore escaped quotes in STRING."
  (replace-regexp-in-string (regexp-quote deterred-template--quote-escape-symbol)
                            "\"" string))

(defun deterred-template--process-value (value)
  "If VALUE is a number, return it.

Otherwise, try to `read' it.

This is meant to be used for parsing s-expressions, e.g. in
<div a=\"(+ 1 2)\" b=\"1\" /> \"a\" will be string but \"b\" will be
number."
  (cond ((numberp value) value)
        ((listp value) (error "Can't process %s as value" value))
        (t (read (deterred-template--unescape value)))))

(defun deterred-template--parse-accessor-expr (accessor)
  "Parse an ACCESSOR expression."
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
  "Parse ACCESSOR into an elisp expression.

The accessor is either a Lisp expression in parentheses, which is
`read', or a drill-down string.

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
  (cond
   ((string-match-p (rx bos "(" (* nonl) ")" eos) accessor)
    (read accessor))
   ((string-match-p (rx bos "/") accessor)
    accessor)
   (t (deterred-template--parse-accessor-expr accessor))))

(defun deterred-template--parse-children (elem)
  "Apply `deterred-template--parse-tree' to all children of ELEM."
  (let (child-res)
    (mapc (lambda (child)
            (setq child-res
                  (deterred-template--parse-tree child child-res)))
          (cddr elem))
    child-res))

(defun deterred-template--parse-tree (elem &optional res)
  "Parse ELEM into a template syntax tree.

ELEM is a `libxml-parse-html-region' node.  RES is the recursive
parameter.

The return value is a list of syntax tree nodes.  One node is either a
string or list with the tree values:
- node type (always a symbol, get with `car')
- parameters (can be anything, get with `cadr')
- children (other nodes, get with `caddr').
Note that this is unlike the HTML tree, where children are appended
after `cadr'.

See `deterred-deftemplate' for docs on syntax.  The nodes aren't
perfectly mapped to tags.

The available node types are:
- eval - just evaluate the expresion.  The cadr is the expression.
  The expression has to return string because it will be concatenated
  later.
- var - print the value of the variable.  The cadr in an alist with
  two keys:
  - accessor (see `deterred-template--parse-accessor')
  - convert (either a function to convert the value to string or nil)
- let - bind the variable for nest.  The cadr is the alist of
  variables to bind.
- mapconcat.  The cadr is an alist with the following keys:
  - accessor (iterate over this value)
  - var (bind the value to this variable)
  - separator (concat the results with this symbol)
- propertize.  The cadr is the list of properties to bind.
- func - call the function on the child nodes.  The cadr is the alist
  with the following keys:
  - func
  - args - the argument list.  `:value' is substituted with the
    evaluation of child nodes.
- br - just linebreak, no parameters.
- trim - trim the evaluation of child nodes."
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
   ;; <mapconcat iter="accessor" var="iter" separator="\n" >...</mapconcat>
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
   ;; <img src=".../" />
   ((eq (car elem) 'img)
    (let ((src (alist-get 'src (cadr elem))))
      (unless src
        (error "<img> must have the src attribute"))
      (push
       `(img ((source . ,(deterred-template--parse-accessor src))
              (params
               . ,(mapcan (lambda (elem)
                            (list (intern (format ":%s" (car elem)))
                                  (deterred-template--process-value (cdr elem))))
                          (seq-filter
                           (lambda (elem)
                             (not (member (car elem) '(src))))
                           (cadr elem)))))
             nil)
       res)))
   ;; <f--function-name>...</f--function-name>
   ;; <f--function-name args="(:value 1 2 3)">...</f--function-name>
   ((string-match-p (rx bos (literal deterred-template--function-start))
                    (symbol-name (car elem)))
    (push `(func
            ((func . ,(intern (substring (symbol-name (car elem))
                                         (length deterred-template--function-start))))
             (args . ,(if (alist-get 'args (cadr elem))
                          (deterred-template--process-value
                           (alist-get 'args (cadr elem)))
                        '(:value))))
            ,(deterred-template--parse-children elem))
          res))
   ((eq (car elem) 'when)
    (let ((cond (alist-get 'cond (cadr elem))))
      (unless cond
        (error "<when> must have the cond attribute"))
      (push `(func
              ((func . when)
               (args . (,(deterred-template--process-value cond) :value)))
              ,(deterred-template--parse-children elem))
            res)))
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
   ;; <line>...</line>
   ((eq (car elem) 'line)
    (push `(func
            ((func . deterred-utils-make-line)
             (args . (:value)))
            ,(deterred-template--parse-children elem))
          res))
   ;; <button click="(expr)>...</button>"
   ((member (car elem) '(button))
    (let ((click (alist-get 'click (cadr elem))))
      (unless click
        (error "<button> must have the \"click\" attribute"))
      (push `(button
              ((callback . ,(deterred-template--process-value click)))
              ,(deterred-template--parse-children elem))
            res)))
   (t (error "Unknown tag: %s" (car elem))))
  res)

(defun deterred-template--reverse-tree (tree)
  "Reverse DETERRED syntax TREE.

This is necessary because `deterred-template--parse-tree' returns it
in the wrong order, and it's impossible to reverse it there because
the same list is appened on different recursive levels."
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
  "Parse the TEMPLATE string and return its syntax tree.

See `deterred-template--parse-tree' for the description of the
latter."
  (let (tree)
    (with-temp-buffer
      (insert (deterred-template--escape template))
      (setq tree (libxml-parse-html-region)))
    (deterred-template--reverse-tree
     (deterred-template--parse-tree tree))))

(defun deterred-template--tree-to-commands (tree)
  "Convert syntax TREE into an elisp expression."
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
             ('func
              `(,(alist-get 'func (cadr elem))
                ,@(mapcar (lambda (arg)
                            (if (eq arg :value)
                                (deterred-template--tree-to-commands (caddr elem))
                              arg))
                          (alist-get 'args (cadr elem)))))
             ('trim `(string-trim ,(deterred-template--tree-to-commands (caddr elem))))
             ('img `(let ((img (create-image ,(alist-get 'source (cadr elem))
                                             nil nil
                                             ,@(alist-get 'params (cadr elem)))))
                      (if (image-type-available-p (image-property img :type))
                          (propertize "[IMG]" 'display img)
                        "[IMG]")))
             ('button
              `(apply #'propertize
                      ,(deterred-template--tree-to-commands (caddr elem))
                      'face 'button
                      (button--properties
                       ,(alist-get 'callback (cadr elem))
                       nil ,(alist-get 'help-echo (cadr elem)))))
             (_ (error "Unknown element in syntax tree: %s" (car elem))))))
       tree)))

(defmacro deterred-deftemplate (name args doc-string-or-value &optional value)
  "Define DETERRED string template function.

The purpose of this is to avoid manual writing of long and unwieldy
expressions of `concat', `format', etc. by using an HTML-esque string,
which is converted to such an expression by a macro.

NAME is the resulting function is name, ARGS is its arguments.  The
third argument, DOC-STRING-OR-VALUE is treated as docstring is VALUE
is non-nil.

The value is a string with the following tags:
- <eval>(expr)</eval> - evaluate expr, which must always return string.
- <var value=\"accessor\" /> - print the value accessed by the
  accessor expression (see `deterred-template--parse-accessor' on
  that).
  If the value isn't string, the convert parameter required, which has
  to be a function converting the value to string (`prin1-to-string'
  is a universal one).
- <let var1=\"accessor1\" var2=\"accessor2\">...</let> - bind the
  variables for the child nodes.
- <mapconcat iter=\"accessor\" var=\"iter\" separator=\"\n\">
  ...</mapconcat> - iterate over iter, binding it to the variable var,
  and concat the results with separator.  Only iter is required.
- <propertize key1=\"value1; value2\" >...</propertize>
- <f--function-name args=\"(:value)\">...</f--function-name> - call
  function-name on children.
  E.g. <f--string-pad args=\"(:value 20 nil t)\">...
- <b>, <i>, <u> - make the children bold, italic, or underline.
- <br> - insert \\n.
- <trim> - trim children."

  (declare (indent defun) (doc-string 3))
  (let ((doc-string (when value doc-string-or-value))
        (value (or value doc-string-or-value)))
    `(defun ,name (,@args)
       ,@(when doc-string `(,doc-string))
       ,(deterred-template--tree-to-commands
         (deterred-template--parse
          (deterred-template--escape value))))))

(defmacro deterred-inline-template (value)
  (declare (indent 0))
  (deterred-template--tree-to-commands
   (deterred-template--parse
    (deterred-template--escape value))))

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
<br>
<f--string-pad args=\"(:value 20 nil t)\">test</f--string-pad>
<button click=\"(lambda (&rest _) (message \\\"Hello\\\"))\">kek</button>
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

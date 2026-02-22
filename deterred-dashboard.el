;;; deterred-dashboard.el --- Dashboard functionality for DETERRED -*- lexical-binding: t -*-

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

;; Dashboard functionality for DETERRED.
;;
;; See `deterred-dashboard' on creating a custom dashboard.  All
;; created dashboards must be registered in `deterred-dashboards'.
;;
;; Invoke a dashboard with `deterred-dashboard-open'.

;;; Code:
(require 'eieio)
(require 'outline)
(require 'validate)
(require 'crm)

(require 'deterred-format)
(require 'deterred-grid)
(require 'deterred-utils)

(defcustom deterred-dashboards nil
  "List of DETERRED dashboard objects."
  :type 'list
  :group 'deterred)

(defcustom deterred-dashboard-python (or
                                      (executable-find "python3")
                                      (executable-find "python"))
  "A Python executable used with DETERRED.

This probably needs to be some virtual env with matplotlib, pandas,
etc."
  :type 'string
  :group 'deterred)

(defconst deterred-dashboard--datasets-schema
  '(repeat
    (cons symbol
          (repeat (choice
                   (cons :tag "Name" (const name) string)
                   (cons :tag "Tags" (const tags) (repeat symbol)))))))

(defvar-local deterred-dashboard-params nil
  "Current parameters for the dashboard.")
(defvar-local deterred-dashboard-current nil
  "The active `deterred-dashboard' instance.")
(defvar-local deterred-dashboard-data nil
  "Current data for the dashboard.")

(defclass deterred-dashboard ()
  ((name :initarg name :type string))
  "Abstract superclass for DETERRED dashboards.

The name parameter in the class is required.

The dashboard works as follows.  First, it requires parameters,
e.g. the date range.  `deterred-dashboard-default-params' returns the
default values of the parameters; `deterred-dashboard-render-params'
renders controls that update parameters.  The current value of the
parameters in stored in `deterred-dashboard-params'.

Then, datasets are retrieved using the parameters.
`deterred-dashboard-list-datasets' lists datasets returned by the
dashboard; `deterred-dashboard-fetch-datasets' actually retrives them.
The current values of the datasets is stored in
`deterred-dashboard-data'.

Then, `deterred-dashboard-render-results' is used to render the
datasets.

See the docs on the mentioned generics for more detail.

There are also some helpers to render parameters:
- `deterred-dashboard-widget-number' - a widget to edit a number.
- `deterred-dashboard-widget-date' - a widget to edit a date.
- `deterred-dashboard-widget-checkbox' - a boolean widget.
All the widgets update the required value in
`deterred-dashboard-params' in the `:notify' function.

And helpers to render results:
- `deterred-dashboard-exec-python'
- `deterred-dashboard-print-images-base64'.

See also `deterred-dashboard-dummy' for an example dashboard."
  :abstract t)

(cl-defgeneric deterred-dashboard-list-datasets (dashboard)
  "List datasets returned by DASHBOARD.

Return an alist with the datasets' metadata.  The keys are symbols
that serve as dataset names, and the values are alists with the
following keys:
- `name' - human-readable name
- `tags' - a list of symbols with dataset tags.")

(cl-defgeneric deterred-dashboard-fetch-datasets (dashboard params)
  "Fetch datasets (with contents) from DASHBOARD.

PARAMS is an alist of parameters.

Return an alist, where the keys are dataset names (as returned by
`deterred-dashboard-list-datasets'), and the values are the contents,
which have to be lists of alists to work correctly.")

(cl-defgeneric deterred-dashboard-default-params (dashboard)
  "Return default parameters for DASHBOARD.

Return an alist, where the keys are parameter names.")

(cl-defmethod deterred-dashboard-default-params ((_ deterred-dashboard))
  "Return nil, meaning that this dashboard has no parameters."
  nil)

(cl-defgeneric deterred-dashboard-render-params (dashboard)
  "Render the parameter section for DASHBOARD.

The proposed implementation is to use `widget-create' and update
`deterred-dashboard-params' in the `:notify' method.

There are some helper macros:
- `deterred-dashboard-widget-number'
- `deterred-dashboard-widget-date'
- `deterred-dashboard-widget-checkbox'")

(cl-defmethod deterred-dashboard-render-params ((_ deterred-dashboard))
  "Do not render the parameter section for dashboard."
  nil)

(cl-defgeneric deterred-dashboard-render-results (dashboard params data)
  "Render results for DASHBOARD.

PARAMS are the parameters, DATA is an alist as returned by
`deterred-dashboard-list-datasets', with data added as the `data'
key.")

(cl-defmethod deterred-dashboard-render-results ((_ deterred-dashboard) _params _data)
  "Do not render the results section for dashboard."
  nil)

(defun deterred-dashboard--data (dashboard params)
  "Collect data for DASHBOARD, according to PARAMS.

Return an alist as returned by `deterred-dashboard-list-datasets', but
with data added to the datasets in the `data' key.

Data has be a list of alists to work correctly."
  (let ((schema (deterred-utils-validate
                 (deterred-dashboard-list-datasets dashboard)
                 deterred-dashboard--datasets-schema
                 (format "Dashboard: %s" dashboard)))
        (datasets (deterred-dashboard-fetch-datasets dashboard params)))
    (cl-loop for (name . params) in schema
             for dataset = (alist-get name datasets)
             ;; unless dataset
             ;; do (error "Dashboard \"%s\" hasn't returned dataset \"%s\""
             ;;           (oref dashboard name) name)
             collect (cons name
                           (append params
                                   (list (cons 'data dataset)))))))

(defvar deterred-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") (lambda ()
                                (interactive)
                                (quit-window t)))
    (define-key map (kbd "<tab>") #'outline-toggle-children)
    (when (fboundp #'evil-define-key*)
      (evil-define-key* '(normal motion) map
        "q" (lambda ()
              (interactive)
              (quit-window t))
        (kbd "<tab>") #'outline-toggle-children))
    map)
  "A keymap for `deterred-dashboard-mode'.")

(define-derived-mode deterred-dashboard-mode fundamental-mode "DETERRED Dashboard"
  :group 'deterred
  (outline-minor-mode 1))

(defun deterred-dashboard--render-actions ()
  "Render the actions section for the dashboard interface."
  (insert (deterred-format (f-h1 "Actions") "\n"))
  (widget-create 'push-button
                 :notify (lambda (&rest _)
                           (deterred-dashboard-refresh))
                 "Refresh")
  (insert "\n\n"))

(defun deterred-dashboard--render-datasets (name data)
  "Render the datasets section for the dashboard interface.

NAME is the dashboard name, DATA is the output of
`deterred-dashboard--data'."
  (insert
   (deterred-format
    (f-h1 "Datasets") "\n"
    (f-mapconcat
     (f "- " (alist-get 'name iter)
        (when (alist-get 'tags iter)
          (f " ("
             (f-ace (mapconcat #'symbol-name (alist-get 'tags iter) " ")
                    'deterred-faces-info)
             ")"))
        " " (f-button "[View table]"
                      (lambda (&rest _)
                        (deterred-grid-show (alist-get 'data iter))))
        " " (f-button "[View raw]"
                      (lambda (&rest _)
                        (let ((buffer
                               (generate-new-buffer
                                (format "*DETERRED-data-%s-%s*"
                                        name (alist-get 'name iter)))))
                          (with-current-buffer buffer
                            (insert
                             ;; No clue
                             (with-output-to-string
                               (pp iter)))
                            (goto-char (point-min)))
                          (switch-to-buffer-other-window buffer)))))
     data))
   "\n\n"))

(defun deterred-dashboard--render-results (dashboard params data)
  "Render the results sections for DASHBOARD.

PARAMS is the parameters, DATA is data as returned by
`deterred-dashboard--data'."
  (insert
   (deterred-format
    (f-h1 "Results") "\n"))
  (deterred-dashboard-render-results dashboard params data))

(defun deterred-dashboard-refresh ()
  "Refresh the dashboard interface."
  (interactive)
  (let ((data (deterred-dashboard--data deterred-dashboard-current
                                        deterred-dashboard-params))
        (inhibit-read-only t)
        (name (oref deterred-dashboard-current name)))
    (setq-local deterred-dashboard-data data)
    (save-excursion
      (goto-char (point-min))
      (search-forward (format "* Datasets") nil 'noerror)
      (beginning-of-line)
      (delete-region (point) (point-max))
      (deterred-dashboard--render-datasets name data)
      (deterred-dashboard--render-results
       deterred-dashboard-current
       deterred-dashboard-params data))))

(defun deterred-dashboard-maybe-init ()
  "Initialize `deterred-dashboards' with default dashboards if nil."
  (unless deterred-dashboards
    (require 'deterred-dashboard-dummy)
    (require 'deterred-dashboard-mpd)
    (require 'deterred-dashboard-podcasts)
    (require 'deterred-dashboard-wakatime)
    (require 'deterred-dashboard-activitywatch)
    (require 'deterred-dashboard-read-it-later)
    (require 'deterred-dashboard-digikam)
    (require 'deterred-dashboard-messengers)
    (require 'deterred-dashboard-org-journal-tags)
    (require 'deterred-dashboard-org-roam)
    (require 'deterred-dashboard-hledger)
    (require 'deterred-dashboard-transport)
    (require 'deterred-dashboard-ai)

    (setq deterred-dashboards
          (list (deterred-dashboard-dummy)
                (deterred-dashboard-mpd)
                (deterred-dashboard-podcasts)
                (deterred-dashboard-wakatime)
                (deterred-dashboard-activitywatch)
                (deterred-dashboard-read-it-later)
                (deterred-dashboard-digikam)
                (deterred-dashboard-messengers)
                (deterred-dashboard-org-journal-tags)
                (deterred-dashboard-org-roam)
                (deterred-dashboard-hledger)
                (deterred-dashboard-transport)
                (deterred-dashboard-ai)))))

;;;###autoload
(defun deterred-dashboard-open (dashboard &optional override-params)
  "Open a DETERRED dashboard.

DASHBOARD is a dashboard object.  OVERRIDE-PARAMS is the parameters
object."
  (interactive
   (list
    (progn
      (deterred-dashboard-maybe-init)
      (let ((dashboards-by-name
             (mapcar
              (lambda (dashboard)
                (cons (oref dashboard name) dashboard))
              deterred-dashboards)))
        (alist-get
         (completing-read "Dashboard: " dashboards-by-name nil t)
         dashboards-by-name nil nil #'equal)))))
  (let* ((params (copy-tree (deterred-dashboard-default-params dashboard)))
         (name (oref dashboard name))
         (buffer (generate-new-buffer (format "*DETERRED-dashboard-%s*" name)))
         (inhibit-read-only t))
    (when override-params
      (cl-loop for (k . v) in override-params
               do (setf (alist-get k params) v)))
    (with-current-buffer buffer
      (deterred-dashboard-mode)
      (setq-local deterred-dashboard-params params)
      (setq-local deterred-dashboard-current dashboard)
      (insert (deterred-format "Dashboard: " name "\n\n"))
      (when params
        (insert
         (deterred-format
          (f-h1 "Parameters") "\n"))
        (deterred-dashboard-render-params dashboard)
        (insert "\n"))
      (deterred-dashboard--render-actions)
      (deterred-dashboard-refresh)
      (widget-setup)
      (goto-char (point-min)))
    (switch-to-buffer-other-window buffer)))

(defmacro deterred-dashboard--require-arguments (&rest symbols)
  "Throw error if any of the SYMBOLS are valued nil."
  `(progn
     ,@(mapcar
        (lambda (s)
          `(unless ,s
             (error ,(format "The `%s' argument is required" (symbol-name s)))))
        symbols)))

(cl-defmacro deterred-dashboard-widget-number (&key name key (size 20))
  "A widget to edit a number.

NAME is the displayed name, KEY is the key in
`deterred-dashboard-params'.  The stored value is a number or nil.

SIZE is the size of field."
  (deterred-dashboard--require-arguments name key)
  `(widget-create
    'editable-field
    :size ,size
    :format (deterred-format (f-ace (f ,name ": ") 'widget-button)
                             "%v   ")
    :value (let ((val (alist-get ,key deterred-dashboard-params)))
             (when (numberp val)
               (number-to-string val)))
    :notify (lambda (widget &rest _)
              (let ((var (widget-value widget)))
                (setf
                 (alist-get ,key deterred-dashboard-params)
                 (if (string-empty-p var) nil
                   (string-to-number var)))))))

(defun deterred-dashboard--update-date-overlay (widget timestamp)
  "Update the date overlay for WIDGET with TIMESTAMP."
  (let ((ov (widget-get widget 'date-overlay)))
    (unless ov
      (setq ov (make-overlay (point) (point)))
      (widget-put widget 'date-overlay ov))
    (overlay-put
     ov 'after-string
     (deterred-format
      "=> "
      (f-ace (if timestamp
                 (format-time-string deterred-dispatcher-date-format timestamp)
               "(no date)")
             'deterred-faces-date)))))

(cl-defmacro deterred-dashboard-widget-date
    (&key name key (size 20) kind display-date)
  "A widget to edit a date like `org-read-date'.

NAME is the displayed name, KEY is the key in
`deterred-dashboard-params'.  The stored value is either a UNIX
timestamp or nil.

SIZE is the size of the field.  If KIND is \"from\", ensure that the
timestamp is the start of the day; if it's \"to\", ensure it's the end
of the day.

If DISPLAY-DATE is non-nil, display the resulting date near the widget
using an overlay, like `org-read-date'."
  (deterred-dashboard--require-arguments name key)
  `(let ((widget
          (widget-create
           'editable-field
           :size  ,size
           :format (deterred-format (f-ace (f ,name ": ") 'widget-button)
                                    "%v   ")
           :value (let ((val (alist-get ,key deterred-dashboard-params)))
                    (if (numberp val)
                        (format-time-string "%Y-%m-%d" val)
                      ""))
           :notify (lambda (widget &rest _)
                     (let* ((var (widget-value widget))
                            (timestamp (deterred-utils-read-date var ,kind)))
                       (setf
                        (alist-get ,key deterred-dashboard-params)
                        timestamp)
                       ,@(when display-date
                           `((deterred-dashboard--update-date-overlay
                              widget timestamp))))))))
     ,@(when display-date
         `((deterred-dashboard--update-date-overlay
            widget (alist-get ,key deterred-dashboard-params))))))

(cl-defmacro deterred-dashboard-widget-checkbox
    (&key name key)
  "A widget to edit a boolean value.

NAME is the displayed name, KEY is the key in
`deterred-dashboard-params'.  The stored value is either t or nil."
  (deterred-dashboard--require-arguments name key)
  `(progn
     (insert (propertize ,name 'face 'widget-button) ": ")
     (widget-create
      'checkbox
      :value (let ((val (alist-get ,key deterred-dashboard-params)))
               val)
      :notify (lambda (widget &rest _)
                (let* ((var (widget-value widget)))
                  (setf
                   (alist-get ,key deterred-dashboard-params)
                   var))))))

(defun deterred-dashboard--process-completing-read (selected options)
  "Find SELECTED in OPTIONS.

If SELECTED is nil or an empty string, return nil.

If SELECTED is a non-empty list, call this on each item in SELECTED.

If OPTIONS is a list of symbols, and SELECTED is a string, `intern'
it, otherwise return it as-is.

If OPTIONS is a list of strings, return SELECTED as-is.

If OPTIONS is an alist, return the value corresponding to SELECTED."
  (cond
   ((and (listp selected) (> (seq-length selected) 0))
    (mapcar (lambda (item)
              (deterred-dashboard--process-completing-read
               item options))
            selected))
   ((or (string-empty-p selected) (null selected)) nil)
   ((symbolp (car options)) (if (stringp selected)
                                (intern selected)
                              selected))
   ((stringp (car options)) selected)
   ((and (consp options)
         (symbolp (caar options)))
    (alist-get selected options))
   ((and (consp options)
         (stringp (caar options)))
    (alist-get selected options nil nil #'equal))))

(defun deterred-dashboard--widget-render-option (widget value)
  "Render VALUE after WIDGET."
  (save-excursion
    (goto-char (widget-get widget :to))
    (let ((ov (widget-get widget 'value-overlay))
          (option-start (point))
          (inhibit-read-only t))
      (when ov
        (delete-region (overlay-start ov)
                       (overlay-end ov)))
      (insert
       ": " (cond
             ((null value) (propertize "(nil)" 'face 'deterred-faces-info))
             ((listp value) (deterred-format
                             (f-ace "(" 'deterred-faces-info)
                             (f-mapconcat iter value "; ")
                             (f-ace ")" 'deterred-faces-info)))
             ((stringp value) value)))
      (if ov
          (move-overlay ov option-start (point))
        (setq ov (make-overlay option-start (point)))
        (widget-put widget 'value-overlay ov)))))

(cl-defmacro deterred-dashboard-widget-completing-read
    (&key name key options (prompt "Select: "))
  "A widget to select a value from OPTIONS with `completing-read'.

NAME is the displayed name, KEY is the key in
`deterred-dashboard-params'.  OPTIONS can be a list of symbols, a list
of strings, or an alist with symbols or strings as keys.

PROMPT is passed to `completing-read'."
  (deterred-dashboard--require-arguments name key options)
  `(progn
     (let* ((widget-push-button-prefix "")
            (widget-push-button-suffix "")
            (widget
             (widget-create
              'push-button
              :notify
              (lambda (widget &rest _)
                (let* ((selected (completing-read ,prompt ,options))
                       (value (deterred-dashboard--process-completing-read
                               selected ,options)))
                  (deterred-dashboard--widget-render-option widget selected)
                  (setf (alist-get ,key deterred-dashboard-params)
                        value)))
              ,name)))
       (insert " ")
       (deterred-dashboard--widget-render-option
        widget (alist-get ,key deterred-dashboard-params)))))

(cl-defmacro deterred-dashboard-widget-completing-read-multiple
    (&key name key options (prompt "Select: ") (separator ";"))
  "A widget to select values with `completing-read-multiple'.

NAME is the displayed name, KEY is the key in
`deterred-dashboard-params'.  OPTIONS can be a list of symbols, a list
of strings, or an alist with symbols or strings as keys.

PROMPT is passed to `completing-read', SEPARATOR is bound to
`crm-separator'."
  (deterred-dashboard--require-arguments name key options)
  `(progn
     (let* ((widget-push-button-prefix "")
            (widget-push-button-suffix "")
            (widget
             (widget-create
              'push-button
              :notify
              (lambda (widget &rest _)
                (let* ((crm-separator ,separator)
                       (selected (completing-read-multiple
                                  ,prompt ,options nil nil
                                  (when current-prefix-arg
                                    (string-join
                                     (alist-get ,key deterred-dashboard-params)
                                     crm-separator))))
                       (value (deterred-dashboard--process-completing-read
                               selected ,options)))
                  (deterred-dashboard--widget-render-option widget selected)
                  (setf (alist-get ,key deterred-dashboard-params)
                        value)))
              ,name)))
       (insert " ")
       (deterred-dashboard--widget-render-option
        widget (alist-get ,key deterred-dashboard-params)))))

(defun deterred-dashboard--print-error (desc err output)
  "Format an error for dashboard.

DESC is the error description, ERR is the error object, OUTPUT is the
output string."
  (insert
   (deterred-format
    (f-ace desc 'error) "\n"
    "Error: " (prin1-to-string err) "\n"
    output
    "\n\n")))

(defun deterred-dashboard-save-image-at-point ()
  "Save the image at point to a file."
  (interactive)
  (let ((data (get-text-property (point) 'deterred-image-data)))
    (if (not data)
        (message "No image at point")
      (let* ((img-type (or (get-text-property (point) 'deterred-image-type)
                           'png))
             (file (read-file-name "Save image to: "
                                   nil nil nil
                                   (format "image.%s" img-type))))
        (with-temp-file file
          (set-buffer-multibyte nil)
          (insert data))
        (message "Image saved to %s" file)))))

(defvar deterred-dashboard-image-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "S") #'deterred-dashboard-save-image-at-point)
    (when (fboundp #'evil-define-key*)
      (evil-define-key* '(normal) map
        "S" #'deterred-dashboard-save-image-at-point))
    map)
  "Keymap for images in DETERRED dashboards.")

(defun deterred-dashboard-print-images-base64 (images &rest props)
  "Print IMAGES given as base64 strings.

IMAGES is either one base64-encoded image string or a sequence of them.

PROPS are forwarded to `create-image'.

Press 's' or 'w' on an image to save it to a file."
  (when (stringp images)
    (setq images (list images)))
  (insert
   (mapconcat
    (lambda (image)
      (condition-case-unless-debug err
          (let* ((data (base64-decode-string image))
                 (img (apply #'create-image
                             data nil t props))
                 (img-type (image-property img :type)))
            (if (image-type-available-p img-type)
                (propertize "[IMG]"
                            'display img
                            'deterred-image-data data
                            'deterred-image-type img-type
                            'keymap deterred-dashboard-image-map)
              "[IMG]"))
        (error (deterred-format
                (f-ace "Error: " 'error)
                (prin1-to-string err)))))
    images
    "\n")))

(defun deterred-dashboard--get-pythonpath ()
  "Get PYTHONPATH for DETERRED to load the python module."
  (let* ((deterred-folder
          (or
           (and load-file-name
                (concat (file-name-directory load-file-name) "python/"))
           (concat (string-replace "/dashboards" "" default-directory)
                   "python/"))))
    (string-join
     (append (string-split (or (getenv "PYTHONPATH") "") ":" t)
             (list deterred-folder))
     ":")))

(defvar deterred-dashboard--pythonpath (deterred-dashboard--get-pythonpath)
  "PYTHONPATH for DETERRED.")

(cl-defun deterred-dashboard-exec-python
    (&key python-code python-file
          input (on-error #'deterred-dashboard--print-error)
          on-success)
  "Execute Python code in a dashboard.

PYTHON-CODE is a string of Python code, PYTHON-FILE is a file with
Python code.  Either one of these parameters is required, but not
both.

INPUT is json-encoded and given to the interpreter, e.g. to be read by
json.loads(input()).

ON-ERROR is invoked when something goes wrong, the default value is
`deterred-dashboard--print-error'.

ON-SUCCESS is invoked on the process completion, with json-decoded
stdout of the process as the sole argument."
  (unless (or python-code python-file)
    (error "Set either `python-code' or `python-file'"))
  (let ((args
         `(,@(when python-code
               `("-c" ,python-code))
           ,@(when python-file
               `(,python-file))))
        output parsed-output)
    (condition-case-unless-debug err
        (with-temp-buffer
          (insert (json-encode input))
          (let ((process-environment (copy-sequence process-environment)))
            (setenv "PYTHONPATH" deterred-dashboard--pythonpath)
            (apply #'call-process-region (point-min) (point-max)
                   deterred-dashboard-python t t nil args))
          (goto-char (point-min))
          (setq output (buffer-string))
          (setq parsed-output (json-read)))
      (error
       (funcall on-error "Python execution error" err output)))
    (when (and parsed-output on-success)
      (funcall on-success parsed-output))))

(provide 'deterred-dashboard)
;;; deterred-dashboard.el ends here

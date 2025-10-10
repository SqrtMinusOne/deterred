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

;; TODO

;;; Code:
(require 'eieio)
(require 'outline)
(require 'validate)
(require 'deterred-format)
(require 'deterred-grid)

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

(defvar-local deterred-dashboard-params nil)
(defvar-local deterred-dashboard-current nil)
(defvar-local deterred-dashboard-data nil)

(defclass deterred-dashboard ()
  ((name :initarg name :type string))
  "Abstract superclass for DETERRED dashboards."
  :abstract t)

(cl-defgeneric deterred-dashboard-list-datasets (dashboard)
  "List databasets returned by DASHBOARD.")

(cl-defgeneric deterred-dashboard-fetch-datasets (dashboard params)
  "Fetch datasets (with contents) from DASHBOARD.

PARAMS is an alist of parameters.")

(cl-defgeneric deterred-dashboard-default-params (dashboard)
  "Return default parameters for DASHBOARD.")

(cl-defmethod deterred-dashboard-default-params ((_ deterred-dashboard))
  nil)

(cl-defgeneric deterred-dashboard-render-params (dashboard))

(cl-defmethod deterred-dashboard-render-params ((_ deterred-dashboard))
  nil)

(cl-defgeneric deterred-dashboard-render-results (dashboard data))

(cl-defmethod deterred-dashboard-render-results ((_ deterred-dashboard) data)
  nil)

(defun deterred-dashboard--data (dashboard params)
  (let ((schema (deterred-utils-validate
                 (deterred-dashboard-list-datasets dashboard)
                 deterred-dashboard--datasets-schema
                 (format "Dashboard: %s" dashboard)))
        (datasets (deterred-dashboard-fetch-datasets dashboard params)))
    (cl-loop for (name . params) in schema
             for dataset = (alist-get name datasets)
             unless dataset
             do (error "Dashboard \"%s\" hasn't returned dataset \"%s\""
                       (oref dashboard name) name)
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
  (insert (deterred-format (f-h1 "Actions") "\n"))
  (widget-create 'push-button
                 :notify (lambda (&rest _)
                           (deterred-dashboard-refresh))
                 "Refresh")
  (insert "\n\n"))

(defun deterred-dashboard--render-datasets (name data)
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

(defun deterred-dashboard--render-results (dashboard data)
  (insert
   (deterred-format
    (f-h1 "Results") "\n"))
  (deterred-dashboard-render-results dashboard data))

(defun deterred-dashboard-refresh ()
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
       deterred-dashboard-current data))))

(defun deterred-dashboard-maybe-init ()
  (unless deterred-dashboards
    (require 'deterred-dashboard-dummy)

    (setq deterred-dashboards
          (list (deterred-dashboard-dummy)))))

(defun deterred-dashboard-open (dashboard)
  "Open a DETERRED dashboard.

DASHBOARD is a dashboard object."
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
  (let* ((params (copy-tree
                  (deterred-dashboard-default-params dashboard)))
         (name (oref dashboard name))
         (buffer (generate-new-buffer (format "*DETERRED-dashboard-%s*" name)))
         (inhibit-read-only t))
    (with-current-buffer buffer
      (deterred-dashboard-mode)
      (setq-local deterred-dashboard-params params)
      (setq-local deterred-dashboard-current dashboard)
      (insert (deterred-format "Dashboard: " name "\n\n"))
      (when params
        (insert
         (deterred-format
          (f-h1 "* Parameters") "\n"))
        (deterred-dashboard-render-params dashboard))
      (deterred-dashboard--render-actions)
      (deterred-dashboard-refresh)
      (widget-setup)
      (goto-char (point-min)))
    (switch-to-buffer-other-window buffer)))

(cl-defmacro deterred-dashboard-widget-number (&key name key (size 20))
  (unless name
    (error "The `name' argument is required"))
  (unless key
    (error "The `key' argument is required"))
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
  (unless name
    (error "The `name' argument is required"))
  (unless key
    (error "The `key' argument is required"))
  `(let ((widget
          (widget-create
           'editable-field
           :size  ,size
           :format (deterred-format (f-ace (f ,name ": ") 'widget-button)
                                    "%v   ")
           :value (let ((val (alist-get ,key deterred-dashboard-params)))
                    (when (numberp val)
                      (format-time-string "%Y-%m-%d" val)))
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

(defun deterred-dashboard-print-error (desc err output)
  (insert
   (deterred-format
    (f-ace desc 'error) "\n"
    "Error: " (prin1-to-string err) "\n"
    output
    "\n\n")))

(defun deterred-dashboard-print-images-base64 (images &rest props)
  (unless (sequencep images)
    (setq images (list images)))
  (insert
   (mapconcat
    (lambda (image)
      (condition-case-unless-debug err
          (let* ((data (base64-decode-string image))
                 (img (apply #'create-image
                             data nil t props)))
            (if (image-type-available-p (image-property img :type))
                (propertize "[IMG]" 'display img)
              "[IMG]"))
        (error (deterred-format
                (f-ace "Error: " 'error)
                (prin1-to-string err)))))
    images
    "\n")))

(cl-defun deterred-dashboard-exec-python
    (&key python-code python-file
          input (on-error #'deterred-dashboard-print-error)
          on-success)
  (unless (or python-code python-file)
    (error "Set either `python-code' or `python-file'"))
  (let ((args
         `(,@(when python-code
               `("-c" ,python-code))
           ,@(when python-file
               `(,python-file))))
        output parsed-output)
    (condition-case err
        (with-temp-buffer
          (insert (json-encode input))
          (apply  #'call-process-region (point-min) (point-max)
                  deterred-dashboard-python t t nil args)
          (goto-char (point-min))
          (setq output (buffer-string))
          (setq parsed-output (json-read)))
      (error
       (funcall on-error "Python execution error" err output)))
    (when (and parsed-output on-success)
      (funcall on-success parsed-output))))

(provide 'deterred-dashboard)
;;; deterred-dashboard.el ends here

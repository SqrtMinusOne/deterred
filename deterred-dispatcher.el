;;; deterred-dispatcher.el --- TODO -*- lexical-binding: t -*-

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
(require 'deterred-db)
(require 'deterred-faces)
(require 'deterred-source)
(require 'deterred-utils)
(require 'transient)
(require 'magit-section)

(defconst deterred-dispatcher-buffer-name "*DETERRED*"
  "Default buffer name for org-journal-tags status.")

(defcustom deterred-dispatcher-date-format "%A, %Y-%m-%d"
  "Format string for date entries."
  :group 'deterred
  :type '(choice
          (string :tag "String")
          (function :tag "Function")))

(defcustom deterred-dispatcher-short-date-format "%Y-%m-%d"
  "Format string for date entries.  Should always be the same length."
  :group 'deterred
  :type '(choice
          (string :tag "String")
          (function :tag "Function")))

(defcustom deterred-dispatcher-time-format "%H:%M"
  "Format string for time entries."
  :group 'deterred
  :type '(choice
          (string :tag "String")
          (function :tag "Function")))

(defun deterred-dispatcher--magit-section-toggle-workaround (section)
  "`magit-section-toggle' with a workaround for invisible lines.

SECTION is an instance of `magit-section'.

No idea what I'm doing wrong, but this seems to help."
  (interactive (list (save-excursion
                       (let ((lines (count-lines (point-min) (point-max))))
                         (while (and (invisible-p (point))
                                     (< (line-number-at-pos) lines))
                           (forward-line 1)))
                       (magit-current-section))))
  (magit-section-toggle section))

(defvar deterred-dispatcher-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "RET") #'widget-button-press)
    (define-key map (kbd "r") #'deterred-dispatcher-refresh)
    (define-key map (kbd "q") (lambda ()
                                (interactive)
                                (quit-window t)))
    (when (fboundp #'evil-define-key*)
      (evil-define-key* '(normal motion) map
        (kbd "<tab>") #'deterred-dispatcher--magit-section-toggle-workaround
        (kbd "<RET>") #'widget-button-press
        (kbd "r") #'deterred-dispatcher-refresh
        "q" (lambda ()
              (interactive)
              (quit-window t))))
    map)
  "A keymap for `org-journal-tags-status-mode'.")

(define-derived-mode deterred-dispatcher-mode magit-section "DETERRED"
  :group 'deterred
  (setq-local buffer-read-only t))

(defun deterred-dispatcher--render-sources ()
  "Renderer `deterred-sources' for `deterred-dispatcher'."
  (magit-insert-section (deterred-info-sources)
    (insert (propertize
             (format "Active sources: %s" (length deterred-sources))
             'face 'deterred-faces-section-heading-1))
    (magit-insert-heading)
    (let ((max-name-length
           (seq-max (append
                     (mapcar (lambda (s) (length (oref s name)))
                             deterred-sources)
                     '(0)))))
      (dolist (source deterred-sources)
        (let* ((range (deterred-source-range source))
               (can-sync (deterred-source-sync-p source))
               (can-action (deterred-source-actions-p source))
               (warn-days (oref source warn-days))
               (unsynced-days (floor
                               (/ (float (- (time-convert nil #'integer)
                                            (cdr range)))
                                  (* 60 60 24)))))
          (insert
           (format "%s  %s - %s"
                   (propertize
                    (string-pad (oref source name) max-name-length)
                    'face 'deterred-faces-source-name)
                   (propertize
                    (format-time-string deterred-dispatcher-short-date-format
                                        (car range))
                    'face 'deterred-faces-date)
                   (propertize
                    (format-time-string deterred-dispatcher-short-date-format
                                        (cdr range))
                    'face (if (or (null warn-days)
                                  (> warn-days unsynced-days))
                              'deterred-faces-date
                            'warning))))
          (when can-sync
            (insert " ")
            (widget-create 'push-button
                           :notify (lambda (&rest _)
                                     (deterred-source-sync
                                      source
                                      (lambda ()
                                        (message "Sync done: %s"
                                                 (oref source name))
                                        (deterred-dispatcher-refresh))))
                           "[Sync]"))
          (when can-action
            (insert " ")
            (widget-create 'push-button
                           :notify (lambda (&rest _)
                                     (deterred-source-actions
                                      source
                                      (lambda ()
                                        (message "Action done: %s"
                                                 (oref source name))
                                        (deterred-dispatcher-refresh))))
                           "[Actions...]"))
          (insert "\n"))))))

(defun deterred-dispatcher--on-this-day-data (&optional db)
  (let ((db (or db (deterred-db--init)))
        res)
    (mapcar
     (lambda (source)
       (let* ((range (deterred-source-range source db))
              (days-data (deterred-utils-get-this-day (car range)))
              (source-name (oref source name)))
         (cl-loop
          for (description . timestamp) in days-data
          for value = (condition-case-unless-debug err
                          (deterred-source-day-summary source timestamp db)
                        (error `((:short-description
                                  . ,(propertize (error-message-string err)
                                                 'face 'error)))))
          when value
          do (progn
               (setf (alist-get :description (alist-get timestamp res))
                     description)
               (setf (alist-get :source value) source)
               (setf (alist-get
                      source-name
                      (alist-get timestamp res)
                      nil nil #'equal)
                     value)))))
     deterred-sources)
    (setq res (seq-sort-by #'car '> res))
    res))

(defun deterred-dispatcher--day-data (&optional db timestamp)
  (let ((db (or db (deterred-db--init)))
        datum)
    (mapcar
     (lambda (source)
       (let ((value (condition-case-unless-debug err
                        (deterred-source-day-summary source timestamp db)
                      (error `((:short-description
                                . ,(propertize (error-message-string err)
                                               'face 'error))))))
             (source-name (oref source name)))
         (when value
           (setf (alist-get :source value) source)
           (setf (alist-get source-name datum nil nil #'equal) value))))
     deterred-sources)
    (setf (alist-get :description datum)
          (format-time-string deterred-dispatcher-date-format timestamp))
    datum))

(defun deterred-dispatcher--render-timestamp (timestamp datum)
  (magit-insert-section (deterred-dispatcher-on-this-day-day timestamp nil)
    (insert (propertize
             (format "%s, %s"
                     (alist-get :description datum)
                     (format-time-string deterred-dispatcher-date-format timestamp))
             'face 'deterred-faces-section-heading-2))
    (magit-insert-heading)
    (cl-loop
     for (source-name . item) in datum
     unless (symbolp source-name)
     do (magit-insert-section (deterred-dispatcher-on-this-day-item timestamp t)
          (insert (format "%s: %s"
                          (propertize source-name
                                      'face 'deterred-faces-source-name)
                          (alist-get :short-description item)))
          (magit-insert-heading)
          (when-let (long-description (alist-get :long-description item))
            (insert long-description "\n"))
          (when-let (long-description-fn
                     (alist-get :long-description-fn item))
            (condition-case-unless-debug err
                (funcall long-description-fn item)
              (error (insert (propertize
                              (concat "Render error: " (error-message-string err))
                              'face 'error) "\n"))))))
    (insert "\n")))

(defun deterred-dispatcher-day (timestamp)
  (interactive (list (time-convert (org-read-date nil t) #'integer)))
  (let ((datum (deterred-dispatcher--day-data nil timestamp))
        (buffer-name (format "*DETERRED-<%s>*" (format-time-string "%F" timestamp))))
    (when-let ((buffer (get-buffer buffer-name)))
      (kill-buffer buffer))
    (let ((buffer (get-buffer-create buffer-name)))
      (switch-to-buffer-other-window buffer)
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (setq-local widget-push-button-prefix "")
          (setq-local widget-push-button-suffix "")
          (unless (derived-mode-p #'deterred-dispatcher-mode)
            (deterred-dispatcher-mode))
          (magit-insert-section (deterred-info)
            (deterred-dispatcher--render-timestamp timestamp datum)
            (let ((magit-section-cache-visibility nil))
              (magit-section-show magit-root-section))))))))

(defun deterred-dispatcher--render-on-this-day (&optional db)
  (let* ((db (or db (deterred-db--init)))
         (data (deterred-dispatcher--on-this-day-data db)))
    (magit-insert-section (deterred-dispatcher-on-this-day)
      (insert (propertize "On this day" 'face 'deterred-faces-section-heading-1))
      (magit-insert-heading)
      (cl-loop
       for (timestamp . datum) in data
       do (deterred-dispatcher--render-timestamp timestamp datum)))))

(defun deterred-dispatcher--render-contents ()
  "Render DETERRED dispatcher."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq-local widget-push-button-prefix "")
    (setq-local widget-push-button-suffix "")
    (unless (derived-mode-p #'deterred-dispatcher-mode)
      (deterred-dispatcher-mode))
    (magit-insert-section (deterred-info)
      (magit-insert-section (deterred-info-summary nil nil)
        (insert (format "Date:          %s\n"
                        (propertize (format-time-string deterred-dispatcher-date-format)
                                    'face 'deterred-faces-date)))
        (magit-insert-heading)
        (insert "HELLO\nCampsite Gaia"))
      (insert "\n\n")
      (deterred-dispatcher--render-sources)
      (insert "\n\n")
      (deterred-dispatcher--render-on-this-day)
      (let ((magit-section-cache-visibility nil))
        (magit-section-show magit-root-section))))
  (goto-char (point-min)))

(defun deterred-dispatcher-refresh ()
  "Refresh DETERRED dispatcher."
  (interactive)
  (unless (derived-mode-p #'deterred-dispatcher-mode)
    (user-error "Not in `deterred-dispatcher' mode!"))
  (deterred-dispatcher--render-contents))

(defun deterred-dispatcher ()
  "Open DETERRED interactive buffer."
  (interactive)
  (when-let ((buffer (get-buffer deterred-dispatcher-buffer-name)))
    (kill-buffer buffer))
  (let ((buffer (get-buffer-create deterred-dispatcher-buffer-name)))
    (switch-to-buffer-other-window buffer)
    (with-current-buffer buffer
      (deterred-dispatcher--render-contents))))

(provide 'deterred-dispatcher)
;;; deterred-dispatcher.el ends here

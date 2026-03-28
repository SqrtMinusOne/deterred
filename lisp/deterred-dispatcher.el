;;; deterred-dispatcher.el --- Dispatcher UI for DETERRED. -*- lexical-binding: t -*-

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
(require 'transient)
(require 'magit-section)
(require 'org)

(require 'deterred-db)
(require 'deterred-sync)
(require 'deterred-dashboard)
(require 'deterred-timeline)
(require 'deterred-faces)
(require 'deterred-source)
(require 'deterred-utils)
(require 'deterred-backup)

(declare-function evil-define-key* "evil-core")

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

(defcustom deterred-dispatcher-date-time-format "%A, %Y-%m-%d %H:%M"
  "Format string for date+time entries."
  :type '(choice
          (string :tag "String")
          (function :tag "Function")))

(defcustom deterred-dispatcher-short-date-time-format "%Y-%m-%d %H:%M"
  "Format string for date+time entries.  Should always be the same length."
  :type '(choice
          (string :tag "String")
          (function :tag "Function")))

(defcustom deterred-dispatcher-startup-hook nil
  "Run on the first invocation of the DETERRED dispatcher."
  :group 'deterred
  :type 'hook)

(defvar deterred-dispatcher-startup-p nil
  "If non-nil, the DETERRED dispatched has been invoked.")

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
  "A keymap for `deterred-dispatcher-mode'.")

(defvar deterred-dispatcher--mode nil
  "Either nil, day-summary or range-summary.")

(define-derived-mode deterred-dispatcher-mode magit-section "DETERRED"
  :group 'deterred
  (setq-local buffer-read-only t))

(defun deterred-dispatcher--render-source-line (source max-name-length)
  "Render main line for SOURCE with MAX-NAME-LENGTH padding."
  (let* ((range (deterred-source-range source))
         (can-sync (deterred-source-sync-p source))
         (can-action (deterred-source-actions-p source))
         (warn-days (oref source warn-days))
         (unsynced-days (floor
                         (/ (float (- (time-convert nil 'integer)
                                      (or (cdr range) 0)))
                            (* 60 60 24)))))
    (insert
     (format "%s  %s - %s"
             (propertize
              (string-pad (oref source name) max-name-length)
              'face 'deterred-faces-source-name)
             (if (car range)
                 (propertize
                  (format-time-string deterred-dispatcher-short-date-format
                                      (car range))
                  'face 'deterred-faces-date)
               (propertize "(empty)   " 'face 'deterred-faces-info))
             (if (cdr range)
                 (propertize
                  (format-time-string deterred-dispatcher-short-date-format
                                      (cdr range))
                  'face (if (or (null warn-days)
                                (> warn-days unsynced-days))
                            'deterred-faces-date
                          'warning))
               (propertize "(empty)   " 'face 'deterred-faces-info))))
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
                     "[Actions...]"))))

(defun deterred-dispatcher--render-range-detail (range-detail max-name-length)
  "Render RANGE-DETAIL list as indented lines.

MAX-NAME-LENGTH is used to pad the names for alignment."
  (dolist (detail range-detail)
    (insert
     (format "  %s  %s - %s\n"
             (propertize
              (string-pad (alist-get :name detail) (- max-name-length 2))
              'face 'deterred-faces-info)
             (if (alist-get :start detail)
                 (propertize
                  (format-time-string deterred-dispatcher-short-date-format
                                      (alist-get :start detail))
                  'face 'deterred-faces-date)
               (propertize "(empty)   " 'face 'deterred-faces-info))
             (if (alist-get :end detail)
                 (propertize
                  (format-time-string deterred-dispatcher-short-date-format
                                      (alist-get :end detail))
                  'face 'deterred-faces-date)
               (propertize "(empty)   " 'face 'deterred-faces-info))))))

(defun deterred-dispatcher--render-sources ()
  "Render `deterred-sources' for `deterred-dispatcher'."
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
        (let ((range-detail (deterred-source-range-detail source)))
          (if range-detail
              (magit-insert-section (deterred-source-detail source t)
                (deterred-dispatcher--render-source-line source max-name-length)
                (magit-insert-heading)
                (deterred-dispatcher--render-range-detail range-detail max-name-length))
            (deterred-dispatcher--render-source-line source max-name-length)
            (insert "\n")))))))

(defun deterred-dispatcher--render-sync-state ()
  "Render sync state section for `deterred-dispatcher'."
  (magit-insert-section (deterred-info-sync-state)
    (insert (propertize "Sync state" 'face 'deterred-faces-section-heading-1))
    (magit-insert-heading)
    (let* ((state (deterred-sync-state))
           (max-hostname-length
            (seq-max (append
                      (mapcar (lambda (entry)
                                (+ (length (alist-get 'hostname entry))
                                   (if (alist-get 'current entry) 10 0)))
                              state)
                      '(0))))
           (date-format-length (length (format-time-string
                                        deterred-dispatcher-short-date-format)))
           (max-db-length (max date-format-length 3))
           (max-file-length (max date-format-length 3))
           (max-status-length 9))
      (dolist (entry state)
        (let* ((hostname (alist-get 'hostname entry))
               (is-current (alist-get 'current entry))
               (db-time (alist-get 'db-time entry))
               (file-time (alist-get 'file-time entry))
               (action-info (deterred-sync-get-action entry))
               (action-state (alist-get 'state action-info))
               (action-fn (alist-get 'action action-info))
               (hostname-str hostname)
               (db-str (if db-time
                           (format-time-string deterred-dispatcher-short-date-format db-time)
                         "N/A"))
               (file-str (if file-time
                             (format-time-string deterred-dispatcher-short-date-format file-time)
                           "N/A"))
               (status-str (pcase action-state
                             ('ok "[OK]")
                             ('pending "[PENDING]")
                             ('error "[ERROR]")
                             (_ "[UNKNOWN]"))))
          (insert
           (deterred-format
            (string-pad
             (f
              (f-ace hostname-str 'deterred-faces-source-name)
              (when is-current
                (f (f-ace " (current)" 'bold))))
             max-hostname-length)
            "  DB: "
            (f-ace (string-pad db-str max-db-length)
                   (if db-time 'deterred-faces-date 'shadow))
            "  File: "
            (f-ace (string-pad file-str max-file-length)
                   (if file-time 'deterred-faces-date 'shadow))
            "  "
            (f-ace (string-pad status-str max-status-length)
                   (pcase action-state
                     ('ok 'success)
                     ('pending 'warning)
                     ('error 'error)
                     (_ 'shadow)))))
          (when (and action-fn (memq action-state '(pending error)))
            (insert " ")
            (widget-create 'push-button
                           :notify (lambda (&rest _)
                                     (condition-case-unless-debug err
                                         (progn
                                           (funcall action-fn)
                                           (deterred-dispatcher-refresh))
                                       (error (message "Sync action error: %s"
                                                       (error-message-string err)))))
                           "[Execute]"))
          (insert "\n"))))))

(defun deterred-dispatcher--on-this-day-data (&optional db)
  "Render the \"On this day\" section for DETERRED dispatcher.

DB is a SQLite connection object.

This interates overs sources in `deterred-sources' and invokes
`deterred-source-day-summary' on each.

Returns the following nested alists structure:
- Timestamp
  - Source name (as given by the `name' slot)
    - `:source' - instance of datasource
    - all keys of `deterred-source-day-summary'."
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
  "Return data for a particular TIMESTAMP.

DB is a SQLite connection object.

The return value is the same as in
`deterred-dispatcher--on-this-day-data', but only the value for the
given timestamp."
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
    (nreverse datum)))

(defun deterred-dispatcher--render-items (datum section-type section-value)
  "Render items from DATUM.

DATUM is an alist with source names as keys.
SECTION-TYPE is the magit-section type for each item.
SECTION-VALUE is the value to pass to magit-insert-section."
  (cl-loop
   for (source-name . item) in datum
   unless (symbolp source-name)
   do (magit-insert-section (section-type section-value t)
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
                            'face 'error) "\n")))))))

(defun deterred-dispatcher--render-timestamp (timestamp datum)
  "Render one DATUM for TIMESTAMP.

DATUM is as returned by `deterred-dispatcher--on-this-day-data'."
  (magit-insert-section (deterred-dispatcher-on-this-day-day
                         (cons
                          timestamp
                          (+ (* 60 60 24) timestamp))
                         nil)
    (insert (propertize
             (format "%s, %s"
                     (alist-get :description datum)
                     (format-time-string deterred-dispatcher-date-format timestamp))
             'face 'deterred-faces-section-heading-2))
    (magit-insert-heading)
    (deterred-dispatcher--render-items
     datum
     'deterred-dispatcher-on-this-day-item
     (cons timestamp (+ (* 60 60 24) timestamp)))
    (insert "\n")))

(defun deterred-dispatcher-day (timestamp)
  "Get summary for one particular day.

TIMESTAMP is a UNIX timestamp."
  (interactive (list (deterred-utils-ts-to-day-start
                      (time-convert (org-read-date nil t) 'integer))))
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

(defun deterred-dispatcher--range-data (start-timestamp end-timestamp &optional db)
  "Return data for a date range.

START-TIMESTAMP and END-TIMESTAMP are UNIX timestamps.
DB is a SQLite connection object.

Returns an alist with keys:
- `:description' - formatted description of the range
- Source names (strings) - each containing the result of
  `deterred-source-range-summary' plus `:source' key."
  (let ((db (or db (deterred-db--init)))
        datum)
    (mapcar
     (lambda (source)
       (let ((value (condition-case-unless-debug err
                        (deterred-source-range-summary source start-timestamp end-timestamp db)
                      (error `((:short-description
                                . ,(propertize (error-message-string err)
                                               'face 'error))))))
             (source-name (oref source name)))
         (when value
           (setf (alist-get :source value) source)
           (setf (alist-get source-name datum nil nil #'equal) value))))
     deterred-sources)
    (setf (alist-get :description datum)
          (format "%s - %s"
                  (format-time-string deterred-dispatcher-date-format start-timestamp)
                  (format-time-string deterred-dispatcher-date-format end-timestamp)))
    (nreverse datum)))

(defun deterred-dispatcher--range-dashboards ()
  "Return dashboards that support both `:start-date' and `:end-date'."
  (deterred-dashboard-maybe-init)
  (seq-sort-by
   (lambda (dashboard) (oref dashboard name))
   #'string-lessp
   (seq-filter
    (lambda (dashboard)
      (let ((params (deterred-dashboard-default-params dashboard)))
        (and (assoc :start-date params)
             (assoc :end-date params))))
    deterred-dashboards)))

(defun deterred-dispatcher--render-range-dashboards (start-timestamp end-timestamp)
  "Render dashboard links for [START-TIMESTAMP, END-TIMESTAMP]."
  (when-let ((dashboards (deterred-dispatcher--range-dashboards)))
    (magit-insert-section (deterred-dispatcher-range-dashboards
                           (cons start-timestamp end-timestamp)
                           t)
      (insert (propertize "Dashboards" 'face 'deterred-faces-section-heading-3))
      (magit-insert-heading)
      (dolist (dashboard dashboards)
        (insert "- ")
        (widget-create
         'push-button
         :notify (lambda (&rest _)
                   (deterred-dashboard-open
                    dashboard
                    `((:start-date . ,start-timestamp)
                      (:end-date . ,end-timestamp))))
         (oref dashboard name))
        (insert "\n"))
      (insert "\n"))))

(defun deterred-dispatcher--render-range (start-timestamp end-timestamp datum)
  "Render DATUM for a date range.

START-TIMESTAMP and END-TIMESTAMP are UNIX timestamps.
DATUM is as returned by `deterred-dispatcher--range-data'."
  (magit-insert-section (deterred-dispatcher-range
                         (cons start-timestamp end-timestamp)
                         nil)
    (insert (propertize
             (alist-get :description datum)
             'face 'deterred-faces-section-heading-2))
    (magit-insert-heading)
    (deterred-dispatcher--render-range-dashboards
     start-timestamp end-timestamp)
    (insert "\n")
    (deterred-dispatcher--render-items
     datum
     'deterred-dispatcher-range-item
     (cons start-timestamp end-timestamp))
    (insert "\n")))

(defun deterred-dispatcher-range (start-timestamp end-timestamp)
  "Get summary for a date range.

START-TIMESTAMP and END-TIMESTAMP are UNIX timestamps."
  (interactive
   (list (deterred-utils-ts-to-day-start
          (time-convert (org-read-date nil t nil "Start date: ") 'integer))
         (deterred-utils-ts-to-day-end
          (time-convert (org-read-date nil t nil "End date: ") 'integer))))
  (when (> start-timestamp end-timestamp)
    (user-error "Start date must be before or equal to end date"))
  (let ((datum (deterred-dispatcher--range-data start-timestamp end-timestamp))
        (buffer-name (format "*DETERRED-<%s to %s>*"
                             (format-time-string "%F" start-timestamp)
                             (format-time-string "%F" end-timestamp))))
    (when-let ((buffer (get-buffer buffer-name)))
      (kill-buffer buffer))
    (let ((buffer (get-buffer-create buffer-name)))
      (switch-to-buffer-other-window buffer)
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (setq-local widget-push-button-prefix "")
          (setq-local widget-push-button-suffix "")
          (setq-local deterred-dispatcher--mode 'range-summary)
          (unless (derived-mode-p #'deterred-dispatcher-mode)
            (deterred-dispatcher-mode))
          (magit-insert-section (deterred-info)
            (deterred-dispatcher--render-range start-timestamp end-timestamp datum)
            (let ((magit-section-cache-visibility nil))
              (magit-section-show magit-root-section)))
          (widget-setup))))))

(defun deterred-dispatcher--render-on-this-day (&optional db)
  "Render the \"On this day\" section for DETERRED.

DB is the SQLite connection object."
  (let* ((db (or db (deterred-db--init)))
         (data (deterred-dispatcher--on-this-day-data db)))
    (magit-insert-section (deterred-dispatcher-on-this-day)
      (insert (propertize "On this day" 'face 'deterred-faces-section-heading-1))
      (magit-insert-heading)
      (cl-loop
       for (timestamp . datum) in data
       do (deterred-dispatcher--render-timestamp timestamp (reverse datum))))))

(defun deterred-dispatcher--render-actions ()
  "Render the \"Actions\" section for DETERRED."
  (widget-create 'push-button
                 :notify (lambda (&rest _)
                           (call-interactively #'deterred-dashboard-open))
                 "[Dashboards...]")
  (insert " ")
  (widget-create 'push-button
                 :notify (lambda (&rest _)
                           (call-interactively #'deterred-dispatcher-day))
                 "[View day...]")
  (insert " ")
  (widget-create 'push-button
                 :notify (lambda (&rest _)
                           (call-interactively #'deterred-dispatcher-range))
                 "[View range...]")
  (insert " ")
  (widget-create 'push-button
                 :notify (lambda (&rest _)
                           (call-interactively #'deterred-timeline))
                 "[Timeline]")
  (insert " ")
  (widget-create 'push-button
                 :notify (lambda (&rest _)
                           (deterred-backup))
                 (propertize "[Backup]" 'face
                             (when (deterred-backups-need-p) 'bold))))

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
        (insert (deterred-format
                 "Hostname:      "
                 (system-name) "\n")))
      (insert "\n")
      (deterred-dispatcher--render-actions)
      (insert "\n\n")
      (deterred-dispatcher--render-sources)
      (insert "\n\n")
      (deterred-dispatcher--render-sync-state)
      (insert "\n\n")
      (deterred-dispatcher--render-on-this-day)
      (let ((magit-section-cache-visibility nil))
        (magit-section-show magit-root-section)))
    (widget-setup))
  (goto-char (point-min)))

(defun deterred-dispatcher-refresh ()
  "Refresh DETERRED dispatcher."
  (interactive)
  (unless (derived-mode-p #'deterred-dispatcher-mode)
    (user-error "Not in `deterred-dispatcher' mode!"))
  (deterred-dispatcher--render-contents))

;;;###autoload
(defun deterred-dispatcher ()
  "Open DETERRED interactive buffer."
  (interactive)
  (when-let ((buffer (get-buffer deterred-dispatcher-buffer-name)))
    (kill-buffer buffer))
  (let ((buffer (get-buffer-create deterred-dispatcher-buffer-name)))
    (switch-to-buffer-other-window buffer)
    (with-current-buffer buffer
      (deterred-dispatcher--render-contents)
      (unless deterred-dispatcher-startup-p
        (run-hooks 'deterred-dispatcher-startup-hook)
        (setq deterred-dispatcher-startup-p t)))))

(provide 'deterred-dispatcher)
;;; deterred-dispatcher.el ends here

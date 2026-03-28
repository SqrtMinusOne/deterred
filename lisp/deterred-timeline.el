;; deterred-timeline.el --- Timeline UI for DETERRED. -*- lexical-binding: t -*-

;; Copyright (C) 2026 Korytov Pavel

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
(require 'ct)
(require 'magit-section)

(require 'deterred-chains)
(require 'deterred-dashboard)
(require 'deterred-source)

(declare-function evil-define-key* "evil-core")

(defconst deterred-timeline-buffer-name "*DETERRED Timeline*"
  "Default buffer name for DETERRED timeline.")

(defconst deterred-timeline-max-length 400
  "Maximum timeline length.")

(defcustom deterred-timeline-symbol "■"
  "A symbol to render timelines."
  :group 'deterred
  :type 'string)

(defcustom deterred-timeline-max-prefix-length 20
  "Maximum prefix length for timelines."
  :group 'deterred
  :type 'integer)

(defvar-local deterred-timeline-params nil
  "Current parameters for the timeline view.")
(defvar-local deterred-timeline-sources nil
  "List of active sources for timeline.")
(defvar-local deterred-timeline-sources-order nil
  "Hash map with sources order for the timeline buffer.")
(defvar-local deterred-timeline-data nil
  "Current timeline results.")
(defvar-local deterred-timeline-timestamp nil
  "Current timeline timestamps.")
(defvar-local deterred-timeline-results-loc nil
  "Location markers with the result section boundaries.")

(defun deterred-timeline--source (source start end interval-kind &optional params db)
  "Get data from SOURCE for timeline.

SOURCE is as instance of `deterred-source'.  START and END are UNIX
timestamps.  INTERVAL-KIND is a key from `deterred-intervals' or an
instance of `deterred-interval'.  PARAMS are passed to
`deterred-source-events' (the same parameters that are used by the
source's dashboard).  DB is a SQLite connection instance.

The return value is a cons cell:
- The car is a discretized events chain;
- The cdr is an alist with discretized chains of grouped events, where
  cars are group names, and cdr are the chains themselves.
See `deterred-chains-discretize' for more."
  (when-let ((events-chain (deterred-source-events source start end params db)))
    (let* ((group-chains (deterred-source-events-group source events-chain))
           (timestamps (deterred-intervals-generate start end interval-kind))
           (events-d (deterred-chains-discretize events-chain timestamps))
           (groups-d
            (seq-sort-by
             (lambda (group-d)
               (or (cl-loop for i from 0
                            for elem in (cdr group-d)
                            if (> (nth 3 elem) 0)
                            return i)
                   1.0e+INF))
             #'<
             (mapcar (lambda (chain)
                       (cons (car chain)
                             (deterred-chains-discretize (cdr chain) timestamps)))
                     group-chains))))
      (cons events-d groups-d))))

(defun deterred-timeline--blend-colors (c1 c2 val)
  "Blend colors C1 and C2 by VAL.

C1 and C2 are hex RGS strings, VAL is a number between 0 and 1."
  (let ((color1 (ct-get-rgb c1))
        (color2 (ct-get-rgb c2)))
    (apply #'ct-make-rgb
           (cl-loop for v1 in color1
                    for v2 in color2
                    collect (+ (* (- 1 val) v1) (* val v2))))))

(defun deterred-timeline--make-elem (base-color coef &optional background)
  "Make a string for one timeline element.

BASE-COLOR is the color for COEF = 1, BACKGROUND is background color
from which elements with COEF = 0 should be indistinguishable."
  (if (> coef 0)
      (progn
        (unless background
          (setq background (or (face-background 'default nil t) "#ffffff")))
        (propertize deterred-timeline-symbol
                    'face `(:foreground
                            ,(deterred-timeline--blend-colors
                              background base-color coef))))
    " "))

(defun deterred-timeline--make-chain (chain-d base-color &optional background)
  "Make a string for one timeline CHAIN-D.

CHAIN-D is a discretized chain (see `deterred-chains-discretize').
BASE-COLOR is the color for the elements with the largest
coefficient, BACKGROUND is the background color override."
  (unless background
    (setq background (or (face-background 'default nil t) "#ffffff")))
  (mapconcat
   (lambda (elem)
     (deterred-timeline--make-elem base-color (cadddr elem) background))
   chain-d))

(defun deterred-timeline--make-timestamps (timestamps)
  "Make a string with TIMESTAMPS."
  (let* ((need-date-p
          (not (string-equal (format-time-string "%F" (car timestamps))
                             (format-time-string "%F" (car (last timestamps))))))
         (width (+ (if need-date-p
                       (seq-length
                        (format-time-string deterred-dispatcher-short-date-time-format))
                     (seq-length
                      (format-time-string deterred-dispatcher-time-format)))
                   2)))
    (cl-loop
     for i from 0
     for timestamp in timestamps
     when (= (% i width) 0)
     concat (format "%s  " (if need-date-p
                               (format-time-string
                                deterred-dispatcher-short-date-time-format
                                timestamp)
                             (format-time-string
                              deterred-dispatcher-time-format timestamp))))))

(defun deterred-timeline--get-prefix-length (titles)
  "Return capped maximum length of TITLES.

Capped by `deterred-timeline-max-prefix-length'."
  (min (apply #'max (mapcar #'seq-length titles))
       deterred-timeline-max-prefix-length))

(defun deterred-timeline--adjust-string (string length)
  "Fit STRING in LENGTH."
  (format (format "%%-%ds" length) (truncate-string-to-width string length nil nil t)))

(defun deterred-timeline--render-source-data (source data &optional prefix-length)
  "Render DATA from SOURCE for timeline.

DATA is an output of `deterred-timeline--source'."
  (let ((events-d (car data))
        (groups-d (cdr data)))
    (unless prefix-length
      (setq prefix-length (deterred-timeline--get-prefix-length
                           (append (list (oref source name))
                                   (when groups-d (mapcar #'car groups-d))))))
    (let ((main-title (propertize
                       (deterred-timeline--adjust-string
                        (oref source name) prefix-length)
                       'face `(:foreground ,(deterred-source-color source))))
          (main-chain-string
           (deterred-timeline--make-chain events-d (deterred-source-color source))))
      (if (not groups-d)
          (insert (format "%s %s\n" main-title main-chain-string))
        (magit-insert-section (deterred-timeline-chain nil t)
          (insert (format "%s %s\n" main-title main-chain-string))
          (magit-insert-heading)
          (dolist (group-d groups-d)
            (insert
             (format "%s %s\n"
                     (deterred-timeline--adjust-string (car group-d) prefix-length)
                     (deterred-timeline--make-chain
                      (cdr group-d)
                      (deterred-source-color source (cdr group-d)))))))))))

(defun deterred-timeline--render-sources ()
  "Render the sources section fot the timeline."
  (magit-insert-section (deterred-timeline-sources)
    (insert (propertize "Sources\n" 'face 'deterred-faces-section-heading-1))
    (magit-insert-heading)
    (let ((max-name-length
           (seq-max (append
                     (mapcar (lambda (s) (length (oref s name)))
                             deterred-sources)
                     '(0))))
          (i 0))
      (setq deterred-timeline-sources-order (make-hash-table))
      (dolist (source deterred-sources)
        (let* ((range (deterred-source-range source))
               (warn-days (oref source warn-days))
               (unsynced-days (floor
                               (/ (float (- (time-convert nil 'integer)
                                            (or (cdr range) 0)))
                                  (* 60 60 24)))))
          (puthash source i deterred-timeline-sources-order)
          (cl-incf i)
          (insert " ")
          (widget-create
           'checkbox
           :value (member source deterred-timeline-sources)
           :notify
           (lambda (widget &rest _)
             (let ((val (widget-value widget)))
               (if val
                   (progn
                     (push source deterred-timeline-sources)
                     (setq deterred-timeline-sources
                           (seq-sort-by
                            (lambda (s) (gethash s deterred-timeline-sources-order))
                            #'<
                            deterred-timeline-sources)))
                 (setq deterred-timeline-sources
                       (delq source deterred-timeline-sources))))))
          (insert
           (format " %s  %s - %s\n"
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
                     (propertize "(empty)   " 'face 'deterred-faces-info)))))))))

(defun deterred-timeline--render-general-params ()
  "Render the general parameters section for the timeline."
  (magit-insert-section (deterred-timeline-general-params)
    (insert (propertize "General parameters" 'face 'deterred-faces-section-heading-1))
    (magit-insert-heading)
    (deterred-dashboard-widget-date
     :name "Start"
     :key :start
     :display-date t
     :place deterred-timeline-params)
    (insert "\n")
    (deterred-dashboard-widget-date
     :name "End"
     :key :end
     :display-date t
     :place deterred-timeline-params)
    (insert "\n")
    (deterred-dashboard-widget-completing-read
     :name "Interval"
     :key :interval
     :options (mapcar (lambda (i) (cons (oref (cdr i) display-name) (car i)))
                      deterred-intervals)
     :place deterred-timeline-params)
    (insert "\n")))

(defun deterred-timeline--source-params (source dashboard)
  "Return timeline parameters for SOURCE's DASHBOARD.

Initialize the source entry from
`deterred-dashboard-default-params' when needed."
  (let ((source-params-alist (alist-get :source-params deterred-timeline-params)))
    (if-let ((entry (assq source source-params-alist)))
        (cdr entry)
      (let ((params (copy-tree (deterred-dashboard-default-params dashboard))))
        (setf (alist-get source (alist-get :source-params deterred-timeline-params))
              params)))))

(defun deterred-timeline--set-source-params (source params)
  "Store timeline PARAMS for SOURCE."
  (setf (alist-get source (alist-get :source-params deterred-timeline-params))
        params))

(defun deterred-timeline--wrap-source-param-widget (widget source dashboard)
  "Make WIDGET read and write timeline params for SOURCE's DASHBOARD.

This is ugly but it works..."
  (when-let ((notify (widget-get widget :notify)))
    (widget-put
     widget :notify
     (lambda (&rest args)
       (let ((deterred-dashboard-params
              (deterred-timeline--source-params source dashboard)))
         (prog1
             (apply notify args)
           (deterred-timeline--set-source-params
            source deterred-dashboard-params)))))))

(defun deterred-timeline--render-source-params ()
  "Render the source parameters section for the timeline."
  (magit-insert-section (deterred-timeline-sources-params)
    (insert (propertize "Source parameters\n" 'face 'deterred-faces-section-heading-1))
    (magit-insert-heading)
    (dolist (source deterred-sources)
      (when-let ((dashboard (deterred-source-default-dashboard source)))
        (magit-insert-section (deterred-timeline-source-params source t)
          (insert
           (propertize (oref source name) 'face 'deterred-faces-section-heading-2)
           "\n")
          (magit-insert-heading)
          (let ((widget-create-function (symbol-function 'widget-create))
                widgets)
            (let ((deterred-dashboard-params
                   (deterred-timeline--source-params source dashboard)))
              (cl-letf (((symbol-function 'widget-create)
                         (lambda (&rest args)
                           (let ((widget (apply widget-create-function args)))
                             (push widget widgets)
                             widget))))
                (deterred-dashboard-render-params dashboard)))
            (dolist (widget widgets)
              (deterred-timeline--wrap-source-param-widget
               widget source dashboard)))
          (insert "\n"))))))

(defun deterred-timeline--render-controls ()
  "Render the controls section for timeline."
  (widget-create 'push-button
                 :notify (lambda (&rest _)
                           (deterred-timeline-execute))
                 "[Execute]")
  (insert "\n"))

(defvar deterred-timeline-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "RET") #'widget-button-press)
    (define-key map (kbd "q") (lambda ()
                                (interactive)
                                (quit-window t)))
    (when (fboundp #'evil-define-key*)
      (evil-define-key* '(normal motion) map
        (kbd "<tab>") #'deterred-dispatcher--magit-section-toggle-workaround
        (kbd "<RET>") #'widget-button-press
        "q" (lambda ()
              (interactive)
              (quit-window t))))
    map)
  "A keymap for `deterred-timeline-mode'.")

(define-derived-mode deterred-timeline-mode magit-section "DETERRED Timeline"
  :group 'deterred)

(defun deterred-timeline--render-results-init ()
  "Initialise the results section."
  (let ((start (point-marker)))
    (magit-insert-section (deterred-timeline-results)
      (insert (propertize "Results\n"
                          'face 'deterred-faces-section-heading-1))
      (magit-insert-heading)
      (insert (propertize "Configure parameters and run \"Execute\"."
                          'face 'deterred-faces-info)
              "\n"))
    (setq deterred-timeline-results-loc (cons start (point-marker)))))

(defun deterred-timeline--render ()
  "Render deterred timeline contents for the first time."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq-local widget-push-button-prefix "")
    (setq-local widget-push-button-suffix "")
    (unless (derived-mode-p #'deterred-dispatcher-mode)
      (deterred-timeline-mode))
    (magit-insert-section (deterred-timeline)
      (deterred-timeline--render-sources)
      (insert "\n")
      (deterred-timeline--render-general-params)
      (insert "\n")
      (deterred-timeline--render-source-params)
      (insert "\n")
      (deterred-timeline--render-controls)
      (insert "\n")
      (deterred-timeline--render-results-init)
      (insert "\n")
      (let ((magit-section-cache-visibility nil))
        (magit-section-show magit-root-section)))
    (widget-setup))
  (goto-char (point-min)))

(defun deterred-timeline--get-general-params ()
  "Get the general parameters and timestamps."
  (let ((start (alist-get :start deterred-timeline-params))
        (end (alist-get :end deterred-timeline-params))
        (interval (alist-get :interval deterred-timeline-params)))
    (unless (and start end interval)
      (user-error "Start, end and interval are required"))
    (let ((timestamps (deterred-intervals-generate start end interval)))
      (when (> (seq-length timestamps) deterred-timeline-max-length)
        (user-error "The timeline is too long (%s, max %s)"
                    (seq-length timestamps) deterred-timeline-max-length))
      (list start end interval timestamps))))

(defun deterred-timeline-execute ()
  "Execute and render the timeline."
  (interactive)
  (save-excursion
    (pcase-let ((`(,start ,end ,interval ,timestamps)
                 (deterred-timeline--get-general-params))
                (`(,region-start . ,region-end) deterred-timeline-results-loc)
                (db (deterred-db--init))
                (inhibit-read-only t))
      (setq deterred-timeline-data nil)
      (delete-region region-start region-end)
      (goto-char region-start)
      (magit-insert-section (deterred-timeline-results)
        (insert (propertize "Results\n"
                            'face 'deterred-faces-section-heading-1))
        (magit-insert-heading)
        (insert (propertize (string-pad "Timestamps" deterred-timeline-max-prefix-length)
                            'face 'deterred-faces-section-heading-2)
                (deterred-timeline--make-timestamps timestamps)
                "\n")
        (dolist (source deterred-timeline-sources)
          (when-let ((data
                      (deterred-timeline--source
                       source start end interval
                       (alist-get
                        source (alist-get :source-params deterred-timeline-params))
                       db)))
            (setf (alist-get source deterred-timeline-data) data)
            (deterred-timeline--render-source-data
             source data deterred-timeline-max-prefix-length)
            (insert "\n"))))
      (setq deterred-timeline-results-loc (cons region-start (point-marker))))
    (let ((magit-section-cache-visibility nil))
      (magit-section-show magit-root-section))))

;;;###autoload
(defun deterred-timeline ()
  "Open DETERRED timeline buffer."
  (interactive)
  (let ((buffer (get-buffer-create deterred-timeline-buffer-name)))
    (display-buffer-full-frame buffer nil)
    (with-current-buffer buffer
      ;; Copy list
      (setq-local deterred-timeline-sources `(,@deterred-sources))
      (deterred-timeline--render))))

(provide 'deterred-timeline)
;;; deterred-timeline.el ends here

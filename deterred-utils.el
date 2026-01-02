;;; deterred-utils.el --- Different utility functions for DETERRED -*- lexical-binding: t -*-

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

;; Different utility functions for DETERRED.

;;; Code:
(require 'pcsv)
(require 'seq)
(require 'validate)
(require 'backtrace)
(require 'request)

(require 'deterred-format)

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
            (shell-quote-argument (expand-file-name file))))
   :object-type 'alist))

(defun deterred-utils-ts-to-day-start (&optional timestamp)
  "Move TIMESTAMP to start of day."
  (let ((time (decode-time timestamp)))
    (setf (decoded-time-second time) 0
          (decoded-time-minute time) 0
          (decoded-time-hour time) 0)
    (time-convert (encode-time time) 'integer)))

(defun deterred-utils-ts-to-day-end (&optional timestamp)
  "Move TIMESTAMP to start of day."
  (let ((time (decode-time timestamp)))
    (setf (decoded-time-second time) 59
          (decoded-time-minute time) 59
          (decoded-time-hour time) 23)
    (time-convert (encode-time time) 'integer)))

(defun deterred-utils-get-this-day (start &optional today)
  "Get TODAY on all years since START.

E.g., this day one year ago, two years ago, etc.  START and TODAY are
time-values.

Return a list of cons cells, where the car is a human-readable value
and the cdr is the timestamp."
  (let ((time (decode-time today))
        (i 0)
        res)
    (setf (decoded-time-second time) 0
          (decoded-time-minute time) 0
          (decoded-time-hour time) 0)
    (while (time-less-p start (encode-time time))
      (setf (decoded-time-year time) (1- (decoded-time-year time)))
      (setq i (1+ i))
      (push (cons (format (if (= i 1) "%s year ago" "%s years ago") i)
                  (time-convert (encode-time time) 'integer))
            res))
    (nreverse res)))

(defun deterred-utils-read-date (val &optional kind)
  "Parse VAL into a timestamp, containing only date.

VAL accepts everything `org-read-date' does, e.g. YYYY-MM-DD, -D,
etc.

KIND can be 'from (then cast the date to day start), 'to (then cast
the date to day end), or nil."
  (if (string-empty-p val)
      nil
    (when-let* ((def (current-time))
                (def-decode (decode-time def))
                (date (ignore-errors
                        (org-read-date-analyze val def def-decode)))
                (timestamp (time-convert (encode-time date) 'integer)))
      (pcase kind
        ('from (deterred-utils-ts-to-day-start timestamp))
        ('to (deterred-utils-ts-to-day-end timestamp))
        (_ timestamp)))))

(defvar deterred-utils-report-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") (lambda ()
                                (interactive)
                                (quit-window t)))
    (when (fboundp #'evil-define-key*)
      (evil-define-key* '(normal motion) map
        "q" (lambda ()
              (interactive)
              (quit-window t))))
    map)
  "A keymap for `deterred-utils-report-mode'.")

(define-derived-mode deterred-utils-report-mode special-mode "DETERRED Report"
  :group 'deterred
  (setq-local buffer-read-only t)
  (outline-minor-mode 1))

(defun deterred-utils-validate (value schema &optional comment)
  "Validate VALUE against SCHEMA.

Pop an error in a new buffer if there's an error, otherwise return
VALUE.  Display COMMENT if passed."
  (let ((report (validate--check value schema)))
    (if report
        (let ((buf (generate-new-buffer "*deterred-error-report*")))
          (with-current-buffer buf
            (insert
             (deterred-format
              (f-h1 "Schema validation error")
              "\n\n"
              (f-h2 "Report")
              "\n" report
              "\n\n"
              (when comment
                (f (f-h2 "Comment")
                   "\n" comment "\n\n"))
              (f-h2 "Backtrace")
              "\n" (backtrace-to-string)))
            (goto-char (point-min))
            (deterred-utils-report-mode))
          (display-buffer buf)
          (user-error "Schema validation error"))
      value)))

(cl-defun deterred-utils-on-request-error (&key response &allow-other-keys)
  "Display error RESPONSE in a buffer."
  (let ((data (request-response-data response))
        (error-thrown (request-response-error-thrown response))
        (status-code (request-response-status-code response))
        (symbol-status (request-response-symbol-status response))
        (url (request-response-url response))
        (settings (request-response-settings response))
        (raw-headers (request-response--raw-header response))
        (raw-body (when (buffer-live-p (request-response--buffer response))
                    (with-current-buffer (request-response--buffer response)
                      (buffer-string)))))
    (if error-thrown
        (let ((buf (generate-new-buffer "*deterred-error-report*")))
          (with-current-buffer buf
            (insert
             (deterred-format
              (f-h1 "Request error") "\n"
              "Request has returned status " (f-ace (f-num (or status-code -1)) 'error)
              " " (prin1-to-string error-thrown) "\n"
              "URL: " (f-ace url 'link) "\n\n"
              (f-h2 "Response Body") "\n"
              (or raw-body (prin1-to-string data) "(no body available)")
              "\n\n"
              (f-h2 "Raw Headers") "\n"
              (or raw-headers "(not available)")
              "\n\n"
              (f-h2 "Request Headers") "\n"
              (mapconcat (lambda (h) (format "%s: %s" (car h) (cdr h)))
                         (plist-get settings :headers) "\n")
              "\n\n"
              (f-h2 "Params") "\n"
              (prin1-to-string (plist-get settings :params))))
            (goto-char (point-min))
            (deterred-utils-report-mode))
          (display-buffer buf)))))

(defmacro deterred-utils-assert-var-set (var-name)
  "Signal error is VAR-NAME is nil."
  `(unless ,var-name
     (user-error ,(format "%s not set!" var-name))))

(defmacro deterred-utils-make-alist (&rest vars)
  "Make an alist from VARS.

VARS is a list of symbols."
  `(list
    ,@(mapcar
       (lambda (var)
         `(cons ',var ,var))
       vars)))

(defun deterred-utils-add-fraction (data total &optional key)
  "Add a fraction column to DATA.

DATA is a list of alists, which has to contain the KEY column, which
is \"hours\" by default.  TOTAL is the sum of all values."
  (mapcar (lambda (datum)
            (append
             datum
             (list
              (cons 'fraction
                    (format "%.2f%%"
                            (* 100.0 (/ (float (alist-get (or key 'hours) datum))
                                        total)))))))
          data))

(defun deterred-utils-merge-hashes (target source)
  "Merge hash table SOURCE into TARGET.

TARGET and SOURCE are hash tables.  All key-value pairs from SOURCE
are added to TARGET, overwriting existing keys if present."
  (maphash (lambda (k v)
             (puthash k v target))
           source))

(defun deterred-utils-pick-list (list keys)
  "Leave only elements with `car' in KEYS in a LIST of alists."
  (mapcar
   (lambda (item)
     (seq-filter
      (lambda (elem)
        (member (car elem) keys))
      item))
   list))

(defun deterred-utils-normalize-by-timeout (chains timeout)
  "Normalize CHAINS of timestamps by TIMEOUT.

A chain is a either a list of UNIX timestamps, or a list of cons cells
with UNIX timestamps, where `car' is the start and `cdr' is the end of
a timespan.

TIMEOUT is a number of seconds.

The function returns chains in the same order, converted to timespans
\(cons cells\), with gaps less than TIMEOUT removed.  This is similar to
what WakaTime does by converting a list of individual \"heartbeats\"
into timestamps."
  (let (series
        normalized-series
        current-item)
    ;; Merge chain into one series
    ;; A series is a list of lists (chain-id, start, end)
    (cl-loop for i from 0
             for chain in chains
             do (cl-loop for value in chain
                         do (if (consp value)
                                (push (list i (car value) (cdr value)) series)
                              (push (list i value value) series))))
    ;; Normalize series
    (dolist (item (append (seq-sort-by (lambda (d) (nth 1 d)) #'< series) (list nil)))
      (cond
       ((not current-item) (setq current-item item))
       ((null item) (push current-item normalized-series))
       (t (let ((is-timeout (> (- (nth 1 item) (nth 2 current-item)) timeout))
                (is-chain-switch (not (= (nth 0 item) (nth 0 current-item)))))
            (unless is-timeout
              (setf (nth 2 current-item) (nth 1 item)))
            (when (or is-timeout is-chain-switch)
              (push current-item normalized-series)
              (setq current-item item))))))
    (setq my/test normalized-series)
    ;; Back into chains
    (mapcar
     (lambda (series)
       (seq-sort-by
        #'car
        #'<
        (mapcar
         (lambda (item) (cons (nth 1 item) (nth 2 item)))
         (cdr series))))
     (seq-sort-by
      #'car
      #'<
      (seq-group-by #'car normalized-series)))))

(provide 'deterred-utils)
;;; deterred-utils.el ends here

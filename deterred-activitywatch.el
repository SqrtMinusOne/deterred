;;; deterred-activitywatch.el --- TODO -*- lexical-binding: t -*-

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
(require 'deterred-db)
(require 'deterred-locations)
(require 'request)
(require 'cl-lib)

(defconst deterred-activitywatch-uuid-namespace
  "6c4ea183-e81a-4e9d-bffc-11ed5aacb130")

(defcustom deterred-activitywatch-api "http://localhost:5600/api"
  "ActivityWatch API URL."
  :group 'deterred
  :type 'string)

(defcustom deterred-activitywatch-convert-unknown "Emacs"
  "How to interpret \"unknown\" from the current window watcher.

I set this to \"Emacs\" because this usually means EXWM for me."
  :group 'deterred
  :type 'string)

(defun deterred-activitywatch--bucket-store-afk (events hostname)
  "Store AFK EVENTS for HOSTNAME in DETERRED.

EVENTS is a list of events from the ActivityWatch API."
  (let ((db (deterred-db--init))
        values)
    (unless (seq-empty-p events)
      (cl-mapc
       (lambda (event)
         (let ((timestamp (time-convert
                           (encode-time
                            (iso8601-parse
                             (alist-get 'timestamp event)))
                           'integer)))
           (when (equal (alist-get 'status (alist-get 'data event)) "not-afk")
             (push `((hostname . ,hostname)
                     (notafk_start_timestamp . ,timestamp)
                     (notafk_end_timestamp . ,(round (+ timestamp (alist-get 'duration event)))))
                   values))))
       (seq-sort-by
        (lambda (event)
          (alist-get 'timestamp event))
        #'string-lessp
        events))
      (when values
        (with-sqlite-transaction db
          (deterred-db-insert-unsafe
           db :table-name 'activitywatch_notafk_period
           :values values
           :conflict-action 'do-nothing
           :conflict-attrs '(hostname notafk_start_timestamp notafk_end_timestamp))
          (deterred-db--mark-updated db 'activitywatch_notafk_period)))
      (message "Saved %d not-AFK periods from %s" (length values) hostname))))

(defun deterred-activitywatch--bucket-load-afk (bucket-id hostname &optional callback)
  "Parse AFK BUCKET-ID for HOSTNAME in DETERRED.

Call CALLBACK on success.  This is necessary because other bucket
loading logic might depend on the AFK bucket."
  (let* ((db (deterred-db--init))
         (start (caar
                 (sqlite-select
                  db "SELECT MAX(notafk_end_timestamp)
                      FROM activitywatch_notafk_period"))))
    (request (concat deterred-activitywatch-api "/0/buckets/" bucket-id "/events")
      :parser 'json-read
      :params (when start
                `(("start" .            ; Some overlap to be sure
                   ,(format-time-string "%Y-%m-%dT%H:%M:%S"
                                        (- start (* 60 60 24)) t))))
      :encoding 'utf-8
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (deterred-activitywatch--bucket-store-afk data hostname)
                  (when callback
                    (funcall callback))))
      :error (cl-function
              (lambda (&key error-thrown &allow-other-keys)
                (message "Error!: %S" error-thrown))))))

(defun deterred-activitywatch--bucket-load-afk-recursive (buckets callback)
  "Recursively load afk BUCKETS and call CALLBACK when done."
  (if buckets
      (let ((bucket (car buckets)))
        (deterred-activitywatch--bucket-load-afk
         (alist-get 'id (cdr bucket))
         (alist-get 'hostname (cdr bucket))
         (lambda ()
           (deterred-activitywatch--bucket-load-afk-recursive
            (cdr buckets) callback))))
    (funcall callback)))

(defun deterred-activitywatch--load-currentwindow-get-days (db hostname created-at)
  "Get the list of ActivityWatch days to parse.

DB is the sqlite database object, HOSTNAME is the hostname, CREATED-AT
is the bucket creation date.

Return the list of decoded-times to process."
  (let* ((start-date (caar
                      (sqlite-select
                       db "SELECT MAX(day) FROM activitywatch_currentwindow_agg
                          WHERE hostname = ?"
                       (list hostname))))
         (start-timestamp (if start-date
                              (parse-time-string start-date)
                            (parse-time-string created-at)))
         (end-timestamp (decode-time (time-subtract
                                      (current-time)
                                      (* 60 60 24))))
         times)
    (setf (decoded-time-hour end-timestamp) 23
          (decoded-time-minute end-timestamp) 59
          (decoded-time-second end-timestamp) 59
          (decoded-time-hour start-timestamp) 0
          (decoded-time-minute start-timestamp) 0
          (decoded-time-second start-timestamp) 0)
    (let* ((start-time (encode-time start-timestamp))
           (end-time (encode-time end-timestamp)))
      (when created-at
        (setq start-time (time-add start-time (* 60 60 24))))
      (while (time-less-p start-time end-time)
        (push (decode-time start-time) times)
        (setq start-time (time-add start-time (* 60 60 24))))
      (nreverse times))))

(defun deterred-activitywatch--get-borders (db day hostname)
  "Get the borders of day for ActivityWatch currentwindow parsing.

DB is the sqlite database object.  DAY is the target day in the
decoded time form.  HOSTNAME is the hostname."
  (let ((start-day (copy-sequence day))
        (end-day (copy-sequence day)))
    (setf (decoded-time-hour start-day) 0
          (decoded-time-minute start-day) 0
          (decoded-time-second start-day) 0
          (decoded-time-hour end-day) 23
          (decoded-time-minute end-day) 59
          (decoded-time-second end-day) 59)
    (let* ((offset-timestamp (+
                              (time-convert (encode-time start-day) 'integer)
                              (* 12 60 60)))
           (offset (deterred-locations-offset-at offset-timestamp hostname db)))
      (setf (decoded-time-zone start-day) offset
            (decoded-time-zone end-day) offset))
    (cons (format-time-string "%FT%T%z" (encode-time start-day) t)
          (format-time-string "%FT%T%z" (encode-time end-day) t))))

(defun deterred-activitywatch--bucket-process-currentwindow-afk (db hostname events)
  "Add AFK data to currentwindow EVENTS and transform them.

Return a list of cons cells, where car is the timestamp, and cdr is
one of:
- afk-start (symbol)
- afk-end (symbol)
- a string with the active program name.

HOSTNAME is the hostname.  DB is the sqlite database object."
  (let* ((events-data
          (seq-sort-by
           #'car #'<
           (mapcar
            (lambda (event)
              (let ((timestamp (time-convert
                                (encode-time
                                 (iso8601-parse (alist-get 'timestamp event)))
                                'integer))
                    (app (alist-get 'app (alist-get 'data event))))
                (cons
                 timestamp
                 (if (and (equal app "unknown")
                          deterred-activitywatch-convert-unknown)
                     deterred-activitywatch-convert-unknown
                   app))))
            events)))
         (afk-data
          (mapcan
           (lambda (datum)
             (list (cons (nth 0 datum) 'notafk-start)
                   (cons (nth 1 datum) 'notafk-end)))
           (sqlite-select
            db
            "SELECT notafk_start_timestamp, notafk_end_timestamp
             FROM activitywatch_notafk_period
             WHERE hostname = ? AND notafk_end_timestamp >= ?
              AND notafk_start_timestamp <= ?"
            (list hostname (caar events-data) (caar (last events-data))))))
         (all-data
          (seq-sort-by #'car #'< (append events-data afk-data))))
    (cl-loop with active-notafk = nil
             for datum in all-data
             if (stringp (cdr datum)) collect datum
             else if (and (null active-notafk) (eq (cdr datum) 'notafk-start))
             do (setq active-notafk t) and collect datum
             else if (and active-notafk (eq (cdr datum) 'notafk-end))
             do (setq active-notafk nil) and collect datum)))

(defun deterred-activitywatch--bucket-process-currentwindow (db hostname events)
  "Group activitywatch currentwindow EVENTS.

This accounts for AFK data using
`deterred-activitywatch--bucket-process-currentwindow-afk'.

Return a hash map with app names with keys and total non-AFK seconds
spent there as values.

HOSTNAME is the hostname.  DB is the sqlite database object."
  (let ((data (deterred-activitywatch--bucket-process-currentwindow-afk
               db hostname events))
        (group-data (make-hash-table :test #'equal))
        active-notafk active-datum)
    (dolist (datum data)
      (cond
       ((eq (cdr datum) 'notafk-start)
        (setq active-notafk t)
        (when active-datum
          (setq active-datum
                (cons (car datum) (cdr active-datum)))))
       ((eq (cdr datum) 'notafk-end)
        (setq active-notafk nil)
        (when active-datum
          (puthash (cdr active-datum)
                   (+ (- (car datum) (car active-datum))
                      (gethash (cdr active-datum) group-data 0))
                   group-data)
          (setq active-datum nil)))
       ((stringp (cdr datum))
        ;; Ich bin immer noch hier
        (when (and active-notafk active-datum)
          (puthash (cdr active-datum)
                   (+ (- (car datum) (car active-datum))
                      (gethash (cdr active-datum) group-data 0))
                   group-data))
        (setq active-datum datum))))
    group-data))

(defun deterred-activitywatch--bucket-store-currentwindow (db day hostname events)
  "Save ActivityWatch current windows EVENTS on DAY to DB.

DB is the sqlite database object.  DAY is the day in decoded time
form.  HOSTNAME is the hostname.  EVENTS is the list of events, as
returned by the ActivityWatch API."
  (let ((data
         (cl-loop for app being the hash-keys of
                  (deterred-activitywatch--bucket-process-currentwindow
                   db hostname events)
                  using (hash-values duration)
                  collect `((hostname . ,hostname)
                            (day . ,(format-time-string "%F" (encode-time day)))
                            (app . ,app)
                            (total_duration . ,duration)))))
    (when data
      (with-sqlite-transaction db
        (deterred-db-insert-unsafe
         db
         :table-name 'activitywatch_currentwindow_agg
         :values data
         :conflict-action 'do-update
         :conflict-attrs '(day hostname app))))))

(defun deterred-activitywatch--load-currentwindow
    (bucket-id hostname &optional created-at days)
  "Load data from ActivityWatch's currentwindow BUCKET-ID on DAYS.

This is a recursive function.

HOSTNAME is the hostname.

CREATED-AT is the bucket creation date in the ISO8601 format (as
returned by the ActivityWatch API).  Only use it on the first pass.

DAYS is a list of decoded times (as returned by `iso8601-parse' for
convinience).  Used internally for recursion."
  (when-let* ((db (deterred-db--init))
              (days (or days
                        (when created-at
                          (deterred-activitywatch--load-currentwindow-get-days
                           db hostname created-at))))
              (day (car days))
              (border (deterred-activitywatch--get-borders db day hostname)))
    (message "Saving ActivityWatch day: %s" (format-time-string "%F" (encode-time day)))
    (request (concat deterred-activitywatch-api "/0/buckets/"
                     bucket-id "/events")
      :parser 'json-read
      :params `(("start" . ,(car border))
                ("end" . ,(cdr border)))
      :encoding 'utf-8
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (deterred-activitywatch--bucket-store-currentwindow
                   db day hostname data)
                  (deterred-activitywatch--load-currentwindow
                   bucket-id hostname nil (cdr days))))
      :error (cl-function
              (lambda (&key error-thrown &allow-other-keys)
                (message "Error!: %S" error-thrown))))))

(defun deterred-activitywatch-load ()
  "Load data from ActivityWatch API into DETERRED."
  (interactive)
  (request (concat deterred-activitywatch-api "/0/buckets")
    :parser 'json-read
    :encoding 'utf-8
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                ;; Load AFK buckets
                (deterred-activitywatch--bucket-load-afk-recursive
                 (seq-filter
                  (lambda (bucket)
                    (equal (alist-get 'type (cdr bucket))
                           "afkstatus"))
                  data)
                 ;; Load the dependent buckets
                 (lambda ()
                   (dolist (bucket data)
                     (pcase (alist-get 'type (cdr bucket))
                       ("currentwindow"
                        (deterred-activitywatch--load-currentwindow
                         (alist-get 'id (cdr bucket))
                         (alist-get 'hostname (cdr bucket))
                         (alist-get 'created (cdr bucket))))
                       (_ nil)))))))
    :error (cl-function
            (lambda (&key error-thrown &allow-other-keys)
              (message "Error!: %S" error-thrown)))))

(provide 'deterred-activitywatch)
;;; deterred-activitywatch.el ends here

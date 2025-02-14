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
        values notafk-start)
    (unless (seq-empty-p events)
      (cl-mapc
       (lambda (event)
         (let ((timestamp (time-convert
                           (encode-time
                            (iso8601-parse
                             (alist-get 'timestamp event)))
                           'integer)))
           (if (equal (alist-get 'status (alist-get 'data event)) "not-afk")
               (setq notafk-start timestamp)
             (push `((hostname . ,hostname)
                     (notafk_start_timestamp . ,notafk-start)
                     (notafk_end_timestamp . ,timestamp))
                   values))))
       (seq-sort-by
        (lambda (event)
          (alist-get 'timestamp event))
        #'string-lessp
        events))
      (with-sqlite-transaction db
        (deterred-db-insert-unsafe
         db :table-name 'activitywatch_notafk_period
         :values values
         :conflict-action 'do-nothing
         :conflict-attrs '(hostname notafk_start_timestamp notafk_end_timestamp))
        (deterred-db--mark-updated 'activitywatch_notafk_period db))
      (message "Saved %d not-AFK periods from %s" (length values) hostname))))

(defun deterred-activitywatch--bucket-load-afk (bucket-id hostname)
  "Parse AFK BUCKET-ID for HOSTNAME in DETERRED."
  (let* ((db (deterred-db--init))
         (start (caar
                 (sqlite-select
                  db "SELECT MAX(notafk_end_timestamp)
                      FROM activitywatch_notafk_period"))))
    (request (concat deterred-activitywatch-api "/0/buckets/" bucket-id "/events")
      :parser 'json-read
      :params (when start `(("start" . ,(format-time-string "%Y-%m-%dT%H:%M:%S" start))))
      :encoding 'utf-8
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (deterred-activitywatch--bucket-store-afk data hostname)))
      :error (cl-function
              (lambda (&key error-thrown &allow-other-keys)
                (message "Error!: %S" error-thrown))))))

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

(defun deterred-activitywatch--bucket-store-currentday (db day hostname events)
  "Save ActivityWatch current windows EVENTS on DAY to DB.

DB is the sqlite database object.  DAY is the day in decoded time
form.  HOSTNAME is the hostname.  EVENTS is the list of events, as
returned by the ActivityWatch API."
  (let ((data
         (thread-last
           events
           (seq-group-by
            (lambda (event)
              (let ((app (alist-get 'app (alist-get 'data event))))
                (if (and (equal app "unknown")
                         deterred-activitywatch-convert-unknown)
                    deterred-activitywatch-convert-unknown
                  app))))
           (mapcar
            (lambda (events)
              (cons (car events)
                    (apply
                     #'+ (mapcar (lambda (event) (alist-get 'duration event))
                                 (cdr events))))))
           (seq-sort-by #'cdr #'>)
           (mapcar (lambda (datum)
                     `((hostname . ,hostname)
                       (day . ,(format-time-string "%F" (encode-time day)))
                       (app . ,(car datum))
                       (total_duration . ,(cdr datum))))))))
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
                  (deterred-activitywatch--bucket-store-currentday
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
                (dolist (bucket data)
                  (pcase (alist-get 'type (cdr bucket))
                    ("afkstatus"
                     (deterred-activitywatch--bucket-load-afk
                      (alist-get 'id (cdr bucket))
                      (alist-get 'hostname (cdr bucket))))
                    ("currentwindow"
                     (deterred-activitywatch--load-currentwindow
                      (alist-get 'id (cdr bucket))
                      (alist-get 'hostname (cdr bucket))
                      (alist-get 'created (cdr bucket))))
                    (_ nil)))))
    :error (cl-function
            (lambda (&key error-thrown &allow-other-keys)
              (message "Error!: %S" error-thrown)))))

(provide 'deterred-activitywatch)
;;; deterred-activitywatch.el ends here

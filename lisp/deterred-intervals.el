;; deterred-intervals.el --- Intervals utilities for DETERRED. -*- lexical-binding: t -*-

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

;; Helpers for working with time intervals in DETERRED.
;;
;; This file defines interval objects used to align timestamps to
;; clock- and calendar-based buckets and to generate bucket boundaries
;; for dashboard ranges.

;;; Code:
(require 'eieio)
(require 'cl-lib)

(defclass deterred-interval ()
  ((display-name :initarg :display-name :type string))
  "An abstract interval class."
  :abstract t)

(defun deterred-interval--timestamp (&optional timestamp)
  "Return TIMESTAMP or the current time."
  (or timestamp (current-time)))

(defun deterred-interval--encode-time (time)
  "Return TIME encoded as a Unix timestamp."
  (time-convert (encode-time time) 'integer))

(defun deterred-interval--zero-clock (time)
  "Move TIME to the start of its day."
  (setf (decoded-time-second time) 0
        (decoded-time-minute time) 0
        (decoded-time-hour time) 0)
  time)

(cl-defgeneric deterred-interval-start (interval &optional timestamp)
  "Return the Unix timestamp at the start of INTERVAL containing TIMESTAMP.

If TIMESTAMP is nil, use the current time.")

(cl-defgeneric deterred-interval-end (interval &optional timestamp)
  "Return the Unix timestamp at the end of INTERVAL containing TIMESTAMP.

If TIMESTAMP is nil, use the current time.")

(cl-defgeneric deterred-interval-next (interval timestamp)
  "Return the Unix timestamp at the start of INTERVAL after TIMESTAMP.")

(cl-defgeneric deterred-interval-display-name (interval)
  "Return the display name of the INTERVAL.")

(cl-defmethod deterred-interval-display-name ((interval deterred-interval))
  "Return the display name of INTERVAL."
  (oref interval display-name))

(defclass deterred-interval-clock (deterred-interval)
  ((unit :initarg :unit :type symbol)
   (step :initarg :step :type integer))
  "An abstract clock-aligned interval measured in minutes or hours."
  :abstract t)

(defclass deterred-interval-minute (deterred-interval-clock)
  ()
  "A clock-aligned interval measured in minutes.")

(defclass deterred-interval-hour (deterred-interval-clock)
  ()
  "A clock-aligned interval measured in hours.")

(cl-defmethod deterred-interval-start
  ((interval deterred-interval-clock) &optional timestamp)
  "Return the start of INTERVAL containing TIMESTAMP."
  (let ((time (decode-time (deterred-interval--timestamp timestamp)))
        (step (oref interval step)))
    (pcase (oref interval unit)
      ('minute
       (setf (decoded-time-second time) 0
             (decoded-time-minute time) (* step (floor (decoded-time-minute time) step))))
      ('hour
       (setf (decoded-time-second time) 0
             (decoded-time-minute time) 0
             (decoded-time-hour time) (* step (floor (decoded-time-hour time) step))))
      (_
       (error "Unsupported clock interval unit: %s" (oref interval unit))))
    (deterred-interval--encode-time time)))

(cl-defmethod deterred-interval-end
  ((interval deterred-interval-clock) &optional timestamp)
  "Return the end of INTERVAL containing TIMESTAMP."
  (1- (deterred-interval-next interval
                              (deterred-interval-start interval timestamp))))

(cl-defmethod deterred-interval-next
  ((interval deterred-interval-clock) timestamp)
  "Return the start of INTERVAL after TIMESTAMP."
  (let ((time (decode-time (deterred-interval-start interval timestamp)))
        (step (oref interval step)))
    (pcase (oref interval unit)
      ('minute (cl-incf (decoded-time-minute time) step))
      ('hour (cl-incf (decoded-time-hour time) step))
      (_
       (error "Unsupported clock interval unit: %s" (oref interval unit))))
    (deterred-interval--encode-time time)))

(defclass deterred-interval-day (deterred-interval)
  ()
  "A day-long interval.")

(cl-defmethod deterred-interval-start
  ((_interval deterred-interval-day) &optional timestamp)
  "Return the Unix timestamp at the start of the day containing TIMESTAMP."
  (let ((time (decode-time (deterred-interval--timestamp timestamp))))
    (deterred-interval--zero-clock time)
    (deterred-interval--encode-time time)))

(cl-defmethod deterred-interval-end
  ((_interval deterred-interval-day) &optional timestamp)
  "Return the Unix timestamp at the end of the day containing TIMESTAMP."
  (let ((time (decode-time (deterred-interval--timestamp timestamp))))
    (setf (decoded-time-second time) 59
          (decoded-time-minute time) 59
          (decoded-time-hour time) 23)
    (deterred-interval--encode-time time)))

(cl-defmethod deterred-interval-next
  ((_interval deterred-interval-day) timestamp)
  "Return the Unix timestamp at the start of the day after TIMESTAMP."
  (let ((time (decode-time (deterred-interval-start (deterred-interval-day)
                                                    timestamp))))
    (cl-incf (decoded-time-day time))
    (deterred-interval--encode-time time)))

(defclass deterred-interval-calendar (deterred-interval)
  ((unit :initarg :unit :type symbol))
  "A calendar-aligned interval measured in whole days or longer.")

(cl-defmethod deterred-interval-start
  ((interval deterred-interval-calendar) &optional timestamp)
  "Return the start of INTERVAL containing TIMESTAMP."
  (let ((time (decode-time (deterred-interval--timestamp timestamp))))
    (pcase (oref interval unit)
      ('week
       (deterred-interval--zero-clock time)
       (cl-decf (decoded-time-day time)
                (mod (1- (decoded-time-weekday time)) 7)))
      ('month
       (deterred-interval--zero-clock time)
       (setf (decoded-time-day time) 1))
      ('quarter
       (deterred-interval--zero-clock time)
       (setf (decoded-time-day time) 1
             (decoded-time-month time)
             (+ 1 (* 3 (/ (1- (decoded-time-month time)) 3)))))
      ('year
       (deterred-interval--zero-clock time)
       (setf (decoded-time-day time) 1
             (decoded-time-month time) 1))
      (_
       (error "Unsupported calendar interval unit: %s" (oref interval unit))))
    (deterred-interval--encode-time time)))

(cl-defmethod deterred-interval-end
  ((interval deterred-interval-calendar) &optional timestamp)
  "Return the end of INTERVAL containing TIMESTAMP."
  (1- (deterred-interval-next interval
                              (deterred-interval-start interval timestamp))))

(cl-defmethod deterred-interval-next
  ((interval deterred-interval-calendar) timestamp)
  "Return the start of INTERVAL after TIMESTAMP."
  (let ((time (decode-time (deterred-interval-start interval timestamp))))
    (pcase (oref interval unit)
      ('week (cl-incf (decoded-time-day time) 7))
      ('month (cl-incf (decoded-time-month time)))
      ('quarter (cl-incf (decoded-time-month time) 3))
      ('year (cl-incf (decoded-time-year time)))
      (_
       (error "Unsupported calendar interval unit: %s" (oref interval unit))))
    (deterred-interval--encode-time time)))

(defconst deterred-intervals
  `((minutes-5 . ,(deterred-interval-minute :unit 'minute :step 5 :display-name "5 Minutes"))
    (minutes-10 . ,(deterred-interval-minute :unit 'minute :step 10 :display-name "10 Minutes"))
    (minutes-15 . ,(deterred-interval-minute :unit 'minute :step 15 :display-name "15 Minutes"))
    (minutes-20 . ,(deterred-interval-minute :unit 'minute :step 20 :display-name "20 Minutes"))
    (minutes-30 . ,(deterred-interval-minute :unit 'minute :step 30 :display-name "30 Minutes"))
    (hours-2 . ,(deterred-interval-hour :unit 'hour :step 2 :display-name "2 Hours"))
    (hours-4 . ,(deterred-interval-hour :unit 'hour :step 4 :display-name "4 Hours"))
    (hours-8 . ,(deterred-interval-hour :unit 'hour :step 8 :display-name "8 Hours"))
    (day . ,(deterred-interval-day :display-name "Day"))
    (week . ,(deterred-interval-calendar :unit 'week :display-name "Week"))
    (month . ,(deterred-interval-calendar :unit 'month :display-name "Month"))
    (quarter . ,(deterred-interval-calendar :unit 'quarter :display-name "Quarter"))
    (year . ,(deterred-interval-calendar :unit 'year :display-name "Year")))
  "Alist of available interval kinds in DETERRED.")

(defun deterred-intervals-generate (start end kind)
  "Return interval boundary timestamps from START to END.

The result contains the start of each KIND interval that overlaps the
requested range.  KIND is a key in `deterred-intervals'."
  (let ((int (alist-get kind deterred-intervals)))
    (unless int
      (error "Unknown interval: %s" kind))
    (let* ((start (deterred-interval-start int start))
           (end (deterred-interval-end int end))
           (curr start)
           res)
      (while (<= curr end)
        (push curr res)
        (setq curr (deterred-interval-next int curr)))
      (nreverse res))))

(provide 'deterred-intervals)
;;; deterred-intervals.el ends here

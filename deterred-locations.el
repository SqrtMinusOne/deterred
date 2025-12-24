;;; deterred-locations.el --- Locations functionality for DETERRED. -*- lexical-binding: t -*-

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
(require 'calendar)
(require 'cl-lib)
(require 'org)

(require 'deterred-db)
(require 'deterred-source)

(defconst deterred-locations-uuid-namespace
  "51038aa1-8fb1-4e05-b697-fa651ba8786d")

;; TODO: Location editing interface

(defun deterred-locations-register (location-id timestamp &optional db)
  "Sign up at LOCATION-ID at TIMESTAMP in DB."
  (interactive
   (let* ((db (deterred-db--init))
          (locations (mapcar
                      (lambda (loc) (cons (nth 1 loc) (nth 0 loc)))
                      (sqlite-select db "SELECT id, name FROM location")))
          (location-name (completing-read "Location: " locations)))
     (list (alist-get location-name locations nil nil #'equal)
           (time-convert
            (encode-time
             (org-parse-time-string
              (org-read-date t)))
            'integer)
           db)))
  (deterred-db-insert-unsafe
   db :table-name 'location_times
   :values `(((location_id . ,location-id)
              (timestamp . ,timestamp)))))

(defun deterred-locations--dst-offset-hours (timestamp dst-mode)
  "Calculate DST offset at TIMESTAMP.

DST-MODE is either \"EU\", \"RU\", or nil."
  (let* ((time (decode-time timestamp))
         (year (decoded-time-year time))
         (month (decoded-time-month time))
         (last-sun-march (calendar-nth-named-day -1 0 3 year))
         (last-sun-oct (calendar-nth-named-day -1 0 10 year))
         (dst-start
          (encode-time 0 0 1 (nth 1 last-sun-march) 3 year))
         (dst-end (encode-time 0 0 1 (nth 1 last-sun-oct) 10 year)))
    (pcase dst-mode
      ("EU"
       (cond
        ((> year 2024) 0)
        ((and (time-less-p dst-start (encode-time time))
              (time-less-p (encode-time time) dst-end))
         1)
        (t 0)))
      ("RU"
       (cond
        ((> year 2015) 0)
        ((> year 2010) 1)
        ((and (time-less-p dst-start (encode-time time))
              (time-less-p (encode-time time) dst-end))
         1)
        (t 0)))
      ('nil 0)
      (_ (error "Unknown DST mode: %s" dst-mode)))))

(defun deterred-locations-locate-at (db timestamp)
  "Return location info at TIMESTAMP.

TIMESTAMP is a UNIX timestamp.  DB is the sqlite database object.

The return value is an alist with the following keys:
- id
- name
- lat
- lon
- timezone
- dst-mode

\"timezone\" is adjusted for DST."
  (let ((raw-data (car (sqlite-select
                        db "SELECT l.id, name, latitude, longitude, timezone, dst_mode FROM location l
                   INNER JOIN location_times lt ON l.id = lt.location_id
                   WHERE lt.timestamp <= ?
                   ORDER BY lt.timestamp DESC LIMIT 1"
                        (list timestamp)))))
    (unless raw-data
      (error "No location data found for timestamp %s" timestamp))
    (let* ((data (deterred-db-list-to-alist raw-data '(id name lat lon timezone dst-mode)))
           (dst-hours (deterred-locations--dst-offset-hours
                       timestamp (alist-get 'dst-mode data))))
      (setf (alist-get 'timezone data)
            (+ (alist-get 'timezone data) dst-hours))
      data)))

(defun deterred-locations-offset-at (timestamp &optional hostname db)
  "Return timezone offset at TIMESTAMP.

HOSTNAME is an optional parameter to process hostnames with known
timezones.

DB is the sqlite database object."
  (let* ((db (or db (deterred-db--init)))
         timezone dst-mode)
    (when hostname
      (setq timezone (caar (sqlite-select
                            db "SELECT timezone FROM location_static_hostnames
                                WHERE hostname = ?"
                            (list hostname)))))
    (unless timezone
      (let ((data (deterred-locations-locate-at db timestamp)))
        (setq timezone (alist-get 'timezone data))))
    (unless timezone
      (error "No timezone found for timestamp %s" timestamp))
    (* 60 60 timezone)))

(defclass deterred-locations (deterred-source)
  ((name :initform "Location")
   (warn-days :initform nil))
  "DETERRED source for locations.")

(cl-defmethod deterred-source-range ((_source deterred-locations) &optional db)
  "Get the data availability range for locations.

DB is the sqlite database object.

Return a cons cell, with car as the start timestamp, and cdr as the
end timestamp."
  (let* ((db (or db (deterred-db--init)))
         (data (sqlite-select
                db "SELECT MIN(timestamp), MAX(timestamp)
                    FROM location_times")))
    (cons (caar data) (cadar data))))

(cl-defmethod deterred-source-day-summary
  ((_source deterred-locations) timestamp &optional db)
  "Make locations summary for TIMESTAMP.

DB is the sqlite database object."
  (let* ((db (or db (deterred-db--init)))
         (loc (deterred-locations-locate-at db timestamp)))
    `((:short-description
       . ,(format "%s (offset %s hours)"
                  (alist-get 'name loc)
                  (alist-get 'timezone loc))))))

(cl-defmethod deterred-source-range-summary
  ((_source deterred-locations) start end &optional db)
  "Make locations summary for [START, END].

DB is the sqlite database object."
  (let* ((db (or db (deterred-db--init)))
         (location-changes
          (deterred-db-select-alist
           db "SELECT l.name, l.timezone, l.dst_mode, lt.timestamp
               FROM location l
               INNER JOIN location_times lt ON l.id = lt.location_id
               WHERE lt.timestamp BETWEEN ? AND ?
               ORDER BY lt.timestamp ASC"
           (list start end))))
    (when location-changes
      `((:short-description
         . ,(format "%d location change%s"
                    (seq-length location-changes)
                    (if (= (seq-length location-changes) 1) "" "s")))
        (:long-description
         . ,(deterred-format
             (f-mapconcat
              (f "- " (format-time-string "%Y-%m-%d" (alist-get 'timestamp iter))
                 ": " (f-acc "iter->'name")
                 " (offset "
                 (f-num (+ (alist-get 'timezone iter)
                           (deterred-locations--dst-offset-hours
                            (alist-get 'timestamp iter)
                            (alist-get 'dst_mode iter))))
                 " hours)")
              location-changes)))))))

(provide 'deterred-locations)
;;; deterred-locations.el ends here

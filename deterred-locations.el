;;; deterred-locations.el --- TODO -*- lexical-binding: t -*-

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
(require 'cl-lib)

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

(defun deterred-locations-offset-at (timestamp &optional db)
  "Return timezone offset at TIMESTAMP.

DB is the sqlite database object."
  (let* ((db (or db (deterred-db--init)))
         (timezone (caar (sqlite-select
                          db "SELECT timezone FROM location l
                        INNER JOIN location_times lt ON l.id = lt.location_id
                        WHERE lt.timestamp <= ?
                        ORDER BY lt.timestamp DESC LIMIT 1"
                          (list timestamp)))))
    (unless timezone
      (error "No timezone found for timestamp %s" timestamp))
    (* 60 60 timezone)))

(provide 'deterred-locations)
;;; deterred-locations.el ends here

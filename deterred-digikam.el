;;; deterred-digikam.el --- TODO -*- lexical-binding: t -*-

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
(require 'deterred-source)
(require 'deterred-locations)
(require 'cl-lib)

(defun deterred-digikam--get-data (db digikam-db)
  "Read data from DIGIKAM-DB.

DB and DIGIKAM-DB are both sqlite database objects.

Return two lists of alists:
- albums:
  - id
  - path
- photos
  - id
  - filename
  - album_id
  - lat
  - lon
  - alt
  - camera"
  (let ((album-data
         (sqlite-select digikam-db "SELECT id, relativePath FROM Albums"))
        (photos-data
         (sqlite-select digikam-db
                        "SELECT
  i.id,
  i.name filename,
  i.album album_id,
  unixepoch(COALESCE(ii.creationDate, i.modificationDate)) t,
  ip.latitudeNumber lat,
  ip.latitudeNumber lon,
  ip.altitude alt,
  CASE WHEN instr(im.model, im.make) THEN im.model ELSE im.make || ' ' || im.model END camera
FROM Images i
LEFT JOIN ImageMetadata im ON im.imageid = i.id
LEFT JOIN ImagePositions ip ON ip.imageid = i.id
LEFT JOIN ImageInformation ii ON ii.imageid = i.id
WHERE i.album IS NOT NULL")))
    (list
     (mapcar (lambda (d) (deterred-db-list-to-alist d '(id path))) album-data)
     (mapcar (lambda (d)
               (let* ((datum
                       (deterred-db-list-to-alist
                        d '(id filename album_id timestamp lat lon alt camera)))
                      (offset (deterred-locations-offset-at
                               (alist-get 'timestamp datum) nil db)))
                 (setf (alist-get 'timestamp datum)
                       (- (alist-get 'timestamp datum) offset))
                 datum))
             photos-data))))

(defun deterred-digikam-store (db album-data photo-data)
  "Insert ALBUM-DATA and PHOTO-DATA into DB.

For ALBUM-DATA and PHOTO-DATA, see `deterred-digikam--get-data'.  DB
is the sqlite database object."
  (with-sqlite-transaction db
    (deterred-db-insert-unsafe
     db
     :table-name 'digikam_album
     :values album-data
     :conflict-attrs '(id)
     :conflict-action 'do-update)
    (deterred-db-insert-unsafe
     db
     :table-name 'digikam_photo
     :values photo-data
     :conflict-attrs '(id)
     :conflict-action 'do-update)
    (deterred-db-cleanup-unsafe
     db 'digikam_album 'id
     (mapcar (lambda (s) (alist-get 'id s)) album-data))
    (deterred-db-cleanup-unsafe
     db 'digikam_photo 'id
     (mapcar (lambda (s) (alist-get 'id s)) photo-data))
    (deterred-db--mark-update-batch
     db
     '(digikam_album digikam_photo))))

(defun deterred-digikam-load (file)
  "Load digiKam database into DERERRED.

FILE is the SQLite database file, which should be called digikam4.db,
I think."
  (interactive
   (list
    (read-file-name "DigiKam SQLite file: ")))
  (let ((db (deterred-db--init))
        (digikam-db (sqlite-open file)))
    (sqlite-pragma digikam-db "foreign_keys = ON")
    (let ((data (deterred-digikam--get-data db digikam-db)))
      (deterred-digikam-store
       db (nth 0 data) (nth 1 data)))))

(defclass deterred-digikam (deterred-source)
  ((name :initform "Photos (Digikam)")
   (warn-days :initform 7)
   (digikam-db :initarg :digikam-db))
  "DETERRED source for digikam.")

(cl-defmethod deterred-source-range ((_source deterred-digikam) &optional db)
  "Get the data availability range for ActivityWatch.

DB is the sqlite database object.

Return a cons cell, with car as the start timestamp, and cdr as the
end timestamp."
  (let* ((db (or db (deterred-db--init)))
         (data (sqlite-select
                db "SELECT MIN(timestamp), MAX(timestamp)
                    FROM digikam_photo")))
    (cons (caar data) (cadar data))))

(cl-defmethod deterred-source-sync ((source deterred-digikam) &optional callback)
  "Sync DETERRED with digikam.

Call CALLBACK when done.

SOURCE is an instance of `deterred-digikam'."
  (unless (oref source digikam-db)
    (user-error "No digikam-db file set"))
  (deterred-digikam-load (oref source digikam-db))
  (when callback (funcall callback)))

(provide 'deterred-digikam)
;;; deterred-digikam.el ends here

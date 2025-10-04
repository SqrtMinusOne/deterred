;;; deterred-mpd.el --- TODO -*- lexical-binding: t -*-

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
(require 'pcsv)
(require 'libmpdel)

(defconst deterred-mpd-uuid-namespace
  "c66d74fc-c243-4d8e-9e0a-72a88b78132b")

(defun deterred-mpd--list-all (callback)
  "List all songs in MPD (with listallinfo).

Call CALLBACK with the results."
  (libmpdel-ensure-connection)
  (libmpdel-send-command
   "listallinfo "
   (lambda (response)
     (run-with-timer
      0 nil
      callback
      (let ((alists nil)
            (alist nil))
        (dolist (cell response)
          (if (member (car cell) '(file directory playlist))
              (setq alists (cons alist alists)
                    alist (list cell))
            (setq alist (cons cell alist))))
        (when alist
          (setq alists (cons alist alists)))
        alists)))))

(defun deterred-mpd--ensure-year (string)
  "Extract the first four digits of STRING or return nil."
  (when (and string
             (string-match-p (rx bos (= 4 digit)) string))
    (substring string 0 4)))

(defun deterred-mpd--song-uuid (file)
  "Create a UUID from FILE."
  (uuidgen-3 deterred-mpd-uuid-namespace file))

(defun deterred-mpd--insert-songs (data db)
  "Insert or update MPD songs into database.

DATA is the output of `deterred-mpd--list-all'.  DB a sqlite database
instance."
  (let* ((values
          (mapconcat
           (lambda (datum)
             (concat
              "("
              (string-join
               (mapcar (lambda (item)
                         (cond ((null item) "NULL")
                               ((integerp item) (number-to-string item))
                               ((stringp item) (deterred-db--escape item))))
                       (list
                        (deterred-mpd--song-uuid (alist-get 'file datum))
                        (alist-get 'file datum)
                        (alist-get 'Time datum)
                        (alist-get 'Artist datum)
                        (or (alist-get 'AlbumArtist datum)
                            (alist-get 'Artist datum))
                        (alist-get 'Album datum)
                        (alist-get 'Title datum)
                        (deterred-mpd--ensure-year
                         (alist-get 'Date datum))
                        (alist-get 'MUSICBRAINZ_TRACKID datum)))
               ", ")
              ")"))
           (seq-filter
            (lambda (datum)
              (alist-get 'file datum))
            data)
           ",\n"))
         (query
          (format
           "INSERT into mpd_song (id, file, duration, artist, album_artist, album, title, year, musicbrainz_trackid)
         VALUES %s
         ON CONFLICT (id) DO UPDATE SET
           file=excluded.file,
           duration=excluded.duration,
           artist=excluded.artist,
           album_artist=excluded.album_artist,
           album=excluded.album,
           title=excluded.title,
           year=excluded.year,
           musicbrainz_trackid=excluded.musicbrainz_trackid"
           values)))
    (with-sqlite-transaction db
      (sqlite-execute db query)
      (deterred-db-mark-updated db "mpd_song"))))

(defun deterred-mpd-update-library ()
  "Update MPD songs in the DETERRED database."
  (interactive)
  (let ((db (deterred-db--init)))
    (deterred-mpd--list-all
     (lambda (data)
       (deterred-mpd--insert-songs data db)
       (message "%s MPD songs upserted" (seq-length data))))))

(defun deterred-mpd--migrate--upsert-song-listened (datum db)
  "Insert DATUM about a listened song in DB.

DATUM is an alist with the following keys:
- album_artist
- title
- album
- file
- time, an `iso8601-parse'-able string.

DB is a sqlite connection."
  (let* ((album-artist (alist-get 'album_artist datum))
         (album (alist-get 'album datum))
         (title (alist-get 'title datum))
         (file (alist-get 'file datum))
         (time (time-convert
                (encode-time
                 (iso8601-parse (string-replace " " "T" (alist-get 'time datum))))
                'integer))
         (song-id
          (or
           (caar (sqlite-select
                  db "SELECT id FROM mpd_song WHERE file = ?" (list file)))
           (caar (sqlite-select
                  db "SELECT id FROM mpd_song
                      WHERE album_artist = ?
                        AND title = ?
                        AND album = ?"
                  (list album-artist title album))))))
    (if song-id
        (sqlite-execute db "INSERT INTO mpd_song_listened (mpd_song_id, timestamp)
                        VALUES (?, ?)
                        ON CONFLICT (mpd_song_id, timestamp) DO NOTHING"
                        (list song-id time))
      (let ((msg (format "Can't find song for datum %s" datum)))
        (unless (y-or-n-p (concat msg ". Continue?"))
          (user-error msg))))))

(defun deterred-mpd-migrate-load-csv (file)
  "Load a csv FILE with the MPD listen log into DETERRED.

The columns of the file have to match the input of
`deterred-mpd--migrate--upsert-song-listened'."
  (interactive
   (list
    (read-file-name "CSV file: " nil nil nil nil
                    (lambda (f)
                      (or (file-directory-p f)
                          (string-match-p (rx ".csv" eos) f))))))
  ;; Parse CSV line-by-line
  (let* ((data (pcsv-parse-file file))
         (header (mapcar #'intern (car data)))
         (db (deterred-db--init)))
    (with-sqlite-transaction db
      (cl-loop
       with total = (1- (seq-length data))
       for row in (cdr data)
       for i from 0
       for datum = (cl-loop for key in header
                            for value in row
                            collect (cons key value))
       do (deterred-mpd--migrate--upsert-song-listened datum db)
       do (message "Processed: %s/%s" i total))
      (deterred-db-mark-updated db "mpd_song_listened"))))

(defun deterred-mpd-migrate-fix-timestamp ()
  (interactive)
  (user-error "Don't call this!!!")
  (let* ((db (deterred-db--init))
         (data (sqlite-select
                db "SELECT mpd_song_id, timestamp FROM mpd_song_listened
                    ORDER BY timestamp"))
         (default-offset (* 3 60 60))
         (update-data (cl-loop
                       with len = (length data)
                       for datum in data
                       for i from 0
                       for offset = (deterred-locations-offset-at (nth 1 datum) nil db)
                       if (not (= offset default-offset))
                       collect (list
                                (nth 0 datum)
                                (nth 1 datum)
                                (+ (nth 1 datum) (- offset) default-offset))
                       if (= (% i 100) 0)
                       do (message "Processed %d/%d" i len))))
    (with-sqlite-transaction db
      (dolist (update-datum update-data)
        (sqlite-execute
         db "UPDATE mpd_song_listened
             SET timestamp = ?
             WHERE timestamp = ? AND mpd_song_id = ?"
         (list
          (nth 2 update-datum)
          (nth 1 update-datum)
          (nth 0 update-datum)))))))

(defclass deterred-mpd (deterred-source)
  ((name :initform "Music (MPD)"))
  "DETERRED source for mpd.")

(cl-defmethod deterred-source-range ((_source deterred-mpd) &optional db)
  "Get the data availability range for MPD.

DB is the sqlite database object.

Return a cons cell, with car as the start timestamp, and cdr as the
end timestamp."
  (let* ((db (or db (deterred-db--init)))
         (data (sqlite-select
                db "SELECT MIN(timestamp), MAX(timestamp)
                    FROM mpd_song_listened")))
    (cons (caar data) (cadar data))))

(cl-defmethod deterred-source-day-summary
  ((_source deterred-mpd) timestamp &optional db)
  (let* ((db (or db (deterred-db--init)))
         (data (deterred-db-select-alist
                db "SELECT * FROM mpd_song ms
                    INNER JOIN mpd_song_listened msl ON msl.mpd_song_id = ms.id
                    WHERE timestamp BETWEEN ? AND ?"
                (list timestamp (+ (* 60 60 24) timestamp))))
         (time-total (apply #'+ (mapcar (lambda (d) (alist-get 'duration d)) data)))
         (time-by-artist
          (deterred-db-select-alist
           db "SELECT ms.album_artist, sum(ms.duration) duration FROM mpd_song ms
               INNER JOIN mpd_song_listened msl ON msl.mpd_song_id = ms.id
               WHERE timestamp BETWEEN ? AND ?
               GROUP BY ms.album_artist
               ORDER BY duration DESC"
           (list timestamp (+ (* 60 60 24) timestamp))))
         (time-by-album
          (deterred-db-select-alist
           db "SELECT
                 ms.album || ' (' || ms.album_artist || ')' album,
                 sum(ms.duration) duration,
                 min(msl.timestamp) started
               FROM mpd_song ms
               INNER JOIN mpd_song_listened msl ON msl.mpd_song_id = ms.id
               WHERE timestamp BETWEEN ? AND ?
               GROUP BY ms.album_artist, ms.album
               ORDER BY started ASC"
           (list timestamp (+ (* 60 60 24) timestamp)))))
    (when data
      `((:short-description
         . ,(deterred-format
             (deterred-utils-duration-from-minutes (/ time-total 60.0))
             " total: "
             (f (f-acc "time-by-artist[0]->'album_artist")
                " (" (deterred-utils-duration-from-minutes
                      (/ (f-acc "time-by-artist[0]->'duration") 60.0)) ")"
                (when (> (seq-length time-by-artist) 1)
                  (f ", " (f-acc "time-by-artist[1]->'album_artist")
                     " (" (deterred-utils-duration-from-minutes
                           (/ (f-acc "time-by-artist[1]->'duration") 60.0)) ")"))
                (when (> (seq-length time-by-artist) 2)
                  (f " and " (f-num (- (seq-length time-by-artist) 2)) " others")))))
        (:long-description
         . ,(deterred-format
             "Albums listened:\n"
             (f-mapconcat
              (f "- " (f-acc "iter->'album") " ("
                 (deterred-utils-duration-from-minutes
                  (/ (f-acc "iter->'duration") 60.0))
                 ")")
              time-by-album)))))))

(provide 'deterred-mpd)
;;; deterred-mpd.el ends here

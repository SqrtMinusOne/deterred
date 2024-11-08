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

(defun deterred-mpd--song-uuid (file)
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
                        (alist-get 'MUSICBRAINZ_TRACKID datum)) )
               ", ")
              ")"))
           (seq-filter
            (lambda (datum)
              (alist-get 'file datum))
            data)
           ",\n"))
         (query
          (format
           "INSERT into mpd_song (id, file, duration, artist, album_artist, album, title, musicbrainz_trackid)
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
      (deterred-db--mark-updated "mpd_song" db))))

(defun deterred-mpd-update-library ()
  (interactive)
  (let ((db (deterred-db--init)))
    (deterred-mpd--list-all
     (lambda (data)
       (setq my/test data)
       (deterred-mpd--insert-songs data db)))))

(provide 'deterred-mpd)
;;; deterred-mpd.el ends here

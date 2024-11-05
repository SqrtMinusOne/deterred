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
(require 'libmpdel)

(defun deterred-mpd--list-all (callback)
  (libmpdel-ensure-connection)
  (libmpdel-send-command
   "listallinfo "
   (lambda (response)
     (funcall
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

(defun deterred-mpd--insert-songs (data db)
  (let* ((values (mapconcat
                  (lambda (datum)
                    (concat
                     "("
                     (string-join
                      (list
                       (deterred-utils-uuid-to-sqlite-hex
                        (uuidgen-3 "mpd" (alist-get 'file datum)))
                       (format "'%s'" (alist-get 'file datum))
                       (alist-get 'Time datum)
                       (format "'%s'" (alist-get 'Artist datum))
                       (format "'%s'" (alist-get 'AlbumArtist datum))
                       (format "'%s'" (alist-get 'Album datum))
                       (format "'%s'" (alist-get 'Title datum))
                       (format "'%s'" (alist-get 'MUSICBRAINZ_TRACKID datum)))
                      ", ")
                     ")"))
                  (seq-filter
                   (lambda (datum)
                     (alist-get 'file datum))
                   data)
                  ",\n")))
    (sqlite-execute
     db
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
      values))))

(deterred-mpd--insert-songs my/test2 my/test)

(provide 'deterred-mpd)
;;; deterred-mpd.el ends here

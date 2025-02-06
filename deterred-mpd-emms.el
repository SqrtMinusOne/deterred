;;; deterred-mpd-emms.el --- TODO -*- lexical-binding: t -*-

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
(require 'emms)
(require 'emms-browser)

(defun deterred-mpd-emms--browser-next-mapping-type-around (f type)
  "Advise `emms-browser-next-mapping-type' to process last-played.

F is the function, which see for the meaning of TYPE."
  (if (eq type 'last-played)
      'info-artist
    (funcall f type)))

(defun deterred-mpd-emms--browser-get-face--around (f bdata)
  "Advise `emms-browser-get-face' to process last-played.

F is the function, which see for the meaning of BDATA."
  (if (eq (emms-browser-bdata-type bdata) 'last-played)
      'emms-browser-year/genre-face
    (funcall f bdata)))

(defun deterred-mpd-emms--assert-advice ()
  "Advise EMMS functions to work with last-played."
  (advice-add #'emms-browser-next-mapping-type
              :around #'deterred-mpd-emms--browser-next-mapping-type-around)
  (advice-add #'emms-browser-get-face
              :around #'deterred-mpd-emms--browser-get-face--around))

(defun deterred-mpd-emms-set-last-listened ()
  "Update last-played and play-count in EMMS from DETERRED."
  (interactive)
  (let* ((db (deterred-db--init))
         (music-root (file-name-as-directory
                      (expand-file-name emms-player-mpd-music-directory)))
         (data (sqlite-select
                db "SELECT
                        s.file,
                        COUNT(*) as play_count,
                        MAX(l.timestamp) as last_played
                    FROM mpd_song s
                    INNER JOIN mpd_song_listened l ON s.id = l.mpd_song_id
                    GROUP BY s.file
                    ORDER BY last_played DESC")))
    (dolist (datum data)
      (let* ((file (concat music-root (nth 0 datum)))
             (play-count (nth 1 datum))
             (timestamp (nth 2 datum))
             (last-played (seconds-to-time (time-convert timestamp #'integer)))
             (hash-datum (gethash file emms-cache-db)))
        (when hash-datum
          (setf (alist-get 'last-played (cdr hash-datum))
                last-played
                (alist-get 'play-count (cdr hash-datum))
                play-count))))))

(defun deterred-emms-browser-get-track (track type)
  "Format the last played data in TRACK, if TYPE is last-played.

Otherwise fallback to `emms-browser-get-track-field-albumartist',
which is the default value of `emms-browser-get-track-field'.  It
helps EMMS to render the last-played date correctly."
  (cond ((eq type 'last-played)
         (if-let ((last-played (emms-track-get track 'last-played)))
             (format-time-string "%Y-%m-%d" last-played)
           "Never"))
        ((eq type 'play-count)
         (format "%s times" (emms-track-get track 'play-count 0)))
        (t (emms-browser-get-track-field-albumartist track type))))

;;;###autoload
(defun deterred-emms-browse-by-last-played ()
  "Browse by last-played."
  (interactive)
  (deterred-mpd-emms--assert-advice)
  (let ((emms-browser-get-track-field-function
         #'deterred-emms-browser-get-track)
        (emms-browser-alpha-sort-function
         (lambda (s1 s2)
           (cond ((equal s1 "Never") nil)
                 ((equal s2 "Never") t)
                 ((string-match-p
                   (rx (= 4 digit) "-" (= 2 digit) "-" (= 2 digit))
                   s1)
                  (string-collate-lessp s2 s1))
                 ((string-match-p
                   (rx (= 4 digit) "-" (= 2 digit) "-" (= 2 digit))
                   s2)
                  (string-collate-lessp s2 s1))
                 (t (string-collate-equalp s1 s2))))))
    (emms-browse-by 'last-played)))

(provide 'deterred-mpd-emms)
;;; deterred-mpd-emms.el ends here

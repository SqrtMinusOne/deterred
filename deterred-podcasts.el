;;; deterred-podcasts.el --- TODO -*- lexical-binding: t -*-

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

(defconst deterred-podcasts-uuid-namespace
  "ce099187-5d4f-4754-8ac7-4156d8582bb2")

(defun deterred-podcasts--match-feed (db title url &optional language)
  "Insert or update a podcast feed into the DETERRED database.

DB is the database object.  TITLE, URL, and LANGUAGE are feed
attributes.

The function returns the feed's ID.  The IDs are UUIDv4 because, in
principle, URLs can change.  As such, this is an extensibility point
for future merging."
  (let ((feed-id (or (caar (sqlite-select
                            db "SELECT id FROM podcasts_feed WHERE url LIKE ?"
                            (list url)))
                     (caar (sqlite-select
                            db "SELECT id FROM podcasts_feed WHERE title LIKE ?"
                            (list url)))
                     (uuidgen-4))))
    (deterred-db-insert-unsafe db :table-name 'podcasts_feed
                               :values `(((id . ,feed-id)
                                          (title . ,title)
                                          (url . ,url)
                                          ,@(when language
                                              `(((language . ,language))))))
                               :conflict-attrs '(id)
                               :conflict-action 'do-update)
    feed-id))

(defun deterred-podcasts--antennapod-load-feeds (db antennapod-db)
  "Add feeds from ANTENNAPOD-DB to the DETERRED DB.

Both arguments are sqlite database objects.  The function returns an
alist, mapping AntennaPod's IDs to DETERRED's IDs."
  (let ((feeds (sqlite-select
                antennapod-db
                "SELECT id, COALESCE(custom_title, title) title, download_url
                 FROM Feeds")))
    (mapcar (lambda (feed-datum)
              (cons (car feed-datum)
                    (deterred-podcasts--match-feed
                     db
                     (nth 1 feed-datum)
                     (nth 2 feed-datum))))
            feeds)))

(defun deterred-podcasts--antennapod-load-listened (db antennapod-db)
  "Add listened podcasts from ANTENNAPOD-DB to the DETERRED DB."
  (let ((feeds-mapping (deterred-podcasts--antennapod-load-feeds db antennapod-db))
        (listened-data
         (sqlite-select
          antennapod-db
          "SELECT
            fi.feed,
            COALESCE(fi.link, fm.download_url),
            fi.title,
            fi.pubDate / 1000,
            fm.duration / 1000,
            fm.played_duration / 1000,
            fm.last_played_time / 1000
           FROM FeedItems fi INNER JOIN FeedMedia fm ON fm.feeditem = fi.id
           WHERE fm.played_duration IS NOT NULL")))
    (deterred-db-insert-unsafe
     db :table-name 'podcasts_listened
     :values (mapcar
              (lambda (datum)
                `((feed_id . ,(alist-get (nth 0 datum) feeds-mapping))
                  (item_id . ,(uuidgen-3 deterred-podcasts-uuid-namespace
                                         (nth 1 datum)))
                  (url . ,(nth 1 datum))
                  (title . ,(nth 2 datum))
                  (published_timestamp . ,(nth 3 datum))
                  (total_duration . ,(nth 4 datum))
                  (played_duration . ,(nth 5 datum))
                  (timestamp . ,(nth 6 datum))))
              listened-data)
     :conflict-action 'do-update
     :conflict-attrs '(feed_id  item_id))))

(defun deterred-podcasts-load-antennapod (file)
  "Load an AntennaPod SQLite database file into DETERRED.

FILE is the path to the database."
  (interactive
   (list
    (read-file-name "AntennaPod SQLite file: " nil nil nil nil)))
  (let ((db (deterred-db--init))
        (antennapod-db (sqlite-open file)))
    (sqlite-pragma antennapod-db "foreign_keys = ON")
    (with-sqlite-transaction db
      (deterred-podcasts--antennapod-load-listened db antennapod-db)
      (deterred-db--mark-update-batch
       db '(podcasts_listened podcasts_feed)))))

(provide 'deterred-podcasts)
;;; deterred-podcasts.el ends here

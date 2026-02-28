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
(require 'deterred-format)
(require 'deterred-source)

(require 'magit-section)
(require 'uuidgen)
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
     :conflict-attrs '(feed_id item_id))))

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
      (deterred-db-mark-updated-batch
       db '(podcasts_listened podcasts_feed)))))

;;;###autoload
(defclass deterred-podcasts (deterred-source)
  ((name :initform "Podcasts"))
  "DETERRED source for podcasts.")

(cl-defmethod deterred-source-range ((_source deterred-podcasts) &optional db)
  "Get the data availability range for Mastodon.

DB is the sqlite database object.

Return a cons cell, with car as the start timestamp, and cdr as the
end timestamp."
  (let* ((db (or db (deterred-db--init)))
         (data (sqlite-select
                db "SELECT MIN(timestamp), MAX(timestamp)
                    FROM podcasts_listened")))
    (cons (caar data) (cadar data))))

(cl-defmethod deterred-source-actions ((_source deterred-podcasts) &optional callback)
  "Run an action for the podcasts source.

Run CALLBACK when done."
  (deterred-source--actions-pick
   '(("Load AntennaPod DB" deterred-podcasts-load-antennapod nil))
   callback))

(cl-defmethod deterred-source-day-summary
  ((_source deterred-podcasts) timestamp &optional db)
  (let* ((db (or db (deterred-db--init)))
         (podcasts (deterred-db-select-alist
                    db "SELECT l.*, f.title podcast FROM podcasts_listened l
                        INNER JOIN podcasts_feed f ON f.id = l.feed_id
                        WHERE timestamp BETWEEN ? AND ?"
                    (list timestamp (+ (* 60 60 24) timestamp))))
         (titles (seq-uniq (mapcar
                            (lambda (p)
                              (truncate-string-to-width
                               (alist-get 'podcast p)
                               35 nil nil t))
                            podcasts))))
    (when podcasts
      `((:short-description
         . ,(deterred-format
             (if (> (seq-length titles) 2)
                 (f (f-num (seq-length podcasts)) " podcasts listened")
               (f
                (f-join titles ", ")
                " (" (f-num (seq-length podcasts)) " total)"))))
        (:long-description
         . ,(deterred-format
             (f-mapconcat
              (f "- " (f-button
                       (f-acc "iter->'title")
                       (lambda (&rest _)
                         (browse-url (alist-get 'url iter))))
                 (when (> (seq-length titles) 1)
                   (f " (" (f-acc "iter->'podcast") ")")))
              podcasts)))))))

(defun deterred-podcasts--render-episodes-by-podcast (episodes-by-podcast)
  "Render podcast EPISODES-BY-PODCAST grouped by podcast.

EPISODES-BY-PODCAST is a list of (podcast-name . episodes-list) pairs."
  (dolist (podcast-group (seq-sort-by
                          (lambda (group)
                            (apply #'+ (mapcar (lambda (ep) (alist-get 'played_duration ep))
                                               (cdr group))))
                          #'>
                          episodes-by-podcast))
    (let ((podcast-name (car podcast-group))
          (episodes (cdr podcast-group)))
      (magit-insert-section (deterred-podcast-feed t t)
        (insert
         (propertize
          (format "%s (%d episode%s)"
                  podcast-name
                  (length episodes)
                  (if (= (length episodes) 1) "" "s"))
          'face 'deterred-faces-section-heading-3))
        (magit-insert-heading)
        (dolist (episode episodes)
          (magit-insert-section (deterred-podcast-episode t t)
            (insert
             (propertize
              (alist-get 'title episode)
              'face 'deterred-faces-section-heading-4
              'mouse-face 'highlight
              'help-echo "Click to open URL"
              'keymap (let ((map (make-sparse-keymap)))
                        (define-key map [mouse-1]
                                    (lambda ()
                                      (interactive)
                                      (browse-url (alist-get 'url episode))))
                        (define-key map (kbd "RET")
                                    (lambda ()
                                      (interactive)
                                      (browse-url (alist-get 'url episode))))
                        map)))
            (magit-insert-heading)))))))

(cl-defmethod deterred-source-range-summary
  ((_source deterred-podcasts) start end &optional db)
  "Make podcasts summary for [START, END].

DB is the sqlite database object."
  (let* ((db (or db (deterred-db--init)))
         (episodes
          (deterred-db-select-alist
           db "SELECT l.*, f.title podcast FROM podcasts_listened l
               INNER JOIN podcasts_feed f ON f.id = l.feed_id
               WHERE l.timestamp BETWEEN ? AND ?
               ORDER BY f.title, l.timestamp DESC"
           (list start end)))
         (episodes-by-podcast (seq-group-by (lambda (ep) (alist-get 'podcast ep)) episodes))
         (podcast-count (length episodes-by-podcast)))
    (when episodes
      `((:short-description
         . ,(format "%d podcast%s (%d episode%s)"
                    podcast-count
                    (if (= podcast-count 1) "" "s")
                    (length episodes)
                    (if (= (length episodes) 1) "" "s")))
        (:long-description-fn
         . ,(lambda (&rest _)
              (deterred-podcasts--render-episodes-by-podcast episodes-by-podcast)))))))

(provide 'deterred-podcasts)
;;; deterred-podcasts.el ends here

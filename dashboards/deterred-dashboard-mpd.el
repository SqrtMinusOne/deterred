;;; deterred-dashboard-mpd.el --- DETERRED dashboard for MPD -*- lexical-binding: t -*-

;; Copyright (C) 2025 Korytov Pavel

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

;; A dashboard for MPD, corresponding to `deterred-mpd'.

;;; Code:
(require 'deterred-dashboard)
(require 'deterred-db)

(defclass deterred-dashboard-mpd (deterred-dashboard)
  ((name :initform "MPD"))
  "A DETERRED dashboard for MPD.")

(cl-defmethod deterred-dashboard-default-params ((_dashboard deterred-dashboard-mpd))
  "Default parameters for the MPD dashboard."
  '((:start-date)
    (:end-date)
    (:artist)
    (:album)
    (:n-top-artists . 5)))

(cl-defmethod deterred-dashboard-render-params ((_dashboard deterred-dashboard-mpd))
  "Render the parameters section for the MPD dashboard."
  (deterred-dashboard-widget-date
   :name "Start date"
   :key :start-date
   :display-date t)
  (insert "\n")
  (deterred-dashboard-widget-date
   :name "End date"
   :key :end-date
   :display-date t)
  (insert "\n")
  (let* ((db (deterred-db--init))
         (artists
          (mapcar
           (lambda (item) (alist-get 'album_artist item))
           (deterred-db-select-alist
            db "SELECT DISTINCT album_artist FROM mpd_song ORDER BY album_artist")))
         (albums
          (mapcar
           (lambda (item) (alist-get 'album item))
           (deterred-db-select-alist
            db "SELECT DISTINCT album || ' (' || album_artist || ')' album
                FROM mpd_song ORDER BY album"))))
    (deterred-dashboard-widget-completing-read-multiple
     :name "Artist"
     :key :artist
     :options artists)
    (insert "\n")
    (deterred-dashboard-widget-completing-read-multiple
     :name "Album"
     :key :album
     :options albums)
    (insert "\n")
    (deterred-dashboard-widget-number
     :name "Top N artists"
     :key :n-top-artists)
    (insert "\n")))

(cl-defmethod deterred-dashboard-list-datasets ((_dashboard deterred-dashboard-mpd))
  "List datasets for the MPD dashboard."
  '((top-album-artists (name . "Top album artists"))
    (top-albums (name . "Top albums"))
    (top-songs (name . "Top songs"))
    (listened-by-year (name . "Hours listened by year"))
    (listened-by-month (name . "Hours listened by month"))
    (listened-by-week (name . "Hours listened by week"))
    (artists-discovered (name . "Artists discovered"))
    (albums-discovered (name . "Albums discovered"))
    (new-albums-listened (name . "Hours listened to new albums by year"))
    (average-album-age-per-month (name . "Average album age per month"))
    (top-days (name . "Top days by listened time"))
    (top-weeks (name . "Top weeks by listened time"))
    (top-months (name . "Top months by listened time"))
    (listened-to-top-by-month (name . "Hours listened to top N artists by month"))))

(cl-defmethod deterred-dashboard-fetch-datasets ((_dashboard deterred-dashboard-mpd)
                                                 params)
  "Fetch datasets for the MPD dashboard.

PARAMS is as returned by `deterred-dashboard-default-params'."
  (let ((db (deterred-db--init)))
    `((top-album-artists
       . ,(deterred-db-select-template-alist
           db
           "WITH cts AS (
  SELECT count(*) c, mpd_song_id FROM mpd_song_listened msl
  WHERE 1 = 1 [[AND timestamp >= :start-date]] [[AND timestamp <= :end-date]]
  GROUP BY mpd_song_id
)
SELECT
  sum(c * duration) * 100 / (60 * 60) / 100.0 hours,
  album_artist
FROM mpd_song ms
INNER JOIN cts ON cts.mpd_song_id = ms.id
WHERE 1 = 1 [[AND album_artist IN :artist]] [[AND album IN :album]]
GROUP BY album_artist
ORDER BY hours DESC
LIMIT 20"
           params))
      (top-albums
       . ,(deterred-db-select-template-alist
           db
           "WITH cts AS (
  SELECT count(*) c, mpd_song_id FROM mpd_song_listened msl
  WHERE 1 = 1 [[AND timestamp >= :start-date]] [[AND timestamp <= :end-date]]
  GROUP BY mpd_song_id
)
SELECT
  sum(c * duration) * 100 / (60 * 60) / 100.0 hours,
  album,
  album_artist
FROM mpd_song ms
INNER JOIN cts ON cts.mpd_song_id = ms.id
WHERE 1 = 1 [[AND album_artist IN :artist]] [[AND album IN :album]]
GROUP BY album, album_artist
ORDER BY hours DESC
LIMIT 40"
           params))
      (top-songs
       . ,(deterred-db-select-template-alist
           db
           "WITH cts AS (
  SELECT count(*) c, mpd_song_id FROM mpd_song_listened msl
  WHERE 1 = 1 [[AND timestamp >= :start-date]] [[AND timestamp <= :end-date]]
  GROUP BY mpd_song_id
)
SELECT
  c count,
  title,
  album,
  album_artist,
  year
FROM mpd_song ms
INNER JOIN cts ON cts.mpd_song_id = ms.id
WHERE 1 = 1 [[AND album_artist IN :artist]] [[AND album IN :album]]
GROUP BY title, album, album_artist
ORDER BY count DESC
LIMIT 40"
           params))
      (listened-by-year
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y', msl.timestamp, 'unixepoch') year,
  sum(ms.duration) * 100 / (60 * 60) / 100.0 total
FROM mpd_song_listened msl
INNER JOIN mpd_song ms ON ms.id = msl.mpd_song_id
WHERE 1 = 1 [[AND timestamp >= :start-date]] [[AND timestamp <= :end-date]]
  [[AND album_artist IN :artist]] [[AND album IN :album]]
GROUP BY strftime('%Y', msl.timestamp, 'unixepoch')"
           params))
      (listened-by-month
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y-%m', msl.timestamp, 'unixepoch') month,
  sum(ms.duration) * 100 / (60 * 60) / 100.0 total
FROM mpd_song_listened msl
INNER JOIN mpd_song ms ON ms.id = msl.mpd_song_id
WHERE 1 = 1 [[AND timestamp >= :start-date]] [[AND timestamp <= :end-date]]
  [[AND album_artist IN :artist]] [[AND album IN :album]]
GROUP BY strftime('%Y-%m', msl.timestamp, 'unixepoch')"
           params))
      (listened-by-week
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y-%W', msl.timestamp, 'unixepoch') week,
  sum(ms.duration) * 100 / (60 * 60) / 100.0 total
FROM mpd_song_listened msl
INNER JOIN mpd_song ms ON ms.id = msl.mpd_song_id
WHERE 1 = 1 [[AND timestamp >= :start-date]] [[AND timestamp <= :end-date]]
  [[AND album_artist IN :artist]] [[AND album IN :album]]
GROUP BY strftime('%Y-%W', msl.timestamp, 'unixepoch')"
           params))
      (artists-discovered
       . ,(deterred-db-select-template-alist
           db
           "WITH artist_discovered_years AS (
  SELECT DISTINCT
    strftime('%Y', min(timestamp), 'unixepoch') year_discovered,
    ms.album_artist
  FROM mpd_song ms
  INNER JOIN mpd_song_listened msl ON msl.mpd_song_id = ms.id
  WHERE 1 = 1 [[AND timestamp >= :start-date]] [[AND timestamp <= :end-date]]
    [[AND album_artist IN :artist]] [[AND album IN :album]]
  GROUP BY ms.album_artist
)
SELECT year_discovered, count(*) new_artists FROM artist_discovered_years
GROUP BY year_discovered"
           params))
      (albums-discovered
       . ,(deterred-db-select-template-alist
           db
           "WITH album_discovered_years AS (
  SELECT DISTINCT
    strftime('%Y', min(timestamp), 'unixepoch') year_discovered,
    ms.album_artist
  FROM mpd_song ms
  INNER JOIN mpd_song_listened msl ON msl.mpd_song_id = ms.id
  WHERE 1 = 1 [[AND timestamp >= :start-date]] [[AND timestamp <= :end-date]]
    [[AND album_artist IN :artist]] [[AND album IN :album]]
  GROUP BY ms.album_artist, ms.album
)
SELECT year_discovered, count(*) new_albums FROM album_discovered_years
GROUP BY year_discovered"
           params))
      (new-albums-listened
       . ,(deterred-db-select-template-alist
           db
           "WITH album_discovered_years AS (
  SELECT STRFTIME('%Y', min(timestamp), 'unixepoch') YEAR, ms.album_artist, ms.album FROM mpd_song ms
  INNER JOIN mpd_song_listened msl ON msl.mpd_song_id = ms.id
  GROUP BY ms.album_artist, ms.album
)
SELECT
  STRFTIME('%Y', msl.timestamp, 'unixepoch') \"year\",
  sum(CASE WHEN ady.year = STRFTIME('%Y', msl.timestamp, 'unixepoch') THEN ms.duration ELSE 0 END) * 100 / (60 * 60) / 100.0 \"new\",
  sum(CASE WHEN ady.year != STRFTIME('%Y', msl.timestamp, 'unixepoch') THEN ms.duration ELSE 0 END) * 100 / (60 * 60) / 100.0 \"old\"
FROM mpd_song_listened msl
INNER JOIN mpd_song ms ON ms.id = msl.mpd_song_id
INNER JOIN album_discovered_years ady ON ady.album_artist = ms.album_artist AND ady.album = ms.album
WHERE 1 = 1 [[AND timestamp >= :start-date]] [[AND timestamp <= :end-date]]
  [[AND ms.album_artist IN :artist]] [[AND ms.album IN :album]]
GROUP BY STRFTIME('%Y', msl.timestamp, 'unixepoch')"
           params))
      (top-months
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y-%m', msl.timestamp, 'unixepoch') month,
  sum(ms.duration) * 100 / (60 * 60) / 100.0 total
FROM mpd_song_listened msl
INNER JOIN mpd_song ms ON ms.id = msl.mpd_song_id
WHERE 1 = 1 [[AND timestamp >= :start-date]] [[AND timestamp <= :end-date]]
  [[AND album_artist IN :artist]] [[AND album IN :album]]
GROUP BY strftime('%Y-%m', msl.timestamp, 'unixepoch')
ORDER BY total DESC
LIMIT 20"
           params))
      (top-weeks
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y-%W', msl.timestamp, 'unixepoch') week,
  sum(ms.duration) * 100 / (60 * 60) / 100.0 total
FROM mpd_song_listened msl
INNER JOIN mpd_song ms ON ms.id = msl.mpd_song_id
WHERE 1 = 1 [[AND timestamp >= :start-date]] [[AND timestamp <= :end-date]]
  [[AND album_artist IN :artist]] [[AND album IN :album]]
GROUP BY strftime('%Y-%W', msl.timestamp, 'unixepoch')
ORDER BY total DESC
LIMIT 20"
           params))
      (top-days
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y-%m-%d', msl.timestamp, 'unixepoch') day,
  sum(ms.duration) * 100 / (60 * 60) / 100.0 total
FROM mpd_song_listened msl
INNER JOIN mpd_song ms ON ms.id = msl.mpd_song_id
WHERE 1 = 1 [[AND timestamp >= :start-date]] [[AND timestamp <= :end-date]]
  [[AND album_artist IN :artist]] [[AND album IN :album]]
GROUP BY strftime('%Y-%m-%d', msl.timestamp, 'unixepoch')
ORDER BY total DESC
LIMIT 20"
           params))
      (listened-to-top-by-month
       . ,(deterred-db-select-template-alist
           db
           "WITH cts AS (
  SELECT count(*) c, mpd_song_id FROM mpd_song_listened msl
  WHERE 1 = 1 [[AND timestamp >= :start-date]] [[AND timestamp <= :end-date]]
  GROUP BY mpd_song_id
), top_artists AS (
  SELECT
    sum(c * duration) * 100 / (60 * 60) / 100.0 hours,
    album_artist
  FROM mpd_song ms
  INNER JOIN cts ON cts.mpd_song_id = ms.id
  WHERE 1 = 1 [[AND album_artist IN :artist]] [[AND album IN :album]]
  GROUP BY album_artist
  ORDER BY hours DESC
  LIMIT :n-top-artists
)
SELECT
  strftime('%Y-%m', msl.timestamp, 'unixepoch') month,
  ms.album_artist,
  sum(ms.duration) * 100 / (60 * 60) / 100.0 total
FROM mpd_song_listened msl
INNER JOIN mpd_song ms ON ms.id = msl.mpd_song_id
INNER JOIN top_artists ta ON ta.album_artist = ms.album_artist
WHERE 1 = 1 [[AND timestamp >= :start-date]] [[AND timestamp <= :end-date]]
GROUP BY strftime('%Y-%m', msl.timestamp, 'unixepoch'), ms.album_artist"
           params))
      (average-album-age-per-month
       . ,(deterred-db-select-template-alist
           db
           "WITH album_discovered_months AS (
  SELECT
    CAST(STRFTIME('%Y', min(timestamp), 'unixepoch') AS INTEGER) * 12 +
    CAST(STRFTIME('%m', min(timestamp), 'unixepoch') AS INTEGER) start_month,
    ms.album_artist,
    ms.album
  FROM mpd_song_listened msl
  INNER JOIN mpd_song ms ON ms.id = msl.mpd_song_id
  GROUP BY ms.album_artist, ms.album
), hours_per_month AS (
  SELECT
    STRFTIME('%Y-%m', msl.timestamp, 'unixepoch') \"month\",
    SUM(ms.duration) / (60.0 * 60.0) hours_spent
  FROM mpd_song_listened msl
  INNER JOIN mpd_song ms ON ms.id = msl.mpd_song_id
  WHERE 1 = 1 [[AND msl.timestamp >= :start-date]] [[AND msl.timestamp <= :end-date]]
    [[AND ms.album_artist IN :artist]] [[AND ms.album IN :album]]
  GROUP BY STRFTIME('%Y-%m', msl.timestamp, 'unixepoch')
), album_data AS (
  SELECT
    (
      (
        CAST(STRFTIME('%Y', msl.timestamp, 'unixepoch') AS INTEGER) * 12 +
        CAST(STRFTIME('%m', msl.timestamp, 'unixepoch') AS INTEGER)
      ) - adm.start_month
    ) album_age,
    SUM(ms.duration / (60.0 * 60.0)) / hpm.hours_spent fraction,
    STRFTIME('%Y-%m', msl.timestamp, 'unixepoch') \"month\",
    ms.album_artist,
    ms.album
  FROM mpd_song_listened msl
  INNER JOIN mpd_song ms ON ms.id = msl.mpd_song_id
  INNER JOIN album_discovered_months adm ON adm.album_artist = ms.album_artist AND adm.album = ms.album
  INNER JOIN hours_per_month hpm ON hpm.month = STRFTIME('%Y-%m', msl.timestamp, 'unixepoch')
  WHERE 1 = 1
    [[AND msl.timestamp >= :start-date]] [[AND msl.timestamp <= :end-date]]
    [[AND ms.album_artist IN :artist]] [[AND ms.album IN :album]]
  GROUP BY STRFTIME('%Y-%m', msl.timestamp, 'unixepoch'), ms.album_artist, ms.album
  ORDER BY month, fraction DESC
)
SELECT sum(album_age * fraction) age, month
FROM album_data
GROUP BY month
ORDER BY month ASC"
           params)))))

(cl-defmethod deterred-dashboard-render-results ((_dashboard deterred-dashboard-mpd)
                                                 _params data)
  "Render DATA for the MPD dashboard."
  (insert (deterred-format (f-h2 "Top items") "\n")
          (deterred-format (f-h3 "Top artists") "\n")
          (deterred-grid-print-with-org
           (alist-get 'data (alist-get 'top-album-artists data))
           :column-names '((hours . "Hours") (album_artist . "Artist"))
           :max-rows 10
           :grid-button t)
          "\n"
          (deterred-format (f-h3 "Top albums") "\n")
          (deterred-grid-print-with-org
           (alist-get 'data (alist-get 'top-albums data))
           :column-names '((hours . "Hours") (album . "Album")
                           (album_artist . "Artist"))
           :max-rows 10
           :grid-button t)
          "\n"
          (deterred-format (f-h3 "Top songs") "\n")
          (deterred-grid-print-with-org
           (alist-get 'data (alist-get 'top-songs data))
           :column-names '((count . "Count") (album . "Album")
                           (album_artist . "Artist")
                           (title . "Title")
                           (year . "Year"))
           :max-rows 10
           :max-column-width 22
           :grid-button t)
          "\n")
  (insert (deterred-format (f-h2 "Hours listened over time") "\n"))
  (deterred-dashboard-exec-python
   :python-code
   "from matplotlib import pyplot as plt
from matplotlib.ticker import MaxNLocator
from deterred import fig_to_b64

import pandas as pd

import json
import os
import base64
import io

data = json.loads(input())
df_y = pd.DataFrame(data['listened-by-year']['data'])
df_m = pd.DataFrame(data['listened-by-month']['data'])
df_w = pd.DataFrame(data['listened-by-week']['data'])

images = []

fig, ax = plt.subplots(figsize=(8, 5))
df_y.plot(ax=ax, kind='bar', x='year', y='total')
ax.set_title('Hours listened per year')
ax.xaxis.set_major_locator(MaxNLocator(nbins=40))
images.append(fig_to_b64(fig))

fig, ax = plt.subplots(figsize=(8, 5))
df_m.plot(ax=ax, kind='bar', x='month', y='total')
ax.set_title('Hours listened per month')
ax.xaxis.set_major_locator(MaxNLocator(nbins=40))
images.append(fig_to_b64(fig))

if len(df_w) < 40:
    fig, ax = plt.subplots(figsize=(8, 5))
    df_w.plot(ax=ax, kind='bar', x='week', y='total')
    ax.set_title('Hours listened per week')
    ax.xaxis.set_major_locator(MaxNLocator(nbins=40))
    images.append(fig_to_b64(fig))

print(json.dumps(images))"
   :input data
   :on-success
   (lambda (images)
     (insert (deterred-format (f-h3 "Listened per year") "\n"))
     (deterred-dashboard-print-images-base64 (elt images 0))
     (insert "\n")
     (insert (deterred-format (f-h3 "Listened per month") "\n"))
     (deterred-dashboard-print-images-base64 (elt images 1))
     (insert "\n")
     (when (> (seq-length images) 2)
       (insert (deterred-format (f-h3 "Listened per week") "\n"))
       (deterred-dashboard-print-images-base64 (elt images 2))
       (insert "\n"))))
  (insert (deterred-format (f-h2 "New music over the years") "\n"))
  (deterred-dashboard-exec-python
   :python-code
   "from matplotlib import pyplot as plt
from matplotlib.ticker import MaxNLocator
from deterred import fig_to_b64

import pandas as pd
import numpy as np

import json

data = json.loads(input())
df_artists = pd.DataFrame(data['artists-discovered']['data'])
df_albums = pd.DataFrame(data['albums-discovered']['data'])
df_new = pd.DataFrame(data['new-albums-listened']['data'])
df_age = pd.DataFrame(data['average-album-age-per-month']['data'])

images = []

fig, ax = plt.subplots(figsize=(8, 5))
df_artists.plot(ax=ax, kind='bar', x='year_discovered', y='new_artists')
ax.set_title('New artists per year')
images.append(fig_to_b64(fig))

fig, ax = plt.subplots(figsize=(8, 5))
df_albums.plot(ax=ax, kind='bar', x='year_discovered', y='new_albums')
ax.set_title('New albums per year')
images.append(fig_to_b64(fig))

fig, ax = plt.subplots(figsize=(8, 5))
df_new.plot(ax=ax, kind='bar', x='year', stacked=True)
ax.set_title('Hours listened to new vs. old albums')
images.append(fig_to_b64(fig))

fig, ax = plt.subplots(figsize=(8, 5))
ax.set_title('Average album age (in months) per month')
x = np.arange(len(df_age))
y = df_age.age
z = np.polyfit(x, y, 1)
p = np.poly1d(z)
ax.plot(x, p(x), '--', color='red', linewidth=1, label='trend')
df_age.plot(ax=ax, kind='line', x='month', y='age', legend=False)
ax.legend()
images.append(fig_to_b64(fig))

print(json.dumps(images))"
   :input data
   :on-success
   (lambda (images)
     (insert (deterred-format (f-h3 "New artists per year") "\n"))
     (deterred-dashboard-print-images-base64 (elt images 0))
     (insert "\n")
     (insert (deterred-format (f-h3 "New albums per year") "\n"))
     (deterred-dashboard-print-images-base64 (elt images 1))
     (insert "\n")
     (insert (deterred-format (f-h3 "Hours listened to new vs. old albums") "\n"))
     (deterred-dashboard-print-images-base64 (elt images 2))
     (insert "\n")
     (insert (deterred-format (f-h3 "Average album age per month") "\n"))
     (deterred-dashboard-print-images-base64 (elt images 3))
     (insert "\n")))

  (insert
   (deterred-format (f-h2 "Top periods")) "\n"
   (deterred-format (f-h3 "Top days by listened time")) "\n"
   (deterred-grid-print-with-org
    (alist-get 'data (alist-get 'top-days data))
    :column-names '((day . "Day") (total . "Hours listened"))
    :max-rows 10)
   "\n"
   (deterred-format (f-h3 "Top weeks by listened time")) "\n"
   (deterred-grid-print-with-org
    (alist-get 'data (alist-get 'top-weeks data))
    :column-names '((week . "Week") (total . "Hours listened"))
    :max-rows 10)
   "\n"
   (deterred-format (f-h3 "Top months by listened time")) "\n"
   (deterred-grid-print-with-org
    (alist-get 'data (alist-get 'top-months data))
    :column-names '((month . "Month") (total . "Hours listened"))
    :max-rows 10)
   "\n")

  (insert
   (deterred-format (f-h2 "Dynamics by artists")) "\n")
  (deterred-dashboard-exec-python
   :python-code
   "from matplotlib import pyplot as plt
from deterred import fig_to_b64

import pandas as pd

import json

data = json.loads(input())
df = pd.DataFrame(data['listened-to-top-by-month']['data'])
df_p = df.pivot(index='month', columns='album_artist', values='total').fillna(0)

images = []

fig, ax = plt.subplots(figsize=(8, 5))
df_p.plot(ax=ax, kind='line')
ax.set_title('Hours listened to top N artists by month')
images.append(fig_to_b64(fig))

print(json.dumps(images))"
   :input data
   :on-success
   #'deterred-dashboard-print-images-base64))

(provide 'deterred-dashboard-mpd)
;;; deterred-dashboard-mpd.el ends here

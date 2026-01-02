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
    (last-discovered-artists (name . "Last discovered artists"))
    (last-discovered-albums (name . "Last discovered albums"))
    (new-albums-listened (name . "Hours listened to new albums by year"))
    (current-year-releases-listened (name . "Hours listened to current year releases vs. older"))
    (average-album-age-per-month (name . "Average album age per month"))
    (top-days (name . "Top days by listened time"))
    (top-weeks (name . "Top weeks by listened time"))
    (top-months (name . "Top months by listened time"))
    (listened-to-top-by-month (name . "Hours listened to top N artists by month"))
    (artist-comparison-by-year (name . "Artist comparison by year"))))

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
      (last-discovered-artists
       . ,(deterred-db-select-template-alist
           db
           "WITH artist_discovered AS (
  SELECT
    ms.album_artist,
    min(msl.timestamp) discovery_timestamp
  FROM mpd_song_listened msl
  INNER JOIN mpd_song ms ON ms.id = msl.mpd_song_id
  GROUP BY ms.album_artist
),
cts AS (
  SELECT count(*) c, mpd_song_id FROM mpd_song_listened
  GROUP BY mpd_song_id
)
SELECT
  ad.album_artist artist,
  datetime(ad.discovery_timestamp, 'unixepoch') discovery_date,
  round(sum(cts.c * ms.duration) / (60.0 * 60.0), 2) hours_listened
FROM artist_discovered ad
INNER JOIN mpd_song ms ON ms.album_artist = ad.album_artist
INNER JOIN cts ON cts.mpd_song_id = ms.id
WHERE 1 = 1 [[AND ad.discovery_timestamp >= :start-date]] [[AND ad.discovery_timestamp <= :end-date]]
GROUP BY ad.album_artist, ad.discovery_timestamp
ORDER BY ad.discovery_timestamp DESC
LIMIT 20"
           params))
      (last-discovered-albums
       . ,(deterred-db-select-template-alist
           db
           "WITH album_discovered AS (
  SELECT
    ms.album_artist,
    ms.album,
    min(msl.timestamp) discovery_timestamp
  FROM mpd_song_listened msl
  INNER JOIN mpd_song ms ON ms.id = msl.mpd_song_id
  GROUP BY ms.album_artist, ms.album
),
cts AS (
  SELECT count(*) c, mpd_song_id FROM mpd_song_listened
  GROUP BY mpd_song_id
)
SELECT
  ad.album,
  ad.album_artist,
  datetime(ad.discovery_timestamp, 'unixepoch') discovery_date,
  round(sum(cts.c * ms.duration) / (60.0 * 60.0), 2) hours_listened
FROM album_discovered ad
INNER JOIN mpd_song ms ON ms.album_artist = ad.album_artist AND ms.album = ad.album
INNER JOIN cts ON cts.mpd_song_id = ms.id
WHERE 1 = 1 [[AND ad.discovery_timestamp >= :start-date]] [[AND ad.discovery_timestamp <= :end-date]]
GROUP BY ad.album_artist, ad.album, ad.discovery_timestamp
ORDER BY ad.discovery_timestamp DESC
LIMIT 50"
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
      (current-year-releases-listened
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  STRFTIME('%Y', msl.timestamp, 'unixepoch') \"year\",
  round(sum(CASE WHEN ms.year = STRFTIME('%Y', msl.timestamp, 'unixepoch') THEN ms.duration ELSE 0 END) / (60.0 * 60.0), 2) \"current_year\",
  round(sum(CASE WHEN ms.year != STRFTIME('%Y', msl.timestamp, 'unixepoch') OR ms.year IS NULL THEN ms.duration ELSE 0 END) / (60.0 * 60.0), 2) \"older\"
FROM mpd_song_listened msl
INNER JOIN mpd_song ms ON ms.id = msl.mpd_song_id
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
WHERE ms.album_artist IN (SELECT album_artist FROM top_artists)
  [[AND timestamp >= :start-date]] [[AND timestamp <= :end-date]]
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
           params))
      (artist-comparison-by-year
       . ,(deterred-db-select-template-alist
           db
           "WITH years_in_range AS (
  SELECT DISTINCT strftime('%Y', timestamp, 'unixepoch') year
  FROM mpd_song_listened
  WHERE 1 = 1 [[AND timestamp >= :start-date]] [[AND timestamp <= :end-date]]
  ORDER BY year DESC
  LIMIT 2
),
year_totals AS (
  SELECT
    strftime('%Y', msl.timestamp, 'unixepoch') year,
    sum(ms.duration) / (60.0 * 60.0) total_hours
  FROM mpd_song_listened msl
  INNER JOIN mpd_song ms ON ms.id = msl.mpd_song_id
  WHERE strftime('%Y', msl.timestamp, 'unixepoch') IN (SELECT year FROM years_in_range)
    [[AND msl.timestamp >= :start-date]] [[AND msl.timestamp <= :end-date]]
    [[AND ms.album_artist IN :artist]] [[AND ms.album IN :album]]
  GROUP BY strftime('%Y', msl.timestamp, 'unixepoch')
),
artist_by_year AS (
  SELECT
    strftime('%Y', msl.timestamp, 'unixepoch') year,
    ms.album_artist,
    round(sum(ms.duration) / (60.0 * 60.0), 2) hours
  FROM mpd_song_listened msl
  INNER JOIN mpd_song ms ON ms.id = msl.mpd_song_id
  WHERE strftime('%Y', msl.timestamp, 'unixepoch') IN (SELECT year FROM years_in_range)
    [[AND msl.timestamp >= :start-date]] [[AND msl.timestamp <= :end-date]]
    [[AND ms.album_artist IN :artist]] [[AND ms.album IN :album]]
  GROUP BY strftime('%Y', msl.timestamp, 'unixepoch'), ms.album_artist
)
SELECT
  aby.album_artist artist,
  round(COALESCE(max(CASE WHEN aby.year = (SELECT max(year) FROM years_in_range) THEN aby.hours END), 0), 2) current_year_hours,
  round(COALESCE(max(CASE WHEN aby.year = (SELECT min(year) FROM years_in_range) THEN aby.hours END), 0), 2) prev_year_hours,
  round(COALESCE(max(CASE WHEN aby.year = (SELECT max(year) FROM years_in_range) THEN aby.hours * 100.0 / yt.total_hours END), 0), 2) current_year_pct,
  round(COALESCE(max(CASE WHEN aby.year = (SELECT min(year) FROM years_in_range) THEN aby.hours * 100.0 / yt.total_hours END), 0), 2) prev_year_pct,
  (SELECT max(year) FROM years_in_range) current_year,
  (SELECT min(year) FROM years_in_range) prev_year
FROM artist_by_year aby
LEFT JOIN year_totals yt ON yt.year = aby.year
GROUP BY aby.album_artist
ORDER BY current_year_hours DESC
LIMIT 20"
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
if len(df_m) > 30:
    ax.xaxis.set_major_locator(MaxNLocator(nbins=40))
images.append(fig_to_b64(fig))

if len(df_w) < 40:
    fig, ax = plt.subplots(figsize=(8, 5))
    df_w.plot(ax=ax, kind='bar', x='week', y='total')
    ax.set_title('Hours listened per week')
    if len(df_w) > 30:
        ax.xaxis.set_major_locator(MaxNLocator(nbins=40))
    images.append(fig_to_b64(fig))

print(json.dumps(images))"
   :input data
   :on-success
   (lambda (images)
     (insert (deterred-format (f-h3 "Listened per year") "\n"))
     (deterred-dashboard-print-images-base64 (elt images 0))
     (insert "\n")
     (insert
      (deterred-grid-print-with-org
       (alist-get 'data (alist-get 'listened-by-year data))
       :column-names '((year . "Year") (total . "Hours listened"))
       :max-rows 20
       :grid-button t)
      "\n")
     (insert (deterred-format (f-h3 "Listened per month") "\n"))
     (deterred-dashboard-print-images-base64 (elt images 1))
     (insert "\n")
     (insert
      (deterred-grid-print-with-org
       (alist-get 'data (alist-get 'listened-by-month data))
       :column-names '((month . "Month") (total . "Hours listened"))
       :max-rows 20
       :grid-button t)
      "\n")
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
df_current_year = pd.DataFrame(data['current-year-releases-listened']['data'])

images = []

# Combined: New artists per year + New albums per year
fig, ax1 = plt.subplots(figsize=(8, 5))
ax1.set_title('New artists and albums per year')
ax1.set_xlabel('Year')
ax1.set_ylabel('New Artists', color='C0')

x_pos = np.arange(len(df_artists))
width = 0.35

bars1 = ax1.bar(x_pos - width/2 - 0.05, df_artists['new_artists'], width, color='C0', label='New Artists')
ax1.tick_params(axis='y', labelcolor='C0')
ax1.set_xticks(x_pos)
ax1.set_xticklabels(df_artists['year_discovered'], rotation=45)
ax1.legend(loc='upper left')

# Add value labels on top of artist bars
for bar in bars1:
    height = bar.get_height()
    ax1.text(bar.get_x() + bar.get_width()/2., height,
            f'{int(height)}',
            ha='center', va='bottom', fontsize=8, color='C0')

ax2 = ax1.twinx()
ax2.set_ylabel('New Albums', color='C1')
bars2 = ax2.bar(x_pos + width/2 + 0.05, df_albums['new_albums'], width, color='C1', label='New Albums')
ax2.tick_params(axis='y', labelcolor='C1')
ax2.legend(loc='upper right')

# Add value labels on top of album bars
for bar in bars2:
    height = bar.get_height()
    ax2.text(bar.get_x() + bar.get_width()/2., height,
            f'{int(height)}',
            ha='center', va='bottom', fontsize=8, color='C1')

fig.tight_layout()
images.append(fig_to_b64(fig))

# Combined: Hours listened to new vs. old albums + current year releases vs. older
fig, ax1 = plt.subplots(figsize=(8, 5))
ax1.set_title('Hours listened: new vs. old albums & current year releases')
ax1.set_xlabel('Year')
ax1.set_ylabel('Hours (New vs. Old)', color='black')

x_pos = np.arange(len(df_new))
width = 0.35

# Stacked bars for new vs. old on primary axis
ax1.bar(x_pos - width/2 - 0.05, df_new['new'], width, label='New albums', color='C0', alpha=0.7)
ax1.bar(x_pos - width/2 - 0.05, df_new['old'], width, bottom=df_new['new'], label='Old albums', color='C1', alpha=0.7)

ax1.set_xticks(x_pos)
ax1.set_xticklabels(df_new['year'], rotation=45)
ax1.legend(loc='upper left')

# Stacked bars for current year releases on secondary axis
ax2 = ax1.twinx()
ax2.set_ylabel('Hours (Current Year Releases)', color='black')
ax2.bar(x_pos + width/2 + 0.05, df_current_year['current_year'], width, label='Current year', color='C2', alpha=0.7)
ax2.bar(x_pos + width/2 + 0.05, df_current_year['older'], width, bottom=df_current_year['current_year'], label='Older', color='C3', alpha=0.7)
ax2.legend(loc='upper right')

fig.tight_layout()
images.append(fig_to_b64(fig))

# Average album age per month
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
     (insert (deterred-format (f-h3 "New artists and albums per year") "\n"))
     (deterred-dashboard-print-images-base64 (elt images 0))
     (insert "\n")
     (insert (deterred-format (f-h3 "Hours listened: new vs. old albums & current year releases") "\n"))
     (deterred-dashboard-print-images-base64 (elt images 1))
     (insert "\n")
     (insert (deterred-format (f-h3 "Average album age per month") "\n"))
     (deterred-dashboard-print-images-base64 (elt images 2))
     (insert "\n")))

  (insert
   (deterred-format (f-h3 "Last discovered artists") "\n")
   (deterred-grid-print-with-org
    (alist-get 'data (alist-get 'last-discovered-artists data))
    :column-names '((artist . "Artist")
                    (discovery_date . "Discovery Date")
                    (hours_listened . "Hours Listened"))
    :max-rows 10
    :grid-button t)
   "\n"
   (deterred-format (f-h3 "Last discovered albums") "\n")
   (deterred-grid-print-with-org
    (alist-get 'data (alist-get 'last-discovered-albums data))
    :column-names '((album . "Album")
                    (album_artist . "Artist")
                    (discovery_date . "Discovery Date")
                    (hours_listened . "Hours Listened"))
    :max-rows 10
    :grid-button t)
   "\n")
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
  (let* ((comparison-data (alist-get 'data (alist-get 'artist-comparison-by-year data)))
         (first-row (car comparison-data))
         (current-year (alist-get 'current_year first-row))
         (prev-year (alist-get 'prev_year first-row)))
    (when (and comparison-data
               current-year
               prev-year
               (not (equal current-year prev-year)))
      (insert
       (deterred-format (f-h3 "Artist comparison by year")) "\n"
       (deterred-grid-print-with-org
        (deterred-utils-pick-list
         comparison-data
         '(artist current_year_hours prev_year_hours current_year_pct prev_year_pct))
        :column-names `((artist . "Artist")
                        (current_year_hours . ,(format "Hours in %s" current-year))
                        (prev_year_hours . ,(format "Hours in %s" prev-year))
                        (current_year_pct . ,(format "%% in %s" current-year))
                        (prev_year_pct . ,(format "%% in %s" prev-year)))
        :max-rows 20
        :grid-button t)
       "\n")))
  (insert
   (deterred-format (f-h3 "Hours listened to top N artists by month")) "\n")
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

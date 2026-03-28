;;; deterred-dashboard-digikam.el --- DETERRED dashboard for digikam -*- lexical-binding: t -*-

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

;; A dashboard for digiKam photos, corresponding to `deterred-digikam'.

;;; Code:
(require 'deterred-dashboard)
(require 'deterred-db)
(require 'deterred-utils)

(defclass deterred-dashboard-digikam (deterred-dashboard)
  ((name :initform "Photos (digiKam)"))
  "A DETERRED dashboard for digiKam photos.")

(cl-defmethod deterred-dashboard-default-params ((_dashboard deterred-dashboard-digikam))
  "Default parameters for the digiKam dashboard."
  '((:start-date)
    (:end-date)
    (:cameras)
    (:albums)
    (:locations)))

(cl-defmethod deterred-dashboard-render-params ((_dashboard deterred-dashboard-digikam))
  "Render the parameters section for the digiKam dashboard."
  (deterred-dashboard-widget-date
   :name "Start date"
   :key :start-date
   :kind 'from
   :display-date t)
  (insert "\n")
  (deterred-dashboard-widget-date
   :name "End date"
   :key :end-date
   :kind 'to
   :display-date t)
  (insert "\n")
  (let* ((db (deterred-db--init))
         (cameras (mapcar
                   (lambda (datum) (cons (alist-get 'camera datum)
                                         (alist-get 'camera datum)))
                   (deterred-db-select-alist
                    db "SELECT DISTINCT camera FROM digikam_photo WHERE camera IS NOT NULL ORDER BY camera")))
         (albums (mapcar
                  (lambda (datum) (cons (alist-get 'path datum)
                                        (alist-get 'id datum)))
                  (deterred-db-select-alist
                   db "SELECT id, path FROM digikam_album ORDER BY path")))
         (locations (mapcar
                     (lambda (datum) (cons (alist-get 'name datum)
                                           (alist-get 'id datum)))
                     (deterred-db-select-alist
                      db "SELECT id, name FROM location ORDER BY name"))))
    (deterred-dashboard-widget-completing-read-multiple
     :name "Camera"
     :key :cameras
     :options cameras)
    (insert "\n")
    (deterred-dashboard-widget-completing-read-multiple
     :name "Album"
     :key :albums
     :options albums)
    (insert "\n")
    (deterred-dashboard-widget-completing-read-multiple
     :name "Location"
     :key :locations
     :options locations))
  (insert "\n"))

(cl-defmethod deterred-dashboard-list-datasets ((_dashboard deterred-dashboard-digikam))
  "List datasets for the digiKam dashboard."
  '((photos-per-year (name . "Photos taken per year"))
    (photos-per-month (name . "Photos taken per month"))
    (photos-per-month-in-year (name . "Photos taken per month in year"))
    (photos-per-year-per-camera (name . "Photos per year per camera"))
    (top-albums (name . "Top albums"))
    (top-cameras (name . "Top cameras"))
    (top-locations (name . "Top locations"))
    (top-days (name . "Top days"))
    (top-weeks (name . "Top weeks"))
    (top-months (name . "Top months"))
    (camera-first-use (name . "Camera first use dates"))
    (average-camera-age-per-month (name . "Average camera age per month"))
    (numbers-data (name . "Numerical data"))))

(cl-defmethod deterred-dashboard-fetch-datasets ((_dashboard deterred-dashboard-digikam)
                                                 params)
  "Fetch datasets for the digiKam dashboard.

PARAMS is as returned by `deterred-dashboard-default-params'."
  (let* ((db (deterred-db--init))
         (numbers-data
          (deterred-db-select-template-alist
           db
           "SELECT (
  SELECT COUNT(*)
  FROM digikam_photo_with_location pwl
  WHERE 1 = 1 [[AND pwl.timestamp >= :start-date]] [[AND pwl.timestamp <= :end-date]]
  [[AND pwl.camera IN :cameras]]
  [[AND pwl.album_id IN :albums]]
  [[AND pwl.location_id IN :locations]]
) AS total_photos,
(
  SELECT COUNT(DISTINCT pwl.album_id)
  FROM digikam_photo_with_location pwl
  WHERE 1 = 1 [[AND pwl.timestamp >= :start-date]] [[AND pwl.timestamp <= :end-date]]
  [[AND pwl.camera IN :cameras]]
  [[AND pwl.album_id IN :albums]]
  [[AND pwl.location_id IN :locations]]
) AS unique_albums,
(
  SELECT COUNT(DISTINCT pwl.camera)
  FROM digikam_photo_with_location pwl
  WHERE pwl.camera IS NOT NULL
    [[AND pwl.timestamp >= :start-date]] [[AND pwl.timestamp <= :end-date]]
    [[AND pwl.camera IN :cameras]]
    [[AND pwl.album_id IN :albums]]
    [[AND pwl.location_id IN :locations]]
) AS unique_cameras,
(
  SELECT COUNT(DISTINCT pwl.location_id)
  FROM digikam_photo_with_location pwl
  WHERE pwl.location_id IS NOT NULL
    [[AND pwl.timestamp >= :start-date]] [[AND pwl.timestamp <= :end-date]]
    [[AND pwl.camera IN :cameras]]
    [[AND pwl.album_id IN :albums]]
    [[AND pwl.location_id IN :locations]]
) AS unique_locations;"
           params))
         (total-photos (alist-get 'total_photos (car numbers-data))))
    `((photos-per-year
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y', pwl.timestamp, 'unixepoch') year,
  COUNT(*) photos
FROM digikam_photo_with_location pwl
WHERE 1 = 1 [[AND pwl.timestamp >= :start-date]] [[AND pwl.timestamp <= :end-date]]
  [[AND pwl.camera IN :cameras]]
  [[AND pwl.album_id IN :albums]]
  [[AND pwl.location_id IN :locations]]
GROUP BY strftime('%Y', pwl.timestamp, 'unixepoch')"
           params))
      (photos-per-month
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y-%m', pwl.timestamp, 'unixepoch') month,
  COUNT(*) photos
FROM digikam_photo_with_location pwl
WHERE 1 = 1 [[AND pwl.timestamp >= :start-date]] [[AND pwl.timestamp <= :end-date]]
  [[AND pwl.camera IN :cameras]]
  [[AND pwl.album_id IN :albums]]
  [[AND pwl.location_id IN :locations]]
GROUP BY strftime('%Y-%m', pwl.timestamp, 'unixepoch')"
           params))
      (photos-per-month-in-year
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  CAST(strftime('%m', pwl.timestamp, 'unixepoch') AS INTEGER) month,
  CAST(COUNT(*) AS REAL) / COUNT(DISTINCT strftime('%Y', pwl.timestamp, 'unixepoch')) avg_photos
FROM digikam_photo_with_location pwl
WHERE 1 = 1 [[AND pwl.timestamp >= :start-date]] [[AND pwl.timestamp <= :end-date]]
  [[AND pwl.camera IN :cameras]]
  [[AND pwl.album_id IN :albums]]
  [[AND pwl.location_id IN :locations]]
GROUP BY CAST(strftime('%m', pwl.timestamp, 'unixepoch') AS INTEGER)
ORDER BY month ASC"
           params))
      (photos-per-year-per-camera
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y', pwl.timestamp, 'unixepoch') year,
  COALESCE(pwl.camera, 'Unknown') camera,
  COUNT(*) photos
FROM digikam_photo_with_location pwl
WHERE 1 = 1 [[AND pwl.timestamp >= :start-date]] [[AND pwl.timestamp <= :end-date]]
  [[AND pwl.camera IN :cameras]]
  [[AND pwl.album_id IN :albums]]
  [[AND pwl.location_id IN :locations]]
GROUP BY strftime('%Y', pwl.timestamp, 'unixepoch'), COALESCE(pwl.camera, 'Unknown')
ORDER BY year ASC"
           params))
      (top-albums
       . ,(deterred-utils-add-fraction
           (deterred-db-select-template-alist
            db
            "SELECT
  pwl.album_path album,
  l.name location,
  COUNT(*) photos
FROM digikam_photo_with_location pwl
LEFT JOIN location l ON l.id = pwl.location_id
WHERE 1 = 1 [[AND pwl.timestamp >= :start-date]] [[AND pwl.timestamp <= :end-date]]
  [[AND pwl.camera IN :cameras]]
  [[AND pwl.album_id IN :albums]]
  [[AND pwl.location_id IN :locations]]
GROUP BY pwl.album_id
ORDER BY photos DESC
LIMIT 20"
            params)
           total-photos
           'photos))
      (top-cameras
       . ,(deterred-utils-add-fraction
           (deterred-db-select-template-alist
            db
            "SELECT
  pwl.camera,
  COUNT(*) photos,
  strftime('%Y-%m-%d', MIN(pwl.timestamp), 'unixepoch') start_date,
  strftime('%Y-%m-%d', MAX(pwl.timestamp), 'unixepoch') end_date
FROM digikam_photo_with_location pwl
WHERE pwl.camera IS NOT NULL
  [[AND pwl.timestamp >= :start-date]] [[AND pwl.timestamp <= :end-date]]
  [[AND pwl.camera IN :cameras]]
  [[AND pwl.album_id IN :albums]]
  [[AND pwl.location_id IN :locations]]
GROUP BY pwl.camera
ORDER BY photos DESC
LIMIT 20"
            params)
           total-photos
           'photos))
      (top-locations
       . ,(deterred-utils-add-fraction
           (deterred-db-select-template-alist
            db
            "SELECT
  l.name location,
  COUNT(*) photos,
  COUNT(DISTINCT strftime('%Y-%m-%d', pwl.timestamp, 'unixepoch')) unique_days
FROM digikam_photo_with_location pwl
INNER JOIN location l ON l.id = pwl.location_id
WHERE 1 = 1 [[AND pwl.timestamp >= :start-date]] [[AND pwl.timestamp <= :end-date]]
  [[AND pwl.camera IN :cameras]]
  [[AND pwl.album_id IN :albums]]
  [[AND pwl.location_id IN :locations]]
GROUP BY l.name
ORDER BY photos DESC
LIMIT 20"
            params)
           total-photos
           'photos))
      (top-days
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y-%m-%d', pwl.timestamp, 'unixepoch') day,
  l.name location,
  COUNT(*) photos
FROM digikam_photo_with_location pwl
LEFT JOIN location l ON l.id = pwl.location_id
WHERE 1 = 1 [[AND pwl.timestamp >= :start-date]] [[AND pwl.timestamp <= :end-date]]
  [[AND pwl.camera IN :cameras]]
  [[AND pwl.album_id IN :albums]]
  [[AND pwl.location_id IN :locations]]
GROUP BY strftime('%Y-%m-%d', pwl.timestamp, 'unixepoch')
ORDER BY photos DESC
LIMIT 20"
           params))
      (top-weeks
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y-%W', pwl.timestamp, 'unixepoch') week,
  COUNT(*) photos
FROM digikam_photo_with_location pwl
WHERE 1 = 1 [[AND pwl.timestamp >= :start-date]] [[AND pwl.timestamp <= :end-date]]
  [[AND pwl.camera IN :cameras]]
  [[AND pwl.album_id IN :albums]]
  [[AND pwl.location_id IN :locations]]
GROUP BY strftime('%Y-%W', pwl.timestamp, 'unixepoch')
ORDER BY photos DESC
LIMIT 20"
           params))
      (top-months
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y-%m', pwl.timestamp, 'unixepoch') month,
  COUNT(*) photos
FROM digikam_photo_with_location pwl
WHERE 1 = 1 [[AND pwl.timestamp >= :start-date]] [[AND pwl.timestamp <= :end-date]]
  [[AND pwl.camera IN :cameras]]
  [[AND pwl.album_id IN :albums]]
  [[AND pwl.location_id IN :locations]]
GROUP BY strftime('%Y-%m', pwl.timestamp, 'unixepoch')
ORDER BY photos DESC
LIMIT 20"
           params))
      (camera-first-use
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  camera,
  MIN(timestamp) first_use
FROM digikam_photo_with_location
WHERE camera IS NOT NULL
GROUP BY camera"
           params))
      (average-camera-age-per-month
       . ,(deterred-db-select-template-alist
           db
           "WITH camera_first_use AS (
  SELECT
    camera,
    CAST(STRFTIME('%Y', MIN(timestamp), 'unixepoch') AS INTEGER) * 12 +
    CAST(STRFTIME('%m', MIN(timestamp), 'unixepoch') AS INTEGER) start_month
  FROM digikam_photo_with_location
  WHERE camera IS NOT NULL
  GROUP BY camera
), photos_per_month AS (
  SELECT
    STRFTIME('%Y-%m', timestamp, 'unixepoch') month,
    COUNT(*) photos_count
  FROM digikam_photo_with_location
  WHERE camera IS NOT NULL
    [[AND timestamp >= :start-date]] [[AND timestamp <= :end-date]]
    [[AND camera IN :cameras]]
    [[AND album_id IN :albums]]
    [[AND location_id IN :locations]]
  GROUP BY STRFTIME('%Y-%m', timestamp, 'unixepoch')
), camera_data AS (
  SELECT
    (
      (
        CAST(STRFTIME('%Y', timestamp, 'unixepoch') AS INTEGER) * 12 +
        CAST(STRFTIME('%m', timestamp, 'unixepoch') AS INTEGER)
      ) - cfu.start_month
    ) camera_age_months,
    COUNT(*) / CAST(ppm.photos_count AS REAL) fraction,
    STRFTIME('%Y-%m', timestamp, 'unixepoch') month,
    pwl.camera
  FROM digikam_photo_with_location pwl
  INNER JOIN camera_first_use cfu ON cfu.camera = pwl.camera
  INNER JOIN photos_per_month ppm ON ppm.month = STRFTIME('%Y-%m', pwl.timestamp, 'unixepoch')
  WHERE pwl.camera IS NOT NULL
    [[AND pwl.timestamp >= :start-date]] [[AND pwl.timestamp <= :end-date]]
    [[AND pwl.camera IN :cameras]]
    [[AND pwl.album_id IN :albums]]
    [[AND pwl.location_id IN :locations]]
  GROUP BY STRFTIME('%Y-%m', pwl.timestamp, 'unixepoch'), pwl.camera
  ORDER BY month, fraction DESC
)
SELECT SUM(camera_age_months * fraction) age, month
FROM camera_data
GROUP BY month
ORDER BY month ASC"
           params))
      (numbers-data . ,numbers-data))))

(cl-defmethod deterred-dashboard-render-results ((_dashboard deterred-dashboard-digikam)
                                                 _params data)
  "Render DATA for the digiKam dashboard."
  (insert
   (deterred-format
    "I have "
    (f-ace (f (f-num (f-acc "data->'numbers-data->'data[0]->'total_photos")))
           'bold)
    " photos in "
    (f-ace (f (f-num (f-acc "data->'numbers-data->'data[0]->'unique_albums")))
           'bold)
    " albums, taken with "
    (f-ace (f (f-num (f-acc "data->'numbers-data->'data[0]->'unique_cameras")))
           'bold)
    " cameras, across "
    (f-ace (f (f-num (f-acc "data->'numbers-data->'data[0]->'unique_locations")))
           'bold)
    " locations.\n\n"
    (f-h2 "Top over all time") "\n"
    (f-h3 "Top albums") "\n")
   (deterred-grid-print-with-org
    (alist-get 'data (alist-get 'top-albums data))
    :column-names '((album . "Album") (location . "Location") (photos . "Photos") (fraction . "%"))
    :max-rows 10
    :max-column-width 30
    :grid-button t)
   "\n"
   (deterred-format (f-h3 "Top cameras") "\n")
   (deterred-grid-print-with-org
    (alist-get 'data (alist-get 'top-cameras data))
    :column-names '((camera . "Camera") (photos . "Photos") (start_date . "Start") (end_date . "End") (fraction . "%"))
    :max-rows 10
    :grid-button t)
   "\n"
   (deterred-format (f-h3 "Top locations") "\n")
   (deterred-grid-print-with-org
    (alist-get 'data (alist-get 'top-locations data))
    :column-names '((location . "Location") (photos . "Photos") (unique_days . "Days") (fraction . "%"))
    :max-rows 10
    :grid-button t)
   "\n"
   (deterred-format (f-h2 "Dynamics over time") "\n"))
  (deterred-dashboard-exec-python
   :python-code
   "from matplotlib import pyplot as plt
from matplotlib.ticker import MaxNLocator
from deterred import fig_to_b64

import pandas as pd
import numpy as np

import json
import os
import base64
import io

data = json.loads(input())
df_y = pd.DataFrame(data['photos-per-year']['data'])
df_m = pd.DataFrame(data['photos-per-month']['data'])
df_miy = pd.DataFrame(data['photos-per-month-in-year']['data'])
df_c = pd.DataFrame(data['photos-per-year-per-camera']['data'])
df_cp = df_c.pivot(index='year', columns='camera', values='photos').fillna(0)
df_age = pd.DataFrame(data['average-camera-age-per-month']['data'])
# Apply 3-month rolling average
df_age['age_smooth'] = df_age['age'].rolling(window=3, center=True).mean()

images = []

fig, ax = plt.subplots(figsize=(8, 5))
df_y.plot(ax=ax, kind='bar', x='year', y='photos')
ax.set_title('Photos taken per year')
for container in ax.containers:
    ax.bar_label(container, fmt='%.0f')
images.append(fig_to_b64(fig))

fig, ax = plt.subplots(figsize=(8, 5))
df_m.plot(ax=ax, kind='bar', x='month', y='photos')
ax.set_title('Photos taken per month')
if len(df_m) > 30:
    ax.xaxis.set_major_locator(MaxNLocator(nbins=40))
if len(df_m) < 30:
    for container in ax.containers:
        ax.bar_label(container, fmt='%.0f')
images.append(fig_to_b64(fig))

fig, ax = plt.subplots(figsize=(8, 5))
month_names = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
df_miy['month_name'] = df_miy['month'].apply(lambda x: month_names[int(x)-1])
df_miy.plot(ax=ax, kind='bar', x='month_name', y='avg_photos')
ax.set_title('Photos taken per month in year (averaged across all years)')
ax.set_xlabel('Month')
for container in ax.containers:
    ax.bar_label(container, fmt='%.1f')
images.append(fig_to_b64(fig))

fig, ax = plt.subplots(figsize=(8, 5))
df_cp.plot(ax=ax, kind='bar', stacked=True)
ax.set_title('Photos per year per camera')
ax.legend(loc='upper center', bbox_to_anchor=(0.5, -0.15), ncol=3)
plt.tight_layout()
images.append(fig_to_b64(fig))

fig, ax = plt.subplots(figsize=(8, 5))
ax.set_title('Average camera age (in months) per month (3-month rolling avg)')
x = np.arange(len(df_age))
y = df_age.age_smooth.dropna()
x_smooth = x[df_age['age_smooth'].notna()]
z = np.polyfit(x_smooth, y, 1)
p = np.poly1d(z)
ax.plot(x_smooth, p(x_smooth), '--', color='red', linewidth=1, label='trend')
df_age.plot(ax=ax, kind='line', x='month', y='age_smooth', legend=False)
ax.legend()
images.append(fig_to_b64(fig))

print(json.dumps(images))"
   :input data
   :on-success
   (lambda (images)
     (insert (deterred-format (f-h3 "Photos taken per year") "\n"))
     (deterred-dashboard-print-images-base64 (elt images 0))
     (insert "\n")
     (insert (deterred-format (f-h3 "Photos taken per month") "\n"))
     (deterred-dashboard-print-images-base64 (elt images 1))
     (insert "\n")
     (insert (deterred-format (f-h3 "Photos taken per month in year") "\n"))
     (deterred-dashboard-print-images-base64 (elt images 2))
     (insert "\n")
     (insert (deterred-format (f-h3 "Photos per year per camera") "\n"))
     (deterred-dashboard-print-images-base64 (elt images 3))
     (insert "\n")
     (insert (deterred-format (f-h3 "Average camera age per month") "\n"))
     (deterred-dashboard-print-images-base64 (elt images 4))
     (insert "\n")))
  (insert
   (deterred-format (f-h2 "Top periods") "\n"
                    (f-h3 "Top months") "\n")
   (deterred-grid-print-with-org
    (alist-get 'data (alist-get 'top-months data))
    :column-names '((month . "Month") (photos . "Photos"))
    :max-rows 10
    :grid-button t)
   "\n"
   (deterred-format (f-h3 "Top weeks") "\n")
   (deterred-grid-print-with-org
    (alist-get 'data (alist-get 'top-weeks data))
    :column-names '((week . "Week") (photos . "Photos"))
    :max-rows 10
    :grid-button t)
   "\n"
   (deterred-format (f-h3 "Top days") "\n")
   (deterred-grid-print-with-org
    (alist-get 'data (alist-get 'top-days data))
    :column-names '((day . "Day") (location . "Location") (photos . "Photos"))
    :max-rows 10
    :grid-button t)
   "\n"))

(provide 'deterred-dashboard-digikam)
;;; deterred-dashboard-digikam.el ends here

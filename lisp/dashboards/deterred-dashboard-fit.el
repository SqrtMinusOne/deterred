;;; deterred-dashboard-fit.el --- DETERRED dashboard for FIT activities -*- lexical-binding: t -*-

;; Copyright (C) 2026 Korytov Pavel

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
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A dashboard for FIT activities, corresponding to `deterred-fit'.

;;; Code:
(require 'deterred-dashboard)
(require 'deterred-db)
(require 'deterred-format)
(require 'deterred-grid)

(defclass deterred-dashboard-fit (deterred-dashboard)
  ((name :initform "FIT"))
  "A DETERRED dashboard for FIT activities.")

(cl-defmethod deterred-dashboard-default-params ((_dashboard deterred-dashboard-fit))
  "Default parameters for the FIT dashboard."
  '((:start-date)
    (:end-date)))

(cl-defmethod deterred-dashboard-render-params ((_dashboard deterred-dashboard-fit))
  "Render the parameters section for the FIT dashboard."
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
  (insert "\n"))

(cl-defmethod deterred-dashboard-list-datasets ((_dashboard deterred-dashboard-fit))
  "List datasets for the FIT dashboard."
  '((activities-per-week (name . "Activities per week"))
    (activities-per-month (name . "Activities per month"))
    (distance-per-week (name . "Distance covered per week"))
    (distance-per-month (name . "Distance covered per month"))
    (time-per-month (name . "Time spent per month"))
    (activities-vs-transport-per-week
     (name . "Activities vs transport trips per week"))
    (activities-vs-podcasts-per-week
     (name . "Activities vs podcasts listened per week"))
    (numbers-data (name . "Numerical data"))))

(cl-defmethod deterred-dashboard-fetch-datasets ((_dashboard deterred-dashboard-fit)
                                                 params)
  "Fetch datasets for the FIT dashboard.

PARAMS is as returned by `deterred-dashboard-default-params'."
  (let* ((db (deterred-db--init))
         (where "WHERE 1 = 1
  [[AND start_timestamp >= :start-date]]
  [[AND start_timestamp <= :end-date]]")
         (week-start
          "date(start_timestamp, 'unixepoch', 'weekday 0', '-6 days')")
         (transport-week-start
          "date(timestamp, 'unixepoch', 'weekday 0', '-6 days')")
         (podcast-week-start
          "date(timestamp, 'unixepoch', 'weekday 0', '-6 days')")
         (week-series
          (concat "WITH RECURSIVE
filtered AS (
  SELECT * FROM fit_activity
" where "
),
bounds AS (
  SELECT
    COALESCE(:start-date, MIN(start_timestamp)) start_timestamp,
    COALESCE(:end-date, MAX(start_timestamp)) end_timestamp
  FROM filtered
),
weeks(week) AS (
  SELECT date(start_timestamp, 'unixepoch', 'weekday 0', '-6 days')
  FROM bounds
  WHERE start_timestamp IS NOT NULL
    AND end_timestamp IS NOT NULL
  UNION ALL
  SELECT date(week, '+7 days')
  FROM weeks, bounds
  WHERE week < date(end_timestamp, 'unixepoch', 'weekday 0', '-6 days')
),
activity_by_week AS (
  SELECT
    " week-start " week,
    COUNT(*) activities,
    CAST(COALESCE(SUM(distance), 0) / 1000.0 * 100 AS integer) / 100.0 distance_km
  FROM filtered
  GROUP BY " week-start "
)"))
         (month-series
          (concat "WITH RECURSIVE
filtered AS (
  SELECT * FROM fit_activity
" where "
),
bounds AS (
  SELECT
    COALESCE(:start-date, MIN(start_timestamp)) start_timestamp,
    COALESCE(:end-date, MAX(start_timestamp)) end_timestamp
  FROM filtered
),
months(month) AS (
  SELECT date(start_timestamp, 'unixepoch', 'start of month')
  FROM bounds
  WHERE start_timestamp IS NOT NULL
    AND end_timestamp IS NOT NULL
  UNION ALL
  SELECT date(month, '+1 month')
  FROM months, bounds
  WHERE month < date(end_timestamp, 'unixepoch', 'start of month')
),
activity_by_month AS (
  SELECT
    strftime('%Y-%m', start_timestamp, 'unixepoch') month,
    COUNT(*) activities,
    CAST(COALESCE(SUM(distance), 0) / 1000.0 * 100 AS integer) / 100.0 distance_km,
    CAST(COALESCE(SUM(end_timestamp - start_timestamp), 0) / 3600.0 * 100 AS integer) / 100.0 hours
  FROM filtered
  GROUP BY strftime('%Y-%m', start_timestamp, 'unixepoch')
)"))
         (overlap-week-series
          (concat "WITH RECURSIVE
fit_filtered AS (
  SELECT * FROM fit_activity
" where "
),
transport_filtered AS (
  SELECT * FROM transport_trips
  WHERE 1 = 1
    [[AND timestamp >= :start-date]]
    [[AND timestamp <= :end-date]]
),
bounds AS (
  SELECT
    (SELECT MAX(timestamp) FROM (
      SELECT MIN(start_timestamp) timestamp FROM fit_filtered
      UNION ALL
      SELECT MIN(timestamp) timestamp FROM transport_filtered
    )) start_timestamp,
    (SELECT MAX(timestamp) FROM (
      SELECT MAX(start_timestamp) timestamp FROM fit_filtered
      UNION ALL
      SELECT MAX(timestamp) timestamp FROM transport_filtered
    )) end_timestamp,
    (SELECT MIN(start_timestamp) FROM fit_filtered) fit_start_timestamp,
    (SELECT MIN(timestamp) FROM transport_filtered) transport_start_timestamp
),
weeks(week) AS (
  SELECT date(start_timestamp, 'unixepoch', 'weekday 0', '-6 days')
  FROM bounds
  WHERE fit_start_timestamp IS NOT NULL
    AND transport_start_timestamp IS NOT NULL
    AND start_timestamp <= end_timestamp
  UNION ALL
  SELECT date(week, '+7 days')
  FROM weeks, bounds
  WHERE week < date(end_timestamp, 'unixepoch', 'weekday 0', '-6 days')
),
activity_by_week AS (
  SELECT
    " week-start " week,
    COUNT(*) activities
  FROM fit_filtered
  GROUP BY " week-start "
),
transport_by_week AS (
  SELECT
    " transport-week-start " week,
    COUNT(*) transport_trips
  FROM transport_filtered
  GROUP BY " transport-week-start "
)"))
         (podcast-overlap-week-series
          (concat "WITH RECURSIVE
fit_filtered AS (
  SELECT * FROM fit_activity
" where "
),
podcast_filtered AS (
  SELECT * FROM podcasts_listened
  WHERE 1 = 1
    [[AND timestamp >= :start-date]]
    [[AND timestamp <= :end-date]]
),
bounds AS (
  SELECT
    (SELECT MAX(timestamp) FROM (
      SELECT MIN(start_timestamp) timestamp FROM fit_filtered
      UNION ALL
      SELECT MIN(timestamp) timestamp FROM podcast_filtered
    )) start_timestamp,
    (SELECT MAX(timestamp) FROM (
      SELECT MAX(start_timestamp) timestamp FROM fit_filtered
      UNION ALL
      SELECT MAX(timestamp) timestamp FROM podcast_filtered
    )) end_timestamp,
    (SELECT MIN(start_timestamp) FROM fit_filtered) fit_start_timestamp,
    (SELECT MIN(timestamp) FROM podcast_filtered) podcast_start_timestamp
),
weeks(week) AS (
  SELECT date(start_timestamp, 'unixepoch', 'weekday 0', '-6 days')
  FROM bounds
  WHERE fit_start_timestamp IS NOT NULL
    AND podcast_start_timestamp IS NOT NULL
    AND start_timestamp <= end_timestamp
  UNION ALL
  SELECT date(week, '+7 days')
  FROM weeks, bounds
  WHERE week < date(end_timestamp, 'unixepoch', 'weekday 0', '-6 days')
),
activity_by_week AS (
  SELECT
    " week-start " week,
    COUNT(*) activities
  FROM fit_filtered
  GROUP BY " week-start "
),
podcast_by_week AS (
  SELECT
    " podcast-week-start " week,
    COUNT(*) podcasts_listened
  FROM podcast_filtered
  GROUP BY " podcast-week-start "
)")))
    `((activities-per-week
       . ,(deterred-db-select-template-alist
           db
           (concat week-series "
SELECT
  weeks.week,
  COALESCE(activity_by_week.activities, 0) activities
FROM weeks
LEFT JOIN activity_by_week ON activity_by_week.week = weeks.week
ORDER BY weeks.week")
           params))
      (activities-per-month
       . ,(deterred-db-select-template-alist
           db
           (concat month-series "
SELECT
  strftime('%Y-%m', months.month) month,
  COALESCE(activity_by_month.activities, 0) activities
FROM months
LEFT JOIN activity_by_month
  ON activity_by_month.month = strftime('%Y-%m', months.month)
ORDER BY months.month")
           params))
      (distance-per-week
       . ,(deterred-db-select-template-alist
           db
           (concat week-series "
SELECT
  weeks.week,
  COALESCE(activity_by_week.distance_km, 0) distance_km
FROM weeks
LEFT JOIN activity_by_week ON activity_by_week.week = weeks.week
ORDER BY weeks.week")
           params))
      (distance-per-month
       . ,(deterred-db-select-template-alist
           db
           (concat month-series "
SELECT
  strftime('%Y-%m', months.month) month,
  COALESCE(activity_by_month.distance_km, 0) distance_km
FROM months
LEFT JOIN activity_by_month
  ON activity_by_month.month = strftime('%Y-%m', months.month)
ORDER BY months.month")
           params))
      (time-per-month
       . ,(deterred-db-select-template-alist
           db
           (concat month-series "
SELECT
  strftime('%Y-%m', months.month) month,
  COALESCE(activity_by_month.hours, 0) hours
FROM months
LEFT JOIN activity_by_month
  ON activity_by_month.month = strftime('%Y-%m', months.month)
ORDER BY months.month")
           params))
      (activities-vs-transport-per-week
       . ,(deterred-db-select-template-alist
           db
           (concat overlap-week-series "
SELECT
  weeks.week,
  COALESCE(activity_by_week.activities, 0) activities,
  COALESCE(transport_by_week.transport_trips, 0) transport_trips
FROM weeks
LEFT JOIN activity_by_week ON activity_by_week.week = weeks.week
LEFT JOIN transport_by_week ON transport_by_week.week = weeks.week
ORDER BY weeks.week")
           params))
      (activities-vs-podcasts-per-week
       . ,(deterred-db-select-template-alist
           db
           (concat podcast-overlap-week-series "
SELECT
  weeks.week,
  COALESCE(activity_by_week.activities, 0) activities,
  COALESCE(podcast_by_week.podcasts_listened, 0) podcasts_listened
FROM weeks
LEFT JOIN activity_by_week ON activity_by_week.week = weeks.week
LEFT JOIN podcast_by_week ON podcast_by_week.week = weeks.week
ORDER BY weeks.week")
           params))
      (numbers-data
       . ,(deterred-db-select-template-alist
           db
           (concat "SELECT
  COUNT(*) total_activities,
  CAST(COALESCE(SUM(distance), 0) / 1000.0 * 100 AS integer) / 100.0 total_distance_km,
  CAST(COALESCE(SUM(end_timestamp - start_timestamp), 0) / 3600.0 * 100 AS integer) / 100.0 total_hours,
  COALESCE(MAX(sport_name), '') sport_name
FROM fit_activity
" where)
           params)))))

(cl-defmethod deterred-dashboard-render-results ((_dashboard deterred-dashboard-fit)
                                                 _params data)
  "Render DATA for the FIT dashboard."
  (insert
   (deterred-format
    "There are "
    (f-ace (f (f-num (f-acc "data->'numbers-data->'data[0]->'total_activities")))
           'bold)
    " FIT activities, covering "
    (f-ace (f (f-num (f-acc "data->'numbers-data->'data[0]->'total_distance_km")))
           'bold)
    " km in "
    (f-ace (f (f-num (f-acc "data->'numbers-data->'data[0]->'total_hours")))
           'bold)
    " hours.\n\n"
    (f-h2 "Activities") "\n"))
  (deterred-dashboard-exec-python
   :python-code
   "from matplotlib import pyplot as plt
from matplotlib.ticker import MaxNLocator
from deterred import fig_to_b64

import pandas as pd
import json

data = json.loads(input())
images = []

def add_bar(dataset, x_col, y_col, title, xlabel, ylabel, fmt='%.0f'):
    df = pd.DataFrame(data[dataset]['data'])
    if df.empty:
        images.append(None)
        return
    fig, ax = plt.subplots(figsize=(10, 5))
    df.plot(ax=ax, kind='bar', x=x_col, y=y_col, legend=False)
    ax.set_title(title)
    ax.set_xlabel(xlabel)
    ax.set_ylabel(ylabel)
    if len(df) > 20:
        ax.xaxis.set_major_locator(MaxNLocator(nbins=20))
    for container in ax.containers:
        ax.bar_label(container, fmt=fmt)
    plt.xticks(rotation=45, ha='right')
    images.append(fig_to_b64(fig))

add_bar('activities-per-week', 'week', 'activities',
        'Activities per week', 'Week', 'Activities')
add_bar('activities-per-month', 'month', 'activities',
        'Activities per month', 'Month', 'Activities')
add_bar('distance-per-week', 'week', 'distance_km',
        'Distance covered per week', 'Week', 'Distance (km)', '%.1f')
add_bar('distance-per-month', 'month', 'distance_km',
        'Distance covered per month', 'Month', 'Distance (km)', '%.1f')
add_bar('time-per-month', 'month', 'hours',
        'Time spent per month', 'Month', 'Hours', '%.1f')

df_overlap = pd.DataFrame(data['activities-vs-transport-per-week']['data'])
if not df_overlap.empty:
    fig, ax = plt.subplots(figsize=(10, 5))
    df_overlap.plot(
        ax=ax,
        kind='line',
        x='week',
        y=['activities', 'transport_trips'],
        marker='o')
    ax.set_title('Activities vs transport trips per week')
    ax.set_xlabel('Week')
    ax.set_ylabel('Count')
    ax.legend(['FIT activities', 'Transport trips'])
    if len(df_overlap) > 20:
        ax.xaxis.set_major_locator(MaxNLocator(nbins=20))
    plt.xticks(rotation=45, ha='right')
    images.append(fig_to_b64(fig))
else:
    images.append(None)

df_podcasts = pd.DataFrame(data['activities-vs-podcasts-per-week']['data'])
if not df_podcasts.empty:
    fig, ax = plt.subplots(figsize=(10, 5))
    df_podcasts.plot(
        ax=ax,
        kind='line',
        x='week',
        y=['activities', 'podcasts_listened'],
        marker='o')
    ax.set_title('Activities vs podcasts listened per week')
    ax.set_xlabel('Week')
    ax.set_ylabel('Count')
    ax.legend(['FIT activities', 'Podcasts listened'])
    if len(df_podcasts) > 20:
        ax.xaxis.set_major_locator(MaxNLocator(nbins=20))
    plt.xticks(rotation=45, ha='right')
    images.append(fig_to_b64(fig))
else:
    images.append(None)

print(json.dumps(images))"
   :input data
   :on-success
   (lambda (images)
     (when (elt images 0)
       (insert (deterred-format (f-h3 "Activities per week") "\n"))
       (deterred-dashboard-print-images-base64 (elt images 0))
       (insert "\n"))
     (when (elt images 1)
       (insert (deterred-format (f-h3 "Activities per month") "\n"))
       (deterred-dashboard-print-images-base64 (elt images 1))
       (insert "\n"))
     (insert (deterred-format (f-h2 "Distance") "\n"))
     (when (elt images 2)
       (insert (deterred-format (f-h3 "Distance covered per week") "\n"))
       (deterred-dashboard-print-images-base64 (elt images 2))
       (insert "\n"))
     (when (elt images 3)
       (insert (deterred-format (f-h3 "Distance covered per month") "\n"))
       (deterred-dashboard-print-images-base64 (elt images 3))
       (insert "\n"))
     (insert (deterred-format (f-h2 "Time") "\n"))
     (when (elt images 4)
       (insert (deterred-format (f-h3 "Time spent per month") "\n"))
       (deterred-dashboard-print-images-base64 (elt images 4))
       (insert "\n"))
     (when (elt images 5)
       (insert (deterred-format
                (f-h2 "Transport comparison") "\n"
                (f-h3 "Activities vs transport trips per week") "\n"))
       (deterred-dashboard-print-images-base64 (elt images 5))
       (insert "\n"))
     (when (elt images 6)
       (insert (deterred-format
                (f-h2 "Podcast comparison") "\n"
                (f-h3 "Activities vs podcasts listened per week") "\n"))
       (deterred-dashboard-print-images-base64 (elt images 6))
       (insert "\n"))))
  (insert
   (deterred-format (f-h2 "Tables") "\n"
                    (f-h3 "Monthly activity") "\n")
   (deterred-grid-print-with-org
    (alist-get 'data (alist-get 'activities-per-month data))
    :column-names '((month . "Month") (activities . "Activities"))
    :grid-button t)
   "\n"
   (deterred-format (f-h3 "Monthly distance") "\n")
   (deterred-grid-print-with-org
    (alist-get 'data (alist-get 'distance-per-month data))
    :column-names '((month . "Month") (distance_km . "Distance, km"))
    :grid-button t)
   "\n"
   (deterred-format (f-h3 "Monthly time") "\n")
   (deterred-grid-print-with-org
    (alist-get 'data (alist-get 'time-per-month data))
    :column-names '((month . "Month") (hours . "Hours"))
    :grid-button t)
   "\n"))

(provide 'deterred-dashboard-fit)
;;; deterred-dashboard-fit.el ends here

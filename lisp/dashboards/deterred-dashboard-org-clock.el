;;; deterred-dashboard-org-clock.el --- DETERRED dashboard for Org Clock -*- lexical-binding: t -*-

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

;; A dashboard for Org Clock, corresponding to `deterred-org-clock'.

;;; Code:
(require 'deterred-dashboard)
(require 'deterred-db)
(require 'deterred-format)
(require 'deterred-grid)

(defconst deterred-dashboard-org-clock-free-weekend-threshold-seconds
  (* 30 60)
  "Maximum clocked duration for a weekend day to be considered free.

The comparison is strict: a day with exactly this many clocked
seconds is not free.")

(defconst deterred-dashboard-org-clock--daily-cte
  "WITH RECURSIVE
bounds AS (
  SELECT
    COALESCE(:start-date, MIN(start_timestamp)) start_timestamp,
    COALESCE(:end-date + 1, MAX(end_timestamp)) end_timestamp
  FROM org_clock_item
  WHERE end_timestamp > start_timestamp
),
hours(hour) AS (
  SELECT 0
  UNION ALL
  SELECT hour + 1
  FROM hours
  WHERE hour < 23
),
filtered_clocks AS (
  SELECT
    oci.headline_id,
    oh.title,
    oh.file_name,
    COALESCE(NULLIF(oh.category, ''), 'Uncategorized') category,
    MAX(
      oci.start_timestamp,
      COALESCE(:start-date, oci.start_timestamp)
    ) start_timestamp,
    MIN(
      oci.end_timestamp,
      COALESCE(:end-date + 1, oci.end_timestamp)
    ) end_timestamp
  FROM org_clock_item oci
  INNER JOIN org_headline oh ON oh.id = oci.headline_id
  WHERE oci.end_timestamp > oci.start_timestamp
    [[AND oci.end_timestamp > :start-date]]
    [[AND oci.start_timestamp <= :end-date]]
),
days(day) AS (
  SELECT date(start_timestamp, 'unixepoch', 'localtime')
  FROM bounds
  WHERE start_timestamp IS NOT NULL
    AND end_timestamp IS NOT NULL
    AND start_timestamp < end_timestamp
  UNION ALL
  SELECT date(day, '+1 day')
  FROM days, bounds
  WHERE day < date(end_timestamp - 1, 'unixepoch', 'localtime')
),
clock_days(
  headline_id,
  title,
  file_name,
  category,
  start_timestamp,
  end_timestamp,
  day
) AS (
  SELECT
    headline_id,
    title,
    file_name,
    category,
    start_timestamp,
    end_timestamp,
    date(start_timestamp, 'unixepoch', 'localtime')
  FROM filtered_clocks
  WHERE start_timestamp < end_timestamp
  UNION ALL
  SELECT
    headline_id,
    title,
    file_name,
    category,
    start_timestamp,
    end_timestamp,
    date(day, '+1 day')
  FROM clock_days
  WHERE day < date(end_timestamp - 1, 'unixepoch', 'localtime')
),
clock_segments AS (
  SELECT
    headline_id,
    title,
    file_name,
    category,
    day,
    MIN(
      end_timestamp,
      unixepoch(date(day, '+1 day'), 'utc')
    ) - MAX(
      start_timestamp,
      unixepoch(day, 'utc')
    ) clocked_seconds
  FROM clock_days
),
daily_clocked AS (
  SELECT day, SUM(clocked_seconds) clocked_seconds
  FROM clock_segments
  GROUP BY day
),
daily_base AS (
  SELECT
    days.day,
    substr(days.day, 1, 7) month,
    substr(days.day, 1, 4) || '-Q' ||
      CASE
        WHEN CAST(substr(days.day, 6, 2) AS INTEGER) <= 3 THEN 1
        WHEN CAST(substr(days.day, 6, 2) AS INTEGER) <= 6 THEN 2
        WHEN CAST(substr(days.day, 6, 2) AS INTEGER) <= 9 THEN 3
        ELSE 4
      END quarter,
    CASE CAST(strftime('%w', days.day) AS INTEGER)
      WHEN 0 THEN 7
      ELSE CAST(strftime('%w', days.day) AS INTEGER)
    END day_of_week_number,
    CASE CAST(strftime('%w', days.day) AS INTEGER)
      WHEN 0 THEN 'Sunday'
      WHEN 1 THEN 'Monday'
      WHEN 2 THEN 'Tuesday'
      WHEN 3 THEN 'Wednesday'
      WHEN 4 THEN 'Thursday'
      WHEN 5 THEN 'Friday'
      WHEN 6 THEN 'Saturday'
    END day_of_week,
    CASE
      WHEN strftime('%w', days.day) IN ('0', '6') THEN 1
      ELSE 0
    END is_weekend,
    COALESCE(daily_clocked.clocked_seconds, 0) clocked_seconds
  FROM days
  LEFT JOIN daily_clocked USING (day)
),
daily AS (
  SELECT
    daily_base.*,
    CASE
      WHEN is_weekend = 1
       AND clocked_seconds < :free-weekend-threshold THEN 1
      ELSE 0
    END is_free_weekend,
    CASE
      WHEN is_weekend = 1
       AND clocked_seconds >= :free-weekend-threshold THEN 1
      ELSE 0
    END is_non_free_weekend
  FROM daily_base
)"
  "Common SQL dataset for splitting Org Clock intervals into local days.")

(defclass deterred-dashboard-org-clock (deterred-dashboard)
  ((name :initform "Org Clock"))
  "A DETERRED dashboard for Org Clock.")

(cl-defmethod deterred-dashboard-default-params
  ((_dashboard deterred-dashboard-org-clock))
  "Default parameters for the Org Clock dashboard."
  '((:start-date)
    (:end-date)))

(cl-defmethod deterred-dashboard-render-params
  ((_dashboard deterred-dashboard-org-clock))
  "Render the parameters section for the Org Clock dashboard."
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

(cl-defmethod deterred-dashboard-list-datasets
  ((_dashboard deterred-dashboard-org-clock))
  "List datasets for the Org Clock dashboard."
  '((hours-per-month (name . "Clocked hours per month"))
    (hours-by-day-of-week
     (name . "Average clocked hours by day of week"))
    (days-worked-by-hour (name . "Days worked by hour"))
    (hours-by-category (name . "Clocked hours by category"))
    (free-weekends-per-month (name . "Free wekends per month"))
    (free-weekends-per-quarter (name . "Free wekends per quarter"))
    (top-headlines (name . "Top headlines"))
    (numbers-data (name . "Numerical data"))))

(defun deterred-dashboard-org-clock--select (db params query)
  "Run dashboard QUERY against DB using PARAMS and the daily dataset."
  (deterred-db-select-template-alist
   db
   (concat deterred-dashboard-org-clock--daily-cte "\n" query)
   (cons
    (cons :free-weekend-threshold
          deterred-dashboard-org-clock-free-weekend-threshold-seconds)
    params)))

(defun deterred-dashboard-org-clock--fetch-datasets (db params)
  "Fetch Org Clock dashboard datasets from DB using PARAMS."
  `((hours-per-month
     . ,(deterred-dashboard-org-clock--select
         db params
         "SELECT
  month,
  ROUND(SUM(clocked_seconds) / 3600.0, 2) hours
FROM daily
GROUP BY month
ORDER BY month ASC"))
    (hours-by-day-of-week
     . ,(deterred-dashboard-org-clock--select
         db params
         "SELECT
  day_of_week,
  day_of_week_number,
  ROUND(AVG(clocked_seconds) / 3600.0, 2) average_hours,
  ROUND(SUM(clocked_seconds) / 3600.0, 2) total_hours,
  COUNT(*) days
FROM daily
GROUP BY day_of_week_number, day_of_week
ORDER BY day_of_week_number ASC"))
    (days-worked-by-hour
     . ,(deterred-dashboard-org-clock--select
         db params
         "SELECT
  hours.hour,
  COUNT(DISTINCT clock_days.day) worked_days,
  (SELECT COUNT(*)
   FROM daily
   WHERE clocked_seconds > 0) tracked_days
FROM hours
LEFT JOIN clock_days
  ON clock_days.start_timestamp < unixepoch(
    clock_days.day,
    printf('+%d hours', hours.hour + 1),
    'utc'
  )
 AND clock_days.end_timestamp > unixepoch(
    clock_days.day,
    printf('+%d hours', hours.hour),
    'utc'
  )
GROUP BY hours.hour
ORDER BY hours.hour ASC"))
    (hours-by-category
     . ,(deterred-dashboard-org-clock--select
         db params
         "SELECT
  category,
  ROUND(SUM(end_timestamp - start_timestamp) / 3600.0, 2) hours
FROM filtered_clocks
WHERE start_timestamp < end_timestamp
GROUP BY category
ORDER BY hours ASC"))
    (free-weekends-per-month
     . ,(deterred-dashboard-org-clock--select
         db params
         "SELECT
  month,
  SUM(is_weekend) weekend_days,
  SUM(is_free_weekend) free_weekends,
  SUM(is_non_free_weekend) non_free_weekends,
  ROUND(
    SUM(CASE WHEN is_weekend = 1 THEN clocked_seconds ELSE 0 END) / 3600.0,
    2
  ) weekend_hours
FROM daily
GROUP BY month
ORDER BY month ASC"))
    (free-weekends-per-quarter
     . ,(deterred-dashboard-org-clock--select
         db params
         "SELECT
  quarter,
  SUM(is_weekend) weekend_days,
  SUM(is_free_weekend) free_weekends,
  SUM(is_non_free_weekend) non_free_weekends,
  ROUND(
    SUM(CASE WHEN is_weekend = 1 THEN clocked_seconds ELSE 0 END) / 3600.0,
    2
  ) weekend_hours
FROM daily
GROUP BY quarter
ORDER BY quarter ASC"))
    (top-headlines
     . ,(deterred-dashboard-org-clock--select
         db params
         "SELECT
  title,
  file_name,
  category,
  ROUND(SUM(end_timestamp - start_timestamp) / 3600.0, 2) hours
FROM filtered_clocks
WHERE start_timestamp < end_timestamp
GROUP BY headline_id, title, file_name, category
ORDER BY hours DESC
LIMIT 20"))
    (numbers-data
     . ,(deterred-dashboard-org-clock--select
         db params
         "SELECT
  ROUND(
    COALESCE((SELECT SUM(end_timestamp - start_timestamp)
              FROM filtered_clocks
              WHERE start_timestamp < end_timestamp), 0) / 3600.0,
    2
  ) total_hours,
  (SELECT COUNT(*)
   FROM filtered_clocks
   WHERE start_timestamp < end_timestamp) clock_count,
  (SELECT COUNT(DISTINCT headline_id)
   FROM filtered_clocks
   WHERE start_timestamp < end_timestamp) headline_count,
  (SELECT COUNT(*) FROM daily WHERE clocked_seconds > 0) active_days,
  (SELECT COALESCE(SUM(is_weekend), 0) FROM daily) weekend_days,
  (SELECT COALESCE(SUM(is_free_weekend), 0) FROM daily) free_weekends"))))

(cl-defmethod deterred-dashboard-fetch-datasets
  ((_dashboard deterred-dashboard-org-clock) params)
  "Fetch datasets for the Org Clock dashboard.

PARAMS is as returned by `deterred-dashboard-default-params'."
  (deterred-dashboard-org-clock--fetch-datasets
   (deterred-db--init)
   params))

(cl-defmethod deterred-dashboard-render-results
  ((_dashboard deterred-dashboard-org-clock) _params data)
  "Render DATA for the Org Clock dashboard."
  (insert
   (deterred-format
    "Clocked "
    (f-ace (f (f-num (f-acc "data->'numbers-data->'data[0]->'total_hours")))
           'bold)
    " hours in "
    (f-ace (f (f-num (f-acc "data->'numbers-data->'data[0]->'clock_count")))
           'bold)
    " clock entries across "
    (f-ace (f (f-num (f-acc "data->'numbers-data->'data[0]->'headline_count")))
           'bold)
    " headlines and "
    (f-ace (f (f-num (f-acc "data->'numbers-data->'data[0]->'active_days")))
           'bold)
    " active days. "
    (f-ace (f (f-num (f-acc "data->'numbers-data->'data[0]->'free_weekends")))
           'bold)
    " of "
    (f-ace (f (f-num (f-acc "data->'numbers-data->'data[0]->'weekend_days")))
           'bold)
    " weekend days were free (less than 30 minutes clocked)."
    "\n\n"
    (f-h2 "Clocked time") "\n"))
  (deterred-dashboard-exec-python
   :python-code
   "from matplotlib import pyplot as plt
from matplotlib.ticker import MaxNLocator
from deterred import fig_to_b64

import json
import pandas as pd

data = json.loads(input())
images = []

def add_bar(dataset, x_column, y_column, title, xlabel, ylabel,
            *, stacked=False, horizontal=False, legend=False,
            legend_labels=None, value_format=None):
    df = pd.DataFrame(data[dataset]['data'])
    if df.empty:
        images.append(None)
        return

    fig, ax = plt.subplots(figsize=(9, 5))
    kind = 'barh' if horizontal else 'bar'
    df.plot(
        ax=ax,
        kind=kind,
        x=x_column,
        y=y_column,
        stacked=stacked,
        legend=legend)
    ax.set_title(title)
    ax.set_xlabel(xlabel)
    ax.set_ylabel(ylabel)
    if legend_labels is not None:
        ax.legend(legend_labels)

    if not horizontal and len(df) > 24:
        ax.xaxis.set_major_locator(MaxNLocator(nbins=24))
    if not horizontal:
        plt.xticks(rotation=45, ha='right')
    if value_format is not None and not stacked and len(df) <= 24:
        for container in ax.containers:
            ax.bar_label(container, fmt=value_format)

    plt.tight_layout()
    images.append(fig_to_b64(fig))

def add_days_worked_by_hour():
    df = pd.DataFrame(data['days-worked-by-hour']['data'])
    tracked_days = int(df['tracked_days'].max()) if not df.empty else 0
    if tracked_days == 0:
        images.append(None)
        return

    fig, ax = plt.subplots(figsize=(10, 5))
    bars = ax.bar(df['hour'], df['worked_days'])
    ax.set_title(f'Days worked by hour ({tracked_days} tracked days)')
    ax.set_xlabel('Hour')
    ax.set_ylabel('Days with tracked work')
    ax.set_xlim(-0.5, 23.5)
    ax.set_ylim(0, tracked_days)
    ax.set_xticks(range(24))
    tick_steps = min(tracked_days, 6)
    ax.set_yticks(sorted({
        round(step * tracked_days / tick_steps)
        for step in range(tick_steps + 1)
    }))
    ax.bar_label(
        bars,
        labels=[str(value) if value else '' for value in df['worked_days']],
        padding=2)
    plt.tight_layout()
    images.append(fig_to_b64(fig))

add_bar(
    'hours-per-month',
    'month',
    'hours',
    'Clocked hours per month',
    'Month',
    'Hours',
    value_format='%.1f')
add_bar(
    'hours-by-day-of-week',
    'day_of_week',
    'average_hours',
    'Average clocked hours by day of week',
    'Day of week',
    'Average hours',
    value_format='%.1f')
add_days_worked_by_hour()
add_bar(
    'hours-by-category',
    'category',
    'hours',
    'Clocked hours by category',
    'Hours',
    'Category',
    horizontal=True,
    value_format='%.1f')
add_bar(
    'free-weekends-per-month',
    'month',
    ['free_weekends', 'non_free_weekends'],
    'Free wekends per month',
    'Month',
    'Saturday and Sunday days',
    stacked=True,
    legend=True,
    legend_labels=['Free (< 30 min)', 'Clocked (30+ min)'])
add_bar(
    'free-weekends-per-quarter',
    'quarter',
    ['free_weekends', 'non_free_weekends'],
    'Free wekends per quarter',
    'Quarter',
    'Saturday and Sunday days',
    stacked=True,
    legend=True,
    legend_labels=['Free (< 30 min)', 'Clocked (30+ min)'])

print(json.dumps(images))"
   :input data
   :on-success
   (lambda (images)
     (when (elt images 0)
       (insert (deterred-format (f-h3 "Clocked hours per month") "\n"))
       (deterred-dashboard-print-images-base64 (elt images 0))
       (insert "\n"))
     (when (elt images 1)
       (insert
        (deterred-format
         (f-h3 "Average clocked hours by day of week") "\n"))
       (deterred-dashboard-print-images-base64 (elt images 1))
       (insert "\n"))
     (when (elt images 2)
       (insert (deterred-format (f-h3 "Days worked by hour") "\n"))
       (deterred-dashboard-print-images-base64 (elt images 2))
       (insert "\n"))
     (when (elt images 3)
       (insert (deterred-format (f-h3 "Clocked hours by category") "\n"))
       (deterred-dashboard-print-images-base64 (elt images 3))
       (insert "\n"))
     (when (elt images 4)
       (insert (deterred-format (f-h3 "Free wekends per month") "\n"))
       (deterred-dashboard-print-images-base64 (elt images 4))
       (insert "\n"))
     (when (elt images 5)
       (insert (deterred-format (f-h3 "Free wekends per quarter") "\n"))
       (deterred-dashboard-print-images-base64 (elt images 5))
       (insert "\n"))))
  (insert
   (deterred-format (f-h2 "Top headlines") "\n")
   (deterred-grid-print-with-org
    (alist-get 'data (alist-get 'top-headlines data))
    :column-names '((title . "Headline")
                    (file_name . "File")
                    (category . "Category")
                    (hours . "Hours"))
    :max-rows 20
    :grid-button t)
   "\n"))

(provide 'deterred-dashboard-org-clock)
;;; deterred-dashboard-org-clock.el ends here

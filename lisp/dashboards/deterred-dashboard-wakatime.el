;;; deterred-dashboard-wakatime.el --- DETERRED dashboard for wakatime -*- lexical-binding: t -*-

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

;; A dashboard for wakatime, corresponding to `deterred-wakatime'.

;;; Code:
(require 'deterred-dashboard)
(require 'deterred-chains)
(require 'deterred-db)
(require 'deterred-utils)

(defconst deterred-dashboard-wakatime-ai-usage-normalize-timeout (* 15 60))

(defclass deterred-dashboard-wakatime (deterred-dashboard)
  ((name :initform "WakaTime"))
  "A DETERRED dashboard for WakaTime.")

(cl-defmethod deterred-dashboard-default-params ((_dashboard deterred-dashboard-wakatime))
  "Default parameters for the WakaTime dashboard."
  '((:start-date)
    (:end-date)
    (:projects)
    (:n-top-projects . 5)
    (:folders-to-compare)))

(cl-defmethod deterred-dashboard-render-params ((_dashboard deterred-dashboard-wakatime))
  "Render the parameters section for the WakaTime dashboard."
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
         (projects (mapcar
                    (lambda (datum) (cons (alist-get 'name datum)
                                          (alist-get 'id datum)))
                    (deterred-db-select-alist
                     db "SELECT id, name FROM wakatime_projects"))))
    (deterred-dashboard-widget-completing-read-multiple
     :name "Project"
     :key :projects
     :options projects))
  (insert "\n")
  (deterred-dashboard-widget-number
   :name "Top N projects"
   :key :n-top-projects)
  (insert "\n")
  (let* ((db (deterred-db--init))
         (project-roots (deterred-db-select-alist
                         db "SELECT DISTINCT project_root FROM wakatime_projects
WHERE project_root IS NOT NULL AND name != 'Unknown Project'"))
         (folders (make-hash-table :test 'equal)))
    ;; Collect all parent folders
    (dolist (root project-roots)
      (let* ((path (alist-get 'project_root root))
             (parts (file-name-split path)))
        (cl-loop for i from 1 to (1- (seq-length parts))
                 for folder = (apply #'file-name-concat "/" (seq-take parts i))
                 do (puthash folder t folders))))
    (let ((folder-options (sort (hash-table-keys folders) #'string<)))
      (deterred-dashboard-widget-completing-read-multiple
       :name "Folders to compare"
       :key :folders-to-compare
       :options (mapcar (lambda (f) (cons f f)) folder-options))))
  (insert "\n"))

(defun deterred-dashboard-wakatime--fetch-hours-by-folder (db params folders period)
  "Fetch hours grouped by folder and time period.

DB is the database connection.
PARAMS are the dashboard parameters.
FOLDERS is a list of folder paths to compare.
PERIOD is either 'month or 'year."
  (let* ((time-format (if (eq period 'month) "%Y-%m" "%Y"))
         (time-column (if (eq period 'month) "month" "year"))
         ;; Sort folders by length (longest first) for proper matching
         (sorted-folders (sort (copy-sequence folders) (lambda (a b) (> (length a) (length b)))))
         ;; Build CASE statement for folder matching
         (case-stmt
          (concat
           "CASE\n"
           (mapconcat
            (lambda (folder)
              (format "  WHEN wp.project_root LIKE '%s%%' THEN '%s'"
                      (replace-regexp-in-string "'" "''" folder)  ; Escape single quotes
                      (replace-regexp-in-string "'" "''" folder)))
            sorted-folders
            "\n")
           "\nEND")))
    ;; Query with folder grouping in SQL
    (deterred-db-select-template-alist
     db
     (format "SELECT
  strftime('%s', wgt.timestamp, 'unixepoch') %s,
  %s AS folder,
  CAST(sum(wgt.total_seconds) / (60 * 60) * 100 AS integer) / 100.0 hours
FROM wakatime_grand_total wgt
INNER JOIN wakatime_projects wp ON wp.id = wgt.project_id
WHERE wp.project_root IS NOT NULL AND wp.name != 'Unknown Project'
  AND (%s) IS NOT NULL
[[AND wgt.timestamp >= :start-date]] [[AND wgt.timestamp <= :end-date]]
[[AND wgt.project_id IN :projects]]
GROUP BY strftime('%s', wgt.timestamp, 'unixepoch'), folder
ORDER BY %s ASC" time-format time-column case-stmt case-stmt time-format time-column)
     params)))

(defun deterred-dashboard-wakatime--get-ai-usage (db params)
  "Calculate fraction of time programmed using AI.

DB is the SQLite database object, PARAMS is the dashboard parameters
object.

Return 3 datasets, each having three columns:
- month / week / day
- total_hours
- ai_hours."
  (when-let*
      ((ai-usage-raw
        (deterred-db-select-template-alist
         db
         "SELECT timestamp
FROM ai_usage_item
WHERE is_stats = 0
  [[AND timestamp >= :start-date]]
  [[AND timestamp <= :end-date]]"
         params))
       (ai-usage (car (deterred-chains-normalize
                       (list
                        (mapcar (lambda (e) (list (alist-get 'timestamp e) nil nil))
                                ai-usage-raw))
                       deterred-dashboard-wakatime-ai-usage-normalize-timeout)))
       (ai-usage-start (caar ai-usage))
       (ai-usage-end (caar (last ai-usage)))
       (wakatime-data-raw
        (deterred-db-select-template-alist
         db
         "SELECT wi.start_timestamp, wi.end_timestamp
FROM wakatime_item wi
INNER JOIN wakatime_projects wp ON wp.id = wi.project_id
WHERE 1 = 1 [[AND wi.start_timestamp >= :start-date]]
  [[AND wi.end_timestamp <= :end-date]]
  [[AND wi.project_id IN :projects]]
  [[AND wi.start_timestamp >= :ai-usage-start]]
  [[AND wi.end_timestamp <= :ai-usage-end]]"
         (append params `((:ai-usage-start . ,ai-usage-start)
                          (:ai-usage-end . ,ai-usage-end)))))
       (wakatime-data
        (car (deterred-chains-normalize
              (list
               (mapcar (lambda (e)
                         (list
                          (alist-get 'start_timestamp e)
                          (alist-get 'end_timestamp e)
                          nil))
                       wakatime-data-raw))
              deterred-dashboard-wakatime-ai-usage-normalize-timeout)))
       (intersection-data
        (deterred-chains-intersection
         (list ai-usage wakatime-data))))
    (list
     (seq-sort-by
      #'cdar #'string-lessp
      (deterred-utils-cells-to-alists
       (list
        (deterred-chains-group-by wakatime-data "%Y-%m")
        (deterred-chains-group-by intersection-data "%Y-%m"))
       'month '(total_hours ai_hours)
       (lambda (sec) (when sec
                       (deterred-utils-round-to (/ (float sec) (* 60 60)) 2)))))
     (seq-sort-by
      #'cdar #'string-lessp
      (deterred-utils-cells-to-alists
       (list
        (deterred-chains-group-by wakatime-data "%Y-%W")
        (deterred-chains-group-by intersection-data "%Y-%W"))
       'week '(total_hours ai_hours)
       (lambda (sec) (when sec
                       (deterred-utils-round-to (/ (float sec) (* 60 60)) 2)))))
     (seq-sort-by
      #'cdar #'string-lessp
      (deterred-utils-cells-to-alists
       (list
        (deterred-chains-group-by wakatime-data "%Y-%m-%d")
        (deterred-chains-group-by intersection-data "%Y-%m-%d"))
       'day '(total_hours ai_hours)
       (lambda (sec) (when sec
                       (deterred-utils-round-to (/ (float sec) (* 60 60)) 2))))))))

(cl-defmethod deterred-dashboard-list-datasets ((_dashboard deterred-dashboard-wakatime))
  "List datasets for the WakaTime dashboard."
  '((top-languages (name . "Top languages"))
    (top-operating-systems (name . "Top operating systems"))
    (top-projects (name . "Top projects"))
    (top-editors (name . "Top editors"))
    (top-entities (name . "Top entities"))
    (hours-per-year (name . "Hours logged per year"))
    (hours-per-month (name . "Hours per month"))
    (hours-in-top-by-month  (name . "Hours in top-N projects by month"))
    (hours-in-new-projects-per-year (name . "Hours in new projects per year"))
    (average-project-age-per-month (name . "Average project age per month"))
    (top-days (name . "Top days"))
    (top-weeks (name . "Top weeks"))
    (top-months (name . "Top months"))
    (ai-usage-per-month (name . "AI usage per month"))
    (ai-usage-per-week (name . "AI usage per week"))
    (ai-usage-per-day (name . "AI usage per day"))
    (hours-by-folder-per-month (name . "Hours by folder per month"))
    (hours-by-folder-per-year (name . "Hours by folder per year"))
    (numbers-data (name . "Numerical data"))))

(cl-defmethod deterred-dashboard-fetch-datasets ((_dashboard deterred-dashboard-wakatime)
                                                 params)
  "Fetch datasets for the WakaTime dashboard.

PARAMS is as returned by `deterred-dashboard-default-params'."
  (let* ((db (deterred-db--init))
         (ai-usage-datasets (deterred-dashboard-wakatime--get-ai-usage db params))
         (numbers-data
          (deterred-db-select-template-alist
           db
           "SELECT (
  SELECT
    CAST(sum(wgt.total_seconds) / (60 * 60) * 100 AS integer) / 100.0 hours
  FROM wakatime_grand_total wgt
  WHERE 1 = 1 [[AND wgt.timestamp >= :start-date]] [[AND wgt.timestamp <= :end-date]]
  [[AND wgt.project_id IN :projects]]
) AS total_hours,
(
  SELECT
    count(DISTINCT wgt.project_id)
  FROM wakatime_grand_total wgt
  WHERE 1 = 1 [[AND wgt.timestamp >= :start-date]] [[AND wgt.timestamp <= :end-date]]
  [[AND wgt.project_id IN :projects]]
) AS project_count,
(
  SELECT count(DISTINCT we.project_id || we.project_path)
  FROM wakatime_entities we
  WHERE 1 = 1 [[AND we.timestamp >= :start-date]] [[AND we.timestamp <= :end-date]]
  [[AND we.project_id IN :projects]]
) AS entities_count,
(
  SELECT count(DISTINCT wl.name)
  FROM wakatime_languages wl
  WHERE 1 = 1 [[AND wl.timestamp >= :start-date]] [[AND wl.timestamp <= :end-date]]
  [[AND wl.project_id IN :projects]]
) AS languages_count;"
           params))
         (total-hours (alist-get 'total_hours (car numbers-data))))
    `((top-languages
       . ,(deterred-utils-add-fraction
           (deterred-db-select-template-alist
            db
            "SELECT
  wl.name,
  CAST(sum(wl.total_seconds) / (60 * 60) * 100 AS integer) / 100.0 hours
FROM wakatime_languages wl
WHERE 1 = 1 [[AND wl.timestamp >= :start-date]] [[AND wl.timestamp <= :end-date]]
  [[AND wl.project_id IN :projects]]
GROUP BY wl.name
ORDER BY hours DESC"
            params)
           total-hours))
      (top-operating-systems
       . ,(deterred-utils-add-fraction
           (deterred-db-select-template-alist
            db
            "SELECT
  wos.name,
  CAST(sum(wos.total_seconds) / (60 * 60) * 100 AS integer) / 100.0 hours
FROM wakatime_operating_systems wos
WHERE 1 = 1 [[AND wos.timestamp >= :start-date]] [[AND wos.timestamp <= :end-date]]
  [[AND wos.project_id IN :projects]]
GROUP BY wos.name
ORDER BY hours DESC"
            params)
           total-hours))
      (top-projects
       . ,(deterred-utils-add-fraction
           (deterred-db-select-template-alist
            db
            "SELECT
  wp.name project,
  CAST(sum(wgt.total_seconds) / (60 * 60) * 100 AS integer) / 100.0 hours
FROM wakatime_grand_total wgt
INNER JOIN wakatime_projects wp ON wp.id = wgt.project_id
WHERE 1 = 1 [[AND wgt.timestamp >= :start-date]] [[AND wgt.timestamp <= :end-date]]
  [[AND wgt.project_id IN :projects]]
GROUP BY wp.name
ORDER BY hours DESC
LIMIT 20"
            params)
           total-hours))
      (top-entities
       . ,(deterred-utils-add-fraction
           (deterred-db-select-template-alist
            db
            "SELECT
  we.project_path,
  wp.name project,
  CAST(sum(we.total_seconds) / (60 * 60) * 100 AS integer) / 100.0 hours
FROM wakatime_entities we
INNER JOIN wakatime_projects wp ON wp.id = we.project_id
WHERE 1 = 1 [[AND we.timestamp >= :start-date]] [[AND we.timestamp <= :end-date]]
  [[AND we.project_id IN :projects]]
GROUP BY we.project_path
ORDER BY hours DESC
LIMIT 100"
            params)
           total-hours))
      (top-editors
       . ,(deterred-utils-add-fraction
           (deterred-db-select-template-alist
            db
            "SELECT
  we.name,
  CAST(sum(we.total_seconds) / (60 * 60) * 100 AS integer) / 100.0 hours
FROM wakatime_editors we
WHERE 1 = 1 [[AND we.timestamp >= :start-date]] [[AND we.timestamp <= :end-date]]
  [[AND we.project_id IN :projects]]
GROUP BY we.name
ORDER BY hours DESC"
            params)
           total-hours))
      (hours-per-year
       . ,(deterred-db-select-template-alist
           db
           "SELECT
   strftime('%Y', wgt.timestamp, 'unixepoch') year,
   CAST(sum(total_seconds) / (60 * 60) * 100 AS integer) / 100.0 hours
FROM wakatime_grand_total wgt
WHERE 1 = 1 [[AND wgt.timestamp >= :start-date]] [[AND wgt.timestamp <= :end-date]]
  [[AND wgt.project_id IN :projects]]
GROUP BY strftime('%Y', wgt.timestamp, 'unixepoch')"
           params))
      (hours-per-month
       . ,(deterred-db-select-template-alist
           db
           "SELECT
   strftime('%Y-%m', wgt.timestamp, 'unixepoch') month,
   CAST(sum(total_seconds) / (60 * 60) * 100 AS integer) / 100.0 hours
FROM wakatime_grand_total wgt
WHERE 1 = 1 [[AND wgt.timestamp >= :start-date]] [[AND wgt.timestamp <= :end-date]]
  [[AND wgt.project_id IN :projects]]
GROUP BY strftime('%Y-%m', wgt.timestamp, 'unixepoch')"
           params))
      (hours-in-top-by-month
       . ,(deterred-db-select-template-alist
           db
           "WITH top_projects AS (
    SELECT
      wgt.project_id id,
      sum(wgt.total_seconds) total
    FROM wakatime_grand_total wgt
    INNER JOIN wakatime_projects wp ON wp.id = wgt.project_id
    WHERE wp.name != 'Unknown Project'
      [[AND wgt.timestamp >= :start-date]] [[AND wgt.timestamp <= :end-date]]
      [[AND wgt.project_id IN :projects]]
    GROUP BY wgt.project_id
    ORDER BY total DESC
    LIMIT :n-top-projects
)
SELECT
   strftime('%Y-%m', wgt.timestamp, 'unixepoch') month,
   wp.name project,
   CAST(sum(total_seconds) / (60 * 60) * 100 AS integer) / 100.0 hours
FROM wakatime_grand_total wgt
INNER JOIN wakatime_projects wp ON wp.id = wgt.project_id
INNER JOIN top_projects tp ON tp.id = wp.id
WHERE 1 = 1 [[AND wgt.timestamp >= :start-date]] [[AND wgt.timestamp <= :end-date]]
  [[AND wgt.project_id IN :projects]]
GROUP BY strftime('%Y-%m', wgt.timestamp, 'unixepoch'), wp.name
ORDER BY month ASC"
           params))
      (hours-in-new-projects-per-year
       . ,(deterred-db-select-template-alist
           db
           "WITH project_discovered_years AS (
  SELECT STRFTIME('%Y', min(timestamp), 'unixepoch') \"year\", wgt.project_id FROM wakatime_grand_total wgt
  GROUP BY wgt.project_id
)
SELECT
   strftime('%Y', wgt.timestamp, 'unixepoch') year,
   CAST(sum(CASE WHEN pdy.\"year\" = STRFTIME('%Y', timestamp, 'unixepoch') THEN total_seconds ELSE 0 END) / (60 * 60) * 100 AS integer) / 100.0 \"new\",
   CAST(sum(CASE WHEN pdy.\"year\" != STRFTIME('%Y', timestamp, 'unixepoch') THEN total_seconds ELSE 0 END) / (60 * 60) * 100 AS integer) / 100.0 \"old\"
FROM wakatime_grand_total wgt
INNER JOIN project_discovered_years pdy ON pdy.project_id = wgt.project_id
INNER JOIN wakatime_projects wp ON wp.id = wgt.project_id
WHERE wp.name != 'Unknown Project'
  [[AND wgt.timestamp >= :start-date]] [[AND wgt.timestamp <= :end-date]]
  [[AND wgt.project_id IN :projects]]
GROUP BY strftime('%Y', wgt.timestamp, 'unixepoch')"
           params))
      (average-project-age-per-month
       . ,(deterred-db-select-template-alist
           db
           "WITH project_discovered_months AS (
  SELECT
    CAST(STRFTIME('%Y', min(timestamp), 'unixepoch') AS INTEGER) * 12 +
    CAST(STRFTIME('%m', min(timestamp), 'unixepoch') AS INTEGER) start_month,
    wgt.project_id
  FROM wakatime_grand_total wgt
  GROUP BY wgt.project_id
), hours_per_month AS (
  SELECT
    STRFTIME('%Y-%m', timestamp, 'unixepoch') \"month\",
    SUM(total_seconds / (60.0 * 60.0)) hours_spent
  FROM wakatime_grand_total wgt
  INNER JOIN wakatime_projects wp ON wp.id = wgt.project_id
  WHERE wp.name != 'Unknown Project' [[AND wgt.timestamp >= :start-date]] [[AND wgt.timestamp <= :end-date]]
    [[AND wgt.project_id IN :projects]]
  GROUP BY STRFTIME('%Y-%m', timestamp, 'unixepoch')
), project_data AS (
  SELECT
    (
      (
        CAST(STRFTIME('%Y', timestamp, 'unixepoch') AS INTEGER) * 12 +
        CAST(STRFTIME('%m', timestamp, 'unixepoch') AS INTEGER)
      ) - pdm.start_month
    ) project_age,
    SUM(total_seconds / (60.0 * 60.0)) / hpr.hours_spent fraction,
    STRFTIME('%Y-%m', timestamp, 'unixepoch') \"month\",
    wgt.project_id
  FROM wakatime_grand_total wgt
  INNER JOIN project_discovered_months pdm ON pdm.project_id = wgt.project_id
  INNER JOIN hours_per_month hpr ON hpr.month = STRFTIME('%Y-%m', timestamp, 'unixepoch')
  INNER JOIN wakatime_projects wp ON wp.id = wgt.project_id
  WHERE wp.name != 'Unknown Project'
    [[AND wgt.timestamp >= :start-date]] [[AND wgt.timestamp <= :end-date]]
    [[AND wgt.project_id IN :projects]]
  GROUP BY STRFTIME('%Y-%m', timestamp, 'unixepoch'), wgt.project_id
  ORDER BY month, fraction DESC
)
SELECT sum(project_age * fraction) age, month
FROM project_data
GROUP BY month
ORDER BY month ASC"
           params))
      (top-days
       . ,(deterred-db-select-template-alist
           db
           "SELECT
   strftime('%Y-%m-%d', wgt.timestamp, 'unixepoch') day,
   CAST(sum(total_seconds) / (60 * 60) * 100 AS integer) / 100.0 hours
FROM wakatime_grand_total wgt
WHERE 1 = 1 [[AND wgt.timestamp >= :start-date]] [[AND wgt.timestamp <= :end-date]]
  [[AND wgt.project_id IN :projects]]
GROUP BY strftime('%Y-%m-%d', wgt.timestamp, 'unixepoch')
ORDER BY hours DESC
LIMIT 20"
           params))
      (top-weeks
       . ,(deterred-db-select-template-alist
           db
           "SELECT
   strftime('%Y-%W', wgt.timestamp, 'unixepoch') week,
   CAST(sum(total_seconds) / (60 * 60) * 100 AS integer) / 100.0 hours
FROM wakatime_grand_total wgt
WHERE 1 = 1 [[AND wgt.timestamp >= :start-date]] [[AND wgt.timestamp <= :end-date]]
  [[AND wgt.project_id IN :projects]]
GROUP BY strftime('%Y-%W', wgt.timestamp, 'unixepoch')
ORDER BY hours DESC
LIMIT 20"
           params))
      (top-months
       . ,(deterred-db-select-template-alist
           db
           "SELECT
   strftime('%Y-%m', wgt.timestamp, 'unixepoch') month,
   CAST(sum(total_seconds) / (60 * 60) * 100 AS integer) / 100.0 hours
FROM wakatime_grand_total wgt
WHERE 1 = 1 [[AND wgt.timestamp >= :start-date]] [[AND wgt.timestamp <= :end-date]]
  [[AND wgt.project_id IN :projects]]
GROUP BY strftime('%Y-%m', wgt.timestamp, 'unixepoch')
ORDER BY hours DESC
LIMIT 20"
           params))
      (ai-usage-per-month . ,(nth 0 ai-usage-datasets))
      (ai-usage-per-week . ,(nth 1 ai-usage-datasets))
      (ai-usage-per-day . ,(nth 2 ai-usage-datasets))
      (hours-by-folder-per-month
       . ,(when-let ((folders (alist-get :folders-to-compare params)))
            (deterred-dashboard-wakatime--fetch-hours-by-folder
             db params folders 'month)))
      (hours-by-folder-per-year
       . ,(when-let ((folders (alist-get :folders-to-compare params)))
            (deterred-dashboard-wakatime--fetch-hours-by-folder
             db params folders 'year)))
      (numbers-data . ,numbers-data))))

(cl-defmethod deterred-dashboard-render-results ((_dashboard deterred-dashboard-wakatime)
                                                 _params data)
  "Render DATA for the WakaTime dashboard."
  (insert
   (deterred-format
    "I've spent "
    (f-ace (f (f-num (f-acc "data->'numbers-data->'data[0]->'total_hours")))
           'bold)
    " hours in "
    (f-ace (f (f-num (f-acc "data->'numbers-data->'data[0]->'project_count")))
           'bold)
    " unique projects, "
    (f-ace (f (f-num (f-acc "data->'numbers-data->'data[0]->'languages_count")))
           'bold)
    " unique programming languages, "
    (f-ace (f (f-num (f-acc "data->'numbers-data->'data[0]->'entities_count")))
           'bold)
    " unique files.\n\n"
    (f-h2 "Top over all time") "\n"
    (f-h3 "Top projects") "\n")
   (deterred-grid-print-with-org
    (alist-get 'data (alist-get 'top-projects data))
    :column-names '((project . "Project") (hours . "Hours") (fraction . "%"))
    :max-rows 10
    :grid-button t)
   "\n"
   (deterred-format (f-h3 "Top languages") "\n")
   (deterred-grid-print-with-org
    (alist-get 'data (alist-get 'top-languages data))
    :column-names '((name . "Language") (hours . "Hours") (fraction . "%"))
    :max-rows 10
    :grid-button t)
   "\n"
   (deterred-format (f-h3 "Top editors") "\n")
   (deterred-grid-print-with-org
    (alist-get 'data (alist-get 'top-editors data))
    :column-names '((name . "Editor") (hours . "Hours") (fraction . "%"))
    :max-rows 10
    :grid-button t)
   "\n"
   (deterred-format (f-h3 "Top operating systems") "\n")
   (deterred-grid-print-with-org
    (alist-get 'data (alist-get 'top-operating-systems data))
    :column-names '((name . "OS") (hours . "Hours") (fraction . "%"))
    :max-rows 10
    :grid-button t)
   "\n"
   (deterred-format (f-h3 "Top entities") "\n")
   (deterred-grid-print-with-org
    (alist-get 'data (alist-get 'top-entities data))
    :column-names '((name . "Entity") (project . "Project")
                    (hours . "Hours") (fraction . "%"))
    :max-rows 10
    :max-column-width 35
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
import warnings

import json
import os
import base64
import io

data = json.loads(input())
df_y = pd.DataFrame(data['hours-per-year']['data'])
df_m = pd.DataFrame(data['hours-per-month']['data'])
df_t = pd.DataFrame(data['hours-in-top-by-month']['data'])
df_tp = df_t.pivot(index='month', columns='project', values='hours').fillna(0)
df_new = pd.DataFrame(data['hours-in-new-projects-per-year']['data'])
df_age = pd.DataFrame(data['average-project-age-per-month']['data'])

images = []

fig, ax = plt.subplots(figsize=(8, 5))
df_y.plot(ax=ax, kind='bar', x='year', y='hours')
ax.set_title('Hours coded per year')
for container in ax.containers:
    ax.bar_label(container, fmt='%.0f')
images.append(fig_to_b64(fig))

fig, ax = plt.subplots(figsize=(8, 5))
df_m.plot(ax=ax, kind='bar', x='month', y='hours')
ax.set_title('Hours coded per month')
if len(df_m) > 30:
    ax.xaxis.set_major_locator(MaxNLocator(nbins=40))
if len(df_m) < 30:
    for container in ax.containers:
        ax.bar_label(container, fmt='%.2f')
images.append(fig_to_b64(fig))

fig, ax = plt.subplots(figsize=(8, 5))
df_tp.plot(ax=ax, kind='line')
ax.set_title('Hours spent in top N projects by month')
images.append(fig_to_b64(fig))

fig, ax = plt.subplots(figsize=(8, 5))
df_new.plot(ax=ax, kind='bar', x='year', stacked=True)
ax.set_title('Hours spent in new vs. old projects per year')
images.append(fig_to_b64(fig))

fig, ax = plt.subplots(figsize=(8, 5))
ax.set_title('Average project age (in months) per month')
df_age_plot = df_age.dropna(subset=['age']).copy()
if not df_age_plot.empty:
    df_age_plot = df_age_plot.set_index('month')
    x = np.arange(len(df_age_plot))
    y = df_age_plot['age'].astype(float).to_numpy()
    if len(df_age_plot) >= 2 and np.isfinite(y).all():
        try:
            with warnings.catch_warnings():
                warnings.simplefilter('ignore', RuntimeWarning)
                z = np.polyfit(x, y, 1)
            p = np.poly1d(z)
            ax.plot(x, p(x), '--', color='red', linewidth=1, label='trend')
        except (np.linalg.LinAlgError, ValueError, FloatingPointError):
            pass
    df_age_plot.plot(ax=ax, kind='line')
    ax.legend()

images.append(fig_to_b64(fig))

print(json.dumps(images))"
   :input data
   :on-success
   (lambda (images)
     (insert (deterred-format (f-h3 "Hours coded per year") "\n"))
     (deterred-dashboard-print-images-base64 (elt images 0))
     (insert "\n")
     (insert (deterred-format (f-h3 "Hours coded per month") "\n"))
     (deterred-dashboard-print-images-base64 (elt images 1))
     (insert "\n")
     (insert (deterred-format (f-h3 "Hours spent in top N projects by month") "\n"))
     (deterred-dashboard-print-images-base64 (elt images 2))
     (insert "\n")
     (insert
      (deterred-format (f-h3 "Hours spent in new vs. old projects per year") "\n"))
     (deterred-dashboard-print-images-base64 (elt images 3))
     (insert "\n")
     (insert
      (deterred-format (f-h3 "Average project age per month") "\n"))
     (deterred-dashboard-print-images-base64 (elt images 4))
     (insert "\n")))
  ;; AI usage charts
  (when (or (alist-get 'ai-usage-per-month data)
            (alist-get 'ai-usage-per-week data)
            (alist-get 'ai-usage-per-day data))
    (insert (deterred-format (f-h2 "AI usage") "\n"))
    (deterred-dashboard-exec-python
     :python-code
     "from matplotlib import pyplot as plt
from matplotlib.ticker import MaxNLocator
from deterred import fig_to_b64

import pandas as pd
import json

data = json.loads(input())
df_month = pd.DataFrame(data.get('ai-usage-per-month', {}).get('data', []))
df_week = pd.DataFrame(data.get('ai-usage-per-week', {}).get('data', []))
df_day = pd.DataFrame(data.get('ai-usage-per-day', {}).get('data', []))

images = []

def prepare_df(df, index_column):
    if df.empty:
        return None

    df_plot = df.copy()
    df_plot['ai_hours'] = df_plot['ai_hours'].fillna(0)
    df_plot['total_hours'] = df_plot['total_hours'].fillna(0)
    df_plot['non_ai_hours'] = (df_plot['total_hours'] - df_plot['ai_hours']).clip(lower=0)
    df_plot = df_plot[[index_column, 'ai_hours', 'non_ai_hours']]
    return df_plot.set_index(index_column)

def add_chart(df, title, ylabel, percentage=False, max_bins=None):
    if df is None or df.empty:
        images.append(None)
        return

    df_plot = df
    if percentage:
        totals = df_plot.sum(axis=1)
        df_plot = df_plot.div(totals.where(totals != 0), axis=0).fillna(0) * 100

    fig, ax = plt.subplots(figsize=(8, 5))
    df_plot.plot(ax=ax, kind='bar', stacked=True)
    ax.set_title(title)
    ax.set_ylabel(ylabel)
    if percentage:
        ax.set_ylim(0, 100)
    if max_bins and len(df_plot) > max_bins:
        ax.xaxis.set_major_locator(MaxNLocator(nbins=max_bins))
    plt.xticks(rotation=45, ha='right')
    plt.tight_layout()
    images.append(fig_to_b64(fig))

df_month_plot = prepare_df(df_month, 'month')
add_chart(df_month_plot, 'AI vs non-AI hours per month', 'Hours')
add_chart(df_month_plot, 'AI vs non-AI share per month', '%', percentage=True)

df_week_plot = prepare_df(df_week, 'week')
add_chart(df_week_plot, 'AI vs non-AI hours per week', 'Hours', max_bins=30)
add_chart(df_week_plot, 'AI vs non-AI share per week', '%', percentage=True, max_bins=30)

df_day_plot = prepare_df(df_day, 'day')
add_chart(df_day_plot, 'AI vs non-AI hours per day', 'Hours', max_bins=30)
add_chart(df_day_plot, 'AI vs non-AI share per day', '%', percentage=True, max_bins=30)

print(json.dumps(images))"
     :input data
     :on-success
     (lambda (images)
       (when (elt images 0)
         (insert (deterred-format (f-h3 "AI vs non-AI hours per month") "\n"))
         (deterred-dashboard-print-images-base64 (elt images 0))
         (insert "\n"))
       (when (elt images 1)
         (insert (deterred-format (f-h3 "AI vs non-AI share per month") "\n"))
         (deterred-dashboard-print-images-base64 (elt images 1))
         (insert "\n"))
       (when (elt images 2)
         (insert (deterred-format (f-h3 "AI vs non-AI hours per week") "\n"))
         (deterred-dashboard-print-images-base64 (elt images 2))
         (insert "\n"))
       (when (elt images 3)
         (insert (deterred-format (f-h3 "AI vs non-AI share per week") "\n"))
         (deterred-dashboard-print-images-base64 (elt images 3))
         (insert "\n"))
       (when (elt images 4)
         (insert (deterred-format (f-h3 "AI vs non-AI hours per day") "\n"))
         (deterred-dashboard-print-images-base64 (elt images 4))
         (insert "\n"))
       (when (elt images 5)
         (insert (deterred-format (f-h3 "AI vs non-AI share per day") "\n"))
         (deterred-dashboard-print-images-base64 (elt images 5))
         (insert "\n")))))
  ;; Folder comparison charts
  (when (and (alist-get 'hours-by-folder-per-month data)
             (alist-get 'data (alist-get 'hours-by-folder-per-month data))
             (> (length (alist-get 'data (alist-get 'hours-by-folder-per-month data))) 0))
    (insert (deterred-format (f-h2 "Folder comparison") "\n"))
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
images = []

def remove_common_prefix(folders):
    \"\"\"Remove common prefix from folder paths.\"\"\"
    if not folders or len(folders) == 1:
        return folders

    # Find common prefix
    common_prefix = os.path.commonprefix(list(folders))
    # Ensure we end at a directory boundary
    if common_prefix and not common_prefix.endswith('/'):
        common_prefix = common_prefix.rsplit('/', 1)[0] + '/'

    # Remove common prefix from all folders
    if common_prefix and common_prefix != '/':
        return [f[len(common_prefix):] if f.startswith(common_prefix) else f for f in folders]
    return folders

if data.get('hours-by-folder-per-year') and data['hours-by-folder-per-year'].get('data') and len(data['hours-by-folder-per-year']['data']) > 0:
    df_year = pd.DataFrame(data['hours-by-folder-per-year']['data'])
    df_year_pivot = df_year.pivot(index='year', columns='folder', values='hours').fillna(0)

    # Remove common prefix from column names
    new_columns = remove_common_prefix(df_year_pivot.columns.tolist())
    df_year_pivot.columns = new_columns

    fig, ax = plt.subplots(figsize=(8, 5))
    df_year_pivot.plot(ax=ax, kind='bar', stacked=True)
    ax.set_title('Hours by folder per year')
    ax.set_xlabel('Year')
    ax.set_ylabel('Hours')
    ax.legend(title='Folder', loc='upper center', bbox_to_anchor=(0.5, -0.25), ncol=min(2, len(new_columns)))
    plt.tight_layout()
    images.append(fig_to_b64(fig))
else:
    images.append(None)

if data.get('hours-by-folder-per-month') and data['hours-by-folder-per-month'].get('data') and len(data['hours-by-folder-per-month']['data']) > 0:
    df_month = pd.DataFrame(data['hours-by-folder-per-month']['data'])
    df_month_pivot = df_month.pivot(index='month', columns='folder', values='hours').fillna(0)

    # Remove common prefix from column names
    new_columns = remove_common_prefix(df_month_pivot.columns.tolist())
    df_month_pivot.columns = new_columns

    fig, ax = plt.subplots(figsize=(8, 5))
    df_month_pivot.plot(ax=ax, kind='bar', stacked=True)
    ax.set_title('Hours by folder per month')
    ax.set_xlabel('Month')
    ax.set_ylabel('Hours')
    if len(df_month_pivot) > 30:
        ax.xaxis.set_major_locator(MaxNLocator(nbins=40))
    ax.legend(title='Folder', loc='upper center', bbox_to_anchor=(0.5, -0.25), ncol=min(2, len(new_columns)))
    plt.tight_layout()
    images.append(fig_to_b64(fig))
else:
    images.append(None)

print(json.dumps(images))"
     :input data
     :on-success
     (lambda (images)
       (when (elt images 0)
         (insert (deterred-format (f-h3 "Hours by folder per year") "\n"))
         (deterred-dashboard-print-images-base64 (elt images 0))
         (insert "\n"))
       (when (elt images 1)
         (insert (deterred-format (f-h3 "Hours by folder per month") "\n"))
         (deterred-dashboard-print-images-base64 (elt images 1))
         (insert "\n")))))
  (insert
   (deterred-format (f-h2 "Top periods") "\n"
                    (f-h3 "Top months") "\n")
   (deterred-grid-print-with-org
    (alist-get 'data (alist-get 'top-months data))
    :column-names '((month . "Month") (hours . "Hours"))
    :max-rows 10
    :grid-button t)
   "\n"
   (deterred-format (f-h3 "Top weeks") "\n")
   (deterred-grid-print-with-org
    (alist-get 'data (alist-get 'top-weeks data))
    :column-names '((week . "Week") (hours . "Hours"))
    :max-rows 10
    :grid-button t)
   "\n"
   (deterred-format (f-h3 "Top days") "\n")
   (deterred-grid-print-with-org
    (alist-get 'data (alist-get 'top-days data))
    :column-names '((day . "Day") (hours . "Hours"))
    :max-rows 10
    :grid-button t)
   "\n"))

(provide 'deterred-dashboard-wakatime)
;;; deterred-dashboard-wakatime.el ends here

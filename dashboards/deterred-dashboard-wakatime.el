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
(require 'deterred-db)
(require 'deterred-utils)

(defclass deterred-dashboard-wakatime (deterred-dashboard)
  ((name :initform "WakaTime"))
  "A DETERRED dashboard for WakaTime.")

(cl-defmethod deterred-dashboard-default-params ((_dashboard deterred-dashboard-wakatime))
  "Default parameters for the WakaTime dashboard."
  '((:start-date)
    (:end-date)
    (:projects)
    (:n-top-projects . 5)))

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
  (insert "\n"))

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
    (numbers-data (name . "Numerical data"))))

(cl-defmethod deterred-dashboard-fetch-datasets ((_dashboard deterred-dashboard-wakatime)
                                                 params)
  "Fetch datasets for the WakaTime dashboard.

PARAMS is as returned by `deterred-dashboard-default-params'."
  (let* ((db (deterred-db--init))
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
x = np.arange(len(df_age))
y = df_age.age
z = np.polyfit(x, y, 1)
p = np.poly1d(z)
ax.plot(x, p(x), '--', color='red', linewidth=1, label='trend')
df_age.plot(ax=ax, kind='line')
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

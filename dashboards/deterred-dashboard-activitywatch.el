;;; deterred-dashboard-activitywatch.el --- DETERRED dashboard for ActivityWatch -*- lexical-binding: t -*-

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

;; A dashboard for ActivityWatch, corresponding to `deterred-activitywatch'.

;;; Code:
(require 'deterred-dashboard)
(require 'deterred-db)
(require 'deterred-utils)

(defclass deterred-dashboard-activitywatch (deterred-dashboard)
  ((name :initform "ActivityWatch"))
  "A DETERRED dashboard for ActivityWatch.")

(cl-defmethod deterred-dashboard-default-params ((_dashboard deterred-dashboard-activitywatch))
  "Default parameters for the ActivityWatch dashboard."
  '((:start-date)
    (:end-date)
    (:hostname)
    (:n-top-apps . 5)))

(cl-defmethod deterred-dashboard-render-params ((_dashboard deterred-dashboard-activitywatch))
  "Render the parameters section for the ActivityWatch dashboard."
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
         (hostnames
          (mapcar
           (lambda (item) (alist-get 'hostname item))
           (deterred-db-select-alist
            db "SELECT DISTINCT hostname FROM activitywatch_currentwindow_agg ORDER BY hostname"))))
    (deterred-dashboard-widget-completing-read-multiple
     :name "Hostname"
     :key :hostname
     :options hostnames))
  (insert "\n")
  (deterred-dashboard-widget-number
   :name "Top N apps"
   :key :n-top-apps)
  (insert "\n"))

(cl-defmethod deterred-dashboard-list-datasets ((_dashboard deterred-dashboard-activitywatch))
  "List datasets for the ActivityWatch dashboard."
  '((notafk-per-year (name . "Not-AFK time per year"))
    (notafk-per-month (name . "Not-AFK time per month"))
    (notafk-per-hostname-per-year (name . "Hours per hostname per year"))
    (emacs-fraction-per-month (name . "Fraction of time spent in Emacs per month"))
    (top-apps (name . "Top applications"))
    (top-hostnames (name . "Top hostnames"))
    (top-days (name . "Top days by not-AFK time"))
    (top-weeks (name . "Top weeks by not-AFK time"))
    (top-months (name . "Top months by not-AFK time"))
    (top-apps-by-month (name . "Time spent in top N apps by month"))
    (apps-discovered (name . "Apps discovered per year"))
    (new-apps-per-year (name . "Hours in new vs. old apps per year"))
    (average-app-age-per-month (name . "Average app age per month"))
    (numbers-data (name . "Numerical data"))))

(cl-defmethod deterred-dashboard-fetch-datasets ((_dashboard deterred-dashboard-activitywatch)
                                                 params)
  "Fetch datasets for the ActivityWatch dashboard.

PARAMS is as returned by `deterred-dashboard-default-params'."
  (let* ((db (deterred-db--init))
         (numbers-data
          (deterred-db-select-template-alist
           db
           "SELECT (
  SELECT
    CAST(sum(total_duration) / (60 * 60) * 100 AS integer) / 100.0 hours
  FROM activitywatch_currentwindow_agg
  WHERE 1 = 1 [[AND day >= date(:start-date, 'unixepoch')]]
    [[AND day <= date(:end-date, 'unixepoch')]]
    [[AND hostname IN :hostname]]
) AS total_hours,
(
  SELECT count(DISTINCT app)
  FROM activitywatch_currentwindow_agg
  WHERE 1 = 1 [[AND day >= date(:start-date, 'unixepoch')]]
    [[AND day <= date(:end-date, 'unixepoch')]]
    [[AND hostname IN :hostname]]
) AS app_count,
(
  SELECT count(DISTINCT hostname)
  FROM activitywatch_currentwindow_agg
  WHERE 1 = 1 [[AND day >= date(:start-date, 'unixepoch')]]
    [[AND day <= date(:end-date, 'unixepoch')]]
    [[AND hostname IN :hostname]]
) AS hostname_count,
(
  SELECT count(DISTINCT day)
  FROM activitywatch_currentwindow_agg
  WHERE 1 = 1 [[AND day >= date(:start-date, 'unixepoch')]]
    [[AND day <= date(:end-date, 'unixepoch')]]
    [[AND hostname IN :hostname]]
) AS day_count;"
           params))
         (total-hours (alist-get 'total_hours (car numbers-data))))
    `((top-apps
       . ,(deterred-utils-add-fraction
           (deterred-db-select-template-alist
            db
            "SELECT
  app,
  CAST(sum(total_duration) / (60 * 60) * 100 AS integer) / 100.0 hours
FROM activitywatch_currentwindow_agg
WHERE 1 = 1 [[AND day >= date(:start-date, 'unixepoch')]]
  [[AND day <= date(:end-date, 'unixepoch')]]
  [[AND hostname IN :hostname]]
GROUP BY app
ORDER BY hours DESC
LIMIT 20"
            params)
           total-hours))
      (top-hostnames
       . ,(deterred-utils-add-fraction
           (deterred-db-select-template-alist
            db
            "SELECT
  hostname,
  CAST(sum(total_duration) / (60 * 60) * 100 AS integer) / 100.0 hours
FROM activitywatch_currentwindow_agg
WHERE 1 = 1 [[AND day >= date(:start-date, 'unixepoch')]]
  [[AND day <= date(:end-date, 'unixepoch')]]
  [[AND hostname IN :hostname]]
GROUP BY hostname
ORDER BY hours DESC"
            params)
           total-hours))
      (notafk-per-year
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y', day) year,
  CAST(sum(total_duration) / (60 * 60) * 100 AS integer) / 100.0 hours
FROM activitywatch_currentwindow_agg
WHERE 1 = 1 [[AND day >= date(:start-date, 'unixepoch')]]
  [[AND day <= date(:end-date, 'unixepoch')]]
  [[AND hostname IN :hostname]]
GROUP BY strftime('%Y', day)"
           params))
      (notafk-per-month
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y-%m', day) month,
  CAST(sum(total_duration) / (60 * 60) * 100 AS integer) / 100.0 hours
FROM activitywatch_currentwindow_agg
WHERE 1 = 1 [[AND day >= date(:start-date, 'unixepoch')]]
  [[AND day <= date(:end-date, 'unixepoch')]]
  [[AND hostname IN :hostname]]
GROUP BY strftime('%Y-%m', day)"
           params))
      (notafk-per-hostname-per-year
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y', day) year,
  hostname,
  CAST(sum(total_duration) / (60 * 60) * 100 AS integer) / 100.0 hours
FROM activitywatch_currentwindow_agg
WHERE 1 = 1 [[AND day >= date(:start-date, 'unixepoch')]]
  [[AND day <= date(:end-date, 'unixepoch')]]
  [[AND hostname IN :hostname]]
GROUP BY strftime('%Y', day), hostname
ORDER BY year ASC"
           params))
      (emacs-fraction-per-month
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y-%m', day) month,
  sum(CASE WHEN app = 'Emacs' THEN total_duration ELSE 0 END) * 100.0 / sum(total_duration) fraction
FROM activitywatch_currentwindow_agg
WHERE 1 = 1 [[AND day >= date(:start-date, 'unixepoch')]]
  [[AND day <= date(:end-date, 'unixepoch')]]
  [[AND hostname IN :hostname]]
GROUP BY strftime('%Y-%m', day)"
           params))
      (top-days
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  day,
  CAST(sum(total_duration) / (60 * 60) * 100 AS integer) / 100.0 hours
FROM activitywatch_currentwindow_agg
WHERE 1 = 1 [[AND day >= date(:start-date, 'unixepoch')]]
  [[AND day <= date(:end-date, 'unixepoch')]]
  [[AND hostname IN :hostname]]
GROUP BY day
ORDER BY hours DESC
LIMIT 20"
           params))
      (top-weeks
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y-%W', day) week,
  CAST(sum(total_duration) / (60 * 60) * 100 AS integer) / 100.0 hours
FROM activitywatch_currentwindow_agg
WHERE 1 = 1 [[AND day >= date(:start-date, 'unixepoch')]]
  [[AND day <= date(:end-date, 'unixepoch')]]
  [[AND hostname IN :hostname]]
GROUP BY strftime('%Y-%W', day)
ORDER BY hours DESC
LIMIT 20"
           params))
      (top-months
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y-%m', day) month,
  CAST(sum(total_duration) / (60 * 60) * 100 AS integer) / 100.0 hours
FROM activitywatch_currentwindow_agg
WHERE 1 = 1 [[AND day >= date(:start-date, 'unixepoch')]]
  [[AND day <= date(:end-date, 'unixepoch')]]
  [[AND hostname IN :hostname]]
GROUP BY strftime('%Y-%m', day)
ORDER BY hours DESC
LIMIT 20"
           params))
      (top-apps-by-month
       . ,(deterred-db-select-template-alist
           db
           "WITH top_apps AS (
  SELECT
    app,
    sum(total_duration) total
  FROM activitywatch_currentwindow_agg
  WHERE 1 = 1 [[AND day >= date(:start-date, 'unixepoch')]]
    [[AND day <= date(:end-date, 'unixepoch')]]
    [[AND hostname IN :hostname]]
  GROUP BY app
  ORDER BY total DESC
  LIMIT :n-top-apps
)
SELECT
  strftime('%Y-%m', day) month,
  activitywatch_currentwindow_agg.app app,
  CAST(sum(total_duration) / (60 * 60) * 100 AS integer) / 100.0 hours
FROM activitywatch_currentwindow_agg
INNER JOIN top_apps ta ON ta.app = activitywatch_currentwindow_agg.app
WHERE 1 = 1 [[AND day >= date(:start-date, 'unixepoch')]]
  [[AND day <= date(:end-date, 'unixepoch')]]
  [[AND hostname IN :hostname]]
GROUP BY strftime('%Y-%m', day), activitywatch_currentwindow_agg.app
ORDER BY month ASC"
           params))
      (apps-discovered
       . ,(deterred-db-select-template-alist
           db
           "WITH app_discovered_years AS (
  SELECT DISTINCT
    strftime('%Y', min(day)) year_discovered,
    app
  FROM activitywatch_currentwindow_agg
  WHERE 1 = 1 [[AND day >= date(:start-date, 'unixepoch')]]
    [[AND day <= date(:end-date, 'unixepoch')]]
    [[AND hostname IN :hostname]]
  GROUP BY app
)
SELECT year_discovered, count(*) new_apps FROM app_discovered_years
GROUP BY year_discovered"
           params))
      (new-apps-per-year
       . ,(deterred-db-select-template-alist
           db
           "WITH app_discovered_years AS (
  SELECT STRFTIME('%Y', min(day)) year, app FROM activitywatch_currentwindow_agg
  GROUP BY app
)
SELECT
  STRFTIME('%Y', day) \"year\",
  sum(CASE WHEN ady.year = STRFTIME('%Y', day) THEN total_duration ELSE 0 END) * 100 / (60 * 60) / 100.0 \"new\",
  sum(CASE WHEN ady.year != STRFTIME('%Y', day) THEN total_duration ELSE 0 END) * 100 / (60 * 60) / 100.0 \"old\"
FROM activitywatch_currentwindow_agg
INNER JOIN app_discovered_years ady ON ady.app = activitywatch_currentwindow_agg.app
WHERE 1 = 1 [[AND day >= date(:start-date, 'unixepoch')]]
  [[AND day <= date(:end-date, 'unixepoch')]]
  [[AND hostname IN :hostname]]
GROUP BY STRFTIME('%Y', day)"
           params))
      (average-app-age-per-month
       . ,(deterred-db-select-template-alist
           db
           "WITH app_discovered_months AS (
  SELECT
    CAST(STRFTIME('%Y', min(day)) AS INTEGER) * 12 +
    CAST(STRFTIME('%m', min(day)) AS INTEGER) start_month,
    app
  FROM activitywatch_currentwindow_agg
  GROUP BY app
), hours_per_month AS (
  SELECT
    STRFTIME('%Y-%m', day) \"month\",
    SUM(total_duration / (60.0 * 60.0)) hours_spent
  FROM activitywatch_currentwindow_agg
  WHERE 1 = 1 [[AND day >= date(:start-date, 'unixepoch')]]
    [[AND day <= date(:end-date, 'unixepoch')]]
    [[AND hostname IN :hostname]]
  GROUP BY STRFTIME('%Y-%m', day)
), app_data AS (
  SELECT
    (
      (
        CAST(STRFTIME('%Y', day) AS INTEGER) * 12 +
        CAST(STRFTIME('%m', day) AS INTEGER)
      ) - adm.start_month
    ) app_age,
    SUM(total_duration / (60.0 * 60.0)) / hpm.hours_spent fraction,
    STRFTIME('%Y-%m', day) \"month\",
    activitywatch_currentwindow_agg.app
  FROM activitywatch_currentwindow_agg
  INNER JOIN app_discovered_months adm ON adm.app = activitywatch_currentwindow_agg.app
  INNER JOIN hours_per_month hpm ON hpm.month = STRFTIME('%Y-%m', day)
  WHERE 1 = 1
    [[AND day >= date(:start-date, 'unixepoch')]]
    [[AND day <= date(:end-date, 'unixepoch')]]
    [[AND hostname IN :hostname]]
  GROUP BY STRFTIME('%Y-%m', day), activitywatch_currentwindow_agg.app
  ORDER BY month, fraction DESC
)
SELECT sum(app_age * fraction) age, month
FROM app_data
GROUP BY month
ORDER BY month ASC"
           params))
      (numbers-data . ,numbers-data))))

(cl-defmethod deterred-dashboard-render-results ((_dashboard deterred-dashboard-activitywatch)
                                                 _params data)
  "Render DATA for the ActivityWatch dashboard."
  (insert
   (deterred-format
    "I've spent "
    (f-ace (f (f-num (f-acc "data->'numbers-data->'data[0]->'total_hours")))
           'bold)
    " not-AFK hours across "
    (f-ace (f (f-num (f-acc "data->'numbers-data->'data[0]->'app_count")))
           'bold)
    " unique applications on "
    (f-ace (f (f-num (f-acc "data->'numbers-data->'data[0]->'hostname_count")))
           'bold)
    " hostnames over "
    (f-ace (f (f-num (f-acc "data->'numbers-data->'data[0]->'day_count")))
           'bold)
    " unique days.\n\n"
    (f-h2 "Top applications and hostnames") "\n"
    (f-h3 "Top applications") "\n")
   (deterred-grid-print-with-org
    (alist-get 'data (alist-get 'top-apps data))
    :column-names '((app . "Application") (hours . "Hours") (fraction . "%"))
    :max-rows 10
    :grid-button t)
   "\n"
   (deterred-format (f-h3 "Top hostnames") "\n")
   (deterred-grid-print-with-org
    (alist-get 'data (alist-get 'top-hostnames data))
    :column-names '((hostname . "Hostname") (hours . "Hours") (fraction . "%"))
    :max-rows 10
    :grid-button t)
   "\n"
   (deterred-format (f-h2 "Activity over time") "\n"))
  (deterred-dashboard-exec-python
   :python-code
   "from matplotlib import pyplot as plt
from matplotlib.ticker import MaxNLocator
from deterred import fig_to_b64

import pandas as pd

import json

data = json.loads(input())
df_y = pd.DataFrame(data['notafk-per-year']['data'])
df_m = pd.DataFrame(data['notafk-per-month']['data'])
df_apps = pd.DataFrame(data['top-apps-by-month']['data'])
df_apps_p = df_apps.pivot(index='month', columns='app', values='hours').fillna(0)
df_hostname_year = pd.DataFrame(data['notafk-per-hostname-per-year']['data'])
df_hostname_year_p = df_hostname_year.pivot(index='year', columns='hostname', values='hours').fillna(0)
df_emacs = pd.DataFrame(data['emacs-fraction-per-month']['data'])

images = []

fig, ax = plt.subplots(figsize=(8, 5))
df_y.plot(ax=ax, kind='bar', x='year', y='hours')
ax.set_title('Not-AFK hours per year')
for container in ax.containers:
    ax.bar_label(container, fmt='%.0f')
images.append(fig_to_b64(fig))

fig, ax = plt.subplots(figsize=(8, 5))
df_m.plot(ax=ax, kind='bar', x='month', y='hours')
ax.set_title('Not-AFK hours per month')
if len(df_m) > 30:
    ax.xaxis.set_major_locator(MaxNLocator(nbins=40))
if len(df_m) < 30:
    for container in ax.containers:
        ax.bar_label(container, fmt='%.2f')
images.append(fig_to_b64(fig))

fig, ax = plt.subplots(figsize=(8, 5))
df_hostname_year_p.plot(ax=ax, kind='bar', stacked=True)
ax.set_title('Hours per hostname per year')
ax.set_xlabel('Year')
ax.set_ylabel('Hours')
images.append(fig_to_b64(fig))

fig, ax = plt.subplots(figsize=(8, 5))
df_emacs.plot(ax=ax, kind='line', x='month', y='fraction', legend=False)
ax.set_title('Fraction of time spent in Emacs per month')
ax.set_xlabel('Month')
ax.set_ylabel('Fraction (%)')
images.append(fig_to_b64(fig))

fig, ax = plt.subplots(figsize=(8, 5))
df_apps_p.plot(ax=ax, kind='line')
ax.set_title('Hours spent in top N apps by month')
images.append(fig_to_b64(fig))

print(json.dumps(images))"
   :input data
   :on-success
   (lambda (images)
     (insert (deterred-format (f-h3 "Not-AFK hours per year") "\n"))
     (deterred-dashboard-print-images-base64 (elt images 0))
     (insert "\n")
     (insert (deterred-format (f-h3 "Not-AFK hours per month") "\n"))
     (deterred-dashboard-print-images-base64 (elt images 1))
     (insert "\n")
     (insert (deterred-format (f-h3 "Hours per hostname per year") "\n"))
     (deterred-dashboard-print-images-base64 (elt images 2))
     (insert "\n")
     (insert (deterred-format (f-h3 "Fraction of time spent in Emacs per month") "\n"))
     (deterred-dashboard-print-images-base64 (elt images 3))
     (insert "\n")
     (insert (deterred-format (f-h3 "Hours spent in top N apps by month") "\n"))
     (deterred-dashboard-print-images-base64 (elt images 4))
     (insert "\n")))
  (insert (deterred-format (f-h2 "App discovery over time") "\n"))
  (deterred-dashboard-exec-python
   :python-code
   "from matplotlib import pyplot as plt
from matplotlib.ticker import MaxNLocator
from deterred import fig_to_b64

import pandas as pd
import numpy as np

import json

data = json.loads(input())
df_apps = pd.DataFrame(data['apps-discovered']['data'])
df_new = pd.DataFrame(data['new-apps-per-year']['data'])
df_age = pd.DataFrame(data['average-app-age-per-month']['data'])

images = []

fig, ax = plt.subplots(figsize=(8, 5))
df_apps.plot(ax=ax, kind='bar', x='year_discovered', y='new_apps')
ax.set_title('New apps discovered per year')
images.append(fig_to_b64(fig))

fig, ax = plt.subplots(figsize=(8, 5))
df_new.plot(ax=ax, kind='bar', x='year', stacked=True)
ax.set_title('Hours spent in new vs. old apps per year')
images.append(fig_to_b64(fig))

fig, ax = plt.subplots(figsize=(8, 5))
ax.set_title('Average app age (in months) per month')
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
     (insert (deterred-format (f-h3 "New apps discovered per year") "\n"))
     (deterred-dashboard-print-images-base64 (elt images 0))
     (insert "\n")
     (insert (deterred-format (f-h3 "Hours spent in new vs. old apps per year") "\n"))
     (deterred-dashboard-print-images-base64 (elt images 1))
     (insert "\n")
     (insert (deterred-format (f-h3 "Average app age per month") "\n"))
     (deterred-dashboard-print-images-base64 (elt images 2))
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

(provide 'deterred-dashboard-activitywatch)
;;; deterred-dashboard-activitywatch.el ends here

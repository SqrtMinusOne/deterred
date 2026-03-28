;;; deterred-dashboard-transport.el --- DETERRED dashboard for transport -*- lexical-binding: t -*-

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

;; A dashboard for transport, corresponding to `deterred-transport'.

;;; Code:
(require 'deterred-dashboard)
(require 'deterred-db)
(require 'deterred-locations)
(require 'deterred-utils)

(defclass deterred-dashboard-transport (deterred-dashboard)
  ((name :initform "Transport"))
  "A DETERRED dashboard for transport trips.")

(cl-defmethod deterred-dashboard-default-params ((_dashboard deterred-dashboard-transport))
  "Default parameters for the Transport dashboard."
  '((:start-date)
    (:end-date)
    (:recent-days . 14)))

(cl-defmethod deterred-dashboard-render-params ((_dashboard deterred-dashboard-transport))
  "Render the parameters section for the Transport dashboard."
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
  (deterred-dashboard-widget-number
   :name "Recent days for heatmap"
   :key :recent-days)
  (insert "\n"))

(cl-defmethod deterred-dashboard-list-datasets ((_dashboard deterred-dashboard-transport))
  "List datasets for the Transport dashboard."
  '((trips-per-year (name . "Trips per year"))
    (trips-per-month-by-transport (name . "Trips per month by transport type"))
    (cost-per-year (name . "Cost per year"))
    (cost-per-month (name . "Cost per month"))
    (trips-by-transport (name . "Trips by transport type"))
    (cost-by-transport-by-year (name . "Cost by transport type per year"))
    (trips-by-day-of-week (name . "Trips by day of week"))
    (trips-by-hour (name . "Trips by hour of day"))
    (top-routes (name . "Top routes"))
    (recent-trips (name . "Recent trips by hour"))
    (numbers-data (name . "Numerical data"))))

(cl-defmethod deterred-dashboard-fetch-datasets ((_dashboard deterred-dashboard-transport)
                                                 params)
  "Fetch datasets for the Transport dashboard.

PARAMS is as returned by `deterred-dashboard-default-params'."
  (let* ((db (deterred-db--init))
         (numbers-data
          (deterred-db-select-template-alist
           db
           "SELECT
  (SELECT COUNT(*) FROM transport_trips
   WHERE 1 = 1 [[AND timestamp >= :start-date]]
     [[AND timestamp <= :end-date]]) AS total_trips,
  (SELECT COALESCE(SUM(cost), 0) FROM transport_trips
   WHERE 1 = 1 [[AND timestamp >= :start-date]]
     [[AND timestamp <= :end-date]]) AS total_cost,
  (SELECT COUNT(DISTINCT transport) FROM transport_trips
   WHERE 1 = 1 [[AND timestamp >= :start-date]]
     [[AND timestamp <= :end-date]]) AS transport_types,
  (SELECT COUNT(DISTINCT route) FROM transport_trips
   WHERE 1 = 1 [[AND timestamp >= :start-date]]
     [[AND timestamp <= :end-date]]) AS unique_routes"
           params)))
    `((trips-per-year
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y', timestamp, 'unixepoch') year,
  COUNT(*) trips
FROM transport_trips
WHERE 1 = 1 [[AND timestamp >= :start-date]]
  [[AND timestamp <= :end-date]]
GROUP BY strftime('%Y', timestamp, 'unixepoch')
ORDER BY year"
           params))
      (trips-per-month-by-transport
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y-%m', timestamp, 'unixepoch') month,
  transport,
  COUNT(*) trips
FROM transport_trips
WHERE 1 = 1 [[AND timestamp >= :start-date]]
  [[AND timestamp <= :end-date]]
GROUP BY strftime('%Y-%m', timestamp, 'unixepoch'), transport
ORDER BY month"
           params))
      (cost-per-year
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y', timestamp, 'unixepoch') year,
  CAST(SUM(cost) * 100 AS integer) / 100.0 cost
FROM transport_trips
WHERE 1 = 1 [[AND timestamp >= :start-date]]
  [[AND timestamp <= :end-date]]
GROUP BY strftime('%Y', timestamp, 'unixepoch')
ORDER BY year"
           params))
      (cost-per-month
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y-%m', timestamp, 'unixepoch') month,
  CAST(SUM(cost) * 100 AS integer) / 100.0 cost
FROM transport_trips
WHERE 1 = 1 [[AND timestamp >= :start-date]]
  [[AND timestamp <= :end-date]]
GROUP BY strftime('%Y-%m', timestamp, 'unixepoch')
ORDER BY month"
           params))
      (trips-by-transport
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  transport,
  COUNT(*) trips,
  CAST(SUM(cost) * 100 AS integer) / 100.0 total_cost,
  CAST(AVG(cost) * 100 AS integer) / 100.0 avg_cost
FROM transport_trips
WHERE 1 = 1 [[AND timestamp >= :start-date]]
  [[AND timestamp <= :end-date]]
GROUP BY transport
ORDER BY trips DESC"
           params))
      (cost-by-transport-by-year
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y', timestamp, 'unixepoch') year,
  transport,
  CAST(SUM(cost) * 100 AS integer) / 100.0 cost
FROM transport_trips
WHERE 1 = 1 [[AND timestamp >= :start-date]]
  [[AND timestamp <= :end-date]]
GROUP BY strftime('%Y', timestamp, 'unixepoch'), transport
ORDER BY year"
           params))
      (trips-by-day-of-week
       . ,(let* ((raw-trips (deterred-db-select-template-alist
                             db
                             "SELECT timestamp FROM transport_trips
WHERE 1 = 1 [[AND timestamp >= :start-date]]
  [[AND timestamp <= :end-date]]"
                             params))
                 (day-counts (make-vector 7 0))
                 (day-names ["Sunday" "Monday" "Tuesday" "Wednesday"
                             "Thursday" "Friday" "Saturday"]))
            (dolist (trip raw-trips)
              (let* ((ts (alist-get 'timestamp trip))
                     (offset (deterred-locations-offset-at ts nil db))
                     (local-ts (+ ts offset))
                     (day-num (string-to-number
                               (format-time-string "%w" local-ts t))))
                (aset day-counts day-num (1+ (aref day-counts day-num)))))
            (cl-loop for i from 0 to 6
                     collect `((day_num . ,i)
                               (day_name . ,(aref day-names i))
                               (trips . ,(aref day-counts i))))))
      (trips-by-hour
       . ,(let* ((raw-trips (deterred-db-select-template-alist
                             db
                             "SELECT timestamp FROM transport_trips
WHERE 1 = 1 [[AND timestamp >= :start-date]]
  [[AND timestamp <= :end-date]]"
                             params))
                 (hour-counts (make-vector 24 0)))
            (dolist (trip raw-trips)
              (let* ((ts (alist-get 'timestamp trip))
                     (offset (deterred-locations-offset-at ts nil db))
                     (local-ts (+ ts offset))
                     (hour (string-to-number
                            (format-time-string "%H" local-ts t))))
                (aset hour-counts hour (1+ (aref hour-counts hour)))))
            (cl-loop for i from 0 to 23
                     collect `((hour . ,i)
                               (trips . ,(aref hour-counts i))))))
      (top-routes
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  route,
  transport,
  COUNT(*) trips,
  CAST(SUM(cost) * 100 AS integer) / 100.0 total_cost
FROM transport_trips
WHERE 1 = 1 [[AND timestamp >= :start-date]]
  [[AND timestamp <= :end-date]]
GROUP BY route, transport
ORDER BY trips DESC
LIMIT 20"
           params))
      (recent-trips
       . ,(let ((raw-trips (deterred-db-select-template-alist
                            db
                            "SELECT
  timestamp,
  transport,
  route,
  cost
FROM transport_trips
WHERE date(timestamp, 'unixepoch') >= date(COALESCE(:end-date, strftime('%s', 'now')), 'unixepoch', '-' || :recent-days || ' days')
  [[AND timestamp >= :start-date]]
  [[AND timestamp <= :end-date]]
ORDER BY timestamp"
                            params)))
            (mapcar
             (lambda (trip)
               (let* ((ts (alist-get 'timestamp trip))
                      (offset (deterred-locations-offset-at ts nil db))
                      (local-ts (+ ts offset)))
                 `((day . ,(format-time-string "%Y-%m-%d" local-ts t))
                   (local_timestamp . ,local-ts)
                   (transport . ,(alist-get 'transport trip))
                   (route . ,(alist-get 'route trip))
                   (cost . ,(alist-get 'cost trip)))))
             raw-trips)))
      (numbers-data . ,numbers-data))))

(cl-defmethod deterred-dashboard-render-results ((_dashboard deterred-dashboard-transport)
                                                 _params data)
  "Render DATA for the Transport dashboard."
  (insert
   (deterred-format
    "I've taken "
    (f-ace (f (f-num (f-acc "data->'numbers-data->'data[0]->'total_trips")))
           'bold)
    " trips across "
    (f-ace (f (f-num (f-acc "data->'numbers-data->'data[0]->'transport_types")))
           'bold)
    " transport types on "
    (f-ace (f (f-num (f-acc "data->'numbers-data->'data[0]->'unique_routes")))
           'bold)
    " unique routes, spending "
    (f-ace (f (f-num (f-acc "data->'numbers-data->'data[0]->'total_cost")))
           'bold)
    " in total.\n\n"
    (f-h2 "Trips over time") "\n"))
  (deterred-dashboard-exec-python
   :python-code
   "from matplotlib import pyplot as plt
from matplotlib.ticker import MaxNLocator
from deterred import fig_to_b64

import pandas as pd
import json

data = json.loads(input())
df_y = pd.DataFrame(data['trips-per-year']['data'])
df_m = pd.DataFrame(data['trips-per-month-by-transport']['data'])

images = []

# Trips per year
if not df_y.empty:
    fig, ax = plt.subplots(figsize=(8, 5))
    df_y.plot(ax=ax, kind='bar', x='year', y='trips', legend=False)
    ax.set_title('Trips per year')
    ax.set_xlabel('Year')
    ax.set_ylabel('Trips')
    for container in ax.containers:
        ax.bar_label(container, fmt='%.0f')
    plt.tight_layout()
    images.append(fig_to_b64(fig))
else:
    images.append(None)

# Trips per month by transport type (stacked)
if not df_m.empty:
    fig, ax = plt.subplots(figsize=(10, 5))
    df_pivot = df_m.pivot(index='month', columns='transport', values='trips').fillna(0)
    df_pivot.plot(ax=ax, kind='bar', stacked=True)
    ax.set_title('Trips per month by transport type')
    ax.set_xlabel('Month')
    ax.set_ylabel('Trips')
    if len(df_pivot) > 20:
        ax.xaxis.set_major_locator(MaxNLocator(nbins=20))
    plt.xticks(rotation=45, ha='right')
    plt.tight_layout()
    images.append(fig_to_b64(fig))
else:
    images.append(None)

print(json.dumps(images))"
   :input data
   :on-success
   (lambda (images)
     (when (elt images 0)
       (insert (deterred-format (f-h3 "Trips per year") "\n"))
       (deterred-dashboard-print-images-base64 (elt images 0))
       (insert "\n"))
     (when (elt images 1)
       (insert (deterred-format (f-h3 "Trips per month by transport type") "\n"))
       (deterred-dashboard-print-images-base64 (elt images 1))
       (insert "\n"))))
  (insert (deterred-format (f-h2 "Cost over time") "\n"))
  (deterred-dashboard-exec-python
   :python-code
   "from matplotlib import pyplot as plt
from matplotlib.ticker import MaxNLocator
from deterred import fig_to_b64

import pandas as pd
import json

data = json.loads(input())
df_y = pd.DataFrame(data['cost-per-year']['data'])
df_m = pd.DataFrame(data['cost-per-month']['data'])
df_ct = pd.DataFrame(data['cost-by-transport-by-year']['data'])

images = []

# Cost per year
if not df_y.empty:
    fig, ax = plt.subplots(figsize=(8, 5))
    df_y.plot(ax=ax, kind='bar', x='year', y='cost', legend=False)
    ax.set_title('Cost per year')
    ax.set_xlabel('Year')
    ax.set_ylabel('Cost')
    for container in ax.containers:
        ax.bar_label(container, fmt='%.0f')
    plt.tight_layout()
    images.append(fig_to_b64(fig))
else:
    images.append(None)

# Cost per month
if not df_m.empty:
    fig, ax = plt.subplots(figsize=(10, 5))
    df_m.plot(ax=ax, kind='bar', x='month', y='cost', legend=False)
    ax.set_title('Cost per month')
    ax.set_xlabel('Month')
    ax.set_ylabel('Cost')
    if len(df_m) > 20:
        ax.xaxis.set_major_locator(MaxNLocator(nbins=20))
    plt.xticks(rotation=45, ha='right')
    plt.tight_layout()
    images.append(fig_to_b64(fig))
else:
    images.append(None)

# Cost by transport type per year (stacked)
if not df_ct.empty:
    fig, ax = plt.subplots(figsize=(8, 5))
    df_pivot = df_ct.pivot(index='year', columns='transport', values='cost').fillna(0)
    df_pivot.plot(ax=ax, kind='bar', stacked=True)
    ax.set_title('Cost by transport type per year')
    ax.set_xlabel('Year')
    ax.set_ylabel('Cost')
    plt.tight_layout()
    images.append(fig_to_b64(fig))
else:
    images.append(None)

print(json.dumps(images))"
   :input data
   :on-success
   (lambda (images)
     (when (elt images 0)
       (insert (deterred-format (f-h3 "Cost per year") "\n"))
       (deterred-dashboard-print-images-base64 (elt images 0))
       (insert "\n"))
     (when (elt images 1)
       (insert (deterred-format (f-h3 "Cost per month") "\n"))
       (deterred-dashboard-print-images-base64 (elt images 1))
       (insert "\n"))
     (when (elt images 2)
       (insert (deterred-format (f-h3 "Cost by transport type per year") "\n"))
       (deterred-dashboard-print-images-base64 (elt images 2))
       (insert "\n"))))
  (insert
   (deterred-format (f-h2 "Transport breakdown") "\n"
                    (f-h3 "Trips by transport type") "\n")
   (deterred-grid-print-with-org
    (alist-get 'data (alist-get 'trips-by-transport data))
    :column-names '((transport . "Transport") (trips . "Trips")
                    (total_cost . "Total Cost") (avg_cost . "Avg Cost"))
    :max-rows 10
    :grid-button t)
   "\n")
  (insert (deterred-format (f-h2 "Trip patterns") "\n"))
  (deterred-dashboard-exec-python
   :python-code
   "from matplotlib import pyplot as plt
from deterred import fig_to_b64

import pandas as pd
import json

data = json.loads(input())
df_dow = pd.DataFrame(data['trips-by-day-of-week']['data'])
df_hour = pd.DataFrame(data['trips-by-hour']['data'])

images = []

# Trips by day of week
if not df_dow.empty:
    fig, ax = plt.subplots(figsize=(8, 5))
    df_dow.plot(ax=ax, kind='bar', x='day_name', y='trips', legend=False)
    ax.set_title('Trips by day of week')
    ax.set_xlabel('Day')
    ax.set_ylabel('Trips')
    for container in ax.containers:
        ax.bar_label(container, fmt='%.0f')
    plt.tight_layout()
    images.append(fig_to_b64(fig))
else:
    images.append(None)

# Trips by hour of day
if not df_hour.empty:
    fig, ax = plt.subplots(figsize=(10, 5))
    # Fill missing hours with 0
    all_hours = pd.DataFrame({'hour': range(24)})
    df_hour = all_hours.merge(df_hour, on='hour', how='left').fillna(0)
    ax.bar(df_hour['hour'], df_hour['trips'])
    ax.set_title('Trips by hour of day')
    ax.set_xlabel('Hour')
    ax.set_ylabel('Trips')
    ax.set_xticks(range(24))
    plt.tight_layout()
    images.append(fig_to_b64(fig))
else:
    images.append(None)

print(json.dumps(images))"
   :input data
   :on-success
   (lambda (images)
     (when (elt images 0)
       (insert (deterred-format (f-h3 "Trips by day of week") "\n"))
       (deterred-dashboard-print-images-base64 (elt images 0))
       (insert "\n"))
     (when (elt images 1)
       (insert (deterred-format (f-h3 "Trips by hour of day") "\n"))
       (deterred-dashboard-print-images-base64 (elt images 1))
       (insert "\n"))))
  (insert
   (deterred-format (f-h2 "Top routes") "\n")
   (deterred-grid-print-with-org
    (alist-get 'data (alist-get 'top-routes data))
    :column-names '((route . "Route") (transport . "Transport")
                    (trips . "Trips") (total_cost . "Total Cost"))
    :max-rows 20
    :grid-button t)
   "\n")
  (insert (deterred-format (f-h2 "Recent activity") "\n"))
  (deterred-dashboard-exec-python
   :python-code
   "from matplotlib import pyplot as plt
from matplotlib.patches import Rectangle
from deterred import fig_to_b64
import matplotlib.patches as mpatches
from datetime import datetime, timezone

import pandas as pd
import numpy as np
import json

data = json.loads(input())
df = pd.DataFrame(data['recent-trips']['data'])

images = []

if not df.empty:
    all_days = sorted(df['day'].unique())
    transports = df['transport'].unique()
    colors = plt.cm.tab10(np.linspace(0, 1, len(transports)))
    transport_colors = dict(zip(transports, colors))

    day_to_x = {day: i for i, day in enumerate(all_days)}

    fig, ax = plt.subplots(figsize=(12, 6))

    for _, row in df.iterrows():
        # Use local_timestamp which already has timezone offset applied
        dt = datetime.fromtimestamp(row['local_timestamp'], tz=timezone.utc)
        day = row['day']

        if day in day_to_x:
            x_pos = day_to_x[day]
            hour = dt.hour + dt.minute / 60.0
            color = transport_colors[row['transport']]

            # Draw a small rectangle for each trip
            rect = Rectangle((x_pos - 0.4, hour - 0.25),
                             0.8, 0.5,
                             facecolor=color,
                             alpha=0.7,
                             edgecolor='none')
            ax.add_patch(rect)

    legend_elements = [mpatches.Patch(facecolor=transport_colors[t],
                                      alpha=0.7,
                                      label=t)
                      for t in transports]
    ax.legend(handles=legend_elements, title='Transport')

    ax.set_title('Recent trips by time of day')
    ax.set_xlabel('Day')
    ax.set_ylabel('Hour of day')
    ax.set_xlim(-0.5, len(all_days) - 0.5)
    ax.set_ylim(0, 24)
    ax.set_xticks(range(len(all_days)))
    ax.set_xticklabels(all_days, rotation=45, ha='right')
    ax.set_yticks(range(0, 25, 2))
    ax.grid(True, alpha=0.3)
    plt.tight_layout()
    images.append(fig_to_b64(fig))
else:
    images.append(None)

print(json.dumps(images))"
   :input data
   :on-success
   (lambda (images)
     (when (elt images 0)
       (insert (deterred-format (f-h3 "Recent trips heatmap") "\n"))
       (deterred-dashboard-print-images-base64 (elt images 0))
       (insert "\n")))))

(provide 'deterred-dashboard-transport)
;;; deterred-dashboard-transport.el ends here

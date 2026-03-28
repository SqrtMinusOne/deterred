;;; deterred-dashboard-org-roam.el --- DETERRED dashboard for Org Roam -*- lexical-binding: t -*-

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

;; A dashboard for Org Roam, corresponding to `deterred-org-roam'.

;;; Code:
(require 'deterred-dashboard)
(require 'deterred-db)
(require 'deterred-utils)

(defclass deterred-dashboard-org-roam (deterred-dashboard)
  ((name :initform "Org Roam"))
  "A DETERRED dashboard for Org Roam.")

(cl-defmethod deterred-dashboard-default-params ((_dashboard deterred-dashboard-org-roam))
  "Default parameters for the Org Roam dashboard."
  '((:start-date)
    (:end-date)
    (:tag)))

(cl-defmethod deterred-dashboard-render-params ((_dashboard deterred-dashboard-org-roam))
  "Render the parameters section for the Org Roam dashboard."
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
         (tags
          (mapcar
           (lambda (item) (alist-get 'tag item))
           (deterred-db-select-alist
            db "SELECT DISTINCT tag FROM org_roam_node_tag WHERE timestamp_deleted IS NULL ORDER BY tag"))))
    (deterred-dashboard-widget-completing-read
     :name "Tag"
     :key :tag
     :options tags))
  (insert "\n"))

(cl-defmethod deterred-dashboard-list-datasets ((_dashboard deterred-dashboard-org-roam))
  "List datasets for the Org Roam dashboard."
  '((nodes-per-day (name . "Total nodes per day"))
    (nodes-created-per-month (name . "Nodes created per month"))
    (nodes-created-per-year (name . "Nodes created per year"))
    (nodes-modified-per-month (name . "Nodes modified per month"))
    (nodes-modified-per-year (name . "Nodes modified per year"))
    (nodes-by-tag-per-month (name . "Total nodes by tag per month"))
    (total-nodes-by-tag (name . "Total nodes by tag"))
    (nodes-by-heading-level-per-month (name . "Total nodes by heading level per month"))
    (total-nodes-by-heading-level (name . "Total nodes by heading level"))
    (numbers-data (name . "Numerical data"))))

(cl-defmethod deterred-dashboard-fetch-datasets ((_dashboard deterred-dashboard-org-roam)
                                                 params)
  "Fetch datasets for the Org Roam dashboard.

PARAMS is as returned by `deterred-dashboard-default-params'."
  (let* ((db (deterred-db--init))
         (tag (alist-get :tag params))
         (numbers-data
          (deterred-db-select-template-alist
           db
           "SELECT
  (SELECT count(*) FROM org_roam_node
   WHERE 1 = 1 [[AND timestamp >= :start-date]]
     [[AND timestamp <= :end-date]]
     AND (timestamp_deleted IS NULL OR timestamp_deleted > :end-date)) AS total_nodes,
  (SELECT count(DISTINCT tag) FROM org_roam_node_tag
   WHERE 1 = 1 [[AND timestamp >= :start-date]]
     [[AND timestamp <= :end-date]]
     AND (timestamp_deleted IS NULL OR timestamp_deleted > :end-date)) AS total_tags,
  (SELECT count(*) FROM org_roam_node
   WHERE (title LIKE '#%' AND title NOT LIKE '##%')
     [[AND timestamp >= :start-date]]
     [[AND timestamp <= :end-date]]
     AND (timestamp_deleted IS NULL OR timestamp_deleted > :end-date)) AS structure_single,
  (SELECT count(*) FROM org_roam_node
   WHERE title LIKE '##%'
     [[AND timestamp >= :start-date]]
     [[AND timestamp <= :end-date]]
     AND (timestamp_deleted IS NULL OR timestamp_deleted > :end-date)) AS structure_double"
           params)))
    `((nodes-per-day
       . ,(deterred-db-select-template-alist
           db
           "WITH RECURSIVE dates(day) AS (
  SELECT date(MIN(timestamp), 'unixepoch') FROM org_roam_node
  [[WHERE timestamp >= :start-date]]
  UNION ALL
  SELECT date(day, '+1 day') FROM dates
  WHERE day < (SELECT date(MAX(COALESCE(:end-date, timestamp)), 'unixepoch') FROM org_roam_node)
)
SELECT
  dates.day,
  (SELECT count(*) FROM org_roam_node
   WHERE date(timestamp, 'unixepoch') <= dates.day
     AND (timestamp_deleted IS NULL OR date(timestamp_deleted, 'unixepoch') > dates.day)) AS count
FROM dates
ORDER BY day ASC"
           params))
      (nodes-created-per-month
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y-%m', date(timestamp, 'unixepoch')) month,
  count(*) count
FROM org_roam_node
WHERE 1 = 1 [[AND timestamp >= :start-date]]
  [[AND timestamp <= :end-date]]
GROUP BY strftime('%Y-%m', date(timestamp, 'unixepoch'))
ORDER BY month ASC"
           params))
      (nodes-created-per-year
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y', date(timestamp, 'unixepoch')) year,
  count(*) count
FROM org_roam_node
WHERE 1 = 1 [[AND timestamp >= :start-date]]
  [[AND timestamp <= :end-date]]
GROUP BY strftime('%Y', date(timestamp, 'unixepoch'))
ORDER BY year ASC"
           params))
      (nodes-modified-per-month
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y-%m', date(ornm.timestamp, 'unixepoch')) month,
  count(DISTINCT node_id) count
FROM org_roam_node_modification ornm
INNER JOIN org_roam_node orn ON orn.id = ornm.node_id
WHERE ornm.timestamp != orn.timestamp
  [[AND ornm.timestamp >= :start-date]]
  [[AND ornm.timestamp <= :end-date]]
GROUP BY strftime('%Y-%m', date(ornm.timestamp, 'unixepoch'))
ORDER BY month ASC"
           params))
      (nodes-modified-per-year
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y', date(ornm.timestamp, 'unixepoch')) year,
  count(DISTINCT node_id) count
FROM org_roam_node_modification ornm
INNER JOIN org_roam_node orn ON orn.id = ornm.node_id
WHERE ornm.timestamp != orn.timestamp
  [[AND ornm.timestamp >= :start-date]]
  [[AND ornm.timestamp <= :end-date]]
GROUP BY strftime('%Y', date(ornm.timestamp, 'unixepoch'))
ORDER BY year ASC"
           params))
      (nodes-by-tag-per-month
       . ,(if tag
              (deterred-db-select-template-alist
               db
               "WITH RECURSIVE months(month) AS (
  SELECT strftime('%Y-%m', date(MIN(timestamp), 'unixepoch')) FROM org_roam_node
  [[WHERE timestamp >= :start-date]]
  UNION ALL
  SELECT strftime('%Y-%m', date(month || '-01', '+1 month')) FROM months
  WHERE month < (SELECT strftime('%Y-%m', date(MAX(COALESCE(:end-date, timestamp)), 'unixepoch')) FROM org_roam_node)
)
SELECT
  months.month,
  (SELECT count(DISTINCT orn.id) FROM org_roam_node orn
   INNER JOIN org_roam_node_tag ornt ON ornt.node_id = orn.id
   WHERE ornt.tag = :tag
     AND date(orn.timestamp, 'unixepoch') <= date(months.month || '-01', '+1 month', '-1 day')
     AND date(ornt.timestamp, 'unixepoch') <= date(months.month || '-01', '+1 month', '-1 day')
     AND (orn.timestamp_deleted IS NULL OR date(orn.timestamp_deleted, 'unixepoch') > date(months.month || '-01', '+1 month', '-1 day'))
     AND (ornt.timestamp_deleted IS NULL OR date(ornt.timestamp_deleted, 'unixepoch') > date(months.month || '-01', '+1 month', '-1 day'))) AS tagged,
  (SELECT count(*) FROM org_roam_node orn
   WHERE date(orn.timestamp, 'unixepoch') <= date(months.month || '-01', '+1 month', '-1 day')
     AND (orn.timestamp_deleted IS NULL OR date(orn.timestamp_deleted, 'unixepoch') > date(months.month || '-01', '+1 month', '-1 day'))
     AND orn.id NOT IN (
       SELECT node_id FROM org_roam_node_tag ornt2
       WHERE ornt2.tag = :tag
         AND date(ornt2.timestamp, 'unixepoch') <= date(months.month || '-01', '+1 month', '-1 day')
         AND (ornt2.timestamp_deleted IS NULL OR date(ornt2.timestamp_deleted, 'unixepoch') > date(months.month || '-01', '+1 month', '-1 day'))
     )) AS untagged
FROM months
ORDER BY month ASC"
               params)
            nil))
      (total-nodes-by-tag
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  ornt.tag,
  count(DISTINCT ornt.node_id) count
FROM org_roam_node_tag ornt
INNER JOIN org_roam_node orn ON orn.id = ornt.node_id
WHERE 1 = 1 [[AND orn.timestamp >= :start-date]]
  [[AND orn.timestamp <= :end-date]]
  AND ornt.timestamp_deleted IS NULL
  AND (orn.timestamp_deleted IS NULL OR orn.timestamp_deleted > COALESCE(:end-date, 9999999999))
GROUP BY ornt.tag
ORDER BY count DESC"
           params))
      (nodes-by-heading-level-per-month
       . ,(deterred-db-select-template-alist
           db
           "WITH RECURSIVE months(month) AS (
  SELECT strftime('%Y-%m', date(MIN(timestamp), 'unixepoch')) FROM org_roam_node
  [[WHERE timestamp >= :start-date]]
  UNION ALL
  SELECT strftime('%Y-%m', date(month || '-01', '+1 month')) FROM months
  WHERE month < (SELECT strftime('%Y-%m', date(MAX(COALESCE(:end-date, timestamp)), 'unixepoch')) FROM org_roam_node)
)
SELECT
  months.month,
  (SELECT count(*) FROM org_roam_node
   WHERE title LIKE '#%' AND title NOT LIKE '##%'
     AND date(timestamp, 'unixepoch') <= date(months.month || '-01', '+1 month', '-1 day')
     AND (timestamp_deleted IS NULL OR date(timestamp_deleted, 'unixepoch') > date(months.month || '-01', '+1 month', '-1 day'))) AS single_hash,
  (SELECT count(*) FROM org_roam_node
   WHERE title LIKE '##%'
     AND date(timestamp, 'unixepoch') <= date(months.month || '-01', '+1 month', '-1 day')
     AND (timestamp_deleted IS NULL OR date(timestamp_deleted, 'unixepoch') > date(months.month || '-01', '+1 month', '-1 day'))) AS double_hash,
  (SELECT count(*) FROM org_roam_node
   WHERE title NOT LIKE '#%'
     AND date(timestamp, 'unixepoch') <= date(months.month || '-01', '+1 month', '-1 day')
     AND (timestamp_deleted IS NULL OR date(timestamp_deleted, 'unixepoch') > date(months.month || '-01', '+1 month', '-1 day'))) AS no_hash
FROM months
ORDER BY month ASC"
           params))
      (total-nodes-by-heading-level
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  CASE
    WHEN title LIKE '#%' AND title NOT LIKE '##%' THEN 'Structure nodes (#)'
    WHEN title LIKE '##%' THEN 'Structure nodes (##)'
    ELSE 'Regular nodes'
  END AS heading_type,
  count(*) count
FROM org_roam_node
WHERE 1 = 1 [[AND timestamp >= :start-date]]
  [[AND timestamp <= :end-date]]
  AND (timestamp_deleted IS NULL OR timestamp_deleted > COALESCE(:end-date, 9999999999))
GROUP BY heading_type
ORDER BY count DESC"
           params))
      (numbers-data . ,numbers-data))))

(cl-defmethod deterred-dashboard-render-results ((_dashboard deterred-dashboard-org-roam)
                                                 params data)
  "Render DATA for the Org Roam dashboard."
  (let ((tag (alist-get :tag params)))
    (insert
     (deterred-format
      "Total of "
      (f-ace (f (f-num (f-acc "data->'numbers-data->'data[0]->'total_nodes")))
             'bold)
      " nodes"
      (when (alist-get :start-date params)
        (f " since " (f-ace (f (format-time-string "%Y-%m-%d" (alist-get :start-date params))) 'bold)))
      (when (alist-get :end-date params)
        (f " until " (f-ace (f (format-time-string "%Y-%m-%d" (alist-get :end-date params))) 'bold)))
      ", including "
      (f-ace (f (f-num (f-acc "data->'numbers-data->'data[0]->'structure_single")))
             'bold)
      " structure nodes with \"#\" and "
      (f-ace (f (f-num (f-acc "data->'numbers-data->'data[0]->'structure_double")))
             'bold)
      " structure nodes with \"##\". "
      (f-ace (f (f-num (f-acc "data->'numbers-data->'data[0]->'total_tags")))
             'bold)
      " unique tags.\n\n"
      (f-h2 "Nodes over time") "\n"))
    (deterred-dashboard-exec-python
     :python-code
     "from matplotlib import pyplot as plt
from matplotlib.ticker import MaxNLocator
from deterred import fig_to_b64

import pandas as pd
import json

data = json.loads(input())
df_per_day = pd.DataFrame(data['nodes-per-day']['data'])
df_created_per_month = pd.DataFrame(data['nodes-created-per-month']['data'])
df_created_per_year = pd.DataFrame(data['nodes-created-per-year']['data'])
df_modified_per_month = pd.DataFrame(data['nodes-modified-per-month']['data'])
df_modified_per_year = pd.DataFrame(data['nodes-modified-per-year']['data'])

images = []

fig, ax = plt.subplots(figsize=(8, 5))
df_per_day['day_dt'] = pd.to_datetime(df_per_day['day'])
ax.plot(df_per_day['day_dt'], df_per_day['count'])
ax.set_title('Total nodes per day')
ax.set_xlabel('Day')
ax.set_ylabel('Count')

# Add vertical lines for year boundaries
years = df_per_day['day_dt'].dt.year.unique()
for year in years[1:]:
    year_start = pd.Timestamp(f'{year}-01-01')
    if year_start >= df_per_day['day_dt'].min() and year_start <= df_per_day['day_dt'].max():
        ax.axvline(x=year_start, color='gray', linestyle='--', linewidth=0.8, alpha=0.7)

if len(df_per_day) > 30:
    ax.xaxis.set_major_locator(MaxNLocator(nbins=30))
plt.xticks(rotation=90, ha='right')
plt.tight_layout()
images.append(fig_to_b64(fig))

fig, ax = plt.subplots(figsize=(8, 5))
df_created_per_month.plot(ax=ax, kind='bar', x='month', y='count', legend=False)
ax.set_title('Nodes created per month')
ax.set_xlabel('Month')
ax.set_ylabel('Count')
if len(df_created_per_month) > 30:
    ax.xaxis.set_major_locator(MaxNLocator(nbins=40))
images.append(fig_to_b64(fig))

fig, ax = plt.subplots(figsize=(8, 5))
df_created_per_year.plot(ax=ax, kind='bar', x='year', y='count', legend=False)
ax.set_title('Nodes created per year')
ax.set_xlabel('Year')
ax.set_ylabel('Count')
for container in ax.containers:
    ax.bar_label(container, fmt='%d')
images.append(fig_to_b64(fig))

fig, ax = plt.subplots(figsize=(8, 5))
df_modified_per_month.plot(ax=ax, kind='bar', x='month', y='count', legend=False)
ax.set_title('Nodes modified per month')
ax.set_xlabel('Month')
ax.set_ylabel('Count')
if len(df_modified_per_month) > 30:
    ax.xaxis.set_major_locator(MaxNLocator(nbins=40))
images.append(fig_to_b64(fig))

fig, ax = plt.subplots(figsize=(8, 5))
df_modified_per_year.plot(ax=ax, kind='bar', x='year', y='count', legend=False)
ax.set_title('Nodes modified per year')
ax.set_xlabel('Year')
ax.set_ylabel('Count')
for container in ax.containers:
    ax.bar_label(container, fmt='%d')
images.append(fig_to_b64(fig))

print(json.dumps(images))"
     :input data
     :on-success
     (lambda (images)
       (insert (deterred-format (f-h3 "Total nodes per day") "\n"))
       (deterred-dashboard-print-images-base64 (elt images 0))
       (insert "\n")
       (insert (deterred-format (f-h3 "Nodes created per month") "\n"))
       (deterred-dashboard-print-images-base64 (elt images 1))
       (insert "\n")
       (insert (deterred-format (f-h3 "Nodes created per year") "\n"))
       (deterred-dashboard-print-images-base64 (elt images 2))
       (insert "\n")
       (insert (deterred-format (f-h3 "Nodes modified per month") "\n"))
       (deterred-dashboard-print-images-base64 (elt images 3))
       (insert "\n")
       (insert (deterred-format (f-h3 "Nodes modified per year") "\n"))
       (deterred-dashboard-print-images-base64 (elt images 4))
       (insert "\n")))
    (when tag
      (insert (deterred-format (f-h2 "Tags") "\n"))
      (deterred-dashboard-exec-python
       :python-code
       (format "from matplotlib import pyplot as plt
from matplotlib.ticker import MaxNLocator
from deterred import fig_to_b64

import pandas as pd
import json

data = json.loads(input())
df = pd.DataFrame(data['nodes-by-tag-per-month']['data'])

images = []

fig, ax = plt.subplots(figsize=(8, 5))
df.plot(ax=ax, kind='bar', x='month', stacked=True)
ax.set_title('Total nodes with tag \"%s\" vs untagged per month')
ax.set_xlabel('Month')
ax.set_ylabel('Count')
if len(df) > 30:
    ax.xaxis.set_major_locator(MaxNLocator(nbins=40))
images.append(fig_to_b64(fig))

print(json.dumps(images))" tag)
       :input data
       :on-success
       (lambda (images)
         (insert (deterred-format (f-h3 (format "Total nodes with tag '%s' vs untagged per month" tag)) "\n"))
         (deterred-dashboard-print-images-base64 (elt images 0))
         (insert "\n"))))
    (insert
     (deterred-format (f-h2 "Tag statistics") "\n"
                      (f-h3 "Total nodes by tag") "\n")
     (deterred-grid-print-with-org
      (alist-get 'data (alist-get 'total-nodes-by-tag data))
      :column-names '((tag . "Tag") (count . "Count"))
      :max-rows 20
      :grid-button t)
     "\n")
    (insert (deterred-format (f-h2 "Structure nodes") "\n"))
    (deterred-dashboard-exec-python
     :python-code
     "from matplotlib import pyplot as plt
from matplotlib.ticker import MaxNLocator
from deterred import fig_to_b64

import pandas as pd
import json

data = json.loads(input())
df = pd.DataFrame(data['nodes-by-heading-level-per-month']['data'])

# Calculate percentages
df['total'] = df['single_hash'] + df['double_hash'] + df['no_hash']
df['single_hash_pct'] = (df['single_hash'] / df['total']) * 100
df['double_hash_pct'] = (df['double_hash'] / df['total']) * 100

images = []

fig, ax = plt.subplots(figsize=(8, 5))
x = range(len(df))
ax.bar(x, df['single_hash_pct'], label='Structure nodes (#)')
ax.bar(x, df['double_hash_pct'], bottom=df['single_hash_pct'], label='Structure nodes (##)')
ax.set_xticks(x)
ax.set_xticklabels(df['month'])
ax.set_title('Percentage of structure nodes per month')
ax.set_xlabel('Month')
ax.set_ylabel('Percentage (%)')
ax.legend()
if len(df) > 30:
    ax.xaxis.set_major_locator(MaxNLocator(nbins=40))

plt.xticks(rotation=90, ha='right')

images.append(fig_to_b64(fig))

print(json.dumps(images))"
     :input data
     :on-success
     (lambda (images)
       (insert (deterred-format (f-h3 "Percentage of structure nodes per month") "\n"))
       (deterred-dashboard-print-images-base64 (elt images 0))
       (insert "\n")))
    (insert
     (deterred-format (f-h3 "Total nodes by type") "\n")
     (deterred-grid-print-with-org
      (alist-get 'data (alist-get 'total-nodes-by-heading-level data))
      :column-names '((heading_type . "Type") (count . "Count"))
      :grid-button t)
     "\n")))

(provide 'deterred-dashboard-org-roam)
;;; deterred-dashboard-org-roam.el ends here

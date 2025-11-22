;;; deterred-dashboard-org-journal-tags.el --- DETERRED dashboard for Org Journal Tags -*- lexical-binding: t -*-

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

;; A dashboard for Org Journal Tags, corresponding to `deterred-org-journal-tags'.

;;; Code:
(require 'deterred-dashboard)
(require 'deterred-db)
(require 'deterred-utils)
(require 'deterred-locations)

(defclass deterred-dashboard-org-journal-tags (deterred-dashboard)
  ((name :initform "Org Journal Tags"))
  "A DETERRED dashboard for Org Journal Tags.")

(cl-defmethod deterred-dashboard-default-params ((_dashboard deterred-dashboard-org-journal-tags))
  "Default parameters for the Org Journal Tags dashboard."
  '((:start-date)
    (:end-date)
    (:tags)
    (:n-top-tags . 10)))

(cl-defmethod deterred-dashboard-render-params ((_dashboard deterred-dashboard-org-journal-tags))
  "Render the parameters section for the Org Journal Tags dashboard."
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
         (tags
          (mapcar
           (lambda (item) (alist-get 'name item))
           (deterred-db-select-alist
            db "SELECT DISTINCT name FROM org_journal_tag ORDER BY name"))))
    (deterred-dashboard-widget-completing-read-multiple
     :name "Tags"
     :key :tags
     :options tags))
  (insert "\n")
  (deterred-dashboard-widget-number
   :name "Top N tags"
   :key :n-top-tags)
  (insert "\n"))

(cl-defmethod deterred-dashboard-list-datasets ((_dashboard deterred-dashboard-org-journal-tags))
  "List datasets for the Org Journal Tags dashboard."
  '((records-per-month (name . "Records per month"))
    (records-per-year (name . "Records per year"))
    (top-tags (name . "Top tags"))
    (top-tags-per-month (name . "Time series of top N tags per month"))
    (average-size-per-month (name . "Average record size per month"))
    (tag-discovery-per-year (name . "Tags discovered per year"))
    (records-per-day-of-week (name . "Records per day of week"))
    (records-per-hour (name . "Records per hour of day (local time)"))
    (records-per-location (name . "Records per location"))
    (records-per-location-per-month (name . "Records per location per month"))
    (records-per-host (name . "Records per host"))
    (records-per-host-per-month (name . "Records per host per month"))
    (top-contacts (name . "Top contacts"))
    (top-contacts-per-month (name . "Time series of top N contacts per month"))
    (days-without-records-per-month (name . "Days without records per month"))
    (top-months (name . "Top months by record count"))
    (top-days (name . "Top days by record count"))
    (numbers-data (name . "Numerical data"))))

(cl-defmethod deterred-dashboard-fetch-datasets ((_dashboard deterred-dashboard-org-journal-tags)
                                                 params)
  "Fetch datasets for the Org Journal Tags dashboard.

PARAMS is as returned by `deterred-dashboard-default-params'."
  (let* ((db (deterred-db--init))
         (numbers-data
          (deterred-db-select-template-alist
           db
           "SELECT (
  SELECT COUNT(*)
  FROM org_journal_record
  WHERE 1 = 1 [[AND timestamp >= :start-date]]
    [[AND timestamp <= :end-date]]
    [[AND id IN (
      SELECT record_id FROM org_journal_record_tag
      INNER JOIN org_journal_tag ON org_journal_tag.id = org_journal_record_tag.tag_id
      WHERE org_journal_tag.name IN :tags
    )]]
) AS total_records,
(
  SELECT COUNT(DISTINCT tag_id)
  FROM org_journal_record_tag
  WHERE record_id IN (
    SELECT id FROM org_journal_record
    WHERE 1 = 1 [[AND timestamp >= :start-date]]
      [[AND timestamp <= :end-date]]
  )
  [[AND tag_id IN (
    SELECT id FROM org_journal_tag WHERE name IN :tags
  )]]
) AS total_tags,
(
  SELECT COUNT(DISTINCT date(timestamp, 'unixepoch'))
  FROM org_journal_record
  WHERE 1 = 1 [[AND timestamp >= :start-date]]
    [[AND timestamp <= :end-date]]
    [[AND id IN (
      SELECT record_id FROM org_journal_record_tag
      INNER JOIN org_journal_tag ON org_journal_tag.id = org_journal_record_tag.tag_id
      WHERE org_journal_tag.name IN :tags
    )]]
) AS unique_days,
(
  SELECT CAST(AVG(size) AS INTEGER)
  FROM org_journal_record
  WHERE 1 = 1 [[AND timestamp >= :start-date]]
    [[AND timestamp <= :end-date]]
    [[AND id IN (
      SELECT record_id FROM org_journal_record_tag
      INNER JOIN org_journal_tag ON org_journal_tag.id = org_journal_record_tag.tag_id
      WHERE org_journal_tag.name IN :tags
    )]]
) AS average_size;"
           params)))
    `((records-per-month
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y-%m', timestamp, 'unixepoch') month,
  COUNT(*) count
FROM org_journal_record
WHERE 1 = 1 [[AND timestamp >= :start-date]]
  [[AND timestamp <= :end-date]]
  [[AND id IN (
    SELECT record_id FROM org_journal_record_tag
    INNER JOIN org_journal_tag ON org_journal_tag.id = org_journal_record_tag.tag_id
    WHERE org_journal_tag.name IN :tags
  )]]
GROUP BY strftime('%Y-%m', timestamp, 'unixepoch')
ORDER BY month ASC"
           params))
      (records-per-year
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y', timestamp, 'unixepoch') year,
  COUNT(*) count
FROM org_journal_record
WHERE 1 = 1 [[AND timestamp >= :start-date]]
  [[AND timestamp <= :end-date]]
  [[AND id IN (
    SELECT record_id FROM org_journal_record_tag
    INNER JOIN org_journal_tag ON org_journal_tag.id = org_journal_record_tag.tag_id
    WHERE org_journal_tag.name IN :tags
  )]]
GROUP BY strftime('%Y', timestamp, 'unixepoch')
ORDER BY year ASC"
           params))
      (top-tags
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  org_journal_tag.name tag_name,
  COUNT(*) count
FROM org_journal_record_tag
INNER JOIN org_journal_tag ON org_journal_tag.id = org_journal_record_tag.tag_id
INNER JOIN org_journal_record ON org_journal_record.id = org_journal_record_tag.record_id
WHERE 1 = 1 [[AND org_journal_record.timestamp >= :start-date]]
  [[AND org_journal_record.timestamp <= :end-date]]
  [[AND org_journal_tag.name IN :tags]]
GROUP BY org_journal_tag.name
ORDER BY count DESC
LIMIT 50"
           params))
      (top-tags-per-month
       . ,(deterred-db-select-template-alist
           db
           "WITH top_tags AS (
  SELECT
    org_journal_tag.name,
    COUNT(*) total
  FROM org_journal_record_tag
  INNER JOIN org_journal_tag ON org_journal_tag.id = org_journal_record_tag.tag_id
  INNER JOIN org_journal_record ON org_journal_record.id = org_journal_record_tag.record_id
  WHERE 1 = 1 [[AND org_journal_record.timestamp >= :start-date]]
    [[AND org_journal_record.timestamp <= :end-date]]
    [[AND org_journal_tag.name IN :tags]]
  GROUP BY org_journal_tag.name
  ORDER BY total DESC
  LIMIT :n-top-tags
)
SELECT
  strftime('%Y-%m', org_journal_record.timestamp, 'unixepoch') month,
  org_journal_tag.name tag_name,
  COUNT(*) count
FROM org_journal_record_tag
INNER JOIN org_journal_tag ON org_journal_tag.id = org_journal_record_tag.tag_id
INNER JOIN org_journal_record ON org_journal_record.id = org_journal_record_tag.record_id
INNER JOIN top_tags tt ON tt.name = org_journal_tag.name
WHERE 1 = 1 [[AND org_journal_record.timestamp >= :start-date]]
  [[AND org_journal_record.timestamp <= :end-date]]
  [[AND org_journal_tag.name IN :tags]]
GROUP BY strftime('%Y-%m', org_journal_record.timestamp, 'unixepoch'), org_journal_tag.name
ORDER BY month ASC"
           params))
      (average-size-per-month
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y-%m', timestamp, 'unixepoch') month,
  CAST(AVG(size) AS INTEGER) average_size
FROM org_journal_record
WHERE 1 = 1 [[AND timestamp >= :start-date]]
  [[AND timestamp <= :end-date]]
  [[AND id IN (
    SELECT record_id FROM org_journal_record_tag
    INNER JOIN org_journal_tag ON org_journal_tag.id = org_journal_record_tag.tag_id
    WHERE org_journal_tag.name IN :tags
  )]]
GROUP BY strftime('%Y-%m', timestamp, 'unixepoch')
ORDER BY month ASC"
           params))
      (tag-discovery-per-year
       . ,(deterred-db-select-template-alist
           db
           "WITH tag_first_use AS (
  SELECT
    org_journal_tag.name,
    strftime('%Y', MIN(org_journal_record.timestamp), 'unixepoch') year_discovered
  FROM org_journal_record_tag
  INNER JOIN org_journal_tag ON org_journal_tag.id = org_journal_record_tag.tag_id
  INNER JOIN org_journal_record ON org_journal_record.id = org_journal_record_tag.record_id
  WHERE 1 = 1 [[AND org_journal_record.timestamp >= :start-date]]
    [[AND org_journal_record.timestamp <= :end-date]]
    [[AND org_journal_tag.name IN :tags]]
  GROUP BY org_journal_tag.name
)
SELECT year_discovered, COUNT(*) new_tags
FROM tag_first_use
GROUP BY year_discovered
ORDER BY year_discovered ASC"
           params))
      (records-per-day-of-week
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  CASE CAST(strftime('%w', timestamp, 'unixepoch') AS INTEGER)
    WHEN 0 THEN 'Sunday'
    WHEN 1 THEN 'Monday'
    WHEN 2 THEN 'Tuesday'
    WHEN 3 THEN 'Wednesday'
    WHEN 4 THEN 'Thursday'
    WHEN 5 THEN 'Friday'
    WHEN 6 THEN 'Saturday'
  END day_of_week,
  CAST(strftime('%w', timestamp, 'unixepoch') AS INTEGER) day_num,
  COUNT(*) count
FROM org_journal_record
WHERE 1 = 1 [[AND timestamp >= :start-date]]
  [[AND timestamp <= :end-date]]
  [[AND id IN (
    SELECT record_id FROM org_journal_record_tag
    INNER JOIN org_journal_tag ON org_journal_tag.id = org_journal_record_tag.tag_id
    WHERE org_journal_tag.name IN :tags
  )]]
GROUP BY CAST(strftime('%w', timestamp, 'unixepoch') AS INTEGER)
ORDER BY day_num ASC"
           params))
      (records-per-hour
       . ,(let* ((timestamps
                  (deterred-db-select-template-alist
                   db
                   "SELECT timestamp
FROM org_journal_record
WHERE 1 = 1 [[AND timestamp >= :start-date]]
  [[AND timestamp <= :end-date]]
  [[AND id IN (
    SELECT record_id FROM org_journal_record_tag
    INNER JOIN org_journal_tag ON org_journal_tag.id = org_journal_record_tag.tag_id
    WHERE org_journal_tag.name IN :tags
  )]]"
                   params))
                 (hours (make-vector 24 0)))
            (dolist (ts-alist timestamps)
              (let* ((timestamp (alist-get 'timestamp ts-alist))
                     (offset (deterred-locations-offset-at timestamp nil db))
                     (local-timestamp (+ timestamp offset))
                     (hour (string-to-number
                            (format-time-string "%H" local-timestamp t))))
                (aset hours hour (1+ (aref hours hour)))))
            (cl-loop for hour from 0 to 23
                     collect `((hour . ,hour)
                               (count . ,(aref hours hour))))))
      (records-per-location
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  SUBSTR(org_journal_tag.name, 5) location,
  COUNT(*) count
FROM org_journal_record_tag
INNER JOIN org_journal_tag ON org_journal_tag.id = org_journal_record_tag.tag_id
INNER JOIN org_journal_record ON org_journal_record.id = org_journal_record_tag.record_id
WHERE org_journal_tag.name LIKE 'loc.%'
  [[AND org_journal_record.timestamp >= :start-date]]
  [[AND org_journal_record.timestamp <= :end-date]]
  [[AND org_journal_record.id IN (
    SELECT record_id FROM org_journal_record_tag rt2
    INNER JOIN org_journal_tag t2 ON t2.id = rt2.tag_id
    WHERE t2.name IN :tags
  )]]
GROUP BY org_journal_tag.name
ORDER BY count DESC"
           params))
      (records-per-location-per-month
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y-%m', org_journal_record.timestamp, 'unixepoch') month,
  SUBSTR(org_journal_tag.name, 5) location,
  COUNT(*) count
FROM org_journal_record_tag
INNER JOIN org_journal_tag ON org_journal_tag.id = org_journal_record_tag.tag_id
INNER JOIN org_journal_record ON org_journal_record.id = org_journal_record_tag.record_id
WHERE org_journal_tag.name LIKE 'loc.%'
  [[AND org_journal_record.timestamp >= :start-date]]
  [[AND org_journal_record.timestamp <= :end-date]]
  [[AND org_journal_record.id IN (
    SELECT record_id FROM org_journal_record_tag rt2
    INNER JOIN org_journal_tag t2 ON t2.id = rt2.tag_id
    WHERE t2.name IN :tags
  )]]
GROUP BY strftime('%Y-%m', org_journal_record.timestamp, 'unixepoch'), org_journal_tag.name
ORDER BY month ASC"
           params))
      (records-per-host
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  SUBSTR(org_journal_tag.name, 6) host,
  COUNT(*) count
FROM org_journal_record_tag
INNER JOIN org_journal_tag ON org_journal_tag.id = org_journal_record_tag.tag_id
INNER JOIN org_journal_record ON org_journal_record.id = org_journal_record_tag.record_id
WHERE org_journal_tag.name LIKE 'host.%'
  [[AND org_journal_record.timestamp >= :start-date]]
  [[AND org_journal_record.timestamp <= :end-date]]
  [[AND org_journal_record.id IN (
    SELECT record_id FROM org_journal_record_tag rt2
    INNER JOIN org_journal_tag t2 ON t2.id = rt2.tag_id
    WHERE t2.name IN :tags
  )]]
GROUP BY org_journal_tag.name
ORDER BY count DESC"
           params))
      (records-per-host-per-month
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y-%m', org_journal_record.timestamp, 'unixepoch') month,
  SUBSTR(org_journal_tag.name, 6) host,
  COUNT(*) count
FROM org_journal_record_tag
INNER JOIN org_journal_tag ON org_journal_tag.id = org_journal_record_tag.tag_id
INNER JOIN org_journal_record ON org_journal_record.id = org_journal_record_tag.record_id
WHERE org_journal_tag.name LIKE 'host.%'
  [[AND org_journal_record.timestamp >= :start-date]]
  [[AND org_journal_record.timestamp <= :end-date]]
  [[AND org_journal_record.id IN (
    SELECT record_id FROM org_journal_record_tag rt2
    INNER JOIN org_journal_tag t2 ON t2.id = rt2.tag_id
    WHERE t2.name IN :tags
  )]]
GROUP BY strftime('%Y-%m', org_journal_record.timestamp, 'unixepoch'), org_journal_tag.name
ORDER BY month ASC"
           params))
      (top-contacts
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  SUBSTR(org_journal_tag.name, 9) contact,
  COUNT(*) count
FROM org_journal_record_tag
INNER JOIN org_journal_tag ON org_journal_tag.id = org_journal_record_tag.tag_id
INNER JOIN org_journal_record ON org_journal_record.id = org_journal_record_tag.record_id
WHERE org_journal_tag.name LIKE 'contact:%'
  [[AND org_journal_record.timestamp >= :start-date]]
  [[AND org_journal_record.timestamp <= :end-date]]
  [[AND org_journal_record.id IN (
    SELECT record_id FROM org_journal_record_tag rt2
    INNER JOIN org_journal_tag t2 ON t2.id = rt2.tag_id
    WHERE t2.name IN :tags
  )]]
GROUP BY org_journal_tag.name
ORDER BY count DESC
LIMIT 50"
           params))
      (top-contacts-per-month
       . ,(deterred-db-select-template-alist
           db
           "WITH top_contacts AS (
  SELECT
    org_journal_tag.name,
    COUNT(*) total
  FROM org_journal_record_tag
  INNER JOIN org_journal_tag ON org_journal_tag.id = org_journal_record_tag.tag_id
  INNER JOIN org_journal_record ON org_journal_record.id = org_journal_record_tag.record_id
  WHERE org_journal_tag.name LIKE 'contact:%'
    [[AND org_journal_record.timestamp >= :start-date]]
    [[AND org_journal_record.timestamp <= :end-date]]
    [[AND org_journal_record.id IN (
      SELECT record_id FROM org_journal_record_tag rt2
      INNER JOIN org_journal_tag t2 ON t2.id = rt2.tag_id
      WHERE t2.name IN :tags
    )]]
  GROUP BY org_journal_tag.name
  ORDER BY total DESC
  LIMIT :n-top-tags
)
SELECT
  strftime('%Y-%m', org_journal_record.timestamp, 'unixepoch') month,
  SUBSTR(org_journal_tag.name, 9) contact,
  COUNT(*) count
FROM org_journal_record_tag
INNER JOIN org_journal_tag ON org_journal_tag.id = org_journal_record_tag.tag_id
INNER JOIN org_journal_record ON org_journal_record.id = org_journal_record_tag.record_id
INNER JOIN top_contacts tc ON tc.name = org_journal_tag.name
WHERE org_journal_tag.name LIKE 'contact.%'
  [[AND org_journal_record.timestamp >= :start-date]]
  [[AND org_journal_record.timestamp <= :end-date]]
  [[AND org_journal_record.id IN (
    SELECT record_id FROM org_journal_record_tag rt2
    INNER JOIN org_journal_tag t2 ON t2.id = rt2.tag_id
    WHERE t2.name IN :tags
  )]]
GROUP BY strftime('%Y-%m', org_journal_record.timestamp, 'unixepoch'), org_journal_tag.name
ORDER BY month ASC"
           params))
      (days-without-records-per-month
       . ,(deterred-db-select-template-alist
           db
           "WITH filtered_records AS (
  SELECT timestamp
  FROM org_journal_record
  WHERE 1 = 1 [[AND timestamp >= :start-date]]
    [[AND timestamp <= :end-date]]
    [[AND id IN (
      SELECT record_id FROM org_journal_record_tag
      INNER JOIN org_journal_tag ON org_journal_tag.id = org_journal_record_tag.tag_id
      WHERE org_journal_tag.name IN :tags
    )]]
)
SELECT
  strftime('%Y-%m', timestamp, 'unixepoch') month,
  COUNT(DISTINCT date(timestamp, 'unixepoch')) days_with_records,
  (julianday(MIN(
    date(strftime('%Y-%m', timestamp, 'unixepoch') || '-01', '+1 month', '-1 day'),
    date(COALESCE(:end-date, (SELECT MAX(timestamp) FROM filtered_records)), 'unixepoch')
  )) - julianday(MAX(
    date(strftime('%Y-%m', timestamp, 'unixepoch') || '-01'),
    date(COALESCE(:start-date, (SELECT MIN(timestamp) FROM filtered_records)), 'unixepoch')
  )) + 1) - COUNT(DISTINCT date(timestamp, 'unixepoch')) days_without_records,
  CAST(julianday(MIN(
    date(strftime('%Y-%m', timestamp, 'unixepoch') || '-01', '+1 month', '-1 day'),
    date(COALESCE(:end-date, (SELECT MAX(timestamp) FROM filtered_records)), 'unixepoch')
  )) - julianday(MAX(
    date(strftime('%Y-%m', timestamp, 'unixepoch') || '-01'),
    date(COALESCE(:start-date, (SELECT MIN(timestamp) FROM filtered_records)), 'unixepoch')
  )) + 1 AS INTEGER) total_days
FROM filtered_records
GROUP BY strftime('%Y-%m', timestamp, 'unixepoch')
ORDER BY month ASC"
           params))
      (top-months
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y-%m', timestamp, 'unixepoch') month,
  COUNT(*) count
FROM org_journal_record
WHERE 1 = 1 [[AND timestamp >= :start-date]]
  [[AND timestamp <= :end-date]]
  [[AND id IN (
    SELECT record_id FROM org_journal_record_tag
    INNER JOIN org_journal_tag ON org_journal_tag.id = org_journal_record_tag.tag_id
    WHERE org_journal_tag.name IN :tags
  )]]
GROUP BY strftime('%Y-%m', timestamp, 'unixepoch')
ORDER BY count DESC
LIMIT 20"
           params))
      (top-days
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  date(timestamp, 'unixepoch') day,
  COUNT(*) count
FROM org_journal_record
WHERE 1 = 1 [[AND timestamp >= :start-date]]
  [[AND timestamp <= :end-date]]
  [[AND id IN (
    SELECT record_id FROM org_journal_record_tag
    INNER JOIN org_journal_tag ON org_journal_tag.id = org_journal_record_tag.tag_id
    WHERE org_journal_tag.name IN :tags
  )]]
GROUP BY date(timestamp, 'unixepoch')
ORDER BY count DESC
LIMIT 20"
           params))
      (numbers-data . ,numbers-data))))

(cl-defmethod deterred-dashboard-render-results ((_dashboard deterred-dashboard-org-journal-tags)
                                                 _params data)
  "Render DATA for the Org Journal Tags dashboard."
  (insert
   (deterred-format
    "I've written "
    (f-ace (f (f-num (f-acc "data->'numbers-data->'data[0]->'total_records")))
           'bold)
    " journal records using "
    (f-ace (f (f-num (f-acc "data->'numbers-data->'data[0]->'total_tags")))
           'bold)
    " unique tags over "
    (f-ace (f (f-num (f-acc "data->'numbers-data->'data[0]->'unique_days")))
           'bold)
    " unique days. The average record size is "
    (f-ace (f (f-num (f-acc "data->'numbers-data->'data[0]->'average_size")))
           'bold)
    " characters.\n\n"
    (f-h2 "Top tags") "\n")
   (deterred-grid-print-with-org
    (alist-get 'data (alist-get 'top-tags data))
    :column-names '((tag_name . "Tag") (count . "Count"))
    :max-rows 20
    :grid-button t)
   "\n"
   (deterred-format (f-h2 "Records over time") "\n"))
  (deterred-dashboard-exec-python
   :python-code
   "from matplotlib import pyplot as plt
from matplotlib.ticker import MaxNLocator
from deterred import fig_to_b64

import pandas as pd

import json

data = json.loads(input())
df_y = pd.DataFrame(data['records-per-year']['data'])
df_m = pd.DataFrame(data['records-per-month']['data'])
df_tags = pd.DataFrame(data['top-tags-per-month']['data'])
df_tags_p = df_tags.pivot(index='month', columns='tag_name', values='count').fillna(0)
df_size = pd.DataFrame(data['average-size-per-month']['data'])
df_days_without = pd.DataFrame(data['days-without-records-per-month']['data'])

images = []

fig, ax = plt.subplots(figsize=(8, 5))
df_y.plot(ax=ax, kind='bar', x='year', y='count', legend=False)
ax.set_title('Records per year')
ax.set_xlabel('Year')
ax.set_ylabel('Count')
for container in ax.containers:
    ax.bar_label(container, fmt='%.0f')
images.append(fig_to_b64(fig))

fig, ax = plt.subplots(figsize=(8, 5))
df_m.plot(ax=ax, kind='line', x='month', y='count', legend=False)
ax.set_title('Records per month')
ax.set_xlabel('Month')
ax.set_ylabel('Count')
if len(df_m) > 30:
    ax.xaxis.set_major_locator(MaxNLocator(nbins=40))
images.append(fig_to_b64(fig))

fig, ax = plt.subplots(figsize=(8, 5))
df_tags_p.plot(ax=ax, kind='line')
ax.set_title('Top N tags per month')
ax.set_xlabel('Month')
ax.set_ylabel('Count')
images.append(fig_to_b64(fig))

fig, ax = plt.subplots(figsize=(8, 5))
df_size.plot(ax=ax, kind='line', x='month', y='average_size', legend=False)
ax.set_title('Average record size per month')
ax.set_xlabel('Month')
ax.set_ylabel('Characters')
images.append(fig_to_b64(fig))

if len(df_days_without) > 0:
    fig, ax = plt.subplots(figsize=(8, 5))
    df_days_without.plot(ax=ax, kind='bar', x='month', y=['days_with_records', 'days_without_records'], stacked=True)
    ax.set_title('Days with and without records per month')
    ax.set_xlabel('Month')
    ax.set_ylabel('Days')
    ax.legend(['Days with records', 'Days without records'])
    images.append(fig_to_b64(fig))
else:
    images.append(None)

print(json.dumps(images))"
   :input data
   :on-success
   (lambda (images)
     (insert (deterred-format (f-h3 "Records per year") "\n"))
     (deterred-dashboard-print-images-base64 (elt images 0))
     (insert "\n")
     (insert (deterred-format (f-h3 "Records per month") "\n"))
     (deterred-dashboard-print-images-base64 (elt images 1))
     (insert "\n")
     (insert (deterred-format (f-h3 "Top N tags per month") "\n"))
     (deterred-dashboard-print-images-base64 (elt images 2))
     (insert "\n")
     (insert (deterred-format (f-h3 "Average record size per month") "\n"))
     (deterred-dashboard-print-images-base64 (elt images 3))
     (insert "\n")
     (when (elt images 4)
       (insert (deterred-format (f-h3 "Days with and without records per month") "\n"))
       (deterred-dashboard-print-images-base64 (elt images 4))
       (insert "\n"))))
  (insert (deterred-format (f-h2 "Tag discovery") "\n"))
  (deterred-dashboard-exec-python
   :python-code
   "from matplotlib import pyplot as plt
from deterred import fig_to_b64

import pandas as pd

import json

data = json.loads(input())
df_discovery = pd.DataFrame(data['tag-discovery-per-year']['data'])

images = []

if len(df_discovery) > 0:
    fig, ax = plt.subplots(figsize=(8, 5))
    df_discovery.plot(ax=ax, kind='bar', x='year_discovered', y='new_tags', legend=False)
    ax.set_title('Tags discovered per year')
    ax.set_xlabel('Year')
    ax.set_ylabel('New tags')
    for container in ax.containers:
        ax.bar_label(container, fmt='%.0f')
    images.append(fig_to_b64(fig))
else:
    images.append(None)

print(json.dumps(images))"
   :input data
   :on-success
   (lambda (images)
     (when (elt images 0)
       (insert (deterred-format (f-h3 "Tags discovered per year") "\n"))
       (deterred-dashboard-print-images-base64 (elt images 0))
       (insert "\n"))))
  (insert
   (deterred-format (f-h2 "Location and host patterns") "\n"
                    (f-h3 "Records per location") "\n")
   (deterred-grid-print-with-org
    (alist-get 'data (alist-get 'records-per-location data))
    :column-names '((location . "Location") (count . "Count"))
    :grid-button t)
   "\n"
   (deterred-format (f-h3 "Records per host") "\n")
   (deterred-grid-print-with-org
    (alist-get 'data (alist-get 'records-per-host data))
    :column-names '((host . "Host") (count . "Count"))
    :grid-button t)
   "\n")
  (deterred-dashboard-exec-python
   :python-code
   "from matplotlib import pyplot as plt
from deterred import fig_to_b64

import pandas as pd

import json

data = json.loads(input())
df_loc = pd.DataFrame(data['records-per-location-per-month']['data'])
df_host = pd.DataFrame(data['records-per-host-per-month']['data'])

images = []

if len(df_loc) > 0:
    df_loc_p = df_loc.pivot(index='month', columns='location', values='count').fillna(0)
    fig, ax = plt.subplots(figsize=(8, 5))
    df_loc_p.plot(ax=ax, kind='bar', stacked=True)
    ax.set_title('Records per location per month')
    ax.set_xlabel('Month')
    ax.set_ylabel('Count')
    images.append(fig_to_b64(fig))
else:
    images.append(None)

if len(df_host) > 0:
    df_host_p = df_host.pivot(index='month', columns='host', values='count').fillna(0)
    fig, ax = plt.subplots(figsize=(8, 5))
    df_host_p.plot(ax=ax, kind='bar', stacked=True)
    ax.set_title('Records per host per month')
    ax.set_xlabel('Month')
    ax.set_ylabel('Count')
    images.append(fig_to_b64(fig))
else:
    images.append(None)

print(json.dumps(images))"
   :input data
   :on-success
   (lambda (images)
     (when (elt images 0)
       (insert (deterred-format (f-h3 "Records per location per month") "\n"))
       (deterred-dashboard-print-images-base64 (elt images 0))
       (insert "\n"))
     (when (elt images 1)
       (insert (deterred-format (f-h3 "Records per host per month") "\n"))
       (deterred-dashboard-print-images-base64 (elt images 1))
       (insert "\n"))))
  (insert
   (deterred-format (f-h2 "Contact patterns") "\n"
                    (f-h3 "Top contacts") "\n")
   (deterred-grid-print-with-org
    (alist-get 'data (alist-get 'top-contacts data))
    :column-names '((contact . "Contact") (count . "Count"))
    :max-rows 20
    :grid-button t)
   "\n")
  (deterred-dashboard-exec-python
   :python-code
   "from matplotlib import pyplot as plt
from deterred import fig_to_b64

import pandas as pd

import json

data = json.loads(input())
df_contacts = pd.DataFrame(data['top-contacts-per-month']['data'])

images = []

if len(df_contacts) > 0:
    df_contacts_p = df_contacts.pivot(index='month', columns='contact', values='count').fillna(0)
    fig, ax = plt.subplots(figsize=(8, 5))
    df_contacts_p.plot(ax=ax, kind='line')
    ax.set_title('Top N contacts per month')
    ax.set_xlabel('Month')
    ax.set_ylabel('Count')
    images.append(fig_to_b64(fig))
else:
    images.append(None)

print(json.dumps(images))"
   :input data
   :on-success
   (lambda (images)
     (when (elt images 0)
       (insert (deterred-format (f-h3 "Top N contacts per month") "\n"))
       (deterred-dashboard-print-images-base64 (elt images 0))
       (insert "\n"))))
  (insert (deterred-format (f-h2 "Writing patterns") "\n"))
  (deterred-dashboard-exec-python
   :python-code
   "from matplotlib import pyplot as plt
from deterred import fig_to_b64

import pandas as pd

import json

data = json.loads(input())
df_dow = pd.DataFrame(data['records-per-day-of-week']['data'])
df_hour = pd.DataFrame(data['records-per-hour']['data'])

images = []

fig, ax = plt.subplots(figsize=(8, 5))
df_dow.plot(ax=ax, kind='bar', x='day_of_week', y='count', legend=False)
ax.set_title('Records per day of week')
ax.set_xlabel('Day of week')
ax.set_ylabel('Count')
for container in ax.containers:
    ax.bar_label(container, fmt='%.0f')
images.append(fig_to_b64(fig))

fig, ax = plt.subplots(figsize=(8, 5))
df_hour.plot(ax=ax, kind='bar', x='hour', y='count', legend=False)
ax.set_title('Records per hour of day')
ax.set_xlabel('Hour')
ax.set_ylabel('Count')
for container in ax.containers:
    ax.bar_label(container, fmt='%.0f')
images.append(fig_to_b64(fig))

print(json.dumps(images))"
   :input data
   :on-success
   (lambda (images)
     (insert (deterred-format (f-h3 "Records per day of week") "\n"))
     (deterred-dashboard-print-images-base64 (elt images 0))
     (insert "\n")
     (insert (deterred-format (f-h3 "Records per hour of day") "\n"))
     (deterred-dashboard-print-images-base64 (elt images 1))
     (insert "\n")))
  (insert
   (deterred-format (f-h2 "Top periods") "\n"
                    (f-h3 "Top months") "\n")
   (deterred-grid-print-with-org
    (alist-get 'data (alist-get 'top-months data))
    :column-names '((month . "Month") (count . "Count"))
    :max-rows 10
    :grid-button t)
   "\n"
   (deterred-format (f-h3 "Top days") "\n")
   (deterred-grid-print-with-org
    (alist-get 'data (alist-get 'top-days data))
    :column-names '((day . "Day") (count . "Count"))
    :max-rows 10
    :grid-button t)
   "\n"))

(provide 'deterred-dashboard-org-journal-tags)
;;; deterred-dashboard-org-journal-tags.el ends here

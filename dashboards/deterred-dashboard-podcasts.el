;;; deterred-dashboard-podcasts.el --- DETERRED dashboard for podcasts -*- lexical-binding: t -*-

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

;; A dashboard for podcasts, corresponding to `deterred-podcasts'.

;;; Code:
(require 'deterred-dashboard)
(require 'deterred-db)

(defclass deterred-dashboard-podcasts (deterred-dashboard)
  ((name :initform "Podcasts"))
  "A DETERRED dashboard for podcasts.")

(cl-defmethod deterred-dashboard-default-params ((_dashboard deterred-dashboard-podcasts))
  "Default parameters for the podcasts dashboard."
  '((:start-date)
    (:end-date)
    (:feed)
    (:n-top-feeds . 5)))

(cl-defmethod deterred-dashboard-render-params ((_dashboard deterred-dashboard-podcasts))
  "Render the parameters section for the podcasts dashboard."
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
         (feeds
          (mapcar
           (lambda (item) (alist-get 'title item))
           (deterred-db-select-alist
            db "SELECT DISTINCT title FROM podcasts_feed ORDER BY title"))))
    (deterred-dashboard-widget-completing-read-multiple
     :name "Feed"
     :key :feed
     :options feeds)
    (insert "\n")
    (deterred-dashboard-widget-number
     :name "Top N feeds"
     :key :n-top-feeds)
    (insert "\n")))

(cl-defmethod deterred-dashboard-list-datasets ((_dashboard deterred-dashboard-podcasts))
  "List datasets for the PODCASTS dashboard."
  '((top-feeds (name . "Top feeds"))
    (listened-by-year (name . "Hours listened by year"))
    (listened-by-month (name . "Hours listened by month"))
    (podcasts-new (name . "Discovered new podcasts per year"))
    (podcasts-new-hours (name . "Listened to new podcasts per year"))
    (podcasts-languages (name . "Listened to languages per year"))
    (listened-to-top-by-month (name . "Listened to top N podcasts per month"))
    (days-waited-to-listen (name . "Average days waited to listen per podcast"))))

(cl-defmethod deterred-dashboard-fetch-datasets ((_dashboard deterred-dashboard-podcasts)
                                                 params)
  "Fetch datasets for the PODCASTS dashboard.

PARAMS is as returned by `deterred-dashboard-default-params'."
  (let ((db (deterred-db--init)))
    `((top-feeds
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  pf.title,
  pf.\"language\",
  sum(pl.played_duration) * 100 / (60 * 60) / 100.0 hours
FROM podcasts_listened pl
INNER JOIN podcasts_feed pf ON pf.id = pl.feed_id
WHERE 1 = 1 [[AND pl.timestamp >= :start-date]] [[AND pl.timestamp <= :end-date]]
  [[AND pf.title IN :feed]]
GROUP BY pf.title, pf.\"language\"
ORDER BY hours DESC
LIMIT 200"
           params))
      (listened-by-year
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y', pl.timestamp, 'unixepoch') year,
  sum(pl.played_duration) * 100 / (60 * 60) / 100.0 hours
FROM podcasts_listened pl
INNER JOIN podcasts_feed pf ON pf.id = pl.feed_id
WHERE 1 = 1 [[AND pl.timestamp >= :start-date]] [[AND pl.timestamp <= :end-date]]
  [[AND pf.title IN :feed]]
GROUP BY strftime('%Y', pl.timestamp, 'unixepoch')"
           params))
      (listened-by-month
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y-%m', pl.timestamp, 'unixepoch') month,
  sum(pl.played_duration) * 100 / (60 * 60) / 100.0 hours
FROM podcasts_listened pl
INNER JOIN podcasts_feed pf ON pf.id = pl.feed_id
WHERE 1 = 1 [[AND pl.timestamp >= :start-date]] [[AND pl.timestamp <= :end-date]]
  [[AND pf.title IN :feed]]
GROUP BY strftime('%Y-%m', pl.timestamp, 'unixepoch')"
           params))
      (podcasts-new-hours
       . ,(deterred-db-select-template-alist
           db
           "WITH podcast_discovered_years AS (
  SELECT STRFTIME('%Y', min(timestamp), 'unixepoch') YEAR, pf.title FROM podcasts_feed pf
  INNER JOIN podcasts_listened pl ON pl.feed_id = pf.id
  GROUP BY pf.title
)
SELECT
  STRFTIME('%Y', pl.timestamp, 'unixepoch') \"year\",
  sum(CASE WHEN pdy.year = STRFTIME('%Y', pl.timestamp, 'unixepoch') THEN pl.played_duration ELSE 0 END) / (60 * 60) \"new\",
  sum(CASE WHEN pdy.year != STRFTIME('%Y', pl.timestamp, 'unixepoch') THEN pl.played_duration ELSE 0 END) / (60 * 60) \"old\"
FROM podcasts_feed pf
INNER JOIN podcasts_listened pl ON pl.feed_id = pf.id
INNER JOIN podcast_discovered_years pdy ON pdy.title = pf.title
WHERE 1 = 1 [[AND pl.timestamp >= :start-date]] [[AND pl.timestamp <= :end-date]]
  [[AND pf.title IN :feed]]
GROUP BY STRFTIME('%Y', pl.timestamp, 'unixepoch')"
           params))
      (podcasts-new
       . ,(deterred-db-select-template-alist
           db
           "WITH podcast_discovered_years AS (
  SELECT STRFTIME('%Y', min(timestamp), 'unixepoch') \"year\", pf.title FROM podcasts_feed pf
  INNER JOIN podcasts_listened pl ON pl.feed_id = pf.id
  WHERE 1 = 1 [[AND pl.timestamp >= :start-date]] [[AND pl.timestamp <= :end-date]]
    [[AND pf.title IN :feed]]
  GROUP BY pf.title
)
SELECT \"year\", count(*) new_podcasts FROM podcast_discovered_years
GROUP BY \"year\"
ORDER BY \"year\""
           params))
      (podcasts-languages
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y', pl.timestamp, 'unixepoch') year,
  pf.language,
  sum(pl.played_duration) * 100 / (60 * 60) / 100.0 hours
FROM podcasts_listened pl
INNER JOIN podcasts_feed pf ON pf.id = pl.feed_id
WHERE 1 = 1 [[AND pl.timestamp >= :start-date]] [[AND pl.timestamp <= :end-date]]
  [[AND pf.title IN :feed]]
GROUP BY strftime('%Y', pl.timestamp, 'unixepoch'), pf.language"
           params))
      (listened-to-top-by-month
       . ,(deterred-db-select-template-alist
           db
           "WITH top_podcasts AS (
  SELECT
    pf.id,
    pf.title,
    sum(pl.played_duration) * 100 / (60 * 60) / 100.0 hours
  FROM podcasts_listened pl
  INNER JOIN podcasts_feed pf ON pf.id = pl.feed_id
  WHERE 1 = 1 [[AND pl.timestamp >= :start-date]] [[AND pl.timestamp <= :end-date]]
    [[AND pf.title IN :feed]]
  GROUP BY pf.id, pf.title
  ORDER BY hours DESC
  LIMIT :n-top-feeds
)
SELECT
  strftime('%Y-%m', pl.timestamp, 'unixepoch') month,
  pf.title,
  sum(pl.played_duration) * 100 / (60 * 60) / 100.0 hours
FROM podcasts_listened pl
INNER JOIN podcasts_feed pf ON pf.id = pl.feed_id
INNER JOIN top_podcasts tp ON tp.id = pf.id
WHERE 1 = 1 [[AND pl.timestamp >= :start-date]] [[AND pl.timestamp <= :end-date]]
GROUP BY strftime('%Y-%m', pl.timestamp, 'unixepoch'), pf.title"
           params))
      (days-waited-to-listen
       . ,(deterred-db-select-template-alist
           db
           "WITH top_podcasts AS (
  SELECT
    pf.id,
    pf.title,
    sum(pl.played_duration) * 100 / (60 * 60) / 100.0 hours
  FROM podcasts_listened pl
  INNER JOIN podcasts_feed pf ON pf.id = pl.feed_id
  WHERE 1 = 1 [[AND pl.timestamp >= :start-date]] [[AND pl.timestamp <= :end-date]]
    [[AND pf.title IN :feed]]
  GROUP BY pf.id, pf.title
  ORDER BY hours DESC
  LIMIT 10
)
SELECT
  pf.title,
  CAST(avg(pl.\"timestamp\" - pl.published_timestamp) * 100 / (60 * 60 * 24) AS INTEGER) / 100.0 days_waited_to_listen
FROM podcasts_listened pl
INNER JOIN podcasts_feed pf ON pf.id = pl.feed_id
INNER JOIN top_podcasts tp ON tp.id = pf.id
WHERE 1 = 1 [[AND pl.timestamp >= :start-date]] [[AND pl.timestamp <= :end-date]]
GROUP BY pf.title
ORDER BY days_waited_to_listen"
           params)))))

(cl-defmethod deterred-dashboard-render-results ((_dashboard deterred-dashboard-podcasts)
                                                 _params data)
  "Render DATA for the PODCASTS dashboard."
  (insert (deterred-format (f-h2 "Top feeds") "\n")
          (deterred-grid-print-with-org
           (alist-get 'data (alist-get 'top-feeds data))
           :column-names '((title . "Feed") (language . "Language") (hours . "Hours"))
           :max-rows 10
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

images = []

fig, ax = plt.subplots(figsize=(8, 5))
df_y.plot(ax=ax, kind='bar', x='year', y='hours')
ax.set_title('Hours listened per year')
ax.xaxis.set_major_locator(MaxNLocator(nbins=40))
images.append(fig_to_b64(fig))

fig, ax = plt.subplots(figsize=(8, 5))
df_m.plot(ax=ax, kind='bar', x='month', y='hours')
ax.set_title('Hours listened per month')
if len(df_m) > 30:
    ax.xaxis.set_major_locator(MaxNLocator(nbins=40))
images.append(fig_to_b64(fig))

print(json.dumps(images))"
   :input data
   :on-success
   (lambda (images)
     (insert (deterred-format (f-h3 "Listened per year") "\n"))
     (deterred-dashboard-print-images-base64 (elt images 0))
     (insert "\n")
     (insert (deterred-format (f-h3 "Listened per month") "\n"))
     (deterred-dashboard-print-images-base64 (elt images 1))
     (insert "\n")))
  (insert
   (deterred-format (f-h2 "New podcasts over the years") "\n"))
  (deterred-dashboard-exec-python
   :python-code
   "from matplotlib import pyplot as plt
from matplotlib.ticker import MaxNLocator
from deterred import fig_to_b64

import pandas as pd

import json

data = json.loads(input())
df_podcasts = pd.DataFrame(data['podcasts-new']['data'])
df_new = pd.DataFrame(data['podcasts-new-hours']['data'])

images = []

fig, ax = plt.subplots(figsize=(8, 5))
df_podcasts.plot(ax=ax, kind='bar', x='year', y='new_podcasts')
ax.set_title('Podcasts discovered per year')
images.append(fig_to_b64(fig))

fig, ax = plt.subplots(figsize=(8, 5))
df_new.plot(ax=ax, kind='bar', x='year', stacked=True)
ax.set_title('Hours listened to new podcasts per year')
images.append(fig_to_b64(fig))

print(json.dumps(images))"
   :input data
   :on-success
   (lambda (images)
     (insert (deterred-format (f-h3 "Podcasts discovered per year") "\n"))
     (deterred-dashboard-print-images-base64 (elt images 1))
     (insert "\n")
     (insert (deterred-format (f-h3 "Hours listened to new podcasts per year") "\n"))
     (deterred-dashboard-print-images-base64 (elt images 0))
     (insert "\n")))
  (insert
   (deterred-format (f-h2 "Listening dynamics"))  "\n"
   (deterred-format (f-h3 "Hours listened to top N podcast by month")) "\n")
  (deterred-dashboard-exec-python
   :python-code
   "from matplotlib import pyplot as plt
from matplotlib.ticker import MaxNLocator
from deterred import fig_to_b64

import pandas as pd

import json

data = json.loads(input())
df = pd.DataFrame(data['listened-to-top-by-month']['data'])
df_p = df.pivot(index='month', columns='title', values='hours').fillna(0)

images = []

fig, ax = plt.subplots(figsize=(8, 5))
df_p.plot(ax=ax, kind='bar', stacked=True)
ax.set_title('Hours listened to top N podcast by month')
if len(df_p) > 30:
    ax.xaxis.set_major_locator(MaxNLocator(nbins=40))
images.append(fig_to_b64(fig))

print(json.dumps(images))"
   :input data
   :on-success
   #'deterred-dashboard-print-images-base64)
  (insert
   "\n"
   (deterred-format (f-h3 "Average days waited to listen per podcast")) "\n"
   (deterred-grid-print-with-org
    (alist-get 'data (alist-get 'days-waited-to-listen data))
    :column-names '((title . "Feed") (days_waited_to_listen . "Days"))
    :max-rows 10
    :grid-button t)))

(provide 'deterred-dashboard-podcasts)
;;; deterred-dashboard-podcasts.el ends here

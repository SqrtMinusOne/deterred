;;; deterred-dashboard-read-it-later.el --- DETERRED dashboard for read-it-later -*- lexical-binding: t -*-

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

;; A dashboard for read-it-later apps, corresponding to `deterred-read-it-later'.

;;; Code:
(require 'deterred-dashboard)
(require 'deterred-db)
(require 'deterred-utils)

(defclass deterred-dashboard-read-it-later (deterred-dashboard)
  ((name :initform "Read It Later"))
  "A DETERRED dashboard for read-it-later apps.")

(cl-defmethod deterred-dashboard-default-params ((_dashboard deterred-dashboard-read-it-later))
  "Default parameters for the read-it-later dashboard."
  '((:start-date)
    (:end-date)
    (:hosts)
    (:providers)))

(cl-defmethod deterred-dashboard-render-params ((_dashboard deterred-dashboard-read-it-later))
  "Render the parameters section for the read-it-later dashboard."
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
         (hosts (mapcar
                 (lambda (datum) (cons (alist-get 'host datum)
                                       (alist-get 'host datum)))
                 (deterred-db-select-alist
                  db "SELECT host FROM read_it_later_host ORDER BY host")))
         (providers (mapcar
                     (lambda (datum) (cons (alist-get 'provider datum)
                                           (alist-get 'provider datum)))
                     (deterred-db-select-alist
                      db "SELECT DISTINCT provider FROM read_it_later_article ORDER BY provider"))))
    (deterred-dashboard-widget-completing-read-multiple
     :name "Host"
     :key :hosts
     :options hosts)
    (insert "\n")
    (deterred-dashboard-widget-completing-read-multiple
     :name "Provider"
     :key :providers
     :options providers))
  (insert "\n"))

(cl-defmethod deterred-dashboard-list-datasets ((_dashboard deterred-dashboard-read-it-later))
  "List datasets for the read-it-later dashboard."
  '((articles-per-year (name . "Articles read per year"))
    (unique-hosts-per-year (name . "Unique reading sites per year"))
    (top-five-hosts-share-per-year
     (name . "Share of articles from the top five sites per year"))
    (articles-per-month (name . "Articles read per month"))
    (articles-in-language-per-month (name . "Articles in language per month"))
    (top-hosts (name . "Top hosts"))
    (top-languages (name . "Top languages"))
    (top-providers (name . "Top providers"))
    (top-days (name . "Top days"))
    (top-weeks (name . "Top weeks"))
    (top-months (name . "Top months"))
    (numbers-data (name . "Numerical data"))))

(defun deterred-dashboard-read-it-later--host-statistics-per-year (db params)
  "Fetch annual reading-site counts and concentration from DB using PARAMS.

Rank the full set of stored hosts separately for each year.  Missing
or blank hosts count toward the article total, but not the site count
or top five.  Return only annual aggregate values, without host names."
  (deterred-db-select-template-alist
   db
   "WITH filtered_articles AS (
  SELECT strftime('%Y', rila.read_at, 'unixepoch') year, rila.host
  FROM read_it_later_article rila
  WHERE 1 = 1 [[AND rila.read_at >= :start-date]] [[AND rila.read_at <= :end-date]]
    [[AND rila.host IN :hosts]]
    [[AND rila.provider IN :providers]]
), annual_articles AS (
  SELECT year, COUNT(*) articles
  FROM filtered_articles
  GROUP BY year
), host_articles AS (
  SELECT year, host, COUNT(*) articles
  FROM filtered_articles
  WHERE host IS NOT NULL AND length(trim(host, char(9, 10, 11, 12, 13, 32))) > 0
  GROUP BY year, host
), ranked_hosts AS (
  SELECT year, articles,
    ROW_NUMBER() OVER (PARTITION BY year ORDER BY articles DESC, host) position
  FROM host_articles
), annual_hosts AS (
  SELECT year, COUNT(*) hosts,
    SUM(CASE WHEN position <= 5 THEN articles ELSE 0 END) top_five_articles
  FROM ranked_hosts
  GROUP BY year
)
SELECT aa.year, COALESCE(ah.hosts, 0) hosts, aa.articles,
  COALESCE(ah.top_five_articles, 0) top_five_articles,
  COALESCE(ah.top_five_articles, 0) * 100.0 / aa.articles percentage
FROM annual_articles aa
LEFT JOIN annual_hosts ah ON ah.year = aa.year
ORDER BY aa.year"
   params))

(cl-defmethod deterred-dashboard-fetch-datasets ((_dashboard deterred-dashboard-read-it-later)
                                                 params)
  "Fetch datasets for the read-it-later dashboard.

PARAMS is as returned by `deterred-dashboard-default-params'."
  (let* ((db (deterred-db--init))
         (numbers-data
          (deterred-db-select-template-alist
           db
           "SELECT (
  SELECT COUNT(*)
  FROM read_it_later_article rila
  WHERE 1 = 1 [[AND rila.read_at >= :start-date]] [[AND rila.read_at <= :end-date]]
  [[AND rila.host IN :hosts]]
  [[AND rila.provider IN :providers]]
) AS total_articles,
(
  SELECT COUNT(DISTINCT rila.host)
  FROM read_it_later_article rila
  WHERE 1 = 1 [[AND rila.read_at >= :start-date]] [[AND rila.read_at <= :end-date]]
  [[AND rila.host IN :hosts]]
  [[AND rila.provider IN :providers]]
) AS unique_hosts,
(
  SELECT COUNT(DISTINCT rilh.language)
  FROM read_it_later_article rila
  INNER JOIN read_it_later_host rilh ON rilh.host = rila.host
  WHERE rilh.language IS NOT NULL
    [[AND rila.read_at >= :start-date]] [[AND rila.read_at <= :end-date]]
    [[AND rila.host IN :hosts]]
    [[AND rila.provider IN :providers]]
) AS unique_languages,
(
  SELECT COUNT(DISTINCT rila.provider)
  FROM read_it_later_article rila
  WHERE 1 = 1 [[AND rila.read_at >= :start-date]] [[AND rila.read_at <= :end-date]]
  [[AND rila.host IN :hosts]]
  [[AND rila.provider IN :providers]]
) AS unique_providers;"
           params))
         (total-articles (alist-get 'total_articles (car numbers-data)))
         (host-statistics
          (deterred-dashboard-read-it-later--host-statistics-per-year db params)))
    `((articles-per-year
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y', rila.read_at, 'unixepoch') year,
  COUNT(*) articles
FROM read_it_later_article rila
WHERE 1 = 1 [[AND rila.read_at >= :start-date]] [[AND rila.read_at <= :end-date]]
  [[AND rila.host IN :hosts]]
  [[AND rila.provider IN :providers]]
GROUP BY strftime('%Y', rila.read_at, 'unixepoch')"
           params))
      (unique-hosts-per-year
       . ,(mapcar
           (lambda (row)
             `((year . ,(alist-get 'year row))
               (hosts . ,(alist-get 'hosts row))))
           host-statistics))
      (top-five-hosts-share-per-year
       . ,(mapcar
           (lambda (row)
             `((year . ,(alist-get 'year row))
               (articles . ,(alist-get 'articles row))
               (top_five_articles . ,(alist-get 'top_five_articles row))
               (percentage . ,(alist-get 'percentage row))))
           host-statistics))
      (articles-per-month
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y-%m', rila.read_at, 'unixepoch') month,
  COUNT(*) articles
FROM read_it_later_article rila
WHERE 1 = 1 [[AND rila.read_at >= :start-date]] [[AND rila.read_at <= :end-date]]
  [[AND rila.host IN :hosts]]
  [[AND rila.provider IN :providers]]
GROUP BY strftime('%Y-%m', rila.read_at, 'unixepoch')"
           params))
      (articles-in-language-per-month
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y-%m', rila.read_at, 'unixepoch') month,
  COALESCE(rilh.language, 'Unknown') language,
  COUNT(*) articles
FROM read_it_later_article rila
LEFT JOIN read_it_later_host rilh ON rilh.host = rila.host
WHERE 1 = 1 [[AND rila.read_at >= :start-date]] [[AND rila.read_at <= :end-date]]
  [[AND rila.host IN :hosts]]
  [[AND rila.provider IN :providers]]
GROUP BY strftime('%Y-%m', rila.read_at, 'unixepoch'), COALESCE(rilh.language, 'Unknown')
ORDER BY month ASC"
           params))
      (top-hosts
       . ,(deterred-utils-add-fraction
           (deterred-db-select-template-alist
            db
            "SELECT
  rila.host,
  COUNT(*) articles
FROM read_it_later_article rila
WHERE 1 = 1 [[AND rila.read_at >= :start-date]] [[AND rila.read_at <= :end-date]]
  [[AND rila.host IN :hosts]]
  [[AND rila.provider IN :providers]]
GROUP BY rila.host
ORDER BY articles DESC
LIMIT 20"
            params)
           total-articles
           'articles))
      (top-languages
       . ,(deterred-utils-add-fraction
           (deterred-db-select-template-alist
            db
            "SELECT
  COALESCE(rilh.language, 'Unknown') language,
  COUNT(*) articles
FROM read_it_later_article rila
LEFT JOIN read_it_later_host rilh ON rilh.host = rila.host
WHERE 1 = 1 [[AND rila.read_at >= :start-date]] [[AND rila.read_at <= :end-date]]
  [[AND rila.host IN :hosts]]
  [[AND rila.provider IN :providers]]
GROUP BY COALESCE(rilh.language, 'Unknown')
ORDER BY articles DESC
LIMIT 20"
            params)
           total-articles
           'articles))
      (top-providers
       . ,(deterred-utils-add-fraction
           (deterred-db-select-template-alist
            db
            "SELECT
  rila.provider,
  COUNT(*) articles
FROM read_it_later_article rila
WHERE 1 = 1 [[AND rila.read_at >= :start-date]] [[AND rila.read_at <= :end-date]]
  [[AND rila.host IN :hosts]]
  [[AND rila.provider IN :providers]]
GROUP BY rila.provider
ORDER BY articles DESC"
            params)
           total-articles
           'articles))
      (top-days
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y-%m-%d', rila.read_at, 'unixepoch') day,
  COUNT(*) articles
FROM read_it_later_article rila
WHERE 1 = 1 [[AND rila.read_at >= :start-date]] [[AND rila.read_at <= :end-date]]
  [[AND rila.host IN :hosts]]
  [[AND rila.provider IN :providers]]
GROUP BY strftime('%Y-%m-%d', rila.read_at, 'unixepoch')
ORDER BY articles DESC
LIMIT 20"
           params))
      (top-weeks
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y-%W', rila.read_at, 'unixepoch') week,
  COUNT(*) articles
FROM read_it_later_article rila
WHERE 1 = 1 [[AND rila.read_at >= :start-date]] [[AND rila.read_at <= :end-date]]
  [[AND rila.host IN :hosts]]
  [[AND rila.provider IN :providers]]
GROUP BY strftime('%Y-%W', rila.read_at, 'unixepoch')
ORDER BY articles DESC
LIMIT 20"
           params))
      (top-months
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y-%m', rila.read_at, 'unixepoch') month,
  COUNT(*) articles
FROM read_it_later_article rila
WHERE 1 = 1 [[AND rila.read_at >= :start-date]] [[AND rila.read_at <= :end-date]]
  [[AND rila.host IN :hosts]]
  [[AND rila.provider IN :providers]]
GROUP BY strftime('%Y-%m', rila.read_at, 'unixepoch')
ORDER BY articles DESC
LIMIT 20"
           params))
      (numbers-data . ,numbers-data))))

(cl-defmethod deterred-dashboard-render-results ((_dashboard deterred-dashboard-read-it-later)
                                                 _params data)
  "Render DATA for the read-it-later dashboard."
  (insert
   (deterred-format
    "I've read "
    (f-ace (f (f-num (f-acc "data->'numbers-data->'data[0]->'total_articles")))
           'bold)
    " articles from "
    (f-ace (f (f-num (f-acc "data->'numbers-data->'data[0]->'unique_hosts")))
           'bold)
    " unique hosts, in "
    (f-ace (f (f-num (f-acc "data->'numbers-data->'data[0]->'unique_languages")))
           'bold)
    " languages, across "
    (f-ace (f (f-num (f-acc "data->'numbers-data->'data[0]->'unique_providers")))
           'bold)
    " providers.\n\n"
    (f-h2 "Top over all time") "\n"
    (f-h3 "Top hosts") "\n")
   (deterred-grid-print-with-org
    (alist-get 'data (alist-get 'top-hosts data))
    :column-names '((host . "Host") (articles . "Articles") (fraction . "%"))
    :max-rows 10
    :grid-button t)
   "\n"
   (deterred-format (f-h3 "Top languages") "\n")
   (deterred-grid-print-with-org
    (alist-get 'data (alist-get 'top-languages data))
    :column-names '((language . "Language") (articles . "Articles") (fraction . "%"))
    :max-rows 10
    :grid-button t)
   "\n"
   (deterred-format (f-h3 "Top providers") "\n")
   (deterred-grid-print-with-org
    (alist-get 'data (alist-get 'top-providers data))
    :column-names '((provider . "Provider") (articles . "Articles") (fraction . "%"))
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
df_y = pd.DataFrame(data['articles-per-year']['data'])
df_m = pd.DataFrame(data['articles-per-month']['data'])
df_l = pd.DataFrame(data['articles-in-language-per-month']['data'])
df_lp = df_l.pivot(index='month', columns='language', values='articles').fillna(0)

images = []

fig, ax = plt.subplots(figsize=(8, 5))
df_y.plot(ax=ax, kind='bar', x='year', y='articles')
ax.set_title('Articles read per year')
for container in ax.containers:
    ax.bar_label(container, fmt='%.0f')
images.append(fig_to_b64(fig))

fig, ax = plt.subplots(figsize=(8, 5))
df_m.plot(ax=ax, kind='bar', x='month', y='articles')
ax.set_title('Articles read per month')
if len(df_m) > 30:
    ax.xaxis.set_major_locator(MaxNLocator(nbins=40))
if len(df_m) < 30:
    for container in ax.containers:
        ax.bar_label(container, fmt='%.0f')
images.append(fig_to_b64(fig))

fig, ax = plt.subplots(figsize=(8, 5))
df_lp.plot(ax=ax, kind='bar', stacked=True)
ax.set_title('Articles in language per month')
if len(df_lp) > 30:
    ax.xaxis.set_major_locator(MaxNLocator(nbins=40))
images.append(fig_to_b64(fig))

print(json.dumps(images))"
   :input data
   :on-success
   (lambda (images)
     (insert (deterred-format (f-h3 "Articles read per year") "\n"))
     (deterred-dashboard-print-images-base64 (elt images 0))
     (insert "\n")
     (insert (deterred-format (f-h3 "Articles read per month") "\n"))
     (deterred-dashboard-print-images-base64 (elt images 1))
     (insert "\n")
     (insert (deterred-format (f-h3 "Articles in language per month") "\n"))
     (deterred-dashboard-print-images-base64 (elt images 2))
     (insert "\n")))
  (insert
   (deterred-format (f-h2 "Top periods") "\n"
                    (f-h3 "Top months") "\n")
   (deterred-grid-print-with-org
    (alist-get 'data (alist-get 'top-months data))
    :column-names '((month . "Month") (articles . "Articles"))
    :max-rows 10
    :grid-button t)
   "\n"
   (deterred-format (f-h3 "Top weeks") "\n")
   (deterred-grid-print-with-org
    (alist-get 'data (alist-get 'top-weeks data))
    :column-names '((week . "Week") (articles . "Articles"))
    :max-rows 10
    :grid-button t)
   "\n"
   (deterred-format (f-h3 "Top days") "\n")
   (deterred-grid-print-with-org
    (alist-get 'data (alist-get 'top-days data))
    :column-names '((day . "Day") (articles . "Articles"))
    :max-rows 10
    :grid-button t)
   "\n"))

(provide 'deterred-dashboard-read-it-later)
;;; deterred-dashboard-read-it-later.el ends here

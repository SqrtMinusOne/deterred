;;; deterred-dashboard-ai.el --- DETERRED dashboard for AI usage -*- lexical-binding: t -*-

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
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A dashboard for AI usage data, corresponding to `deterred-ai-usage'.

;;; Code:
(require 'deterred-dashboard)
(require 'deterred-chains)
(require 'deterred-db)
(require 'deterred-utils)

(defconst deterred-dashboard-ai-usage-normalize-timeout (* 15 60))

(defun deterred-dashboard-ai--get-wakatime-overlap (db params)
  "Calculate overlap between AI usage and WakaTime activity.

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
  [[AND timestamp <= :end-date]]
  [[AND hostname IN :hostname]]
  [[AND model_name IN :model]]"
         params))
       (ai-usage (car (deterred-chains-normalize
                       (list
                        (mapcar (lambda (e) (list (alist-get 'timestamp e) nil nil))
                                ai-usage-raw))
                       deterred-dashboard-ai-usage-normalize-timeout)))
       (ai-usage-start (caar ai-usage))
       (ai-usage-end (caar (last ai-usage)))
       (wakatime-data-raw
        (deterred-db-select-template-alist
         db
         "SELECT wi.start_timestamp, wi.end_timestamp
FROM wakatime_item wi
WHERE 1 = 1
  [[AND wi.start_timestamp >= :start-date]]
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
              deterred-dashboard-ai-usage-normalize-timeout)))
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

(defun deterred-dashboard-ai--format-tokens-column (data)
  "Format the `tokens' column in DATA for human display."
  (mapcar (lambda (row)
            (let ((copy (copy-alist row)))
              (when-let* ((tokens (alist-get 'tokens copy))
                          (_ (numberp tokens)))
                (setf (alist-get 'tokens copy)
                      (cond
                       ((>= tokens 1000000) (format "%.1fM" (/ tokens 1000000.0)))
                       ((>= tokens 1000) (format "%.1fk" (/ tokens 1000.0)))
                       (t (number-to-string tokens)))))
              copy))
          data))

(defclass deterred-dashboard-ai (deterred-dashboard)
  ((name :initform "AI Usage"))
  "A DETERRED dashboard for AI usage data.")

(cl-defmethod deterred-dashboard-default-params ((_dashboard deterred-dashboard-ai))
  "Default parameters for the AI Usage dashboard."
  '((:start-date)
    (:end-date)
    (:hostname)
    (:model)
    (:projects)
    (:recent-days . 14)))

(cl-defmethod deterred-dashboard-render-params ((_dashboard deterred-dashboard-ai))
  "Render the parameters section for the AI Usage dashboard."
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
         (hostnames
          (mapcar
           (lambda (item) (alist-get 'hostname item))
           (deterred-db-select-alist
            db "SELECT DISTINCT hostname FROM ai_usage_item
WHERE is_stats = 0 ORDER BY hostname")))
         (models
          (mapcar
           (lambda (item) (alist-get 'model_name item))
           (deterred-db-select-alist
            db "SELECT DISTINCT model_name FROM ai_usage_item
WHERE is_stats = 0 ORDER BY model_name")))
         (projects
          (mapcar
           (lambda (datum) (cons (alist-get 'name datum)
                                 (alist-get 'id datum)))
           (deterred-db-select-alist
            db "SELECT wp.id, wp.name FROM wakatime_projects wp
WHERE wp.id IN (SELECT DISTINCT project_id FROM ai_usage_item
                WHERE project_id IS NOT NULL AND is_stats = 0)
ORDER BY wp.name"))))
    (deterred-dashboard-widget-completing-read-multiple
     :name "Hostname"
     :key :hostname
     :options hostnames)
    (insert "\n")
    (deterred-dashboard-widget-completing-read-multiple
     :name "Model"
     :key :model
     :options models)
    (insert "\n")
    (deterred-dashboard-widget-completing-read-multiple
     :name "Project"
     :key :projects
     :options projects))
  (insert "\n")
  (deterred-dashboard-widget-number
   :name "Recent days"
   :key :recent-days)
  (insert "\n"))

(cl-defmethod deterred-dashboard-list-datasets ((_dashboard deterred-dashboard-ai))
  "List datasets for the AI Usage dashboard."
  '((numbers-data (name . "Summary numbers"))
    (top-models (name . "Top models"))
    (top-projects (name . "Top projects"))
    (ai-usage-per-month (name . "AI usage per month"))
    (ai-usage-per-week (name . "AI usage per week"))
    (ai-usage-per-day (name . "AI usage per day"))
    (cost-by-model-per-day (name . "Cost by model per day"))
    (cost-by-model-per-week (name . "Cost by model per week"))
    (cost-by-model-per-month (name . "Cost by model per month"))
    (tokens-by-model-per-day (name . "Tokens by model per day"))
    (tokens-by-model-per-week (name . "Tokens by model per week"))
    (tokens-by-model-per-month (name . "Tokens by model per month"))
    (messages-by-model-per-week (name . "Messages by model per week"))
    (messages-by-model-per-month (name . "Messages by model per month"))
    (messages-by-hostname-per-day (name . "Messages by hostname per day"))
    (messages-by-hostname-per-week (name . "Messages by hostname per week"))
    (messages-by-hostname-per-month (name . "Messages by hostname per month"))
    (avg-cost-per-message-per-month (name . "Average cost per message per month"))
    (recent-messages (name . "Recent messages by time of day"))
    (top-files (name . "Top files by AI changes"))
    (top-days-by-cost (name . "Top days by cost"))
    (top-days-by-tokens (name . "Top days by tokens"))
    (top-weeks (name . "Top weeks by cost"))
    (top-months (name . "Top months by cost"))))

(cl-defmethod deterred-dashboard-fetch-datasets ((_dashboard deterred-dashboard-ai)
                                                  params)
  "Fetch datasets for the AI Usage dashboard.

PARAMS is as returned by `deterred-dashboard-default-params'."
  (let* ((db (deterred-db--init))
         (ai-usage-datasets (deterred-dashboard-ai--get-wakatime-overlap db params))
         (numbers-data
          (deterred-db-select-template-alist
           db
           "SELECT (
  SELECT ROUND(SUM(usd_cost) / 1000000.0, 2)
  FROM ai_usage_item
  WHERE is_stats = 0 [[AND timestamp >= :start-date]]
    [[AND timestamp <= :end-date]]
    [[AND hostname IN :hostname]]
    [[AND model_name IN :model]]
    [[AND project_id IN :projects]]
) AS total_cost,
(
  SELECT SUM(total_tokens)
  FROM ai_usage_item
  WHERE is_stats = 0 [[AND timestamp >= :start-date]]
    [[AND timestamp <= :end-date]]
    [[AND hostname IN :hostname]]
    [[AND model_name IN :model]]
    [[AND project_id IN :projects]]
) AS total_tokens,
(
  SELECT COUNT(*)
  FROM ai_usage_item
  WHERE is_stats = 0 [[AND timestamp >= :start-date]]
    [[AND timestamp <= :end-date]]
    [[AND hostname IN :hostname]]
    [[AND model_name IN :model]]
    [[AND project_id IN :projects]]
) AS msg_count,
(
  SELECT COUNT(DISTINCT model_name)
  FROM ai_usage_item
  WHERE is_stats = 0 [[AND timestamp >= :start-date]]
    [[AND timestamp <= :end-date]]
    [[AND hostname IN :hostname]]
    [[AND model_name IN :model]]
    [[AND project_id IN :projects]]
) AS model_count,
(
  SELECT COUNT(DISTINCT hostname)
  FROM ai_usage_item
  WHERE is_stats = 0 [[AND timestamp >= :start-date]]
    [[AND timestamp <= :end-date]]
    [[AND hostname IN :hostname]]
    [[AND model_name IN :model]]
    [[AND project_id IN :projects]]
) AS hostname_count,
(
  SELECT COUNT(DISTINCT date(timestamp, 'unixepoch'))
  FROM ai_usage_item
  WHERE is_stats = 0 [[AND timestamp >= :start-date]]
    [[AND timestamp <= :end-date]]
    [[AND hostname IN :hostname]]
    [[AND model_name IN :model]]
    [[AND project_id IN :projects]]
) AS day_count"
           params))
         (total-cost (alist-get 'total_cost (car numbers-data))))
    `((top-models
       . ,(deterred-utils-add-fraction
           (deterred-db-select-template-alist
            db
            "SELECT
  model_name,
  ROUND(SUM(usd_cost) / 1000000.0, 2) cost,
  SUM(total_tokens) tokens,
  COUNT(*) messages
FROM ai_usage_item
WHERE is_stats = 0 [[AND timestamp >= :start-date]]
  [[AND timestamp <= :end-date]]
  [[AND hostname IN :hostname]]
  [[AND model_name IN :model]]
  [[AND project_id IN :projects]]
GROUP BY model_name
ORDER BY cost DESC"
            params)
           total-cost 'cost))
      (top-projects
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  wp.name project_name,
  COUNT(*) messages,
  ROUND(SUM(i.usd_cost) / 1000000.0, 2) cost,
  SUM(i.total_tokens) tokens
FROM ai_usage_item i
INNER JOIN wakatime_projects wp ON wp.id = i.project_id
WHERE i.is_stats = 0
  AND i.project_id IS NOT NULL
  [[AND i.timestamp >= :start-date]]
  [[AND i.timestamp <= :end-date]]
  [[AND i.hostname IN :hostname]]
  [[AND i.model_name IN :model]]
  [[AND i.project_id IN :projects]]
GROUP BY i.project_id
ORDER BY cost DESC
LIMIT 30"
           params))
      (ai-usage-per-month . ,(nth 0 ai-usage-datasets))
      (ai-usage-per-week . ,(nth 1 ai-usage-datasets))
      (ai-usage-per-day . ,(nth 2 ai-usage-datasets))
      (cost-by-model-per-day
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  date(timestamp, 'unixepoch') day,
  model_name,
  ROUND(SUM(usd_cost) / 1000000.0, 4) cost
FROM ai_usage_item
WHERE is_stats = 0 [[AND timestamp >= :start-date]]
  [[AND timestamp <= :end-date]]
  [[AND hostname IN :hostname]]
  [[AND model_name IN :model]]
  [[AND project_id IN :projects]]
GROUP BY date(timestamp, 'unixepoch'), model_name
ORDER BY day ASC"
           params))
      (cost-by-model-per-week
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y-%W', timestamp, 'unixepoch') week,
  model_name,
  ROUND(SUM(usd_cost) / 1000000.0, 2) cost
FROM ai_usage_item
WHERE is_stats = 0 [[AND timestamp >= :start-date]]
  [[AND timestamp <= :end-date]]
  [[AND hostname IN :hostname]]
  [[AND model_name IN :model]]
  [[AND project_id IN :projects]]
GROUP BY strftime('%Y-%W', timestamp, 'unixepoch'), model_name
ORDER BY week ASC"
           params))
      (cost-by-model-per-month
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y-%m', timestamp, 'unixepoch') month,
  model_name,
  ROUND(SUM(usd_cost) / 1000000.0, 2) cost
FROM ai_usage_item
WHERE is_stats = 0 [[AND timestamp >= :start-date]]
  [[AND timestamp <= :end-date]]
  [[AND hostname IN :hostname]]
  [[AND model_name IN :model]]
  [[AND project_id IN :projects]]
GROUP BY strftime('%Y-%m', timestamp, 'unixepoch'), model_name
ORDER BY month ASC"
           params))
      (tokens-by-model-per-day
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  date(timestamp, 'unixepoch') day,
  model_name,
  SUM(total_tokens) tokens
FROM ai_usage_item
WHERE is_stats = 0 [[AND timestamp >= :start-date]]
  [[AND timestamp <= :end-date]]
  [[AND hostname IN :hostname]]
  [[AND model_name IN :model]]
  [[AND project_id IN :projects]]
GROUP BY date(timestamp, 'unixepoch'), model_name
ORDER BY day ASC"
           params))
      (tokens-by-model-per-week
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y-%W', timestamp, 'unixepoch') week,
  model_name,
  SUM(total_tokens) tokens
FROM ai_usage_item
WHERE is_stats = 0 [[AND timestamp >= :start-date]]
  [[AND timestamp <= :end-date]]
  [[AND hostname IN :hostname]]
  [[AND model_name IN :model]]
  [[AND project_id IN :projects]]
GROUP BY strftime('%Y-%W', timestamp, 'unixepoch'), model_name
ORDER BY week ASC"
           params))
      (tokens-by-model-per-month
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y-%m', timestamp, 'unixepoch') month,
  model_name,
  SUM(total_tokens) tokens
FROM ai_usage_item
WHERE is_stats = 0 [[AND timestamp >= :start-date]]
  [[AND timestamp <= :end-date]]
  [[AND hostname IN :hostname]]
  [[AND model_name IN :model]]
  [[AND project_id IN :projects]]
GROUP BY strftime('%Y-%m', timestamp, 'unixepoch'), model_name
ORDER BY month ASC"
           params))
      (messages-by-model-per-week
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y-%W', timestamp, 'unixepoch') week,
  model_name,
  COUNT(*) messages
FROM ai_usage_item
WHERE is_stats = 0 [[AND timestamp >= :start-date]]
  [[AND timestamp <= :end-date]]
  [[AND hostname IN :hostname]]
  [[AND model_name IN :model]]
  [[AND project_id IN :projects]]
GROUP BY strftime('%Y-%W', timestamp, 'unixepoch'), model_name
ORDER BY week ASC"
           params))
      (messages-by-model-per-month
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y-%m', timestamp, 'unixepoch') month,
  model_name,
  COUNT(*) messages
FROM ai_usage_item
WHERE is_stats = 0 [[AND timestamp >= :start-date]]
  [[AND timestamp <= :end-date]]
  [[AND hostname IN :hostname]]
  [[AND model_name IN :model]]
  [[AND project_id IN :projects]]
GROUP BY strftime('%Y-%m', timestamp, 'unixepoch'), model_name
ORDER BY month ASC"
           params))
      (messages-by-hostname-per-day
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  date(timestamp, 'unixepoch') day,
  hostname,
  COUNT(*) messages
FROM ai_usage_item
WHERE is_stats = 0 [[AND timestamp >= :start-date]]
  [[AND timestamp <= :end-date]]
  [[AND hostname IN :hostname]]
  [[AND model_name IN :model]]
  [[AND project_id IN :projects]]
GROUP BY date(timestamp, 'unixepoch'), hostname
ORDER BY day ASC"
           params))
      (messages-by-hostname-per-week
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y-%W', timestamp, 'unixepoch') week,
  hostname,
  COUNT(*) messages
FROM ai_usage_item
WHERE is_stats = 0 [[AND timestamp >= :start-date]]
  [[AND timestamp <= :end-date]]
  [[AND hostname IN :hostname]]
  [[AND model_name IN :model]]
  [[AND project_id IN :projects]]
GROUP BY strftime('%Y-%W', timestamp, 'unixepoch'), hostname
ORDER BY week ASC"
           params))
      (messages-by-hostname-per-month
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y-%m', timestamp, 'unixepoch') month,
  hostname,
  COUNT(*) messages
FROM ai_usage_item
WHERE is_stats = 0 [[AND timestamp >= :start-date]]
  [[AND timestamp <= :end-date]]
  [[AND hostname IN :hostname]]
  [[AND model_name IN :model]]
  [[AND project_id IN :projects]]
GROUP BY strftime('%Y-%m', timestamp, 'unixepoch'), hostname
ORDER BY month ASC"
           params))
      (avg-cost-per-message-per-month
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y-%m', timestamp, 'unixepoch') month,
  ROUND(SUM(usd_cost) / 1000000.0 / COUNT(*), 4) avg_cost
FROM ai_usage_item
WHERE is_stats = 0 [[AND timestamp >= :start-date]]
  [[AND timestamp <= :end-date]]
  [[AND hostname IN :hostname]]
  [[AND model_name IN :model]]
  [[AND project_id IN :projects]]
GROUP BY strftime('%Y-%m', timestamp, 'unixepoch')
ORDER BY month ASC"
           params))
      (recent-messages
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  timestamp,
  model_name,
  usd_cost / 1000000.0 cost
FROM ai_usage_item
WHERE is_stats = 0
  AND timestamp >= COALESCE(:end-date, strftime('%s', 'now')) - :recent-days * 86400
  [[AND timestamp >= :start-date]]
  [[AND timestamp <= :end-date]]
  [[AND hostname IN :hostname]]
  [[AND model_name IN :model]]
  [[AND project_id IN :projects]]
ORDER BY timestamp ASC"
           params))
      (top-files
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  f.file_path,
  COUNT(DISTINCT f.message_id) messages,
  COALESCE(SUM(f.lines_added), 0) lines_added,
  COALESCE(SUM(f.lines_removed), 0) lines_removed,
  COALESCE(SUM(f.lines_added), 0) + COALESCE(SUM(f.lines_removed), 0) total_changes
FROM ai_usage_file f
INNER JOIN ai_usage_item i ON i.message_id = f.message_id
WHERE i.is_stats = 0
  [[AND i.timestamp >= :start-date]]
  [[AND i.timestamp <= :end-date]]
  [[AND i.hostname IN :hostname]]
  [[AND i.model_name IN :model]]
  [[AND i.project_id IN :projects]]
GROUP BY f.file_path
ORDER BY total_changes DESC
LIMIT 30"
           params))
      (top-days-by-cost
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  date(timestamp, 'unixepoch') day,
  ROUND(SUM(usd_cost) / 1000000.0, 2) cost,
  SUM(total_tokens) tokens,
  COUNT(*) messages
FROM ai_usage_item
WHERE is_stats = 0 [[AND timestamp >= :start-date]]
  [[AND timestamp <= :end-date]]
  [[AND hostname IN :hostname]]
  [[AND model_name IN :model]]
  [[AND project_id IN :projects]]
GROUP BY date(timestamp, 'unixepoch')
ORDER BY cost DESC
LIMIT 20"
           params))
      (top-days-by-tokens
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  date(timestamp, 'unixepoch') day,
  SUM(total_tokens) tokens,
  ROUND(SUM(usd_cost) / 1000000.0, 2) cost,
  COUNT(*) messages
FROM ai_usage_item
WHERE is_stats = 0 [[AND timestamp >= :start-date]]
  [[AND timestamp <= :end-date]]
  [[AND hostname IN :hostname]]
  [[AND model_name IN :model]]
  [[AND project_id IN :projects]]
GROUP BY date(timestamp, 'unixepoch')
ORDER BY tokens DESC
LIMIT 20"
           params))
      (top-weeks
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y-%W', timestamp, 'unixepoch') week,
  ROUND(SUM(usd_cost) / 1000000.0, 2) cost,
  SUM(total_tokens) tokens,
  COUNT(*) messages
FROM ai_usage_item
WHERE is_stats = 0 [[AND timestamp >= :start-date]]
  [[AND timestamp <= :end-date]]
  [[AND hostname IN :hostname]]
  [[AND model_name IN :model]]
  [[AND project_id IN :projects]]
GROUP BY strftime('%Y-%W', timestamp, 'unixepoch')
ORDER BY cost DESC
LIMIT 20"
           params))
      (top-months
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y-%m', timestamp, 'unixepoch') month,
  ROUND(SUM(usd_cost) / 1000000.0, 2) cost,
  SUM(total_tokens) tokens,
  COUNT(*) messages
FROM ai_usage_item
WHERE is_stats = 0 [[AND timestamp >= :start-date]]
  [[AND timestamp <= :end-date]]
  [[AND hostname IN :hostname]]
  [[AND model_name IN :model]]
  [[AND project_id IN :projects]]
GROUP BY strftime('%Y-%m', timestamp, 'unixepoch')
ORDER BY cost DESC
LIMIT 20"
           params))
      (numbers-data . ,numbers-data))))

(cl-defmethod deterred-dashboard-render-results ((_dashboard deterred-dashboard-ai)
                                                  _params data)
  "Render DATA for the AI Usage dashboard."
  ;; Summary
  (insert
   (deterred-format
    "I've spent $"
    (f-ace (f (f-num (f-acc "data->'numbers-data->'data[0]->'total_cost")))
           'bold)
    " across "
    (f-ace (f (f-num (f-acc "data->'numbers-data->'data[0]->'msg_count")))
           'bold)
    " messages using "
    (f-ace (f (f-num (f-acc "data->'numbers-data->'data[0]->'model_count")))
           'bold)
    " models on "
    (f-ace (f (f-num (f-acc "data->'numbers-data->'data[0]->'hostname_count")))
           'bold)
    " hostnames over "
    (f-ace (f (f-num (f-acc "data->'numbers-data->'data[0]->'day_count")))
           'bold)
    " days.\n\n"
    (f-h2 "Top models") "\n")
   (deterred-grid-print-with-org
    (deterred-dashboard-ai--format-tokens-column
     (alist-get 'data (alist-get 'top-models data)))
    :column-names '((model_name . "Model") (cost . "Cost ($)")
                    (tokens . "Tokens") (messages . "Messages")
                    (fraction . "%"))
    :max-rows 10
    :grid-button t)
   "\n"
   (deterred-format (f-h2 "Top projects") "\n")
   (deterred-grid-print-with-org
    (deterred-dashboard-ai--format-tokens-column
     (alist-get 'data (alist-get 'top-projects data)))
    :column-names '((project_name . "Project") (cost . "Cost ($)")
                    (tokens . "Tokens") (messages . "Messages"))
    :max-rows 15
    :grid-button t)
   "\n"
   (deterred-format (f-h2 "Programming time %") "\n"))
  ;; Coding overlap charts
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

    fig, ax = plt.subplots(figsize=(10, 5))
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
       (insert "\n"))))
  (insert
   (deterred-format (f-h2 "Cost over time") "\n"))
  ;; Cost charts
  (deterred-dashboard-exec-python
   :python-code
   "from matplotlib import pyplot as plt
from matplotlib.ticker import MaxNLocator
from deterred import fig_to_b64

import pandas as pd
import json

data = json.loads(input())
df_cost_day = pd.DataFrame(data['cost-by-model-per-day']['data'])
df_cost_week = pd.DataFrame(data['cost-by-model-per-week']['data'])
df_cost_month = pd.DataFrame(data['cost-by-model-per-month']['data'])

images = []

if not df_cost_day.empty:
    fig, ax = plt.subplots(figsize=(10, 5))
    df_pivot = df_cost_day.pivot(index='day', columns='model_name', values='cost').fillna(0)
    df_pivot.plot(ax=ax, kind='bar', stacked=True)
    ax.set_title('Cost by model per day ($)')
    ax.set_ylabel('Cost ($)')
    if len(df_pivot) > 30:
        ax.xaxis.set_major_locator(MaxNLocator(nbins=30))
    plt.xticks(rotation=45, ha='right')
    plt.tight_layout()
    images.append(fig_to_b64(fig))
else:
    images.append(None)

if not df_cost_week.empty:
    fig, ax = plt.subplots(figsize=(10, 5))
    df_pivot = df_cost_week.pivot(index='week', columns='model_name', values='cost').fillna(0)
    df_pivot.plot(ax=ax, kind='bar', stacked=True)
    ax.set_title('Cost by model per week ($)')
    ax.set_ylabel('Cost ($)')
    totals = df_pivot.sum(axis=1)
    for i, total in enumerate(totals):
        ax.text(i, total, f'${total:.0f}', ha='center', va='bottom', fontsize=7)
    if len(df_pivot) > 30:
        ax.xaxis.set_major_locator(MaxNLocator(nbins=30))
    plt.xticks(rotation=45, ha='right')
    plt.tight_layout()
    images.append(fig_to_b64(fig))
else:
    images.append(None)

if not df_cost_month.empty:
    fig, ax = plt.subplots(figsize=(10, 5))
    df_pivot = df_cost_month.pivot(index='month', columns='model_name', values='cost').fillna(0)
    df_pivot.plot(ax=ax, kind='bar', stacked=True)
    ax.set_title('Cost by model per month ($)')
    ax.set_ylabel('Cost ($)')
    totals = df_pivot.sum(axis=1)
    for i, total in enumerate(totals):
        ax.text(i, total, f'${total:.0f}', ha='center', va='bottom', fontsize=7)
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
       (insert (deterred-format (f-h3 "Cost by model per day") "\n"))
       (deterred-dashboard-print-images-base64 (elt images 0))
       (insert "\n"))
     (when (elt images 1)
       (insert (deterred-format (f-h3 "Cost by model per week") "\n"))
       (deterred-dashboard-print-images-base64 (elt images 1))
       (insert "\n"))
     (when (elt images 2)
       (insert (deterred-format (f-h3 "Cost by model per month") "\n"))
       (deterred-dashboard-print-images-base64 (elt images 2))
       (insert "\n"))))
  ;; Token usage charts
  (insert (deterred-format (f-h2 "Token usage") "\n"))
  (deterred-dashboard-exec-python
   :python-code
   "from matplotlib import pyplot as plt
from matplotlib.ticker import MaxNLocator, FuncFormatter
from deterred import fig_to_b64

import pandas as pd
import json

data = json.loads(input())
df_day = pd.DataFrame(data['tokens-by-model-per-day']['data'])
df_week = pd.DataFrame(data['tokens-by-model-per-week']['data'])
df_month = pd.DataFrame(data['tokens-by-model-per-month']['data'])

tok_fmt = FuncFormatter(lambda x, p: f'{x/1e6:.1f}M' if x >= 1e6 else f'{x/1e3:.0f}k' if x >= 1e3 else f'{x:.0f}')

images = []

if not df_day.empty:
    fig, ax = plt.subplots(figsize=(10, 5))
    df_pivot = df_day.pivot(index='day', columns='model_name', values='tokens').fillna(0)
    df_pivot.plot(ax=ax, kind='bar', stacked=True)
    ax.set_title('Tokens by model per day')
    ax.set_ylabel('Tokens')
    ax.yaxis.set_major_formatter(tok_fmt)
    if len(df_pivot) > 30:
        ax.xaxis.set_major_locator(MaxNLocator(nbins=30))
    plt.xticks(rotation=45, ha='right')
    plt.tight_layout()
    images.append(fig_to_b64(fig))
else:
    images.append(None)

if not df_week.empty:
    fig, ax = plt.subplots(figsize=(10, 5))
    df_pivot = df_week.pivot(index='week', columns='model_name', values='tokens').fillna(0)
    df_pivot.plot(ax=ax, kind='bar', stacked=True)
    ax.set_title('Tokens by model per week')
    ax.set_ylabel('Tokens')
    ax.yaxis.set_major_formatter(tok_fmt)
    if len(df_pivot) > 30:
        ax.xaxis.set_major_locator(MaxNLocator(nbins=30))
    plt.xticks(rotation=45, ha='right')
    plt.tight_layout()
    images.append(fig_to_b64(fig))
else:
    images.append(None)

if not df_month.empty:
    fig, ax = plt.subplots(figsize=(10, 5))
    df_pivot = df_month.pivot(index='month', columns='model_name', values='tokens').fillna(0)
    df_pivot.plot(ax=ax, kind='bar', stacked=True)
    ax.set_title('Tokens by model per month')
    ax.set_ylabel('Tokens')
    ax.yaxis.set_major_formatter(tok_fmt)
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
       (insert (deterred-format (f-h3 "Tokens by model per day") "\n"))
       (deterred-dashboard-print-images-base64 (elt images 0))
       (insert "\n"))
     (when (elt images 1)
       (insert (deterred-format (f-h3 "Tokens by model per week") "\n"))
       (deterred-dashboard-print-images-base64 (elt images 1))
       (insert "\n"))
     (when (elt images 2)
       (insert (deterred-format (f-h3 "Tokens by model per month") "\n"))
       (deterred-dashboard-print-images-base64 (elt images 2))
       (insert "\n"))))
  ;; Messages charts
  (insert (deterred-format (f-h2 "Messages") "\n"))
  (deterred-dashboard-exec-python
   :python-code
   "from matplotlib import pyplot as plt
from matplotlib.ticker import MaxNLocator
from deterred import fig_to_b64

import pandas as pd
import json

data = json.loads(input())
df_model_week = pd.DataFrame(data['messages-by-model-per-week']['data'])
df_model_month = pd.DataFrame(data['messages-by-model-per-month']['data'])
df_host_day = pd.DataFrame(data['messages-by-hostname-per-day']['data'])
df_host_week = pd.DataFrame(data['messages-by-hostname-per-week']['data'])
df_host_month = pd.DataFrame(data['messages-by-hostname-per-month']['data'])

images = []

if not df_model_week.empty:
    fig, ax = plt.subplots(figsize=(10, 5))
    df_pivot = df_model_week.pivot(index='week', columns='model_name', values='messages').fillna(0)
    df_pivot.plot(ax=ax, kind='bar', stacked=True)
    ax.set_title('Messages by model per week')
    ax.set_ylabel('Messages')
    if len(df_pivot) > 30:
        ax.xaxis.set_major_locator(MaxNLocator(nbins=30))
    plt.xticks(rotation=45, ha='right')
    plt.tight_layout()
    images.append(fig_to_b64(fig))
else:
    images.append(None)

if not df_model_month.empty:
    fig, ax = plt.subplots(figsize=(10, 5))
    df_pivot = df_model_month.pivot(index='month', columns='model_name', values='messages').fillna(0)
    df_pivot.plot(ax=ax, kind='bar', stacked=True)
    ax.set_title('Messages by model per month')
    ax.set_ylabel('Messages')
    plt.xticks(rotation=45, ha='right')
    plt.tight_layout()
    images.append(fig_to_b64(fig))
else:
    images.append(None)

if not df_host_day.empty:
    fig, ax = plt.subplots(figsize=(10, 5))
    df_pivot = df_host_day.pivot(index='day', columns='hostname', values='messages').fillna(0)
    df_pivot.plot(ax=ax, kind='bar', stacked=True)
    ax.set_title('Messages by hostname per day')
    ax.set_ylabel('Messages')
    if len(df_pivot) > 30:
        ax.xaxis.set_major_locator(MaxNLocator(nbins=30))
    plt.xticks(rotation=45, ha='right')
    plt.tight_layout()
    images.append(fig_to_b64(fig))
else:
    images.append(None)

if not df_host_week.empty:
    fig, ax = plt.subplots(figsize=(10, 5))
    df_pivot = df_host_week.pivot(index='week', columns='hostname', values='messages').fillna(0)
    df_pivot.plot(ax=ax, kind='bar', stacked=True)
    ax.set_title('Messages by hostname per week')
    ax.set_ylabel('Messages')
    if len(df_pivot) > 30:
        ax.xaxis.set_major_locator(MaxNLocator(nbins=30))
    plt.xticks(rotation=45, ha='right')
    plt.tight_layout()
    images.append(fig_to_b64(fig))
else:
    images.append(None)

if not df_host_month.empty:
    fig, ax = plt.subplots(figsize=(10, 5))
    df_pivot = df_host_month.pivot(index='month', columns='hostname', values='messages').fillna(0)
    df_pivot.plot(ax=ax, kind='bar', stacked=True)
    ax.set_title('Messages by hostname per month')
    ax.set_ylabel('Messages')
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
       (insert (deterred-format (f-h3 "Messages by model per week") "\n"))
       (deterred-dashboard-print-images-base64 (elt images 0))
       (insert "\n"))
     (when (elt images 1)
       (insert (deterred-format (f-h3 "Messages by model per month") "\n"))
       (deterred-dashboard-print-images-base64 (elt images 1))
       (insert "\n"))
     (when (elt images 2)
       (insert (deterred-format (f-h3 "Messages by hostname per day") "\n"))
       (deterred-dashboard-print-images-base64 (elt images 2))
       (insert "\n"))
     (when (elt images 3)
       (insert (deterred-format (f-h3 "Messages by hostname per week") "\n"))
       (deterred-dashboard-print-images-base64 (elt images 3))
       (insert "\n"))
     (when (elt images 4)
       (insert (deterred-format (f-h3 "Messages by hostname per month") "\n"))
       (deterred-dashboard-print-images-base64 (elt images 4))
       (insert "\n"))))
  ;; Activity charts
  (insert (deterred-format (f-h2 "Activity") "\n"))
  (deterred-dashboard-exec-python
   :python-code
   "from matplotlib import pyplot as plt
from matplotlib.ticker import MaxNLocator
from deterred import fig_to_b64

import pandas as pd
import numpy as np
import json

data = json.loads(input())
df_avg_cost = pd.DataFrame(data['avg-cost-per-message-per-month']['data'])
df_recent = pd.DataFrame(data['recent-messages']['data'])

images = []

# Average cost per message per month (line)
if not df_avg_cost.empty:
    fig, ax = plt.subplots(figsize=(10, 5))
    df_avg_cost.plot(ax=ax, kind='line', x='month', y='avg_cost', legend=False)
    ax.set_title('Average cost per message per month ($)')
    ax.set_ylabel('Avg cost ($)')
    ax.set_xlabel('Month')
    plt.xticks(rotation=45, ha='right')
    plt.tight_layout()
    images.append(fig_to_b64(fig))
else:
    images.append(None)

# Activity by time of day (scatter)
if not df_recent.empty:
    df_recent['dt'] = pd.to_datetime(df_recent['timestamp'], unit='s')
    df_recent['day'] = df_recent['dt'].dt.date.astype(str)
    df_recent['hour'] = df_recent['dt'].dt.hour + df_recent['dt'].dt.minute / 60.0

    all_days = sorted(df_recent['day'].unique())
    day_to_x = {day: i for i, day in enumerate(all_days)}
    df_recent['x'] = df_recent['day'].map(day_to_x)

    models = df_recent['model_name'].unique()
    colors = plt.cm.tab10(np.linspace(0, 1, max(len(models), 1)))
    model_colors = dict(zip(models, colors))

    fig, ax = plt.subplots(figsize=(12, 6))

    for model in models:
        mask = df_recent['model_name'] == model
        subset = df_recent[mask]
        sizes = np.clip(subset['cost'] * 1500, 15, 600)
        ax.scatter(subset['x'], subset['hour'],
                   c=[model_colors[model]], s=sizes,
                   alpha=0.6, label=model, edgecolors='none')

    ax.set_title('Activity by time of day (recent)')
    ax.set_xlabel('Day')
    ax.set_ylabel('Hour of day')
    ax.set_xlim(-0.5, len(all_days) - 0.5)
    ax.set_ylim(0, 24)
    ax.set_xticks(range(len(all_days)))
    ax.set_xticklabels(all_days, rotation=45, ha='right')
    ax.set_yticks(range(0, 25))
    ax.grid(True, alpha=0.3)
    ax.legend(title='Model', fontsize=7)
    plt.tight_layout()
    images.append(fig_to_b64(fig))
else:
    images.append(None)

print(json.dumps(images))"
   :input data
   :on-success
   (lambda (images)
     (when (elt images 0)
       (insert (deterred-format (f-h3 "Average cost per message per month") "\n"))
       (deterred-dashboard-print-images-base64 (elt images 0))
       (insert "\n"))
     (when (elt images 1)
       (insert (deterred-format (f-h3 "Activity by time of day (recent)") "\n"))
       (deterred-dashboard-print-images-base64 (elt images 1))
       (insert "\n"))))
  ;; Top files table
  (insert
   (deterred-format (f-h2 "Top files by AI changes") "\n")
   (deterred-grid-print-with-org
    (alist-get 'data (alist-get 'top-files data))
    :column-names '((file_path . "File") (messages . "Messages")
                    (lines_added . "+ Lines") (lines_removed . "- Lines")
                    (total_changes . "Total"))
    :max-rows 20
    :max-column-width 60
    :grid-button t)
   "\n")
  ;; Top periods tables
  (insert
   (deterred-format (f-h2 "Top periods") "\n"
                    (f-h3 "Top months by cost") "\n")
   (deterred-grid-print-with-org
    (deterred-dashboard-ai--format-tokens-column
     (alist-get 'data (alist-get 'top-months data)))
    :column-names '((month . "Month") (cost . "Cost ($)")
                    (tokens . "Tokens") (messages . "Messages"))
    :max-rows 10
    :grid-button t)
   "\n"
   (deterred-format (f-h3 "Top weeks by cost") "\n")
   (deterred-grid-print-with-org
    (deterred-dashboard-ai--format-tokens-column
     (alist-get 'data (alist-get 'top-weeks data)))
    :column-names '((week . "Week") (cost . "Cost ($)")
                    (tokens . "Tokens") (messages . "Messages"))
    :max-rows 10
    :grid-button t)
   "\n"
   (deterred-format (f-h3 "Top days by cost") "\n")
   (deterred-grid-print-with-org
    (deterred-dashboard-ai--format-tokens-column
     (alist-get 'data (alist-get 'top-days-by-cost data)))
    :column-names '((day . "Day") (cost . "Cost ($)")
                    (tokens . "Tokens") (messages . "Messages"))
    :max-rows 10
    :grid-button t)
   "\n"
   (deterred-format (f-h3 "Top days by tokens") "\n")
   (deterred-grid-print-with-org
    (deterred-dashboard-ai--format-tokens-column
     (alist-get 'data (alist-get 'top-days-by-tokens data)))
    :column-names '((day . "Day") (tokens . "Tokens")
                    (cost . "Cost ($)") (messages . "Messages"))
    :max-rows 10
    :grid-button t)
   "\n"))

(provide 'deterred-dashboard-ai)
;;; deterred-dashboard-ai.el ends here

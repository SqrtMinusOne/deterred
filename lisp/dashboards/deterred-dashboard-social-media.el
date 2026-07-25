;;; deterred-dashboard-social-media.el --- DETERRED social media dashboard -*- lexical-binding: t -*-

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

;; A dashboard for authored social media activity from Reddit, Mastodon,
;; Twitter, VK, and Discord.

;;; Code:

(require 'cl-lib)
(require 'seq)

(require 'deterred-dashboard)
(require 'deterred-db)
(require 'deterred-messengers)
(require 'deterred-utils)

(defconst deterred-dashboard-social-media--activity-types
  '(((platform . "reddit")
     (activity_type . "reddit_post")
     (label . "Reddit posts")
     (lane_order . 0)
     (color . "#ff4500"))
    ((platform . "reddit")
     (activity_type . "reddit_comment")
     (label . "Reddit comments")
     (lane_order . 1)
     (color . "#ff8b60"))
    ((platform . "mastodon")
     (activity_type . "mastodon_post")
     (label . "Mastodon posts")
     (lane_order . 2)
     (color . "#6364ff"))
    ((platform . "twitter")
     (activity_type . "twitter_post")
     (label . "Twitter posts")
     (lane_order . 3)
     (color . "#1d9bf0"))
    ((platform . "vk")
     (activity_type . "vk_post")
     (label . "VK posts")
     (lane_order . 4)
     (color . "#0077ff"))
    ((platform . "discord")
     (activity_type . "discord_message")
     (label . "Discord messages")
     (lane_order . 5)
     (color . "#5865f2")))
  "Metadata for social media activity types.")

(defconst deterred-dashboard-social-media--platforms
  '("reddit" "mastodon" "twitter" "vk" "discord")
  "Platforms included in the social media dashboard.")

(defconst deterred-dashboard-social-media--event-query
  "SELECT *
FROM (
  SELECT
    timestamp,
    'reddit' platform,
    'reddit_post' activity_type,
    subreddit context_id,
    subreddit context,
    NULL replies_count,
    NULL reblogs_count,
    NULL favourites_count
  FROM reddit_post
  UNION ALL
  SELECT
    timestamp,
    'reddit' platform,
    'reddit_comment' activity_type,
    subreddit context_id,
    subreddit context,
    NULL replies_count,
    NULL reblogs_count,
    NULL favourites_count
  FROM reddit_comment
  UNION ALL
  SELECT
    timestamp,
    'mastodon' platform,
    'mastodon_post' activity_type,
    server context_id,
    server context,
    replies_count,
    reblogs_count,
    favourites_count
  FROM mastodon_post
  WHERE is_reply = 0
  UNION ALL
  SELECT
    timestamp,
    'twitter' platform,
    'twitter_post' activity_type,
    NULL context_id,
    NULL context,
    NULL replies_count,
    NULL reblogs_count,
    NULL favourites_count
  FROM twitter_post
  UNION ALL
  SELECT
    timestamp,
    'vk' platform,
    'vk_post' activity_type,
    NULL context_id,
    NULL context,
    NULL replies_count,
    NULL reblogs_count,
    NULL favourites_count
  FROM vk_post
  UNION ALL
  SELECT
    mm.timestamp,
    'discord' platform,
    'discord_message' activity_type,
    mc.id context_id,
    COALESCE(mc.name, '(unnamed chat)') context,
    NULL replies_count,
    NULL reblogs_count,
    NULL favourites_count
  FROM messenger_message mm
  INNER JOIN messenger_chat mc ON mc.id = mm.chat_id
  WHERE mm.messenger = 'discord'
    AND mm.sender_id = :my-id
    AND :include-discord = 1
    [[AND mc.name IN :discord-non-work]]
)
WHERE 1 = 1
  [[AND timestamp >= :start-date]]
  [[AND timestamp <= :end-date]]
  [[AND platform IN :platform]]
ORDER BY timestamp"
  "Query returning the canonical social media event stream.")

(defclass deterred-dashboard-social-media (deterred-dashboard)
  ((name :initform "Social Media"))
  "A DETERRED dashboard for authored social media activity.")

(cl-defmethod deterred-dashboard-default-params
  ((_dashboard deterred-dashboard-social-media))
  "Return default parameters for the social media dashboard."
  '((:start-date)
    (:end-date)
    (:platform)
    (:n-top-contexts . 10)))

(cl-defmethod deterred-dashboard-render-params
  ((_dashboard deterred-dashboard-social-media))
  "Render parameters for the social media dashboard."
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
  (deterred-dashboard-widget-completing-read-multiple
   :name "Platforms"
   :key :platform
   :options deterred-dashboard-social-media--platforms)
  (insert "\n")
  (deterred-dashboard-widget-number
   :name "Top N communities/chats"
   :key :n-top-contexts)
  (insert "\n"))

(cl-defmethod deterred-dashboard-list-datasets
  ((_dashboard deterred-dashboard-social-media))
  "List datasets for the social media dashboard."
  '((numbers-data (name . "Numerical data"))
    (activity-by-type (name . "Activity summary by type"))
    (activity-per-year (name . "Activity per year"))
    (activity-per-month (name . "Activity per month"))
    (usage-bands (name . "Monthly activity usage bands")
                 (tags gantt export))
    (activity-by-weekday-hour
     (name . "Activity by weekday and hour"))
    (top-reddit-subreddits (name . "Top Reddit subreddits"))
    (top-discord-chats (name . "Top Discord chats"))
    (mastodon-engagement-per-year
     (name . "Mastodon engagement per year"))))

(defun deterred-dashboard-social-media--selected-metadata (params)
  "Return activity metadata selected by PARAMS."
  (let ((platforms (alist-get :platform params)))
    (seq-filter
     (lambda (metadata)
       (or (null platforms)
           (member (alist-get 'platform metadata) platforms)))
     deterred-dashboard-social-media--activity-types)))

(defun deterred-dashboard-social-media--fetch-events (db params)
  "Fetch canonical social events from DB according to PARAMS."
  (deterred-db-select-template-alist
   db
   deterred-dashboard-social-media--event-query
   (append
    `((:my-id . ,deterred-messengers-my-id)
      (:include-discord
       . ,(if deterred-messengers-discord-non-work 1 0))
      (:discord-non-work
       . ,deterred-messengers-discord-non-work))
    params)))

(defun deterred-dashboard-social-media--timestamp-parts (timestamp)
  "Return local calendar parts for TIMESTAMP.

The result is (YEAR MONTH DAY WEEKDAY HOUR), where WEEKDAY uses
Emacs' Sunday-as-zero convention."
  (let ((decoded (decode-time timestamp)))
    (list (nth 5 decoded)
          (nth 4 decoded)
          (nth 3 decoded)
          (nth 6 decoded)
          (nth 2 decoded))))

(defun deterred-dashboard-social-media--month-index (timestamp)
  "Return a monotonically increasing local-calendar month for TIMESTAMP."
  (pcase-let ((`(,year ,month . ,_)
               (deterred-dashboard-social-media--timestamp-parts timestamp)))
    (+ (* year 12) (1- month))))

(defun deterred-dashboard-social-media--month-data (month-index)
  "Return display values for MONTH-INDEX.

The result is (MONTH MONTH-START MONTH-END YEAR)."
  (let* ((year (floor month-index 12))
         (month (1+ (% month-index 12)))
         (next-index (1+ month-index))
         (next-year (floor next-index 12))
         (next-month (1+ (% next-index 12))))
    (list (format "%04d-%02d" year month)
          (format "%04d-%02d-01" year month)
          (format "%04d-%02d-01" next-year next-month)
          (number-to-string year))))

(defun deterred-dashboard-social-media--increment (table key &optional amount)
  "Increment KEY in hash TABLE by AMOUNT, defaulting to one."
  (puthash key (+ (gethash key table 0) (or amount 1)) table))

(defun deterred-dashboard-social-media--fraction
    (count total &optional digits)
  "Return COUNT as a percentage of TOTAL.

If DIGITS is non-nil, round the result to that many decimal digits."
  (if (zerop total)
      0.0
    (let ((fraction (* 100.0 (/ (float count) total))))
      (if digits
          (deterred-utils-round-to fraction digits)
        fraction))))

(defun deterred-dashboard-social-media--rank-and-take
    (rows count-key name-key limit)
  "Sort ROWS by COUNT-KEY and NAME-KEY, then take LIMIT rows.

Add a one-based `rank' field to each returned row."
  (let* ((sorted
          (seq-sort
           (lambda (a b)
             (let ((a-count (alist-get count-key a))
                   (b-count (alist-get count-key b)))
               (if (= a-count b-count)
                   (string-lessp (or (alist-get name-key a) "")
                                 (or (alist-get name-key b) ""))
                 (> a-count b-count))))
           rows))
         (limited (seq-take sorted (max 0 limit))))
    (cl-loop for row in limited
             for rank from 1
             collect (append row `((rank . ,rank))))))

(defun deterred-dashboard-social-media--build-datasets (events params)
  "Build all social-media datasets from canonical EVENTS and PARAMS."
  (let* ((metadata
          (deterred-dashboard-social-media--selected-metadata params))
         (top-limit
          (max 0 (truncate (or (alist-get :n-top-contexts params) 10))))
         (type-counts (make-hash-table :test #'equal))
         (type-active-days (make-hash-table :test #'equal))
         (type-first (make-hash-table :test #'equal))
         (type-last (make-hash-table :test #'equal))
         (seen-type-days (make-hash-table :test #'equal))
         (seen-days (make-hash-table :test #'equal))
         (seen-platforms (make-hash-table :test #'equal))
         (month-counts (make-hash-table :test #'equal))
         (year-counts (make-hash-table :test #'equal))
         (weekday-hour-counts (make-hash-table :test #'equal))
         (reddit-contexts (make-hash-table :test #'equal))
         (discord-contexts (make-hash-table :test #'equal))
         (mastodon-engagement (make-hash-table :test #'equal))
         first-timestamp last-timestamp)
    (dolist (event events)
      (let* ((timestamp (alist-get 'timestamp event))
             (platform (alist-get 'platform event))
             (activity-type (alist-get 'activity_type event))
             (parts
              (deterred-dashboard-social-media--timestamp-parts timestamp))
             (year-number (nth 0 parts))
             (month-number (nth 1 parts))
             (day-number (nth 2 parts))
             (weekday (nth 3 parts))
             (hour (nth 4 parts))
             (year (number-to-string year-number))
             (month (format "%04d-%02d" year-number month-number))
             (day (format "%04d-%02d-%02d"
                          year-number month-number day-number))
             (type-day-key (cons activity-type day)))
        (deterred-dashboard-social-media--increment
         type-counts activity-type)
        (deterred-dashboard-social-media--increment
         month-counts (cons activity-type month))
        (deterred-dashboard-social-media--increment
         year-counts (cons activity-type year))
        (deterred-dashboard-social-media--increment
         weekday-hour-counts (list activity-type weekday hour))
        (unless (gethash type-day-key seen-type-days)
          (puthash type-day-key t seen-type-days)
          (deterred-dashboard-social-media--increment
           type-active-days activity-type))
        (puthash day t seen-days)
        (puthash platform t seen-platforms)
        (unless (gethash activity-type type-first)
          (puthash activity-type timestamp type-first))
        (puthash activity-type timestamp type-last)
        (setq first-timestamp
              (if first-timestamp
                  (min first-timestamp timestamp)
                timestamp)
              last-timestamp
              (if last-timestamp
                  (max last-timestamp timestamp)
                timestamp))
        (pcase activity-type
          ((or "reddit_post" "reddit_comment")
           (let* ((context (or (alist-get 'context event) "(unknown)"))
                  (counts (or (gethash context reddit-contexts)
                              (vector 0 0))))
             (if (equal activity-type "reddit_post")
                 (aset counts 0 (1+ (aref counts 0)))
               (aset counts 1 (1+ (aref counts 1))))
             (puthash context counts reddit-contexts)))
          ("discord_message"
           (let* ((context-id (or (alist-get 'context_id event)
                                  (alist-get 'context event)
                                  "(unknown)"))
                  (entry (or (gethash context-id discord-contexts)
                             (cons (or (alist-get 'context event)
                                       "(unnamed chat)")
                                   0))))
             (setcdr entry (1+ (cdr entry)))
             (puthash context-id entry discord-contexts)))
          ("mastodon_post"
           (let* ((entry (or (gethash year mastodon-engagement)
                             (vector 0 0 0 0))))
             (aset entry 0 (1+ (aref entry 0)))
             (aset entry 1 (+ (aref entry 1)
                              (or (alist-get 'favourites_count event) 0)))
             (aset entry 2 (+ (aref entry 2)
                              (or (alist-get 'reblogs_count event) 0)))
             (aset entry 3 (+ (aref entry 3)
                              (or (alist-get 'replies_count event) 0)))
             (puthash year entry mastodon-engagement))))))
    (let* ((total-items (length events))
           (numbers-data
            `(((total_items . ,total-items)
               (active_days . ,(hash-table-count seen-days))
               (active_platforms . ,(hash-table-count seen-platforms))
               (first_timestamp . ,first-timestamp)
               (last_timestamp . ,last-timestamp)
               (first_date
                . ,(when first-timestamp
                     (format-time-string "%Y-%m-%d" first-timestamp)))
               (last_date
                . ,(when last-timestamp
                     (format-time-string "%Y-%m-%d" last-timestamp))))))
           (activity-by-type
            (mapcar
             (lambda (type-metadata)
               (let* ((activity-type
                       (alist-get 'activity_type type-metadata))
                      (count (gethash activity-type type-counts 0))
                      (first (gethash activity-type type-first))
                      (last (gethash activity-type type-last)))
                 (append
                  (copy-tree type-metadata)
                  `((count . ,count)
                    (active_days
                     . ,(gethash activity-type type-active-days 0))
                    (first_timestamp . ,first)
                    (last_timestamp . ,last)
                    (first_date
                     . ,(when first
                          (format-time-string "%Y-%m-%d" first)))
                    (last_date
                     . ,(when last
                          (format-time-string "%Y-%m-%d" last)))
                    (fraction
                     . ,(deterred-dashboard-social-media--fraction
                         count total-items 2))))))
             metadata))
           (start-timestamp
            (or (alist-get :start-date params) first-timestamp))
           (end-timestamp
            (or (alist-get :end-date params) last-timestamp))
           (start-month
            (when start-timestamp
              (deterred-dashboard-social-media--month-index
               start-timestamp)))
           (end-month
            (when end-timestamp
              (deterred-dashboard-social-media--month-index end-timestamp)))
           (peaks (make-hash-table :test #'equal))
           usage-bands activity-per-month activity-per-year
           activity-by-weekday-hour top-reddit-subreddits
           top-discord-chats mastodon-engagement-per-year)
      (maphash
       (lambda (key count)
         (let ((activity-type (car key)))
           (puthash activity-type
                    (max count (gethash activity-type peaks 0))
                    peaks)))
       month-counts)
      (when (and start-month end-month (<= start-month end-month))
        (dolist (type-metadata metadata)
          (let* ((activity-type
                  (alist-get 'activity_type type-metadata))
                 (peak (gethash activity-type peaks 0)))
            (cl-loop for month-index from start-month to end-month
                     do
                     (pcase-let*
                         ((`(,month ,month-start ,month-end ,_)
                           (deterred-dashboard-social-media--month-data
                            month-index))
                          (count
                           (gethash (cons activity-type month)
                                    month-counts 0))
                          (intensity
                           (if (or (zerop count) (zerop peak))
                               0.0
                             (/ (log (1+ count))
                                (log (1+ peak))))))
                       (push
                        (append
                         (copy-tree type-metadata)
                         `((month . ,month)
                           (month_start . ,month-start)
                           (month_end . ,month-end)
                           (count . ,count)
                           (active . ,(if (zerop count) 0 1))
                           (intensity . ,intensity)))
                        usage-bands)))))
        (setq usage-bands (nreverse usage-bands)))
      (setq
       activity-per-month
       (mapcar
        (lambda (row)
          `((month . ,(alist-get 'month row))
            (platform . ,(alist-get 'platform row))
            (activity_type . ,(alist-get 'activity_type row))
            (label . ,(alist-get 'label row))
            (lane_order . ,(alist-get 'lane_order row))
            (color . ,(alist-get 'color row))
            (count . ,(alist-get 'count row))))
        usage-bands))
      (when (and start-month end-month (<= start-month end-month))
        (let ((start-year (floor start-month 12))
              (end-year (floor end-month 12)))
          (dolist (type-metadata metadata)
            (let ((activity-type
                   (alist-get 'activity_type type-metadata)))
              (cl-loop for year-number from start-year to end-year
                       for year = (number-to-string year-number)
                       do
                       (push
                        (append
                         (copy-tree type-metadata)
                         `((year . ,year)
                           (count
                            . ,(gethash (cons activity-type year)
                                       year-counts 0))))
                        activity-per-year)))))
        (setq activity-per-year
              (seq-sort
               (lambda (a b)
                 (let ((a-year (alist-get 'year a))
                       (b-year (alist-get 'year b)))
                   (if (equal a-year b-year)
                       (< (alist-get 'lane_order a)
                          (alist-get 'lane_order b))
                     (string-lessp a-year b-year))))
               activity-per-year)))
      (dolist (type-metadata metadata)
        (let* ((activity-type (alist-get 'activity_type type-metadata))
               (type-total (gethash activity-type type-counts 0)))
          (cl-loop for weekday-order from 0 to 6
                   for weekday = (% (1+ weekday-order) 7)
                   for weekday-label =
                   (aref ["Sunday" "Monday" "Tuesday" "Wednesday"
                          "Thursday" "Friday" "Saturday"]
                         weekday)
                   do
                   (cl-loop for hour from 0 to 23
                            for count =
                            (gethash
                             (list activity-type weekday hour)
                             weekday-hour-counts 0)
                            do
                            (push
                             (append
                              (copy-tree type-metadata)
                              `((weekday . ,weekday)
                                (weekday_order . ,weekday-order)
                                (weekday_label . ,weekday-label)
                                (hour . ,hour)
                                (count . ,count)
                                (fraction
                                 . ,(deterred-dashboard-social-media--fraction
                                     count type-total))))
                             activity-by-weekday-hour)))))
      (setq activity-by-weekday-hour
            (nreverse activity-by-weekday-hour))
      (maphash
       (lambda (subreddit counts)
         (let ((post-count (aref counts 0))
               (comment-count (aref counts 1)))
           (push `((subreddit . ,subreddit)
                   (post_count . ,post-count)
                   (comment_count . ,comment-count)
                   (total_count . ,(+ post-count comment-count)))
                 top-reddit-subreddits)))
       reddit-contexts)
      (setq top-reddit-subreddits
            (deterred-dashboard-social-media--rank-and-take
             top-reddit-subreddits 'total_count 'subreddit top-limit))
      (maphash
       (lambda (chat-id entry)
         (push `((chat_id . ,chat-id)
                 (chat . ,(car entry))
                 (count . ,(cdr entry)))
               top-discord-chats))
       discord-contexts)
      (setq top-discord-chats
            (deterred-dashboard-social-media--rank-and-take
             top-discord-chats 'count 'chat top-limit))
      (maphash
       (lambda (year entry)
         (push `((year . ,year)
                 (posts . ,(aref entry 0))
                 (favourites . ,(aref entry 1))
                 (reblogs . ,(aref entry 2))
                 (replies . ,(aref entry 3)))
               mastodon-engagement-per-year))
       mastodon-engagement)
      (setq mastodon-engagement-per-year
            (seq-sort-by
             (lambda (row) (alist-get 'year row))
             #'string-lessp
             mastodon-engagement-per-year))
      `((numbers-data . ,numbers-data)
        (activity-by-type . ,activity-by-type)
        (activity-per-year . ,activity-per-year)
        (activity-per-month . ,activity-per-month)
        (usage-bands . ,usage-bands)
        (activity-by-weekday-hour . ,activity-by-weekday-hour)
        (top-reddit-subreddits . ,top-reddit-subreddits)
        (top-discord-chats . ,top-discord-chats)
        (mastodon-engagement-per-year
         . ,mastodon-engagement-per-year)))))

(cl-defmethod deterred-dashboard-fetch-datasets
  ((_dashboard deterred-dashboard-social-media) params)
  "Fetch social media datasets according to PARAMS."
  (let* ((db (deterred-db--init))
         (events
          (deterred-dashboard-social-media--fetch-events db params)))
    (deterred-dashboard-social-media--build-datasets events params)))

(defun deterred-dashboard-social-media--print-chart (images index title)
  "Print chart INDEX from IMAGES under TITLE when present."
  (when-let ((image (elt images index)))
    (insert (deterred-format (f-h3 title) "\n"))
    (deterred-dashboard-print-images-base64 image)
    (insert "\n")))

(cl-defmethod deterred-dashboard-render-results
  ((_dashboard deterred-dashboard-social-media) _params data)
  "Render social media dashboard DATA."
  (let* ((numbers
          (car (alist-get 'data (alist-get 'numbers-data data))))
         (total (or (alist-get 'total_items numbers) 0))
         (active-days (or (alist-get 'active_days numbers) 0))
         (active-platforms (or (alist-get 'active_platforms numbers) 0))
         (first-date (alist-get 'first_date numbers))
         (last-date (alist-get 'last_date numbers))
         (activity-by-type
          (mapcar
           (lambda (row)
             `((label . ,(alist-get 'label row))
               (count . ,(alist-get 'count row))
               (active_days . ,(alist-get 'active_days row))
               (first_date . ,(alist-get 'first_date row))
               (last_date . ,(alist-get 'last_date row))
               (fraction . ,(alist-get 'fraction row))))
           (alist-get 'data (alist-get 'activity-by-type data)))))
    (insert
     (deterred-format
      "Recorded "
      (f-ace (f (f-num total)) 'bold)
      " authored items on "
      (f-ace (f (f-num active-days)) 'bold)
      " active days across "
      (f-ace (f (f-num active-platforms)) 'bold)
      " platforms"
      (when (and first-date last-date)
        (f ", from " (f-ace first-date 'deterred-faces-date)
           " through " (f-ace last-date 'deterred-faces-date)))
     ".\n\n"
      (f-h2 "Summary") "\n")
     (deterred-grid-print-with-org
      activity-by-type
      :column-names
      '((label . "Activity")
        (count . "Items")
        (active_days . "Active days")
        (first_date . "First")
        (last_date . "Last")
        (fraction . "%"))
      :max-column-width 24
      :grid-button t)
     "\n"
     (deterred-format (f-h2 "Dynamics over time") "\n")))
  (deterred-dashboard-exec-python
   :python-code
   "from matplotlib import pyplot as plt
from matplotlib import dates as mdates
from matplotlib.patches import Rectangle
from matplotlib.ticker import MaxNLocator
from deterred import fig_to_b64

import json
import math
import numpy as np
import pandas as pd
import warnings

warnings.filterwarnings(
    'ignore',
    message='This figure includes Axes that are not compatible with tight_layout.*')

data = json.loads(input())

def frame(key):
    return pd.DataFrame((data.get(key) or {}).get('data') or [])

def metadata(df):
    if df.empty:
        return pd.DataFrame()
    return (df[['activity_type', 'label', 'lane_order', 'color']]
            .drop_duplicates()
            .sort_values('lane_order'))

def save(fig):
    image = fig_to_b64(fig)
    plt.close(fig)
    return image

images = [None] * 7

df_year = frame('activity-per-year')
if not df_year.empty and df_year['count'].sum() > 0:
    meta = metadata(df_year)
    years = sorted(df_year['year'].unique())
    fig, ax = plt.subplots(figsize=(11, 5))
    bottom = np.zeros(len(years))
    for _, lane in meta.iterrows():
        subset = (df_year[df_year['activity_type'] == lane['activity_type']]
                  .set_index('year')
                  .reindex(years))
        values = subset['count'].fillna(0).to_numpy()
        ax.bar(years, values, bottom=bottom, label=lane['label'],
               color=lane['color'])
        bottom += values
    ax.set_title('Authored social media activity per year')
    ax.set_xlabel('Year')
    ax.set_ylabel('Items')
    ax.legend(fontsize=8, ncol=3)
    ax.tick_params(axis='x', rotation=45)
    images[0] = save(fig)

df_month = frame('activity-per-month')
if not df_month.empty and df_month['count'].sum() > 0:
    meta = metadata(df_month)
    n_lanes = len(meta)
    n_cols = 2
    n_rows = math.ceil(n_lanes / n_cols)
    fig, axes = plt.subplots(n_rows, n_cols,
                             figsize=(12, max(3, n_rows * 2.7)),
                             squeeze=False)
    months = sorted(df_month['month'].unique())
    x = pd.to_datetime(months)
    for ax, (_, lane) in zip(axes.flat, meta.iterrows()):
        subset = (df_month[df_month['activity_type'] == lane['activity_type']]
                  .set_index('month')
                  .reindex(months))
        values = subset['count'].fillna(0).to_numpy()
        ax.plot(x, values, color=lane['color'], linewidth=1.4)
        ax.fill_between(x, values, color=lane['color'], alpha=0.2)
        ax.set_title(lane['label'])
        ax.set_ylabel('Items')
        ax.grid(True, alpha=0.25)
        locator = mdates.AutoDateLocator(minticks=3, maxticks=8)
        ax.xaxis.set_major_locator(locator)
        ax.xaxis.set_major_formatter(mdates.ConciseDateFormatter(locator))
    for ax in axes.flat[n_lanes:]:
        ax.set_visible(False)
    fig.suptitle('Monthly activity by type')
    images[1] = save(fig)

df_bands = frame('usage-bands')
if not df_bands.empty:
    meta = metadata(df_bands)
    active = df_bands[df_bands['active'] == 1]
    if not active.empty:
        fig, ax = plt.subplots(figsize=(14, max(3.5, len(meta) * 0.8)))
        lane_positions = {
            lane['activity_type']: index
            for index, (_, lane) in enumerate(meta.iterrows())
        }
        for _, row in active.iterrows():
            start = mdates.date2num(pd.Timestamp(row['month_start']))
            end = mdates.date2num(pd.Timestamp(row['month_end']))
            alpha = 0.18 + 0.82 * float(row['intensity'])
            ax.add_patch(Rectangle(
                (start, lane_positions[row['activity_type']] - 0.33),
                end - start, 0.66,
                facecolor=row['color'], alpha=alpha,
                linewidth=0, antialiased=False))
        range_start = mdates.date2num(pd.Timestamp(df_bands['month_start'].min()))
        range_end = mdates.date2num(pd.Timestamp(df_bands['month_end'].max()))
        ax.set_xlim(range_start, range_end)
        ax.set_ylim(-0.6, len(meta) - 0.4)
        ax.set_yticks(range(len(meta)))
        ax.set_yticklabels(meta['label'])
        ax.invert_yaxis()
        locator = mdates.AutoDateLocator(minticks=5, maxticks=16)
        ax.xaxis.set_major_locator(locator)
        ax.xaxis.set_major_formatter(mdates.ConciseDateFormatter(locator))
        ax.grid(axis='x', alpha=0.25)
        ax.set_title('Monthly usage bands')
        images[2] = save(fig)

df_heat = frame('activity-by-weekday-hour')
if not df_heat.empty and df_heat['count'].sum() > 0:
    meta = metadata(df_heat)
    n_lanes = len(meta)
    n_cols = 2
    n_rows = math.ceil(n_lanes / n_cols)
    fig, axes = plt.subplots(n_rows, n_cols,
                             figsize=(13, max(4, n_rows * 3.2)),
                             squeeze=False)
    vmax = max(float(df_heat['fraction'].max()), 0.01)
    image = None
    for ax, (_, lane) in zip(axes.flat, meta.iterrows()):
        subset = df_heat[df_heat['activity_type'] == lane['activity_type']]
        matrix = (subset.pivot(index='weekday_order',
                               columns='hour',
                               values='fraction')
                  .reindex(index=range(7), columns=range(24), fill_value=0))
        image = ax.imshow(matrix.to_numpy(), aspect='auto', origin='upper',
                          cmap='viridis', vmin=0, vmax=vmax)
        ax.set_title(lane['label'])
        ax.set_yticks(range(7))
        ax.set_yticklabels(['Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun'])
        ax.set_xticks(range(0, 24, 3))
        ax.set_xlabel('Local hour')
    for ax in axes.flat[n_lanes:]:
        ax.set_visible(False)
    if image is not None:
        fig.colorbar(image, ax=list(axes.flat[:n_lanes]),
                     label='% of activity-type items', shrink=0.75)
    fig.suptitle('Within-type activity distribution by weekday and hour')
    images[3] = save(fig)

df_reddit = frame('top-reddit-subreddits')
if not df_reddit.empty:
    plot = df_reddit.sort_values('rank', ascending=False)
    fig, ax = plt.subplots(figsize=(10, max(4, len(plot) * 0.45)))
    ax.barh(plot['subreddit'], plot['post_count'],
            color='#ff4500', label='Posts')
    ax.barh(plot['subreddit'], plot['comment_count'],
            left=plot['post_count'], color='#ff8b60', label='Comments')
    ax.set_title('Top Reddit subreddits')
    ax.set_xlabel('Items')
    ax.legend()
    images[4] = save(fig)

df_discord = frame('top-discord-chats')
if not df_discord.empty:
    plot = df_discord.sort_values('rank', ascending=False)
    fig, ax = plt.subplots(figsize=(10, max(4, len(plot) * 0.45)))
    ax.barh(plot['chat'], plot['count'], color='#5865f2')
    ax.set_title('Top Discord chats')
    ax.set_xlabel('Messages')
    images[5] = save(fig)

df_mastodon = frame('mastodon-engagement-per-year')
if not df_mastodon.empty:
    plot = df_mastodon.set_index('year')
    fig, ax = plt.subplots(figsize=(10, 5))
    plot[['favourites', 'reblogs', 'replies']].plot(
        ax=ax, kind='bar', color=['#f6c177', '#6364ff', '#eb6f92'])
    ax.set_title('Engagement received by Mastodon posts')
    ax.set_xlabel('Year')
    ax.set_ylabel('Interactions')
    ax.legend(['Favourites', 'Boosts', 'Replies'])
    images[6] = save(fig)

print(json.dumps(images))"
   :input data
   :on-success
   (lambda (images)
     (deterred-dashboard-social-media--print-chart
      images 0 "Activity per year")
     (deterred-dashboard-social-media--print-chart
      images 1 "Activity per month")
     (insert (deterred-format (f-h2 "Usage history") "\n"))
     (deterred-dashboard-social-media--print-chart
      images 2 "Monthly usage bands")
     (insert (deterred-format (f-h2 "Activity rhythm") "\n"))
     (deterred-dashboard-social-media--print-chart
      images 3 "Activity by weekday and hour")
     (insert (deterred-format (f-h2 "Top communities") "\n"))
     (deterred-dashboard-social-media--print-chart
      images 4 "Reddit subreddits")
     (deterred-dashboard-social-media--print-chart
      images 5 "Discord chats")
     (insert (deterred-format (f-h2 "Mastodon engagement") "\n"))
     (deterred-dashboard-social-media--print-chart
      images 6 "Engagement received per year")))
  (insert
   (deterred-format (f-h2 "Top communities and chats") "\n"
                    (f-h3 "Reddit subreddits") "\n")
   (deterred-grid-print-with-org
    (mapcar
     (lambda (row)
       `((rank . ,(alist-get 'rank row))
         (subreddit . ,(alist-get 'subreddit row))
         (post_count . ,(alist-get 'post_count row))
         (comment_count . ,(alist-get 'comment_count row))
         (total_count . ,(alist-get 'total_count row))))
     (alist-get 'data (alist-get 'top-reddit-subreddits data)))
    :column-names
    '((rank . "#")
      (subreddit . "Subreddit")
      (post_count . "Posts")
      (comment_count . "Comments")
      (total_count . "Total"))
    :max-rows 20
    :max-column-width 35
    :grid-button t)
   "\n"
   (deterred-format (f-h3 "Discord chats") "\n")
   (deterred-grid-print-with-org
    (mapcar
     (lambda (row)
       `((rank . ,(alist-get 'rank row))
         (chat . ,(alist-get 'chat row))
         (count . ,(alist-get 'count row))))
     (alist-get 'data (alist-get 'top-discord-chats data)))
    :column-names
    '((rank . "#") (chat . "Chat") (count . "Messages"))
    :max-rows 20
    :max-column-width 45
    :grid-button t)
   "\n"))

(provide 'deterred-dashboard-social-media)
;;; deterred-dashboard-social-media.el ends here

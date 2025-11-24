;;; deterred-dashboard-messengers.el --- DETERRED dashboard for Messengers -*- lexical-binding: t -*-

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

;; A dashboard for Messengers, corresponding to `deterred-messengers'.

;;; Code:
(require 'deterred-dashboard)
(require 'deterred-db)
(require 'deterred-utils)
(require 'deterred-messengers)

(defclass deterred-dashboard-messengers (deterred-dashboard)
  ((name :initform "Messengers"))
  "A DETERRED dashboard for Messengers.")

(cl-defmethod deterred-dashboard-default-params ((_dashboard deterred-dashboard-messengers))
  "Default parameters for the Messengers dashboard."
  '((:start-date)
    (:end-date)
    (:chat-type)
    (:chat-name)
    (:messenger)
    (:n-top-chats . 5)))

(cl-defmethod deterred-dashboard-render-params ((_dashboard deterred-dashboard-messengers))
  "Render the parameters section for the Messengers dashboard."
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
         (chat-types '("personal_chat" "group"))
         (chat-names
          (mapcar
           (lambda (item) (alist-get 'name item))
           (deterred-db-select-alist
            db "SELECT DISTINCT name FROM messenger_chat
WHERE name IS NOT NULL
ORDER BY name")))
         (messengers (copy-sequence deterred-messengers-messenger-list)))
    (deterred-dashboard-widget-completing-read-multiple
     :name "Chat type"
     :key :chat-type
     :options chat-types)
    (insert "\n")
    (deterred-dashboard-widget-completing-read-multiple
     :name "Chat name"
     :key :chat-name
     :options chat-names)
    (insert "\n")
    (deterred-dashboard-widget-completing-read-multiple
     :name "Messenger"
     :key :messenger
     :options messengers))
  (insert "\n")
  (deterred-dashboard-widget-number
   :name "Top N chats"
   :key :n-top-chats)
  (insert "\n"))

(cl-defmethod deterred-dashboard-list-datasets ((_dashboard deterred-dashboard-messengers))
  "List datasets for the Messengers dashboard."
  '((sent-received-per-year (name . "Messages sent/received per year"))
    (personal-sent-received-per-year (name . "Personal messages sent/received per year"))
    (group-sent-received-per-year (name . "Group messages sent/received per year"))
    (sent-received-per-month (name . "Messages sent/received per month"))
    (personal-sent-received-per-month (name . "Personal messages sent/received per month"))
    (group-sent-received-per-month (name . "Group messages sent/received per month"))
    (top-personal-chats (name . "Top personal chats"))
    (top-group-chats (name . "Top group chats"))
    (top-group-chat-users (name . "Top users in group chats"))
    (top-chats-per-month (name . "Top N chats per month"))
    (top-messengers (name . "Top messengers"))
    (messenger-per-year (name . "Messages per messenger per year"))
    (top-days (name . "Top days by sent messages"))
    (top-weeks (name . "Top weeks by sent messages"))
    (top-months (name . "Top months by sent messages"))
    (numbers-data (name . "Numerical data"))))

(cl-defmethod deterred-dashboard-fetch-datasets ((_dashboard deterred-dashboard-messengers)
                                                 params)
  "Fetch datasets for the Messengers dashboard.

PARAMS is as returned by `deterred-dashboard-default-params'."
  (let* ((db (deterred-db--init))
         (my-id deterred-messengers-my-id)
         (numbers-data
          (deterred-db-select-template-alist
           db
           "SELECT (
  SELECT count(*)
  FROM messenger_message mm
  INNER JOIN messenger_chat mc ON mc.id = mm.chat_id
  WHERE sender_id = :my-id
    [[AND date(mm.timestamp, 'unixepoch') >= date(:start-date, 'unixepoch')]]
    [[AND date(mm.timestamp, 'unixepoch') <= date(:end-date, 'unixepoch')]]
    [[AND mc.type IN :chat-type]]
    [[AND mc.name IN :chat-name]]
    [[AND mm.messenger IN :messenger]]
) AS sent_count,
(
  SELECT count(*)
  FROM messenger_message mm
  INNER JOIN messenger_chat mc ON mc.id = mm.chat_id
  WHERE sender_id != :my-id
    [[AND date(mm.timestamp, 'unixepoch') >= date(:start-date, 'unixepoch')]]
    [[AND date(mm.timestamp, 'unixepoch') <= date(:end-date, 'unixepoch')]]
    [[AND mc.type IN :chat-type]]
    [[AND mc.name IN :chat-name]]
    [[AND mm.messenger IN :messenger]]
) AS received_count,
(
  SELECT count(DISTINCT mc.id)
  FROM messenger_message mm
  INNER JOIN messenger_chat mc ON mc.id = mm.chat_id
  WHERE 1 = 1
    [[AND date(mm.timestamp, 'unixepoch') >= date(:start-date, 'unixepoch')]]
    [[AND date(mm.timestamp, 'unixepoch') <= date(:end-date, 'unixepoch')]]
    [[AND mc.type IN :chat-type]]
    [[AND mc.name IN :chat-name]]
    [[AND mm.messenger IN :messenger]]
) AS chat_count;"
           (append params `((:my-id . ,my-id))))))
    `((sent-received-per-year
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y', mm.timestamp, 'unixepoch') year,
  sum(CASE WHEN mm.sender_id = :my-id THEN 1 ELSE 0 END) sent,
  sum(CASE WHEN mm.sender_id != :my-id THEN 1 ELSE 0 END) received
FROM messenger_message mm
INNER JOIN messenger_chat mc ON mc.id = mm.chat_id
WHERE 1 = 1
  [[AND date(mm.timestamp, 'unixepoch') >= date(:start-date, 'unixepoch')]]
  [[AND date(mm.timestamp, 'unixepoch') <= date(:end-date, 'unixepoch')]]
  [[AND mc.type IN :chat-type]]
  [[AND mc.name IN :chat-name]]
  [[AND mm.messenger IN :messenger]]
GROUP BY strftime('%Y', mm.timestamp, 'unixepoch')"
           (append params `((:my-id . ,my-id)))))
      (personal-sent-received-per-year
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y', mm.timestamp, 'unixepoch') year,
  sum(CASE WHEN mm.sender_id = :my-id THEN 1 ELSE 0 END) sent,
  sum(CASE WHEN mm.sender_id != :my-id THEN 1 ELSE 0 END) received
FROM messenger_message mm
INNER JOIN messenger_chat mc ON mc.id = mm.chat_id
WHERE mc.type = 'personal_chat'
  [[AND date(mm.timestamp, 'unixepoch') >= date(:start-date, 'unixepoch')]]
  [[AND date(mm.timestamp, 'unixepoch') <= date(:end-date, 'unixepoch')]]
  [[AND mc.name IN :chat-name]]
  [[AND mm.messenger IN :messenger]]
GROUP BY strftime('%Y', mm.timestamp, 'unixepoch')"
           (append params `((:my-id . ,my-id)))))
      (group-sent-received-per-year
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y', mm.timestamp, 'unixepoch') year,
  sum(CASE WHEN mm.sender_id = :my-id THEN 1 ELSE 0 END) sent,
  sum(CASE WHEN mm.sender_id != :my-id THEN 1 ELSE 0 END) received
FROM messenger_message mm
INNER JOIN messenger_chat mc ON mc.id = mm.chat_id
WHERE mc.type = 'group'
  [[AND date(mm.timestamp, 'unixepoch') >= date(:start-date, 'unixepoch')]]
  [[AND date(mm.timestamp, 'unixepoch') <= date(:end-date, 'unixepoch')]]
  [[AND mc.name IN :chat-name]]
  [[AND mm.messenger IN :messenger]]
GROUP BY strftime('%Y', mm.timestamp, 'unixepoch')"
           (append params `((:my-id . ,my-id)))))
      (sent-received-per-month
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y-%m', mm.timestamp, 'unixepoch') month,
  sum(CASE WHEN mm.sender_id = :my-id THEN 1 ELSE 0 END) sent,
  sum(CASE WHEN mm.sender_id != :my-id THEN 1 ELSE 0 END) received
FROM messenger_message mm
INNER JOIN messenger_chat mc ON mc.id = mm.chat_id
WHERE 1 = 1
  [[AND date(mm.timestamp, 'unixepoch') >= date(:start-date, 'unixepoch')]]
  [[AND date(mm.timestamp, 'unixepoch') <= date(:end-date, 'unixepoch')]]
  [[AND mc.type IN :chat-type]]
  [[AND mc.name IN :chat-name]]
  [[AND mm.messenger IN :messenger]]
GROUP BY strftime('%Y-%m', mm.timestamp, 'unixepoch')"
           (append params `((:my-id . ,my-id)))))
      (personal-sent-received-per-month
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y-%m', mm.timestamp, 'unixepoch') month,
  sum(CASE WHEN mm.sender_id = :my-id THEN 1 ELSE 0 END) sent,
  sum(CASE WHEN mm.sender_id != :my-id THEN 1 ELSE 0 END) received
FROM messenger_message mm
INNER JOIN messenger_chat mc ON mc.id = mm.chat_id
WHERE mc.type = 'personal_chat'
  [[AND date(mm.timestamp, 'unixepoch') >= date(:start-date, 'unixepoch')]]
  [[AND date(mm.timestamp, 'unixepoch') <= date(:end-date, 'unixepoch')]]
  [[AND mc.name IN :chat-name]]
  [[AND mm.messenger IN :messenger]]
GROUP BY strftime('%Y-%m', mm.timestamp, 'unixepoch')"
           (append params `((:my-id . ,my-id)))))
      (group-sent-received-per-month
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y-%m', mm.timestamp, 'unixepoch') month,
  sum(CASE WHEN mm.sender_id = :my-id THEN 1 ELSE 0 END) sent,
  sum(CASE WHEN mm.sender_id != :my-id THEN 1 ELSE 0 END) received
FROM messenger_message mm
INNER JOIN messenger_chat mc ON mc.id = mm.chat_id
WHERE mc.type = 'group'
  [[AND date(mm.timestamp, 'unixepoch') >= date(:start-date, 'unixepoch')]]
  [[AND date(mm.timestamp, 'unixepoch') <= date(:end-date, 'unixepoch')]]
  [[AND mc.name IN :chat-name]]
  [[AND mm.messenger IN :messenger]]
GROUP BY strftime('%Y-%m', mm.timestamp, 'unixepoch')"
           (append params `((:my-id . ,my-id)))))
      (top-personal-chats
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  mc.name,
  sum(CASE WHEN mm.sender_id = :my-id THEN 1 ELSE 0 END) sent,
  sum(CASE WHEN mm.sender_id != :my-id THEN 1 ELSE 0 END) received,
  count(*) total
FROM messenger_message mm
INNER JOIN messenger_chat mc ON mc.id = mm.chat_id
WHERE mc.type = 'personal_chat'
  [[AND date(mm.timestamp, 'unixepoch') >= date(:start-date, 'unixepoch')]]
  [[AND date(mm.timestamp, 'unixepoch') <= date(:end-date, 'unixepoch')]]
  [[AND mc.name IN :chat-name]]
  [[AND mm.messenger IN :messenger]]
GROUP BY mc.name
ORDER BY total DESC
LIMIT 20"
           (append params `((:my-id . ,my-id)))))
      (top-group-chats
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  mc.name,
  sum(CASE WHEN mm.sender_id = :my-id THEN 1 ELSE 0 END) sent,
  sum(CASE WHEN mm.sender_id != :my-id THEN 1 ELSE 0 END) received,
  count(*) total
FROM messenger_message mm
INNER JOIN messenger_chat mc ON mc.id = mm.chat_id
WHERE mc.type = 'group'
  [[AND date(mm.timestamp, 'unixepoch') >= date(:start-date, 'unixepoch')]]
  [[AND date(mm.timestamp, 'unixepoch') <= date(:end-date, 'unixepoch')]]
  [[AND mc.name IN :chat-name]]
  [[AND mm.messenger IN :messenger]]
GROUP BY mc.name
ORDER BY total DESC
LIMIT 20"
           (append params `((:my-id . ,my-id)))))
      (top-group-chat-users
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  mu.name,
  count(*) total
FROM messenger_message mm
INNER JOIN messenger_chat mc ON mc.id = mm.chat_id
INNER JOIN messenger_user mu ON mu.id = mm.sender_id
WHERE mc.type = 'group'
  AND mm.sender_id != :my-id
  [[AND date(mm.timestamp, 'unixepoch') >= date(:start-date, 'unixepoch')]]
  [[AND date(mm.timestamp, 'unixepoch') <= date(:end-date, 'unixepoch')]]
  [[AND mc.name IN :chat-name]]
  [[AND mm.messenger IN :messenger]]
GROUP BY mu.name
ORDER BY total DESC
LIMIT 20"
           (append params `((:my-id . ,my-id)))))
      (top-chats-per-month
       . ,(deterred-db-select-template-alist
           db
           "WITH top_chats AS (
  SELECT
    mc.name,
    count(*) total
  FROM messenger_message mm
  INNER JOIN messenger_chat mc ON mc.id = mm.chat_id
  WHERE mc.type = 'personal_chat'
    [[AND date(mm.timestamp, 'unixepoch') >= date(:start-date, 'unixepoch')]]
    [[AND date(mm.timestamp, 'unixepoch') <= date(:end-date, 'unixepoch')]]
    [[AND mc.name IN :chat-name]]
    [[AND mm.messenger IN :messenger]]
  GROUP BY mc.name
  ORDER BY total DESC
  LIMIT :n-top-chats
)
SELECT
  strftime('%Y-%m', mm.timestamp, 'unixepoch') month,
  mc.name,
  sum(CASE WHEN mm.sender_id = :my-id THEN 1 ELSE 0 END) sent,
  sum(CASE WHEN mm.sender_id != :my-id THEN 1 ELSE 0 END) received
FROM messenger_message mm
INNER JOIN messenger_chat mc ON mc.id = mm.chat_id
WHERE mc.name IN (SELECT tc.name FROM top_chats tc)
  [[AND date(mm.timestamp, 'unixepoch') >= date(:start-date, 'unixepoch')]]
  [[AND date(mm.timestamp, 'unixepoch') <= date(:end-date, 'unixepoch')]]
  [[AND mm.messenger IN :messenger]]
GROUP BY strftime('%Y-%m', mm.timestamp, 'unixepoch'), mc.name
ORDER BY month ASC"
           (append params `((:my-id . ,my-id)))))
      (top-messengers
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  mm.messenger,
  sum(CASE WHEN mm.sender_id = :my-id THEN 1 ELSE 0 END) sent,
  sum(CASE WHEN mm.sender_id != :my-id THEN 1 ELSE 0 END) received,
  count(*) total
FROM messenger_message mm
INNER JOIN messenger_chat mc ON mc.id = mm.chat_id
WHERE 1 = 1
  [[AND date(mm.timestamp, 'unixepoch') >= date(:start-date, 'unixepoch')]]
  [[AND date(mm.timestamp, 'unixepoch') <= date(:end-date, 'unixepoch')]]
  [[AND mc.type IN :chat-type]]
  [[AND mc.name IN :chat-name]]
  [[AND mm.messenger IN :messenger]]
GROUP BY mm.messenger
ORDER BY total DESC"
           (append params `((:my-id . ,my-id)))))
      (messenger-per-year
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y', mm.timestamp, 'unixepoch') year,
  mm.messenger,
  sum(CASE WHEN mm.sender_id = :my-id THEN 1 ELSE 0 END) sent,
  sum(CASE WHEN mm.sender_id != :my-id THEN 1 ELSE 0 END) received
FROM messenger_message mm
INNER JOIN messenger_chat mc ON mc.id = mm.chat_id
WHERE 1 = 1
  [[AND date(mm.timestamp, 'unixepoch') >= date(:start-date, 'unixepoch')]]
  [[AND date(mm.timestamp, 'unixepoch') <= date(:end-date, 'unixepoch')]]
  [[AND mc.type IN :chat-type]]
  [[AND mc.name IN :chat-name]]
  [[AND mm.messenger IN :messenger]]
GROUP BY strftime('%Y', mm.timestamp, 'unixepoch'), mm.messenger
ORDER BY year ASC"
           (append params `((:my-id . ,my-id)))))
      (top-days
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  date(mm.timestamp, 'unixepoch') day,
  sum(CASE WHEN mm.sender_id = :my-id THEN 1 ELSE 0 END) sent,
  sum(CASE WHEN mm.sender_id != :my-id THEN 1 ELSE 0 END) received
FROM messenger_message mm
INNER JOIN messenger_chat mc ON mc.id = mm.chat_id
WHERE 1 = 1
  [[AND date(mm.timestamp, 'unixepoch') >= date(:start-date, 'unixepoch')]]
  [[AND date(mm.timestamp, 'unixepoch') <= date(:end-date, 'unixepoch')]]
  [[AND mc.type IN :chat-type]]
  [[AND mc.name IN :chat-name]]
  [[AND mm.messenger IN :messenger]]
GROUP BY date(mm.timestamp, 'unixepoch')
ORDER BY sent DESC
LIMIT 20"
           (append params `((:my-id . ,my-id)))))
      (top-weeks
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y-%W', mm.timestamp, 'unixepoch') week,
  sum(CASE WHEN mm.sender_id = :my-id THEN 1 ELSE 0 END) sent,
  sum(CASE WHEN mm.sender_id != :my-id THEN 1 ELSE 0 END) received
FROM messenger_message mm
INNER JOIN messenger_chat mc ON mc.id = mm.chat_id
WHERE 1 = 1
  [[AND date(mm.timestamp, 'unixepoch') >= date(:start-date, 'unixepoch')]]
  [[AND date(mm.timestamp, 'unixepoch') <= date(:end-date, 'unixepoch')]]
  [[AND mc.type IN :chat-type]]
  [[AND mc.name IN :chat-name]]
  [[AND mm.messenger IN :messenger]]
GROUP BY strftime('%Y-%W', mm.timestamp, 'unixepoch')
ORDER BY sent DESC
LIMIT 20"
           (append params `((:my-id . ,my-id)))))
      (top-months
       . ,(deterred-db-select-template-alist
           db
           "SELECT
  strftime('%Y-%m', mm.timestamp, 'unixepoch') month,
  sum(CASE WHEN mm.sender_id = :my-id THEN 1 ELSE 0 END) sent,
  sum(CASE WHEN mm.sender_id != :my-id THEN 1 ELSE 0 END) received
FROM messenger_message mm
INNER JOIN messenger_chat mc ON mc.id = mm.chat_id
WHERE 1 = 1
  [[AND date(mm.timestamp, 'unixepoch') >= date(:start-date, 'unixepoch')]]
  [[AND date(mm.timestamp, 'unixepoch') <= date(:end-date, 'unixepoch')]]
  [[AND mc.type IN :chat-type]]
  [[AND mc.name IN :chat-name]]
  [[AND mm.messenger IN :messenger]]
GROUP BY strftime('%Y-%m', mm.timestamp, 'unixepoch')
ORDER BY sent DESC
LIMIT 20"
           (append params `((:my-id . ,my-id)))))
      (numbers-data . ,numbers-data))))

(cl-defmethod deterred-dashboard-render-results ((_dashboard deterred-dashboard-messengers)
                                                 _params data)
  "Render DATA for the Messengers dashboard."
  (insert
   (deterred-format
    "I've sent "
    (f-ace (f (f-num (f-acc "data->'numbers-data->'data[0]->'sent_count")))
           'bold)
    " and received "
    (f-ace (f (f-num (f-acc "data->'numbers-data->'data[0]->'received_count")))
           'bold)
    " messages across "
    (f-ace (f (f-num (f-acc "data->'numbers-data->'data[0]->'chat_count")))
           'bold)
    " chats.\n\n"
    (f-h2 "Top chats and users") "\n"
    (f-h3 "Top personal chats") "\n")
   (deterred-grid-print-with-org
    (alist-get 'data (alist-get 'top-personal-chats data))
    :column-names '((name . "Chat") (sent . "Sent") (received . "Received") (total . "Total"))
    :max-rows 10
    :grid-button t)
   "\n"
   (deterred-format (f-h3 "Top group chats") "\n")
   (deterred-grid-print-with-org
    (alist-get 'data (alist-get 'top-group-chats data))
    :column-names '((name . "Chat") (sent . "Sent") (received . "Received") (total . "Total"))
    :max-rows 10
    :grid-button t)
   "\n"
   (deterred-format (f-h3 "Top users in group chats") "\n")
   (deterred-grid-print-with-org
    (alist-get 'data (alist-get 'top-group-chat-users data))
    :column-names '((name . "User") (total . "Messages"))
    :max-rows 10
    :grid-button t)
   "\n"
   (deterred-format (f-h2 "Messengers") "\n"
                    (f-h3 "Top messengers") "\n")
   (deterred-grid-print-with-org
    (alist-get 'data (alist-get 'top-messengers data))
    :column-names '((messenger . "Messenger") (sent . "Sent") (received . "Received") (total . "Total"))
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
df_year = pd.DataFrame(data['sent-received-per-year']['data'])
df_personal_year = pd.DataFrame(data['personal-sent-received-per-year']['data'])
df_group_year = pd.DataFrame(data['group-sent-received-per-year']['data'])
df_month = pd.DataFrame(data['sent-received-per-month']['data'])
df_personal_month = pd.DataFrame(data['personal-sent-received-per-month']['data'])
df_group_month = pd.DataFrame(data['group-sent-received-per-month']['data'])
df_top_chats = pd.DataFrame(data['top-chats-per-month']['data'])
df_messenger_year = pd.DataFrame(data['messenger-per-year']['data'])

images = []

# Helper function to plot sent/received with sent below X axis
def plot_sent_received(ax, df, x_col, title):
    if len(df) == 0:
        return
    x = df[x_col]
    sent = -df['sent']  # Negative for below X axis
    received = df['received']

    width = 0.8
    ax.bar(x, sent, width, label='Sent')
    ax.bar(x, received, width, label='Received')
    ax.axhline(y=0, color='black', linewidth=0.5)
    ax.set_title(title)
    ax.legend()
    ax.set_xlabel(x_col.capitalize())
    ax.set_ylabel('Messages')

# Total per year
fig, ax = plt.subplots(figsize=(8, 5))
plot_sent_received(ax, df_year, 'year', 'Messages sent/received per year')
images.append(fig_to_b64(fig))

# Personal per year
fig, ax = plt.subplots(figsize=(8, 5))
plot_sent_received(ax, df_personal_year, 'year', 'Personal messages sent/received per year')
images.append(fig_to_b64(fig))

# Group per year
fig, ax = plt.subplots(figsize=(8, 5))
plot_sent_received(ax, df_group_year, 'year', 'Group messages sent/received per year')
images.append(fig_to_b64(fig))

# Total per month
fig, ax = plt.subplots(figsize=(8, 5))
plot_sent_received(ax, df_month, 'month', 'Messages sent/received per month')
if len(df_month) > 30:
    ax.xaxis.set_major_locator(MaxNLocator(nbins=40))
images.append(fig_to_b64(fig))

# Personal per month
fig, ax = plt.subplots(figsize=(8, 5))
plot_sent_received(ax, df_personal_month, 'month', 'Personal messages sent/received per month')
if len(df_personal_month) > 30:
    ax.xaxis.set_major_locator(MaxNLocator(nbins=40))
images.append(fig_to_b64(fig))

# Group per month
fig, ax = plt.subplots(figsize=(8, 5))
plot_sent_received(ax, df_group_month, 'month', 'Group messages sent/received per month')
if len(df_group_month) > 30:
    ax.xaxis.set_major_locator(MaxNLocator(nbins=40))
images.append(fig_to_b64(fig))

# Top chats per month - sent
if len(df_top_chats) > 0:
    df_top_chats_sent = df_top_chats.pivot(index='month', columns='name', values='sent').fillna(0)
    fig, ax = plt.subplots(figsize=(8, 5))
    df_top_chats_sent.plot(ax=ax, kind='line')
    ax.set_title('Messages sent in top N personal chats per month')
    ax.set_xlabel('Month')
    ax.set_ylabel('Messages sent')
    images.append(fig_to_b64(fig))

    # Top chats per month - received
    df_top_chats_received = df_top_chats.pivot(index='month', columns='name', values='received').fillna(0)
    fig, ax = plt.subplots(figsize=(8, 5))
    df_top_chats_received.plot(ax=ax, kind='line')
    ax.set_title('Messages received in top N personal chats per month')
    ax.set_xlabel('Month')
    ax.set_ylabel('Messages received')
    images.append(fig_to_b64(fig))
else:
    images.append(None)
    images.append(None)

# Messenger per year - sent
if len(df_messenger_year) > 0:
    df_messenger_sent = df_messenger_year.pivot(index='year', columns='messenger', values='sent').fillna(0)
    fig, ax = plt.subplots(figsize=(8, 5))
    df_messenger_sent.plot(ax=ax, kind='bar', stacked=True)
    ax.set_title('Messages sent per messenger per year')
    ax.set_xlabel('Year')
    ax.set_ylabel('Messages sent')
    images.append(fig_to_b64(fig))

    # Messenger per year - received
    df_messenger_received = df_messenger_year.pivot(index='year', columns='messenger', values='received').fillna(0)
    fig, ax = plt.subplots(figsize=(8, 5))
    df_messenger_received.plot(ax=ax, kind='bar', stacked=True)
    ax.set_title('Messages received per messenger per year')
    ax.set_xlabel('Year')
    ax.set_ylabel('Messages received')
    images.append(fig_to_b64(fig))
else:
    images.append(None)
    images.append(None)

print(json.dumps(images))"
   :input data
   :on-success
   (lambda (images)
     (insert (deterred-format (f-h3 "Messages sent/received per year") "\n"))
     (deterred-dashboard-print-images-base64 (elt images 0))
     (insert "\n")
     (insert (deterred-format (f-h3 "Personal messages sent/received per year") "\n"))
     (deterred-dashboard-print-images-base64 (elt images 1))
     (insert "\n")
     (insert (deterred-format (f-h3 "Group messages sent/received per year") "\n"))
     (deterred-dashboard-print-images-base64 (elt images 2))
     (insert "\n")
     (insert (deterred-format (f-h3 "Messages sent/received per month") "\n"))
     (deterred-dashboard-print-images-base64 (elt images 3))
     (insert "\n")
     (insert (deterred-format (f-h3 "Personal messages sent/received per month") "\n"))
     (deterred-dashboard-print-images-base64 (elt images 4))
     (insert "\n")
     (insert (deterred-format (f-h3 "Group messages sent/received per month") "\n"))
     (deterred-dashboard-print-images-base64 (elt images 5))
     (insert "\n")
     (when (elt images 6)
       (insert (deterred-format (f-h3 "Messages sent in top N personal chats per month") "\n"))
       (deterred-dashboard-print-images-base64 (elt images 6))
       (insert "\n"))
     (when (elt images 7)
       (insert (deterred-format (f-h3 "Messages received in top N personal chats per month") "\n"))
       (deterred-dashboard-print-images-base64 (elt images 7))
       (insert "\n"))
     (when (elt images 8)
       (insert (deterred-format (f-h3 "Messages sent per messenger per year") "\n"))
       (deterred-dashboard-print-images-base64 (elt images 8))
       (insert "\n"))
     (when (elt images 9)
       (insert (deterred-format (f-h3 "Messages received per messenger per year") "\n"))
       (deterred-dashboard-print-images-base64 (elt images 9))
       (insert "\n"))))
  (insert
   (deterred-format (f-h2 "Top periods") "\n"
                    (f-h3 "Top months") "\n")
   (deterred-grid-print-with-org
    (alist-get 'data (alist-get 'top-months data))
    :column-names '((month . "Month") (sent . "Sent") (received . "Received"))
    :max-rows 10
    :grid-button t)
   "\n"
   (deterred-format (f-h3 "Top weeks") "\n")
   (deterred-grid-print-with-org
    (alist-get 'data (alist-get 'top-weeks data))
    :column-names '((week . "Week") (sent . "Sent") (received . "Received"))
    :max-rows 10
    :grid-button t)
   "\n"
   (deterred-format (f-h3 "Top days") "\n")
   (deterred-grid-print-with-org
    (alist-get 'data (alist-get 'top-days data))
    :column-names '((day . "Day") (sent . "Sent") (received . "Received"))
    :max-rows 10
    :grid-button t)
   "\n"))

(provide 'deterred-dashboard-messengers)
;;; deterred-dashboard-messengers.el ends here

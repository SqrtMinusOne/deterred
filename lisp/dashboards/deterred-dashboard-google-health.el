;;; deterred-dashboard-google-health.el --- Google Health dashboard -*- lexical-binding: t -*-

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
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Sleep, exercise and daily measurements from Google Health Takeout.
;; Dataset tables and CSV exports include all stored measurements.
;; Dates use the session's recorded local date, or the daily summary's
;; calendar date.  Missing measurements are never filled with zero.

;;; Code:

(require 'deterred-dashboard)
(require 'deterred-db)
(require 'deterred-format)
(require 'deterred-google-health)

(defclass deterred-dashboard-google-health (deterred-dashboard)
  ((name :initform "Google Health"))
  "A DETERRED dashboard for imported Google Health measurements.")

(cl-defmethod deterred-dashboard-default-params
  ((_dashboard deterred-dashboard-google-health))
  "Return the Google Health dashboard's default parameters."
  '((:start-date) (:end-date)))

(cl-defmethod deterred-dashboard-render-params
  ((_dashboard deterred-dashboard-google-health))
  "Render the inclusive local-date range for Google Health."
  (deterred-dashboard-widget-date
   :name "Start date" :key :start-date :kind 'from :display-date t)
  (insert "\n")
  (deterred-dashboard-widget-date
   :name "End date" :key :end-date :kind 'to :display-date t)
  (insert "\n"))

(cl-defmethod deterred-dashboard-list-datasets
  ((_dashboard deterred-dashboard-google-health))
  "Describe Google Health datasets, including all stored measurements."
  '((sleep-sessions (name . "Sleep sessions"))
    (activity-sessions (name . "Exercise, sedentary and meditation sessions"))
    (daily-health (name . "Daily health measurements"))
    (sleep-stages (name . "Sleep stages"))
    (sleep-per-day (name . "Sleep per waking date"))
    (exercise-by-type (name . "Recorded exercise by type"))
    (sleep-hourly (name . "Hourly sleep probability (%)"))
    (activity-hourly (name . "Hourly activity probabilities (%)"))
    (sedentary-hourly (name . "Hourly sedentary probability (%)"))
    (sleep-lengths (name . "Sleep session lengths (hours)"))))

(defun deterred-dashboard-google-health--date-params (params)
  "Convert date widget values in PARAMS to the selected calendar dates.
The widgets encode dates in Emacs's local timezone.  Formatting them
in the import timezone would shift dates for users in another zone."
  (mapcar (lambda (key)
            (cons key (when-let* ((value (alist-get key params)))
                        (format-time-string "%Y-%m-%d" value))))
          '(:start-date :end-date)))

(defun deterred-dashboard-google-health--fetch-raw-datasets (db params)
  "Fetch Google Health datasets from DB within the dates in PARAMS.
Sleep reports consistently use the effective-session view, which
selects v2 sleep in preference to overlapping v1 estimates.
Daily sleep combines entire sessions by local waking date, including
naps and sleep starting the previous evening.  A missing asleep
duration leaves that day's total unknown."
  (let ((dates (deterred-dashboard-google-health--date-params params))
        (session-range "
    [[AND s.local_date >= :start-date]]
    [[AND s.local_date <= :end-date]]")
        (session-columns "SELECT s.*,
    datetime(s.start_timestamp + s.start_utc_offset, 'unixepoch') local_start,
    datetime(s.end_timestamp + s.end_utc_offset, 'unixepoch') local_end,
    (s.end_timestamp - s.start_timestamp) / 3600.0 duration_hours
  FROM google_health_session_effective s"))
    `((sleep-sessions
       . ,(deterred-db-select-template-alist
           db (concat session-columns " WHERE s.kind = 'sleep'"
                      session-range " ORDER BY s.start_timestamp, s.id") dates))
      (activity-sessions
       . ,(deterred-db-select-template-alist
           db (concat session-columns " WHERE s.kind <> 'sleep'"
                      "
    [[AND date(s.end_timestamp - 1 + s.end_utc_offset, 'unixepoch') >= :start-date]]
    [[AND date(s.start_timestamp + s.start_utc_offset, 'unixepoch') <= :end-date]]
  ORDER BY s.start_timestamp, s.id") dates))
      (daily-health
       . ,(deterred-db-select-template-alist
           db "SELECT * FROM google_health_daily WHERE 1 = 1
    [[AND date >= :start-date]]
    [[AND date <= :end-date]]
  ORDER BY date" dates))
      (sleep-stages
       . ,(deterred-db-select-template-alist
           db (concat "SELECT g.*, s.local_date,
    (g.end_timestamp - g.start_timestamp) / 60.0 duration_minutes
  FROM google_health_sleep_stage g
  JOIN google_health_session_effective s ON s.id = g.session_id
  WHERE s.kind = 'sleep'" session-range
                      " ORDER BY g.start_timestamp, g.id") dates))
      (sleep-per-day
       . ,(deterred-db-select-template-alist
           db (concat "SELECT s.local_date date, COUNT(*) sessions,
    SUM(s.end_timestamp - s.start_timestamp) / 3600.0 period_hours,
    CASE WHEN COUNT(s.minutes_asleep) = COUNT(*)
         THEN SUM(s.minutes_asleep) / 60.0 END asleep_hours,
    CASE WHEN COUNT(s.minutes_awake) = COUNT(*)
         THEN SUM(s.minutes_awake) / 60.0 END awake_hours,
    AVG(s.sleep_score) mean_sleep_score,
    COUNT(s.sleep_score) scored_sessions
  FROM google_health_session_effective s
  WHERE s.kind = 'sleep'" session-range
                      " GROUP BY s.local_date ORDER BY s.local_date") dates))
      (exercise-by-type
       . ,(deterred-db-select-template-alist
           db (concat "SELECT s.activity_type, COUNT(*) sessions,
    SUM(s.end_timestamp - s.start_timestamp) / 3600.0 hours
  FROM google_health_session_effective s
  WHERE s.kind = 'exercise'" session-range
                      " GROUP BY s.activity_type ORDER BY hours DESC") dates)))))

(defun deterred-dashboard-google-health--interval-pieces (row)
  "Split ROW into (DATE HOUR START END) wall-clock pieces.
START and END are seconds within HOUR.  Use the recorded offsets;
when they change, use the configured zone only if both endpoints
match it.  Unknown offset transitions cannot be reconstructed.
Repeated clock minutes are unioned by the profile calculation."
  (let* ((start (alist-get 'start_timestamp row))
         (end (alist-get 'end_timestamp row))
         (offset (alist-get 'start_utc_offset row))
         (end-offset (alist-get 'end_utc_offset row))
         (changing (not (equal offset end-offset)))
         (zone deterred-google-health-timezone)
         pieces)
    (when (and (numberp offset) (numberp end-offset)
               (or (not changing)
                   (and (= offset (car (current-time-zone start zone)))
                        (= end-offset (car (current-time-zone end zone))))))
      (while (< start end)
        (let* ((current-offset (if changing (car (current-time-zone start zone)) offset))
               (wall (+ start current-offset))
               (second (mod wall 3600))
               ;; Minute boundaries also split modern timezone transitions.
               (next (min end (+ start (- (if changing 60 3600)
                                         (mod wall (if changing 60 3600)))))))
          (push (list (format-time-string "%F" wall t)
                      (decoded-time-hour (decode-time wall t))
                      second (+ second (- next start))) pieces)
          (setq start next)))
      (nreverse pieces))))

(defun deterred-dashboard-google-health--union-seconds (intervals)
  "Return the duration of the union of INTERVALS, each a (START . END)."
  (let ((sorted (sort (copy-sequence intervals) (lambda (a b) (< (car a) (car b)))))
        start end (total 0))
    (dolist (interval sorted)
      (if (and end (<= (car interval) end))
          (setq end (max end (cdr interval)))
        (when end (cl-incf total (- end start)))
        (setq start (car interval) end (cdr interval))))
    (+ total (if end (- end start) 0))))

(defun deterred-dashboard-google-health--add-piece (table category date piece)
  "Add PIECE to TABLE under CATEGORY, DATE and its clock hour."
  (let ((key (list category date (nth 1 piece))))
    (push (cons (nth 2 piece) (nth 3 piece)) (gethash key table))))

(defun deterred-dashboard-google-health--probabilities (table categories days)
  "Return hourly percentages from TABLE for CATEGORIES over DAYS.
Union overlapping intervals per date and category before averaging.
With no observed days, values are nil rather than zero."
  (let ((totals (make-hash-table :test #'eq)))
    (dolist (category categories) (puthash category (make-vector 24 0) totals))
    (maphash (lambda (key intervals)
               (cl-incf (aref (gethash (car key) totals) (nth 2 key))
                        (deterred-dashboard-google-health--union-seconds intervals)))
             table)
    (maphash (lambda (_category values)
               (dotimes (hour 24)
                 (aset values hour (when (> days 0)
                                     (* 100.0 (/ (aref values hour) (* 3600.0 days)))))))
             totals)
    totals))

(defun deterred-dashboard-google-health--sleep-profile (stages)
  "Calculate a 24-hour sleep profile from effective STAGES.
Distinct waking dates with stage coverage form the denominator.
Naps share that date; awake and restless stages do not count as sleep."
  (let ((days (make-hash-table :test #'equal))
        (intervals (make-hash-table :test #'equal)))
    (dolist (row stages)
      (let ((pieces (deterred-dashboard-google-health--interval-pieces row))
            (stage (upcase (or (alist-get 'stage row) "")))
            (date (alist-get 'local_date row)))
        (when (and pieces (member stage '("LIGHT" "DEEP" "REM" "ASLEEP" "AWAKE" "RESTLESS")))
          (puthash date t days)
          (when (member stage '("LIGHT" "DEEP" "REM" "ASLEEP"))
            (dolist (piece pieces)
              (deterred-dashboard-google-health--add-piece intervals 'sleep date piece))))))
    (let* ((count (hash-table-count days))
           (values (gethash 'sleep (deterred-dashboard-google-health--probabilities
                                   intervals '(sleep) count))))
      (cl-loop for hour below 24 collect
               `((hour . ,hour) (probability . ,(aref values hour)) (observed_days . ,count))))))

(defun deterred-dashboard-google-health--day-minutes (date zone)
  "Return the actual number of minutes in DATE in ZONE, including DST."
  (pcase-let ((`(,year ,month ,day) (mapcar #'string-to-number (split-string date "-"))))
    (/ (- (time-convert (encode-time 0 0 0 (1+ day) month year zone) 'integer)
          (time-convert (encode-time 0 0 0 day month year zone) 'integer)) 60)))

(defun deterred-dashboard-google-health--in-range-p (date dates)
  "Return non-nil if DATE falls within the inclusive limits in DATES."
  (and (or (not (alist-get :start-date dates))
           (not (string< date (alist-get :start-date dates))))
       (or (not (alist-get :end-date dates))
           (not (string< (alist-get :end-date dates) date)))))

(defun deterred-dashboard-google-health--activity-profiles (sessions daily dates)
  "Return exercise and sedentary hourly profiles from SESSIONS and DAILY.
DATES contains inclusive calendar-date limits.  Complete energy days
provide the denominator, including days with no exercise.  If energy
coverage is entirely absent, fall back to recorded activity dates."
  (let ((days (make-hash-table :test #'equal))
        (intervals (make-hash-table :test #'equal))
        coverage rows)
    (dolist (day daily)
      (when-let* ((minutes (alist-get 'energy_sample_minutes day)))
        (setq coverage t)
        (when (= minutes (deterred-dashboard-google-health--day-minutes
                          (alist-get 'date day) (alist-get 'timezone day)))
          (puthash (alist-get 'date day) t days))))
    (dolist (session sessions)
      (let ((category
             (pcase (alist-get 'kind session)
               ("sedentary" 'sedentary)
               ("exercise"
                (pcase (downcase (or (alist-get 'activity_type session) ""))
                  ((or "walk" "walking") 'walking)
                  ((or "bike" "biking" "bicycle" "bicycling" "cycling") 'biking)
                  (_ 'other))))))
        (when category
          (dolist (piece (deterred-dashboard-google-health--interval-pieces session))
            (when (deterred-dashboard-google-health--in-range-p (car piece) dates)
              (push (cons category piece) rows)
              (unless coverage (puthash (car piece) t days)))))))
    (dolist (row rows)
      (when (gethash (cadr row) days)
        (deterred-dashboard-google-health--add-piece intervals (car row) (cadr row) (cdr row))))
    (let* ((count (hash-table-count days))
           (values (deterred-dashboard-google-health--probabilities
                    intervals '(walking biking other sedentary) count)))
      `((activity-hourly
         . ,(cl-loop for hour below 24 collect
                     `((hour . ,hour)
                       (walking_probability . ,(aref (gethash 'walking values) hour))
                       (biking_probability . ,(aref (gethash 'biking values) hour))
                       (other_probability . ,(aref (gethash 'other values) hour))
                       (observed_days . ,count))))
        (sedentary-hourly
         . ,(cl-loop for hour below 24 collect
                     `((hour . ,hour) (probability . ,(aref (gethash 'sedentary values) hour))
                       (observed_days . ,count))))))))

(defun deterred-dashboard-google-health--fetch-datasets (db params)
  "Fetch raw measurements and distribution datasets from DB using PARAMS."
  (let ((data (deterred-dashboard-google-health--fetch-raw-datasets db params)))
    (append data
            `((sleep-hourly . ,(deterred-dashboard-google-health--sleep-profile
                                (alist-get 'sleep-stages data))))
            (deterred-dashboard-google-health--activity-profiles
             (alist-get 'activity-sessions data) (alist-get 'daily-health data)
             (deterred-dashboard-google-health--date-params params))
            `((sleep-lengths
               . ,(cl-loop for row in (alist-get 'sleep-sessions data)
                           when (numberp (alist-get 'minutes_asleep row)) collect
                           `((session_id . ,(alist-get 'id row))
                             (date . ,(alist-get 'local_date row))
                             (hours . ,(/ (alist-get 'minutes_asleep row) 60.0)))))))))

(cl-defmethod deterred-dashboard-fetch-datasets
  ((_dashboard deterred-dashboard-google-health) params)
  "Fetch Google Health datasets using PARAMS."
  (deterred-dashboard-google-health--fetch-datasets (deterred-db--init) params))

(defconst deterred-dashboard-google-health--python
  "import json
import numpy as np
import pandas as pd
from matplotlib import pyplot as plt
from matplotlib.ticker import PercentFormatter, MaxNLocator
from deterred import fig_to_b64

data = json.loads(input())
images = []

def frame(name):
    return pd.DataFrame(data[name]['data'] or [])

def numbers(table, column):
    if column not in table:
        return pd.Series(dtype=float)
    return pd.to_numeric(table[column], errors='coerce').dropna()

def save(fig):
    fig.tight_layout(pad=2.0)
    images.append(fig_to_b64(fig))
    plt.close(fig)

def profile(table, column, label, full_scale=True, color='C0'):
    values = numbers(table, column)
    if values.empty:
        images.append(None)
        return
    fig, ax = plt.subplots(figsize=(8, 5))
    peak = values.max()
    hours = values.sum() / 100
    ax.bar(table.loc[values.index, 'hour'], values, width=1, align='edge',
           color=color, edgecolor='white', linewidth=0.5, alpha=0.85,
           label=f'{label} · {hours:.1f} h/day')
    days = int(table['observed_days'].iloc[0])
    ax.set_title(f'{label} · {days} days')
    ax.set_xlim(0, 24)
    ax.set_xticks(range(0, 25, 3))
    ax.set_xlabel('Hour of day')
    ax.set_ylabel('Probability')
    ax.set_ylim(0, 100 if full_scale else min(100, max(5, np.ceil(peak / 5) * 5)))
    ax.yaxis.set_major_formatter(PercentFormatter())
    ax.set_axisbelow(True)
    ax.grid(axis='y', alpha=0.2)
    ax.legend(loc='best')
    save(fig)

def histogram(series, labels, title, unit, ylabel, width=None):
    present = [(values, label) for values, label in zip(series, labels) if not values.empty]
    if not present:
        images.append(None)
        return
    fig, ax = plt.subplots(figsize=(8, 5))
    all_values = np.concatenate([values.to_numpy() for values, _ in present])
    if width:
        lower = np.floor(all_values.min() / width) * width
        upper = np.floor(all_values.max() / width) * width + width
        bins = np.arange(lower, upper + width / 2, width)
    else:
        bins = np.histogram_bin_edges(all_values, bins='auto')
        # Bound the chart cost for long-tailed measurements.
        if len(bins) > 51:
            bins = np.linspace(all_values.min(), all_values.max(), 51)
    for values, label in present:
        caption = (f'{label} · n={len(values)}\\n'
                   f'mean {values.mean():,.1f} · median {values.median():,.1f}')
        ax.hist(values, bins=bins, histtype='bar', alpha=0.75,
                edgecolor='white', linewidth=0.5, label=caption)
    ax.set_title(title)
    ax.set_xlabel(unit)
    ax.set_ylabel(ylabel)
    ax.yaxis.set_major_locator(MaxNLocator(integer=True))
    ax.set_axisbelow(True)
    ax.grid(axis='y', alpha=0.2)
    ax.legend(fontsize=9, loc='upper right' if len(present) > 1 else 'best')
    save(fig)

sleep = frame('sleep-hourly')
daily_sleep = numbers(frame('sleep-per-day'), 'asleep_hours')
profile(sleep, 'probability', 'Asleep')
histogram([daily_sleep], ['Sleep'], 'Daily sleep', 'Hours asleep per day', 'Days', 0.5)

activity = frame('activity-hourly')
sedentary = frame('sedentary-hourly')
profile(activity, 'walking_probability', 'Walking', False)
profile(activity, 'biking_probability', 'Biking', False, 'C1')
profile(activity, 'other_probability', 'Other activities', False, 'C2')
profile(sedentary, 'probability', 'Sedentary', color='C3')

daily = frame('daily-health')
steps = numbers(daily, 'steps')
energy = numbers(daily, 'energy_kcal')
active_energy = numbers(daily, 'active_energy_kcal')
distance = numbers(daily, 'distance_m') / 1000
histogram([steps], ['Steps'], 'Daily steps', 'Steps', 'Days')
histogram([energy, active_energy], ['Total', 'Active'], 'Daily energy', 'kcal', 'Days')
histogram([distance], ['Distance'], 'Daily distance', 'km', 'Days')

print(json.dumps(images))"
  "Render distribution charts from datasets calculated in Emacs Lisp.
Return nine image slots: two sleep, four activity and three daily charts.
JSON is only the dashboard's existing subprocess transport; no JSON
is stored in the database.  Python performs chart rendering only.")

(cl-defmethod deterred-dashboard-render-results
  ((_dashboard deterred-dashboard-google-health) _params data)
  "Render Google Health distributions and statistics from DATA."
  (insert (format "%d sleep sessions, %d activity sessions, %d daily summaries.\n\n"
                  (length (alist-get 'data (alist-get 'sleep-sessions data)))
                  (length (alist-get 'data (alist-get 'activity-sessions data)))
                  (length (alist-get 'data (alist-get 'daily-health data)))))
  (deterred-dashboard-exec-python
   :python-code deterred-dashboard-google-health--python
   :input data
   :on-success
   (lambda (images)
     (dolist (section '(("Sleep probability" 0)
                        ("Daily sleep" 1)
                        ("Activity" 2 3 4 5)
                        ("Daily steps" 6)
                        ("Daily energy" 7)
                        ("Daily distance" 8)))
       (let ((charts (delq nil (mapcar (lambda (index) (elt images index))
                                      (cdr section)))))
         (when charts
           (insert (deterred-format (f-h2 (car section)) "\n"))
           (dolist (chart charts)
             (deterred-dashboard-print-images-base64 chart)
             (insert "\n\n"))))))))

(provide 'deterred-dashboard-google-health)
;;; deterred-dashboard-google-health.el ends here

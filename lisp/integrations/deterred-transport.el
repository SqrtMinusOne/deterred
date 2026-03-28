;;; deterred-transport.el --- Public transport integration for DETERRED. -*- lexical-binding: t -*-

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

;; Public transport integration for DETERRED.
;;
;; This provides integration with the Podorozhnik API to track public
;; transport trips in Saint Petersburg.

;;; Code:
(require 'deterred-db)
(require 'deterred-source)
(require 'deterred-utils)
(require 'request)
(require 'iso8601)
(require 'cl-lib)
(require 'uuidgen)

(defconst deterred-transport-uuid-namespace
  "a7b8c9d0-1234-5678-9abc-def012345678")

(defconst deterred-transport-vehicle-type-mapping
  '((1 . "Bus")
    (2 . "Tram")
    (3 . "Trolleybus")
    (4 . "Commercial Bus")
    (5 . "Railway")
    (7 . "Metro"))
  "Mapping from Podorozhnik vehicle type IDs to English names.")

(defconst deterred-transport-russian-vehicle-type-mapping
  '(("Автобус" . "Bus")
    ("Трамвай" . "Tram")
    ("Троллейбус" . "Trolleybus")
    ("Метро" . "Metro"))
  "Mapping from Russian vehicle type names to English names.")

(defcustom deterred-transport-podorozhnik-api-endpoint "https://podorozhnik.spb.ru/api/"
  "API endpoint for Podorozhnik."
  :group 'deterred-sources
  :type 'string)

(defcustom deterred-transport-podorozhnik-login nil
  "Login (email) for Podorozhnik."
  :group 'deterred-sources
  :type 'string)

(defcustom deterred-transport-podorozhnik-password nil
  "Password for Podorozhnik."
  :group 'deterred-sources
  :type 'string)

(defcustom deterred-transport-podorozhnik-page-size 100
  "Number of trips to fetch per page."
  :group 'deterred-sources
  :type 'number)

(defvar deterred-transport-podorozhnik--auth-token nil
  "Current authentication token for Podorozhnik API.")

(defun deterred-transport-podorozhnik--api-login (callback)
  "Login to Podorozhnik API and call CALLBACK with the token."
  (unless deterred-transport-podorozhnik-login
    (user-error "Podorozhnik login not set!"))
  (unless deterred-transport-podorozhnik-password
    (user-error "Podorozhnik password not set!"))
  (let ((request-curl-options (append request-curl-options '("-k"))))
    (request (concat deterred-transport-podorozhnik-api-endpoint "auth/login")
      :type "POST"
      :headers '(("Content-Type" . "application/json;charset=utf-8")
                 ("x-ppa-language" . "ru"))
      :data (json-encode `((login . ,deterred-transport-podorozhnik-login)
                           (password . ,deterred-transport-podorozhnik-password)))
      :parser 'json-read
      :encoding 'utf-8
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (let ((token (alist-get 'token data)))
                    (setq deterred-transport-podorozhnik--auth-token token)
                    (funcall callback token))))
      :error #'deterred-utils-on-request-error)))

(defun deterred-transport-podorozhnik--api-get-trips (token page callback)
  "Fetch trips from Podorozhnik API.

TOKEN is the authentication token.
PAGE is the page number to fetch.
CALLBACK is called with the response data."
  (let ((request-curl-options (append request-curl-options '("-k"))))
    (request (concat deterred-transport-podorozhnik-api-endpoint "v3/trips")
      :params `(("filters" . "")
                ("sorts" . "-DateTime")
                ("page" . ,(number-to-string page))
                ("pageSize" . ,(number-to-string deterred-transport-podorozhnik-page-size)))
      :headers `(("Authorization" . ,(concat "Bearer " token))
                 ("x-ppa-language" . "ru"))
      :parser 'json-read
      :encoding 'utf-8
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (funcall callback data)))
      :error #'deterred-utils-on-request-error)))

(defun deterred-transport-podorozhnik--transform-trip (trip)
  "Transform a single TRIP from API format to database format.

TRIP is an alist from the API response.

Return an alist with keys: id, source, timestamp, transport, route, cost."
  (let* ((datetime-string (alist-get 'dateTime trip))
         (timestamp (time-convert
                     (encode-time (iso8601-parse datetime-string))
                     'integer))
         (vehicle-type-id (alist-get 'vehicleTypeId trip))
         (transport (or (alist-get vehicle-type-id
                                   deterred-transport-vehicle-type-mapping)
                        "Unknown"))
         (route (or (alist-get 'vehicleRoute trip) ""))
         (cost (/ (alist-get 'amountInMinorUnits trip) 100.0))
         (id (uuidgen-3 deterred-transport-uuid-namespace
                        (format "%s-podorozhnik" timestamp))))
    `((id . ,id)
      (source . "podorozhnik")
      (timestamp . ,timestamp)
      (transport . ,transport)
      (route . ,route)
      (cost . ,cost))))

(defun deterred-transport-podorozhnik--fetch-all-trips (token page total-pages callback &optional accumulated-trips)
  "Recursively fetch all trips from the API.

TOKEN is the authentication token.
PAGE is the current page number.
TOTAL-PAGES is the total number of pages (nil if unknown).
CALLBACK is called with all collected trips when all pages are fetched.
ACCUMULATED-TRIPS is the list of trips collected so far."
  (message "Fetching transport trips page %d%s..."
           page
           (if total-pages (format "/%d" total-pages) ""))
  (deterred-transport-podorozhnik--api-get-trips
   token
   page
   (lambda (data)
     (let* ((items (alist-get 'items data))
            (pages-count (alist-get 'pagesCount data))
            (transformed-trips (mapcar #'deterred-transport-podorozhnik--transform-trip items))
            (all-trips (append accumulated-trips transformed-trips)))
       (if (< page pages-count)
           (deterred-transport-podorozhnik--fetch-all-trips
            token (1+ page) pages-count callback all-trips)
         (funcall callback all-trips))))))

(defun deterred-transport--parse-pdf-datetime (datetime-str date-str)
  "Parse DATETIME-STR (HH:MM:SS) and DATE-STR (DD.MM.YYYY) to timestamp.

Return a Unix timestamp as integer."
  (let* ((time-parts (split-string datetime-str ":"))
         (date-parts (split-string date-str "\\."))
         (hour (string-to-number (nth 0 time-parts)))
         (minute (string-to-number (nth 1 time-parts)))
         (second (string-to-number (nth 2 time-parts)))
         (day (string-to-number (nth 0 date-parts)))
         (month (string-to-number (nth 1 date-parts)))
         (year (string-to-number (nth 2 date-parts)))
         (decoded-time (list second minute hour day month year nil nil nil)))
    (time-convert (encode-time decoded-time) 'integer)))

(defun deterred-transport--parse-pdf-line (line)
  "Parse a single LINE from the PDF table.

Return an alist with keys: timestamp, transport, route, cost, or nil if invalid."
  (when (string-match
         (rx (+ digit) (+ space)  ; ticket number
             "Подорожник" (+ space)  ; description
             "Единый билет (ЕБ)" (+ space)  ; ticket type
             (group (+ (any digit ":"))) (+ space)  ; time (HH:MM:SS)
             (group (+ (any digit "."))) (+ space)  ; date (DD.MM.YYYY)
             (group (+ (not (any space)))) (+ space)  ; route
             (group (+ (any "А-Яа-я"))) (+ space)  ; transport type
             (group (+ digit)))  ; cost in rubles
         line)
    (let* ((time-str (match-string 1 line))
           (date-str (match-string 2 line))
           (route (match-string 3 line))
           (transport-ru (match-string 4 line))
           (cost-rubles (string-to-number (match-string 5 line)))
           (transport (or (alist-get transport-ru
                                     deterred-transport-russian-vehicle-type-mapping
                                     nil nil #'equal)
                          "Unknown"))
           (timestamp (deterred-transport--parse-pdf-datetime time-str date-str))
           (cost (* cost-rubles 1))  ; Don't convert rubles to minor units (kopecks)
           (id (uuidgen-3 deterred-transport-uuid-namespace
                          (format "%s-podorozhnik" timestamp))))
      `((id . ,id)
        (source . "podorozhnik")
        (timestamp . ,timestamp)
        (transport . ,transport)
        (route . ,route)
        (cost . ,cost)))))

(defun deterred-transport--parse-pdf-text (text)
  "Parse TEXT from PDF export.

Return a list of trips as alists."
  (let ((lines (split-string text "\n"))
        trips)
    (dolist (line lines)
      (when-let ((trip (deterred-transport--parse-pdf-line line)))
        (push trip trips)))
    (nreverse trips)))

(defun deterred-transport-load-pdf (file)
  "Load Podorozhnik PDF export FILE into DETERRED."
  (interactive
   (list
    (expand-file-name
     (read-file-name "PDF file: " nil nil nil nil
                     (lambda (f)
                       (or (file-directory-p f)
                           (string-match-p (rx ".pdf" eos) f)))))))
  (let* ((db (deterred-db--init))
         (pdf-text (with-temp-buffer
                     (call-process "pdftotext" nil t nil "-layout" file "-")
                     (buffer-string)))
         (trips (deterred-transport--parse-pdf-text pdf-text)))
    (with-sqlite-transaction db
      (when trips
        (deterred-db-insert-unsafe
         db
         :table-name 'transport_trips
         :values trips
         :attrs '(id source timestamp transport route cost)
         :conflict-action 'do-nothing))
      (deterred-db-mark-updated-batch db '("transport_trips"))
      (message "Imported %d trips from PDF" (length trips)))))

(defun deterred-transport-yandex-taxi--transform-order (order)
  "Transform a single Yandex Taxi ORDER to database format.

ORDER is an alist parsed from JSON.

Return an alist with keys: id, source, timestamp, transport, route, cost."
  (let* ((data (alist-get 'data order))
         (timestamp (time-convert
                     (encode-time (iso8601-parse (alist-get 'created_at data)))
                     'integer))
         (route (format "%s → %s"
                        (alist-get 'source (alist-get 'route data))
                        (alist-get 'destination (alist-get 'route data))))
         (cost (alist-get 'parsedValue (alist-get 'cost (alist-get 'payment data))))
         (id (uuidgen-3 deterred-transport-uuid-namespace
                        (format "%s-yandex-taxi" timestamp))))
    `((id . ,id)
      (source . "yandex-taxi")
      (timestamp . ,timestamp)
      (transport . "Taxi")
      (route . ,route)
      (cost . ,cost))))

(defun deterred-transport-yandex-taxi-import (source)
  "Import Yandex Taxi orders from JSON SOURCE into DETERRED.

SOURCE can be:
- A file path (JSON file)
- The symbol 'clipboard (to read from clipboard)

The JSON should be the response from /orderhistory/v2/list endpoint."
  (interactive
   (list
    (if (y-or-n-p "Import from clipboard? ")
        'clipboard
      (expand-file-name
       (read-file-name "Yandex Taxi JSON file: " nil nil t nil
                       (lambda (f)
                         (or (file-directory-p f)
                             (string-match-p (rx ".json" eos) f))))))))
  (let* ((db (deterred-db--init))
         (json-data (if (eq source 'clipboard)
                        (with-temp-buffer
                          (insert (current-kill 0))
                          (goto-char (point-min))
                          (json-read))
                      (with-temp-buffer
                        (insert-file-contents source)
                        (goto-char (point-min))
                        (json-read))))
         (orders-vec (alist-get 'orders json-data))
         (orders (append orders-vec nil))  ; Convert vector to list
         (trips (mapcar #'deterred-transport-yandex-taxi--transform-order orders)))
    (with-sqlite-transaction db
      (when trips
        (deterred-db-insert-unsafe
         db
         :table-name 'transport_trips
         :values trips
         :attrs '(id source timestamp transport route cost)
         :conflict-action 'do-nothing))
      (deterred-db-mark-updated-batch db '("transport_trips"))
      (message "Imported %d Yandex Taxi trips" (length trips)))))

(defun deterred-transport-yandex-taxi-fetch ()
  "Open Yandex Taxi website with instructions to copy order history.

This function will:
1. Open the Yandex Taxi website in your browser
2. Display instructions for copying the JSON response

To get the data:
1. Open browser DevTools (F12)
2. Go to the Network tab
3. Navigate to order history on the website
4. Find the request to '/orderhistory/v2/list'
5. Click on it and go to the Response tab
6. Copy the entire JSON response
7. Run `deterred-transport-yandex-taxi-import' and paste from clipboard."
  (interactive)
  (browse-url "https://taxi.yandex.ru/ru_ru/")
  (message "Browser opened. Please:
1. Open DevTools (F12) → Network tab
2. Navigate to order history
3. Find '/orderhistory/v2/list' request
4. Copy the Response JSON
5. Run M-x deterred-transport-yandex-taxi-import"))

(defun deterred-transport-purge ()
  "Clear all transport data from the DETERRED database."
  (interactive)
  (when (y-or-n-p "Are you sure you want to purge transport data from DETERRED?")
    (let ((db (deterred-db--init)))
      (with-sqlite-transaction db
        (sqlite-execute db "DELETE FROM transport_trips")
        (deterred-db-mark-updated-batch db '("transport_trips"))))))

(defun deterred-transport-podorozhnik-sync (&optional callback)
  "Sync transport trips from Podorozhnik API.

Call CALLBACK when done."
  (interactive)
  (deterred-transport-podorozhnik--api-login
   (lambda (token)
     (deterred-transport-podorozhnik--fetch-all-trips
      token 1 nil
      (lambda (all-trips)
        (let ((db (deterred-db--init)))
          (with-sqlite-transaction db
            (when all-trips
              (deterred-db-insert-unsafe
               db
               :table-name 'transport_trips
               :values all-trips
               :attrs '(id source timestamp transport route cost)
               :conflict-action 'do-nothing))
            (deterred-db-mark-updated-batch db '("transport_trips"))
            (if callback
                (funcall callback)
              (message "Done syncing %d transport trips" (length all-trips))))))))))

;;;###autoload
(defclass deterred-transport (deterred-source)
  ((name :initform "Transport"))
  "DETERRED source for public transport trips.")

(cl-defmethod deterred-source-range ((_source deterred-transport) &optional db)
  "Get the data availability range for transport trips.

DB is the sqlite database object.

Return a cons cell, with car as the start timestamp, and cdr as the
end timestamp."
  (let* ((db (or db (deterred-db--init)))
         (data (sqlite-select
                db "SELECT MIN(timestamp), MAX(timestamp)
                    FROM transport_trips")))
    (cons (caar data) (cadar data))))

(cl-defmethod deterred-source-range-detail ((_source deterred-transport) &optional db)
  "Get the data availability range for each transport source.

DB is the sqlite database object.

Return a list of alists with :name, :start, :end for each source."
  (let* ((db (or db (deterred-db--init)))
         (data (deterred-db-select-alist
                db "SELECT source, MIN(timestamp) as start, MAX(timestamp) as end
                    FROM transport_trips
                    GROUP BY source
                    ORDER BY source")))
    (mapcar (lambda (row)
              `((:name . ,(alist-get 'source row))
                (:start . ,(alist-get 'start row))
                (:end . ,(alist-get 'end row))))
            data)))

(cl-defmethod deterred-source-actions ((_source deterred-transport) &optional callback)
  "Run an action for the transport source.

Run CALLBACK when done."
  (deterred-source--actions-pick
   '(("Load podorozhnik PDF" deterred-transport-load-pdf nil)
     ("Sync podorozhnik trips" deterred-transport-podorozhnik-sync nil)
     ("Yandex Taxi: Open website + instructions" deterred-transport-yandex-taxi-fetch nil)
     ("Yandex Taxi: Import JSON" deterred-transport-yandex-taxi-import nil)
     ("Purge transport data" deterred-transport-purge nil))
   callback))

(cl-defmethod deterred-source-sync ((_source deterred-transport) &optional callback)
  "Sync transport trips with DETERRED.

Call CALLBACK when done."
  (unless deterred-transport-podorozhnik-login
    (user-error "Podorozhnik login not set!"))
  (unless deterred-transport-podorozhnik-password
    (user-error "Podorozhnik password not set!"))
  (deterred-transport-podorozhnik-sync callback))

(cl-defmethod deterred-source-range-summary
  ((_source deterred-transport) start end &optional db)
  "Make transport summary for [START, END].

DB is the sqlite database object."
  (let* ((db (or db (deterred-db--init)))
         (trip-count-data
          (deterred-db-select-alist
           db "SELECT COUNT(*) as count FROM transport_trips
               WHERE timestamp BETWEEN ? AND ?"
           (list start end)))
         (trip-count (alist-get 'count (car trip-count-data)))
         (by-transport-data
          (deterred-db-select-alist
           db "SELECT transport, COUNT(*) as count
               FROM transport_trips
               WHERE timestamp BETWEEN ? AND ?
               GROUP BY transport
               ORDER BY count DESC"
           (list start end))))
    (when (> trip-count 0)
      `((:short-description
         . ,(deterred-format
             (f (f-num trip-count) " transport trip"
                (when (> trip-count 1) "s"))))
        (:long-description
         . ,(deterred-format
             "Transport trips: \n"
             (f-mapconcat
             (f "- " (f-acc "iter->'transport") ": "
                 (f-num (alist-get 'count iter)))
              by-transport-data
              "\n")))))))

(cl-defmethod deterred-source-events
  ((_source deterred-transport) start end &optional params db)
  "Return transport trip events for [START, END].

PARAMS may contain the same filters as the transport dashboard,
namely `:start-date' and `:end-date'.  The third event field is the
transport type, so default grouping is by transport type.

DB is the sqlite database object."
  (let ((db (or db (deterred-db--init))))
    (deterred-db-select-template
     db
     "SELECT timestamp, null, transport
FROM transport_trips
WHERE timestamp BETWEEN :start AND :end
  [[AND timestamp >= :start-date]]
  [[AND timestamp <= :end-date]]
ORDER BY timestamp"
     (append params `((:start . ,start) (:end . ,end))))))

(declare-function deterred-dashboard-transport "deterred-dashboard-transport")

(cl-defmethod deterred-source-default-dashboard ((_source deterred-transport))
  "Return the default dashboard for transport."
  (deterred-dashboard-transport))

(provide 'deterred-transport)
;;; deterred-transport.el ends here

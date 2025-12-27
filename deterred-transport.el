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

(defcustom deterred-transport-api-endpoint "https://podorozhnik.spb.ru/api/"
  "API endpoint for Podorozhnik."
  :group 'deterred-sources
  :type 'string)

(defcustom deterred-transport-login nil
  "Login (email) for Podorozhnik."
  :group 'deterred-sources
  :type 'string)

(defcustom deterred-transport-password nil
  "Password for Podorozhnik."
  :group 'deterred-sources
  :type 'string)

(defcustom deterred-transport-page-size 100
  "Number of trips to fetch per page."
  :group 'deterred-sources
  :type 'number)

(defvar deterred-transport--auth-token nil
  "Current authentication token for Podorozhnik API.")

(defun deterred-transport--api-login (callback)
  "Login to Podorozhnik API and call CALLBACK with the token."
  (unless deterred-transport-login
    (user-error "Podorozhnik login not set!"))
  (unless deterred-transport-password
    (user-error "Podorozhnik password not set!"))
  (let ((request-curl-options (append request-curl-options '("-k"))))
    (request (concat deterred-transport-api-endpoint "auth/login")
      :type "POST"
      :headers '(("Content-Type" . "application/json;charset=utf-8")
                 ("x-ppa-language" . "ru"))
      :data (json-encode `((login . ,deterred-transport-login)
                           (password . ,deterred-transport-password)))
      :parser 'json-read
      :encoding 'utf-8
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (let ((token (alist-get 'token data)))
                    (setq deterred-transport--auth-token token)
                    (funcall callback token))))
      :error #'deterred-utils-on-request-error)))

(defun deterred-transport--api-get-trips (token page callback)
  "Fetch trips from Podorozhnik API.

TOKEN is the authentication token.
PAGE is the page number to fetch.
CALLBACK is called with the response data."
  (let ((request-curl-options (append request-curl-options '("-k"))))
    (request (concat deterred-transport-api-endpoint "v3/trips")
      :params `(("filters" . "")
                ("sorts" . "-DateTime")
                ("page" . ,(number-to-string page))
                ("pageSize" . ,(number-to-string deterred-transport-page-size)))
      :headers `(("Authorization" . ,(concat "Bearer " token))
                 ("x-ppa-language" . "ru"))
      :parser 'json-read
      :encoding 'utf-8
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (funcall callback data)))
      :error #'deterred-utils-on-request-error)))

(defun deterred-transport--transform-trip (trip)
  "Transform a single TRIP from API format to database format.

TRIP is an alist from the API response.

Return an alist with keys: id, source, timestamp, transport, route."
  (let* ((datetime-string (alist-get 'dateTime trip))
         (timestamp (time-convert
                     (encode-time (iso8601-parse datetime-string))
                     'integer))
         (vehicle-type-id (alist-get 'vehicleTypeId trip))
         (transport (or (alist-get vehicle-type-id
                                   deterred-transport-vehicle-type-mapping)
                        "Unknown"))
         (route (or (alist-get 'vehicleRoute trip) ""))
         (id (uuidgen-3 deterred-transport-uuid-namespace
                        (format "%s-podorozhnik" timestamp))))
    `((id . ,id)
      (source . "podorozhnik")
      (timestamp . ,timestamp)
      (transport . ,transport)
      (route . ,route))))

(defun deterred-transport--fetch-all-trips (token page total-pages callback &optional accumulated-trips)
  "Recursively fetch all trips from the API.

TOKEN is the authentication token.
PAGE is the current page number.
TOTAL-PAGES is the total number of pages (nil if unknown).
CALLBACK is called with all collected trips when all pages are fetched.
ACCUMULATED-TRIPS is the list of trips collected so far."
  (message "Fetching transport trips page %d%s..."
           page
           (if total-pages (format "/%d" total-pages) ""))
  (deterred-transport--api-get-trips
   token
   page
   (lambda (data)
     (let* ((items (alist-get 'items data))
            (pages-count (alist-get 'pagesCount data))
            (transformed-trips (mapcar #'deterred-transport--transform-trip items))
            (all-trips (append accumulated-trips transformed-trips)))
       (if (< page pages-count)
           (deterred-transport--fetch-all-trips
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

Return an alist with keys: timestamp, transport, route, or nil if invalid."
  (when (string-match
         (rx (+ digit) (+ space)  ; ticket number
             "Подорожник" (+ space)  ; description
             "Единый билет (ЕБ)" (+ space)  ; ticket type
             (group (+ (any digit ":"))) (+ space)  ; time (HH:MM:SS)
             (group (+ (any digit "."))) (+ space)  ; date (DD.MM.YYYY)
             (group (+ (not (any space)))) (+ space)  ; route
             (group (+ (any "А-Яа-я"))))  ; transport type
         line)
    (let* ((time-str (match-string 1 line))
           (date-str (match-string 2 line))
           (route (match-string 3 line))
           (transport-ru (match-string 4 line))
           (transport (or (alist-get transport-ru
                                     deterred-transport-russian-vehicle-type-mapping
                                     nil nil #'equal)
                          "Unknown"))
           (timestamp (deterred-transport--parse-pdf-datetime time-str date-str))
           (id (uuidgen-3 deterred-transport-uuid-namespace
                          (format "%s-podorozhnik" timestamp))))
      `((id . ,id)
        (source . "podorozhnik")
        (timestamp . ,timestamp)
        (transport . ,transport)
        (route . ,route)))))

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
         :attrs '(id source timestamp transport route)
         :conflict-action 'do-nothing))
      (deterred-db-mark-updated-batch db '("transport_trips"))
      (message "Imported %d trips from PDF" (length trips)))))

(defun deterred-transport-purge ()
  "Clear all transport data from the DETERRED database."
  (interactive)
  (when (y-or-n-p "Are you sure you want to purge transport data from DETERRED?")
    (let ((db (deterred-db--init)))
      (with-sqlite-transaction db
        (sqlite-execute db "DELETE FROM transport_trips")
        (deterred-db-mark-updated-batch db '("transport_trips"))))))

(defun deterred-transport-sync (&optional callback)
  "Sync transport trips from Podorozhnik API.

Call CALLBACK when done."
  (interactive)
  (deterred-transport--api-login
   (lambda (token)
     (deterred-transport--fetch-all-trips
      token 1 nil
      (lambda (all-trips)
        (let ((db (deterred-db--init)))
          (with-sqlite-transaction db
            (when all-trips
              (deterred-db-insert-unsafe
               db
               :table-name 'transport_trips
               :values all-trips
               :attrs '(id source timestamp transport route)
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

(cl-defmethod deterred-source-actions ((_source deterred-transport) &optional callback)
  "Run an action for the transport source.

Run CALLBACK when done."
  (deterred-source--actions-pick
   '(("Load transport PDF" deterred-transport-load-pdf nil)
     ("Sync transport trips" deterred-transport-sync nil)
     ("Purge transport data" deterred-transport-purge nil))
   callback))

(cl-defmethod deterred-source-sync ((_source deterred-transport) &optional callback)
  "Sync transport trips with DETERRED.

Call CALLBACK when done."
  (unless deterred-transport-login
    (user-error "Podorozhnik login not set!"))
  (unless deterred-transport-password
    (user-error "Podorozhnik password not set!"))
  (deterred-transport-sync callback))

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

(provide 'deterred-transport)
;;; deterred-transport.el ends here

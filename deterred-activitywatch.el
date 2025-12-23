;;; deterred-activitywatch.el --- ActivityWatch integration for DETERRED -*- lexical-binding: t -*-

;; Copyright (C) 2024 Korytov Pavel

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

;; ActivityWatch integration for DETERRED.
;;
;; This reads the AFK bucket and the currentwindow bucket; the latter
;; is processed to only include data when the computer isn't not AFK,
;; and then aggregated by applications per day.  I'm afraid saving
;; detailed timestamps will increaase the database size too much.
;;
;; `deterred-activitywatch-load' starts the loading flow from API.
;;
;; CSV import from old sqrt-data project is available via:
;; - `deterred-activitywatch-import-afkstatus-csv' for afkstatus data
;; - `deterred-activitywatch-import-notafk-window-csv' for notafk_window data
;;
;; Export ActivityWatch SQLite databases to JSON:
;; - `deterred-activitywatch-export-sqlite-to-json' converts SQLite
;;   databases to JSON format for import into ActivityWatch (which
;;   only supports JSON import)

;;; Code:
(require 'deterred-db)
(require 'deterred-locations)
(require 'deterred-source)
(require 'deterred-format)
(require 'deterred-utils)
(require 'request)
(require 'org-duration)
(require 'cl-lib)

(defconst deterred-activitywatch-uuid-namespace
  "6c4ea183-e81a-4e9d-bffc-11ed5aacb130")

(defcustom deterred-activitywatch-api "http://localhost:5600/api"
  "ActivityWatch API URL."
  :group 'deterred-sources
  :type 'string)

(defcustom deterred-activitywatch-convert-unknown "Emacs"
  "How to interpret \"unknown\" from the current window watcher.

I set this to \"Emacs\" because this usually means EXWM for me."
  :group 'deterred-sources
  :type 'string)

(defcustom deterred-activitywatch-show-top-in-summary 5
  "Show that many top apps in the daily ActivityWatch summary."
  :group 'deterred-sources
  :type 'number)

(defcustom deterred-activitywatch-export-bucket-types '("currentwindow" "afkstatus")
  "List of bucket types to export when converting SQLite to JSON."
  :group 'deterred-sources
  :type '(repeat string))

(defcustom deterred-activitywatch-app-map
  '(;; Firefox variants
    ("firefox" . "Firefox")
    ("firefox-default" . "Firefox")
    ("floorp" . "Firefox")
    ;; Chromium/Chrome variants
    ("Chromium-browser" . "Chromium")
    ("Google-chrome" . "Chrome")
    ("Google-chrome-stable" . "Chrome")
    ;; Edge variants
    ("Msedge" . "Microsoft Edge")
    ("Microsoft-edge" . "Microsoft Edge")
    ;; Brave variants
    ("Brave-browser" . "Brave")
    ;; Vivaldi variants
    ("Vivaldi-stable" . "Vivaldi")
    ;; Yandex variants
    ("Yandex-browser" . "Yandex")
    ;; Communication apps
    ("discord" . "Discord")
    ("TelegramDesktop" . "Telegram")
    ("Telegram-desktop" . "Telegram")
    (".telegram-desktop-real" . "Telegram")
    ("Skypeforlinux" . "Skype")
    ("VK" . "VK Messenger")
    ("Vk" . "VK Messenger")
    ("Vk-messenger" . "VK Messenger")
    ("Rocket.Chat" . "Rocket.Chat")
    ("Rocketchat-desktop" . "Rocket.Chat")
    ;; LibreOffice variants
    ("Soffice" . "LibreOffice")
    ("libreoffice-startcenter" . "LibreOffice")
    ;; PDF Viewers
    ("FoxitReader.enu.setup.2.4.4.0911(r057d814).x64.run" . "Foxit Reader")
    ;; GIMP variants
    ("Gimp-2.10" . "GIMP")
    ("Gimp-2.8" . "GIMP")
    ("Gimp" . "GIMP")
    ;; Inkscape variants
    (".inkscape-real" . "Inkscape")
    ;; Media Players
    ("Google Play Music Desktop Player" . "Google Play Music")
    ("google-musicmanager" . "Google Play Music")
    ;; Development tools
    ("Dbeaver" . "DBeaver")
    ("_Postman" . "Postman")
    ("Drawio" . "Draw.io")
    ("draw.io" . "Draw.io")
    ;; Virtualization
    ("VirtualBoxVM" . "VirtualBox")
    ("VirtualBox Manager" . "VirtualBox")
    ("VirtualBox Machine" . "VirtualBox")
    (".virt-manager-real" . "virt-manager")
    ("..virt-manager-real-real" . "virt-manager")
    ("Genymotion Player" . "Genymotion")
    ;; Wine variants
    ("wineboot.exe" . "Wine")
    ("winedbg.exe" . "Wine")
    ("wineconsole.exe" . "Wine")
    ("winhlp32.exe" . "Wine")
    ("notepad.exe" . "Wine Notepad")
    ("clipswin.exe" . "Wine")
    ("control.exe" . "Wine Control Panel")
    ;; Obsidian variants
    ("obsidian" . "Obsidian")
    ;; Electron variants (generic app wrapper)
    ("Electron7" . "Electron")
    ;; Blueman variants
    (".blueman-applet-real" . "Blueman")
    ("Blueman-applet" . "Blueman")
    ("Blueman-adapters" . "Blueman")
    ("Blueman-sendto" . "Blueman")
    ("Blueman-manager" . "Blueman")
    ;; BalenaEtcher variants
    ("Balena-etcher" . "balenaEtcher")
    ;; Font viewers
    ("gnome-font-viewer" . "Gnome-font-viewer")
    ;; AutoKey variants
    (".autokey-gtk-real" . "AutoKey")
    (".autokey-qt-real" . "AutoKey")
    ;; ScreenKey variants
    (".screenkey-real" . "ScreenKey")
    ;; Outline variants
    ("Outline-client" . "Outline")
    ;; Python system tools
    ("Blueberry.py" . "Blueberry")
    ("Cinnamon-settings.py" . "Cinnamon Settings")
    ("cinnamon-settings sound" . "Cinnamon Settings")
    ("cinnamon-settings network" . "Cinnamon Settings")
    ("System-config-printer.py" . "System Config Printer")
    ("MintSources.py" . "Mint Sources")
    ("MintUpdate.py" . "Mint Update")
    ("Mintinstall.py" . "Mint Install")
    ("Mintstick.py" . "Mint Stick")
    ;; PolicyKit
    ("Polkit-gnome-authentication-agent-1" . "PolicyKit")
    ;; Matplotlib variants
    ("matplotlib" . "Matplotlib")
    ;; FreeFileSync variants
    ("FreeFileSync_x86_64" . "FreeFileSync")
    ;; VeraCrypt variants
    ("Veracrypt" . "VeraCrypt")
    ;; Starsector variants
    ("Starsector 0.95a-RC15" . "Starsector")
    ;; JetBrains variants
    ("jetbrains-datagrip" . "JetBrains DataGrip")
    ("jetbrains-idea-ce" . "JetBrains IDEA")
    ("jetbrains-studio" . "JetBrains Studio")
    ("jetbrains-toolbox" . "JetBrains Toolbox")
    ;; Zen browser
    ("zen-alpha" . "Zen Browser"))
  "Mapping of app names to canonical names.

This is an alist where keys are variant names and values are the
canonical names that should be used in the database."
  :group 'deterred-sources
  :type '(alist :key-type string :value-type string))

(defun deterred-activitywatch--normalize-app-name (app)
  "Normalize APP name using `deterred-activitywatch-app-map'."
  (or (alist-get app deterred-activitywatch-app-map nil nil #'equal)
      app))

(defun deterred-activitywatch-merge-apps ()
  "Merge app variants in activitywatch_currentwindow_agg table.

This function uses `deterred-activitywatch-app-map' to consolidate
different names of the same application into a canonical name.

The merge process:
1. Fetch all records for apps mentioned in the map (both variants and
   canonical names)
2. Delete those records from the database
3. Merge records by (day, hostname, canonical_app)
4. Insert merged records back"
  (interactive)
  (let* ((db (deterred-db--init))
         (apps-to-fetch (delete-dups
                         (append (mapcar #'car deterred-activitywatch-app-map)
                                 (mapcar #'cdr deterred-activitywatch-app-map))))
         (total-fetched 0)
         (total-merged 0)
         merged-records)
    (message "Fetching records for %d apps..." (length apps-to-fetch))

    ;; Fetch all records for apps in the map
    (let ((all-records
           (deterred-db-select-alist
            db
            (format "SELECT day, hostname, app, total_duration
                     FROM activitywatch_currentwindow_agg
                     WHERE app IN (%s)"
                    (mapconcat (lambda (app) (format "'%s'" (string-replace "'" "''" app)))
                               apps-to-fetch ", ")))))
      (setq total-fetched (length all-records))
      (message "Fetched %d records" total-fetched)

      (when all-records
        (with-sqlite-transaction db
          ;; Delete all fetched records
          (sqlite-execute
           db
           (format "DELETE FROM activitywatch_currentwindow_agg WHERE app IN (%s)"
                   (mapconcat (lambda (app) (format "'%s'" (string-replace "'" "''" app)))
                              apps-to-fetch ", ")))

          ;; Merge records
          (let ((groups (make-hash-table :test 'equal)))
            (dolist (record all-records)
              (let* ((day (alist-get 'day record))
                     (hostname (alist-get 'hostname record))
                     (app (alist-get 'app record))
                     (duration (alist-get 'total_duration record))
                     (canonical-app (deterred-activitywatch--normalize-app-name app))
                     (key (list day hostname canonical-app)))
                (puthash key
                         (+ duration (gethash key groups 0))
                         groups)))

            (maphash
             (lambda (key total-duration)
               (push `((day . ,(nth 0 key))
                       (hostname . ,(nth 1 key))
                       (app . ,(nth 2 key))
                       (total_duration . ,total-duration))
                     merged-records))
             groups))

          (setq total-merged (length merged-records))
          (message "Merged into %d unique records" total-merged)

          ;; Insert merged records back
          (when merged-records
            (deterred-db-insert-unsafe
             db
             :table-name 'activitywatch_currentwindow_agg
             :values merged-records
             :conflict-action 'do-update
             :conflict-attrs '(day hostname app))
            (deterred-db-mark-updated db 'activitywatch_currentwindow_agg)))))

    (message "App merge complete: %d records fetched, merged into %d records"
             total-fetched total-merged)))

(defun deterred-activitywatch--bucket-store-afk (events hostname)
  "Store AFK EVENTS for HOSTNAME in DETERRED.

EVENTS is a list of events from the ActivityWatch API."
  (let ((db (deterred-db--init))
        values)
    (unless (seq-empty-p events)
      (cl-mapc
       (lambda (event)
         (let ((timestamp (time-convert
                           (encode-time
                            (iso8601-parse
                             (alist-get 'timestamp event)))
                           'integer)))
           (when (equal (alist-get 'status (alist-get 'data event)) "not-afk")
             (push `((hostname . ,hostname)
                     (notafk_start_timestamp . ,timestamp)
                     (notafk_end_timestamp . ,(round (+ timestamp (alist-get 'duration event)))))
                   values))))
       (seq-sort-by
        (lambda (event)
          (alist-get 'timestamp event))
        #'string-lessp
        events))
      (when values
        (with-sqlite-transaction db
          (deterred-db-insert-unsafe
           db :table-name 'activitywatch_notafk_period
           :values values
           :conflict-action 'do-nothing
           :conflict-attrs '(hostname notafk_start_timestamp notafk_end_timestamp))
          (deterred-db-mark-updated db 'activitywatch_notafk_period)))
      (message "Saved %d not-AFK periods from %s" (length values) hostname))))

(defun deterred-activitywatch--bucket-load-afk (bucket-id hostname &optional callback)
  "Parse AFK BUCKET-ID for HOSTNAME in DETERRED.

Call CALLBACK on success.  This is necessary because other bucket
loading logic might depend on the AFK bucket."
  (let* ((db (deterred-db--init))
         (start (caar
                 (sqlite-select
                  db "SELECT MAX(notafk_end_timestamp)
                      FROM activitywatch_notafk_period
                      WHERE hostname = ?" (list hostname)))))
    (request (concat deterred-activitywatch-api "/0/buckets/" bucket-id "/events")
      :parser 'json-read
      :params (when start
                `(("start" .            ; Some overlap to be sure
                   ,(format-time-string "%Y-%m-%dT%H:%M:%S"
                                        (- start (* 60 60 24)) t))))
      :encoding 'utf-8
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (deterred-activitywatch--bucket-store-afk data hostname)
                  (when callback
                    (funcall callback))))
      :error #'deterred-utils-on-request-error)))

(defun deterred-activitywatch--bucket-load-afk-recursive (buckets callback)
  "Recursively load afk BUCKETS and call CALLBACK when done."
  (if buckets
      (let ((bucket (car buckets)))
        (deterred-activitywatch--bucket-load-afk
         (alist-get 'id (cdr bucket))
         (alist-get 'hostname (cdr bucket))
         (lambda ()
           (deterred-activitywatch--bucket-load-afk-recursive
            (cdr buckets) callback))))
    (funcall callback)))

(defun deterred-activitywatch--load-currentwindow-get-days (db hostname created-at)
  "Get the list of ActivityWatch days to parse.

DB is the sqlite database object, HOSTNAME is the hostname, CREATED-AT
is the bucket creation date.

Return the list of decoded-times to process."
  (let* ((start-date (caar
                      (sqlite-select
                       db "SELECT MAX(day) FROM activitywatch_currentwindow_agg
                          WHERE hostname = ?"
                       (list hostname))))
         (start-timestamp (if start-date
                              (parse-time-string start-date)
                            (parse-time-string created-at)))
         (end-timestamp (decode-time (time-subtract
                                      (current-time)
                                      (* 60 60 24))))
         times)
    (setf (decoded-time-hour end-timestamp) 23
          (decoded-time-minute end-timestamp) 59
          (decoded-time-second end-timestamp) 59
          (decoded-time-hour start-timestamp) 0
          (decoded-time-minute start-timestamp) 0
          (decoded-time-second start-timestamp) 0)
    (let* ((start-time (encode-time start-timestamp))
           (end-time (encode-time end-timestamp)))
      (when created-at
        (setq start-time (time-add start-time (* 60 60 24))))
      (while (time-less-p start-time end-time)
        (push (decode-time start-time) times)
        (setq start-time (time-add start-time (* 60 60 24))))
      (nreverse times))))

(defun deterred-activitywatch--get-borders (db day hostname)
  "Get the borders of day for ActivityWatch currentwindow parsing.

DB is the sqlite database object.  DAY is the target day in the
decoded time form.  HOSTNAME is the hostname."
  (let ((start-day (copy-sequence day))
        (end-day (copy-sequence day)))
    (setf (decoded-time-hour start-day) 0
          (decoded-time-minute start-day) 0
          (decoded-time-second start-day) 0
          (decoded-time-hour end-day) 23
          (decoded-time-minute end-day) 59
          (decoded-time-second end-day) 59)
    (let* ((offset-timestamp (+
                              (time-convert (encode-time start-day) 'integer)
                              (* 12 60 60)))
           (offset (deterred-locations-offset-at offset-timestamp hostname db)))
      (setf (decoded-time-zone start-day) offset
            (decoded-time-zone end-day) offset))
    (cons (format-time-string "%FT%T%z" (encode-time start-day) t)
          (format-time-string "%FT%T%z" (encode-time end-day) t))))

(defun deterred-activitywatch--bucket-process-currentwindow-afk (db hostname events)
  "Add AFK data to currentwindow EVENTS and transform them.

Return a list of cons cells, where car is the timestamp, and cdr is
one of:
- afk-start (symbol)
- afk-end (symbol)
- a string with the active program name.

HOSTNAME is the hostname.  DB is the sqlite database object."
  (let* ((events-data
          (seq-sort-by
           #'car #'<
           (mapcar
            (lambda (event)
              (let ((timestamp (time-convert
                                (encode-time
                                 (iso8601-parse (alist-get 'timestamp event)))
                                'integer))
                    (app (alist-get 'app (alist-get 'data event))))
                (cons
                 timestamp
                 (if (and (equal app "unknown")
                          deterred-activitywatch-convert-unknown)
                     deterred-activitywatch-convert-unknown
                   app))))
            events)))
         (afk-data
          (mapcan
           (lambda (datum)
             (list (cons (nth 0 datum) 'notafk-start)
                   (cons (nth 1 datum) 'notafk-end)))
           (sqlite-select
            db
            "SELECT notafk_start_timestamp, notafk_end_timestamp
             FROM activitywatch_notafk_period
             WHERE hostname = ? AND notafk_end_timestamp >= ?
              AND notafk_start_timestamp <= ?"
            (list hostname (caar events-data) (caar (last events-data))))))
         (all-data
          (seq-sort-by #'car #'< (append events-data afk-data))))
    (cl-loop with active-notafk = nil
             for datum in all-data
             if (stringp (cdr datum)) collect datum
             else if (and (null active-notafk) (eq (cdr datum) 'notafk-start))
             do (setq active-notafk t) and collect datum
             else if (and active-notafk (eq (cdr datum) 'notafk-end))
             do (setq active-notafk nil) and collect datum)))

(defun deterred-activitywatch--bucket-process-currentwindow (db hostname events)
  "Group activitywatch currentwindow EVENTS.

This accounts for AFK data using
`deterred-activitywatch--bucket-process-currentwindow-afk'.

Return a hash map with app names with keys and total non-AFK seconds
spent there as values.

HOSTNAME is the hostname.  DB is the sqlite database object."
  (let ((data (deterred-activitywatch--bucket-process-currentwindow-afk
               db hostname events))
        (group-data (make-hash-table :test #'equal))
        active-notafk active-datum)
    (dolist (datum data)
      (cond
       ((eq (cdr datum) 'notafk-start)
        (setq active-notafk t)
        (when active-datum
          (setq active-datum
                (cons (car datum) (cdr active-datum)))))
       ((eq (cdr datum) 'notafk-end)
        (setq active-notafk nil)
        (when active-datum
          (puthash (cdr active-datum)
                   (+ (- (car datum) (car active-datum))
                      (gethash (cdr active-datum) group-data 0))
                   group-data)
          (setq active-datum nil)))
       ((stringp (cdr datum))
        ;; Ich bin immer noch hier
        (when (and active-notafk active-datum)
          (puthash (cdr active-datum)
                   (+ (- (car datum) (car active-datum))
                      (gethash (cdr active-datum) group-data 0))
                   group-data))
        (setq active-datum datum))))
    group-data))

(defun deterred-activitywatch--bucket-store-currentwindow (db day hostname events)
  "Save ActivityWatch current windows EVENTS on DAY to DB.

DB is the sqlite database object.  DAY is the day in decoded time
form.  HOSTNAME is the hostname.  EVENTS is the list of events, as
returned by the ActivityWatch API."
  (let ((data
         (cl-loop for app being the hash-keys of
                  (deterred-activitywatch--bucket-process-currentwindow
                   db hostname events)
                  using (hash-values duration)
                  collect `((hostname . ,hostname)
                            (day . ,(format-time-string "%F" (encode-time day)))
                            (app . ,(deterred-activitywatch--normalize-app-name app))
                            (total_duration . ,duration)))))
    (when data
      (with-sqlite-transaction db
        (deterred-db-insert-unsafe
         db
         :table-name 'activitywatch_currentwindow_agg
         :values data
         :conflict-action 'do-update
         :conflict-attrs '(day hostname app))
        (deterred-db-mark-updated db 'activitywatch_currentwindow_agg)))))

(defun deterred-activitywatch--load-currentwindow
    (bucket-id hostname &optional created-at callback days)
  "Load data from ActivityWatch's currentwindow BUCKET-ID on DAYS.

This is a recursive function.

HOSTNAME is the hostname.

CREATED-AT is the bucket creation date in the ISO8601 format (as
returned by the ActivityWatch API).  Only use it on the first pass.

If CALLBACK is non-nil, call it when the sync is done.

DAYS is a list of decoded times (as returned by `iso8601-parse' for
convinience).  Used internally for recursion."
  (if-let* ((db (deterred-db--init))
            (days (or days
                      (when created-at
                        (deterred-activitywatch--load-currentwindow-get-days
                         db hostname created-at))))
            (day (car days))
            (border (deterred-activitywatch--get-borders db day hostname)))
      (progn
        (message "Saving ActivityWatch day: %s" (format-time-string "%F" (encode-time day)))
        (request (concat deterred-activitywatch-api "/0/buckets/"
                         bucket-id "/events")
          :parser 'json-read
          :params `(("start" . ,(car border))
                    ("end" . ,(cdr border)))
          :encoding 'utf-8
          :success (cl-function
                    (lambda (&key data &allow-other-keys)
                      (deterred-activitywatch--bucket-store-currentwindow
                       db day hostname data)
                      (deterred-activitywatch--load-currentwindow
                       bucket-id hostname nil callback (cdr days))))
          :error #'deterred-utils-on-request-error))
    (when callback (funcall callback))))

(defun deterred-activitywatch-import-afkstatus-csv (file)
  "Import afkstatus data from CSV FILE into DETERRED.

FILE should be a CSV file with the following columns:
- id: unique identifier (ignored)
- hostname: the hostname
- timestamp: timestamp in YYYY-MM-DD HH:mm:dd.sss format (local time)
- duration: duration in seconds
- status: \\\"true\\\" (not afk) or \\\"false\\\" (afk)

The timestamp is in local time and will be converted to UTC using
location data from `deterred-locations-offset-at'.

Only rows with status=true are imported into activitywatch_notafk_period
table with columns (hostname, notafk_start_timestamp, notafk_end_timestamp).
Existing records are not overwritten."
  (interactive
   (list
    (read-file-name "Afkstatus CSV file: " nil nil nil nil
                    (lambda (f)
                      (or (file-directory-p f)
                          (string-match-p (rx ".csv" eos) f))))))
  (let ((db (deterred-db--init))
        (data (deterred-utils-csv-to-alist file))
        values)
    (dolist (row data)
      (when (equal (alist-get 'status row) "true")
        (let* ((hostname (alist-get 'hostname row))
               (timestamp-str (alist-get 'timestamp row))
               (duration (string-to-number (alist-get 'duration row)))
               ;; Parse timestamp and treat as UTC to get an approximate time
               (parsed-time (parse-time-string timestamp-str))
               (approx-timestamp (time-convert (encode-time parsed-time) 'integer))
               ;; Get the timezone offset at that approximate time
               (offset (deterred-locations-offset-at approx-timestamp hostname db))
               ;; Subtract offset to convert from local time to UTC
               (start-timestamp (- approx-timestamp offset))
               (end-timestamp (+ start-timestamp (round duration))))
          (push `((hostname . ,hostname)
                  (notafk_start_timestamp . ,start-timestamp)
                  (notafk_end_timestamp . ,end-timestamp))
                values))))
    (when values
      (with-sqlite-transaction db
        (deterred-db-insert-unsafe
         db :table-name 'activitywatch_notafk_period
         :values values
         :conflict-action 'do-nothing
         :conflict-attrs '(hostname notafk_start_timestamp notafk_end_timestamp))
        (deterred-db-mark-updated db 'activitywatch_notafk_period)))
    (message "Imported %d not-AFK periods from %s" (length values) file)))

(defun deterred-activitywatch-import-notafk-window-csv (file)
  "Import notafk_window data from CSV FILE into DETERRED.

FILE should be a CSV file with the following columns:
- hostname: the hostname
- date: date in YYYY-MM-DD format
- total_minutes: total minutes spent (in minutes, not seconds)
- app: application name
- title: window title (ignored)

Rows where app is \"AFK\" are ignored.  The data is grouped by hostname,
date, and app, summing total_minutes across all titles.  The aggregated
data is then imported into activitywatch_currentwindow_agg table with
columns \(hostname, day, app, total_duration\), where total_duration is
converted from minutes to seconds.

Existing records are not overwritten."
  (interactive
   (list
    (read-file-name "Notafk window CSV file: " nil nil nil nil
                    (lambda (f)
                      (or (file-directory-p f)
                          (string-match-p (rx ".csv" eos) f))))))
  (let ((db (deterred-db--init))
        (data (deterred-utils-csv-to-alist file))
        (aggregated (make-hash-table :test #'equal))
        values)
    ;; Aggregate data by (hostname, date, canonical_app)
    (dolist (row data)
      (let* ((hostname (alist-get 'hostname row))
             (day (alist-get 'date row))
             (app (alist-get 'app row))
             (canonical-app (deterred-activitywatch--normalize-app-name app))
             (total-minutes (string-to-number (alist-get 'total_minutes row)))
             (key (list hostname day canonical-app)))
        ;; Ignore AFK entries
        (unless (equal app "AFK")
          (puthash key
                   (+ total-minutes (gethash key aggregated 0))
                   aggregated))))
    ;; Convert aggregated data to values list
    (maphash
     (lambda (key total-minutes)
       (push `((hostname . ,(nth 0 key))
               (day . ,(nth 1 key))
               (app . ,(nth 2 key))
               (total_duration . ,(round (* total-minutes 60))))
             values))
     aggregated)
    (when values
      (with-sqlite-transaction db
        (deterred-db-insert-unsafe
         db
         :table-name 'activitywatch_currentwindow_agg
         :values values
         :conflict-action 'do-nothing
         :conflict-attrs '(day hostname app))
        (deterred-db-mark-updated db 'activitywatch_currentwindow_agg)))
    (message "Imported %d currentwindow records from %s (aggregated from %d rows)"
             (length values) file (length data))))

(defun deterred-activitywatch-export-sqlite-to-json (sqlite-file output-file)
  "Export ActivityWatch SQLITE-FILE to OUTPUT-FILE in JSON format.

I've made this because I have some old ActivityWatch databases, and
while the application supports importing old data, it can only do so
from its JSON exports.

SQLITE-FILE should be an ActivityWatch SQLite database with tables:
- bucketmodel: key, id, created, name, type, client, hostname
- eventmodel: id, bucket_id, timestamp, duration, datastr

Only buckets with types listed in
`deterred-activitywatch-export-bucket-types' are exported.

The output is a JSON file with the following structure:
- buckets: hash table keyed by bucket id
  - created: bucket creation timestamp
  - name: bucket name
  - type: bucket type
  - client: client name
  - hostname: hostname
  - events: list of events
    - timestamp: event timestamp
    - duration: event duration
    - data: event data (parsed from datastr JSON)"
  (interactive
   (list
    (read-file-name "ActivityWatch SQLite file: " nil nil t nil
                    (lambda (f)
                      (or (file-directory-p f)
                          (string-match-p (rx ".db" eos) f))))
    (read-file-name "Output JSON file: " nil nil nil nil
                    (lambda (f)
                      (or (file-directory-p f)
                          (string-match-p (rx ".json" eos) f))))))
  (let* ((aw-db (sqlite-open sqlite-file))
         (type-list (mapconcat (lambda (type) (format "'%s'" type))
                               deterred-activitywatch-export-bucket-types
                               ", "))
         (buckets-data
          (deterred-db-select-alist
           aw-db
           (format
            "SELECT key, id, created, name, type, client, hostname
             FROM bucketmodel
             WHERE type IN (%s)"
            type-list)))
         (result (make-hash-table :test 'equal)))
    (dolist (bucket buckets-data)
      (let* ((key (alist-get 'key bucket))
             (bucket-id (alist-get 'id bucket))
             (events-data
              (deterred-db-select-alist
               aw-db
               "SELECT timestamp, duration, datastr
                FROM eventmodel
                WHERE bucket_id = ?
                ORDER BY timestamp"
               (list key)))
             (events
              (mapcar
               (lambda (event)
                 (let ((datastr (alist-get 'datastr event)))
                   `((timestamp . ,(alist-get 'timestamp event))
                     (duration . ,(alist-get 'duration event))
                     (data . ,(json-read-from-string datastr)))))
               events-data))
             (bucket-data
              `((created . ,(alist-get 'created bucket))
                (id . ,(alist-get 'id bucket))
                (name . ,(alist-get 'name bucket))
                (type . ,(alist-get 'type bucket))
                (client . ,(alist-get 'client bucket))
                (hostname . ,(alist-get 'hostname bucket))
                (events . ,events))))
        (puthash bucket-id bucket-data result)))
    (sqlite-close aw-db)
    (with-temp-file output-file
      (insert (json-encode `((buckets . ,result)))))
    (message "Exported %d buckets to %s" (hash-table-count result) output-file)))

(defun deterred-activitywatch-load (&optional callback)
  "Load data from ActivityWatch API into DETERRED.

If CALLBACK is non-nil, call it when the sync is done."
  (interactive)
  (request (concat deterred-activitywatch-api "/0/buckets")
    :parser 'json-read
    :encoding 'utf-8
    :success
    (cl-function
     (lambda (&key data &allow-other-keys)
       ;; Load AFK buckets
       (deterred-activitywatch--bucket-load-afk-recursive
        (seq-filter
         (lambda (bucket)
           (equal (alist-get 'type (cdr bucket))
                  "afkstatus"))
         data)
        ;; Load the dependent buckets
        (lambda ()
          (let ((buckets-to-sync
                 (seq-filter
                  (lambda (bucket)
                    (member (alist-get 'type (cdr bucket)) '("currentwindow")))
                  data))
                (synced 0))
            (when (seq-empty-p buckets-to-sync)
              (when callback (funcall callback)))
            (dolist (bucket buckets-to-sync)
              (pcase (alist-get 'type (cdr bucket))
                ("currentwindow"
                 (deterred-activitywatch--load-currentwindow
                  (alist-get 'id (cdr bucket))
                  (alist-get 'hostname (cdr bucket))
                  (alist-get 'created (cdr bucket))
                  (lambda ()
                    (cl-incf synced)
                    (when (eql synced (seq-length buckets-to-sync))
                      (when callback (funcall callback))))))
                (_ nil))))))))
    :error #'deterred-utils-on-request-error))

;;;###autoload
(defclass deterred-activitywatch (deterred-source)
  ((name :initform "ActivityWatch")
   (warn-days :initform 1))
  "DETERRED source for ActivityWatch")

(cl-defmethod deterred-source-actions ((_source deterred-activitywatch) &optional callback)
  "Run an action for the ActivityWatch source.

Run CALLBACK when done."
  (deterred-source--actions-pick
   '(("Import afkstatus CSV" deterred-activitywatch-import-afkstatus-csv nil)
     ("Import notafk_window CSV" deterred-activitywatch-import-notafk-window-csv nil))
   callback))

(cl-defmethod deterred-source-sync ((source deterred-activitywatch) &optional callback)
  "Sync DETERRED with ActivityWatch.

Call CALLBACK when done.

SOURCE is an instance of `deterred-activitywatch'."
  (deterred-activitywatch-load callback))

(cl-defmethod deterred-source-range ((_source deterred-activitywatch) &optional db)
  "Get the data availability range for ActivityWatch.

DB is the sqlite database object.

Return a cons cell, with car as the start timestamp, and cdr as the
end timestamp."
  (let* ((db (or db (deterred-db--init)))
         (data (sqlite-select
                db "SELECT MIN(notafk_start_timestamp), MAX(notafk_end_timestamp)
                    FROM activitywatch_notafk_period")))
    (cons (caar data) (cadar data))))

(cl-defmethod deterred-source-range-summary
  ((_source deterred-activitywatch) timestamp &optional db)
  "Make ActivityWatch summary for TIMESTAMP.

DB is the sqlite database object."
  (let* ((db (or db (deterred-db--init)))
         (day (format-time-string "%F" timestamp))
         (total-minutes
          (or (alist-get
               'total (car
                       (deterred-db-select-alist
                        db "SELECT sum(total_duration) / 60 total
                        FROM activitywatch_currentwindow_agg
                        WHERE day = ?"
                        (list day))))
              0))
         (app-data
          (deterred-db-select-alist
           db "SELECT app, total_duration / 60 total
               FROM activitywatch_currentwindow_agg
               WHERE day = ?
               ORDER BY total DESC
               LIMIT ?"
           (list day deterred-activitywatch-show-top-in-summary)))
         (hostname-data
          (deterred-db-select-alist
           db "SELECT hostname, sum(total_duration) / 60 total
               FROM activitywatch_currentwindow_agg
               WHERE day = ?"
           (list day))))
    (when (> total-minutes 0)
      `((:short-description
         . ,(format "%s hours"
                    (org-duration-from-minutes total-minutes)))
        (:long-description
         . ,(deterred-format
             "Hostnames: "
             (f-mapconcat
              (f (org-duration-from-minutes (alist-get 'total iter))
                 " on " (f-acc "iter->'hostname"))
              hostname-data "; ")
             "\n"
             "Top " (f-num deterred-activitywatch-show-top-in-summary) " apps:\n"
             (f-mapconcat
              (f "- " (org-duration-from-minutes (alist-get 'total iter))
                 " on " (f-acc "iter->'app"))
              app-data)))))))

(provide 'deterred-activitywatch)
;;; deterred-activitywatch.el ends here

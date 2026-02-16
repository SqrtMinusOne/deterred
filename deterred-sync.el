;;; deterred-backup.el --- Sync functionality for DETERRED -*- lexical-binding: t -*-

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

;; Sync functionality for DETERRED.
;;
;; In a general case, syncing two databases is more complicated, but
;; this works given the specific needs of the package, albeit
;; imperfectly.
;;
;; The sync works in two directions:
;; - Current to others - by copying the current database into
;;   `deterred-sync-location' (`deterred-sync--execute-current')
;; - Others to current - by copying data from other databases in
;;   `deterred-sync-location' into the current one
;;   (`deterred-sync--execute-other')
;;
;; `deterred-sync-state' returns the list of known databases, and
;; `deterred-sync-get-action' returns the data on which action to
;; apply to which database.
;;
;; Naturally, the second flow (others to current) is more complicated.
;; The sync is implemented table-by-table, where each table is
;; processed by a particular strategy.  Some strategies process tables
;; in bulk.  The available strategies are listed in
;; `deterred-sync-strategies', and the order of application is listed
;; in `deterred-sync-config'.
;;
;; The strategies are as follows:
;; - `deterred-sync--replace' - replace the current table with other
;;   if its row count if higher.
;; - `deterred-sync--merge-keys' - merge the two tables by key
;;   attributes.
;; - `deterred-sync--merge-hostname' - add records from the other
;;   table to the current by using a hostname attribute and a
;;   timestamp attribute.
;;
;; Each strategy has its purpose.  The replace strategy only works if
;; a table is filled from a datasource outside the machine, e.g. as it
;; is with `deterred-wakatime', and if the records are never deleted.
;; That way it is guaranteed that the table with most records will be
;; most accurate.
;;
;; Otherwise, e.g. if the table is edited on two machines, one of the
;; edits will be lost.
;;
;; The merge-keys strategy preserves all edits, but it's expensive,
;; disallows row deletion and only syncs edits from NULL to not-NULL
;; values.  I might add an updated_at attribute to sync edits better,
;; but so far it's not there.
;;
;; The merge-hostname is less expensive than merge-keys because it
;; only syncs missing records (i.e. ones later than the latest
;; timestamp in the current database), but it doesn't sync edits.
;;
;; Each strategy has to accept the following arguments:
;; - `:table-name' or `:table-names' (in order to run
;;   `deterred-sync-strategy-sanity-check')
;; - `:dry-run'.  If non-nil, only print the actions to be done into a
;;   buffer.

;;; Code:
(require 'deterred-db)
(require 'deterred-utils)

(defcustom deterred-sync-location "~/.deterred/sync/"
  "The path to where sync DB snapshots are stored.  Change this."
  :group 'deterred
  :type 'string)

(defconst deterred-sync-config
  '(;; `deterred-activitywatch'
    (merge-hostname :table-name activitywatch_currentwindow_agg
                    :timestamp-attr day)
    (merge-hostname :table-name activitywatch_notafk_period
                    :timestamp-attr notafk_start_timestamp)
    ;; `deterred-digikam'
    (replace :table-names (digikam_photo digikam_album))
    ;; `deterred-habits'
    (replace :table-names (habit_record))
    ;; `deterred-locations'
    (merge-keys :table-name location)
    (merge-keys :table-name location_static_hostnames
                :key-attrs (hostname))
    ;; A choice is either to break adding new entries on two machines
    ;; (with replace) or to break deleting them (with merge-keys).  I
    ;; choose the former.
    (replace :table-names (location_times))
    ;; `deterred-social'
    (replace :table-names (mastodon_post_mention mastodon_post mastodon_account))
    (replace :table-names (reddit_comment reddit_post vk_post twitter_post))
    ;; `deterred-messengers'
    ;; (merge-with-extra-keys :table-name messenger_user
    ;;                        :extra-keys (telegram_id vk_id discord_id)
    ;;                        :update-tables-map ((messenger_chat . target_user_id)
    ;;                                            (messenger_message . sender_id)))
    (replace :table-names (messenger_user messenger_chat messenger_message))
    ;; `deterred-mpd'
    (merge-keys :table-name mpd_song)
    (merge-keys :table-name mpd_song_listened :key-attrs (mpd_song_id timestamp))
    ;; `deterred-org-roam'
    (replace :table-names (org_roam_node_tag org_roam_node_modification org_roam_node))
    ;; `deterred-org-journal-tags'
    (replace :table-names (org_journal_record_tag org_journal_tag org_journal_record))
    ;; `deterred-podcasts'
    (replace :table-names (podcasts_listened podcasts_feed))
    ;; `deterred-read-it-later'
    (replace :table-names (read_it_later_article read_it_later_host))
    ;; `deterred-transport'
    (merge-keys :table-name transport_trips)
    ;; `deterred-wakatime'
    (replace :table-names (
                           wakatime_branches wakatime_categories wakatime_editors
                           wakatime_entities wakatime_grand_total wakatime_languages
                           wakatime_machines wakatime_operating_systems
                           wakatime_projects)))
  "The order of sync strategy application for DETERRED.

This is a list.  In each element, the first item is a strategy name
\(which `deterred-sync-strategies' maps to functions) and the
remaining elements are the arguments in the form of plist.  The
argument list must have either a `:table-name' or `:table-names'
key (in order to run `deterred-sync-strategy-sanity-check').

See the comments in the `deterred-sync' package for more detail.")

(defconst deterred-sync-strategies
  '((merge-hostname . deterred-sync--merge-hostname)
    (replace . deterred-sync--replace)
    (merge-keys . deterred-sync--merge-keys)
    (merge-with-extra-keys . deterred-sync--merge-with-extra-keys))
  "An alist mapping DETERRED sync stragey names to functions.

See the comments in the `deterred-sync' package for more detail.")

(defun deterred-sync--get-file-name (hostname)
  "Get path to a database from HOSTNAME in the sync directory."
  (concat
   (file-name-as-directory
    (expand-file-name deterred-sync-location))
   "database." hostname ".db"))

(defun deterred-sync-state ()
  "Return the current DETERRED sync state.

The return value is a list of alist with the following keys:
- hostname
- current - if t, the entry corresponds to the current hostname
- db-time
- file-time"
  (let* ((files-data
          (mapcar
           (lambda (f)
             (cons
              (save-match-data
                (string-match (rx "database." (group (* (not "."))) ".db" eos) (car f))
                (match-string 1 (car f)))
              (time-convert
               (file-attribute-modification-time (cdr f))
               'integer)))
           (directory-files-and-attributes
            deterred-sync-location nil (rx "database." (* nonl) ".db" eos))))
         (db (deterred-db--init))
         (db-data
          (mapcar
           (lambda (d) (cons (alist-get 'hostname d) (alist-get 'last_synced d)))
           (deterred-db-select-alist db "select * from meta_sync_info")))
         (all-hostnames-except-current
          (seq-sort-by
           #'identity
           #'string-lessp
           (seq-filter
            (lambda (h) (not (equal h (system-name))))
            (seq-uniq
             (append
              (mapcar #'car files-data)
              (mapcar #'car db-data))))))
         (current-update-time
          (time-convert
           (file-attribute-modification-time
            (file-attributes deterred-db-location))
           'integer)))
    `(((hostname . ,(system-name))
       (current . ,t)
       (db-time . ,current-update-time)
       (file-time . ,(alist-get (system-name) files-data nil nil #'equal)))
      ,@(mapcar
         (lambda (h)
           `((hostname . ,h)
             (current . ,nil)
             (file-time . ,(alist-get h files-data nil nil #'equal))
             (db-time . ,(alist-get h db-data nil nil #'equal))))
         all-hostnames-except-current))))

(defun deterred-sync-get-action (entry)
  "Get sync action for ENTRY.

ENTRY is an alist as returned by `deterred-sync-state'.

The return value is an alist with the following keys:
- state - one of the following symbols:
  - ok - no actions necessary
  - pending - an action is necessary
  - error - something went wrong
- action - a function to be called.

Normal conditions:
1. db-time is equal to file-time - no actions necessary
2. db-time is greater than file-time for the current entry - the
  current database has to be copied to the sync directory
  (`deterred-sync--execute-current')
3. db-time is less than file-time for other entries - the other database
  has to be synced with the current database (TODO action)

Abnormal conditions:
4. db-time is less than file-time for the current entry - somehow the
  current database is older than its copy in the sync directory.  This
  might happen if the current database was restored from a backup.
  Manual resolution is necessary.
5. db-time is greater than file-name for other entires - the current
  database probably (!) has a more relevant copy of data than its
  target in the sync directory.  Manual resolution is necessary."
  ;; 1
  (cond ((eql (alist-get 'file-time entry)
              (alist-get 'db-time entry))
         `((state . ok)
           (action . ,(lambda () (message "No action necessary.")))))
        ;; 2
        ((and (alist-get 'current entry)
              (> (alist-get 'db-time entry)
                 (or (alist-get 'file-time entry) 0)))
         `((state . pending)
           (action . deterred-sync--execute-current)))
        ;; 3
        ((and (not (alist-get 'current entry))
              (< (or (alist-get 'db-time entry) 0)
                 (or (alist-get 'file-time entry) 0)))
         `((state . pending)
           (action
            . ,(lambda ()
                 (deterred-sync--execute-other
                  (alist-get 'hostname entry)
                  (eq current-prefix-arg '(4)))))))
        ;; 4
        ((and (alist-get 'current entry)
              (< (alist-get 'db-time entry)
                 (or (alist-get 'file-time entry) 0)))
         `((state . error)
           (action
            . ,(lambda ()
                 (user-error "The current database is older than its copy in the sync directory")))))
        ;; 5
        ((and (not (alist-get 'current entry))
              (< (or (alist-get 'db-time entry) 0)
                 (or (alist-get 'file-time entry) 0)))
         `((state . error)
           (action
            . ,(lambda ()
                 (user-error "The current database may have a more relevant copy than its target in the sync directory")))))))

(defun deterred-sync--execute-current ()
  "Copy the current database to the sync location."
  (copy-file deterred-db-location
             (deterred-sync--get-file-name (system-name))
             t t t t))

(defun deterred-sync--execute-other (hostname dry-run)
  "Sync data from HOSTNAME database into the current database.

If DRY-RUN is non-nil, only print the actions to be done into a
buffer without executing them."
  (let* ((db (deterred-db--init))
         (db-other-path (deterred-sync--get-file-name hostname))
         (file-time (time-convert
                     (file-attribute-modification-time
                      (file-attributes db-other-path))
                     'integer))
         (log-buffer (get-buffer-create
                      (format "*deterred-sync-%s*" hostname))))
    (with-current-buffer log-buffer
      (erase-buffer)
      (insert (format "Syncing %s to current database%s\n\n"
                      hostname (if dry-run " (DRY RUN)" ""))))
    (sqlite-execute db (format "ATTACH DATABASE '%s' AS other_db" db-other-path))
    (let ((current-migrations (caar (sqlite-select db "SELECT COUNT(*) FROM main.meta_db_migrations")))
          (other-migrations (caar (sqlite-select db "SELECT COUNT(*) FROM other_db.meta_db_migrations"))))
      (unless (= current-migrations other-migrations)
        (sqlite-execute db "DETACH DATABASE other_db")
        (user-error "Schema mismatch: current DB has %d migrations, %s has %d"
                    current-migrations hostname other-migrations)))
    (unwind-protect
        (with-sqlite-transaction db
          (dolist (strategy-config deterred-sync-config)
            (let* ((strategy-name (car strategy-config))
                   (strategy-args (cdr strategy-config))
                   (strategy-fn (alist-get strategy-name deterred-sync-strategies)))
              (unless strategy-fn
                (error "Unknown strategy: %s" strategy-name))
              (with-current-buffer log-buffer
                (insert (format "Executing strategy: %s %s\n" strategy-name strategy-args)))
              (apply strategy-fn db
                     (append strategy-args `(
                                             :hostname ,hostname
                                             :dry-run ,dry-run
                                             :log-buffer ,log-buffer)))
              (with-current-buffer log-buffer
                (insert "\n"))))
          (unless dry-run
            (sqlite-execute
             db
             "INSERT INTO meta_sync_info (hostname, last_synced)
              VALUES (?, ?)
              ON CONFLICT (hostname)
              DO UPDATE SET last_synced = ?"
             (list hostname file-time file-time))))
      (sqlite-execute db "DETACH DATABASE other_db")
      (with-current-buffer log-buffer
        (goto-char (point-min)))
      (if dry-run
          (display-buffer log-buffer)
        (kill-buffer log-buffer)))))

(cl-defun deterred-sync--replace (db &key table-names hostname dry-run log-buffer)
  "Replace tables in DB with data from other_db if any has more rows.

TABLE-NAMES is a list of table names to process.  If at least one
table in TABLE-NAMES has more rows in other_db, all tables are
replaced.  If DRY-RUN is non-nil, only print the actions to
LOG-BUFFER without executing them.

HOSTNAME is unused."
  (let (should-replace)
    (dolist (table-name table-names)
      (let* ((table-str (if (symbolp table-name) (symbol-name table-name) table-name))
             (count-current (caar (sqlite-select
                                   db
                                   (format "SELECT COUNT(*) FROM main.%s" table-str))))
             (count-other (caar (sqlite-select
                                 db
                                 (format "SELECT COUNT(*) FROM other_db.%s" table-str)))))
        (when log-buffer
          (with-current-buffer log-buffer
            (insert (format "  Table %s: current=%d, other=%d\n"
                            table-str count-current count-other))))
        (when (> count-other count-current)
          (setq should-replace t))))
    (when (and should-replace (not dry-run))
      (dolist (table-name table-names)
        (let ((table-str (if (symbolp table-name) (symbol-name table-name) table-name)))
          (sqlite-execute db (format "DELETE FROM main.%s" table-str))))
      (dolist (table-name (reverse table-names))
        (let* ((table-str (if (symbolp table-name) (symbol-name table-name) table-name))
               (sample-row (car (deterred-db-select-alist
                                 db
                                 (format "SELECT * FROM other_db.%s LIMIT 1" table-str)))))
          (when sample-row
            (let* ((attrs (mapcar #'car sample-row))
                   (attrs-str (mapconcat #'symbol-name attrs ", ")))
              (sqlite-execute db
                              (format "INSERT INTO main.%s (%s) SELECT %s FROM other_db.%s"
                                      table-str attrs-str attrs-str table-str))))
          (deterred-db-mark-updated db table-name))))))

(cl-defun deterred-sync--merge-hostname
    (db &key table-name hostname (hostname-attr 'hostname)
        (timestamp-attr 'timestamp) dry-run log-buffer)
  "Merge records from other_db to DB by hostname and timestamp.

Only syncs missing records from HOSTNAME (ones later than or equal to
the latest timestamp for that HOSTNAME in DB).  TABLE-NAME is the
table name to sync.  HOSTNAME-ATTR is the name of the hostname column,
TIMESTAMP-ATTR is the name of the timestamp column.  If DRY-RUN is
non-nil, only print the actions to LOG-BUFFER without executing them."
  (let* ((table-str (if (symbolp table-name) (symbol-name table-name) table-name))
         (hostname-str (if (symbolp hostname-attr) (symbol-name hostname-attr) hostname-attr))
         (timestamp-str (if (symbolp timestamp-attr) (symbol-name timestamp-attr) timestamp-attr))
         (max-timestamp (caar
                         (sqlite-select db
                                        (format "SELECT MAX(%s) FROM main.%s WHERE %s = ?"
                                                timestamp-str table-str hostname-str)
                                        (list hostname))))
         (count (caar
                 (if max-timestamp
                     (sqlite-select
                      db
                      (format "SELECT COUNT(*) FROM other_db.%s WHERE %s >= ? AND %s = ?"
                              table-str timestamp-str hostname-str)
                      (list max-timestamp hostname))
                   (sqlite-select
                    db
                    (format "SELECT COUNT(*) FROM other_db.%s WHERE %s = ?"
                            table-str hostname-str)
                    (list hostname))))))
    (when log-buffer
      (with-current-buffer log-buffer
        (insert (format "  Table %s: %d new records from %s\n"
                        table-str count hostname))))
    (when (and (> count 0) (not dry-run))
      (let* ((sample-row (car (deterred-db-select-alist
                               db
                               (format "SELECT * FROM other_db.%s LIMIT 1" table-str))))
             (attrs (mapcar #'car sample-row))
             (attrs-str (mapconcat #'symbol-name attrs ", "))
             (where-clause
              (if max-timestamp
                  (format "WHERE %s >= %s AND %s = %s"
                          timestamp-str (deterred-db--format-value max-timestamp)
                          hostname-str (deterred-db--escape hostname))
                (format "WHERE %s = %s"
                        hostname-str (deterred-db--escape hostname)))))
        (sqlite-execute db
                        (format "INSERT OR IGNORE INTO main.%s (%s) SELECT %s FROM other_db.%s %s"
                                table-str attrs-str attrs-str table-str where-clause)))
      (deterred-db-mark-updated db table-name))))

(cl-defun deterred-sync--merge-keys (db &key table-name hostname (key-attrs '(id)) dry-run log-buffer)
  "Merge records from other_db to DB by key attributes.

TABLE-NAME is the target table name.

For each row in other_db, insert it into DB.  On conflict with
existing keys, update NULL values in DB with non-NULL values from
other_db.  KEY-ATTRS specifies the key columns.

If DRY-RUN is non-nil, only print the actions to LOG-BUFFER without
executing them.

HOSTNAME is unused."
  (let* ((table-str (if (symbolp table-name) (symbol-name table-name) table-name))
         (count (caar (sqlite-select db (format "SELECT COUNT(*) FROM other_db.%s" table-str)))))
    (when log-buffer
      (with-current-buffer log-buffer
        (insert (format "  Table %s: %d rows to merge\n"
                        table-str count))))
    (unless (or dry-run (= count 0))
      (let* ((sample-row (car (deterred-db-select-alist
                               db
                               (format "SELECT * FROM other_db.%s LIMIT 1" table-str))))
             (attrs (mapcar #'car sample-row))
             (non-key-attrs (seq-difference attrs key-attrs))
             (key-attrs-str (mapconcat #'symbol-name key-attrs ", "))
             (attrs-str (mapconcat #'symbol-name attrs ", "))
             (update-clauses
              (mapconcat (lambda (attr)
                           (let ((attr-str (symbol-name attr)))
                             (format "%s = COALESCE(%s.%s, excluded.%s)"
                                     attr-str table-str attr-str attr-str)))
                         non-key-attrs ", "))
             (query (format "INSERT INTO main.%s (%s) SELECT %s FROM other_db.%s WHERE true ON CONFLICT (%s) DO UPDATE SET %s"
                            table-str attrs-str attrs-str table-str key-attrs-str update-clauses)))
        (sqlite-execute db query))
      (deterred-db-mark-updated db table-name))))

(defun deterred-sync--select-other-with-main-matches
    (db table-name key-attr extra-keys)
  "Select other-only rows LEFT JOINed to main-only records by EXTRA-KEYS.

DB is the SQLite connection.  TABLE-NAME and KEY-ATTR identify the
table and its key column.  EXTRA-KEYS is a list of columns to match on.

Return alists with all columns from other_db plus `main_match_id'
\(nil when no match was found)."
  (let* ((table-str (deterred-utils-ensure-string table-name))
         (key-str (deterred-utils-ensure-string key-attr))
         (join-clause
          (mapconcat
           (lambda (ek)
             (let ((ek-str (deterred-utils-ensure-string ek)))
               (format "m.%s = o.%s" ek-str ek-str)))
           extra-keys " OR ")))
    (deterred-db-select-alist
     db
     (format "SELECT DISTINCT o.*, m.%s AS main_match_id
FROM other_db.%s o
LEFT JOIN main.%s m ON (%s)
WHERE o.%s != m.%s OR m.%s IS NULL"
             key-str table-str table-str join-clause
             key-str key-str key-str))))

(defun deterred-sync--resolve-extra-key-merges
    (db table-name key-attr update-tables-map rows dry-run log-buffer)
  "Process merged rows: remap linked tables and delete main records.

ROWS is the result of `deterred-sync--select-other-with-main-matches'.
Only rows with non-nil `main_match_id' are processed.  For each,
linked tables in UPDATE-TABLES-MAP are remapped from the main
KEY-ATTR to the other KEY-ATTR, then the main record is deleted.

DB is the SQLite connection.  TABLE-NAME and KEY-ATTR identify the
table.  If DRY-RUN is non-nil, only log to LOG-BUFFER."
  (when-let* ((merge-rows (seq-filter (lambda (r) (alist-get 'main_match_id r)) rows))
              (table-str (deterred-utils-ensure-string table-name))
              (key-str (deterred-utils-ensure-string key-attr)))
    (dolist (row merge-rows)
      (let ((main-id (alist-get 'main_match_id row))
            (other-id (alist-get key-attr row)))
        (when log-buffer
          (with-current-buffer log-buffer
            (insert (format "    Merge: main %s -> other %s\n"
                            main-id other-id))))
        (unless dry-run
          (dolist (mapping update-tables-map)
            (let ((linked-table (deterred-utils-ensure-string (car mapping)))
                  (fk-col (deterred-utils-ensure-string (cdr mapping))))
              (sqlite-execute
               db
               (format "UPDATE main.%s SET %s = ? WHERE %s = ?"
                       linked-table fk-col fk-col)
               (list other-id main-id)))))))
    (unless dry-run
      (sqlite-execute
       db
       (format "DELETE FROM main.%s WHERE %s IN (%s)"
               table-str key-str
               (mapconcat
                (lambda (r) (deterred-db--format-value
                             (alist-get 'main_match_id r)))
                merge-rows ", "))))))

(defun deterred-sync--insert-unmatched-other
    (db table-name rows dry-run log-buffer)
  "Insert other-only rows that have no main match by extra keys.

ROWS is the result of `deterred-sync--select-other-with-main-matches'.
Only rows with nil `main_match_id' are inserted.  The
`main_match_id' column is stripped before inserting.

DB is the SQLite connection.  TABLE-NAME identifies the table.
If DRY-RUN is non-nil, only log to LOG-BUFFER."
  (let ((new-rows (seq-filter
                   (lambda (r) (not (alist-get 'main_match_id r)))
                   rows)))
    (when log-buffer
      (with-current-buffer log-buffer
        (insert (format "    Insert: %d new, %d merged\n"
                        (length new-rows)
                        (- (length rows) (length new-rows))))))
    (when (and new-rows (not dry-run))
      (deterred-db-insert-unsafe
       db :table-name table-name
       :values (mapcar (lambda (r)
                         (assq-delete-all 'main_match_id (copy-alist r)))
                       new-rows)))))

(cl-defun deterred-sync--merge-with-extra-keys
    (db &key table-name hostname (key-attr 'id) extra-keys update-tables-map
        dry-run log-buffer)
  "Merge records by KEY-ATTR accounting for merged rows by EXTRA-KEYS.

This works in one weird case - if TABLE-NAME has multiple nullable
columns with unique attributes that can be merged.  This is currently
used in `deterred-messengers', where the users table has multiple
columns with ids in different messengers.

EXTRA-KEYS is a list of additional key attributes in table.
UPDATE-TABLES-MAP is an alist with tables linked to TABLE-NAME, where
car is the linked table name, and cdr is the foreign key to
TABLE-NAME.KEY-ATTR.

The works as follows:
- Find all records by KEY-ATTR, present in main but not in other_db
  and vice versa.
- For each extra record in main, look for a record in other_db with
  the same value of extra_key.  If found, this is the merged record.
  Then:
  - Set the value of extra_key in main to the value of extra_key in
    other_db.
  - Update all linked tables in UPDATE-TABLES-MAP accordingly.
  - Delete the extra record in main.
- For each extra record in other_db, look for a record in main with
  the same value of extra_key.  If found, ignore this extra record
  because it was merged.  Otherwise, insert the record in main.
- For each record in other_db that was not merged, update NULL values
  unset in it main but set in other_db, like
  `deterred-sync--merge-keys'.

If DRY-RUN is non-nil, only print the actions to LOG-BUFFER without
executing them.  DB is the SQLite connection object.

HOSTNAME is unused."
  (let* ((table-str (deterred-utils-ensure-string table-name))
         (rows (deterred-sync--select-other-with-main-matches
                db table-name key-attr extra-keys)))
    (when log-buffer
      (with-current-buffer log-buffer
        (insert (format "  Table %s: %d records only in other\n"
                        table-str (length rows)))))
    (with-sqlite-transaction db
      (deterred-sync--resolve-extra-key-merges
       db table-name key-attr update-tables-map rows dry-run log-buffer)
      (deterred-sync--insert-unmatched-other
       db table-name rows dry-run log-buffer)
      (deterred-sync--merge-keys
       db :table-name table-name :key-attrs (list key-attr)
       :dry-run dry-run :log-buffer log-buffer))))

(defun deterred-sync-config-sanity-check ()
  "Sanity check for DETERRED sync.

Checks if:
- All tables (except for meta_*) have a sync strategy that covers
  them
- The sync config (`deterred-sync-config' doesn't have any extra
  tables."
  (interactive)
  (let* ((db (deterred-db--init))
         (all-tables
          (mapcar #'car
                  (sqlite-select
                   db
                   "SELECT name FROM sqlite_master WHERE type='table' AND name NOT LIKE 'meta_%'")))
         (config-tables
          (seq-uniq
           (apply #'append
                  (mapcar (lambda (config)
                            (let ((plist (cdr config)))
                              (cond ((plist-get plist :table-name)
                                     (list (symbol-name (plist-get plist :table-name))))
                                    ((plist-get plist :table-names)
                                     (mapcar #'symbol-name (plist-get plist :table-names)))
                                    (t nil))))
                          deterred-sync-config))))
         (missing-in-config (seq-difference all-tables config-tables #'equal))
         (extra-in-config (seq-difference config-tables all-tables #'equal))
         (buffer (get-buffer-create "*deterred-sync-sanity-check*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "DETERRED Sync Configuration Sanity Check\n")
      (insert "=========================================\n\n")
      (if (and (null missing-in-config) (null extra-in-config))
          (insert "All checks passed!\n")
        (when missing-in-config
          (insert "Tables missing in sync config:\n")
          (dolist (table missing-in-config)
            (insert (format "  - %s\n" table)))
          (insert "\n"))
        (when extra-in-config
          (insert "Tables in sync config but not in database:\n")
          (dolist (table extra-in-config)
            (insert (format "  - %s\n" table)))
          (insert "\n")))
      (goto-char (point-min)))
    (display-buffer buffer)))

(provide 'deterred-sync)
;;; deterred-sync.el ends here

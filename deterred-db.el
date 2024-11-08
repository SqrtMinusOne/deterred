;;; deterred-db.el --- TODO -*- lexical-binding: t -*-

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

;; TODO

;;; Code:
(require 'seq)

(defcustom deterred-db-location "~/.deterred/database.db"
  "The path to file where the Deterred database is stored."
  :group 'deterred
  :type 'string)

(defcustom deterred-db-backups-location "~/.deterred/backups/"
  "The path to where the backups are stored.  Change this."
  :group 'deterred
  :type 'string)

(defconst deterred-db-migrations-location
  (or (and load-file-name (concat (file-name-directory load-file-name) "migrations"))
      (concat default-directory "migrations"))
  "The path to Deterred migrations.")

(defconst deterred-db--query-create-version-table
  "CREATE TABLE meta_db_migrations (
    file varchar(255) primary key
  )")

(defun deterred-db--escape (string)
  "Escape STRING for sqlite.

This is not sanitization.  Don't use for untrusted inputs."
  (when string
    (concat
     "'"
     (string-replace "'" "''" string)
     "'")))

(defun deterred-db--is-initialized (db)
  "Return non-nil if the `deterred' database has been initialized.

DB is a sqlite database object."
  (sqlite-select
   db "select * from sqlite_master where type = 'table'
        and name = 'meta_db_migrations'"))

(defun deterred-db--migrations-get-pending (db)
  "Return a list of pending migrations for the `deterred' database.

DB is a sqlite database object."
  (let* ((all-migrations (thread-last
                           deterred-db-migrations-location
                           (directory-files)
                           (seq-filter (lambda (f) (string-match-p (rx ".sql" eos) f)))
                           (seq-sort-by #'> #'string-lessp)))
         (applied-migrations
          (mapcar #'car (sqlite-select db "select file from meta_db_migrations")))
         (pending-migrations (seq-difference all-migrations applied-migrations #'equal))
         (unknown-migrations (seq-difference applied-migrations all-migrations #'equal)))
    (when unknown-migrations
      (user-error "Unknown migrations in db: %s.  Maybe update Deterred?"))
    pending-migrations))

(defun deterred-db--migrations-execute (db pending)
  "Execute migrations on database.

DB is a sqlite database object.  PENDING is a list of migrations."
  (dolist (file pending)
    (let* ((sql-string
            (with-temp-buffer
              (insert-file-contents
               (concat deterred-db-migrations-location "/" file))
              (buffer-string)))
           (statements
            (thread-last
              (split-string sql-string ";" t)
              (mapcar #'string-trim)
              (seq-filter (lambda (s) (not (string-empty-p s)))))))
      (with-sqlite-transaction db
        (dolist (statement statements)
          (sqlite-execute db statement))
        (sqlite-execute db "insert into meta_db_migrations values (?)" (list file))))))

(defun deterred-db--init ()
  "Initialize the `deterred' database.  Return a sqlite object."
  (let* ((db-loc (expand-file-name deterred-db-location))
         (db-dir (directory-file-name
                  (file-name-directory db-loc)))
         (backups-dir
          (directory-file-name
           (file-name-directory
            (expand-file-name deterred-db-backups-location)))))
    (mkdir db-dir t)
    (mkdir backups-dir t)
    (let ((db (sqlite-open db-loc)))
      (sqlite-pragma db "foreign_keys = ON")
      (unless (deterred-db--is-initialized db)
        (sqlite-execute db deterred-db--query-create-version-table))
      (deterred-db--migrations-execute
       db (deterred-db--migrations-get-pending db))
      db)))

(defun deterred-db--mark-updated (table-name db)
  (sqlite-execute
   db "INSERT INTO meta_table_updates (table_name, last_updated)
       VALUES (?, unixepoch(CURRENT_TIMESTAMP))
         ON CONFLICT (table_name)
         DO UPDATE SET last_updated = unixepoch(CURRENT_TIMESTAMP)"
   (list table-name)))

(provide 'deterred-db)
;;; deterred-db.el ends here

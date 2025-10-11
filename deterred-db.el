;;; deterred-db.el --- Database interface for DETERRED -*- lexical-binding: t -*-

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

;; Database interface for DETERRED.
;;
;; To initialise the database, call `deterred-db--init'.  It will
;; execute all new migrations in `deterred-db-migrations-location' and
;; return the database object.
;;
;; The connection is initialised only once, but some parts of the
;; package pass the database object between function calls anyway.
;;
;; To implement sync, the database stores table update times in the
;; meta_table_updates folder.  Set these with
;; `deterred-db-mark-updated' and `deterred-db-mark-updated-batch'.
;;
;; Some other useful functions:
;; - `deterred-db-insert-unsafe' to insert data;
;; - `deterred-db-cleanup-unsafe' to delete data;
;; - `deterred-db-select-alist' to select rows into alists.

;;; Code:
(require 'seq)
(require 'sqlite)

(defcustom deterred-db-location "~/.deterred/database.db"
  "The path to file where the Deterred database is stored."
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

(defvar deterred-db--conn nil
  "The SQLite connection object.")

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

(defun deterred-db--init (&optional force-reconnect)
  "Initialize the `deterred' database.  Return a sqlite object.

If FORCE-RECONNECT is t, create a new connection.  Otherwise, reusing
the existing one.

In principle, unused connections should be garbage collected, but I'm
not sure they do because I've had the \"too many open files\" error a
few times.  Hence this approach."
  (if (and deterred-db--conn (not force-reconnect))
      deterred-db--conn
    (let* ((db-loc (expand-file-name deterred-db-location))
           (db-dir (directory-file-name
                    (file-name-directory db-loc))))
      (mkdir db-dir t)
      (let ((db (sqlite-open db-loc)))
        (sqlite-pragma db "foreign_keys = ON")
        (unless (deterred-db--is-initialized db)
          (sqlite-execute db deterred-db--query-create-version-table))
        (deterred-db--migrations-execute
         db (deterred-db--migrations-get-pending db))
        (setq deterred-db--conn db)
        db))))

(defun deterred-db-mark-updated (db table-name)
  "Mark TABLE-NAME as updated.

TABLE-NAME is either string or symbol.  DB is the sqlite database
object."
  (when (symbolp table-name)
    (setq table-name (symbol-name table-name)))
  (sqlite-execute
   db "INSERT INTO meta_table_updates (table_name, last_updated)
       VALUES (?, unixepoch(CURRENT_TIMESTAMP))
         ON CONFLICT (table_name)
         DO UPDATE SET last_updated = unixepoch(CURRENT_TIMESTAMP)"
   (list table-name)))

(defun deterred-db-mark-updated-batch (db table-names)
  "Mark TABLE-NAMES as updated.

TABLE-NAMES is a list of strings or symbols.  DB is the sqlite
database object."
  (mapcar (lambda (name) (deterred-db-mark-updated
                          db name))
          table-names))

(defun deterred-db-execute-trace (db query &optional values)
  "Excecute SQLite query and throw a detailed error.

DB, QUERY and VALUES are the same as `sqlite-execute'."
  (condition-case err
      (sqlite-execute db query values)
    (error
     (message "Error: %s" err)
     (message "Query: %s" query)
     (message "Values: %s" values)
     (signal (car err) (cdr err)))))

(defun deterred-db--format-value (value)
  "Format VALUE for use in SQLite queries.  Unsafe."
  (cond ((null value) "NULL")
        ((integerp value) (number-to-string value))
        ((floatp value) (number-to-string value))
        ((stringp value) (deterred-db--escape value))
        (t (error "Bad type for `deterred-db--format-value': %s"
                  value))))

(defun deterred-db--insert-format-values (values attrs)
  "Format VALUES for use in SQLite insert query.  Unsafe.

ATTRS is the list of attributes."
  (mapconcat
   (lambda (datum)
     (concat
      "("
      (string-join
       (mapcar (lambda (attr)
                 (deterred-db--format-value (alist-get attr datum)))
               attrs)
       ", ")
      ")"))
   values
   ",\n"))

(defun deterred-db--insert-format-conflict (attrs conflict-attrs conflict-action)
  "Format an on conflict clause for SQLite insert query.

ATTRS is the list of all attributes, CONFLICT-ATTRS is the subset of
ATTRS on which the conflict is checked, CONFLICT-ACTION is either
\\='do-nothing and \\='do-update."
  (let ((non-conflict-attrs (when conflict-attrs
                              (seq-difference attrs conflict-attrs))))
    (concat "ON CONFLICT "
            (when conflict-attrs
              (concat
               "("
               (string-join (mapcar #'symbol-name conflict-attrs) ",")
               ") "))
            (pcase conflict-action
              ('do-nothing "DO NOTHING")
              ('do-update (concat "DO UPDATE SET "
                                  (mapconcat (lambda (a)
                                               (format "%s=excluded.%s" a a))
                                             non-conflict-attrs ", ")))
              (_ (error "Unknown conflict-action: %s" conflict-action))))))

(cl-defun deterred-db-insert-unsafe
    (db &key table-name values attrs conflict-attrs conflict-action)
  "Unsafely insert VALUES into DB.

VALUES have to be a list of alists, where the keys are column names.

DB is the sqlite database object, TABLE-NAME is the name of the target
table.  The remaining keys are optional.

ATTRS is the list of column names; if not given, the keys of the first
items in VALUES.

CONFLICT-ACTION can be \\='do-nothing or \\='do-update.  If the
latter, CONFLICT-ATTRS is also required."
  (let* ((attrs (or attrs (mapcar #'car (car values))))
         (values-query (deterred-db--insert-format-values values attrs))
         (conflict-query
          (when conflict-action
            (deterred-db--insert-format-conflict
             attrs conflict-attrs conflict-action)))
         (query (concat "INSERT INTO " (symbol-name table-name)
                        " (" (mapconcat #'symbol-name attrs ",") ") "
                        "VALUES "
                        values-query
                        " "
                        conflict-query)))
    (when values
      (deterred-db-execute-trace db query))))

(defun deterred-db-cleanup-unsafe (db table-name id-attr ids)
  "Delete records from TABLE-NAME with ids not in IDS.

ID-ATTR is the name of ID attribute, DB is the sqlite database
object."
  (let* ((table-name (if (symbolp table-name) (symbol-name table-name) table-name))
         (id-attr (if (symbolp id-attr) (symbol-name id-attr) id-attr))
         (q (format "DELETE FROM %s WHERE %s NOT IN (%s)"
                    table-name id-attr
                    (mapconcat #'deterred-db--format-value ids ", "))))
    (sqlite-execute db q)))

(defmacro deterred-db-list-to-alist (list-var fields)
  "Convert a list LIST-VAR to an alist based on FIELDS.
FIELDS is a list of symbols that will be used as keys."
  `(cl-loop for field in ,fields
            for i from 0
            collect (cons field (nth i ,list-var))))

(defun deterred-db-select-alist (db query &optional values return-type)
  "Perform a select QUERY on DB, return the result as alists.

VALUES is the list of values to be interpolated into QUERY.

If RETURN-TYPE is string, return strings.  Otherwise return symbols."
  (let* ((data (sqlite-select db query values 'full))
         (fields (car data)))
    (unless (eq return-type 'string)
      (setq fields (mapcar #'intern fields)))
    (cl-loop for row in (cdr data)
             collect (cl-loop for field in fields
                              for datum in row
                              collect (cons field datum)))))

(defun deterred-db--select-process-template (query &optional values)
  "Prepare VALUES for interpolation in QUERY."
  (let (stored-values)
    (with-temp-buffer
      (insert query)
      (goto-char (point-min))
      (save-match-data
        (while (re-search-forward (rx (| (: ":" (+ alnum))
                                         "[["))
                                  nil t)
          (if (string-match-p (rx bos ":") (match-string 0))
              (let ((key (intern (match-string 0))))
                (push (alist-get key values) stored-values)
                (delete-region (- (point) (length (match-string 0))) (point))
                (insert "?"))
            (let ((brackets-start (point)))
              (unless (re-search-forward (rx "]]") nil t)
                (error "Couldn't find the pair ]] for [["))
              (let ((brackets-end (point)))
                (goto-char brackets-start)
                (unless (re-search-forward (rx ":" (+ alnum)) nil t)
                  (error "Couldn't find the interpolation symbol in brackets"))
                (let ((key (intern (match-string 0))))
                  (if (alist-get key values)
                      (progn
                        (push (alist-get key values) stored-values)
                        (save-excursion
                          (goto-char brackets-end)
                          (delete-region (point) (- (point) 2)))
                        (delete-region (- (point) (length (match-string 0))) (point))
                        (insert "?")
                        ;; This should keep the point on "?", I think
                        (save-excursion
                          (goto-char brackets-start)
                          (delete-region (point) (- (point) 2))))
                    (delete-region (- brackets-start 2)
                                   brackets-end))))))))
      (cons (buffer-string) (nreverse stored-values)))))

(defun deterred-db-select-template (db query &optional values return-type)
  "Perform sqlite select with templating.

VALUES is an list with query parameters, where the keys are symbols
starting with \":\".  They are safely interpolated in QUERY,
i.e. every instance of a key is replaced with its value from the
VALUES parameter using the standard ?-syntax.

If a key in QUERY is enclosed in [[ ... ]], interpolate the key if the
value is non-nil and delete the expression in the brackets otherwise.

See `sqlite-select' on DB and RETURN-TYPE."

  (pcase-let ((`(,query . ,values) (deterred-db--select-process-template query values)))
    (sqlite-select db query values return-type)))

(defun deterred-db-select-template-alist (db query &optional values return-type)
  "Like `deterred-db-select-template', but return alists.

See `deterred-db-select-template' on DB, QUERY and VALUES, and
`deterred-db-select-alist' on RETURN-TYPE."

  (pcase-let ((`(,query . ,values) (deterred-db--select-process-template query values)))
    (deterred-db-select-alist db query values return-type)))

(provide 'deterred-db)
;;; deterred-db.el ends here

;;; deterred-dataset.el --- Dataset management for DETERRED -*- lexical-binding: t -*-

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

;; Dataset management for DETERRED.
;; TODO

;;; Code:
(require 'deterred-db)
(require 'deterred-format)
(require 'deterred-utils)

(defun deterred-dataset--list ()
  "List all datasets in the database.

Return a list of alists:
- `:name' - dataset name
- `:exists' - whether it was saved in the database
- `:saved' - whether it was saved in the meta table
- `:updated_at' - last update time.

I don't actually know what to do if either `:saved' or `:exists' isn't
true."
  (let* ((db (deterred-db--init))
         (tables
          (mapcar
           (lambda (datum) (alist-get 'name datum))
           (deterred-db-select-alist
            db "SELECT name FROM sqlite_master
WHERE type = 'table' AND name LIKE 'dataset_%'")))
         (dataset-name-hash (make-hash-table :test #'equal))
         (data (deterred-db-select-alist db "SELECT * FROM meta_dataset_info"))
         (data-by-name (make-hash-table :test #'equal))
         names)
    (dolist (table tables)
      (puthash (string-replace "dataset_" "" table) t dataset-name-hash))
    (dolist (datum data)
      (puthash (alist-get 'name datum) datum data-by-name))
    (cl-loop for key in (seq-uniq (append (hash-table-keys dataset-name-hash)
                                          (hash-table-keys data-by-name)))
             for datum = (gethash key data-by-name)
             collect`((:name . ,key)
                      (:exists . ,(not (null (gethash key dataset-name-hash))))
                      (:saved . ,(not (null datum)))
                      (:updated_at . ,(alist-get 'updated_at datum))))))

(defun deterred-dataset--get-first-non-nil (data)
  "Find the first non-nil value in a list of alists DATA.

Return an alist with keys of alists in DATA as keys and the first
non-nil value in DATA corresponding to the key as values."
  (let ((res (mapcar (lambda (d) (cons (car d) nil)) (car data))))
    (cl-block dataset-iter
      (dolist (datum data)
        (let ((done t))
          (cl-loop for (k . saved-v) in res
                   for v = (alist-get k datum)
                   when (and (not saved-v) v)
                   do (progn
                        (setf (alist-get k res) v)
                        (setq done nil)))
          (when done
            (cl-return-from dataset-iter)))))
    res))

(defun deterred-dataset--get-create-table (data name)
  "Get a CREATE TABLE expression for DATA with the table NAME."
  (deterred-format
   "CREATE TABLE dataset_" name " (\n"
   (f-mapconcat
    (lambda (d)
      (concat "  " (symbol-name (car d))
              " "
              (cond ((floatp (cdr d)) "REAL")
                    ((integerp (cdr d)) "INT")
                    ((stringp (cdr d)) "TEXT")
                    (_ "ANY"))
              " NULL"))
    (deterred-dataset--get-first-non-nil data)
    ",\n")
   "\n) STRICT;"))

(defun deterred-dataset--optimistically-to-cast-data (data)
  "Try to optimistically cast ints and floats in DATA.

DATA is a list of alists."
  (let (res)
    (mapcar
     (lambda (datum)
       (let (datum-res)
         (pcase-dolist (`(,k . ,v) datum)
           (setf (alist-get k datum-res)
                 (cond ((equal v "") nil)
                       ((string-match-p (rx bos (+ digit) (? "." (+ digit)) eos) v)
                        (string-to-number v))
                       (t v))))
         (push datum-res res)))
     data)
    (nreverse res)))

(defun deterred-dataset--save (data name)
  "Save dataset DATA with NAME into DETERRED.

This creates a table called \"dataset_<name>\" and adds a record to
\"meta_dataset_info\"."
  (let* ((db (deterred-db--init))
         (data (deterred-dataset--optimistically-to-cast-data data))
         (create-expr (deterred-dataset--get-create-table data name)))
    (with-sqlite-transaction db
      (sqlite-execute db (format "DROP TABLE IF EXISTS dataset_%s" name))
      (sqlite-execute db create-expr)
      (deterred-db-insert-unsafe
       db :table-name (intern (format "dataset_%s" name)) :values data)
      (deterred-db-insert-unsafe
       db :table-name 'meta_dataset_info
       :values `(((name . ,name) (updated_at . ,(time-convert nil 'integer))))
       :conflict-action 'do-update
       :conflict-attrs '(name)))))

(defun deterred-dataset--unsave (name)
  "Remove dataset NAME from DETERRED."
  (let ((db (deterred-db--init)))
    (with-sqlite-transaction db
      (sqlite-execute db (format "DROP TABLE IF EXISTS dataset_%s" name))
      (sqlite-execute db "DELETE FROM meta_dataset_info WHERE name = ?"
                      (list name)))))

(defun deterred-dataset-add-csv (csv-path name)
  "Add csv to DETERRED datasets.

CSV-PATH is the path to the CSV file.  NAME is the dataset name."
  (interactive
   (list
    (read-file-name "CSV file: " nil nil nil nil
                    (lambda (f)
                      (or (file-directory-p f)
                          (string-match-p (rx ".csv" eos) f))))
    (read-from-minibuffer "Name: ")))
  (let ((data (deterred-utils-read-csv-with-python csv-path nil 'list)))
    (deterred-dataset--save data name)
    (message "Saved dataset %s with %s rows" name (seq-length data))))

(defun deterred-dataset-delete (name)
  "Remove dataset NAME from DETERRED."
  (interactive
   (list
    (let ((data
           (mapcar
            (lambda (d)
              (cons (format "%s (%s)" (alist-get :name d)
                            (format-time-string
                             "%FT%T%z" (alist-get :updated_at d)))
                    (alist-get :name d)))
            (deterred-dataset--list))))
      (alist-get
       (completing-read "Dataset: " data)
       data nil nil #'equal))))
  (deterred-dataset--unsave name))

(provide 'deterred-dataset)
;;; deterred-dataset.el ends here

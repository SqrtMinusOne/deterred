;;; deterred-habits.el --- TODO -*- lexical-binding: t -*-

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
(require 'deterred-db)
(require 'deterred-source)
(require 'deterred-format)
(require 'org)
(require 'org-habit)
(require 'cl-lib)

(defconst deterred-habits-uuid-namespace
  "51038aa1-8fb1-4e05-b697-fa651ba8786d")

(defun deterred-habits--parse-buffer ()
  "Parse the current `org-habit' buffer."
  (let (res)
    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (headline)
        (save-excursion
          (goto-char (org-element-property :begin headline))
          (when (org-is-habit-p)
            ;; Partly copied from `org-habit-parse-todo'
            (let* ((reversed org-log-states-order-reversed)
                   (search (if reversed 're-search-forward 're-search-backward))
                   (end (org-entry-end-position))
                   (limit (if reversed end (point)))
                   (re (format
                        "^[ \t]*-[ \t]+\\(?:State \"%s\".*%s%s\\)"
                        (regexp-opt org-done-keywords)
                        org-ts-regexp-inactive
                        (let ((value (cdr (assq 'done org-log-note-headings))))
                          (if (not value) ""
                            (concat "\\|"
                                    (org-replace-escapes
                                     (regexp-quote value)
                                     `(("%d" . ,org-ts-regexp-inactive)
                                       ("%D" . ,org-ts-regexp)
                                       ("%s" . "\"\\S-+\"")
                                       ("%S" . "\"\\S-+\"")
                                       ("%t" . ,org-ts-regexp-inactive)
                                       ("%T" . ,org-ts-regexp)
                                       ("%u" . ".*?")
                                       ("%U" . ".*?")))))))))
              (unless reversed (goto-char end))
              (while (funcall search re limit t)
                (let ((timestamp (time-convert
                                  (org-time-string-to-time
                                   (or (match-string-no-properties 1)
                                       (match-string-no-properties 2)))
                                  'integer)))
                  (push `((habit . ,(org-element-property :raw-value headline))
                          (timestamp . ,timestamp))
                        res))))))))
    res))

(defun deterred-habits-load (file)
  "Load `org-habit' data from FILE into DETERRED."
  (interactive (list
                (read-file-name "Org file: " nil nil nil nil
                                (lambda (f)
                                  (or (file-directory-p f)
                                      (string-match-p (rx ".org" eos) f))))))
  (let ((db (deterred-db--init)))
    (with-temp-buffer
      (insert-file-contents file)
      (let (org-mode-hook)
        (org-mode))
      (with-sqlite-transaction db
        (deterred-db-insert-unsafe
         db :table-name 'habit_record
         :values (deterred-habits--parse-buffer)
         :conflict-action 'do-nothing)))))

(defclass deterred-habits (deterred-source)
  ((name :initform "Habits (org-habit)")
   (warn-days :initform 31)
   (org-files :initarg :org-files))
  "DETERRED source for org-habit.")

(cl-defmethod deterred-source-range ((_source deterred-habits) &optional db)
  "Get the data availability range for org-habit.

DB is the sqlite database object."
  (let* ((db (or db (deterred-db--init)))
         (data (sqlite-select
                db "SELECT MIN(timestamp), MAX(timestamp)
                    FROM habit_record")))
    (cons (caar data) (cadar data))))

(cl-defmethod deterred-source-sync ((source deterred-habits) &optional callback)
  "Sync DETERRED with org-habit.

Call CALLBACK when done.

SOURCE is an instance of `deterred-habits'."
  (unless (oref source org-files)
    (user-error "No org-files set for `deterred-habits'"))
  (dolist (file (oref source org-files))
    (deterred-habits-load file))
  (when callback (funcall callback)))

(cl-defmethod deterred-source-day-summary
  ((_source deterred-habits) timestamp &optional db)
  (let ((db (or db (deterred-db--init)))
        (habits
         (deterred-db-select-alist
          db "SELECT * FROM habit_record
              WHERE timestamp BETWEEN ? AND ?"
          (list timestamp (+ (* 60 60 24) timestamp)))))
    (when habits
      `((:short-description
         . ,(format "%d records" (seq-length habits)))
        (:long-description
         . ,(deterred-format
             (f-mapconcat (f "- " (f-acc "iter->'habit")) habits)))))))

(provide 'deterred-habits)
;;; deterred-habits.el ends here

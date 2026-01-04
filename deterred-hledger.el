;;; deterred-hledger.el --- hledger integration for DETERRED. -*- lexical-binding: t -*-

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

;; hledger integration for DETERRED.
;;
;; This datasource queries hledger directly without storing data
;; in the database.  It uses hledger's JSON output and various
;; reporting commands to provide summaries.

;;; Code:
(require 'deterred-source)
(require 'deterred-utils)
(require 'iso8601)
(require 'cl-lib)
(require 'magit-section)

(defcustom deterred-hledger-binary (executable-find "hledger")
  "Path to hledger binary."
  :group 'deterred-sources
  :type 'string)

(defcustom deterred-hledger-file
  (or (getenv "LEDGER_FILE")
      (when (boundp 'hledger-jfile)
        hledger-jfile))
  "Path to hledger file."
  :group 'deterred-sources
  :type 'string)

(defvar deterred-hledger--cache (make-hash-table :test #'equal)
  "Caching variable.  See `deterred-hledger--with-cache'.")

;;;###autoload
(defclass deterred-hledger (deterred-source)
  ((name :initform "Finance (hledger)"))
  "DETERRED source for hledger.")

(defun deterred-hledger--parse-date (date-string)
  "Parse DATE-STRING (YYYY-MM-DD) to UNIX timestamp at midnight UTC."
  (let ((date (iso8601-parse-date date-string)))
    (setf (decoded-time-second date) 0
          (decoded-time-minute date) 0
          (decoded-time-hour date) 0)
    (time-convert (encode-time date) 'integer)))

(defun deterred-hledger--timestamp-to-date (timestamp)
  "Convert UNIX TIMESTAMP to YYYY-MM-DD string."
  (format-time-string "%Y-%m-%d" timestamp))

(defun deterred-hledger--call-json (&rest args)
  "Call hledger with ARGS and parse JSON output.
ARGS is a list of command-line arguments."
  (unless (executable-find deterred-hledger-binary)
    (user-error "Hledger binary not found: %s" deterred-hledger-binary))
  (let ((json-output (with-temp-buffer
                       (apply #'call-process deterred-hledger-binary nil t nil args)
                       (buffer-string))))
    (condition-case err
        (json-read-from-string json-output)
      (json-parse-error
       (error "Invalid JSON from hledger: %s"
              (error-message-string err))))))

(defmacro deterred-hledger--with-cache (key &rest body)
  "Cache the results until the hledger file is updated.

KEY is the cache key (can be string or symbol), BODY is to be
executed.  The cache variable is `deterred-hledger--cache'."
  (declare (indent 1) (debug (form body)))
  `(progn
     (when (file-has-changed-p deterred-hledger-file)
       (setq deterred-hledger--cache (make-hash-table :test #'equal)))
     (let ((k ,key))
       (or (gethash k deterred-hledger--cache)
           (progn
             (let ((res (progn ,@body)))
               (puthash k res deterred-hledger--cache)
               res))))))

(cl-defmethod deterred-source-range ((_source deterred-hledger) &optional _db)
  "Get the data availability range for hledger.
Return a cons cell, with car as the start timestamp, and cdr as the
end timestamp."
  (unless (executable-find deterred-hledger-binary)
    (user-error "Hledger binary not found: %s" deterred-hledger-binary))
  (deterred-hledger--with-cache 'range
    (let* ((stats-output (with-temp-buffer
                           (call-process deterred-hledger-binary nil t nil "stats")
                           (buffer-string)))
           (span-line (when (string-match "Txns span[ \t]*:[ \t]*\\([0-9-]+\\) to \\([0-9-]+\\)"
                                          stats-output)
                        (cons (match-string 1 stats-output)
                              (match-string 2 stats-output))))
           (start-date (when span-line
                         (deterred-hledger--parse-date (car span-line))))
           (end-date (when span-line
                       (deterred-hledger--parse-date (cdr span-line)))))
      (cons start-date end-date))))

(defun deterred-hledger--process-total-rows (total-rows)
  "Process the total rows vector in an hledger subreport rows.

TOTAL-ROWS is the vector.  It seems to be available under the
\"prrTotal\" key.

The return value is an alist, where the keys are commodities and the
values are amounts.  Commodities are strings, amounts are floats."
  (cl-loop
   for total-row across total-rows
   for commodity = (alist-get 'acommodity total-row)
   for amount = (alist-get 'floatingPoint (alist-get 'aquantity total-row))
   collect (cons commodity amount)))

(defun deterred-hledger--make-sort-key (amounts-alist)
  "Make sort key from AMOUNTS-ALIST for sorting by currency then value.
Returns a string with primary currency and padded value."
  (if amounts-alist
      (let* ((sorted-by-currency (seq-sort-by #'car #'string< amounts-alist))
             (primary (car sorted-by-currency))
             (commodity (car primary))
             (amount (cdr primary)))
        ;; Pad amount to 20 digits with leading zeros, negate for descending sort
        (concat commodity (format "%020.2f" (- amount))))
    ""))

(defun deterred-hledger--get-is-in-acc-by-desc (start end acct is-revenue)
  "Get incomestatement on ACCT, pivoted by desc.

START, END is the range.  IS-REVENUE is either t (in which case the
report is done for revenues), or `:json-false' (for expenses).

The return value is an alist sorted by currency then value:
- key: descs
- value: an alist (the output of `deterred-hledger--process-total-rows'):
   - key: commodity
   - value: amount."
  (let* ((data (deterred-hledger--call-json "is" acct "-b" start "-e" end
                                            "-O" "json" "--pivot" "desc"))
         (subreport (seq-find
                     (lambda (rep) (eq (elt rep 2) is-revenue))
                     (alist-get 'cbrSubreports data)))
         res)
    (cl-loop
     for row across (alist-get 'prRows (elt subreport 1))
     for desc = (alist-get 'prrName row)
     for c = (deterred-hledger--process-total-rows (alist-get 'prrTotal row))
     do (setf (alist-get desc res nil nil #'equal)
              c))
    (seq-sort-by
     (lambda (pair) (deterred-hledger--make-sort-key (cdr pair))) #'string> res)))

(defun deterred-hledger--get-is-breakdown (start end)
  "Get incomestatement from START to END with breakdown by descs.

This is somewhat slow because it runs N+1 hledger commands, where N is
the number of accounts used from START to END.

The return value is a nested alist with accounts sorted by currency then value:
- Report name \(\"Expenses\" or \"Revenues\")
  - `total': an alist like \(commodity . amount)
  - `<account-name>':
    - `total': an alist like \(commodity . amount)
    - `breakdown': an alist (the output of
      `deterred-hledger--get-is-in-acc-by-desc')
       - key: desc
       - value: an alist like \(commodity. amount)."
  (deterred-hledger--with-cache (format "is-breakdown-%s-%s" start end)
    (let* ((data (deterred-hledger--call-json "is" "-b" start "-e" end "-O" "json"))
           res)
      (cl-loop
       for report across (alist-get 'cbrSubreports data)
       for name = (elt report 0)
       do (setf (alist-get 'total (alist-get name res nil nil #'equal))
                (deterred-hledger--process-total-rows
                 (alist-get 'prrTotal (alist-get 'prTotals (elt report 1)))))
       do (cl-loop
           for row across (alist-get 'prRows (elt report 1))
           for acct = (alist-get 'prrName row)
           for c = (deterred-hledger--process-total-rows (alist-get 'prrTotal row))
           do (setf (alist-get acct (alist-get name res nil nil #'equal) nil nil #'equal)
                    `((total . ,c)
                      (breakdown . ,(deterred-hledger--get-is-in-acc-by-desc
                                     start end acct (elt report 2)))))))
      ;; Sort accounts within each report by currency, then value
      (cl-loop for (report-name . report-data) in res
               do (setf (alist-get report-name res nil nil #'equal)
                        (cons (cons 'total (alist-get 'total report-data))
                              (seq-sort-by (lambda (pair)
                                             (deterred-hledger--make-sort-key
                                              (alist-get 'total (cdr pair))))
                                           #'string>
                                           (seq-filter (lambda (pair) (not (eq (car pair) 'total)))
                                                       report-data)))))
      res)))

(defun deterred-hledger--format-amounts (amounts-alist)
  "Format AMOUNTS-ALIST as a string.
AMOUNTS-ALIST is an alist with (commodity . amount) pairs.
Returns string like '100.00 RUB, 50.00 USD'."
  (if amounts-alist
      (mapconcat (lambda (pair)
                   (format "%.2f %s" (cdr pair) (car pair)))
                 amounts-alist
                 ", ")
    "0"))

(defun deterred-hledger--render-breakdown (breakdown-data report-name)
  "Render BREAKDOWN-DATA for REPORT-NAME using magit sections.
BREAKDOWN-DATA is the output from deterred-hledger--get-is-breakdown."
  (let ((report-data (alist-get report-name breakdown-data nil nil #'equal)))
    (when (and report-data (alist-get 'total report-data))
      (insert (propertize report-name 'face 'deterred-faces-section-heading-3))
      (insert "\n")
      (cl-loop for (account . account-data) in report-data
               unless (eq account 'total)
               do (let ((total (alist-get 'total account-data))
                        (breakdown (alist-get 'breakdown account-data)))
                    (magit-insert-section (deterred-hledger-account account t)
                      (insert (propertize
                               (format "%s: %s" account
                                       (deterred-hledger--format-amounts total))
                               'face 'deterred-faces-section-heading-4))
                      (magit-insert-heading)
                      (when breakdown
                        (cl-loop for (desc . amounts) in breakdown
                                 do (insert (format "- %s: %s\n"
                                                    desc
                                                    (deterred-hledger--format-amounts amounts))))))))
      (insert "\n"))))

(cl-defmethod deterred-source-range-summary
  ((_source deterred-hledger) start end &optional _db)
  "Make hledger summary for [START, END]."
  (let* ((start-date (deterred-hledger--timestamp-to-date start))
         (end-date (deterred-hledger--timestamp-to-date (1+ end)))
         (breakdown (deterred-hledger--get-is-breakdown start-date end-date))
         (expenses-data (alist-get "Expenses" breakdown nil nil #'equal))
         (revenues-data (alist-get "Revenues" breakdown nil nil #'equal))
         (total-expenses (alist-get 'total expenses-data))
         (total-revenues (alist-get 'total revenues-data)))
    (when (or total-expenses total-revenues)
      `((:short-description
         . ,(concat "+ " (deterred-hledger--format-amounts total-revenues)
                    " / - " (deterred-hledger--format-amounts total-expenses)))
        (:long-description-fn
         . ,(lambda (&rest _)
              (deterred-hledger--render-breakdown breakdown "Expenses")
              (deterred-hledger--render-breakdown breakdown "Revenues")))))))

(defun deterred-hledger--format-transaction (txn)
  "Format a single transaction TXN for display."
  (let* ((tdate (alist-get 'tdate txn))
         (tdescription (alist-get 'tdescription txn))
         (tpostings (alist-get 'tpostings txn)))
    (concat (format "%s %s\n"
                    (propertize tdate 'face 'deterred-faces-date) tdescription)
            (mapconcat
             (lambda (posting)
               (let* ((account (alist-get 'paccount posting))
                      (pamount (alist-get 'pamount posting))
                      (amounts (cl-loop for amt across pamount
                                        for commodity = (alist-get 'acommodity amt)
                                        for quantity = (alist-get 'floatingPoint
                                                                  (alist-get 'aquantity amt))
                                        collect (format "%.2f %s" quantity commodity))))
                 (format "  %s  %s" account (string-join amounts ", "))))
             tpostings
             "\n")
            "\n")))

(cl-defmethod deterred-source-day-summary
  ((_source deterred-hledger) timestamp &optional _db)
  "Make hledger summary for a single day at TIMESTAMP."
  (deterred-hledger--with-cache (format "daily-%s" timestamp)
    (let* ((start-date (deterred-hledger--timestamp-to-date timestamp))
           (end-date (deterred-hledger--timestamp-to-date (+ (* 60 60 24) timestamp)))
           (transactions (deterred-hledger--call-json "print" "-b" start-date "-e" end-date "-O" "json"))
           (txn-count (length transactions)))
      (when (> txn-count 0)
        `((:short-description . ,(format "%d transaction%s" txn-count (if (> txn-count 1) "s" "")))
          (:long-description . ,(mapconcat #'deterred-hledger--format-transaction transactions "\n")))))))

(provide 'deterred-hledger)
;;; deterred-hledger.el ends here

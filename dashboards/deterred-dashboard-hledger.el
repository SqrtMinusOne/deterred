;;; deterred-dashboard-activitywatch.el --- DETERRED dashboard for hldeger -*- lexical-binding: t -*-

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

;; A dashboard for hlegder, corresponding to `deterred-hledger'.

;;; Code:
(require 'deterred-dashboard)
(require 'deterred-hledger)
(require 'deterred-utils)

(defun deterred-dashboard-hledger--get (command)
  "Execute hledger COMMAND and return the result split by newlines.

COMMAND is a symbol.

This makes sense for the following commands:
- accounts
- descriptions
- tags
- payees
- notes
- codes"
  (deterred-hledger--with-cache (format "get-%s" command)
    (with-temp-buffer
      (call-process deterred-hledger-binary nil t nil
                    (symbol-name command))
      (string-split (buffer-string) "\n" t))))

(defun deterred-dashboard-hledger--get-types ()
  "Get unique account types defined in hledger."
  (deterred-hledger--with-cache "get-types"
    (with-temp-buffer
      (call-process deterred-hledger-binary nil t nil
                    "accounts" "--types")
      (goto-char (point-min))
      (let ((res (make-hash-table :test #'equal)))
        (save-match-data
          (while (re-search-forward (rx bol (* nonl) "; type: " (group alpha))
                                    nil t)
            (puthash (match-string 1) t res)))
        (hash-table-keys res)))))

(defclass deterred-dashboard-hledger (deterred-dashboard)
  ((name :initform "hledger"))
  "A DETERRED dashboard for DETERRED.")

(cl-defmethod deterred-dashboard-default-params
  ((_dashboard deterred-dashboard-hledger))
  "Default parameters for the hledger dashboard."
  '((:start-date)
    (:end-date)
    (:accounts)
    (:descriptions)
    (:commodities)
    (:exchange)
    (:tags)
    (:payees)
    (:notes)
    (:codes)
    (:status)
    (:types)))

(cl-defmethod deterred-dashboard-render-params ((_dasbhoard deterred-dashboard-hledger))
  "Render the parameters section for the hledger dashboard."
  (deterred-dashboard-widget-date
   :name "Start date"
   :key :start-date
   :display-date t)
  (insert "\n")
  (deterred-dashboard-widget-date
   :name "End date"
   :key :end-date
   :display-date t)
  (insert "\n")
  (deterred-dashboard-widget-completing-read-multiple
   :name "Account"
   :key :accounts
   :options (deterred-dashboard-hledger--get 'accounts))
  (insert "\n")
  (deterred-dashboard-widget-completing-read-multiple
   :name "Description"
   :key :descriptions
   :options (deterred-dashboard-hledger--get 'descriptions))
  (insert "\n")
  (deterred-dashboard-widget-completing-read-multiple
   :name "Commodities"
   :key :commodities
   :options (deterred-dashboard-hledger--get 'commodities))
  (insert "\n")
  (deterred-dashboard-widget-completing-read
   :name "Exchange"
   :key :exchange
   :options (deterred-dashboard-hledger--get 'commodities))
  (insert "\n")
  (deterred-dashboard-widget-completing-read-multiple
   :name "Tag"
   :key :tags
   :options (deterred-dashboard-hledger--get 'tags))
  (insert "\n")
  (deterred-dashboard-widget-completing-read-multiple
   :name "Payee"
   :key :payees
   :options (deterred-dashboard-hledger--get 'payees))
  (insert "\n")
  (deterred-dashboard-widget-completing-read-multiple
   :name "Note"
   :key :notes
   :options (deterred-dashboard-hledger--get 'notes))
  (insert "\n")
  (deterred-dashboard-widget-completing-read-multiple
   :name "Code"
   :key :codes
   :options (deterred-dashboard-hledger--get 'codes))
  (insert "\n")
  (deterred-dashboard-widget-completing-read-multiple
   :name "Status"
   :key :status
   :options '(("Unmarked" . "")
              ("Pending" . "!")
              ("Cleared" . "*")))
  (insert "\n")
  (deterred-dashboard-widget-completing-read-multiple
   :name "Type"
   :key :types
   :options (deterred-dashboard-hledger--get-types))
  (insert "\n"))

(defun deterred-dashboard-hledger--params-to-flags (params)
  "Convert PARAMS to hledger flags.

PARAMS is as returned by `deterred-dashboard-default-params'."
  (let (flags)
    (when-let (d (alist-get :start-date params))
      (push (format "--begin=%s" (format-time-string "%F" d)) flags))
    (when-let (d (alist-get :end-date params))
      (push (format "--end=%s" (format-time-string "%F" d)) flags))
    (when-let (d (alist-get :exchange params))
      (push (format "--exchange=%s" d) flags)
      (dolist (param deterred-hledger-exchange-params)
        (when (file-exists-p (car param))
          (push (format "--file=%s" (car param)) flags)))
      (push (format "--file=%s" deterred-hledger-file) flags))
    flags))

(defun deterred-dashboard-hledger--format-query-item (key values)
  "Format one query item for hledger.

KEY is a valid hledger query type, VALUES is a list of values.

If VALUES has more than one value, return an OR query."
  (cond ((null values) nil)
        ((= (length values) 1) (format "%s:'%s'" key (car values)))
        (t (concat
            "("
            (mapconcat (lambda (v) (format "%s:'%s'" key v)) values " or ")
            ")"))))

(defconst deterred-dashboard-hledger--param-key-to-query
  '((:accounts . "acct")
    (:descriptions . "desc")
    (:commodities . "cur")
    (:tags . "tag")
    (:payees . "payee")
    (:notes . "note")
    (:codes . "code")
    (:status . "status")
    (:types . "type")))

(defun deterred-dashboard-hledger--params-to-query (params)
  "Convert PARAMS to hledger query.

PARAMS is as returned by `deterred-dashboard-default-params'."
  (let ((expr
         (string-join
          (seq-filter
           #'identity
           (mapcar
            (lambda (elem)
              (when-let (value (alist-get (car elem) params))
                (deterred-dashboard-hledger--format-query-item (cdr elem) value)))
            deterred-dashboard-hledger--param-key-to-query))
          " and ")))
    (unless (string-empty-p expr)
      (format "expr:(%s)" expr))))

(cl-defmethod deterred-dashboard-list-datasets
  ((_dashboard deterred-dashboard-hledger))
  "List datasets for the hledger dashboard."
  '((bs (name . "Balancesheet"))
    (is (name . "Income Statement"))
    (is-desc (name . "Income Statement (pivot by description)"))
    (net-worth-by-month (name . "Net worth by month"))))

(cl-defmethod deterred-dashboard-fetch-datasets
  ((_dashboard deterred-dashboard-hledger) params)
  "Fetch datasets for the hledger dashboard.

PARAMS is as returned by `deterred-dashboard-default-params'."
  (let* ((query (deterred-dashboard-hledger--params-to-query params))
         (flags (deterred-dashboard-hledger--params-to-flags params))
         (args `(,@flags ,@(when query (list query))))
         (cache-key (prin1-to-string args)))
    `((bs . ,(deterred-hledger--with-cache (format "bs-%s" cache-key)
               (apply #'deterred-hledger--call "bs" "--layout=tall" args)))
      (is . ,(deterred-hledger--with-cache (format "is-%s" cache-key)
               (apply #'deterred-hledger--call "is" "--layout=tall" args)))
      (is-desc . ,(deterred-hledger--with-cache (format "is-desc-%s" cache-key)
                    (apply #'deterred-hledger--call "is" "--pivot=desc" "--layout=tall" args)))
      (net-worth-by-month
       . ,(deterred-hledger--with-cache (format "nw-by-month-%s" cache-key)
            (let* ((data (apply #'deterred-hledger--call-json "bs" "-M" "-O" "json" args))
                   (commodities (deterred-dashboard-hledger--get 'commodities))
                   (dates (mapcar (lambda (d) (alist-get 'contents (elt d 1)))
                                  (alist-get 'cbrDates data))))
              (cl-loop for amounts across (alist-get 'prrAmounts (alist-get 'cbrTotals data))
                       for date in dates
                       for value = (cons
                                    (cons 'date date)
                                    (cl-loop for c in commodities collect (cons (intern c) 0)))
                       do (cl-loop for amount across amounts
                                   for q = (alist-get 'floatingPoint (alist-get 'aquantity amount))
                                   for commodity = (intern (alist-get 'acommodity amount))
                                   do (setf (alist-get commodity value nil nil #'equal)
                                            (+ q (alist-get commodity value 0 nil #'equal))))
                       collect value)))))))

(cl-defmethod deterred-dashboard-render-results
  ((_dashboard deterred-dashboard-hledger) params data)
  "Render DATA for hledger dashboard.

PARAMS is as returned by `deterred-dashboard-default-params'."
  (insert
   (deterred-format
    (f-h2 "hledger parameters") "\n"
    "hledger " (f-join (deterred-dashboard-hledger--params-to-flags params) " ")
    " "
    (deterred-dashboard-hledger--params-to-query params)
    "\n\n"
    (f-h2 "General reports") "\n"
    (f-h3 "Balancesheet") "\n"
    (alist-get 'data (alist-get 'bs data))
    (f-h3 "Income Statement") "\n"
    (alist-get 'data (alist-get 'is data))
    (f-h3 "Income Statement (pivot by description)") "\n"
    (alist-get 'data (alist-get 'is-desc data)))))

(provide 'deterred-dashboard-hledger)
;;; deterred-dashboard-hledger.el ends here

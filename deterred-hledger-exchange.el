;;; deterred-hledger-exchange.el --- Fetch exchange rates for hledger.  -*- lexical-binding: t -*-

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

;; TODO

;;; Code:
(require 'request)

(require 'deterred-format)
(require 'deterred-hledger)
(require 'deterred-utils)

(defcustom deterred-hledger-exchange-params nil
  "Update parameters for hledger market rate files.

This is a list to make it more `customize'-friendly.  One item has to
have the following elements in this order:
- Path to hledger market price file.
- Start date.  Can be a Emacs time value (e.g. a UNIX timestamp) or
  YYYY-MM-DD.
- Target currencies, a list of strings.
- Source currencies, a list of strings.
- Data source parameters.

One item means fetching the following data: how much one unit of each
of target currencies was worth each of source-currencies at a
particular date.  Note that one item corresponds to only one actual
currency; target currencies and source currencies are lists because in
my setup I might have different currencies that worth the same,
e.g. USD and USDT.

Data source parameters is a list.  The first item is a symbol, which
can be:
- cbr for Central Bank of Russia
- frankfurter for the Frankfurter API
- coinmarketcap
- rosstat for Rosstat Consumer Price Index.
Only cbr, frankfurter, and rosstat are automated.

For CBR, the parameter list is as follows:
- cbr (a symbol)
- currency code (get one on https://cbr.ru/scripts/XML_val.asp?d=0)

For Frankfurter:
- frankfurter (a symbol)
- base currency (get symbols on https://api.frankfurter.dev/v1/currencies)
- target currency

For Coinmarketcap:
- coinmarketcap (a symbol)
- URL, e.g. https://coinmarketcap.com/currencies/bitcoin/historical-data/.
  It doesn't serve any useful purpose, but the package will propose to
  open it in a browser.

For Rosstat:
- rosstat (a symbol)."
  :group 'deterred
  :type '(repeat
          (list
           (file :tag "Path to hledger market price file")
           (sexp :tag "Start date value, a Emacs time value or YYYY-MM-DD.")
           (repeat :tag "Target currencies" string)
           (repeat :tag "Source currencies" string)
           (choice
            :tag "Data source and parameters"
            (list
             :tag "Central Bank of Russia"
             (const cbr)
             (string :tag "CBR currency code."))
            (list
             :tag "Frankfurter"
             (const frankfurter)
             (string :tag "Base currency code")
             (string :tag "Target currency code"))
            (list
             :tag "Coinmarketcap"
             (const coinmarketcap)
             (string :tag "Coinmarketcap URL"))
            (list
             :tag "Rosstat CPI"
             (const rosstat))))))

(defconst deterred-hledger-exchange--cbr-api "https://cbr.ru/scripts/XML_dynamic.asp"
  "URL of the CBR's time series script.")

(defconst deterred-hledger-exchange--frankfurter-api "https://api.frankfurter.dev/v1/"
  "URL of the Frankfurter API.")

(defun deterred-hledger-exchange--cbr-parse-date (date-string)
  "Convert DATE-STRING from DD.MM.YYYY to a UNIX timestamp."
  (save-match-data
    (string-match (rx bos (group (= 2 num)) "." (group (= 2 num))
                      "." (group (= 4 num)) eos)
                  date-string)
    (time-convert
     (encode-time
      (list 0 0 0
            (string-to-number (match-string 1 date-string))
            (string-to-number (match-string 2 date-string))
            (string-to-number (match-string 3 date-string))))
     'integer)))

(cl-defun deterred-hledger-exchange--fetch-cdr (start end callback &key code)
  "Fetch exchange rate from the Central Bank of Russia.

START and END are time values, e.g. UNIX timestamps.  CODE is a
currency code, which see on https://cbr.ru/scripts/XML_val.asp?d=0.

CALLBACK is called with a list of cons cells, where car is a UNIX
timestamp, and cdr is a the value."
  (request deterred-hledger-exchange--cbr-api
    :params `((date_req1 . ,(format-time-string "%d/%m/%Y" start))
              (date_req2 . ,(format-time-string "%d/%m/%Y" end))
              (VAL_NM_RQ . ,code))
    :parser (lambda () (libxml-parse-xml-region (point) (point-max)))
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (funcall
                 callback
                 (cl-loop for record in (dom-children data)
                          for date = (deterred-hledger-exchange--cbr-parse-date
                                      (dom-attr record 'Date))
                          for value = (string-to-number
                                       (string-replace
                                        "," "."
                                        (dom-text (dom-child-by-tag record 'Value))))
                          collect (cons date value)))))
    :error #'deterred-utils-on-request-error))

(cl-defun deterred-hledger-exchange--fetch-frankfurter
    (start end callback &key base target)
  "Fetch extract rate from Frankfurter.

START and END are time values, e.g. UNIX timestamps.  BASE and TARGET
are currency codes, which see on
https://api.frankfurter.dev/v1/currencies.

CALLBACK is called with a list of cons cells, where car is a UNIX
timestamp, and cdr is a the value."
  (request (concat deterred-hledger-exchange--frankfurter-api
                   (format-time-string "%F" start) ".." (format-time-string "%F" end))
    :params `((base . ,base)
              (symbols . ,target))
    :parser 'json-read
    :encoding 'utf-8
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (funcall
                 callback
                 (cl-loop for item in (alist-get 'rates data)
                          for date = (deterred-utils-parse-iso8601-dateonly
                                      (symbol-name (car item)))
                          for value = (cdadr item)
                          collect (cons date value)))))
    :error #'deterred-utils-on-request-error))

(cl-defun deterred-hledger-exhange--parse-coinmarketcap-csv
    (start end callback &key filename)
  "Parse crypto exchange rate CSV from coinmarketcap.com.

FILENAME is the path to the CSV, START and END are optional UNIX
timestamps to filter the CSV.

CALLBACK is called with a list of cons cells, where car is a UNIX
timestamp, and cdr is a the value.  This uses callbacks for
interfaces compatibility with other functions."
  (let ((data (deterred-utils-read-csv-with-python filename ";")))
    (funcall
     callback
     (nreverse
      (cl-loop for datum across data
               for timestamp = (time-convert
                                (encode-time
                                 (iso8601-parse (alist-get 'timestamp datum)))
                                'integer)
               when (and (or (null start) (>= timestamp start))
                         (or (null end) (<= timestamp end)))
               collect (cons timestamp (string-to-number (alist-get 'close datum))))))))

(defconst deterred-hledger-exchange--rosstat-months
  '(("январь" . 1) ("февраль" . 2) ("март" . 3) ("апрель" . 4)
    ("май" . 5) ("июнь" . 6) ("июль" . 7) ("август" . 8)
    ("сентябрь" . 9) ("октябрь" . 10) ("ноябрь" . 11) ("декабрь" . 12))
  "Russian month names to numbers mapping.")

(cl-defun deterred-hledger-exchange--parse-rosstat-cpi (start end callback &key filename)
  "Parse Rosstat's Consumer Price Index XLSX.

FILENAME is the path to the XLSX file, START and END are optional UNIX
timestamps to filter the data.

CALLBACK is called with a list of cons cells, where car is a UNIX
timestamp, and cdr is the cumulative inflation factor value.  This uses
callbacks for interface compatibility with other functions."
  (let* ((temp-dir (make-temp-file "rosstat-cpi-" t))
         (csv-base (expand-file-name "data.csv" temp-dir))
         (csv-file (concat csv-base ".1")))
    (unwind-protect
        (progn
          (unless (zerop (call-process "ssconvert" nil nil nil
                                       "-S" (expand-file-name filename)
                                       csv-base))
            (error "Failed to convert Excel file to CSV"))

          (let* ((content (with-temp-buffer
                            (insert-file-contents csv-file)
                            (goto-char (point-min))
                            (delete-line) (delete-line) (delete-line)
                            (buffer-string)))
                 (raw-data (deterred-utils-read-csv-string-with-python content))
                 (data
                  (seq-sort-by
                   #'car #'<
                   (cl-loop with parsed-months = nil
                            for row across raw-data
                            for month = (alist-get (cdar row) deterred-hledger-exchange--rosstat-months
                                                   nil nil #'equal)
                            when (and month (not (member month parsed-months)))
                            append (cl-loop for datum in (cdr row)
                                            for value = (/ (string-to-number (cdr datum)) 100.0)
                                            for year = (string-to-number (symbol-name (car datum)))
                                            for timestamp = (deterred-utils-parse-iso8601-dateonly
                                                             (format "%s-%02d-01" year month))
                                            when (and (>= timestamp start) (> value 0))
                                            collect (cons timestamp value))
                            and do (push month parsed-months)))))
            (funcall callback
                     (cl-loop with factor = 1
                              for (timestamp . value) in data
                              do (setq factor (* factor value))
                              collect (cons timestamp factor)))))
      ;; Cleanup temporary directory
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(defun deterred-hledger-exchange--get-saved-timestamp (filename)
  "Get last saved timestamp in a ledger market price file.

FILENAME is the path to the file.

Return either a UNIX timestamp or nil."
  (when (file-exists-p filename)
    (with-temp-buffer
      (insert-file-contents filename)
      (goto-char (point-max))
      (when (re-search-backward
             (rx bol "P" (* space) (group (= 4 num) "-" (= 2 num) "-" (= 2 num)))
             nil t)
        (deterred-utils-parse-iso8601-dateonly
         (match-string 1))))))

(defun deterred-hledger-exchange--write
    (filename rate-data target-currencies source-currencies)
  "Update a ledger market price file.

FILENAME is the path to the file.  RATE-DATA is a list of cons cells,
where cars are timestamps and cdrs are values.  One cell means each of
TARGET-CURRENCIES was worth cdr of each of SOURCE-CURRENCIES on or
after car."
  (with-temp-buffer
    (dolist (datum rate-data)
      (dolist (target target-currencies)
        (dolist (source source-currencies)
          (insert
           (deterred-format
            "P " (format-time-string "%F %T" (car datum))
            " " target " " (number-to-string (cdr datum))
            " "
            source "\n")))))
    (write-region (point-min) (point-max) filename t)))

(defun deterred-hledger-exchange-update (filename)
  "Update hledger market rate files.

FILENAME has to be configured in `deterred-hledger-exchange-params'."
  (interactive
   (list
    (let ((data
           (mapcar
            (lambda (item)
              (cons
               (deterred-format
                (file-name-base (car item))
                (when-let
                    (d (deterred-hledger-exchange--get-saved-timestamp (car item)))
                  (f " (last updated: " (format-time-string "%F" d) ")")))
               (car item)))
            deterred-hledger-exchange-params)))
      (alist-get (completing-read "Filename: " data) data nil nil #'equal))))
  (let* ((params (assoc filename deterred-hledger-exchange-params))
         (start (or (deterred-hledger-exchange--get-saved-timestamp (car params))
                    (deterred-utils-parse-iso8601-dateonly (elt params 1))
                    (elt params 1)))
         (update-fn
          (pcase (car (elt params 4))
            ('cbr (lambda (callback)
                    (deterred-hledger-exchange--fetch-cdr
                     start (time-convert nil 'integer) callback
                     :code (elt (elt params 4) 1))))
            ('frankfurter (lambda (callback)
                            (deterred-hledger-exchange--fetch-frankfurter
                             start (time-convert nil 'integer) callback
                             :base (elt (elt params 4) 1)
                             :target (elt (elt params 4) 2))))
            ('coinmarketcap (lambda (callback)
                              (when (y-or-n-p "Open browser?")
                                (browse-url (elt (elt params 4) 1)))
                              (deterred-hledger-exhange--parse-coinmarketcap-csv
                               start (time-convert nil 'integer) callback
                               :filename
                               (read-file-name
                                "CSV file: " nil nil t nil
                                (lambda (f) (or
                                             (directory-name-p f)
                                             (string-match-p (rx ".csv" eos) f)))))))
            ('rosstat (lambda (callback)
                        (deterred-hledger-exchange--parse-rosstat-cpi
                         (or
                          (deterred-utils-parse-iso8601-dateonly (elt params 1))
                          (elt params 1))
                         (time-convert nil 'integer)
                         (lambda (data)
                           (delete-file (elt params 0))
                           (funcall callback data))
                         :filename
                         (read-file-name
                          "Rosstat XLSX file: " nil nil t nil
                          (lambda (f) (or
                                       (directory-name-p f)
                                       (string-match-p (rx ".xlsx" eos) f)))))))
            (_ (error "Wrong type: " (car (elt params 4)))))))
    (funcall update-fn
             (lambda (data)
               (deterred-hledger-exchange--write
                (car params) data (elt params 2) (elt params 3))
               (message "Update complete!")))))

(provide 'deterred-hledger-exchange)
;;; deterred-hledger-exchange.el ends here

;;; deterred-backup.el --- TODO -*- lexical-binding: t -*-

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

(defcustom deterred-backup-keep
  '((daily . 5)
    (weekly . 4)
    (monthly . 4))
  "How long to keep daily, weekly, months."
  :group 'deterred
  :type '(list
          (cons (const :tag "Daily backups" daily)
                (integer :tag "Number of days"))
          (cons (const :tag "Weekly backups" weekly)
                (integer :tag "Number of weeks"))
          (cons (const :tag "Monthly backups" monthly)
                (integer :tag "Number of months"))))

(defun deterred-backup--make-copies (file backups-dir kind stamps)
  "Make copies of FILE to BACKUPS-DIR.

KIND is a symbol, STAMPS is a list of strings.  Filenames to create
are formed as follows: <file>.<kind>.<stamps>.

All files starting with <file>.<kind> with wrong stamps will be
deleted."
  (let* ((file-name (file-name-nondirectory file))
         (source-files
          (directory-files
           backups-dir t
           (rx (literal file-name) "." (literal (symbol-name kind)))))
         (target-files
          (seq-sort
           #'string-greaterp
           (mapcar
            (lambda (stamp)
              (concat
               (file-name-as-directory backups-dir) file-name
               "." (symbol-name kind) "." stamp))
            stamps)))
         (senior-file (car target-files)))
    ;; Remove extra files
    (dolist (extra-file (seq-difference source-files target-files))
      (delete-file extra-file))
    (unless (file-exists-p senior-file)
      (copy-file file senior-file nil t))))

(defun deterred-backup--make-stamps (kind keep-params)
  "Create unique stamps for backup filenames.

KIND is a symbol; either daily, weekly or monthly.  KEEP-PARAMS is an
alist having the given KIND as one of its keys; the value is the
number of stamps to produce.

The produced stamps are:
- YYYY-MM-DD for daily
- YYYY-WW for weekly
- YYYY-MM for monthly
all starting with the current day/iso week/month."
  (let* ((keep-units (alist-get kind keep-params))
         (time (decode-time))
         res)
    (unless keep-units
      (error "No parameters for % backups" kind))
    ;; Set start of day
    (setf (decoded-time-hour time) 0
          (decoded-time-minute time) 0
          (decoded-time-second time) 0)
    ;; If weekly, set start of week
    (when (eq kind 'weekly)
      (setq time
            (decode-time
             (time-subtract
              (encode-time time)
              (* (% (+ 7 (1- (decoded-time-weekday time))) 7)
                 60 60 24)))))
    ;; If monthly, set start of month
    (when (eq kind 'monthly)
      (setf (decoded-time-day time) 0))
    (dotimes (i keep-units)
      (push
       (format-time-string
        (pcase kind
          ('daily "%F")
          ('weekly "%+4Y-%V")
          ('monthly "%+4Y-%m"))
        (encode-time time))
       res)
      (pcase kind
        ('daily (setq time
                      (decode-time
                       (time-subtract
                        (encode-time time)
                        (* 60 60 24)))))
        ('weekly (setq time
                       (decode-time
                        (time-subtract
                         (encode-time time)
                         (* 7 60 60 24)))))
        ('monthly (setf (decoded-time-month time)
                        (1- (decoded-time-month time)))
                  (when (< (decoded-time-month time) 1)
                    (setf (decoded-time-month time) 12
                          (decoded-time-year time) (1- (decoded-time-year time)))))))
    (nreverse res)))

(defun deterred-backup--backup (file backups-dir keep-params)
  "Backup FILE to BACKUPS-DIR according to KEEP-PARAMS.

KEEP-PARAMS is an alist with the following symbols available as keys:
- daily
- weekly
- monthly
The values are the number of days/weeks/months for which to keep the
respective backups.

The resulting filenames are formed as follows:
<backups-dir>/<filename>.<kind>.<stamp>
Where <kind> is the same as the keys of KEEP-PARAMS."
  (unless (file-exists-p file)
    (error "File to backup %s doesn't exist" file))
  (mkdir backups-dir t)
  (dolist (keep-param keep-params)
    (deterred-backup--make-copies
     file backups-dir (car keep-param)
     (deterred-backup--make-stamps
      (car keep-param)
      keep-params))))

(defun deterred-backup ()
  "Backup the DETERRED database."
  (interactive)
  (deterred-backup--backup
   (expand-file-name deterred-db-location)
   (expand-file-name deterred-db-backups-location)
   deterred-backup-keep))


(provide 'deterred-backup)
;;; deterred-backup.el ends here

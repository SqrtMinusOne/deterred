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

;; The sync

;; Strategies:
;; - Replace (use row count)
;; - Replace (use last timestamp)?
;; - Merge by hostname + timestamp attribute (use last timestamp)
;; - Merge by key attributes (use meta_table_updates)
;; Also check if saved row count matches the synced one.

;;; Code:
(require 'deterred-db)

(defcustom deterred-sync-location "~/.deterred/sync/"
  "The path to where sync DB snapshots are stored.  Change this."
  :group 'deterred
  :type 'string)

(defun deterred-sync--get-file-name (hostname)
  "Get path to a database from HOSTNAME in the sync directory."
  (concat
   (file-name-as-directory
    (expand-file-name deterred-sync-location))
   "database." hostname ".db"))

(defun deterred-sync-state ()
  "Return the current DETERRED sync state.

The state is a list of alist with the following keys:
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
           #'string-lessp
           #'identity
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

(defun deterred-sync--execute-current ()
  "Copy the current database to the sync location."
  (copy-file deterred-db-location
             (deterred-sync--get-file-name (system-name))
             t t t t))

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
           (action . ,(lambda () (message "TODO")))))
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

(provide 'deterred-sync)
;;; deterred-sync.el ends here

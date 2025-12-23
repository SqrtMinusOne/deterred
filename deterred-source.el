;;; deterred-source.el --- Abstract class for data source for DETERRED -*- lexical-binding: t -*-

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

;; DETERRED is connected to external data via data sources, which are
;; subclasses of `deterred-source', implemented here.  A data source
;; does two things:
;; - Inject data into the database
;; - Present some of it ot the user
;;
;; Injecting is implemented via `deterred-source-sync'.  If it's
;; possible to call, `deterred-source-sync-p' must return a non-nil
;; value.
;;
;; `deterred-source-day-summary' implements summary for the "on this
;; day" interface, `deterred-source-range-summary' implements summary
;; for a date range.  The default implementation of
;; `deterred-source-day-summary' calls `deterred-source-range-summary'
;; for one day.

;;; Code:
(require 'eieio)

(defcustom deterred-sources nil
  "DETERRED sources list."
  :group 'deterred
  :type 'list)

;;;###autoload
(defclass deterred-source ()
  ((name :initarg :name :type string)
   (warn-days :initarg :warn-days :type (or null integer) :initform 7))
  "Abstract superclass for DETERRED datasources."
  :abstract t)

(cl-defgeneric deterred-source-range (source &optional db)
  "Get the data availability range for SOURCE.

DB is the sqlite database object.

Return a cons cell, with car as the start timestamp, and cdr as the
end timestamp.")

(cl-defgeneric deterred-source-sync (source &optional callback)
  "Sync SOURCE, if possible.

Call CALLBACK when done.")

(cl-defmethod deterred-source-sync ((_source deterred-source) &optional callback)
  "Dummy implementation for syncing SOURCE.

Call CALLBACK."
  (when callback (funcall callback)))

(cl-defmethod deterred-source-sync-p ((source deterred-source))
  "Return non-nil if there's a sync method for SOURCE."
  ;; I don't know how `cl-generic' specializers actually work, so I
  ;; just hope that the `car' of a specializer is a class I need.
  (member
   (list (type-of source))
   (mapcar
    #'cl--generic-method-specializers
    (cl--generic-method-table (cl--generic #'deterred-source-sync)))))

(cl-defgeneric deterred-source-actions (source &optional callback)
  "Dispatch actions on SOURCE.

Call CALLBACK when done.")

(cl-defmethod deterred-source-actions-p ((source deterred-source))
  "Return non-nil if SOURCE can dispatcher actions."
  (member
   (list (type-of source))
   (mapcar
    #'cl--generic-method-specializers
    (cl--generic-method-table (cl--generic #'deterred-source-actions)))))

(cl-defgeneric deterred-source-range-summary (source start end &optional db)
  "Make a summary for [START, END] from SOURCE.

Return an alist with the following keys:
- `:short-description'
- `:long-description'
- `:long-description-fn'
All can be nil.

If `:long-description-fn' is non-nil, it will be used instead of
`long-description'.  This is useful when the description doesn't fit
into string, e.g., it needs to call `magit-insert-section'.")

(cl-defmethod deterred-source-range-summary ((_source deterred-source) _start _end &optional _db)
  "A dummy implementation of `deterred-source-range-summary'."
  nil)

(cl-defgeneric deterred-source-day-summary (source timestamp &optional db)
  "Make a summary for TIMESTAMP from SOURCE.

See `deterred-source-range-summary' for the description.")

(cl-defmethod deterred-source-day-summary ((source deterred-source) timestamp &optional db)
  "A dummy implementation of `deterred-source-day-summary'.

SOURCE is a `deterred-source' instance.  TIMESTAMP has to be rounded
to the start of day, e.g. use `deterred-utils-ts-to-day-start'.  DB is
a SQLite connection objects."
  (deterred-source-range-summary source timestamp (+ (* 60 60 24) timestamp) db))

(defun deterred-source--actions-pick (action-table &optional callback)
  "Prompt the user with ACTION-TABLE and execute the pick.

ACTION-TABLE is a list of lists with the following items:
- action name
- function
- whether to pass CALLBACK as the first argument."
  (let* ((action-name (completing-read "Pick action: " action-table))
         (action-value (alist-get action-name action-table nil nil #'equal))
         (action-fn (nth 0 action-value))
         (action-callback (nth 1 action-value)))
    (unless action-value
      (user-error "Action %s not found" action-name))
    (if action-callback
        (funcall action-fn callback)
      (if (commandp action-fn)
          (call-interactively action-fn)
        (funcall action-fn)
        (when callback
          (funcall callback))))))

(provide 'deterred-source)
;;; deterred-source.el ends here

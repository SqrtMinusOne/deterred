;;; deterred-source.el --- TODO -*- lexical-binding: t -*-

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
(require 'eieio)

(defcustom deterred-sources nil
  "DETERRED sources list."
  :group 'deterred
  :type 'list)

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

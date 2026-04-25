;;; deterred-export.el --- Export DETERRED data -*- lexical-binding: t -*-

;; Copyright (C) 2026 Korytov Pavel

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

;; Export functionality for DETERRED.  See `deterred-export'.

;;; Code:
(require 'cl-lib)
(require 'json)

(require 'deterred-dashboard)
(require 'deterred-utils)

(defun deterred-export (config folder)
  "Export DETERRED data accoding to CONFIG.

Save the resulting JSON files with data into FOLDER.

CONFIG is a list of alists with the following keys:
- `dashboard': an instance of `deterred-dashboard'
- `params': optional, filters to apply to the dashboard
- `dataset': the dataset name provided by the dashboard
- `name': optional, name to save the file
- `process': optional, a function to postprocess the dataset."
  (let ((cache (make-hash-table :test #'equal))
        (i 0) (total (seq-length config))
        (folder (file-name-as-directory folder)))
    (unless (file-directory-p folder)
      (mkdir folder t))
    (dolist (datum config)
      (message "Export %s/%s" i total)
      (let* ((dashboard (alist-get 'dashboard datum))
             (params (deterred-utils-merge-alists
                      (list
                       (deterred-dashboard-default-params dashboard)
                       (alist-get 'params datum))))
             (key (prin1-to-string (list dashboard params)))
             (datasets
              (or (gethash key cache)
                  (puthash key (deterred-dashboard-fetch-datasets dashboard params)
                           cache)))
             (dataset (alist-get (alist-get 'dataset datum) datasets))
             (name (or (alist-get 'name datum)
                       (symbol-name (alist-get 'dataset datum)))))
        (unless dataset
          (error "Dataset %s not found in %s" (alist-get 'dataset datum) dashboard))
        (when (alist-get 'process datum)
          (setq dataset (funcall (alist-get 'process datum) dataset)))
        (with-temp-file (concat folder name ".json")
          (insert (json-encode dataset)))
        (cl-incf i)))))

(provide 'deterred-export)
;;; deterred-export.el ends here

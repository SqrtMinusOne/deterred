;;; deterred-dired.el --- DETERRED & Dired integration. -*- lexical-binding: t -*-

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
(require 'org-duration)
(require 'dired)

(require 'deterred-db)

(defvar deterred-wakatime-dired--project-id-by-path nil
  "A hash map mapping projects paths to their ids.")

(defun deterred-dired--index-build (items &optional sum-field index project-root)
  "Build a time spent per file index.

ITEMS is a list of alists with the following keys:
- path
- SUM-FIELD - the field to summarise.
- timestamp
The list needs to be ordered by timestamp.

If INDEX is non-nil, extend this instead.

If PROJECT-ROOT is non-nil, consider paths in ITEMS as relative from
it, and do not update items above PROJECT-ROOT in the index.

The return value is a hashmap with absolute paths as keys and cons
cells as values, in which the car is the total sum of the sum-field,
and the cdr is the last recorded timestamp."
  (unless index
    (setq index (make-hash-table :test #'equal)))
  (dolist (item items)
    (let* ((parts (file-name-split (alist-get 'path item)))
           (value (alist-get sum-field item))
           (timestamp (alist-get 'timestamp item)))
      (cl-loop
       for i from (if project-root 2 1) to (seq-length parts)
       for path = (apply #'file-name-concat (or project-root "/") (seq-take parts i))
       do (puthash
           path (cons (+ value (car (gethash path index (cons 0 0)))) timestamp)
           index))))
  index)

(defun deterred-dired--get-project-id-by-path ()
  "Get a hashmaps indexing project ids by path.

The first one maps project id to their full paths.  The second one
maps all children project ids to all paths up to the root directory."
  (let* ((db (deterred-db--init))
         (projects (deterred-db-select-alist
                    db "SELECT id, project_root FROM wakatime_projects
WHERE project_root IS NOT NULL AND name != 'Unknown Project'"))
         (id-by-path (make-hash-table :test 'equal))
         (ids-by-root (make-hash-table :test 'equal)))
    (dolist (item projects)
      (puthash (alist-get 'project_root item) (alist-get 'id item) id-by-path)
      (let* ((parts (file-name-split (alist-get 'project_root item))))
        (cl-loop for i from 1 to (seq-length parts)
                 for path = (apply #'file-name-concat "/" (seq-take parts i))
                 do (puthash
                     path (cons (alist-get 'id item) (gethash path ids-by-root))
                     ids-by-root))))
    (list id-by-path ids-by-root)))

(defun deterred-dired--project-id (path)
  "Get wakatime project id by PATH."
  (let* ((parts (file-name-split path))
         (i (seq-length parts)))
    (cl-block search
      (while (> i 0)
        (let* ((cand (apply #'file-name-concat "/" (seq-take parts i)))
               (project-id
                (gethash
                 cand deterred-wakatime-dired--project-id-by-path)))
          (when project-id
            (cl-return-from search project-id)))
        (setq i (1- i))))))

;;;###autoload
(defclass deterred-dired-source ()
  (name :initarg :name :type string)
  "Abstract superclass for DETERRED dired integrations."
  :abstract t)

(cl-defgeneric deterred-dired-source-index-init (source &optional db)
  "Initialise index for Dired integration.

SOURCE is an instance of `deterred-dired-source', DB is the SQLite
connection object.

This should return the result of `deterred-dired--index-build'.")

(cl-defgeneric deterred-dired-source-index-project (source id index &optional db)
  "Update index for a particular project ID.

INDEX is the index value as returned by `deterred-dired--index-build'.
This is mean to add per-file values to index wherever possible.  This
method will be called only once for each project.

SOURCE is an instance of `deterred-dired-source', DB is the SQLite
connection object.")

(cl-defmethod deterred-dired-source-index-project
  ((_source deterred-dired-source) _id _index &optional _db)
  "Do nothing."
  nil)

(defun deterred-dired ())

(provide 'deterred-dired)
;;; deterred-dired.el ends here

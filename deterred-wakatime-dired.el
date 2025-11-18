;;; deterred-wakatime-dired.el --- WakaTime & Dired integration. -*- lexical-binding: t -*-

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

;; WakaTime and Dired integration using DETERRED.
;;
;; This adds two things:
;; - `deterred-wakatime-dired-mode' - shows the amount of time spent per
;;   directory;
;; - `deterred-wakatime-dired-dashboard' - invokes the WakaTime
;;   dashboard for all projects in the current directory.

;;; Code:
(require 'deterred-db)
(require 'deterred-wakatime)
(require 'org-duration)

(defvar deterred-wakatime-dired--index nil
  "Index as defined by `deterred-wakatime-dired--index'.")

(defvar deterred-wakatime-dired--project-id-by-path nil
  "A hash map mapping projects paths to their ids.")

(defvar deterred-wakatime-dired--project-ids-by-root nil
  "A hash map mapping filesystem paths to project ids.")

(defvar deterred-wakatime-dired--range nil
  "A cell (start timestamp . end timestamp) to filter the index.")

(defvar deterred-wakatime-dired--projects-initialised nil
  "A hashmap with initialised project IDs as keys.")

(defun deterred-wakatime-dired--index-build (items &optional index project-root)
  "Build a time spent per file index.

ITEMS is a list of alists with the following keys:
- path
- duration - how many seconds were spent in the file
- timestamp
The list needs to be ordered by timestamp.

If INDEX is non-nil, extend this instead.

If PROJECT-ROOT is non-nil, consider paths in ITEMS as relative from
it, and do not update items above PROJECT-ROOT in the index.

The return value is a hashmap with absolute paths as keys and cons
cells as values, in which the car is the total number of seconds
spent, and the cdr is the last recorded timestamp."
  (unless index
    (setq index (make-hash-table :test #'equal)))
  (dolist (item items)
    (let* ((parts (file-name-split (alist-get 'path item)))
           (duration (alist-get 'duration item))
           (timestamp (alist-get 'timestamp item)))
      (cl-loop
       for i from (if project-root 2 1) to (seq-length parts)
       for path = (apply #'file-name-concat (or project-root "/") (seq-take parts i))
       do (puthash
           path (cons (+ duration (car (gethash path index (cons 0 0)))) timestamp)
           index))))
  index)

(defun deterred-wakatime-dired--index-init (&optional start-date end-date)
  "Initialise `deterred-wakatime-dired--index'.

START-DATE and END-DATE are unix timestamps.

The return value is as defined by
`deterred-wakatime-dired--index-build', initialised using projects and
projectless wakatime entities."
  (let* ((db (deterred-db--init))
         (project-data
          (deterred-db-select-template-alist
           db
           "SELECT
  wp.project_root \"path\",
  sum(wgt.total_seconds) duration,
  max(wgt.\"timestamp\") timestamp
FROM wakatime_grand_total wgt
INNER JOIN wakatime_projects wp ON wp.id = wgt.project_id
WHERE wp.project_root IS NOT NULL AND wp.name != 'Unknown Project'
[[AND wp.timestamp >= :start-date]] [[AND wp.timestamp <= :end-date]]
GROUP BY wp.project_root"
           `((:start-date . ,start-date) (:end-date . ,end-date))))
         (orphan-entities-data
          (deterred-db-select-template-alist
           db
           "SELECT
  we.name \"path\",
  sum(we.total_seconds) duration,
  max(we.\"timestamp\") timestamp
FROM wakatime_entities we
INNER JOIN wakatime_projects wp ON wp.id = we.project_id
WHERE wp.name = 'Unknown Project'
[[AND wp.timestamp >= :start-date]] [[AND wp.timestamp <= :end-date]]
GROUP BY we.name
HAVING duration > 60"
           `((:start-date . ,start-date) (:end-date . ,end-date))))
         index)
    (setq index (deterred-wakatime-dired--index-build project-data))
    (deterred-wakatime-dired--index-build orphan-entities-data index)))

(defun deterred-wakatime-dired--get-project-id-by-path ()
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

(defun deterred-wakatime-dired--init (&optional start-date end-date)
  "Initialise `deterred-wakatime-dired'.

START-DATE and END-DATE are UNIX timestamps."
  (setq deterred-wakatime-dired--index
        (deterred-wakatime-dired--index-init start-date end-date))
  (setq deterred-wakatime-dired--range (when (or start-date end-date)
                                         (cons start-date end-date)))
  (let ((data (deterred-wakatime-dired--get-project-id-by-path)))
    (setq deterred-wakatime-dired--project-id-by-path
          (nth 0 data))
    (setq deterred-wakatime-dired--project-ids-by-root
          (nth 1 data)))
  (setq deterred-wakatime-dired--projects-initialised
        (make-hash-table :test #'equal)))

(defun deterred-wakatime-dired--project-id (path)
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

(defun deterred-wakatime-dired--index-project (id)
  "Initialise `deterred-wakatime-dired--index' with project ID.

Be sure to call this only once for each project."
  (when-let* ((db (deterred-db--init))
              (project-root
               (caar (sqlite-execute
                      db "SELECT project_root FROM wakatime_projects WHERE id = ?"
                      (list id))))
              (project-data
               (deterred-db-select-template-alist
                db
                "SELECT
  we.project_path \"path\",
  sum(we.total_seconds) duration,
  max(we.\"timestamp\") timestamp
FROM wakatime_entities we
INNER JOIN wakatime_projects wp ON wp.id = we.project_id
WHERE wp.id = :id AND we.project_path IS NOT NULL
[[AND wp.timestamp >= :start-date]] [[AND wp.timestamp <= :end-date]]
GROUP BY we.name"
                `((:id . ,id)
                  (:start-date . ,(car deterred-wakatime-dired--range))
                  (:end-date . ,(car deterred-wakatime-dired--range))))))
    (deterred-wakatime-dired--index-build
     project-data deterred-wakatime-dired--index project-root)))

(defun deterred-wakatime-dired--ensure-dir (path)
  "Unsure `deterred-wakatime-dired--index' is initialised for PATH."
  (unless deterred-wakatime-dired--index
    (deterred-wakatime-dired--init
     (car deterred-wakatime-dired--range)
     (cdr deterred-wakatime-dired--range)))
  (let ((project-id (deterred-wakatime-dired--project-id path)))
    (when (and project-id
               (not (gethash
                     project-id
                     deterred-wakatime-dired--projects-initialised)))
      (deterred-wakatime-dired--index-project project-id)
      (puthash project-id t deterred-wakatime-dired--projects-initialised))))

(defun deterred-wakatime-dired--format-info (datum)
  "Format DATUM for display in Dired.

DATUM is a value stored in `deterred-wakatime-dired--index'."
  (let ((str (propertize
              (org-duration-from-minutes (/ (car datum) 60))
              'face 'deterred-faces-info)))
    (let* ((le (line-end-position))
           (aw (- (window-width)
                  (1+ (save-excursion
                        (goto-char le)
                        (current-column))))))
      (if (not (> aw 0)) "\n" (concat str "\n")))))

(defun deterred-wakatime-dired--map-dired-lines (fun)
  "Call FUN on each entry in the Dired buffer.

Same as `dired-map-dired-file-lines', but include directories."
  (save-excursion
    (let (file buffer-read-only)
      (goto-char (point-min))
      (while (not (eobp))
	    (save-excursion
	      (and (not (eolp))
	           (setq file (dired-get-filename nil t)) ; nil on non-file
	           (progn (end-of-line)
		              (funcall fun file))))
	    (forward-line 1)))))

(defun deterred-wakatime-dired--longest-line ()
  "Return the longest line length in the Dired buffer."
  (let ((val 0))
    (deterred-wakatime-dired--map-dired-lines
     (lambda (file)
       (setq val (max val (- (line-end-position) (line-beginning-position))))))
    val))

(defun deterred-wakatime-dired--add-info ()
  "Add `deterred-wakatime-dired' info to the Dired buffer."
  (deterred-wakatime-dired--ensure-dir (expand-file-name default-directory))
  (remove-overlays (point-min) (point-max) 'deterred-wakatime-dired t)
  (let ((max-length (deterred-wakatime-dired--longest-line)))
    (deterred-wakatime-dired--map-dired-lines
     (lambda (file)
       (let ((datum (gethash file deterred-wakatime-dired--index))
             (current-length (- (line-end-position) (line-beginning-position))))
         (when datum
           (goto-char (line-end-position))
           (let* ((ov (make-overlay (point) (1+ (point))))
                  (str (concat
                        (make-string (+ 2 (- max-length current-length)) ? )
                        (deterred-wakatime-dired--format-info datum))))
             (overlay-put ov 'deterred-wakatime-dired t)
             ;; dired-git-info does this...
             (overlay-put ov 'display str)
             (overlay-put ov 'priority -60))))))))

(defun deterred-wakatime-dired--remove-info ()
  "Remove `deterred-wakatime-dired' info from the Dired buffer."
  (remove-overlays (point-min) (point-max) 'deterred-wakatime-dired t))

(define-minor-mode deterred-wakatime-dired-mode
  "Display wakatime info in Dired."
  :global t
  :group 'deterred
  :after-hook
  (progn
    (if deterred-wakatime-dired-mode
        (progn
          (add-hook 'dired-after-readin-hook #'deterred-wakatime-dired--add-info)
          (cl-loop for buffer being the buffers
                   do (with-current-buffer buffer
                        (when (derived-mode-p 'dired-mode)
                          (deterred-wakatime-dired--add-info)))))
      (remove-hook 'dired-after-readin-hook #'deterred-wakatime-dired--add-info)
      (cl-loop for buffer being the buffers
               do (with-current-buffer buffer
                    (when (derived-mode-p 'dired-mode)
                      (deterred-wakatime-dired--remove-info)))))))

(defun deterred-wakatime-dired--related-projects-id (path)
  "Get all project ids related to PATH.

Related meaning stored under PATH or containing project root in PATH."
  (deterred-wakatime-dired--ensure-dir (expand-file-name default-directory))
  (let ((current-project (deterred-wakatime-dired--project-id path)))
    (if current-project
        (list current-project)
      (gethash path deterred-wakatime-dired--project-ids-by-root))))

(defvar deterred-wakatime-dired--dashboard nil
  "A Wakatime Dashboard instance.")

(defun deterred-wakatime-dired-dashboard (paths)
  "Open the Wakatime dashboard for PATHS."
  (interactive
   (list (or (dired-get-marked-files nil 'marked)
             (list (directory-file-name
                    (expand-file-name default-directory))))))
  (let ((table (make-hash-table :test #'equal))
        (all-ids))
    (dolist (path paths)
      (let ((ids (deterred-wakatime-dired--related-projects-id path)))
        (dolist (id ids)
          (puthash id t table))))
    (maphash (lambda (id _v) (push id all-ids)) table)
    (deterred-dashboard-open
     (or deterred-wakatime-dired--dashboard
         (deterred-dashboard-wakatime))
     `((:projects . ,all-ids)))))

(provide 'deterred-wakatime-dired)
;;; deterred-wakatime-dired.el ends here

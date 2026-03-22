;;; deterred-chains.el --- Chain normalization helpers for DETERRED -*- lexical-binding: t -*-

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

;; Helpers for working with timestamp chains in DETERRED.

;;; Code:

(require 'cl-lib)
(require 'seq)

(defun deterred-chains-normalize (chains timeout &optional merge-data-fn)
  "Normalize CHAINS of timestamps by TIMEOUT.

A chain is a list of entries, where each entry is a list
\(START END DATA\).  START is a UNIX timestamp, END is a UNIX timestamp
or nil (defaults to START), and DATA is arbitrary data or nil.

TIMEOUT is a number of seconds.

MERGE-DATA-FN, when non-nil, is called with two DATA values when
entries from the same chain are merged.  It should return the combined
DATA.  When nil, the first entry's DATA is kept.

The function returns chains in the same order, with each entry as
\(START END DATA\), where START and END are always set.  Gaps less than
TIMEOUT are removed.  Resulting chains do not intersect.

This is similar to what WakaTime does by converting a list of
individual \"heartbeats\" into timespans."
  (let (series
        normalized-series
        current-item)
    ;; Merge chains into one series.
    ;; A series item is a list (chain-id start end data).
    (cl-loop for i from 0
             for chain in chains
             do (cl-loop for entry in chain
                         do (push (list i
                                        (nth 0 entry)
                                        (or (nth 1 entry) (nth 0 entry))
                                        (nth 2 entry))
                                  series)))
    ;; Normalize series.
    (dolist (item (append (seq-sort-by (lambda (d) (nth 1 d)) #'< series) (list nil)))
      (cond
       ((not current-item) (setq current-item item))
       ((null item) (push current-item normalized-series))
       (t
        (let ((is-timeout (> (- (nth 1 item) (nth 2 current-item)) timeout))
              (is-chain-switch (not (= (nth 0 item) (nth 0 current-item)))))
          (cond
           (is-timeout
            (push current-item normalized-series)
            (setq current-item item))
           (is-chain-switch
            (setf (nth 2 current-item) (nth 1 item))
            (push current-item normalized-series)
            (setq current-item item))
           (t
            (setf (nth 2 current-item) (max (nth 2 current-item) (nth 2 item)))
            (when merge-data-fn
              (setf (nth 3 current-item)
                    (funcall merge-data-fn
                             (nth 3 current-item)
                             (nth 3 item))))))))))
    ;; Back into chains.
    (mapcar
     (lambda (group)
       (seq-sort-by
        #'car
        #'<
        (mapcar
         (lambda (item) (list (nth 1 item) (nth 2 item) (nth 3 item)))
         (cdr group))))
     (seq-sort-by
      #'car
      #'<
      (seq-group-by #'car normalized-series)))))

(defun deterred-chains-intersection (chains &optional merge-data-fn)
  "Calculate intersection of CHAINS.

A chain is a list of elements like (<start> <end> <data>), where
<start> and <end> are mandatory and <data> is optional.

Return one merged chain.

If MERGE-DATA-FN is non-nil, it will be used to populate the <data>
field of the merged chain.  The function will be called with N
arguments, where N is the number of chains, and each argument is the
data field of the relevant chain entry in the order that CHAINS were
given."
  (let ((series
         ;; A list of (<'start | 'end> <timestamp> <data> <series-i>)
         (seq-sort
          (lambda (e1 e2)
            ;; If timestamps are the same, ends go before starts
            (if (= (nth 1 e1) (nth 1 e2))
                (eq (nth 0 e1) 'end)
              (< (nth 1 e1) (nth 1 e2))))
          (cl-loop for i from 0
                   for chain in chains
                   append (cl-mapcan
                           (lambda (e)
                             (unless (nth 1 e)
                               (error "`deterred-chains-interesction' requires both start and end in entries"))
                             (list
                              `(start ,(nth 0 e) ,(nth 2 e) ,i)
                              `(end ,(nth 1 e) ,(nth 2 e) ,i)))
                           chain))))
        ;; Active entries from each chain
        (entries-bitmap (make-vector (seq-length chains) nil))
        current-intersection-start
        intersections)
    ;; Intersect series
    (dolist (e series)
      (if (eq (car e) 'start)
          ;; We assume chains have no overlapping entries, otherwise
          ;; [s1 s2 e1 e2] will be treated as [s1 e1]
          (aset entries-bitmap (nth 3 e) (or (aref entries-bitmap (nth 3 e)) e))
        (aset entries-bitmap (nth 3 e) nil))
      ;; Check if we are currently in intersection
      (if (seq-every-p #'identity entries-bitmap)
          ;; If so, start the interesection counter unless it's started
          (unless current-intersection-start
            (setq current-intersection-start (nth 1 e)))
        ;; Then we aren't in intersection. We have to record the
        ;; current interaction if there was one
        (when current-intersection-start
          (let ((data (cl-loop for i from 0
                               for i-e across entries-bitmap
                               ;; `entries-bitmap' will have all
                               ;; active entires except one from the
                               ;; recently ended chain
                               if (= i (nth 3 e)) collect (nth 2 e)
                               else collect (nth 2 i-e))))
            (push
             (list current-intersection-start (nth 1 e)
                   (when merge-data-fn
                     (apply merge-data-fn data)))
             intersections))
          (setq current-intersection-start nil))))
    (seq-sort-by #'car #'< intersections)))

(defun deterred-chains-group-by (chain date-format)
  "Group CHAIN by DATE-FORMAT.

Return a list of cons cells, where car is the date in DATE-FORMAT, and
cdr is the total number of seconds in the group.

For DATE-FORMAT, see `format-time-string'."
  (mapcar
   (lambda (group)
     (cons (car group)
           (seq-reduce (lambda (acc e)
                         (+ acc (- (nth 1 e) (nth 0 e))))
                       (cdr group)
                       0)))
   (seq-group-by
    (lambda (e) (format-time-string date-format (car e)))
    chain)))

(provide 'deterred-chains)
;;; deterred-chains.el ends here

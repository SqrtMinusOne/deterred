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

(defun deterred-chains-discretize--ranges (chain timestamps &optional merge-data-fn)
  "Discretize CHAIN by TIMESTAMPS.

See `deterred-chains-discretize' for arguments description, including
MERGE-DATA-FN."
  (let ((data
         ;; One would think it would be faster to assume TIMESTAMP and
         ;; CHAIN are sorted a do a merge sort, but the built-in
         ;; `sort' is actually faster than any merge sort algorithm I
         ;; could write in elisp despite being asymptotically worse
         (seq-sort
          (lambda (e1 e2)
            (if (= (cadr e1) (cadr e2))
                (eq (car e1) 'interval)
              (< (cadr e1) (cadr e2))))
          (append
           (mapcar (lambda (e) (cons 'chain e)) chain)
           (mapcar (lambda (e) (list 'interval e)) timestamps))))
        current-interval current-chain-elems res)
    (dolist (e data)
      ;; If we're at an interval border
      (if (eq (car e) 'interval)
          ;; It may be our first interval, in which case we just start it and do nothing
          (if (not current-interval)
              (setq current-interval (cadr e))
            ;; Otherwise, we have to add an element to res.
            ;; I suppose I could've done it more efficienly but I'm
            ;; writing this at 0.30am because I can't sleep T_T
            (let* ((start current-interval)
                   (end (1- (cadr e)))
                   new-current-chain-elems
                   (total 0))
              ;; We iterate over all current chain elements which
              ;; found themselves in the inverval, calculate the time
              ;; they contributed, and leave only the elments that can
              ;; contribute to the next interval
              (dolist (elem current-chain-elems)
                (cl-incf total
                         (max (- (min end (caddr elem)) (max start (cadr elem))) 0))
                (when (> (caddr elem) end)
                  (push elem new-current-chain-elems)))
              (push (list start end
                          (when merge-data-fn
                            (funcall merge-data-fn current-chain-elems))
                          (/ (float total) (- end start)))
                    res)
              (setq current-chain-elems (nreverse new-current-chain-elems))
              (setq current-interval (cadr e))))
        ;; Otherwise, if it's a chain element, we store it
        (push e current-chain-elems)))
    (nreverse res)))

(defun deterred-chains-discretize--points (chain timestamps &optional merge-data-fn)
  "Discretize CHAIN by TIMESTAMPS.

See `deterred-chains-discretize' for arguments description, including
MERGE-DATA-FN."
  ;; See `deterred-chains-discretize--ranges' for comments, it's
  ;; basically the same
  (let ((data
         (seq-sort
          (lambda (e1 e2)
            (if (= (cadr e1) (cadr e2))
                (eq (car e1) 'interval)
              (< (cadr e1) (cadr e2))))
          (append
           (mapcar (lambda (e) (cons 'chain e)) chain)
           (mapcar (lambda (e) (list 'interval e)) timestamps))))
        current-chain-elems (count 0) (max 0) current-interval res)
    (dolist (e data)
      (if (eq (car e) 'interval)
          (if (not current-interval)
              (setq current-interval (cadr e))
            (let* ((start current-interval)
                   (end (1- (cadr e))))
              (push (list start end
                          (when merge-data-fn
                            (apply merge-data-fn current-chain-elems))
                          count)
                    res)
              (setq current-chain-elems nil)
              (setq count 0)
              (setq current-interval (cadr e))))
        (when current-interval
          (push e current-chain-elems)
          (cl-incf count)
          (setq max (max max count)))))
    (mapcar (lambda (e)
              (list
               (car e)
               (cadr e)
               (cadddr e)
               (/ (float (cadddr e)) max)))
            (nreverse res))))

(defun deterred-chains-discretize (chain timestamps &optional merge-data-fn)
  "Discretize CHAIN by TIMESTAMPS.

CHAIN is a list of elements like (<start> <end> <data>), where
<start> is mandatory, <end> must be either present or abscent in all
elements, and <data> is optional.  TIMESTAMPS is a list of UNIX
timestamps timestamps used as discretization boundaries.

Do not assume TIMESTAMPS and CHAIN are sorted.

Return a list of elements like (<start> <end> <data> <coef>) where
<start> and <end> describe one discretization interval
[<start>,<next-timestamp>), represented as <start> and
<next-timestamp> - 1; <data> is the result of MERGE-DATA-FN applied to
chain elements that overlap that interval; and <coef> is a number from
0 to 1 showing how many CHAIN elements cover the interval.

If CHAIN elements have <end>, <coef> means fraction of the interval
covered by CHAIN.  Otherwise, <coef> is the number of elements in the
interval normalized to 1."
  (if (cadar chain)
      (deterred-chains-discretize--ranges chain timestamps merge-data-fn)
    (deterred-chains-discretize--points chain timestamps merge-data-fn)))

(provide 'deterred-chains)
;;; deterred-chains.el ends here

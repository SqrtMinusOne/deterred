;;; deterred-utils.el --- TODO -*- lexical-binding: t -*-

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
(require 'pcsv)

(defcustom deterred-utils-duration-format '(("d") (special . h:mm))
  "Format defintion for a duration.

Check `org-duration-format' for description."
  :group 'deterred
  :type '(choice
          (const :tag "Use H:MM" h:mm)
          (const :tag "Use H:MM:SS" h:mm:ss)
          (repeat :tag "Use units"
                  (choice
                   (cons :tag "Use units"
                         (string :tag "Unit")
                         (choice (const :tag "Skip when zero" nil)
                                 (const :tag "Always used" t)))
                   (cons :tag "Use a single decimal unit"
                         (const special)
                         (integer :tag "Number of decimals"))
                   (cons :tag "Use both units and H:MM"
                         (const special)
                         (const h:mm))
                   (cons :tag "Use both units and H:MM:SS"
                         (const special)
                         (const h:mm:ss))
                   (const :tag "Use compact form" compact)))))

(defcustom deterred-utils-duration-units
  `(("min" . 1)
    ("h" . 60)
    ("d" . ,(* 60 24))
    ("w" . ,(* 60 24 7))
    ("m" . ,(* 60 24 30))
    ("y" . ,(* 60 24 365.25)))
  "Conversion factor to minutes for a duration.

See `org-duration-units' for description."
  :group 'deterred
  :type '(choice
	      (const :tag "H:MM" h:mm)
	      (const :tag "H:MM:SS" h:mm:ss)
	      (alist :key-type (string :tag "Unit")
		         :value-type (number :tag "Modifier"))))

(defconst deterred-utils-duration-canonical-units
  `(("min" . 1)
    ("h" . 60)
    ("d" . ,(* 60 24)))
  "Canonical time duration units.

See `org-duration-units' for details.")

(defmacro deterred-utils-assert-var-set (var-name)
  "Signal error is VAR-NAME is nil."
  `(unless ,var-name
     (user-error ,(format "%s not set!" var-name))))

(defun deterred-utils-csv-to-alist (file)
  "Read a CSV FILE into alist with `pcsv'."
  (let* ((data (pcsv-parse-file file))
         (header (mapcar #'intern (car data))))
    (cl-loop for row in (cdr data)
             collect (cl-loop for key in header
                              for value in row
                              collect (cons key value)))))

(defun deterred-utils-read-csv-with-python (file)
  "Read a CSV FILE into alist with python.

This works better than `pcsv' for Reddit dump."
  (json-parse-string
   (shell-command-to-string
    (format "cat %s | python -c 'import csv, json, sys; print(json.dumps([dict(r) for r in csv.DictReader(sys.stdin)]))'"
            (shell-quote-argument file)))
   :object-type 'alist))

(defun deterred-utils-duration--modifier (unit &optional canonical)
  "Return modifier associated to string UNIT.
When optional argument CANONICAL is non-nil, refer to
`org-duration-canonical-units' instead of `org-duration-units'."
  (or (cdr (assoc unit (if canonical
			               deterred-utils-duration-canonical-units
			             deterred-utils-duration-units)))
      (error "Unknown unit: %S" unit)))

(defun deterred-utils-string-nw-p (s)
  "Return S if S is a string containing a non-blank character.
Otherwise, return nil."
  (and (stringp s)
       (string-match-p "[^ \r\t\n]" s)
       s))

(defun deterred-utils-duration-from-minutes (minutes &optional fmt canonical)
  "Return duration string for a given number of MINUTES.

This is copied for `org-duration-from-minutes', which see, including
for FMT and CANONICAL.  I just don't want to depend on org in this
package."
  (pcase (or fmt deterred-utils-duration-format)
    (`h:mm
     (format "%d:%02d" (/ minutes 60) (mod minutes 60)))
    (`h:mm:ss
     (let* ((whole-minutes (floor minutes))
            (seconds (mod (* 60 minutes) 60)))
       (format "%s:%02d"
               (deterred-utils-duration-from-minutes whole-minutes 'h:mm)
               seconds)))
    ((pred atom) (error "Invalid duration format specification: %S" fmt))
    ;; Mixed format.  Call recursively the function on both parts.
    ((and duration-format
          (let `(special . ,(and mode (or `h:mm:ss `h:mm)))
            (assq 'special duration-format)))
     (let* ((truncated-format
             ;; Remove "special" mode from duration format in order to
             ;; recurse properly.  Also remove units smaller or equal
             ;; to an hour since H:MM part takes care of it.
             (cl-remove-if-not
              (lambda (pair)
                (pcase pair
                  (`(,(and unit (pred stringp)) . ,_)
                   (> (deterred-utils-duration--modifier unit canonical) 60))
                  (_ nil)))
              duration-format))
            (min-modifier               ;smallest modifier above hour
             (and truncated-format
                  (apply #'min
                         (mapcar (lambda (p)
                                   (deterred-utils-duration--modifier (car p) canonical))
                                 truncated-format)))))
       (if (or (null min-modifier) (< minutes min-modifier))
           ;; There is not unit above the hour or the smallest unit
           ;; above the hour is too large for the number of minutes we
           ;; need to represent.  Use H:MM or H:MM:SS syntax.
           (deterred-utils-duration-from-minutes minutes mode canonical)
         ;; Represent minutes above hour using provided units and H:MM
         ;; or H:MM:SS below.
         (let* ((units-part (* min-modifier (/ (floor minutes) min-modifier)))
                (minutes-part (- minutes units-part))
                (compact (memq 'compact duration-format)))
           (concat
            (deterred-utils-duration-from-minutes units-part truncated-format canonical)
            (and (not compact) " ")
            (deterred-utils-duration-from-minutes minutes-part mode))))))
    ;; Units format.
    (duration-format
     (let* ((fractional
             (let ((digits (cdr (assq 'special duration-format))))
               (and digits
                    (or (wholenump digits)
                        (error "Unknown formatting directive: %S" digits))
                    (format "%%.%df" digits))))
            (selected-units
             (sort (cl-remove-if
                    ;; Ignore special format cells and compact option.
                    (lambda (pair)
                      (pcase pair
                        ((or `compact `(special . ,_)) t)
                        (_ nil)))
                    duration-format)
                   (lambda (a b)
                     (> (deterred-utils-duration--modifier (car a) canonical)
                        (deterred-utils-duration--modifier (car b) canonical)))))
            (separator (if (memq 'compact duration-format) "" " ")))
       (cond
        ;; Fractional duration: use first unit that is either required
        ;; or smaller than MINUTES.
        (fractional
         (let* ((unit (car
                       (or (cl-find-if
                            (lambda (pair)
                              (pcase pair
                                (`(,u . ,req?)
                                 (or req?
                                     (<= (deterred-utils-duration--modifier u canonical)
                                         minutes)))))
                            selected-units)
                           ;; Fall back to smallest unit.
                           (car (last selected-units)))))
                (modifier (deterred-utils-duration--modifier unit canonical)))
           (concat (format fractional (/ (float minutes) modifier)) unit)))
        ;; Otherwise build duration string according to available
        ;; units.
        ((deterred-utils-string-nw-p
          (string-trim
           (mapconcat
            (lambda (units)
              (pcase-let* ((`(,unit . ,required?) units)
                           (modifier (deterred-utils-duration--modifier
                                      unit canonical)))
                (cond ((<= modifier minutes)
                       (let ((value (floor minutes modifier)))
                         (cl-decf minutes (* value modifier))
                         (format "%s%d%s" separator value unit)))
                      (required? (concat separator "0" unit))
                      (t ""))))
            selected-units
            ""))))
        ;; No unit can properly represent MINUTES.  Use the smallest
        ;; one anyway.
        (t
         (pcase-let ((`((,unit . ,_)) (last selected-units)))
           (concat "0" unit))))))))

(defun deterred-utils-ts-to-day-start (&optional timestamp)
  "Move TIMESTAMP to start of day."
  (let ((time (decode-time timestamp)))
    (setf (decoded-time-second time) 0
          (decoded-time-minute time) 0
          (decoded-time-hour time) 0)
    (time-convert (encode-time time) #'integer)))

(defun deterred-utils-get-this-day (start &optional today)
  "Get TODAY on all years since START.

E.g., this day one year ago, two years ago, etc.  START and TODAY are
time-values.

Return a list of cons cells, where the car is a human-readable value
and the cdr is the timestamp."
  (let ((time (decode-time today))
        (i 0)
        res)
    (setf (decoded-time-second time) 0
          (decoded-time-minute time) 0
          (decoded-time-hour time) 0)
    (while (time-less-p start (encode-time time))
      (setf (decoded-time-year time) (1- (decoded-time-year time)))
      (setq i (1+ i))
      (push (cons (format (if (= i 1) "%s year ago" "%s years ago") i)
                  (time-convert (encode-time time) #'integer))
            res))
    (nreverse res)))

(defun deterred-utils-make-line (string)
  (replace-regexp-in-string
   (rx (+ (or whitespace "\n")))
   " "
   (string-trim string)))

(defun deterred-trim-ldots (string max-length)
  (if (<= (seq-length string) max-length)
      string
    (format "%s..." (substring string 0 (- max-length 3)))))

(provide 'deterred-utils)
;;; deterred-utils.el ends here

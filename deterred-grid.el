;;; deterred-grid.el --- TODO -*- lexical-binding: t -*-

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
(require 'vtable)

(define-derived-mode deterred-grid-mode special-mode "DETERRED Grid"
  :group 'deterred
  (setq-local buffer-read-only t))

(defvar deterred-grid-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") (lambda ()
                                (interactive)
                                (quit-window t)))
    (when (fboundp #'evil-define-key*)
      (evil-define-key* '(normal motion) map
        "q" (lambda ()
              (interactive)
              (quit-window t))))
    map)
  "A keymap for `deterred-grid-mode'.")

(defun deterred-grid-show (data &rest vtable-args)
  "Display a list of alists using `vtable'.

DATA is a list of alists, VTABLE-ARGS is passed to `make-vtable'."
  (let ((buffer (generate-new-buffer "*DETERRED-grid*")))
    (with-current-buffer buffer
      (deterred-grid-mode)
      (let* ((inhibit-read-only t)
             (columns (mapcar #'car (car data)))
             (objects (mapcar (lambda (datum)
                                (mapcar (lambda (column)
                                          (alist-get column datum))
                                        columns))
                              data)))
        (apply #'make-vtable
               :objects objects
               :columns (mapcar #'symbol-name columns)
               :use-header-line nil
               vtable-args)))
    (switch-to-buffer-other-window buffer)))

(provide 'deterred-grid)
;;; deterred-grid.el ends here

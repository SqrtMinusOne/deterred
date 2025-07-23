;;; deterred-dispatcher.el --- TODO -*- lexical-binding: t -*-

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
(require 'magit-section)

(defgroup deterred-faces ()
  "Faces for DETERRED."
  :group 'deterred)

(defface deterred-faces-section-heading-1
  '((t (:inherit magit-section-heading)))
  "Face for primary section headings."
  :group 'deterred-faces)

(defface deterred-faces-section-heading-2
  '((t (:inherit magit-section-secondary-heading)))
  "Face for secondary section headings."
  :group 'deterred-faces)

(defface deterred-faces-section-heading-3
  '((t (:inherit warning)))
  "Face for tertiary section headings."
  :group 'deterred-faces)

(defface deterred-faces-section-heading-4
  '((t (:inherit success)))
  "Face for quaternary section headings."
  :group 'deterred-faces)

(defface deterred-faces-source-name
  '((t (:inherit font-lock-string-face)))
  "Face for data source names."
  :group 'deterred-faces)

(defface deterred-faces-date
  '((t (:inherit font-lock-constant-face)))
  "Face for dates."
  :group 'deterred-faces)

(provide 'deterred-faces)
;;; deterred-faces.el ends here

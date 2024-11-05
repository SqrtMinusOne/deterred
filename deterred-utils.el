;;; deterred-utils.el --- TODO -*- lexical-binding: t -*-

;; Copyright (C) 2024 Korytov Pavel

;; Author: Korytov Pavel <thexcloud@gmail.com>
;; Maintainer: Korytov Pavel <thexcloud@gmail.com>
;; Homepage: https://github.com/SqrtMinusOne/deterred-utils.el

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
(defun deterred-utils-uuid-to-sqlite-hex (uuid)
  "Convert UUID string to SQLite hex literal."
  (concat
   "X'" (replace-regexp-in-string "-" "" uuid) "'"))

(defun deterred-utils-sqlite-hex-to-uuid (hex-string)
  "Convert HEX-STRING from SQLite hex() function to UUID format."
  (format "%s-%s-%s-%s-%s"
          (substring hex-string 0 8)
          (substring hex-string 8 12)
          (substring hex-string 12 16)
          (substring hex-string 16 20)
          (substring hex-string 20 32)))

(provide 'deterred-utils)
;;; deterred-utils.el ends here

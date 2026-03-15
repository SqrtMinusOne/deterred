;;; deterred.el --- Dispatcher for Emacs Timeline Examination, Retrospective Review, and Enhanced Dashboard. -*- lexical-binding: t -*-

;; Copyright (C) 2026 Korytov Pavel

;; Author: Korytov Pavel <thexcloud@gmail.com>
;; Maintainer: Korytov Pavel <thexcloud@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29") (uuidgen "1.3") (libmpdel "2.0") (pcsv "1.4.0") (request "0.3.2") (validate "1.0.4") (magit-section "4.3.6") (org "9.6.6") (llm "0.29.0"))
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
;; Utilities
(require 'deterred-faces)
(require 'deterred-format)
(require 'deterred-utils)

;; Core logic
(require 'deterred-source)
(require 'deterred-db)
(require 'deterred-backup)
(require 'deterred-grid)
(require 'deterred-dashboard)
(require 'deterred-dispatcher)
(require 'deterred-source)

(require 'deterred-activitywatch)
(require 'deterred-ai)
(require 'deterred-digikam)
(require 'deterred-habits)
(require 'deterred-hledger)
(require 'deterred-hledger-exchange)
(require 'deterred-locations)
(require 'deterred-messengers-chains)
(require 'deterred-messengers)
(require 'deterred-mpd)
(require 'deterred-mpd-emms)
(require 'deterred-org)
(require 'deterred-org-journal-tags)
(require 'deterred-org-roam)
(require 'deterred-podcasts)
(require 'deterred-read-it-later)
(require 'deterred-social)
(require 'deterred-transport)
(require 'deterred-wakatime-dired)
(require 'deterred-wakatime)

(defgroup deterred nil
  "Dispatcher for Emacs Timeline Examination, Retrospective Review, and Enhanced Dashboard."
  :group 'applications)

;;;###autoload
(defun deterred ()
  "Open DETERRED interactive buffer."
  (interactive)
  (deterred-dispatcher))

(provide 'deterred)
;;; deterred.el ends here

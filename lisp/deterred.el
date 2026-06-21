;;; deterred.el --- Dispatcher for Emacs Timeline Examination, Retrospective Review, and Enhanced Dashboard. -*- lexical-binding: t -*-

;; Copyright (C) 2026 Korytov Pavel

;; Author: Korytov Pavel <thexcloud@gmail.com>
;; Maintainer: Korytov Pavel <thexcloud@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29") (uuidgen "1.3") (libmpdel "2.0") (pcsv "1.4.0") (request "0.3.2") (validate "1.0.4") (magit-section "4.3.6") (org "9.6.6") (llm "0.29.0") (ct "0.3"))
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
(require 'deterred-chains)
(require 'deterred-faces)
(require 'deterred-format)
(require 'deterred-grid)
(require 'deterred-intervals)
(require 'deterred-utils)

;; Core logic
(require 'deterred-source)
(require 'deterred-db)
(require 'deterred-backup)
(require 'deterred-dashboard)
(require 'deterred-dispatcher)
(require 'deterred-source)

;; Sources
(require 'deterred-activitywatch)
(require 'deterred-ai)
(require 'deterred-digikam)
(require 'deterred-fit)
(require 'deterred-habits)
(require 'deterred-hledger)
(require 'deterred-hledger-exchange)
(require 'deterred-locations)
(require 'deterred-messengers-chains)
(require 'deterred-messengers)
(require 'deterred-mpd)
(require 'deterred-mpd-emms)
(require 'deterred-org-clock)
(require 'deterred-org-journal-tags)
(require 'deterred-org-roam)
(require 'deterred-podcasts)
(require 'deterred-read-it-later)
(require 'deterred-social)
(require 'deterred-transport)
(require 'deterred-wakatime-dired)
(require 'deterred-wakatime)

;; Dashboards
(require 'deterred-dashboard-activitywatch)
(require 'deterred-dashboard-ai)
(require 'deterred-dashboard-digikam)
(require 'deterred-dashboard-dummy)
(require 'deterred-dashboard-fit)
(require 'deterred-dashboard-hledger)
(require 'deterred-dashboard-messengers)
(require 'deterred-dashboard-mpd)
(require 'deterred-dashboard-org-journal-tags)
(require 'deterred-dashboard-org-roam)
(require 'deterred-dashboard-podcasts)
(require 'deterred-dashboard-read-it-later)
(require 'deterred-dashboard-transport)
(require 'deterred-dashboard-wakatime)

(setq deterred-dashboards
      (list
       ;; (deterred-dashboard-dummy)
       (deterred-dashboard-activitywatch)
       (deterred-dashboard-ai)
       (deterred-dashboard-digikam)
       (deterred-dashboard-fit)
       (deterred-dashboard-hledger)
       (deterred-dashboard-messengers)
       (deterred-dashboard-mpd)
       (deterred-dashboard-org-journal-tags)
       (deterred-dashboard-org-roam)
       (deterred-dashboard-podcasts)
       (deterred-dashboard-read-it-later)
       (deterred-dashboard-transport)
       (deterred-dashboard-wakatime)))

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

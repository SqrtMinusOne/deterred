;;; deterred-dashboard-dummy.el --- TODO -*- lexical-binding: t -*-

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
(require 'deterred-dashboard)

(defclass deterred-dashboard-dummy (deterred-dashboard)
  ((name :initform "Dummy")))

(cl-defmethod deterred-dashboard-list-datasets ((_dashboard deterred-dashboard-dummy))
  '((foo (name . "Foo")
         (tags tag1 tag2))
    (bar (name . "Bar")
         (tags tab1 tag3))))

(cl-defmethod deterred-dashboard-default-params ((_dashboard deterred-dashboard-dummy))
  '((:a-range . 10)
    (:a-count . 100)
    (:b-range . 10)))

(cl-defmethod deterred-dashboard-render-params ((_dashboard deterred-dashboard-dummy))
  (deterred-dashboard-widget-number
   :name "A range"
   :key :a-range)
  (insert "\n")
  (deterred-dashboard-widget-number
   :name "A count"
   :key :a-count)
  (insert "\n")
  (deterred-dashboard-widget-number
   :name "B range"
   :key :b-range)
  (insert "\n\n"))

(cl-defmethod deterred-dashboard-fetch-datasets ((_dashboard deterred-dashboard-dummy)
                                                 params)
  `((foo . ,(cl-loop for i from 0 to (alist-get :a-count params)
                     collect `((a . ,i)
                               (b . ,(+ i (random (alist-get :a-range params)))))))
    (bar . ,(cl-loop for i from 0 to 10
                     collect (list (cons 'baz (random (alist-get :b-range params))))))))

(cl-defmethod deterred-dashboard-render-results ((_dashboard deterred-dashboard-dummy)
                                                 data)
  (deterred-dashboard-exec-python
   :python-code
   "oh no")
  (deterred-dashboard-exec-python
   :python-code
   "import json

data = json.loads(input())
print(json.dumps({'data': data['bar']['data']}))
"
   :input data
   :on-success (lambda (data)
                 (insert (prin1-to-string data)))))

(provide 'deterred-dashboard-dummy)

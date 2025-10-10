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
    (:b-range . 10)
    (:a-date . 1760114739)))

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
  (insert "\n")
  (deterred-dashboard-widget-date
   :name "A date"
   :key :a-date
   :display-date t)
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
  (insert (deterred-format (f-h2 "Example error") "\n"))
  (deterred-dashboard-exec-python
   :python-code
   "oh no")
  (insert (deterred-format (f-h2 "Example image") "\n"))
  (deterred-dashboard-exec-python
   :python-code
   "from matplotlib import pyplot as plt
import pandas as pd

import json
import os
import base64
import io

data = json.loads(input())
df_foo = pd.DataFrame(data['foo']['data'])

fig, ax = plt.subplots(figsize=(8, 5))
df_foo.plot(ax=ax, kind='scatter', x='a', y='b')
ax.set_title('Foo plot')

buf = io.BytesIO()
plt.tight_layout()
plt.savefig(buf, format='png')
img = base64.b64encode(buf.getvalue()).decode()
print(json.dumps([img]))"
   :input data
   :on-success #'deterred-dashboard-print-images-base64)
  (insert "\n")
  (insert (deterred-format (f-h2 "Example table") "\n")
          (deterred-grid-print-with-org (alist-get 'data (alist-get 'foo data))
                                        :max-rows 10
                                        :grid-button t)))

(provide 'deterred-dashboard-dummy)

#|

This file is a part of DATALOADER project.
Copyright (c) 2019 Masataro Asai (guicho2.71828@gmail.com)
Copyright (c) 2019 IBM Corporation
SPDX-License-Identifier: LGPL-3.0-or-later

DATALOADER is free software: you can redistribute it and/or modify it under the terms
of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any
later version.

DATALOADER is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
DATALOADER.  If not, see <http://www.gnu.org/licenses/>.

|#

(in-package :cl-user)
(defpackage :dataloader.test
  (:use :cl
        :fiveam :iterate :alexandria :trivia))
(in-package :dataloader.test)



(def-suite :dataloader)
(in-suite :dataloader)

;; run test with (run! test-name) 

(defun rel (name &optional (dir #p"t/"))
  (asdf:system-relative-pathname :dataloader (merge-pathnames name dir)))

(defun test-load-save (name name2)
  (finishes
    (dataloader:load (rel name)))
  (finishes
    (dataloader:save (dataloader:load (rel name)) (rel name2)))
  (finishes
    (dataloader:load (rel name2))))

(test csv
  (finishes
    (dataloader:save (dataloader:load (rel "aaa.csv") :mime "text/csv" :data-map-fn #'read-from-string) (rel "aaa.tsv")))
  (finishes
    (dataloader:save (dataloader:load (rel "bbb.tsv") :mime "text/plain" :data-map-fn #'read-from-string) (rel "bbb.csv")))

  (let (loaded)
    (finishes
      (setf loaded (dataloader:load (rel "strings.csv") :mime "text/csv" :type 'string))
      (print loaded))
    (is-true (typep (aref loaded 0 0) 'string))
    (dataloader:save loaded (rel "strings.tsv"))))

(test png
  (test-load-save "lenna.png" "lenna-png.png"))

(test jpg
  (test-load-save "lenna.jpg" "lenna-jpg.jpg"))

(test tiff
  (test-load-save "lenna.tiff" "lenna-tiff.tiff"))

(test conversion
  (test-load-save "lenna.png" "lenna-png.tiff")
  (test-load-save "lenna.png" "lenna-png.jpg")
  (test-load-save "lenna.jpg" "lenna-png.png")
  (test-load-save "lenna.jpg" "lenna-png.tiff")
  (test-load-save "lenna.tiff" "lenna-png.png")
  (test-load-save "lenna.tiff" "lenna-png.jpg"))

(test wav
  (test-load-save "sine.wav" "sine-wav.wav"))

(test npy
      (test-load-save "arange-5.npy" "arange-5-npy.npy")
      (test-load-save "arange-5.0.npy" "arange-5.0-npy.npy"))


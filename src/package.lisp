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
(uiop:define-package dataloader
  (:use)
  (:mix :numcl :iterate :trivia)
  (:shadow :load)
  (:export #:load
           #:save))
(in-package :dataloader)

;; blah blah blah.

(defun external-format (file)
  (intern (string-upcase (magicffi:pathname-mime-encoding file)) :keyword))

(defun load (file &rest args
             &key (mime (magicffi:pathname-mime-type file))
             ;; asarray
               type
               ;; csv args
               csv-reader row-fn map-fn data-map-fn sample skip-first-p 
               (separator                    cl-csv:*separator*)
               (quote                        cl-csv:*quote*)
               (escape                       cl-csv:*quote-escape*)
               unquoted-empty-string-is-nil
               quoted-empty-string-is-nil
               trim-outer-whitespace
               newline
               escape-mode
               ;; wave args
               chunk-data-reader
             &allow-other-keys)

  (declare (ignore type
                   csv-reader row-fn map-fn data-map-fn sample skip-first-p
                   separator quote escape unquoted-empty-string-is-nil
                   quoted-empty-string-is-nil trim-outer-whitespace newline
                   escape-mode chunk-data-reader))
  
  (ematch mime
    ("image/png"
     (with-open-file (s file :direction :input :element-type '(unsigned-byte 8))
       (png:decode s)))
    ("image/jpeg"
     (multiple-value-bind (array height width) (cl-jpeg:decode-image file)
       (make-array (list height width 3)
                   :element-type '(unsigned-byte 8)
                   :displaced-to array)))
    ("image/tiff"
     (match (retrospectiff:read-tiff-file file)
       ((retrospectiff:tiff-image :data data :length h :width w)
        ;; (make-array (list h w 3)
        ;;             :element-type (array-element-type data)
        ;;             :displaced-to data)
        (let ((a (make-array (list h w 3) :element-type '(unsigned-byte 8))))
          (dotimes (i (* h w 3))
            (setf (row-major-aref a i) (row-major-aref data i)))
          a))))
    ((or "application/csv"
         "application/x-csv"
         "application/vnd.ms-excel"
         "text/csv"
         "text/comma-separated-values"
         "text/x-csv"
         "text/x-comma-separated-values"
         "text/tab-separated-values"
         "text/plain")
     (handler-case
         (with-open-file (s file
                            :direction :input
                            :external-format (external-format file))
           (apply #'asarray
                  (apply #'cl-csv:read-csv s :allow-other-keys t args)
                  :allow-other-keys t args))
       (reader-error ()
         (with-open-file (s file
                            :direction :input)
           (apply #'asarray
                  (apply #'cl-csv:read-csv s :allow-other-keys t args)
                  :allow-other-keys t args)))))
    ((or "audio/wav"
         "audio/x-wav"
         "audio/wave"
         "audio/x-wave"
         "audio/vnd.wave")
     ;; decided not to strip out the original metadata
     #+(or)
     (let* ((result (apply #'wav:read-wav-file file :allow-other-keys t args))
            (array  (getf
                     (find "data" result
                           :key  (lambda (plist) (getf plist :chunk-id))
                           :test 'string=)
                     :chunk-data)))
       (make-array (length array)
                   :element-type (array-element-type array)
                   :displaced-to array))
     (apply #'wav:read-wav-file file :allow-other-keys t args))
    #+(or)
    ("audio/flac"
     (let ((decoder (flac:flac-open file)))
       (unwind-protect
            (progn
              
              )
         (flac:flac-close decoder))))
    #+(or)
    ("image/x-ms-bmp"
     (png::decode-file file))
    #+(or)
    ("image/gif"
     (retrospectiff:read-tiff-file file))))

(defun save (array file &key (mime (pathname-type file)))
  (ematch* (array mime)
    ((_ "png")
     (with-open-file (s file :direction :output :if-does-not-exist :create :if-exists :supersede :element-type '(unsigned-byte 8))
       (png:encode array s)))
    (((array :dimensions (list h w _))
      (or "jpg" "jpeg"))
     (cl-jpeg:encode-image file (array-displacement array) 1 h w))
    (((array :dimensions (list h w _))
      "tiff")
     (let ((tiff (make-instance 'retrospectiff:tiff-image
                                :length h
                                :width w
                                :bits-per-sample 8
                                :samples-per-pixel 1
                                :data (array-displacement array)
                                :byte-order tiff::*byte-order*
                                :min-is-white t)))
       (retrospectiff:write-tiff-file file tiff :if-exists :supersede)))
    (((array :dimensions (list h w)) "csv")
     (with-open-file (s file :direction :output :if-does-not-exist :create :if-exists :supersede)
       (iter (for i below h)
             (iter (for j below w)
                   (unless (first-iteration-p)
                     (write-char #\, s))
                   (prin1 (aref array i j) s))
             (terpri s))))
    (((array :dimensions (list h w)) "tsv")
     (with-open-file (s file :direction :output :if-does-not-exist :create :if-exists :supersede)
       (iter (for i below h)
             (iter (for j below w)
                   (unless (first-iteration-p)
                     (write-char #\Tab s))
                   (prin1 (aref array i j) s))
             (terpri s))))
    ((_ "wav")
     #+(or)
     (wav:write-wav-file (list (list :chunk-id "RIFF"
                                     :chunk-data-size 176436
                                     :file-type "WAVE")
                               (list :chunk-id "fmt "
                                     :chunk-data-size 16
                                     :chunk-data
                                     (list :compression-code 1
                                           :number-of-channels 1
                                           :sample-rate 44100
                                           :average-bytes-per-second 88200
                                           :block-align 2
                                           :significant-bits-per-sample 16))
                               (list :chunk-id "data"
                                     :chunk-data-size (length array)
                                     :chunk-data array))
                         file)
     (wav:write-wav-file array file))
    #+(or)
    ("audio/flac"
     (let ((decoder (flac:flac-open file)))
       (unwind-protect
            (progn
              
              )
         (flac:flac-close decoder))))
    #+(or)
    ("image/x-ms-bmp"
     (png::decode-file file))
    #+(or)
    ("image/gif"
     (retrospectiff:read-tiff-file file))))

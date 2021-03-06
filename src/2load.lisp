
(in-package :dataloader)

(define-load-method (file "image/png")
  (with-open-file (s file :direction :input :element-type '(unsigned-byte 8))
    (let ((data (png:decode s)))
      ;; this is a multidimensional simple array. Lets convert it to numcl array
      (multiple-value-bind (array base-array) (numcl:empty-like data)
        ;; base-array is a simple array
        (dotimes (i (array-total-size data))
          (setf (row-major-aref base-array i)
                (row-major-aref data i)))
        array))))

(define-load-method (file "image/jpeg" &rest args &key buffer (colorspace-conversion t) cached-source-p (decode-frame t))
  (multiple-value-bind (array height width components transform) (apply #'cl-jpeg:decode-image file :allow-other-keys t args)
    (declare (ignorable transform))
    ;; cl-jpeg supports sequential only; no progressive. Components mean color dimension.
    (make-array (list height width components)
                :element-type '(unsigned-byte 8)
                :displaced-to array)))

(define-load-method (file "image/tiff")
  (match (retrospectiff:read-tiff-file file)
    ((retrospectiff:tiff-image :data data
                               :length h
                               :width w
                               :samples-per-pixel c
                               :bits-per-sample n)
     ;; the biggest flaw of retrospectiff is that the data array is untyped.
     ;; let's convert it into a typed numcl array.

     ;; n could be an integer or a sequence of integers.
     (when (typep n 'sequence)
       (assert (= (reduce #'max n) (reduce #'min n))) ; length of n is below 4 (channel size), so this is safe.
       (setf n (reduce #'max n)))
     (multiple-value-bind (array base-array) (numcl:empty (list h w c) :type `(unsigned-byte ,n))
       ;; base-array is a simple array
       (dotimes (i (length base-array))
         (setf (row-major-aref base-array i)
               (row-major-aref data i)))
       array))))

(define-load-method (file ("application/csv"
                        "application/x-csv"
                        "application/vnd.ms-excel"
                        "text/csv"
                        "text/comma-separated-values"
                        "text/x-csv"
                        "text/x-comma-separated-values"
                        "text/tab-separated-values"
                        "text/plain")
                       &rest args
                       &key
                       type             ;for asarray
                       csv-reader row-fn map-fn data-map-fn sample skip-first-p 
                       (separator                    cl-csv:*separator*)
                       (quote                        cl-csv:*quote*)
                       (escape                       cl-csv:*quote-escape*)
                       unquoted-empty-string-is-nil
                       quoted-empty-string-is-nil
                       trim-outer-whitespace
                       newline
                       escape-mode)
  (handler-case
      (with-open-file (s file
                         :direction :input
                         :external-format (external-format file))
        (apply #'numcl:asarray
               (apply #'cl-csv:read-csv s :allow-other-keys t args)
               :allow-other-keys t args))
    (reader-error ()
      (with-open-file (s file
                         :direction :input)
        (apply #'numcl:asarray
               (apply #'cl-csv:read-csv s :allow-other-keys t args)
               :allow-other-keys t args)))))


(defun find-chunk (id riff)
  (find-if (lambda (plist) (string= id (getf plist :chunk-id)))
           riff))

(define-load-method (file ("audio/wav"
                        "audio/x-wav"
                        "audio/wave"
                        "audio/x-wave"
                        "audio/vnd.wave"))
  (let* ((riff (wav:read-wav-file file))
         (fmt  (getf (find-chunk "fmt " riff) :chunk-data))
         (data-ck (find-chunk "data" riff))
         (data (getf data-ck :chunk-data))
         (channels (getf fmt :number-of-channels))
         (bytes-per-sample (getf fmt :significant-bits-per-sample)))
    (assert (= 1 (getf fmt :compression-code)) nil "Compression format other than WAVE_FORMAT_PCM(0x0001) is not supported. See Multimedia Programming Interface and Data Specifications 1.0. https://www.aelius.com/njh/wavemetatools/doc/riffmci.pdf")
    (ematch* (channels bytes-per-sample data)
      ((1 8 (array :total-size size))
       (values
        (make-array size :element-type '(unsigned-byte 8) :displaced-to data)
        fmt))
      ((2 8 (array :total-size size))
       (assert (evenp size))
       (values
        (make-array (list (/ size 2) 2) :element-type '(unsigned-byte 8) :displaced-to data)
        fmt))
      ((1 16 (array :total-size size))
       (let ((data2 (make-array (/ size 2) :element-type '(unsigned-byte 16))))
         (dotimes (i (/ size 2))
           (setf (aref data2 i)
                 (+ (aref data (* 2 i))
                    (ash (aref data (1+ (* 2 i))) 8))))
         (values
          (make-array (/ size 2) :element-type '(unsigned-byte 16) :displaced-to data2)
          fmt)))
      ((2 16 (array :total-size size))
       (let ((data2 (make-array (/ size 2) :element-type '(unsigned-byte 16))))
         (dotimes (i (/ size 2))
           (setf (aref data2 i)
                 (+ (aref data (* 2 i))
                    (ash (aref data (1+ (* 2 i))) 8))))
         (values
          (make-array (list (/ size 4) 2) :element-type '(unsigned-byte 16) :displaced-to data2)
          fmt))))))


#+(or)
(define-load-method (file ("audio/flac"))
  (let ((decoder (flac:flac-open file)))
    (unwind-protect
         (progn
           
           )
      (flac:flac-close decoder))))

#+(or)
(define-load-method (file ("image/x-ms-bmp"))
  (png::decode-file file))

#+(or)
(define-load-method (file ("image/gif"))
  (retrospectiff:read-tiff-file file))

#+(or)
(define-load-method (file ())
  )

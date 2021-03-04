
(in-package :dataloader)

(def-load-method (file "image/png" &rest args)
  (with-open-file (s file :direction :input :element-type '(unsigned-byte 8))
    (png:decode s)))

(def-load-method (file "image/jpeg" &rest args &key buffer (colorspace-conversion t) cached-source-p (decode-frame t))
  (multiple-value-bind (array height width) (apply #'cl-jpeg:decode-image file :allow-other-keys t args)
    (make-array (list height width 3)
                :element-type '(unsigned-byte 8)
                :displaced-to array)))

(def-load-method (file "image/tiff" &rest args)
  (match (retrospectiff:read-tiff-file file)
    ((retrospectiff:tiff-image :data data :length h :width w)
     (let ((a (make-array (list h w 3) :element-type '(unsigned-byte 8))))
       (dotimes (i (* h w 3))
         (setf (row-major-aref a i) (row-major-aref data i)))
       a))))

(def-load-method (file ("application/csv"
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
        (apply #'asarray
               (apply #'cl-csv:read-csv s :allow-other-keys t args)
               :allow-other-keys t args))
    (reader-error ()
      (with-open-file (s file
                         :direction :input)
        (apply #'asarray
               (apply #'cl-csv:read-csv s :allow-other-keys t args)
               :allow-other-keys t args)))))

(def-load-method (file ("audio/wav"
                        "audio/x-wav"
                        "audio/wave"
                        "audio/x-wave"
                        "audio/vnd.wave"))
  (wav:read-wav-file file))

#+(or)
(def-load-method (file ("audio/flac"))
  (let ((decoder (flac:flac-open file)))
    (unwind-protect
         (progn
           
           )
      (flac:flac-close decoder))))

#+(or)
(def-load-method (file ("image/x-ms-bmp"))
  (png::decode-file file))

#+(or)
(def-load-method (file ("image/gif"))
  (retrospectiff:read-tiff-file file))

#+(or)
(def-load-method (file ())
  )

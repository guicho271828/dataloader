
(in-package :dataloader)

(def-save-method (array file ("png"))
  (with-open-file (s file :direction :output :if-does-not-exist :create :if-exists :supersede :element-type '(unsigned-byte 8))
    (png:encode (numcl:to-simple-array array) s)))

(def-save-method (array file ("jpg" "jpeg"))
  (ematch array
    ((array :dimensions (list h w c) :displaced-to d)
     (cl-jpeg:encode-image file d c h w))))

(def-save-method (array file ("tiff"))
  (ematch array
    ((array :dimensions (list h w c))
     (let ((tiff (make-instance 'retrospectiff:tiff-image
                                :length h
                                :width w
                                :bits-per-sample 8
                                :samples-per-pixel c
                                :data (array-displacement array)
                                :byte-order tiff::*byte-order*
                                :min-is-white t)))
       (with-open-file (s file :direction :output :if-does-not-exist :create :if-exists :supersede :element-type '(unsigned-byte 8))
         (retrospectiff:write-tiff-stream s tiff))))))

(def-save-method (array file ("csv"))
  (ematch array
    ((array :dimensions (list h w))
     (with-open-file (s file :direction :output :if-does-not-exist :create :if-exists :supersede)
       (iter (for i below h)
             (iter (for j below w)
                   (unless (first-iteration-p)
                     (write-char #\, s))
                   (prin1 (aref array i j) s))
             (terpri s))))))

(def-save-method (array file ("tsv"))
  (ematch array
    ((array :dimensions (list h w))
     (with-open-file (s file :direction :output :if-does-not-exist :create :if-exists :supersede)
       (iter (for i below h)
             (iter (for j below w)
                   (unless (first-iteration-p)
                     (write-char #\Tab s))
                   (prin1 (aref array i j) s))
             (terpri s))))))


;; Using riff and wav requires reading
;; Multimedia Programming Interface and Data Specifications 1.0.

(def-save-method (array file ("wav")
                        &key
                        (bits-per-second 44100))
  (flet ((writer (bits-per-sample data-size data)
           (with-open-file (stream file
                                   :direction :output
                                   :element-type '(unsigned-byte 8)
                                   :if-does-not-exist :create
                                   :if-exists :supersede)
             ;; note: fourcc (four character code) == dword
             (riff:write-riff-chunks
              (list (list :chunk-id "RIFF"
                          :chunk-data-size (+ 2
                                              (+ 2 2 16)
                                              (+ 2 2 data-size))
                          ;; 2
                          :file-type "WAVE")
                    ;; 2 + 2 + 16
                    (list :chunk-id "fmt " ;2
                          :chunk-data-size 16 ;2
                          :chunk-data
                          ;; 16 bytes = 2+2+4+4+2+2
                          (list :compression-code 1 ;word --- actually, wFormatTag in IBM/MS spec
                                :number-of-channels 1 ;word
                                :sample-rate bits-per-second    ;dword
                                :average-bytes-per-second (* 2 bits-per-second (/ bits-per-sample 8)) ;dword
                                ;;word
                                :block-align (* 2 (/ bits-per-sample 8))
                                ;;word
                                :significant-bits-per-sample bits-per-sample))
                    ;; 2 + 2 + length
                    (list :chunk-id "data" ;2
                          :chunk-data-size data-size ;2
                          :chunk-data data))
              stream :chunk-data-writer (wav::wrap-format-chunk-data-writer)))))
    (ematch array
      ((array :dimensions (list size)
              :element-type (list 'unsigned-byte 8))
       (writer 8 size array))
      
      ((array :dimensions (list size 2)
              :element-type (list 'unsigned-byte 8))
       (writer 8 (* 2 size) (numcl:flatten array)))

      ((array :dimensions (list size)
              :element-type (list 'unsigned-byte 16))
       (let ((array2 (make-array (* size 2) :element-type '(unsigned-byte 8))))
         (dotimes (i size)
           (setf (aref array2 (* 2 i))      (logand #xFF (aref array i))
                 (aref array2 (1+ (* 2 i))) (ash (aref array i) -8)))
         (writer 16 (* 2 size) array2)))

      ((array :dimensions (list size 2)
              :element-type (list 'unsigned-byte 16))
       (let ((array2 (make-array (* size 4) :element-type '(unsigned-byte 8))))
         (dotimes (i size)
           (setf (aref array2 (+ 0 (* 4 i))) (logand #xFF (aref array i 0))
                 (aref array2 (+ 1 (* 4 i))) (ash (aref array i 0) -8)
                 (aref array2 (+ 2 (* 4 i))) (logand #xFF (aref array i 1))
                 (aref array2 (+ 3 (* 4 i))) (ash (aref array i 1) -8)))
         (writer 16 (* 4 size) array2))))))

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
 (retrospectiff:read-tiff-file file))

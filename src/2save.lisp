
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


(def-save-method (array file ("wav"))
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
 (retrospectiff:read-tiff-file file))

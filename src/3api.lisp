
(in-package :dataloader)

(defun load (file &rest args
             &key (mime (magicffi:pathname-mime-type file))
               . #.*load-keyword-arg-list*)
  "Loads a file and returns a numcl-compatible array. The file format is detected by libmagic, but it can optionally be specified."
  (declare (ignore . #.(mapcar #'car (mapcar #'ensure-list *load-keyword-arg-list*))))
  (apply #'perform-load file mime args))


(defun save (array file &rest args
             &key (mime (pathname-type file))
               . #.*save-keyword-arg-list*)
  "This function saves the array into a file, using the format specified by the file name extension or by the mime type."
  (declare (ignore . #.(mapcar #'car (mapcar #'ensure-list *save-keyword-arg-list*))))
  (apply #'perform-save array file mime args))



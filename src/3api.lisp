
(in-package :dataloader)

(defun load (file &rest args
             &key (mime (magicffi:pathname-mime-type file))
               . #.*load-keyword-arg-list*)
  (declare (ignore . #.(mapcar #'car (mapcar #'ensure-list *load-keyword-arg-list*))))
  (apply #'%load file mime args))


(defun save (array file &rest args
             &key (mime (pathname-type file))
               . #.*save-keyword-arg-list*)
  (declare (ignore . #.(mapcar #'car (mapcar #'ensure-list *save-keyword-arg-list*))))
  (apply #'%save array file mime args))



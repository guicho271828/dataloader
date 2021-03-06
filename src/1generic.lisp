
(in-package :dataloader)

(defun external-format (file)
  (intern (string-upcase (magicffi:pathname-mime-encoding file)) :keyword))

(defun intern-mime (mime)
  (let ((s (intern (string-upcase mime) :dataloader.mime)))
    (export s :dataloader.mime)
    s))


(defparameter *load-keyword-arg-list* nil)

(defgeneric perform-load (file mime &rest args &key &allow-other-keys)
  (:documentation "A generic function that is defined for each mime type symbol in `DATALOADER.MIME` package."))

(defmethod perform-load ((file string) mime &rest args &key &allow-other-keys)
  "Convert a namestring into a pathanme"
  (apply #'perform-load (pathname file) mime args))

(defmethod perform-load (file (mime string) &rest args &key &allow-other-keys)
  "Convert a mime string into a symbol"
  (apply #'perform-load file (intern-mime mime) args))

(defmacro define-load-method ((filevar mime-string-or-strings &rest lambda-list) &body body)
  "A wrapper over `defmethod`. Made primarily because defmethod does not support `(or (eql ...))` or `member` as a specializer.
`mime-string-or-strings` is a string or a list of strings, where each string specifies a mime type e.g. \"image/png\".
`lambda-list` contains a keyword argument portion of the congruent lambda list of `defgeneric`.
In order to have a compatible lambda-list with the generic function,
it adds `&rest` argument if not present,
and `&allow-other-keys` when `&key` is present and `&allow-other-keys` is missing."
  ;; 
  (when (not (member '&rest lambda-list))
    (with-gensyms (args)
      (setf lambda-list `(&rest ,args ,@lambda-list))))
  (when (and (member '&key lambda-list)
             (not (member '&allow-other-keys lambda-list)))
    (setf lambda-list `(,@lambda-list &allow-other-keys)))
  `(progn
     ,@(when-let ((keyword-arg-list (member '&key lambda-list)))
         `((add-to-load-keyword-arg-list ',(set-difference keyword-arg-list lambda-list-keywords))))
     ,@(iter (for mime-string in (ensure-list mime-string-or-strings))
             (check-type mime-string string)
             (collecting
              `(defmethod perform-load (,filevar (mime (eql ',(intern-mime mime-string))) ,@lambda-list)
                 ,@(when-let ((vars (mapcar #'car (mapcar #'ensure-list (set-difference lambda-list lambda-list-keywords)))))
                     `((declare (ignorable ,@vars))))
                 ,@body)))))

(defun add-to-load-keyword-arg-list (keyword-arg-list)
  (appendf *load-keyword-arg-list*
           keyword-arg-list))


(defparameter *save-keyword-arg-list* nil)

(defgeneric perform-save (array file mime &rest args &key &allow-other-keys)
  (:documentation "A generic function that is defined for each mime type symbol in `DATALOADER.MIME` package."))

(defmethod perform-save (array (file string) mime &rest args &key &allow-other-keys)
  "Convert a namestring into a pathanme"
  (apply #'perform-save array (pathname file) mime args))

(defmethod perform-save (array file (mime string) &rest args &key &allow-other-keys)
  "Convert a mime string into a symbol"
  (apply #'perform-save array file (intern-mime mime) args))

(defmacro define-save-method ((arrayvar filevar mime-string-or-strings &rest lambda-list) &body body)
  "A wrapper over `defmethod`. Made primarily because defmethod does not support `(or (eql ...))` or `member` as a specializer.
`mime-string-or-strings` is a string or a list of strings, where each string specifies a mime type e.g. \"image/png\".
`lambda-list` contains a keyword argument portion of the congruent lambda list of `defgeneric`.
In order to have a compatible lambda-list with the generic function,
it adds `&rest` argument if not present,
and `&allow-other-keys` when `&key` is present and `&allow-other-keys` is missing."
  ;; 
  (when (not (member '&rest lambda-list))
    (with-gensyms (args)
      (setf lambda-list `(&rest ,args ,@lambda-list))))
  (when (and (member '&key lambda-list)
             (not (member '&allow-other-keys lambda-list)))
    (setf lambda-list `(,@lambda-list &allow-other-keys)))
  `(progn
     ,@(when-let ((keyword-arg-list (member '&key lambda-list)))
         `((add-to-save-keyword-arg-list ',(set-difference keyword-arg-list lambda-list-keywords))))
     ,@(iter (for mime-string in (ensure-list mime-string-or-strings))
             (check-type mime-string string)
             (collecting
              `(defmethod perform-save (,arrayvar ,filevar (mime (eql ',(intern-mime mime-string))) ,@lambda-list)
                 ,@(when-let ((vars (mapcar #'car (mapcar #'ensure-list (set-difference lambda-list lambda-list-keywords)))))
                     `((declare (ignorable ,@vars))))
                 ,@body)))))

(defun add-to-save-keyword-arg-list (keyword-arg-list)
  (appendf *save-keyword-arg-list*
           keyword-arg-list))

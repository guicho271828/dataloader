
(defsystem dataloader
  :version "0.1"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :license "LLGPL"
  :defsystem-depends-on ()
  :depends-on (:magicffi
               ;; :cl-wav :flac :mpg123 
               :png :cl-jpeg :retrospectiff
               :cl-csv
               :iterate :alexandria :trivia
               :numcl)
  :pathname "src"
  :components ((:file "package"))
  :description "A universal loader library for various data formats for images/audio"
  :in-order-to ((test-op (test-op :dataloader.test)))
  ;; :defsystem-depends-on (:trivial-package-manager)
  ;; :perform
  #+(or)
  (load-op :before (op c)
           (uiop:symbol-call :trivial-package-manager
                             :ensure-program
                             "minisat"
                             :apt "minisat"
                             :dnf "minisat2"
                             :yum "minisat2"
                             :packman ""
                             :yaourt ""
                             :brew "minisat"
                             :choco ""
                             :from-source (format nil "make -C ~a"
                                                  (asdf:system-source-directory :dataloader)))
           (uiop:symbol-call :trivial-package-manager
                             :ensure-library
                             "libfixposix"
                             :apt "libfixposix0"
                             :dnf ""
                             :yum ""
                             :packman ""
                             :yaourt ""
                             :brew "libfixposix"
                             :choco ""
                             :from-source (format nil "make -C ~a"
                                                  (asdf:system-source-directory :dataloader)))))

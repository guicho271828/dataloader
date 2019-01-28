#|
  This file is a part of dataloader project.
  Copyright (c) 2019 Masataro Asai (guicho2.71828@gmail.com)
|#


(defsystem dataloader.test
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :description "Test system of dataloader"
  :license "LLGPL"
  :depends-on (:dataloader
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "package"))))
  :perform (test-op :after (op c) (eval (read-from-string "(5am:run! :dataloader)"))
))

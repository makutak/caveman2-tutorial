(in-package :cl-user)
(defpackage caveman2-tutorial-test-asd
  (:use :cl :asdf))
(in-package :caveman2-tutorial-test-asd)

(defsystem caveman2-tutorial-test
  :author "Kouno"
  :license "MIT"
  :depends-on (:caveman2-tutorial
               :prove)
  :components ((:module "t"
                :components
                ((:file "caveman2-tutorial"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))

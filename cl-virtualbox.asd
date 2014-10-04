(in-package :cl-user)
(defpackage cl-virtualbox-asd
  (:use :cl :asdf))
(in-package :cl-virtualbox-asd)

(defsystem cl-virtualbox
  :version "0.1"
  :author "Fernando Borretti"
  :license "MIT"
  :depends-on (:cl-ppcre
               :uiop)
  :components ((:module "src"
                :components
                ((:file "cl-virtualbox"))))
  :description "Control VirtualBox from Common Lisp"
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op cl-virtualbox-test))))

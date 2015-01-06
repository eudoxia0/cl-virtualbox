(in-package :cl-user)
(defpackage cl-virtualbox-asd
  (:use :cl :asdf))
(in-package :cl-virtualbox-asd)

(defsystem cl-virtualbox
  :version "0.1"
  :author "Fernando Borretti"
  :license "MIT"
  :homepage "https://github.com/eudoxia0/cl-virtualbox"
  :depends-on (:cl-ppcre
               :uiop
               :alexandria
               :usocket)
  :components ((:module "src"
                :components
                ((:file "cl-virtualbox"))))
  :description "Control VirtualBox from Common Lisp"
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op cl-virtualbox-test))))

(in-package common-lisp-user)

(defpackage #:metabang-bind-test-system
  (:use #:common-lisp #:asdf))
(in-package #:metabang-bind-test-system)

(defsystem metabang-bind-test
  :version "0.1"
  :author "Gary Warren King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style License"
  :description "Tests for metabang-bind"
  :components ((:module "setup"
			:pathname "unit-tests/"
                        :components ((:file "package")
                                     (:file "test-bind" 
                                            :depends-on ("package"))))
               (:module "tests"
			:pathname "unit-tests/"
			:depends-on ("setup")
                        :components ((:file "structures")
				     (:file "classes")
				     (:file "plists")
				     (:file "arrays"))))
  :depends-on (:metabang-bind :lift))

#+asdf-system-connections 
(asdf:defsystem-connection bind-and-cl-ppcre-test
  :requires (metabang-bind-test cl-ppcre)
  :components ((:module
		"bind-and-cl-ppcre"
		:pathname "unit-tests/"
		:components ((:file "regex")))))



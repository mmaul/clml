;; -*- mode: lisp; syntax: common-lisp -*-

(in-package :cl-user)

(defpackage :statistics-asd
  (:use :cl :asdf))

(in-package :statistics-asd)

(asdf:defsystem :statistics
  :author "Peter Salvi / MSI"
  :licence "?"
  :description "Statistics Library"
  :components ((:file "package")
	       (:file "utilities" :depends-on ("package"))
	       (:file "math" :depends-on ("utilities"))
	       (:file "statistics" :depends-on ("math"))))

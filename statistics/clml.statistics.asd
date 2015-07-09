;; -*- mode: lisp; syntax: common-lisp -*-
#+sbcl
(declaim (sb-ext:muffle-conditions sb-kernel:character-decoding-error-in-comment))

(defpackage :clml.statistics-environment (:use :cl :asdf))
(in-package :clml.statistics-environment)

(defun call-with-environment (fun)
  (let ((*read-default-float-format* 'double-float))
    (funcall fun)))


(asdf:defsystem :clml.statistics
  :author "Peter Salvi / MSI (statistics), Ryo Fujii (statistics & distribution), Yotsutsuji Tetsuaki (rand)"
  :licence "?"
  :description "Statistics Library"
  :depends-on (:clml.statistics.rand)
  :serial t
  :around-compile call-with-environment
  :components 
  ((:module "statistics" 
            :pathname "src/"
            :components
            ((:file "package")
             (:file "utilities" )
             (:file "math")
             (:file "statistics")
             (:file "distribution-initial")
             (:file "distribution" :depends-on ("utilities" "math"))
             (:file "distribution-test")
             (:file "histogram")
                                        ;(:file "distribution-test" :depends-on ("distribution"))
             )
            )
   
   
   (:static-file "README.md")
   (:static-file "README.jp")
   (:static-file "TODO")
   (:static-file "BUGS")
   ))


;; -*- mode: lisp; syntax: common-lisp -*-

(in-package :cl-user)

(defpackage :clml.statistics-asd
  (:use :cl :asdf))

(in-package :clml.statistics-asd)


(asdf:defsystem :clml.statistics
  :author "Peter Salvi / MSI (statistics), Ryo Fujii (statistics & distribution), Yotsutsuji Tetsuaki (rand)"
  :licence "?"
  :description "Statistics Library"
  :depends-on (:clml.statistics.rand)
  :serial t
  :components 
  ((:module "statistics" 
            :pathname "src/"
            :components
            ((:file "package")
             (:file "utilities" )
             (:file "math")
             (:file "statistics")
             (:file "distribution-initial")
             (:file "distribution-base")
             (:file "distribution-gamma")
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


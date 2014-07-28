;; -*- mode: lisp; syntax: common-lisp -*-

(in-package :cl-user)

(defpackage :clml.statistics-asd
  (:use :cl :asdf))

(in-package :clml.statistics-asd)

(defsystem :clml.statistics.rand
  :author "Yotsutsuji Tetsuaki"
  :licence "?"
  :description "Random Number Generator Library"
  :serial t
  :components 
  ((:module
    
    :clml.statistics.rand
            :pathname #p "src/rand"
            :components 
            ((:file "rand/package" )
             (:file "rand/utilities" :depends-on ("rand/package"))
             (:file "rand/rand" :depends-on ("rand/utilities"))
             )
            )
   ;(:static-file "src/rand/README.md")
   
   ))

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
             (:file "distribution" :depends-on ("utilities" "math"))
             (:file "histogram")
                                        ;(:file "distribution-test" :depends-on ("distribution"))
             )
            )
   
   
   (:static-file "README.md")
   (:static-file "README.jp")
   (:static-file "TODO")
   (:static-file "BUGS")
   ))


#+sbcl
(declaim (sb-ext:muffle-conditions sb-kernel:character-decoding-error-in-comment))

(defpackage :clml.statistics.rand-environment (:use :cl :asdf))
(in-package :clml.statistics.rand-environment)

(defun call-with-environment (fun)
  (let ((*read-default-float-format* 'double-float))
    (funcall fun)))

(defsystem :clml.statistics.rand
  :author "Yotsutsuji Tetsuaki"
  :licence "?"
  :description "Random Number Generator Library"
  :serial t
  :around-compile call-with-environment  
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

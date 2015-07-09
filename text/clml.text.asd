#+sbcl
(declaim (sb-ext:muffle-conditions sb-kernel:character-decoding-error-in-comment))

(defpackage :clml.text-environment (:use :cl :asdf))
(in-package :clml.text-environment)

(defun call-with-environment (fun)
  (let ((*read-default-float-format* 'double-float))
    (funcall fun)))

(asdf:defsystem :clml.text
                :pathname "src/"
                :serial t
                :around-compile call-with-environment  
                :depends-on (:clml.hjs
                             :split-sequence
                             :clml.nonparametric
                             )
                :components ((:file "package")
                             (:file "text-utils")
                             (:file "hdp-lda")
                             ))

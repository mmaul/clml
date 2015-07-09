#+sbcl
(declaim (sb-ext:muffle-conditions sb-kernel:character-decoding-error-in-comment))

(defpackage :clml.statistics.rand-environment (:use :cl :asdf))
(in-package :clml.statistics.rand-environment)

(defun call-with-environment (fun)
  (let ((*read-default-float-format* 'double-float))
    (funcall fun)))

(asdf:defsystem :clml.utility
                :pathname "src/"
                :serial t
                :around-compile call-with-environment  
                :depends-on (
                             :alexandria
                             :parse-number
                             :cl-ppcre
                             :iterate
                             :cl-fad
                             :drakma
                             :trivial-garbage
                             )
                :components (
                             (:file "package")
                             (:file "csv")
                             (:file "priority-que")
                             (:file "fetch")
                             )
                )

#+sbcl
(declaim (sb-ext:muffle-conditions sb-kernel:character-decoding-error-in-comment))

(defpackage :clml.numeric-environment (:use :common-lisp :asdf))
(in-package :clml.numeric-environment)

(defun call-with-environment (fun)
  (let ((*read-default-float-format* 'double-float))
    (funcall fun)))


(asdf:defsystem :clml.numeric
                :pathname "src/"
                :serial t
                :around-compile call-with-environment
                :depends-on (:clml.hjs
                             )
                :components ((:file "package")
                             (:file "fft")
                             ))

#+sbcl
(declaim (sb-ext:muffle-conditions sb-kernel:character-decoding-error-in-comment))

(defpackage :clml.hjs-environment (:use :common-lisp :asdf))
(in-package :clml.hjs-environment)

(defun call-with-environment (fun)
  (let ((*read-default-float-format* 'double-float))
    (funcall fun)))

(asdf:defsystem :clml.hjs
  :pathname "src/"
  :serial t
  :around-compile call-with-environment
  :depends-on (
               :alexandria
               :iterate
               :f2cl
               :clml.blas
               :clml.lapack
               :future
               :clml.statistics
               :clml.utility
               )
  :components ((:file "package")
               (:file "utils")
               (:file "vars")
               (:file "vector")
               (:file "missing-value")
               (:file "matrix")
               (:file "read-data")
               (:file "eigenproblem")
               (:file "k-means")
               ))

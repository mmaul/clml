#+sbcl
(declaim (sb-ext:muffle-conditions sb-kernel:character-decoding-error-in-comment))

(defpackage :clml.hjs-environment (:use :common-lisp :asdf))
(in-package :clml.hjs-environment)

(defun call-with-environment (fun)
  (let ((*read-default-float-format* 'double-float))
    (funcall fun)))

(asdf:defsystem :clml.hjs
  :description "CLML core"
  :author"
     Original Authors: (One or more of)
       Salvi Péter,
       Naganuma Shigeta,
       Tada Masashi,
       Abe Yusuke,
       Jianshi Huang,
       Fujii Ryo,
       Abe Seika,
       Kuroda Hisao
     Author Post MSI CLML Contribution:
       Mike Maul  <maul.mike@gmail.com>"
  :maintainer "Mike Maul  <maul.mike@gmail.com>"
  :license "LLGPL"
  :pathname "src/"
  :serial t
  :around-compile call-with-environment
  :depends-on (
               :alexandria
               :iterate
               :clml.blas
               :clml.lapack
               :future
               :clml.statistics
               :clml.utility
               :introspect-environment
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

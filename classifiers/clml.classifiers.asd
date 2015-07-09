#+sbcl
(declaim (sb-ext:muffle-conditions sb-kernel:character-decoding-error-in-comment))

(defpackage :clml.classifiers-environment (:use :common-lisp :asdf))
(in-package :clml.classifiers-environment)

(defun call-with-environment (fun)
  (let ((*read-default-float-format* 'double-float))
    (funcall fun)))

(asdf:defsystem :clml.classifiers
                :pathname "src/"
                :serial t
                :around-compile call-with-environment
                :depends-on (:clml.hjs
                             :clml.svm
                             :clml.clustering
                             )
                :components ((:file "package")
                             (:file "logistic")
                             (:file "linear-regression")
                             (:file "nbayes")
                             ))

#+sbcl
(declaim (sb-ext:muffle-conditions sb-kernel:character-decoding-error-in-comment))

(defpackage :clml.nearest-search-environment (:use :common-lisp :asdf))
(in-package :clml.nearest-search-environment)

(defun call-with-environment (fun)
  (let ((*read-default-float-format* 'double-float))
    (funcall fun)))

(asdf:defsystem :clml.nearest-search
                :pathname "src/"
                :serial t
                :around-compile call-with-environment
                :depends-on (
                             :clml.hjs
                             :clml.pca
                             :clml.nonparametric
                             )
                :components ((:file "package")
                             (:file "k-nn")
                             (:file "k-nn-new")
                             (:file "nearest-search")
                             ))

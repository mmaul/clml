#+sbcl
(declaim (sb-ext:muffle-conditions sb-kernel:character-decoding-error-in-comment))

(defpackage :clml.nearest-search-environment (:use :common-lisp :asdf))
(in-package :clml.nearest-search-environment)

(defun call-with-environment (fun)
  (let ((*read-default-float-format* 'double-float))
    (funcall fun)))



(asdf:defsystem :clml.decision-tree
                :pathname "src/"
                :serial t
                :around-compile call-with-environment
                :depends-on (:clml.hjs
                             :lparallel
                             )
                :components (
                             (:file "package")
                             (:file "decision-tree")
                             (:file "random-forest")
                             ))

#+sbcl
(declaim (sb-ext:muffle-conditions sb-kernel:character-decoding-error-in-comment))

(defpackage :clml.som-environment (:use :common-lisp :asdf))
(in-package :clml.som-environment)

(defun call-with-environment (fun)
  (let ((*read-default-float-format* 'double-float))
    (funcall fun)))


(asdf:defsystem :clml.som
                :pathname "src/"
                :serial t
                :around-compile call-with-environment
                :depends-on (:clml.hjs
                             :split-sequence
                             :clml.statistics
                             )
                :components ((:file "package")
                             (:file "param")
                             (:file "som_utils")
                             (:file "lvq_pak")
                             (:file "labels")
                             (:file "fileio")
                             (:file "datafile")
                             (:file "randinit")
                             (:file "som_rout")
                             (:file "som_pak")
                             (:file "vsom")
                             (:file "vcal")
                             (:file "sammon")
                             (:file "visual")
                             ))

(asdf:defsystem :clml.som.example
                :pathname "examples/"
                :serial t
                :around-compile call-with-environment
                :depends-on (:clml.hjs
                             :split-sequence
                             :clml.som
                             )
                
                :components (
                             (:file "package")
                             (:file "test")
                             ))


#+sbcl
(declaim (sb-ext:muffle-conditions sb-kernel:character-decoding-error-in-comment))

(defpackage :clml.graph-environment (:use :common-lisp :asdf))
(in-package :clml.graph-environment)

(defun call-with-environment (fun)
  (let ((*read-default-float-format* 'double-float))
    (funcall fun)))

(asdf:defsystem :clml.graph
                :pathname "src/"
                :serial t
                :around-compile call-with-environment
                :depends-on (:clml.hjs
                             :clml.time-series
                             :clml.statistics
                             :split-sequence
                             :cl-fad
                             )
                :components ((:file "package")
                             (:file "read-graph")
                             (:file "anomaly-detection")
                             (:file "centrality")
                             (:file "shortest-path")
                             (:file "utils")
))

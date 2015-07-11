#+sbcl
(declaim (sb-ext:muffle-conditions sb-kernel:character-decoding-error-in-comment))

(defpackage :clml.graph-environment (:use :common-lisp :asdf))
(in-package :clml.graph-environment)

(defun call-with-environment (fun)
  (let ((*read-default-float-format* 'double-float))
    (funcall fun)))

(asdf:defsystem :clml.graph
  :description "CLML graph library"
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

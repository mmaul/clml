#+sbcl
(declaim (sb-ext:muffle-conditions sb-kernel:character-decoding-error-in-comment))

(defpackage :clml.association-rule-environment (:use :cl :asdf))
(in-package :clml.association-rule-environment)

(defun call-with-environment (fun)
  (let ((*read-default-float-format* 'double-float))
    (funcall fun)))

(asdf:defsystem :clml.association-rule
    :description "Reorginized CLML (Common Lisp Machine Learming) library from MSI"
    :author"
     Original Authors:
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
                 )
    :components ((:file "package")
                 (:file "association-rule")
                 (:file "fp-growth")
                 (:file "assoc-da")
                 (:file "eclat")
                 (:file "lcm")
                 ))

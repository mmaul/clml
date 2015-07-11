#+sbcl
(declaim (sb-ext:muffle-conditions sb-kernel:character-decoding-error-in-comment))

(defpackage :clml.svm-asd (:use :cl :asdf))
(in-package :clml.svm-asd)

(defun call-with-environment (fun)
  (let ((*read-default-float-format* 'double-float))
    (funcall fun)))

(asdf:defsystem :clml.svm
  :description "CLML SVM Library"
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
               :clml.decision-tree
               :lparallel
               :future
               )
  :components (
               (:file "package")
               (:file "wss3-svm")
               (:file "svm")
               (:file "one-class-svm")
               (:file "pwss3-svm")
               (:file "smo-svm")
               (:file "svr")
               ))

(asdf:defsystem :clml.svm.examples
  :description "CLML SVM Examples"
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
  :pathname "examples/"
  :serial t
  :around-compile call-with-environment
  :depends-on (:clml.hjs
               :clml.svm
               )
  :components (
               (:file "svm-test")
               ))

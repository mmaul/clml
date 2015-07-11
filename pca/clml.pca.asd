#+sbcl
(declaim (sb-ext:muffle-conditions sb-kernel:character-decoding-error-in-comment))

(defpackage :clml.pca-environment (:use :common-lisp :asdf))
(in-package :clml.pca-environment)

(defun call-with-environment (fun)
  (let ((*read-default-float-format* 'double-float))
    (funcall fun)))


(asdf:defsystem :clml.pca
  :description "CLML Principal Component Analysis Library"
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
                             )
                :components ((:file "package")
                             (:file "pca")
                             (:file "face-recognition")
                             ))
(asdf:defsystem :clml.pca.examples
                :pathname "examples/"
                :serial t
                :around-compile call-with-environment
                :depends-on (:clml.hjs
                             :clml.pca
                             )
                :components (
                             (:file "package")
                             (:file "pca-bench")
                             
                             ))

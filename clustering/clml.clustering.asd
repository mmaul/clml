#+sbcl
(declaim (sb-ext:muffle-conditions sb-kernel:character-decoding-error-in-comment))

(defpackage :clml.nearest-search-environment (:use :common-lisp :asdf))
(in-package :clml.nearest-search-environment)

(defun call-with-environment (fun)
  (let ((*read-default-float-format* 'double-float))
    (funcall fun)))

(asdf:defsystem :clml.clustering
  :description "CLML Clustering Library"
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
  :defsystem-depends-on #+lispworks ("asdf-encodings") #-lispworks nil
  :around-compile call-with-environment
  :depends-on (:clml.hjs
               :clml.blas
               :iterate
               :clml.nearest-search
               )
  :components (
               (:file "package")
               (:file "hc")
               (:file "nmf")
               (:file "optics")
               (:file "spectral-clustering")
               (:file "cluster-validation")
               (:file "optics-speed")
               (:file "k-means2")
               ))

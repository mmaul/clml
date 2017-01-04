#+sbcl
(declaim (sb-ext:muffle-conditions sb-kernel:character-decoding-error-in-comment))

(defpackage :clml.test-environment (:use :common-lisp :asdf))
(in-package :clml.test-environment)

(defun call-with-environment (fun)
  (let ((*read-default-float-format* 'double-float))
    (funcall fun)))

(asdf:defsystem :clml.test
  :description "CLML Test Suite"
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
  :depends-on (:lisp-unit
               :clml)

  :components ((:file "package")
               (:file "test-groups")
               (:file "test-utils")
               (:file "test-stat")
               (:file "test-decision-tree")
               (:file "test-random-forest")
               (:file "test-hc")
               (:file "test-nmf")
               (:file "test-optics")
               (:file "test-cluster-validation")
               (:file "test-spectral-clustering")
               (:file "test-ts-anomaly-detection")
               (:file "test-ts-ar")
               (:file "test-ts-burst-detection")
               (:file "test-ts-read-data")
               (:file "test-ts-stat")
               (:file "test-ts-stsp")
               (:file "test-expl-smthing")
               (:file "test-svm")
               (:file "test-smo-svm")
               (:file "test-wss3-svm")
               (:file "test-pwss3-svm")
               (:file "test-one-class-svm")
               (:file "test-linear-regression")
               (:file "test-assoc")
               (:file "test-changefinder")
               (:file "test-classifier")
               (:file "test-dpm")
               (:file "test-hdp-lda")
               (:file "test-k-means")
               (:file "test-k-nn")
               (:file "test-matrix")
               (:file "test-missing-value")
               (:file "test-nbayes")
               (:file "test-pca")
               (:file "test-som")
               (:file "test-svr")
               (:file "test-text-utils")
               (:file "test-read-data")

               )
  )

(asdf:defsystem :clml.test-package
                :pathname "src/"
                :serial t
                :components (
                             (:file "package"))
                )


(asdf:defsystem :clml.test
                :pathname "src/"
                :serial t
                :depends-on (:lisp-unit
                             :clml.statistics
                             :hjs
                             :blas
                             :clml.decision-tree
                             :clml.clustering
                             :clml.time-series
                             :clml.svm
                             :clml.classifiers
                             :clml.association-rule
                             :clml.nonparametric
                             :clml.som
                             :clml.text
                             :clml.pca
                             :clml.nearest-search
                             :clml.test-package
                             )
                :components (
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
                             (:file "test-groups")
                             )
                )

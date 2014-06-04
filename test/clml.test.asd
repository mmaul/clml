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
                             :statistics
                             :hjs
                             :clml.decision-tree
                             :clml.clustering
                             :clml.time-series
                             :clml.svm
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
                             (:file "test-spectral-clustering")
                             (:file "test-cluster-validation")
                             (:file "test-ts-anomaly-detection")
                             (:file "test-changefinder")
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
                             (:file "test-groups")
                             )
                )

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
                             )
                )

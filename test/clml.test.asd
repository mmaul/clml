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
                             :clml.test-package
                             )
                :components (
                             (:file "test-decision-tree")
                             )
                )

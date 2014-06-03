(asdf:defsystem :clml.svm-package
                :pathname "src/"
                :serial t
                :components (
                             (:file "package")))


(asdf:defsystem :clml.svm
                :pathname "src/"
                :serial t
                :depends-on (:hjs
                             :clml.decision-tree
                             :future
                             :clml.svm-package
                             )
                :components (
                             (:file "svm")
                             (:file "wss3-svm")
                             (:file "one-class-svm")
                             (:file "pwss3-svm")
                             (:file "smo-svm")
                             ))
(asdf:defsystem :clml.svm.examples
                :pathname "examples/"
                :serial t
                :depends-on (:hjs
                             :clml.svm
                             )
                :components (
                             (:file "svm-test")
                             ))

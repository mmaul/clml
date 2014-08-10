(asdf:defsystem :clml.svm-package
                :pathname "src/"
                :serial t
                :components (
                             (:file "package")))


(asdf:defsystem :clml.svm
                :pathname "src/"
                :serial t
                :depends-on (:clml.hjs
                             :clml.decision-tree
                             :future
                             :clml.svm-package
                             )
                :components (
                             (:file "wss3-svm")
                             (:file "svm")
                             (:file "one-class-svm")
                             (:file "pwss3-svm")
                             (:file "smo-svm")
                             (:file "svr")
                             ))
(asdf:defsystem :clml.svm.examples
                :pathname "examples/"
                :serial t
                :depends-on (:clml.hjs
                             :clml.svm
                             )
                :components (
                             (:file "svm-test")
                             ))

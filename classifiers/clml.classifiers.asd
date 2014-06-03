(asdf:defsystem :clml.classifiers-package
                :pathname "src/"
                :serial t
                :components (
                             (:file "package")))


(asdf:defsystem :clml.decision-tree
                :pathname "src/"
                :serial t
                :depends-on (:hjs
                             :clml.mlr
                             :clml.hc
                             :clml.classifiers-package
                             )
                :components (
                             
                             ;(:file "logistic")
                             (:file "linear-regession")
                             ))

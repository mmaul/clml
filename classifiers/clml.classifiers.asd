(asdf:defsystem :clml.classifiers-package
                :pathname "src/"
                :serial t
                :components (
                             (:file "package")))


(asdf:defsystem :clml.classifiers
                :pathname "src/"
                :serial t
                :depends-on (:hjs
                             :clml.clustering
                             :clml.classifiers-package
                             )
                :components (
                             
                             (:file "logistic")
                             (:file "linear-regression")
                             ))

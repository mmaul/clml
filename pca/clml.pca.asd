(asdf:defsystem :clml.pca.package
                :pathname "src/"
                :serial t
                :components (
                             (:file "package")))


(asdf:defsystem :clml.pca
                :pathname "src/"
                :serial t
                :depends-on (:clml.hjs
                             :clml.decision-tree
                             :clml.pca.package
                             )
                :components (
                             (:file "pca")
                             (:file "face-recognition")
                             ))
(asdf:defsystem :clml.pca.examples
                :pathname "examples/"
                :serial t
                :depends-on (:clml.hjs
                             :clml.pca
                             )
                :components (
                             (:file "package")
                             (:file "pca-bench")
                             
                             ))

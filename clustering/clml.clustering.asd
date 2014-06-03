(asdf:defsystem :clml.clustering-package
                :pathname "src/"
                :serial t
                :components (
                             (:file "package")))


(asdf:defsystem :clml.clustering
                :pathname "src/"
                :serial t
                :depends-on (:hjs
                             :iterate
                             :clml.clustering-package
                             )
                :components (
                             (:file "hc")
                             (:file "nmf")
                             (:file "optics")
                             (:file "spectral-clustering")
                             (:file "cluster-validation")
                             (:file "k-means2")
                             ))

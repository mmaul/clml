(asdf:defsystem :clml.clustering.package
                :pathname "src/"
                :serial t
                :components (
                             (:file "package")))


(asdf:defsystem :clml.clustering
                :pathname "src/"
                :serial t
                :depends-on (:hjs
                             :iterate
                             :clml.nearest-search
                             :clml.clustering.package
                             )
                :components (
                             (:file "hc")
                             (:file "nmf")
                             (:file "optics")
                             (:file "spectral-clustering")
                             (:file "cluster-validation")
                             (:file "optics-speed")
                             #+ignlre ;; k-means2 is a work in progess
                             (:file "k-means2")
                             
                             ))

(asdf:defsystem :clml.graph-package
                :pathname "src/"
                :serial t
                :components (
                             (:file "package")))


(asdf:defsystem :clml.graph
                :pathname "src/"
                :serial t
                :depends-on (:clml.hjs
                             :clml.time-series
                             :clml.graph-package
                             :clml.statistics
                             :split-sequence
                             )
                :components (
                             (:file "read-graph")
                             (:file "anomaly-detection")
                             (:file "centrality")
                             (:file "shortest-path")
                             (:file "utils")
))

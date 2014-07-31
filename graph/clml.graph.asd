(asdf:defsystem :clml.graph-package
                :pathname "src/"
                :serial t
                :components (
                             (:file "package")))


(asdf:defsystem :clml.graph
                :pathname "src/"
                :serial t
                :depends-on (:hjs
                             :clml.time-series
                             :clml.graph-package
                             :clml.statistics
                             )
                :components (
                             (:file "read-graph")
                             (:file "anomaly-detection")
                             (:file "centrality")
                             (:file "shortest-path")
                             (:file "utils")
))

(asdf:defsystem :clml.time-series-package
                :pathname "src/"
                :serial t
                :components (
                             (:file "package")))


(asdf:defsystem :clml.time-series
                :pathname "src/"
                :serial t
                :depends-on (:clml.hjs
                             :iterate
                             :clml.numeric
                             :uiop
                             :array-operations
                             :clml.time-series-package
                             )
                :components (
                             (:file "ts-util-classes")
                             (:file "ts-state-space-model-classes")
                             (:file "ts-ar-classes")
                             (:file "ts-read-data")
                             (:file "ts-util")
                             (:file "ts-stat")
                             (:file "ts-state-space-model")
                             (:file "ts-ar")
                             (:file "changefinder")
                             (:file "ts-anomaly-detection")
                             (:file "ts-burst-detection")
                             (:file "exponential-smoothing")
                             
                             ))

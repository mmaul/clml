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
                             :clml.time-series-package
                             )
                :components (
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

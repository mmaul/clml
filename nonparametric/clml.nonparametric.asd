(asdf:defsystem :clml.nonparametric.package
                :pathname "src/"
                :serial t
                :components (
                             (:file "package")))


(asdf:defsystem :clml.nonparametric
                :pathname "src/"
                :serial t
                :depends-on (
                             :clml.hjs
                             :clml.nonparametric.package
                             )
                :components (
                             (:file "statistics")
                             (:file "gamma")
                             (:file "dpm")
                             (:file "multi-dpm")
                             (:file "hdp-lda")
                             (:file "hdp")
                             (:file "hdp-hmm-class")
                             (:file "hdp-hmm")
                             (:file "gauss-hmm")
                             (:file "sticky-hdp-hmm")
                             (:file "blocked-hdp-hmm")
                             (:file "ihmm")
                             (:file "ftm")
                             (:file "dpm-interface")
                             ))

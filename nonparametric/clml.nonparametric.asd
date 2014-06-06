(asdf:defsystem :clml.nonparametric.package
                :pathname "src/"
                :serial t
                :components (
                             (:file "package")))


(asdf:defsystem :clml.nonparametric
                :pathname "src/"
                :serial t
                :depends-on (
                             :hjs
                             :clml.nonparametric.package
                             )
                :components (
                             (:file "statistics")
                             (:file "gamma")
                             (:file "dpm")
                             (:file "multi-dpm")
                             (:file "hdp-lda")
                             (:file "hdp")
                             (:file "gauss-hmm")
                             (:file "sticky-hdp-hmm")
                             (:file "blocked-hdp-hmm")
                             (:file "ihmm")
                             (:file "ftm")
                             ))

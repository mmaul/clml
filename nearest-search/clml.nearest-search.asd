(asdf:defsystem :clml.nearest-search.package
                :pathname "src/"
                :serial t
                :components (
                             (:file "package")))


(asdf:defsystem :clml.nearest-search
                :pathname "src/"
                :serial t
                :depends-on (
                             :hjs
                             :clml.nonparametric
                             :clml.nearest-search.package
                             )
                :components (
                             (:file "k-nn")
                             (:file "k-nn-new")
                             (:file "nearest-search")
                             ))

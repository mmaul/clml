(asdf:defsystem :hjs-package
                :pathname "src/"
                :serial t
                :components (
                             (:file "package"))
                )


(asdf:defsystem :hjs
                :pathname "src/"
                :serial t
                :depends-on (
                             :alexandria
                             :iterate
                             :f2cl
                             :blas
                             :lapack
                             :future
                             :clml.statistics
                             :clml.utility
                             :hjs-package)
                :components (
                             (:file "utils")
                             (:file "vars")
                             (:file "vector")
                             (:file "missing-value")
                             (:file "matrix")
                             (:file "read-data")
                             (:file "eigenproblem")
                             (:file "k-means")
                             )
                )

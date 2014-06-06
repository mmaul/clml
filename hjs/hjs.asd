(asdf:defsystem :hjs-package
                :pathname "src/"
                :serial t
                :components (
                             (:file "package"))
                )


(asdf:defsystem :hjs
                :default-component-class cl-source-file.cl
                :pathname "src/"
                :serial t
                :depends-on (
                             :iterate
                             :f2cl
                             :blas
                             :lapack
                             :future
                             :statistics
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
                             )
                )

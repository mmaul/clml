#+no-quicklisp-controller
(asdf:defsystem :clml.hjs-package
                :pathname "src/"
                :serial t
                :components (
                             (:file "package"))
                )

 
(asdf:defsystem :clml.hjs
                :pathname "src/"
                :serial t
                :depends-on (
                             :alexandria
                             :iterate
                             :f2cl
                             :clml.blas
                             :clml.lapack
                             :future
                             :clml.statistics
                             :clml.utility
                             :clml.hjs-package)
                :components ((:file "package")
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

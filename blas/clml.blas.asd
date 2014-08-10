(asdf:defsystem :clml.blas-package 
                :pathname "src/"
                :depends-on (:f2cl)
                :components (
                             (:file "package"))
                )
( eval-when (:compile-toplevel :load-toplevel :execute)
  (setq *read-default-float-format* 'double-float)
  (loop while (not (eq *read-default-float-format* 'double-float))
     do
       (restart-case
           (error "Please set *read-default-float-format* to 'double-float before loading/compiling the system.")
         (use-double-float ()
           :report "Set double-float to *read-default-float-format*."
           (setq *read-default-float-format* 'double-float)))))

(asdf:defsystem :clml.blas.hompack
  :pathname "src/"
  :depends-on (:clml.blas-package)
  :serial t
  :components (
               (:file "daxpy")
               (:file "dcopy")
               (:file "ddot")
               (:file "dnrm2")
               (:file "dscal")
               (:file "idamax")
           ))

(asdf:defsystem :clml.blas.real
                :pathname "src/"
                :serial t
                :depends-on (:clml.blas-package)
                :components (
                             (:file "lsame")
                             (:file "xerbla")
                             (:file "dasum")
                             (:file "dcabs1")
                             (:file "dgbmv")
                             (:file "dgemm")
                             (:file "dgemv")
                             (:file "dger")
                             (:file "drot")
                             (:file "drotg")
                             (:file "dsbmv")
                             (:file "dspmv")
                             (:file "dspr")
                             (:file "dspr2")
                             (:file "dswap")
                             (:file "dsymm")
                             (:file "dsymv")
                             (:file "dsyr")
                             (:file "dsyr2")
                             (:file "dsyr2k")
                             (:file "dsyrk")
                             (:file "dtbmv")
                             (:file "dtbsv")
                             (:file "dtpmv")
                             (:file "dtpsv")
                             (:file "dtrmm")
                             (:file "dtrmv")
                             (:file "dtrsm")
                             (:file "dtrsv")
                             (:file "dzasum")
                             (:file "dznrm2")
                             (:file "icamax")
                             (:file "isamax")
                             (:file "izamax")
           ))

(asdf:defsystem :clml.blas.complex
  :pathname "src/"
  :serial t
  :depends-on (:clml.blas-package)
  :components (
               (:file "zaxpy")
               (:file "zcopy")
               (:file "zdotc")
               (:file "zdotu")
               (:file "zdscal")
               (:file "zgbmv")
               (:file "zgemm")
               (:file "zgemv")
               (:file "zgerc")
               (:file "zgeru")
               (:file "zhbmv")
               (:file "zhemm")
               (:file "zhemv")
               (:file "zher")
               (:file "zher2")
               (:file "zher2k")
               (:file "zherk")
               (:file "zhpmv")
               (:file "zhpr")
               (:file "zhpr2")
               (:file "zrotg")
               (:file "zscal")
               (:file "zswap")
               (:file "zsymm")
               (:file "zsyr2k")
               (:file "zsyrk")
               (:file "ztbmv")
               (:file "ztbsv")
               (:file "ztpmv")
               (:file "ztpsv")
               (:file "ztrmm")
               (:file "ztrmv")
               (:file "ztrsm")
               (:file "ztrsv")))

(asdf:defsystem :clml.blas
  :pathname "src/"
  :serial t
  :depends-on (:clml.blas-package
               :clml.blas.hompack
               :clml.blas.real
               :clml.blas.complex))

(asdf:defsystem :blas-package 
                :pathname "src/"
                :serial :f2cl-lib
                :components (
                             (:file "package"))
                )

(asdf:defsystem :blas-hompack
  :pathname "src/"
  :serial :blas-package
  :components (
               (:file "daxpy")
               (:file "dcopy")
               (:file "ddot")
               (:file "dnrm2")
               (:file "dscal")
               (:file "idamax")
           ))

(asdf:defsystem :blas-real
                :pathname "src/"
                :serial :blas-package
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

(asdf:defsystem :blas-complex
  :pathname "src/"
  :serial :blas-package
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

(asdf:defsystem :blas
  :pathname "src/"
  :serial :f2cl-lib
  :depends-on (
           :blas-package
           :blas-hompack
           :blas-real
           :blas-complex))

#+sbcl
(declaim (sb-ext:muffle-conditions sb-kernel:character-decoding-error-in-comment))

(defpackage :clml.blas-environment (:use :cl :asdf))
(in-package :clml.blas-environment)

(defun call-with-environment (fun)
  (let ((*read-default-float-format* 'double-float))
    (funcall fun)))

(asdf:defsystem :f2cl-lib
  :pathname "src/"
  :around-compile call-with-environment  
  :serial t
  :components ((:file "package")
               (:file "macro"))
  )

(asdf:defsystem :clml.blas.hompack
  :pathname "src/"
  :around-compile call-with-environment  
  :serial t
  :depends-on (:f2cl-lib)
  :components (
               (:file "blas-package")
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
                :around-compile call-with-environment
                :depends-on (:f2cl-lib)
                :components (
                             (:file "blas-package")
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
  :around-compile call-with-environment  
  :depends-on (:f2cl-lib)
  :components (
               (:file "blas-package")
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
  :around-compile call-with-environment  
  :depends-on (:clml.blas.hompack
               :clml.blas.real
               :clml.blas.complex))

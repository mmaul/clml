(in-package :mkl.blas)

;;;; blas1

;;; asum
(defblas asum (:single :double) :precision
  (n blas-int)
  (x (:array :precision *))
  (incx blas-int))

(defffun scasum :float
  (n blas-int)
  (x (:array complex-float *))
  (incx blas-int))
(export 'scasum)

(defffun dzasum :double
  (n blas-int)
  (x (:array complex-double *))
  (incx blas-int))
(export 'dzasum)

;;; axpy
(defblas axpy (:single :double :complex-single :complex-double) :void
  (n blas-int)
  (a :precision)
  (x (:array :precision *))
  (incx blas-int)
  (y (:array :precision *) :in-out)
  (incy blas-int))

;;; copy
(defblas copy (:single :double :complex-single :complex-double) :void
  (n blas-int)
  (x (:array :precision *))
  (incx blas-int)
  (y (:array :precision *) :in-out)
  (incy blas-int))

;;; dot
(defblas dot (:single :double) :precision
  (n blas-int)
  (x (:array :precision *))
  (incx blas-int)
  (y (:array :precision *))
  (incy blas-int))

;;; sdsdot, dsdot
(defffun sdsdot :float
  (n blas-int)
  (sb :float)
  (sx (:array :float *))
  (incx blas-int)
  (sy (:array :float *))
  (incy blas-int))
(export 'sdsdot)

(defffun dsdot :double
  (n blas-int)
  (sx (:array :float *))
  (incx blas-int)
  (sy (:array :float *))
  (incy blas-int))
(export 'dsdot)

;;; dotc
(defblas dotc (:complex-single :complex-double) :precision
  (n blas-int)
  (x (:array :precision *))
  (incx blas-int)
  (y (:array :precision *))
  (incy blas-int))

;;; dotu
(defblas dotu (:complex-single :complex-double) :precision
  (n blas-int)
  (x (:array :precision *))
  (incx blas-int)
  (y (:array :precision *))
  (incy blas-int))

;;; nrm2
(defblas nrm2 (:single :double) :precision
  (n blas-int)
  (x (:array :precision *))
  (incx blas-int))

(defffun scnrm2 :float
  (n blas-int)
  (x (:array complex-float *))
  (incx blas-int))
(export 'scnrm2)

(defffun dznrm2 :double
  (n blas-int)
  (x (:array complex-double *))
  (incx blas-int))
(export 'dznrm2)

;;; rot
(defblas rot (:single :double) :void
  (n blas-int)
  (x (:array :precision *) :in-out)
  (incx blas-int)
  (y (:array :precision *) :in-out)
  (incy blas-int)
  (c :precision)
  (s :precision))

;; FIXME: cause SBCL to crash
(defffun csrot :void
  (n blas-int)
  (x (:array complex-float *) :in-out)
  (incx blas-int)
  (y (:array complex-float *) :in-out)
  (incy blas-int)
  (c :float)
  (s :float))
(export 'csrot)

;; FIXME: cause SBCL to crash
(defffun zdrot :void
  (n blas-int)
  (x (:array complex-double *) :in-out)
  (incx blas-int)
  (y (:array complex-double *) :in-out)
  (incy blas-int)
  (c :double)
  (s :double))
(export 'zdrot)

;;; rotg
(defblas rotg (:single :double :complex-single :complex-double) :void
  (a :precision :in-out)
  (b :precision :in-out)
  (c :precision :out)
  (d :precision :out))

;;; rotm
(defblas rotm (:single :double) :void
  (n blas-int)
  (x (:array :precision *) :in-out)
  (incx blas-int)
  (y (:array :precision *) :in-out)
  (incy blas-int)
  (param (:array :precision 5)))

;;; rotmg
(defblas rotmg (:single :double) :void
  (d1 :precision :in-out)
  (d2 :precision :in-out)
  (x1 :precision :in-out)
  (y1 :precision)
  (param (:array :precision 5) :out))

;;; scal
(defblas scal (:single :double :complex-single :complex-double) :void
  (n blas-int)
  (a :precision)
  (x (:array :precision *) :in-out)
  (incx blas-int))

;;; swap
(defblas swap (:single :double :complex-single :complex-double) :void
  (n blas-int)
  (x (:array :precision *) :in-out)
  (incx blas-int)
  (y (:array :precision *) :in-out)
  (incy blas-int))

;;; i?amax
;;; it's generated and modified from
;; (defblas amax (:single :double :complex-single :complex-double)
;;     :precision
;;   (n blas-int)
;;   (x (:array :precision *))
;;   (incx blas-int))
(PROGN
  (DEFFFUN iSAMAX :FLOAT
    (N BLAS-INT)
    (X (:ARRAY :FLOAT *))
    (INCX BLAS-INT))
  (EXPORT 'iSAMAX)
  (DEFFFUN iDAMAX :DOUBLE
    (N BLAS-INT)
    (X (:ARRAY :DOUBLE *))
    (INCX BLAS-INT))
  (EXPORT 'iDAMAX)
  (DEFFFUN iCAMAX COMPLEX-FLOAT
    (N BLAS-INT)
    (X (:ARRAY COMPLEX-FLOAT *))
    (INCX BLAS-INT))
  (EXPORT 'iCAMAX)
  (DEFFFUN iZAMAX COMPLEX-DOUBLE
    (N BLAS-INT)
    (X (:ARRAY COMPLEX-DOUBLE *))
    (INCX BLAS-INT))
  (EXPORT 'iZAMAX))

;;; i?amin
;;; it's generated and modified from
;; (defblas amin (:single :double :complex-single :complex-double)
;;     :precision
;;   (n blas-int)
;;   (x (:array :precision *))
;;   (incx blas-int))
(PROGN
  (DEFFFUN iSAMIN :FLOAT
    (N BLAS-INT)
    (X (:ARRAY :FLOAT *))
    (INCX BLAS-INT))
  (EXPORT 'iSAMIN)
  (DEFFFUN iDAMIN :DOUBLE
    (N BLAS-INT)
    (X (:ARRAY :DOUBLE *))
    (INCX BLAS-INT))
  (EXPORT 'iDAMIN)
  (DEFFFUN iCAMIN COMPLEX-FLOAT
    (N BLAS-INT)
    (X (:ARRAY COMPLEX-FLOAT *))
    (INCX BLAS-INT))
  (EXPORT 'iCAMIN)
  (DEFFFUN iZAMIN COMPLEX-DOUBLE
    (N BLAS-INT)
    (X (:ARRAY COMPLEX-DOUBLE *))
    (INCX BLAS-INT))
  (EXPORT 'iZAMIN))

;;; cabs1
#+nil
(defblas cabs1 (:double) :double
  (z :complex-double))



;;;; blas2

;;; gbmv
(defblas gbmv (:single :double :complex-single :complex-double) :void
  (trans :string)
  (m blas-int)
  (n blas-int)
  (kl blas-int)
  (ku blas-int)
  (alpha :precision)
  (a (:array :precision * *))
  (lda blas-int)
  (x :precision)
  (incx blas-int)
  (beta :precision)
  (y (:array :precision *) :in-out)
  (incy blas-int))

;;; gemv
(defblas gemv (:single :double :complex-single :complex-double) :void
  (trans :string)
  (m blas-int)
  (n blas-int)
  (alpha :precision)
  (a (:array :precision * *))
  (lda blas-int)
  (x (:array :precision *))
  (incx blas-int)
  (beta :precision)
  (y (:array :precision *) :in-out)
  (incy blas-int))

;;; ger
(defblas ger (:single :double) :void
  (m blas-int)
  (n blas-int)
  (alpha :precision)
  (x (:array :precision *))
  (incx blas-int)
  (y (:array :precision *))
  (incy blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int))

;;; gerc
(defblas gerc (:complex-single :complex-double) :void
  (m blas-int)
  (n blas-int)
  (alpha :precision)
  (x (:array :precision *))
  (incx blas-int)
  (y (:array :precision *))
  (incy blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int))

;;; geru
(defblas geru (:complex-single :complex-double) :void
  (m blas-int)
  (n blas-int)
  (alpha :precision)
  (x (:array :precision *))
  (incx blas-int)
  (y (:array :precision *))
  (incy blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int))

;;; hbmv
(defblas hbmv (:complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (k blas-int)
  (alpha :precision)
  (a (:array :precision * *))
  (lda blas-int)
  (x (:array :precision *))
  (incx blas-int)
  (beta :precision)
  (y (:array :precision *) :in-out)
  (incy blas-int))

;;; hemv
(defblas hemv (:complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (k blas-int)
  (alpha :precision)
  (a (:array :precision * *))
  (lda blas-int)
  (x (:array :precision *))
  (incx blas-int)
  (beta :precision)
  (y (:array :precision *) :in-out)
  (incy blas-int))

;;; her
(defblas her (:complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (alpha (case :precision
	   (:complex-single :single)
	   (:complex-double :double)))
  (x (:array :precision *))
  (incx blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int))

;;; her2
(defblas her2 (:complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (alpha :precision)
  (x (:array :precision *))
  (incx blas-int)
  (y (:array :precision *))
  (incy blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int))

;;; hpmv
(defblas hpmv (:complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (alpha :precision)
  (ap (:array :precision *))
  (x (:array :precision *))
  (incx blas-int)
  (beta :precision)
  (y (:array :precision *) :in-out)
  (incy blas-int))

;;; hpr
(defblas hpr (:complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (alpha (case :precision
	   (:complex-single :single)
	   (:complex-double :double)))
  (x (:array :precision *))
  (incx blas-int)
  (ap (:array :precision *) :in-out))

;;; hpr2
(defblas hpr2 (:complex-single :complex-double) :void
  (uplo :string)
  (n blas-int)
  (alpha :precision)
  (x (:array :precision *))
  (incx blas-int)
  (y (:array :precision *))
  (incy blas-int)
  (ap (:array :precision *) :in-out))

;;; sbmv
(defblas sbmv (:single :double) :void
  (uplo :string)
  (n blas-int)
  (k blas-int)
  (alpha :precision)
  (a (:array :precision * *))
  (lda blas-int)
  (x (:array :precision *))
  (incx blas-int)
  (beta :precision)
  (y (:array :precision *) :in-out)
  (incy blas-int))

;;; spmv
(defblas spmv (:single :double) :void
  (uplo :string)
  (n blas-int)
  (alpha :precision)
  (ap (:array :precision *))
  (x (:array :precision *))
  (incx blas-int)
  (beta :precision)
  (y (:array :precision *) :in-out)
  (incy blas-int))

;;; spr
(defblas spr (:single :double) :void
  (uplo :string)
  (n blas-int)
  (alpha :precision)
  (x (:array :precision *))
  (incx blas-int)
  (ap (:array :precision *) :in-out))

;;; spr2
(defblas spr2 (:single :double) :void
  (uplo :string)
  (n blas-int)
  (alpha :precision)
  (x (:array :precision *))
  (incx blas-int)
  (y (:array :precision *))
  (incy blas-int)
  (ap (:array :precision *) :in-out))

;;; symv
(defblas symv (:single :double) :void
  (uplo :string)
  (n blas-int)
  (alpha :precision)
  (a (:array :precision * *))
  (lda blas-int)
  (x (:array :precision *))
  (incx blas-int)
  (beta :precision)
  (y (:array :precision *) :in-out)
  (incy blas-int))

;;; syr
(defblas syr (:single :double) :void
  (uplo :string)
  (n blas-int)
  (alpha :precision)
  (x (:array :precision *))
  (incx blas-int)
  (a (:array :precision * *) :in-out)
  (lda blas-int))

;;; syr2
(defblas syr2 (:single :double) :void
  (uplo :string)
  (n blas-int)
  (alpha :precision)
  (x (:array :precision *))
  (incx blas-int)
  (y (:array :precision *))
  (incy blas-int)
  (a (:array :precision *) :in-out)
  (lda blas-int))

;;; tbmv
(defblas tbmv (:single :double :complex-single :complex-double) :void
  (uplo :string)
  (trans :string)
  (diag :string)
  (n blas-int)
  (k blas-int)
  (a (:array :precision * *))
  (lda blas-int)
  (x (:array :precision *) :in-out)
  (incx blas-int))

;;; tbsv
(defblas tbsv (:single :double :complex-single :complex-double) :void
  (uplo :string)
  (trans :string)
  (diag :string)
  (n blas-int)
  (k blas-int)
  (a (:array :precision * *))
  (lda blas-int)
  (x (:array :precision *) :in-out)
  (incx blas-int))

;;; tpmv
(defblas tpmv (:single :double :complex-single :complex-double) :void
  (uplo :string)
  (trans :string)
  (diag :string)
  (n blas-int)
  (ap (:array :precision *))
  (x (:array :precision *) :in-out)
  (incx blas-int))

;;; tpsv
(defblas tpsv (:single :double :complex-single :complex-double) :void
  (uplo :string)
  (trans :string)
  (diag :string)
  (n blas-int)
  (ap (:array :precision *))
  (x (:array :precision *) :in-out)
  (incx blas-int))

;;; trmv
(defblas trmv (:single :double :complex-single :complex-double) :void
  (uplo :string)
  (trans :string)
  (diag :string)
  (n blas-int)
  (a (:array :precision * *))
  (lda blas-int)
  (x (:array :precision *) :in-out)
  (incx blas-int))

;;; trsv
(defblas trsv (:single :double :complex-single :complex-double) :void
  (uplo :string)
  (trans :string)
  (diag :string)
  (n blas-int)
  (a (:array :precision * *))
  (lda blas-int)
  (x (:array :precision *) :in-out)
  (incx blas-int))



;;;; blas 3

;;; gemm
(defblas gemm (:single :double :complex-single :complex-double) :void
  (transa :string)
  (transb :string)
  (m blas-int)
  (n blas-int)
  (k blas-int)
  (alpha :precision)
  (a (:array :precision * *))
  (lda blas-int)
  (b (:array :precision * *))
  (ldb blas-int)
  (beta :precision)
  (c (:array :precision * *) :in-out)
  (ldc blas-int))

;;; hemm
(defblas hemm (:complex-single :complex-double) :void
  (side :string)
  (uplo :string)
  (m blas-int)
  (n blas-int)
  (alpha :precision)
  (a (:array :precision * *))
  (lda blas-int)
  (b (:array :precision * *))
  (ldb blas-int)
  (beta :precision)
  (c (:array :precision * *) :in-out)
  (ldc blas-int))

;;; herk
(defblas herk (:complex-single :complex-double) :void
  (uplo :string)
  (trans :string)
  (n blas-int)
  (k blas-int)
  (alpha (case :precision
	   (:complex-single :single)
	   (:complex-double :double)))
  (a (:array :precision * *))
  (lda blas-int)
  (beta (case :precision
	  (:complex-single :single)
	  (:complex-double :double)))
  (c (:array :precision * *) :in-out)
  (ldc blas-int))

;;; her2k
(defblas her2k (:complex-single :complex-double) :void
  (uplo :string)
  (trans :string)
  (n blas-int)
  (k blas-int)
  (alpha :precision)
  (a (:array :precision * *))
  (lda blas-int)
  (beta :precision)
  (b (:array :precision * *))
  (ldb blas-int)
  (c (:array :precision * *) :in-out)
  (ldc blas-int))

;;; symm
(defblas symm (:single :double :complex-single :complex-double) :void
  (side :string)
  (uplo :string)
  (m blas-int)
  (n blas-int)
  (alpha :precision)
  (a (:array :precision * *))
  (lda blas-int)
  (b (:array :precision * *))
  (ldb blas-int)
  (beta :precision)
  (c (:array :precision * *) :in-out)
  (ldc blas-int))

;;; syrk
(defblas syrk (:single :double :complex-single :complex-double) :void
  (uplo :string)
  (trans :string)
  (n blas-int)
  (k blas-int)
  (alpha :precision)
  (a (:array :precision * *))
  (lda blas-int)
  (beta :precision)
  (c (:array :precision * *) :in-out)
  (ldc blas-int))

;;; syrk2
(defblas syr2k (:single :double :complex-single :complex-double) :void
  (uplo :string)
  (trans :string)
  (n blas-int)
  (k blas-int)
  (alpha :precision)
  (a (:array :precision * *))
  (lda blas-int)
  (b (:array :precision * *))
  (ldb blas-int)
  (beta :precision)
  (c (:array :precision * *) :in-out)
  (ldc blas-int))

;;; trmm
(defblas trmm (:single :double :complex-single :complex-double) :void
  (side :string)
  (uplo :string)
  (transa :string)
  (diag :string)
  (m blas-int)
  (n blas-int)
  (alpha :precision)
  (a (:array :precision * *))
  (lda blas-int)
  (b (:array :precision * *) :in-out)
  (ldb blas-int))

;;; trsm
(defblas trsm (:single :double :complex-single :complex-double) :void
  (side :string)
  (uplo :string)
  (transa :string)
  (diag :string)
  (m blas-int)
  (n blas-int)
  (alpha :precision)
  (a (:array :precision * *))
  (lda blas-int)
  (b (:array :precision * *) :in-out)
  (ldb blas-int))


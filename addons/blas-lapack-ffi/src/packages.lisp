(in-package :cl)

(defpackage :ffi-utils
  (:use :cl :alexandria)
  (:export #:complex-float
	   #:complex-double
	   #:defffun
	   #:with-safe-foreign-function-call-settings
	   #:make-static-array
	   #:with-arrays-as-foreign-arrays
	   ))

(defpackage :mkl.blas-lapack-common
  (:use :cl :alexandria :ffi-utils)
  (:export #:blas-int
	   #:+precision-definitions+
	   #:+matrix-type-definitions+
	   #:defblas
	   #:deflapack
	   ))

(defpackage :mkl.blas
  (:use :cl :alexandria :ffi-utils :mkl.blas-lapack-common))

(defpackage :mkl.lapack
  (:use :cl :alexandria :ffi-utils :mkl.blas-lapack-common))



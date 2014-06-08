(defpackage :mkl.blas-lapack.test
    (:use :cl :ffi-utils :iterate :stefil)
  (:export #:one-value
           #:make-random-array
           #:make-random-symmetric-matrix
           #:setup-array
           #:defcached
           #:reset-value
           #:~=
           #:coerce-to-double
           #:slice
           #:round-array
           #:transpose-list-array
           ))

(defpackage :mkl.blas.test
    (:use :cl :ffi-utils :mkl.blas-lapack.test :mkl.blas :iterate :stefil))

(defpackage :mkl.lapack.test
    (:use :cl :ffi-utils :mkl.blas-lapack.test :mkl.lapack :iterate :stefil))

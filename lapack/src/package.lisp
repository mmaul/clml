( eval-when (:compile-toplevel)
  (setq *read-default-float-format* 'double-float))
(defpackage "LAPACK"
  (:use "BLAS" "COMMON-LISP")
  (:shadow :zswap)
)

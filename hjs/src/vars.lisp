
(in-package :clml.hjs.vars)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (proclaim '(type double-float *epsilon*)))

(defparameter *epsilon* (coerce  1e-8 'double-float))

(defparameter *workers* 4)

(define-constant +most-negative-exp-able-float+ #.(log least-positive-double-float) :test #'equal)
(define-constant +most-positive-exp-able-float+ #.(log most-positive-double-float) :test #'equal)

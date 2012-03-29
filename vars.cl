(defpackage :hjs.learn.vars
  (:use :cl)
  (:nicknames :vars)
  (:export #:*epsilon*
           #:*workers*
           #:*most-negative-exp-able-float*
           #:*most-positive-exp-able-float*
           ))

(in-package :hjs.learn.vars)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (proclaim '(type double-float *epsilon*)))

(defparameter *epsilon* 1e-8)

(defparameter *workers* 4)

(defconstant *most-negative-exp-able-float* #.(log least-positive-double-float))
(defconstant *most-positive-exp-able-float* #.(log most-positive-double-float))

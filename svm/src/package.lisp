(in-package :cl-user)

(defpackage :clml.svm.mu
  (:nicknames :svm)
  (:use :common-lisp
        :hjs.util.meta)
  (:export :kernel
           :polynomial-kernel 
           :+linear-kernel+
           :radial-kernel :gaussian-kernel
           :sigmoid-kernel
           :svm))

(defpackage :clml.svm.wss3
  (:nicknames :svm.wss3)
  (:use :cl
	:hjs.util.meta
	:hjs.util.vector
	:hjs.learn.read-data
    :hjs.util.matrix)
  (:import-from :decision-tree
		#:sum-up)
  (:export #:make-svm-learner
	   #:load-svm-learner
	   #:make-linear-kernel
	   #:make-rbf-kernel
	   #:make-polynomial-kernel
	   #:make-one-class-svm-kernel
	   #:svm-validation
       #:sign
       #:call-kernel-function-uncached
       #:call-kernel-function
       #:define-kernel-function
	   ))


(defpackage :clml.svm.one-class
  (:use :cl
	:clml.svm.wss3
	:hjs.util.meta
	:hjs.util.vector
	:hjs.learn.read-data
    :hjs.util.matrix)
  (:nicknames :one-class-svm)
  (:import-from :svm.wss3
		#:sign
		#:call-kernel-function-uncached
		#:call-kernel-function)
  (:export #:one-class-svm
           #:qp-solver
           ))

(defpackage :clml.svm.pwss3
  (:nicknames :pwss3-svm)
  (:use :cl
          :hjs.util.meta
          :hjs.util.vector
          :hjs.learn.read-data
          :hjs.util.matrix)
  (:import-from :decision-tree
		#:sum-up)
  (:export #:make-svm-learner
	   #:load-svm-learner
	   #:make-linear-kernel
	   #:make-rbf-kernel
	   #:make-polynomial-kernel
	   #:make-one-class-svm-kernel
	   #:svm-validation
	   ))

(defpackage :clml.svm.smo
  (:use :cl
	:hjs.learn.read-data
        :hjs.util.vector
        :hjs.util.matrix
        :hjs.util.meta)
  (:import-from :decision-tree
		#:sum-up)
  (:export
   ;;#:linear-kernel
   ;;#:make-rbf-kernel
   ;;#:make-polynomial-kernel
   ;;#:make-svm-learner
   ;;#:svm-validation
   ;;#:load-svm-learner
   ;;#:call-kernel-function-with-indices
   ;;#:call-kernel-function-with-vectors))
   ))
(defpackage :clml.svm.svr
  (:nicknames :svr)
  (:use :cl
	:svm.wss3
	:hjs.util.meta
	:hjs.util.vector
	:hjs.learn.read-data
        :hjs.util.matrix)
  (:import-from :svm.wss3
		#:call-kernel-function-uncached
		#:call-kernel-function
		#:define-kernel-function)
  (:export #:make-svr-learner
	   #:load-svr-learner
	   #:svr-validation
	   ))

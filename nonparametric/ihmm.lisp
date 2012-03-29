;; generalized sticky and blocked HDP-HMM
(defpackage :nonparametric.ihmm
  ; (:nicknames :ihmm)
  (:use :cl :hjs.util.meta
	:nonpara.stat
	:nonparametric.dpm
	:nonparametric.hdp
	:nonparametric.hdp-hmm
	:nonparametric.sticky-hdp-hmm
	:nonparametric.blocked-hdp-hmm)
  (:export :ihmm
	   :ihmm-state
	   :ihmm-state-uniform))

(in-package :nonparametric.ihmm)

(defclass ihmm (blocked-hdp-hmm sticky-hdp-hmm)
  ((base-distribution :initform (make-instance 'ihmm-state-uniform))))

(defclass ihmm-state (blocked-hidden-state sticky-hidden-state) ())

(defclass ihmm-state-uniform (block-uniform sticky-state-uniform)
  ((cluster-class :initform 'ihmm-state)))

(defmethod sampling-pi ((state ihmm-state) (dpm ihmm))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((spi (state-pi state))
	(states (dpm-clusters dpm))
	(L (hdp-hmm-L dpm))
	(p (dpm-p dpm))
	(alpha (dpm-hyper dpm))
	(table (cluster-dist-table state))
	(before (sorted-before state)))
    (declare (type hash-table table)
	     (type fixnum L)
	     (type double-float alpha)
	     (type vector before)
	     (type (vector double-float) p spi))
    (when (< (the fixnum (array-dimension before 0)) l)
      (adjust-array before l)
      (adjust-array spi l))
    (setf (fill-pointer before) l)
    (setf (fill-pointer spi) l)
    (loop for i fixnum from 0 below L
	for s = (aref states i) do
	  (setf (aref p i) (the double-float (+ (* alpha (the double-float (cluster-beta s)))
						(the double-float
						  (+
						   (the double-float
						     (if (eq state s)
							 (sticky-kappa dpm)
						       0d0))
						   (the fixnum (gethash s table 0)))))))
	  (setf (aref before i) s))
    (dirichlet-random p p)
    ;; copy
    (loop for i fixnum from 0 below L do
	  (setf (aref spi i) (aref p i))
	  ;; sort spi
	finally (sort spi #'(lambda (x y) (declare (type double-float x y)) (> x y))))
    ;; sort before
    (sort before #'(lambda (x y) (declare (type fixnum x y)) (< x y))
	  :key #'(lambda (x) (position (aref p (position x states)) spi)))
      ))

;; exapmle
(defclass gauss-ihmm-state (ihmm-state gaussian-state) ())

(defclass gauss-ihmm (ihmm gauss-hdp-hmm)
  ((base-distribution :initform (make-instance 'ihmm-gaussian))))

(defclass ihmm-gaussian (ihmm-state-uniform state-gaussian)
  ((cluster-class :initform 'gauss-ihmm-state)))

;; for compare
(defclass gauss-block-state (blocked-hidden-state gaussian-state) ())

(defclass gauss-block-hmm (blocked-hdp-hmm gauss-hdp-hmm)
  ((base-distribution :initform (make-instance 'block-gaussian))))

(defclass block-gaussian (block-uniform state-gaussian)
  ((cluster-class :initform 'gauss-block-state)))

;; two type test
(defun make-blocked-test (length states &optional (stds (map 'vector #'(lambda (x) (declare (ignore x)) 1d0) states)))
  (let ((trans (make-array length :element-type 'fixnum))
	(ans (make-array length)))
    (loop for i from 0 below length
	for s = 0 then (case (random 100)
			 (98 (ecase s
			       (0 1)
			       (1 2)
			       (2 0)))
			 (99 (ecase s
			       (0 2)
			       (1 0)
			       (2 1)))
			 (t s))
	for d = (normal-random (aref states s) (aref stds s)) do
	  (setf (aref trans i) s)
	  (setf (aref ans   i) (make-instance 'seq-point :data d)))
    (values trans (make-instance 'point-sequence :seq ans))))

(defun make-cyclic-test (length states &optional (stds (map 'vector #'(lambda (x) (declare (ignore x)) 1d0) states)))
  (let ((trans (make-array length :element-type 'fixnum))
	(ans (make-array length)))
    (loop for i from 0 below length
	for s = 0 then (let ((r (random 100)))
			 (if (> r 97)
			     (let ((new (1+ s)))
			       (if (= new (length states))
				   0
				 new))
			   s))
	for d = (normal-random (aref states s) (aref stds s)) do
	  (setf (aref trans i) s)
	  (setf (aref ans   i) (make-instance 'seq-point :data d)))
    (values trans (make-instance 'point-sequence :seq ans))))
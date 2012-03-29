;;; -*- mode: lisp; syntax: common-lisp -*-

;;; Support Vector Machine Library
;;; Peter Salvi, 2008

;;; Iterative solution, as in
;;; Multiplicative Updates for Nonnegative Quadratic Programming in
;;; Support Vector Machines, F. Sha, L. K. Saul, D. D. Lee.
;;; and the sum constraint is described in
;;; Multiplicative Updates for Large Margin Classifiers,
;;; F. Sha, L. K. Saul, D. D. Lee.

;;; A nice and clear explanation of SVMs can be found in
;;; Support Vector Machines Explained, Tristan Fletcher, 2008
;;; http://www.csd.uwo.ca/courses/CS9860b/papers/

;;; TODO: Regression

(in-package :cl-user)

(defpackage :svm.mu
  (:use :common-lisp :hjs.util.meta)
  (:export :kernel
           :polynomial-kernel 
           :+linear-kernel+
           :radial-kernel :gaussian-kernel
           :sigmoid-kernel
           :svm))

(in-package :svm.mu)

#+ignore                                ; too general
(defun v- (&rest u)
  (apply #'mapcar #'- u))

(defun v- (u v)
  (declare (optimize speed (safety 0) (debug 0)))
  (loop for ui in u
      for vi in v
      collect (- (the double-float ui) (the double-float vi))))

#+ignore                                ; too general
(defun scalar-product (u v)
  (apply #'+ (mapcar #'* u v)))

(defun scalar-product (u v)
  (declare (optimize speed (safety 0) (debug 0)))
  (loop for ui in u
      for vi in v
      sum (* (the double-float ui)
             (the double-float vi)) of-type double-float))

#+ignore                                ; use matrix:m*v
(defun m*v (m v)
  "Multiplies a square matrix with a vector."
  (let ((n (array-dimension m 0)))
    (loop for i from 0 below n
       collect (loop for j from 0 below n as x across v
		  sum (* (aref m i j) x)))))

(defclass kernel () ((biasedp :initarg :biasedp :reader biasedp)))
(defgeneric kernel (kernel x1 x2))

(defclass polynomial-kernel (kernel)
  ((biasedp :initform t)
   (dimension :initarg :dimension :reader dimension)
   (homogeneousp :initarg :homogeneousp :reader homogeneousp)))
(defmethod print-object ((obj polynomial-kernel) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream ": D = ~a ~:[NON-~;~]HOMOGENEOUS"
	    (dimension obj) (homogeneousp obj))))
(defmethod kernel ((kernel polynomial-kernel) x1 x2)
  (if (homogeneousp kernel)
      (expt (scalar-product x1 x2) (dimension kernel))
      (expt (1+ (scalar-product x1 x2)) (dimension kernel))))
(defun polynomial-kernel (dimension homogeneousp)
  (make-instance 'polynomial-kernel
		 :dimension dimension :homogeneousp homogeneousp))

(defparameter +linear-kernel+ (polynomial-kernel 1 t))

(defclass radial-kernel (kernel)
  ((biasedp :initform nil)
   (gamma :initarg :gamma :reader gamma)))
(defmethod print-object ((obj radial-kernel) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream ": GAMMA = ~a" (gamma obj))))
(defmethod kernel ((kernel radial-kernel) x1 x2)
  (let ((d (v- x1 x2)))
    (handler-case
        (exp (* (scalar-product d d) (- (gamma kernel))))
      (FLOATING-POINT-UNDERFLOW (c)
        (declare (ignore c))
        0.0d0))))
(defun radial-kernel (gamma)
  "For GAMMA > 0."
  (make-instance 'radial-kernel :gamma gamma))

(defun gaussian-kernel (sigma2)
  (radial-kernel (/ (* 2 sigma2))))

(defclass sigmoid-kernel (kernel)
  ((biasedp :initform t)
   (kappa :initarg :kappa :reader kappa)
   (shift :initarg :shift :reader shift)))
(defmethod print-object ((obj sigmoid-kernel) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream ": KAPPA = ~a, SHIFT = ~a" (kappa obj) (shift obj))))
(defmethod kernel ((kernel sigmoid-kernel) x1 x2)
  (tanh (+ (* (kappa kernel) (scalar-product x1 x2)) (shift kernel))))
(defun sigmoid-kernel (kappa shift)
  "For some [not every] KAPPA > 0 and SHIFT < 0."
  (make-instance 'sigmoid-kernel :kappa kappa :shift shift))

(defun decision (kernel weighted-data bias)
  (lambda (z)
    (let ((value (+ (loop for weight-xi in weighted-data
		       sum (* (first weight-xi)
			      (kernel kernel (second weight-xi) z)))
		    bias)))
      (values (>= value 0) value))))

(defun svm-init (kernel positive-data negative-data)
  "AP and AM are the matrices A+ and A- in the paper, respectively."
  (declare (optimize speed (safety 0) (debug 0))
           (type cons positive-data negative-data))
  (let* ((np (length positive-data))
         (nm (length negative-data))
         (n (+ np nm))
         (ap (make-array (list n n) :initial-element 0.0d0 :element-type 'double-float))
         (am (make-array (list n n) :initial-element 0.0d0 :element-type 'double-float)))
    (declare (type fixnum n np nm))
    (macrolet ((init (posi-p posj-p)
                 (let ((eq-ij (eq posi-p posj-p))
                       (data-i (if posi-p 'positive-data 'negative-data))
                       (data-j (if posj-p 'positive-data 'negative-data)))
                   `(dotimes (i ,(if posi-p 'np 'nm))
                      (declare (type fixnum i))
                      (dotimes (j ,(if posj-p 'np 'nm))
                        (declare (type fixnum i))
                        (let ((x (kernel kernel
                                         (nth i ,data-i) (nth j ,data-j))))
                          (declare (type double-float x))
                          (if (,(if eq-ij '>= '<) x 0d0)
                              (setf (aref ap
                                          ,(if posi-p 'i '(+ np i))
                                          ,(if posj-p 'j '(+ np j)))
                                ,(if eq-ij 'x '(- x)))
                            (setf (aref am
                                        ,(if posi-p 'i '(+ np i))
                                        ,(if posj-p 'j '(+ np j)))
                              ,(if eq-ij '(- x) 'x)))))))))
      (init t t) (init t nil) (init nil t) (init nil nil))
    (values n np nm ap am)))

(defun svm (kernel positive-data negative-data
	    &key (iterations 100) (lagrange-iterations 20) (tolerance 1.0d-20))
  "Returns a decision function based on the given kernel function and
training data."
  (declare (optimize speed (safety 0) (debug 0)))
  (multiple-value-bind (n np nm ap am)
      (svm-init kernel positive-data negative-data)
    (declare (type dvec ap am))
    (flet ((res (i L apalpha amalpha)
             (declare (optimize speed (safety 0) (debug 0))
                      (type fixnum i)
                      (type double-float L)
                      (type dvec apalpha amalpha))
	     (let ((signed-L (- (* (if (< i np) L  (- L))) 1.0d0)))
               (declare (type double-float signed-L))
	       (safe-/ (- (sqrt (+ (* signed-L signed-L)
				   (* 4.0d0 (aref apalpha i) (aref amalpha i))))
			  signed-L)
		       (* 2.0d0 (aref apalpha i))))))      
      (let ((alpha (make-array n :initial-element 1.0d0 :element-type 'double-float))
	    (tmp (make-array n :element-type 'double-float)))
        (declare (type dvec alpha tmp))
	(loop repeat iterations
	   for apalpha = (matrix:m*v ap alpha)
	   for amalpha = (matrix:m*v am alpha)
	   for R =
	     (loop for i from 0 below n sum
		  (safe-/ (aref alpha i) (aref apalpha i)))
	   for L =
	     (loop repeat lagrange-iterations with result = 0.0d0 do
		  (incf result
			(/ (- (loop for i from 0 below np
				 sum (* (aref alpha i)
					(res i result apalpha amalpha)))
			      (loop for i from np below n
				 sum (* (aref alpha i)
					(res i result apalpha amalpha))))
			   R))
		  finally (return result))
	   do
	     (dotimes (i n)
	       (setf (aref tmp i) (* (aref alpha i) (res i L apalpha amalpha))))
	     (dotimes (i n) (setf (aref alpha i) (aref tmp i))))
	(let ((all-data (append positive-data negative-data)))
	  (decision kernel
		    (loop for i upfrom 0 as a across alpha
		       for x in all-data
		       when (> a tolerance)
		       collect (list (* a (if (< i np) 1 -1)) x))
		    (if (biasedp kernel)
			(/ (- np nm
			      (loop for xj in all-data sum
				   (- (loop for xi in positive-data
					 for a across alpha
					 sum (* a (kernel kernel xi xj)))
				      (loop for xi in negative-data
					 for a across (subseq alpha np)
					 sum (* a (kernel kernel xi xj))))))
			   n)
			0.0d0)))))))

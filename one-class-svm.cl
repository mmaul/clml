;;;Support Vector Regression Package using SMO-type algorithm
;;;Abe Yusuke, Jianshi Huang. 2010 May
;;;Reference: "A Study on SMO-type Decomposition Methods for Support Vector Machines"
;;;Pai-Hsuen Chen, Rong-En Fan, and Chih-Jen Lin


(defpackage :one-class-svm
  (:use :cl
	:svm.wss3
	:hjs.util.meta
	:hjs.util.vector
	:hjs.learn.read-data
        :hjs.util.matrix)
  (:import-from :svm.wss3
		#:sign
		#:call-kernel-function-uncached
		#:call-kernel-function)
  (:export #:one-class-svm
	   ))

(in-package one-class-svm)

;; (declaim (optimize speed (safety 0) (debug 1)))

(defparameter *eps* 1d-3)
(defparameter *tau* 1d-12)
(defparameter *training-size* 0)
(defparameter *alpha-array* (make-array 0 :element-type 'double-float))
(defparameter *gradient-array* (make-array 0 :element-type 'double-float))
(defparameter *kernel-function-result* (make-array 1 :element-type 'double-float :initial-element 0.0d0))


(declaim (type double-float *eps* *tau*)
         (type fixnum *training-size*)
         (type dvec *alpha-array* *gradient-array*)
         (type (simple-array double-float (1)) *kernel-function-result*))


(defun qp-solver (training-vector kernel-function nu)
  "for one-class-svm"
  (declare (type simple-vector training-vector)
	   (type function kernel-function)
	   (type double-float nu))
  
  (setf *training-size* (length training-vector))
  (setf *alpha-array* (make-array *training-size* :element-type 'double-float :initial-element 0.0d0))
  (setf *gradient-array* (make-array *training-size* :element-type 'double-float :initial-element 0.0d0))
  
  (let ((tau *tau*)
	(training-size *training-size*)
	(alpha-array *alpha-array*)
	(gradient-array *gradient-array*))
    
    (declare (type double-float tau)
	     (type fixnum training-size)
	     (type (simple-array double-float (*)) alpha-array gradient-array)
	     (ignorable training-size))
    
    
    ;;initialize alpha-array
    (multiple-value-bind (n r) (floor (* nu training-size))
      
      (declare (type fixnum n)
	       (type double-float r))
      (loop
	  for i of-type fixnum below n
	  do (setf (aref alpha-array i) 1.0d0)
	  finally (setf (aref alpha-array n) r))
      
      ;;initialize gradient-array
      (loop
	  for i of-type fixnum below training-size
	  with point-n of-type (simple-array double-float (*)) = (svref training-vector n)
	  as point-i of-type (simple-array double-float (*)) = (svref training-vector i)
	  as k-i of-type double-float = (coerce (loop 
						    for j of-type fixnum below n
						    as point-j of-type (simple-array double-float (*)) = (svref training-vector j)
						    sum (call-kernel-function kernel-function point-i point-j)) 'double-float)
	  do (setf (aref gradient-array i) (+ k-i
					      (* r 
						 (call-kernel-function kernel-function point-i point-n))))))
    (loop
	while t
	do (multiple-value-bind (i j)
	       (working-set-selection3 training-vector kernel-function)
	     (declare (type fixnum i j))
	     (when (= -1 j)
	       (return-from qp-solver *alpha-array*))
             
	     (let ((a (eta training-vector kernel-function i j))
		   (b (- (aref gradient-array j)
			 (aref gradient-array i))))
               
	       (declare (type double-float a b))

	       (when (<= a 0.0d0)
		 (setf a tau))
		 
	       ;;update alpha
	       (let ((old-a-i (aref alpha-array i))
		     (old-a-j (aref alpha-array j)))
                 
		 (declare (type double-float old-a-i old-a-j))
		 
		 (incf (aref alpha-array i) (/ b a))
		 (decf (aref alpha-array j) (/ b a))
		 
		 ;;clipping
		 (let ((sum (+ old-a-i old-a-j)))
                   
		   (declare (type double-float sum))
                   
		   (when (> (aref alpha-array i) 1.0d0)
		     (setf (aref alpha-array i) 1.0d0))
                   
		   (when (< (aref alpha-array i) 0.0d0)
		     (setf (aref alpha-array i) 0.0d0))
		   
		   (setf (aref alpha-array j) (- sum (aref alpha-array i)))
                   
		   (when (> (aref alpha-array j) 1.0d0)
		     (setf (aref alpha-array j) 1.0d0))
                   
		   (when (< (aref alpha-array j) 0.0d0)
		     (setf (aref alpha-array j) 0.0d0))
		   
		   (setf (aref alpha-array i) (- sum (aref alpha-array j)))
		   
		   (update-gradient training-vector kernel-function i j old-a-i old-a-j))))))))


(defun update-gradient (training-vector kernel-function i j old-a-i old-a-j)
  "for one-class-svm"
  (declare (type simple-vector training-vector)
	   (type function kernel-function)
	   (type double-float old-a-i old-a-j))
  
  (let* ((alpha-array *alpha-array*)
	 (gradient-array *gradient-array*)
	 (training-size *training-size*)
	 (delta-a-i (- (aref alpha-array i) old-a-i))
	 (delta-a-j (- (aref alpha-array j) old-a-j)))
                     
    (declare (type fixnum i j training-size)
	     (type (simple-array double-float (*)) alpha-array gradient-array)
	     (type double-float delta-a-i delta-a-j))
    (loop
	for k of-type fixnum below training-size
	with point-i of-type (simple-array double-float (*)) = (svref training-vector i)
	with point-j of-type (simple-array double-float (*)) = (svref training-vector j)
	as point-k of-type (simple-array double-float (*)) = (svref training-vector k)
	do (incf (the double-float (aref gradient-array k))
		 (+ (* (call-kernel-function kernel-function point-k point-i) delta-a-i)
		    (* (call-kernel-function kernel-function point-k point-j) delta-a-j))))))


(defun working-set-selection3 (training-vector kernel-function)
  "for one-class-svm"
  (declare (type simple-vector training-vector)
	   (type function kernel-function))
  
  (let ((i -1)
	(j -1)
	(eps *eps*))
    
    (declare (type fixnum i j)
	     (type double-float eps))
    
    (let ((g-max most-negative-double-float)
	  (g-min most-positive-double-float))
      
      (declare (type double-float g-max g-min))
      
      (multiple-value-setq (i g-max) (select-i))
      
      (multiple-value-setq (j g-min) (select-j training-vector kernel-function i g-max))
      
      (when (< (- g-max g-min) eps)
        (return-from working-set-selection3 (values -1 -1)))
      
      (values i j))))


(defun select-i ()
  "for one-class-svm"
  (let ((training-size *training-size*)
	(alpha-array *alpha-array*)
	(gradient-array *gradient-array*)
	(i -1)
	(g-max most-negative-double-float))
    
    (declare (type fixnum i training-size)
	     (type (simple-array double-float (*)) alpha-array gradient-array)
	     (type double-float g-max))
	
    (loop
        for k of-type fixnum below training-size
        as a-k of-type double-float = (aref alpha-array k)
        as g-k of-type double-float = (aref gradient-array k)
        as g-temp of-type double-float = (- g-k)
	if (< a-k 1.0)
	do (when (>= g-temp g-max)
	     (setf i k)
	     (setf g-max g-temp))
	finally (return (values i g-max)))))


(defun select-j (training-vector kernel-function i g-max)
  "for one-class-svm"
  (declare (type simple-vector training-vector)
	   (type function kernel-function))
  
  (let ((training-size *training-size*)
	(alpha-array *alpha-array*)
	(gradient-array *gradient-array*)
	(tau *tau*)
	(j -1)
	(g-min most-positive-double-float)
	(obj-min most-positive-double-float))
    
    (declare (type fixnum i j training-size)
	     (type (simple-array double-float (*)) alpha-array gradient-array)
	     (type double-float tau g-min g-max obj-min))
    
    (loop
        for k of-type fixnum below training-size
        as a-k of-type double-float = (aref alpha-array k)
        as g-k of-type double-float = (aref gradient-array k)
        as g-temp of-type double-float = (- g-k)
        with a of-type double-float = 0.0d0
        with b of-type double-float  = 0.0d0
        if (> a-k 0.0d0)
	do (setf b (- g-max g-temp))
	   (when (<= g-temp g-min)
	     (setf g-min g-temp))
	   (when (> b 0.0d0)
	     (setf a (the double-float (eta training-vector kernel-function i k)))
	     (when (<= a 0.0d0)
	       (setf a tau))
	     (let ((temp (/ (- (* b b)) a)))
	       (declare (type double-float temp))
	       (when (<= temp obj-min)
		 (setf j k)
		 (setf obj-min temp))))
	finally (return (values j g-min)))))


#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (get 'eta 'sys::immed-args-call)
    '((:lisp :lisp :lisp :lisp) double-float)))
(declaim (inline eta))

(defun eta (training-vector kernel-function i j)
  "for one-class-svm"
  (declare (type simple-vector training-vector)
	   (type function kernel-function)
  	   (type fixnum i j)
           (ignorable kernel-function training-vector))
  
  (let ((point-i (svref training-vector i))
	(point-j (svref training-vector j)))
    
    (declare (type (simple-array double-float (*)) point-i point-j))
    
    (+ (call-kernel-function kernel-function point-i point-i)
       (call-kernel-function kernel-function point-j point-j)
       (* -2.0d0 (call-kernel-function kernel-function point-i point-j)))))

;;for check
(defun print-rho (training-vector kernel-function alpha-array)
  
  (loop
      for i below (length alpha-array)
      as a-i = (aref alpha-array i)
      as point-i = (svref training-vector i)
      if (< 0.0d0 a-i 1.0d0)
      do (print (loop
		    for j below (length alpha-array)
		    as a-j = (aref alpha-array j)
		    as point-j = (svref training-vector j)
		    unless (= 0.0d0 a-j)
		    sum (* a-j
			   (call-kernel-function kernel-function point-i point-j))))))


(defun compute-rho (training-vector kernel-function alpha-array)
  (declare (type simple-vector training-vector)
	   (type function kernel-function)
	   (type dvec alpha-array)
           (ignorable kernel-function))
  (/ (loop
	 for i of-type fixnum below (length alpha-array)
	 as a-i of-type double-float = (aref alpha-array i)
	 as point-i of-type (simple-array double-float (*)) = (svref training-vector i)
	 if (< 0.0d0 a-i 1.0d0)
	 sum (loop
		 for j of-type fixnum below (length alpha-array)
		 as a-j of-type double-float = (aref alpha-array j)
		 as point-j of-type (simple-array double-float (*)) = (svref training-vector j)
		 unless (= 0.0d0 a-j)
		 sum (* a-j
			(call-kernel-function kernel-function point-i point-j))))
     
     (loop
	 for alpha of-type double-float across alpha-array count (< 0.0d0 alpha 1.0d0))))


(defun make-discriminant-function (training-vector kernel-function alpha-array rho)
  "for one-class-svm"
  (declare (type simple-vector training-vector)
	   (type function kernel-function)
	   (type dvec alpha-array)
	   (type double-float rho)
           (ignorable kernel-function))
  
  (lambda (point)
    (sign (- (let ((result 0.0d0))
	       (declare (type double-float result))  
	       (loop 
                   for i of-type fixnum below (length alpha-array)
                   as a-i of-type double-float = (aref alpha-array i)
                   unless (= 0.0d0 a-i)
		   do (incf result
			    (* a-i
			       (call-kernel-function-uncached kernel-function (svref training-vector i) point))))
	       result)
	     rho))))


(defun one-class-svm (data-vector &key nu gamma)
  (assert (and (< 0.0d0 nu 1.0d0) (plusp gamma)))
  (let* ((nu (coerce nu 'double-float))
	 (gamma (coerce gamma 'double-float))
	 (kernel-function (make-one-class-svm-kernel :gamma gamma))
	 (alpha-array (qp-solver data-vector kernel-function nu))
	 (rho (compute-rho data-vector kernel-function alpha-array)))
    
    (make-discriminant-function data-vector kernel-function alpha-array rho)))
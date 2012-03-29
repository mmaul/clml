;;;Support Vector Regression Package using SMO-type algorithm
;;;Abe Yusuke, Jianshi Huang. 2010 May
;;;Reference: "A Study on SMO-type Decomposition Methods for Support Vector Machines"
;;;Pai-Hsuen Chen, Rong-En Fan, and Chih-Jen Lin


(defpackage :svr
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

(in-package svr)

;; (declaim (optimize speed (safety 0) (debug 1)))

(defparameter *eps* 1d-3)
(defparameter *tau* 1d-12)
(defparameter *training-size* 0)
(defparameter *target-index* 0)
(defparameter *alpha-array* (make-array 0 :element-type 'double-float))
(defparameter *gradient-array* (make-array 0 :element-type 'double-float))
(defparameter *kernel-function-result* (make-array 1 :element-type 'double-float :initial-element 0d0))


(declaim (type double-float *eps* *tau*)
         (type fixnum *training-size* *target-index*)
         (type dvec *alpha-array* *gradient-array*)
         (type (simple-array double-float (1)) *kernel-function-result*))


(defun qp-solver (training-vector kernel-function c epsilon)
  "for svr"
  (declare (type simple-vector training-vector)
	   (type function kernel-function)
	   (type double-float c epsilon))
  
  (setf *training-size* (length training-vector))
  (setf *target-index* (1- (length (aref training-vector 0))))
  (setf *alpha-array* (make-array (* 2 *training-size*) :element-type 'double-float :initial-element 0.0d0))
  (setf *gradient-array* (make-array (* 2 *training-size*) :element-type 'double-float :initial-element 0.0d0))
  
  (let ((tau *tau*)
	(training-size *training-size*)
	(target-index *target-index*)
	(alpha-array *alpha-array*)
	(gradient-array *gradient-array*))
    
    (declare (type double-float tau)
	     (type fixnum training-size target-index)
	     (type (simple-array double-float (*)) alpha-array gradient-array)
	     (ignorable training-size))
    
    ;;initialize gradient-array
    (loop
	for i of-type fixnum below training-size
	as z-i of-type double-float = (aref (the (simple-array double-float (*)) (svref training-vector i)) target-index)
	do (setf (aref gradient-array i) (- epsilon z-i)))
    
    (loop
	for i of-type fixnum from training-size below (* 2 training-size)
	as z-i of-type double-float = (aref (the (simple-array double-float (*)) (svref training-vector (mod i training-size))) target-index)
	do (setf (aref gradient-array i) (+ epsilon z-i)))
    
    (loop
	while t
	do (multiple-value-bind (i j) 
	       (working-set-selection3 training-vector kernel-function c)
	     (declare (type fixnum i j))
	     (when (= -1 j)
	      (return-from qp-solver *alpha-array*))
	    
	     (let ((y-i (label i))
		   (y-j (label j)))
             
	       (declare (type double-float y-i y-j))
             
	       (let ((a (eta training-vector kernel-function i j))
		     (b (- (* y-j (aref gradient-array j))
			   (* y-i (aref gradient-array i)))))
               
		 (declare (type double-float a b))

		 (when (<= a 0.0d0)
		   (setf a tau))
		 
		 ;;update alpha
		 (let ((old-a-i (aref alpha-array i))
		       (old-a-j (aref alpha-array j)))
                 
		   (declare (type double-float old-a-i old-a-j))
		   
		   (incf (aref alpha-array i) (/ (* y-i b) a))
		   (decf (aref alpha-array j) (/ (* y-j b) a))
		 
		   ;;clipping
		   (let ((sum (+ (* y-i old-a-i) (* y-j old-a-j))))
                   
		     (declare (type double-float sum))
                   
		     (when (> (aref alpha-array i) c)
		       (setf (aref alpha-array i) c))
                   
		     (when (< (aref alpha-array i) 0.0d0)
		       (setf (aref alpha-array i) 0.0d0))
		   
		     (setf (aref alpha-array j) (* y-j (- sum (* y-i (aref alpha-array i)))))
                   
		     (when (> (aref alpha-array j) c)
		       (setf (aref alpha-array j) c))
                   
		     (when (< (aref alpha-array j) 0.0d0)
		       (setf (aref alpha-array j) 0.0d0))
		     
		     (setf (aref alpha-array i) (* y-i (- sum (* y-j (aref alpha-array j)))))
		   
		     (update-gradient training-vector kernel-function i j old-a-i old-a-j)))))))))


(defun update-gradient (training-vector kernel-function i j old-a-i old-a-j)
  "for svr"
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
	for k of-type fixnum below (* 2 training-size)
	with point-i of-type (simple-array double-float (*)) = (svref training-vector (mod i training-size))
	with point-j of-type (simple-array double-float (*)) = (svref training-vector (mod j training-size))
	with y-i of-type double-float = (label i)
	with y-j of-type double-float = (label j)
	as point-k of-type (simple-array double-float (*)) = (svref training-vector (mod k training-size))
	as y-k of-type double-float = (label k)
	do (incf (the double-float (aref gradient-array k))
		 (+ (* y-k y-i (call-kernel-function kernel-function point-k point-i) delta-a-i)
		    (* y-k y-j (call-kernel-function kernel-function point-k point-j) delta-a-j))))))


(defun label (i)
  (let ((training-size *training-size*))
    (declare (type fixnum training-size i))
    
    (if (< i training-size)
	1.0d0
      -1.0d0)))


(defun working-set-selection3 (training-vector kernel-function c)
  "for svr"
  (declare (type simple-vector training-vector)
	   (type function kernel-function)
	   (type double-float c))
  
  (let ((i -1)
	(j -1)
	(eps *eps*))
    
    (declare (type fixnum i j)
	     (type double-float eps))
    
    (let ((g-max most-negative-double-float)
	  (g-min most-positive-double-float))
      
      (declare (type double-float g-max g-min))
      
      (multiple-value-setq (i g-max) (select-i c))
      
      (multiple-value-setq (j g-min) (select-j training-vector kernel-function c i g-max))
      
      (when (< (- g-max g-min) eps)
        (return-from working-set-selection3 (values -1 -1)))
      
      (values i j))))


(defun select-i (c)
  "for svr"
  (declare (type double-float c))
  
  (let ((training-size *training-size*)
	(alpha-array *alpha-array*)
	(gradient-array *gradient-array*)
	(i -1)
	(g-max most-negative-double-float))
    
    (declare (type fixnum i training-size)
	     (type (simple-array double-float (*)) alpha-array gradient-array)
	     (type double-float g-max))
	
    (loop
        for k of-type fixnum below (* 2 training-size)
        as y-k of-type double-float = (label k)
        as a-k of-type double-float = (aref alpha-array k)
        as g-k of-type double-float = (aref gradient-array k)
        as g-temp of-type double-float = (- (* y-k g-k))
	if (or (and (= y-k 1.0d0) (< a-k c))
	       (and (= y-k -1.0d0) (> a-k 0d0)))
	do (when (>= g-temp g-max)
	     (setf i k)
	     (setf g-max g-temp))
	finally (return (values i g-max)))))


(defun select-j (training-vector kernel-function c i g-max)
  "for svr"
  (declare (type simple-vector training-vector)
	   (type function kernel-function)
	   (type double-float c))
  
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
        for k of-type fixnum below (* 2 training-size)
        as y-k of-type double-float = (label k)
        as a-k of-type double-float = (aref alpha-array k)
        as g-k of-type double-float = (aref gradient-array k)
        as g-temp of-type double-float = (- (* y-k g-k))
        with a of-type double-float = 0.0d0	      
        with b of-type double-float  = 0.0d0
        if (or (and (= y-k 1.0d0) (> a-k 0.0d0))
               (and (= y-k -1.0d0) (< a-k c)))
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
  "for svr"
  (declare (type simple-vector training-vector)
	   (type function kernel-function)
  	   (type fixnum i j)
           (ignorable kernel-function training-vector))
  
  (let* ((training-size *training-size*)
	 (point-i (svref training-vector (mod i training-size)))
	 (point-j (svref training-vector (mod j training-size))))
    
    (declare (type (simple-array double-float (*)) point-i point-j))
    
    (+ (call-kernel-function kernel-function point-i point-i)
       (call-kernel-function kernel-function point-j point-j)
       (* -2.0d0 (call-kernel-function kernel-function point-i point-j)))))


;;for check
(defun print-b (training-vector kernel-function c epsilon alpha-array)
  "for svr"
  (declare (ignorable kernel-function))
  (let ((training-size (length training-vector))
	(target-index (1- (length (aref training-vector 0)))))
    (loop
	for i below training-size
	as point-i = (svref training-vector i)
	as z-i = (aref point-i target-index)
	if (< 0.0d0 (aref alpha-array i) c)
	do (print (- z-i
		     epsilon
		     (loop 
			 for j below training-size
			 as a-j = (- (aref alpha-array j) (aref alpha-array (+ j training-size)))
			 as point-j = (svref training-vector j)
			 sum (* a-j
				(call-kernel-function kernel-function point-i point-j))))))

    (loop
	for i below training-size
	as point-i = (svref training-vector i)
	as z-i = (aref point-i target-index)
	if (< 0.0d0 (aref alpha-array (+ i training-size)) c)
	do (print (- (+ z-i epsilon)
		     (loop 
			 for j below training-size
			 as a-j = (- (aref alpha-array j) (aref alpha-array (+ j training-size)))
			 as point-j = (svref training-vector j)
			 sum (* a-j
				(call-kernel-function kernel-function point-i point-j))))))))


(defun compute-b (training-vector kernel-function c epsilon alpha-array)
  "for svr"
  (declare (type simple-vector training-vector)
	   (type function kernel-function)
	   (type (simple-array double-float (*)) alpha-array)
	   (type double-float c epsilon)
           (ignorable kernel-function))

  (let ((training-size (length training-vector))
	(target-index (1- (length (aref training-vector 0)))))
    
    (/ (+ (loop
	      for i of-type fixnum below training-size
	      as point-i of-type (simple-array double-float (*)) = (svref training-vector i)
	      as z-i of-type double-float = (aref point-i target-index)
	      if (< 0.0d0 (aref alpha-array i) c)
	      sum (- z-i
		     epsilon
		     (loop 
			 for j of-type fixnum below training-size
			 as a-j of-type double-float = (- (aref alpha-array j) (aref alpha-array (+ j training-size)))
			 as point-j of-type (simple-array double-float (*)) = (svref training-vector j)
			 sum (* a-j
				(call-kernel-function kernel-function point-i point-j)))))

	  (loop
	      for i of-type fixnum below training-size
	      as point-i of-type (simple-array double-float (*)) = (svref training-vector i)
	      as z-i of-type double-float = (aref point-i target-index)
	      if (< 0.0d0 (aref alpha-array (+ i training-size)) c)
	      sum (- (+ z-i epsilon)
		     (loop 
			 for j of-type fixnum below training-size
			 as a-j of-type double-float = (- (aref alpha-array j) (aref alpha-array (+ j training-size)))
			 as point-j of-type (simple-array double-float (*)) = (svref training-vector j)
			 sum (* a-j
				(call-kernel-function kernel-function point-i point-j))))))
       
       (loop for a of-type double-float across alpha-array count (< 0.0d0 a c)))))


(defun make-regression-function (training-vector kernel-function alpha-array b)
  (declare (type simple-vector training-vector)
	   (type function kernel-function)
	   (type (simple-array double-float (*)) alpha-array)
	   (type double-float b)
           (ignorable kernel-function))
  
  (let ((training-size (length training-vector)))
    
    (declare (type fixnum training-size))
    
    (lambda (point)
      (+ (let ((result 0.0d0))
	   (declare (type double-float result))  
	   (loop 
	       for i of-type fixnum below training-size
	       as a-i of-type double-float = (- (aref alpha-array i) (aref alpha-array (+ i training-size)))
	       unless (= 0.0d0 a-i)
	       do (incf result
			(* a-i
			   (call-kernel-function-uncached kernel-function (svref training-vector i) point))))
	   result)
	 b))))


(defun make-svr-learner (training-vector kernel-function &key c epsilon file-name external-format)
  (assert (and (plusp c) (plusp epsilon)))
  (let* ((cc (coerce c 'double-float))
	 (alpha-array (qp-solver training-vector kernel-function cc epsilon))
	 (b (compute-b training-vector kernel-function cc epsilon alpha-array)))
    (when (and file-name external-format)
      (with-open-file (out file-name
		       :external-format external-format
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
	(write (list training-vector alpha-array b) :stream out)))
    (make-regression-function training-vector kernel-function alpha-array b)))


(defun load-svr-learner (file-name kernel-function &key external-format)
  (let* ((material-list 
	  (with-open-file (in file-name :external-format external-format :direction :input)
	    (read in)))
	 (training-vector (first material-list))
	 (alpha-array (specialize-vec (second material-list)))
	 (b (third material-list)))
    
    (loop 
	for i of-type fixnum below (length training-vector)
	do (setf (aref training-vector i) (specialize-vec (aref training-vector i))))
     
    (make-regression-function training-vector kernel-function alpha-array b)))


(defun svr-validation (svr-learner test-vector)
  "Mean Squared Error"
  (loop
      for test-sample of-type (simple-array double-float (*)) across test-vector
      with target-index of-type fixnum = (1- (length (svref test-vector 0)))
      sum (expt (- (funcall svr-learner test-sample) 
		   (aref test-sample target-index))
		2) into s
      finally (return (/ s (length test-vector)))))
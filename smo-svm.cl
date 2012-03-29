;;;Support Vector Machine Package using SMO algorithm
;;;Abe Yusuke, Jianshi Huang. 2010 Feburuary
;;;Reference: Jhon C. Platt. "Fast Training of Support Vector Machines using Sequential Minimal Optimization"


(defpackage :svm.smo
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
(in-package svm.smo)

;;; (declaim (optimize speed (safety 0) (debug 1) (compilation-speed 0)))
;;; (declaim (optimize (speed 0) safety debug (compilation-speed 0)))

(defparameter *training-vector* #())
(defparameter *kernel-function* (constantly 0))
(defparameter *c* 0.0d0)		;slack variable
(defparameter *training-size* 0)
(defparameter *label-index* 0)
(defparameter *epsilon* 0.001d0)	;torelance of KKT-condition check
(defparameter *alpha-array* (make-dvec 0))	;solution of QP problem induced by SVM
(defparameter *error-array* (make-dvec 0))	;see Platt's paper
(defparameter *b* 0.0d0)		;threshold
(defparameter *kernel-function-result* (make-array 1 :element-type 'double-float :initial-element 0d0))
(defparameter *kernel-function-result-cache* (make-array (list *training-size* *training-size*)
                                                         :element-type 'double-float
                                                         :initial-element most-negative-double-float))

(declaim (type double-float *c* *epsilon* *b*)
         (type array-index *training-size* *label-index*)
         (type dvec *alpha-array* *error-array* *kernel-function-result*)
         (type dmat *kernel-function-result-cache*)
         (type simple-vector *training-vector*)
         (type function *kernel-function*))

(defmacro with-common-local-bindings (&body body)
  "Create local bindings for global special variables that are either
constant values(won't change), arrays or structures(destructively
updated)."
  `(let ((training-vector *training-vector*)
         (kernel-function *kernel-function*)
         (c *c*)
         (training-size *training-size*)
         (label-index *label-index*)
         (epsilon *epsilon*)
         (alpha-array *alpha-array*)
         (error-array *error-array*)
         ;; *b* will be changed
         (kernel-function-result *kernel-function-result*)
         (kernel-function-result-cache *kernel-function-result-cache*) 
         )
     (declare (type double-float c epsilon)
              (type fixnum training-size)
              (type array-index label-index)
              (type dvec alpha-array error-array kernel-function-result)
              (type dmat kernel-function-result-cache)
              (type simple-vector training-vector)
              (type function kernel-function) 
              (ignorable training-vector kernel-function c training-size
                         label-index epsilon alpha-array error-array
                         kernel-function-result kernel-function-result-cache))
     (locally
         ,@body)))

(defmacro call-kernel-function-with-indices (kernel-function i1 i2)
  `(let ((training-vector *training-vector*)
         (kernel-function-result *kernel-function-result*)
         (kernel-function-result-cache *kernel-function-result-cache*))
     (declare (type dmat kernel-function-result-cache)
              (type dvec kernel-function-result)
              (type simple-vector training-vector)
              (type array-index ,i1 ,i2))
     (multiple-value-bind (x y)
         (if (> ,i1 ,i2) (values ,i1 ,i2) (values ,i2 ,i1))
       (let ((cached-value (aref kernel-function-result-cache x y)))
         (declare (type double-float cached-value))
         (if (= cached-value most-negative-double-float)
             (progn 
               (funcall ,kernel-function (aref training-vector x) (aref training-vector y))
               (the double-float
                 (setf (aref kernel-function-result-cache x y)
                       (aref kernel-function-result 0))))
             (progn
               (the double-float
                 (aref kernel-function-result-cache x y))))))))

(defmacro call-kernel-function-with-vectors (kernel-function point1 point2)
  `(progn
     (funcall ,kernel-function ,point1 ,point2)
     (the double-float
       (aref *kernel-function-result* 0))))

(defun smo-solver (training-vector kernel-function c)
  (declare (type simple-vector training-vector)
           (function kernel-function))
  (check-type training-vector simple-vector)
  (check-type (aref training-vector 0) dvec)
  (check-type kernel-function function) 
  (setf *training-vector* training-vector)
  (setf *kernel-function* kernel-function)
  (setf *c* (coerce c 'double-float))
  (setf *training-size* (length training-vector))
  (setf *label-index* (1- (length (svref training-vector 0))))
  (setf *alpha-array* (make-array *training-size* :element-type 'double-float :initial-element 0.0d0))
  (setf *error-array* (make-array *training-size* :element-type 'double-float :initial-element 0.0d0))
  (setf *b* 0.0d0)
  (setf *kernel-function-result-cache*
        (make-array (list *training-size* *training-size*)
                    :element-type 'double-float
                    :initial-element most-negative-double-float)) 
  
  ;;initialize error cache
  (with-common-local-bindings
    (loop 
      for i of-type array-index below training-size
      do (setf (aref error-array i)
               (- (aref (the dvec (aref training-vector i)) label-index))))
    
    (let ((number-changed 0)
          (examine-all t)
	  (upper-bound (- c epsilon)))
	  
      (declare (type fixnum number-changed)
               (type double-float upper-bound))
      
      (loop 
        while (or (> number-changed 0) examine-all)
        do (setf number-changed 0)
           (if examine-all
               (setf number-changed (loop for i of-type array-index below training-size
                                          sum (the fixnum (examine-example i))
                                            into result of-type fixnum
                                          finally (return result)))
               (setf number-changed (loop for i of-type array-index below training-size
                                          if (< epsilon (aref alpha-array i) upper-bound)
                                            sum (the fixnum (examine-example i))
                                              into result of-type fixnum
                                          finally (return result))))
           
           (if examine-all
               (setf examine-all nil)
               (when (= number-changed 0)
                 (setf examine-all t))))
      
      (values alpha-array *b*))))


(defun examine-example (i2)
  (declare (type array-index i2))
  (with-common-local-bindings
    (let* ((point2 (svref training-vector i2))
           (y2 (aref (the dvec point2) label-index))
           (alpha2 (aref alpha-array i2))
           (e2 (aref error-array i2))
	   (upper-bound (- c epsilon))
           (r2 (* (the double-float e2) (the double-float y2))))
      (declare (type dvec point2)
               (type double-float y2 alpha2 e2 r2 upper-bound))
      
      ;;KKT-condition check of alpha-2
      (when (or (and (< r2 (- epsilon))
                     (< alpha2 upper-bound))
                (and (> r2 epsilon)
                     (> alpha2 epsilon)))
        
        (let ((i1 0)
              (max 0.0d0))
          (declare (type array-index i1)
                   (type double-float max))
          (when (loop for alpha of-type double-float across alpha-array
                      thereis (< epsilon alpha upper-bound))
            (loop 
		for i of-type array-index below training-size
		as delta-error of-type double-float = (abs (- (aref error-array i) e2))
		if (< epsilon (aref alpha-array i) upper-bound)
                do (when (>= delta-error max)
                     (setf max delta-error)
                     (setf i1 i)))
	    
            (when (take-step i1 i2)
              (return-from examine-example 1))
            
            (let ((random-start (random training-size)))
              (declare (type array-index random-start))
              (loop
                for i of-type array-index from random-start below training-size
                if (< epsilon (aref alpha-array i) upper-bound)
                  do (when (take-step i i2)
                       (return-from examine-example 1)))
              
              (loop
                for i of-type array-index below random-start
                if (< epsilon (aref alpha-array i) upper-bound)
                  do (when (take-step i i2)
                       (return-from examine-example 1)))))
          
          (let ((random-start (random training-size)))
            (declare (type array-index random-start))
            (loop
              for i of-type array-index from random-start below training-size
              do (when (take-step i i2)
                   (return-from examine-example 1)))
            
            (loop
              for i of-type array-index below random-start
              do (when (take-step i i2)
                   (return-from examine-example 1)))))
        0)))
  0)



;; #+allegro
;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (setf (get 'f-old 'sys::immed-args-call)
;; 	'((:lisp :lisp :lisp :lisp :lisp) double-float)))
(declaim (inline f-old))
(defun f-old (training-vector kernel-function alpha-array b index)
  (declare (type dvec alpha-array)
           (type simple-vector training-vector)
           (type function kernel-function)
           (type double-float b)
           (type array-index index))
  (- (let ((result 0d0))
       (declare (type double-float result))
       (loop 
         for i of-type array-index below (length alpha-array)
         do (incf result
                  (* (aref alpha-array i)
                     (aref (the dvec (svref training-vector i))
                           (1- (length (svref training-vector 0))))
                     (the double-float (call-kernel-function-with-indices kernel-function i index)))))
       result)
     b))


#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (get 'compute-v 'sys::immed-args-call)
	'((:lisp :lisp :lisp :lisp :lisp :lisp :lisp) double-float)))
(declaim (inline compute-v))
(defun compute-v (training-vector kernel-function alpha-array label-index i1 i2 index)
  (declare (type simple-vector training-vector)
           (type function kernel-function) 
           (type array-index label-index i1 i2 index))
  (let ((result 0d0))
    (declare (type double-float result))
    (loop 
      for i of-type array-index below (length alpha-array)
      if (and (/= i i1) (/= i i2))
        do (incf result
                 (* (aref (the dvec (svref training-vector i)) label-index)
                          (aref alpha-array i)
                          (the double-float (call-kernel-function-with-indices kernel-function index i)))))
    result))


#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (get 'compute-w-const 'sys::immed-args-call)
	'((:lisp :lisp :lisp :lisp :lisp) double-float)))
(declaim (inline compute-w-const))
(defun compute-w-const (training-vector alpha-array label-index i1 i2)
  (declare (type dvec alpha-array)
           (type simple-vector training-vector) 
           (type array-index label-index i1 i2))
  (let ((n (length alpha-array)))
    (declare (type fixnum n))
    (- (let ((result 0d0))
         (declare (type double-float result))
         (loop
           for i of-type array-index below n
           if (and (/= i1 i) (/= i2 i))
             do (incf result (aref alpha-array i)))
         result)
       (* 0.5d0
          (let ((result 0d0))
            (declare (type double-float result))
            (loop
              for i of-type array-index below n
              if (and (/= i1 i) (/= i2 i))
                do (incf result
                         (let ((result 0d0))
                           (declare (type double-float result))
                           (loop
                             for j of-type array-index below n
                             if (and (/= i1 j) (/= i2 j))
                               do (incf result
                                        (* (aref (the dvec (svref training-vector i)) label-index)
                                                 (aref (the dvec (svref training-vector j)) label-index)
                                                 (aref alpha-array i)
                                                 (aref alpha-array j))))
                           result)))
            result)))))


#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (get 'compute-obj-fun 'sys::immed-args-call)
	`(,(make-list 12 :initial-element 'double-float) double-float)))
(declaim (inline compute-obj-fun))
(defun compute-obj-fun (alpha1-old alpha2-old y1 y2 s  k11 k12 k22 v1 v2 w-const alpha)
  (declare (type double-float alpha1-old alpha2-old y1 y2 s  k11 k12 k22 v1 v2 w-const alpha))
  (let ((r 0.0d0))
    (declare (type double-float r))
    (if (= y1 y2)
	(setf r (+ alpha1-old alpha2-old))
        (setf r (- alpha1-old alpha2-old)))
    
    (- r 
       (* s alpha)
       (- alpha)
       (* 0.5 k11 (the double-float (expt (- r (* s alpha)) 2)))
       (* 0.5 k22 (the double-float (expt alpha 2)))
       (* s k12 (- r (* s alpha)) alpha)
       (* y1 (- r (* s alpha)) v1)
       (* y2 alpha v2)
       (- w-const))))



(defun take-step (i1 i2)
  (declare (type array-index i1 i2))
  (when (= i1 i2)
    (return-from take-step nil))
  (with-common-local-bindings
    (let* ((point1 (svref training-vector i1))
           (point2 (svref training-vector i2))
           (alpha1-old (aref alpha-array i1))
           (alpha2-old (aref alpha-array i2))
           (y1 (aref (the dvec point1) label-index))
           (y2 (aref (the dvec point2) label-index))
           (s (* (the double-float y1) (the double-float y2)))
           (alpha1-new 0.0d0)
           (alpha2-new 0.0d0)
           (e1 (aref error-array i1))
           (e2 (aref error-array i2))
           (clipped nil)
           (l 0.0d0)
           (h 0.0d0)) 
      (declare (type dvec point1 point2)
               (type double-float alpha1-old alpha2-old y1 y2 s alpha1-new alpha2-new e1 e2 l h))
      
      (if (= y1 y2)
          (setf l (max 0.0d0 (- (+ alpha1-old alpha2-old) c))
                h (min c (+ alpha1-old alpha2-old)))
          (setf l (max 0.0d0  (- alpha2-old alpha1-old))
                h (min c (+ c (- alpha1-old) alpha2-old))))
      
      (when (= l h)
        (return-from take-step nil))
      
      (let* ((k11 (the double-float (call-kernel-function-with-indices kernel-function i1 i1)))
             (k12 (the double-float (call-kernel-function-with-indices kernel-function i1 i2)))
             (k22 (the double-float (call-kernel-function-with-indices kernel-function i2 i2)))
             (eta (- (* 2.0d0 k12) k11 k22)))
        (declare (type double-float k11 k12 k22 eta))
        
        (if (< eta 0.0d0)
            (progn
              (setf alpha2-new (- alpha2-old
                                  (/ (* y2 (- e1 e2))
                                     eta)))
              ;;clipping
              (when (<= alpha2-new l)
                (setf alpha2-new l)
                (setf clipped t))
              
              (when (>= alpha2-new h)
                (setf alpha2-new h)
                (setf clipped t)))
            
            ;;eta:not negative case
            (let* ((v1 (compute-v training-vector kernel-function alpha-array label-index i1 i2 i1))	       
                   (v2 (compute-v training-vector kernel-function alpha-array label-index i1 i2 i2)))
              (declare (type double-float v1 v2))
              (let ((w-const (compute-w-const training-vector alpha-array label-index i1 i2)))
                (declare (type double-float w-const))
                (let* ((l-obj (compute-obj-fun alpha1-old alpha2-old y1 y2 s k11 k12 k22 v1 v2 w-const l))
                       (h-obj (compute-obj-fun alpha1-old alpha2-old y1 y2 s k11 k12 k22 v1 v2 w-const h)))
                  (declare (type double-float l-obj h-obj))
                  
                  (cond  ((> l-obj (+ h-obj epsilon))
                          (setf alpha2-new l)
                          (setf clipped t))
                        
                        ((< l-obj (- h-obj epsilon))
                         (setf alpha2-new h)
                         (setf clipped t))
                        
                        (t
                         (setf alpha2-new alpha2-old)))))))

        (when (< (abs (- alpha2-new alpha2-old)) (* epsilon (+ alpha2-new alpha2-old epsilon)))
          (return-from take-step nil))
        
        (setf alpha1-new (+ alpha1-old (* s (- alpha2-old alpha2-new))))

        ;;update the threshold b
        (let* ((b1 (+ e1 
                      *b*
                      (* y1 (- alpha1-new alpha1-old) k11)
                      (* y2 (- alpha2-new alpha2-old) k12)))
               
               (b2 (+ e2 
                      *b*
                      (* y1 (- alpha1-new alpha1-old) k12)
                      (* y2 (- alpha2-new alpha2-old) k22)))
               
               (b-old *b*))
          (declare (type double-float b1 b2 b-old))
          
          (cond ((< epsilon alpha1-new (- c epsilon))
                 (setf *b* b1))
                
                ((< epsilon alpha2-new (- c epsilon))
                 (setf *b* b2))
                
                (t
                 (* 0.5d0 (+ b1 b2))))
          
          ;;update the error cache using new Lagrange multipliers
          (loop 
            for i of-type array-index below training-size
            do (setf (aref error-array i) 
                     (+ (aref error-array i)
                        (* y1
                           (- alpha1-new alpha1-old)
                           (the double-float (call-kernel-function-with-indices kernel-function i1 i)))
                        (* y2
                           (- alpha2-new alpha2-old)
                           (the double-float (call-kernel-function-with-indices kernel-function i2 i)))
                        (- b-old *b*))))
          
          (setf (aref error-array i2) 0.0d0)
          
          (when clipped
            (setf (aref error-array i1) (- (the double-float
                                             (f-old training-vector kernel-function alpha-array *b* i1))
                                           y1))
            (setf (aref error-array i2) (- (the double-float
                                             (f-old training-vector kernel-function alpha-array *b* i2))
                                           y2)))
          
          ;;update the alpha array
          (setf (aref alpha-array i1) alpha1-new)
          (setf (aref alpha-array i2) alpha2-new)
          t)))))


(defun make-discriminant-function (training-vector kernel-function alpha-array b)
  (declare (type dvec alpha-array)
           (type simple-vector training-vector)
           (type function kernel-function)
           (type double-float b))
  (check-type training-vector simple-vector)
  (check-type (aref training-vector 0) dvec)
  (check-type kernel-function function)
  (let ((label-index (1- (length (svref training-vector 0)))))
    (declare (type array-index label-index))
    ;; Allegro 8.1 can't optimize the following LOOP
    ;; (loop 
    ;;     for i of-type array-index below (length alpha-array)
    ;;     summing
    ;;       (* (aref alpha-array i) 
    ;;          (aref (the dvec (svref training-vector i)) label-index)
    ;;          (the double-float (call-kernel-function kernel-function (svref training-vector i) point))))
    ;; So we need to use ugly LET
    (lambda (point)
      (sign (- (let ((result 0d0))
                 (declare (type double-float result))
                 (loop 
                   for i of-type array-index below (length alpha-array)
                   do (incf result
                            (* (aref alpha-array i) 
                               (aref (the dvec (svref training-vector i)) label-index)
                               (the double-float (call-kernel-function-with-vectors kernel-function (svref training-vector i) point)))))
                 result)
               b)))))


(defun svm-output (training-vector kernel-function alpha-array b)
  (declare (type dvec alpha-array)
           (type simple-vector training-vector)
           (type function kernel-function)
           (type double-float b))
  (check-type training-vector simple-vector)
  (check-type (aref training-vector 0) dvec)
  (check-type kernel-function function)
  (let ((label-index (1- (length (svref training-vector 0)))))
    (declare (type array-index label-index))
    (lambda (point)
      ;; Allegro 8.1 can't optimize the following LOOP
      ;; (loop 
      ;;     for i of-type array-index below (length alpha-array)
      ;;     summing
      ;;       (* (aref alpha-array i) 
      ;;          (aref (the dvec (svref training-vector i)) label-index)
      ;;          (the double-float (call-kernel-function kernel-function (svref training-vector i) point))))
      ;; So we need to use ugly LET
      (- (let ((result 0d0))
	   (declare (type double-float result))
	   (loop 
	       for i of-type array-index below (length alpha-array)
	       do (incf result
			(* (aref alpha-array i) 
			   (aref (the dvec (svref training-vector i)) label-index)
			   (the double-float (call-kernel-function-with-vectors kernel-function (svref training-vector i) point)))))
	   result)
	 b))))


(defun sign (x)
  (declare (type double-float x))
  (if (>= x 0.0d0)
      1.0d0
    -1.0d0)) 

(defun linear-kernel (z-i z-j)
  "z-i =(x-i, y-i), x-i:input vector, y-i:label (+1 or -1)"
  (declare (type dvec z-i z-j))
  (loop
    for k of-type array-index below (1- (length z-i))
    sum (* (aref z-i k) (aref z-j k))
      into result of-type double-float
    finally (setf (aref *kernel-function-result* 0) result))
  nil)


(defun make-rbf-kernel (&key gamma)
  (assert (> gamma 0.0d0))
  (lambda (z-i z-j)
    (declare (type dvec z-i z-j)
             (type double-float gamma))
    (loop
      for k of-type array-index below (1- (length z-i))
      sum (expt (- (aref z-i k) (aref z-j k)) 2)
        into result of-type double-float
      finally (setf (aref *kernel-function-result* 0)
                    (d-exp (* (- gamma) result)))) 
    nil))


(defun make-polynomial-kernel (&key gamma r d)
  (declare (type double-float gamma r))
  (assert (> gamma 0.0d0))
  (assert (and (integerp d) (> d 0)))
  (let ((dd (coerce d 'double-float)))
    (lambda (z-i z-j)
      (declare (type dvec z-i z-j))
      (setf (aref *kernel-function-result* 0)
            (d-expt (+ (* gamma (call-kernel-function-with-vectors #'linear-kernel z-i z-j)) r) dd))
      nil)))


(defun make-svm-learner (training-vector kernel-function c)
  (multiple-value-bind (alpha-array b) (smo-solver training-vector kernel-function c)
    (make-discriminant-function training-vector kernel-function alpha-array b)))


(defun make-svm-learner2 (training-vector kernel-function c &key (file-name "./ml/svm-dump.sexp"))
  (multiple-value-bind (alpha-array b) (smo-solver training-vector kernel-function c)
    (with-open-file (out file-name
		     :external-format :utf-8
		     :direction :output
		     :if-exists :supersede
		     :if-does-not-exist :create)
      (write (list training-vector alpha-array b) :stream out))
    (svm-output training-vector kernel-function alpha-array b)))

(defun svm-validation (svm-learner test-vector)
  (check-type test-vector simple-vector)
  (check-type (aref test-vector 0) dvec)
  (let ((n (length test-vector))
	(label-index (1- (length (svref test-vector 0)))))
    (declare (type fixnum n)
             (type array-index label-index))
    (sum-up (loop for i of-type array-index below n
                  collect (cons (funcall svm-learner (svref test-vector i))
                                (aref (the dvec (svref test-vector i)) label-index))))))


(defun load-svm-learner (file-name kernel-function)
  (let* ((material-list 
	  (with-open-file (in file-name :external-format :utf-8 :direction :input)
	    (read in)))
	 (training-vector (first material-list))
	 (alpha-array (specialize-vec (second material-list)))
	 (b (third material-list)))
    
    (loop 
	for i below (length training-vector)
	do (setf (aref training-vector i) (specialize-vec (aref training-vector i))))
     
    (svm-output training-vector kernel-function alpha-array b)))

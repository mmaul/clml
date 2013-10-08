(defpackage :nbayes
  (:use :cl
	:hjs.learn.read-data)
  (:export
   :mbnb-learn
   :make-mbnb-learner
   :mnb-learn
   :make-mnb-learner)
  )

(in-package nbayes)


;;to prevent underflow
(defun exp% (x)
  (if (< -700.0d0 x)
      (exp x)
    0.0d0))


;;Multivariate Bernoulli Naive Bayes 
;;alpha is a smoothing parameter
;;we assume that final colum is the class label
(defun mbnb-learn (training-vector &key (alpha 1.0d0));;when alpha = 0.0, it occurs log 0.0 error
  (let* ((l (1- (length (aref training-vector 0))))
	 (classes (loop
		      for v across training-vector
		      collect (aref v l) into cs
		      finally (return (sort (remove-duplicates cs :test #'equal) #'string<))))
	 (k (length classes))
	 (n-wc (make-array k :initial-contents 
			   (loop
			       repeat k
			       collect (make-array l))))
	 (p-wc (make-array k :initial-contents 
			   (loop
			       repeat k
			       collect (make-array l :element-type 'double-float :initial-element 0.0d0))))
	 (n-c (make-array k)))
    
    (loop
	for i below k
	as category = (nth i classes)
	do (setf (aref n-c i)
	     (loop
		 for v across training-vector
		 as class = (aref v l)
		 if (string= category class)
		 count v)))
 
    (loop
	for i below k
	as category = (nth i classes)
	do (loop
	       for j below l
	       do (setf (aref (aref n-wc i) j)
		    (loop
			for v across training-vector
			as class = (aref v l)
			if (and (string= category class)
				(/= 0 (aref v j)))
			count v))))
   
    (loop
	for i below k
	do (loop
	       for j below l
	       do (setf (aref (aref p-wc i) j)
		    (/ (+ (aref (aref n-wc i) j) alpha)
		       (+ (aref n-c i) (* 2 alpha))))))
    (list p-wc classes)))


(defun make-mbnb-learner (p-wc classes)
  #'(lambda (fv)
      (let ((results
	     (loop
		 with n = (1- (length fv))
		 for i below (length classes)
		 collect (loop
			     for p across (aref p-wc i)
			     for j below n
			     as q = (if (= 0 (aref fv j))
					0.0d0
				      1.0d0)
			     sum (log (+ (* p q)
					 (* (- 1.0d0 p) (- 1.0d0 q))))))))
	(nth (position (loop for p in results maximize p) results) classes))))

;;Multinomial Naive Bayes
;;alpha is a smoothing parameter
;;we assume that final colum is the class label
(defun mnb-learn (training-vector &key (alpha 1.0d0));;when alpha = 0.0, it ocuurs log 0.0 error
  (let* ((l (1- (length (aref training-vector 0))))
	 (classes (loop
		      for v across training-vector
		      collect (aref v l) into cs
		      finally (return (sort (remove-duplicates cs :test #'equal) #'string<))))
	 (k (length classes))
	 (n-wc (make-array k :initial-contents 
			   (loop
			       repeat k
			       collect (make-array l))))
	 (q-wc (make-array k :initial-contents 
			   (loop
			       repeat k
			       collect (make-array l :element-type 'double-float :initial-element 0.0d0)))))
    
    (loop
	for i below k
	as category = (nth i classes)
	as n-wci = (aref n-wc i)
	do (loop
	       for j below l
	       do (setf (aref n-wci j)
		    (loop
			for v across training-vector
			as class = (aref v l)
			if (string= category class)
			sum (aref v j)))))
    
    (loop
	for i below k
	as q-wci = (aref q-wc i)
	as n-wci = (aref n-wc i)
	do (loop
	       for j below l
	       do (setf (aref q-wci j)
		    (/ (+ (aref n-wci j) alpha)
		       (+ (loop
			      for n across n-wci
			      sum n)
			  (* l alpha))))))
    
    (list q-wc classes)))


(defun make-mnb-learner (q-wc classes)
  #'(lambda (fv)
      (let ((results
	     (loop
		 for i below (length classes)
		 collect (loop
			     for q across (aref q-wc i)
			     for n-wd across fv
			     sum (* n-wd (log q))))))
	(nth (position (loop for p in results maximize p) results) classes))))


;;return the alist of validation results like '(((predict-class. true-class) n)..)
(defun learner-validation (learner test-vector)
  (let* ((n (length test-vector))
	 (l (1- (length (aref test-vector 0))))
	 (sum-up-list
	  (sum-up (loop
		      for i below n
		      collect (cons (funcall learner (aref test-vector i))
				    (aref (aref test-vector i) l))))))
    (values sum-up-list (accuracy sum-up-list))))


(defun accuracy (sum-up-list)
  (loop
      for obj in sum-up-list
      as category = (first obj)
      sum (cdr obj) into m
      if (equal (car category) (cdr category))
      sum (cdr obj) into n
      finally (return (* 100.0d0 (/ n m)))))


(defun sum-up (lst)
  (loop with alist
      for obj in lst
      as sub-alist = (assoc obj alist :test #'equal)
      do (if sub-alist
	     (incf (cdr sub-alist))
	   (push (cons obj 1) alist))
      finally (return alist)))
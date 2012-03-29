(defpackage :hc
  (:use :cl
	:hjs.learn.read-data
	:hjs.util.vector
	:hjs.util.matrix
        :hjs.util.meta)
  (:export #:cophenetic-matrix
	   #:cophenetic-cc
	   #:distance-matrix
	   #:hc-average
	   #:hc-ward
	   #:hc-single
	   #:hc-complete
	   #:hc-centroid
	   #:hc-median
	   #:cutree))

(in-package :hc)

(defun i-thvector (numeric-dataset i)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  ;; (declare (:explain :inlining))
  (assert (typep numeric-dataset 'numeric-dataset))
  (assert (<= 0 i (- (length (aref (dataset-numeric-points numeric-dataset) 0)) 1)))
  (assert (<= 3 (length (dataset-numeric-points numeric-dataset))))
  (let* ((data (dataset-numeric-points numeric-dataset))
	 (n (length data))
	 (x-i (make-array n :element-type 'double-float)))
    (declare (type dvec x-i)
             (type (simple-array dvec (*)) data))
    (dotimes (k n x-i)
      (setf (aref x-i k) (aref (aref data k) i)))))		   

(defun pick-up-row (matrix n)
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type dmat matrix)
           (type fixnum n))
  ;; (declare (:explain :inlining))
  (assert (< -1 n (array-dimension matrix 0)))
  (let* ((m (array-dimension matrix 1))
	 (v (make-array m :element-type 'double-float)))
    (declare (type dvec v))
    (dotimes (i m v)
      (setf (aref v i) (aref matrix n i)))))
		
(defun pick-up-column (matrix n)
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type dmat matrix)
           (type fixnum n))
  ;; (declare (:explain :inlining))
  (assert (< -1 n (array-dimension matrix 1)))
  (let* ((m (array-dimension matrix 0))
	 (v (make-array m :element-type 'double-float)))
    (declare (type dvec v))
    (dotimes (i m v)
      (setf (aref v i) (aref matrix i n)))))

#+ignore
(eval-when (:compile-toplevel)
  (setf (get 'vector-sum 'sys::immed-args-call)
    '((:lisp) double-float)))

(declaim (ftype (function (simple-array fixnum) double-float)
                max-vector min-vector vector-sum vector-mean))

(defun max-vector (v)
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type dvec v))
  (loop for var of-type double-float across v maximize var of-type double-float))

(defun min-vector (v)
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type dvec v))
  (loop for var of-type double-float across v minimize var of-type double-float))

(defun vector-sum (vector)
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type dvec vector))
  (loop for var of-type double-float across vector summing var of-type double-float))

(defun vector-mean (vector)
  (/ (vector-sum vector) (length vector)))

(defun vector-shift (v const)
  (let* ((n (length v))
  	 (w (make-array n)))
    (loop
	for i below n
	do (setf (aref w i) (- (aref v i) const)))
    w))

(defun product-sum (v1 v2)
  (loop
      for v-1 across v1
      for v-2 across v2
      sum (* v-1 v-2)))

(defun square-sum (v)
  (loop 
      for var across v
      sum (* var var)))

(defun numeric-matrix (numeric-dataset)
  (assert (typep numeric-dataset 'numeric-dataset))
  (let* ((data (dataset-numeric-points numeric-dataset))
	 (m (length data))
	 (n (length (aref data 0)))
	 (a (make-array (list m n) :element-type 'double-float)))
    (dotimes (i m a)
      (dotimes (j n)
	(setf (aref a i j)
	  (aref (i-thvector numeric-dataset j) i))))))


(defun mat-to-list (square-matrix)
  (assert (= (array-dimension square-matrix 0)
	     (array-dimension square-matrix 1)))
  (let* ((n (array-dimension square-matrix 0))
	 (memo '()))
    (dotimes (i n (nreverse memo))
      (dotimes (j i)
	(push (list (aref square-matrix i j) i j) memo)))))

(defun belong-to-merge-mat (p merge-matrix)
 
  (declare (type (SIMPLE-ARRAY T (* *)) merge-matrix))  
  (let ((n (array-dimension merge-matrix 0))
	(row-number nil))
    (dotimes (i n row-number)
      (dotimes (j 2)
	(when (= p (aref merge-matrix i j))
	  (setf row-number i))))))

(defun temporary-cluster-number (p merge-matrix)
 
  (declare (type (SIMPLE-ARRAY T (* *)) merge-matrix))  
  (if (null (belong-to-merge-mat p merge-matrix))
      nil
    (let ((cluster-number (+ 1 (belong-to-merge-mat p merge-matrix)))
	  (top (+ 1 (merge-top merge-matrix))))
      (dotimes (i top cluster-number)
	(dotimes (j 2)
	  (when (= cluster-number (aref merge-matrix i j))
	    (setf cluster-number (+ 1 i))))))))

(defun merge-top (merge-matrix)

  (declare (type (SIMPLE-ARRAY T (* *)) merge-matrix))  
  
  (let ((row-number 0))
    (dotimes (i (array-dimension merge-matrix 0) (- row-number 1))
       (when (integerp (aref merge-matrix i 0))
	 (setf row-number (+ 1 row-number))))))

(defun cluster-member (cluster-number merge-matrix)
 
  (declare (type (SIMPLE-ARRAY T (* *)) merge-matrix))  
  (assert (>= cluster-number 1))
  (let* ((row-number (- cluster-number 1))
	 (p (aref merge-matrix row-number 0))
	 (q (aref merge-matrix row-number 1))
	 (cluster '()))
    
    (cond ((and (< p 1)	(< q 1))
	   (setf cluster (append cluster (list p q))))
	   
	   ((and (> p 0) (< q 1))
	    (setf cluster (append cluster (cluster-member p merge-matrix) (list q))))
	   
	   ((and (< p 1) (> q 0))
	    (setf cluster (append cluster (list p) (cluster-member q merge-matrix))))
	   
	   (t
	    (setf cluster (append cluster (cluster-member p merge-matrix) (cluster-member q merge-matrix)))))))

(defun correspond-cluster (p merge-matrix)
 
  (declare (type (SIMPLE-ARRAY T (* *)) merge-matrix))  
  
  (if (> p 0)
      (cluster-member p merge-matrix)
    (list p)))

(defun order (merge-matrix)
  (mapcar #'- (cluster-member (+ 1 (merge-top merge-matrix)) merge-matrix)))

(defun hc-average (distance-matrix merge-matrix i j)
 
  (declare (type dmat distance-matrix))
  (declare (type (simple-array t (* *)) merge-matrix))  
  
  (let* ((l (merge-top merge-matrix))
	 (p (aref merge-matrix l 0))
	 (q (aref merge-matrix l 1))
	 (p-cluster (correspond-cluster p merge-matrix))
	 (q-cluster (correspond-cluster q merge-matrix))
	 (n-p (length p-cluster))
	 (n-q (length q-cluster))
	 (new-cluster (append p-cluster q-cluster)))
   
    (cond ((and (member (- i) new-cluster) (not (member (- j) new-cluster)))
	
	   (/ (+ (* n-p (aref distance-matrix (- (first p-cluster)) j)) 
		 (* n-q (aref distance-matrix (- (first q-cluster)) j)))
	      (+ n-p n-q)))
	      
	  ((and (not (member (- i) new-cluster)) (member (- j) new-cluster))
	     
	   (/ (+ (* n-p (aref distance-matrix (- (first p-cluster)) i)) 
		 (* n-q (aref distance-matrix (- (first q-cluster)) i)))
	      (+ n-p n-q)))
	      
	  ((and (not (member (- i) new-cluster)) (not (member (- j) new-cluster)))
	   (aref distance-matrix i j))
	      
	  (t
	   (aref distance-matrix i j)))))

(defun hc-single (distance-matrix merge-matrix i j)
 
  (declare (type dmat distance-matrix))
  (declare (type (simple-array t (* *)) merge-matrix))
  
  (let* ((l (merge-top merge-matrix))
	 (p (aref merge-matrix l 0))
	 (q (aref merge-matrix l 1))
	 (p-cluster (correspond-cluster p merge-matrix))
	 (q-cluster (correspond-cluster q merge-matrix))
	 (new-cluster (append p-cluster q-cluster)))
   
    (cond ((and (member (- i) new-cluster) (not (member (- j) new-cluster)))
	   (* 0.5 (+ (aref distance-matrix (- (first p-cluster)) j)
		     (aref distance-matrix (- (first q-cluster)) j)
		     (- (abs (- (aref distance-matrix (- (first p-cluster)) j)
				(aref distance-matrix (- (first q-cluster)) j)))))))
	      
	  ((and (not (member (- i) new-cluster)) (member (- j) new-cluster))
	   (* 0.5 (+ (aref distance-matrix (- (first p-cluster)) i)
		     (aref distance-matrix (- (first q-cluster)) i)
		     (- (abs (- (aref distance-matrix (- (first p-cluster)) i)
				(aref distance-matrix (- (first q-cluster)) i)))))))
	  
	  ((and (not (member (- i) new-cluster)) (not (member (- j) new-cluster)))
	   (aref distance-matrix i j))
	      
	  (t
	   (aref distance-matrix i j)))))

(defun hc-complete (distance-matrix merge-matrix i j)
 
  (declare (type dmat distance-matrix))
  (declare (type (simple-array t (* *)) merge-matrix))  
  
  (let* ((l (merge-top merge-matrix))
	 (p (aref merge-matrix l 0))
	 (q (aref merge-matrix l 1))
	 (p-cluster (correspond-cluster p merge-matrix))
	 (q-cluster (correspond-cluster q merge-matrix))
	 (new-cluster (append p-cluster q-cluster)))
   
    (cond ((and (member (- i) new-cluster) (not (member (- j) new-cluster)))
	   (* 0.5 (+ (aref distance-matrix (- (first p-cluster)) j)
		     (aref distance-matrix (- (first q-cluster)) j)
		     (+ (abs (- (aref distance-matrix (- (first p-cluster)) j)
				(aref distance-matrix (- (first q-cluster)) j)))))))
	  
	  ((and (not (member (- i) new-cluster)) (member (- j) new-cluster))
	   (* 0.5 (+ (aref distance-matrix (- (first p-cluster)) i)
		     (aref distance-matrix (- (first q-cluster)) i)
		     (+ (abs (- (aref distance-matrix (- (first p-cluster)) i)
				(aref distance-matrix (- (first q-cluster)) i)))))))
	  
	  ((and (not (member (- i) new-cluster)) (not (member (- j) new-cluster)))
	   (aref distance-matrix i j))
	  
	  (t
	   (aref distance-matrix i j)))))

(defun hc-centroid (distance-matrix merge-matrix i j)
  (declare (type dmat distance-matrix))
  (declare (type (simple-array t (* *)) merge-matrix))  
  (let* ((l (merge-top merge-matrix))
	 (p (aref merge-matrix l 0))
	 (q (aref merge-matrix l 1))
	 (p-cluster (correspond-cluster p merge-matrix))
	 (q-cluster (correspond-cluster q merge-matrix))
	 (n-p (length p-cluster))
	 (n-q (length q-cluster))
	 (new-cluster (append p-cluster q-cluster)))
   
    (cond ((and (member (- i) new-cluster) (not (member (- j) new-cluster)))
	   (- (/ (+ (* n-p (aref distance-matrix (- (first p-cluster)) j)) 
		    (* n-q (aref distance-matrix (- (first q-cluster)) j)))  
		 (+ n-p n-q))
	      (/ (* n-p n-q (aref distance-matrix (- (first p-cluster))(- (first q-cluster))))
		 (expt (+ n-p n-q) 2))))
		  
	  ((and (not (member (- i) new-cluster)) (member (- j) new-cluster))
	   (- (/ (+ (* n-p (aref distance-matrix (- (first p-cluster)) i)) 
		    (* n-q (aref distance-matrix (- (first q-cluster)) i)))  
		 (+ n-p n-q))
	      (/ (* n-p n-q (aref distance-matrix (- (first p-cluster))(- (first q-cluster))))
		 (expt (+ n-p n-q) 2))))
	     
	  ((and (not (member (- i) new-cluster)) (not (member (- j) new-cluster)))
	   (aref distance-matrix i j))
	      
	  (t
	   (aref distance-matrix i j)))))

(defun hc-median (distance-matrix merge-matrix i j)
  (declare (type dmat distance-matrix))
  (declare (type (simple-array t (* *)) merge-matrix))  
  (let* ((l (merge-top merge-matrix))
	 (p (aref merge-matrix l 0))
	 (q (aref merge-matrix l 1))
	 (p-cluster (correspond-cluster p merge-matrix))
	 (q-cluster (correspond-cluster q merge-matrix))
	 (new-cluster (append p-cluster q-cluster)))
   
    (cond ((and (member (- i) new-cluster) (not (member (- j) new-cluster)))
	   (+ (* 0.5 (+ (aref distance-matrix (- (first p-cluster)) j)
			(aref distance-matrix (- (first q-cluster)) j)))
	      (* -0.25 (aref distance-matrix (- (first p-cluster))(- (first q-cluster))))))
		    
	     	  
	  ((and (not (member (- i) new-cluster)) (member (- j) new-cluster))
	   (+ (* 0.5 (+ (aref distance-matrix (- (first p-cluster)) i)
			(aref distance-matrix (- (first q-cluster)) i)))
	      (* -0.25 (aref distance-matrix (- (first p-cluster))(- (first q-cluster))))))
	       
	  ((and (not (member (- i) new-cluster)) (not (member (- j) new-cluster)))
	   (aref distance-matrix i j))
	      
	  (t
	   (aref distance-matrix i j)))))

(defun hc-ward (distance-matrix merge-matrix i j)
  
  (declare (type dmat distance-matrix))
  (declare (type (simple-array t (* *)) merge-matrix)) 
  
  (let* ((l (merge-top merge-matrix))
	 (p (aref merge-matrix l 0))
	 (q (aref merge-matrix l 1))
	 (p-cluster (correspond-cluster p merge-matrix))
	 (q-cluster (correspond-cluster q merge-matrix))
	 (n-p (length p-cluster))
	 (n-q (length q-cluster))
	 (new-cluster (append p-cluster q-cluster)))
   
    (cond ((and (member (- i) new-cluster) (not (member (- j) new-cluster)))
	   
	   (let ((n-j (if (null (temporary-cluster-number (- j) merge-matrix))
			  1
			(length (correspond-cluster (temporary-cluster-number (- j) merge-matrix)
						    merge-matrix)))))
	     
	     (/ (+ (* (+ n-p n-j) (aref distance-matrix (- (first p-cluster)) j))
		   (* (+ n-q n-j) (aref distance-matrix (- (first q-cluster)) j))
		   (* -1 n-j (aref distance-matrix (- (first p-cluster))(- (first q-cluster)))))
		(+ n-p n-q n-j))))
	
	  ((and (not (member (- i) new-cluster)) (member (- j) new-cluster))
	   
	   (let ((n-i (if (null (temporary-cluster-number (- i) merge-matrix))
			  1
			(length (correspond-cluster (temporary-cluster-number (- i) merge-matrix)
						    merge-matrix)))))
	 
	     (/ (+ (* (+ n-p n-i) (aref distance-matrix (- (first p-cluster)) i))
		   (* (+ n-q n-i) (aref distance-matrix (- (first q-cluster)) i))
		   (* -1 n-i (aref distance-matrix (- (first p-cluster))(- (first q-cluster)))))
		(+ n-p n-q n-i))))
	  
	  ((and (not (member (- i) new-cluster)) (not (member (- j) new-cluster)))
	   (aref distance-matrix i j))
	  
	  (t
	   (aref distance-matrix i j)))))

(defun make-sublst (distance-matrix merge-matrix)
  (declare (type dmat distance-matrix))
  (declare (type (simple-array t (* *)) merge-matrix))  
  
  (let* ((l (merge-top merge-matrix))
	 (p (aref merge-matrix l 0))
	 (q (aref merge-matrix l 1))
	 (p-cluster (correspond-cluster p merge-matrix))
	 (q-cluster (correspond-cluster q merge-matrix))
	 (new-cluster (append p-cluster q-cluster))
	 (sublst nil))
    (dotimes (i (array-dimension distance-matrix 0) sublst)
      (dotimes (j i)
	(when (and (member (- i) new-cluster) (member (- j) new-cluster))
	  (push (list (aref distance-matrix i j) i j) sublst))))))

(defun new-merge-matrix-elements (temporary-d-matrix)
  (let* ((lst (mat-to-list temporary-d-matrix))
	(obj (reduce #'(lambda (x y) (if (<= (first x) (first y))
			      x
			      y)) lst)))
    (values (- (second obj)) (- (third obj)))))

(defun cophenetic-matrix (distance-matrix &optional (method #'hc-average))
 
  (declare (type dmat distance-matrix))
  (let* ((n (array-dimension distance-matrix 0))
	 (sublst '())
	 (working-matrix (make-array (list n n) :initial-element 0.0d0 :element-type 'double-float))
	 (temporary-d-matrix (make-array (list n n) :initial-element 0.0d0 :element-type 'double-float))
	 (merge-matrix (make-array (list (- n 1) 2) :initial-element 0.5)))
    
    (declare (type dmat temporary-d-matrix working-matrix))
    (declare (type (simple-array t (* *)) merge-matrix))  
   
    (dotimes (i n)
      (dotimes (j n)
	(setf (aref temporary-d-matrix i j) (aref distance-matrix i j))))
	 
    (dotimes (l (- n 1))
      
      (multiple-value-bind (p q) (new-merge-matrix-elements temporary-d-matrix)

      	(cond ((and (temporary-cluster-number p merge-matrix)
		    (temporary-cluster-number q merge-matrix))
	       (setf (aref merge-matrix l 0) (temporary-cluster-number p merge-matrix))
	       (setf (aref merge-matrix l 1) (temporary-cluster-number q merge-matrix)))
	      
	      ((and (temporary-cluster-number p merge-matrix)
		    (not (temporary-cluster-number q merge-matrix)))
	       (setf (aref merge-matrix l 0) (temporary-cluster-number p merge-matrix))
	       (setf (aref merge-matrix l 1) q))
	      
	      ((and (not (temporary-cluster-number p merge-matrix))
		    (temporary-cluster-number q merge-matrix))
	       (setf (aref merge-matrix l 0) p)
	       (setf (aref merge-matrix l 1) (temporary-cluster-number q merge-matrix)))
	      
	      (t
	       (setf (aref merge-matrix l 0) p)
	       (setf (aref merge-matrix l 1) q))))
      
      (dotimes (i n)
	(dotimes (j n)
	  (setf (aref working-matrix i j)
	    (funcall method temporary-d-matrix merge-matrix i j))))
      
      (setf sublst (append (make-sublst temporary-d-matrix merge-matrix) sublst))
    				
      (dotimes (i n)
	(dotimes (j n)
	  (setf (aref temporary-d-matrix i j) (aref working-matrix i j))))
      
      (dolist (obj sublst)
	(setf (aref temporary-d-matrix (second obj) (third obj)) most-positive-long-float)))
    
    (setf sublst (remove most-positive-long-float sublst :test #'(lambda (x y) (= x (first y)))))
    
    (dolist (obj sublst)
      (setf (aref temporary-d-matrix (second obj) (third obj)) (first obj)
	    (aref temporary-d-matrix (third obj) (second obj)) (first obj)))
    
    (values temporary-d-matrix merge-matrix)))

(defun pearson-cc (x y)
  (let* ((x-bar (vector-mean x))
	 (y-bar (vector-mean y))
	 (u (vector-shift x x-bar))
	 (v (vector-shift y y-bar)))
 
    (/ (product-sum u v)
       (sqrt (* (product-sum u u)
		(product-sum v v))))))
    			    
(defun mat-to-vector (square-matrix)
  (let ((n (array-dimension square-matrix 0))
	(lst '()))
    (dotimes (i n (coerce (reverse lst) 'vector))
      (dotimes (j i)
	(push (aref square-matrix i j) lst)))))

(defun cophenetic-cc (distance-matrix cophenetic-matrix)
  (pearson-cc (mat-to-vector distance-matrix)
	      (mat-to-vector cophenetic-matrix)))



(defun distance-matrix (matrix  &optional (distance-fn #'euclidean))
  (let* ((n (array-dimension matrix 0))
	 (d (make-array (list n n) :initial-element 0.0d0 :element-type 'double-float)))
    
    (dotimes (i n d)
      (dotimes (j n)
	(setf (aref d i j)
	  (funcall distance-fn (pick-up-row matrix i) (pick-up-row matrix j)))))))

  

(defun euclidean (x y)
  (assert (= (length x) (length y)))
  (sqrt (loop 
	    for x-i across x
	    for y-i across y
	    sum (expt (- x-i y-i) 2))))
  
(defun cosine (x y)
  (assert (= (length x) (length y)))
  (- 1.0d0
     (/ (product-sum x y)
	(sqrt (* (product-sum x x)
		 (product-sum y y))))))
	      
(defun pearson (x y)
  (assert (= (length x) (length y)))
  (- 1.0d0 (pearson-cc x y)))

(defun manhattan (x y)
  (assert (= (length x) (length y)))
  (loop
      for x-i across x
      for y-i across y
      sum (abs (- x-i y-i))))

(defun tanimoto (x y)
  (assert (= (length x) (length y)))
  (- 1.0d0
     (/ (product-sum x y)
	(+ (product-sum x x)
	   (product-sum y y)
	   (- (product-sum x y))))))

(defun canberra (x y)
  (assert (= (length x) (length y)))
    (loop
	for x-i across x
	for y-i across y
	sum (if (= 0.0d0 (+ x-i y-i))
		0.0d0
	      (/ (abs (- x-i y-i))
		 (abs (+ x-i y-i))))))



(defun pre-cutree (k merge-matrix)
  (assert (<= 1 k (+ 1 (array-dimension merge-matrix 0))))
  (let* ((n (+ 1 (array-dimension merge-matrix 0)))
	(v (make-array n :initial-element 0)))
    (dotimes (i (+ 1 (array-dimension merge-matrix 0)) v)
      (when (temporary-cluster-number (- i) (old-merge-matrix (- n k -0.5) merge-matrix))
	(setf (aref v i) (temporary-cluster-number (- i) (old-merge-matrix (- n k -0.5) merge-matrix)))))))
		      
(defun proper-cluster-number (p merge-matrix)
  (do ((i 0 (+ i 1)))
      ((member p (cluster-member (+ i 1) merge-matrix)) (+ i 1))))

(defun old-merge-matrix (l merge-matrix)
  (let* ((n (array-dimension merge-matrix 0))
	 (m (make-array (list n 2))))
    
    (dotimes (i n)
      (dotimes (j 2)
	(setf (aref m i j) (aref merge-matrix i j))))
    
    (do ((i (floor l) (+ i 1)))
	((>= i n) m)
      (setf (aref m i 0) 0.5)
      (setf (aref m i 1) 0.5))))

(defun replace-zero (vector)
  (let* ((n (length vector))
	 (max (max-vector vector))
	 (u (make-array n)))
    (dotimes (i n)
      (setf (aref u i) (aref vector i)))
    (dotimes (i n u)
      (when (= (aref u i) 0)
	(setf (aref u i) (+ i max 1))))))

(defun modify (k vector)
  (let* ((n (length vector))
	 (u (make-array n)))
    (dotimes (i n)
      (setf (aref u i) (aref vector i)))
    (dotimes (i k u)
      (setf u (substitute (+ i 1) (ith-min i u) u)))))

(defun ith-min (l vector)
  (let* ((n (length vector))
	 (u (make-array n))
	 (v nil))
    
    (dotimes (i n)
      (setf (aref u i) (aref vector i)))
    
    (setf u (coerce (sort u #'<) 'list))
    
    (dotimes (i n (nth l (reverse v)))
      (when (not (eql (car v) (car u)))
	(setf v (cons (car u) v)))
      (setf u (rest u)))))

(defun cutree (k merge-matrix)
  (modify k (replace-zero (pre-cutree k merge-matrix))))

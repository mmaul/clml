(defpackage :nmf
  (:use :cl
	:hc
	:blas
	:lapack
	:hjs.learn.read-data
	:hjs.util.matrix
        :hjs.util.meta)
  (:import-from :hc 
		#:i-thvector
		#:vector-sum
		#:vector-mean
		#:vector-shift
		#:product-sum
		#:square-sum
		#:max-vector
		#:min-vector
		#:pick-up-row
		#:pick-up-column
		#:numeric-matrix)
  (:export #:nmf
	   #:nmf-clustering
	   #:rho-k
	   #:nmf-analysis
	   #:nmf-corpus-analysis
	   #:c^3m-cluster-number
	   #:nmf-search
	   #:nmf-corpus-search
	   #:nmf-sc
	   #:sparseness
	   #:sample-matrix))

(in-package :nmf)

(defun m^t (matrix)
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type dmat matrix))
  ;; (declare (:explain :inlining))
  (let* ((m (array-dimension matrix 0))
	 (n (array-dimension matrix 1))
	 (transpose (make-array (list n m) :element-type 'double-float)))
   
    (loop 
	for i below n
	do (loop 
	       for j below m
	       do (setf (aref transpose i j) (aref matrix j i)))
	finally (return transpose))))


(defun non-zero (number)
  "for c^3m"
  (assert (numberp number))
  (if (zerop number)
      0.0d0
    1.0d0))


(defun copy-matrix (matrix)
  
  (declare (type dmat matrix))
  
  (let* ((m (array-dimension matrix 0))
	 (n (array-dimension matrix 1))
	 (copy-mat (make-array (list m n) :element-type 'double-float)))
    
    (loop 
	for i below m
	do (loop 
	       for j below n
	       do (setf (aref copy-mat i j) (aref matrix i j)))
	finally  (return copy-mat))))


(defun make-test-matrix (m n)
  
  (let ((test-mat (make-array (list m n) :element-type 'double-float)))
 
    (loop 
	for i below m
	do (loop 
	       for j below n
	       do (setf (aref test-mat i j) (random 1.0d0)))
	finally (return test-mat))))


(defun sample-matrix (m n)
  (assert (and (< 0 m) (< 0 n)))
  (let ((sample-mat (make-array (list m n) :element-type 'double-float)))
    (loop 
	for i below m
	do (loop 
	       for j below n
	       do (setf (aref sample-mat i j) (coerce (random 100) 'double-float)))
	finally (return sample-mat))))


(defun norm (matrix)
  
  (let ((n (array-dimension matrix 0))
	(m (array-dimension matrix 1)))
  
    (loop 
	for i below n
	sum (loop 
		for j below m 
		sum (* (aref matrix i j) (aref matrix i j))) into temp
	finally (return (sqrt temp)))))	


(defun m- (a b)
  
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type dmat a b))
  ;; (declare (:explain :inlining))
  
  (assert (and (= (array-dimension a 0) (array-dimension b 0))
	       (= (array-dimension b 1) (array-dimension b 1))))
  
  (let* ((m (array-dimension a 0))
	 (n (array-dimension a 1))
	 (c (make-array (list m n) :element-type 'double-float)))
    
    (loop 
	for i below m
	do (loop 
	       for j below n
	       do (setf (aref c i j) (- (aref a i j) (aref b i j))))
	finally (return c))))

#+allegro
(eval-when (:compile-toplevel)
  (setf (get 'a*b-i-j 'sys::immed-args-call)
    '((:lisp :lisp :lisp :lisp)
      double-float)))

(declaim (ftype (function
                 (simple-array simple-array fixnum fixnum) double-float)
                a*b-i-j))

(defun a*b-i-j (a b i j)
  
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type dmat a b)
           (type fixnum i j))
  ;; (declare (:explain :inlining))
  
  (assert (= (array-dimension a 1) (array-dimension b 0)))
  
  (let ((k (array-dimension a 1))
  	(sum 0.0d0))
    
    (declare (type double-float sum))
    
    (dotimes (l k)
      (declare (type fixnum l))
      (incf sum (* (aref a i l) (aref b l j))))
    sum))


(defun elementwise-product (a b)
  "destractive"
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type (simple-array double-float (* *)) a b))
 
  (assert (= (array-dimension a 0) (array-dimension b 0)))
  (assert (= (array-dimension a 1) (array-dimension b 1)))
   
  (let ((m (array-dimension a 0))
	(n (array-dimension a 1)))
     
    (dotimes (i m a)
      (dotimes (j n)
	(setf (aref a i j) (* (aref a i j) (aref b i j)))))))


(defun m-divides-n (a b)
  "destractive"
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type (simple-array double-float (* *)) a b))
  
  (assert (= (array-dimension a 0) (array-dimension b 0)))
  (assert (= (array-dimension a 1) (array-dimension b 1)))
   
  (let ((m (array-dimension a 0))
	(n (array-dimension a 1)))
     
    (dotimes (i m a)
      (dotimes (j n)
	(setf (aref a i j) (/ (aref a i j) (aref b i j)))))))


(defun nmf-euc (x k &optional (iteration 100))

  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type dmat x))
  ;; (declare (:explain :inlining))
  (let* ((m (array-dimension x 0))
	 (n (array-dimension x 1))
	 (w (make-test-matrix m k))
	 (h (make-test-matrix k n))
	 (p (make-array (list m n) :element-type 'double-float)))
    
    (declare (type dmat w h p))
    
    (dotimes (l iteration)
      
      (let ((w^t (m^t w))
	    (wh (m-times-n w h p)))
	(dotimes (i k)
	  (dotimes (j n)
	    (setf (aref h i j)
	      (/ (* (aref h i j)
                    (a*b-i-j w^t x i j))
		 (a*b-i-j w^t wh i j))))))
      
      (let ((h^t (m^t h))
	    (wh (m-times-n w h p)))
	(dotimes (i m)
	  (dotimes (j k)
	    (setf (aref w i j)
	      (/ (* (aref w i j)
                    (a*b-i-j x h^t i j))
		 (a*b-i-j wh h^t i j)))))))
    
    ;;(print (norm (m- x (m-times-n w h))))
    (values w h)))

#+ignore
(defun nmf-euc (x k &key (iteration 100))
  
  (declare (optimize (speed 3) (debug 0) (safety 0))
	   (type dmat x))
 
  (let* ((m (array-dimension x 0))
	 (n (array-dimension x 1))
	 (w (make-test-matrix m k))
	 (h (make-test-matrix k n)))
    
    (declare (type dmat w h))
    
    (dotimes (p iteration)
      
      (elementwise-product h (m-divides-n (m*m (m^t w) x)
					  (m*m (m*m (m^t w) w) h)))
      
      (elementwise-product w (m-divides-n (m*m x (m^t h))
					  (m*m (m*m w h) (m^t h)))))
    
    ;;(print (matrix-norm (m- x (m*m w h))))
    (values w h)))


(defun m-times-n (a b &optional c)
  
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type dmat a b))
  ;; (declare (:explain :inlining))
 
  (assert (= (array-dimension a 1) (array-dimension b 0)))
  
  (let* ((m (array-dimension a 0))
	 (n (array-dimension b 1))
         (c (or c
                (make-array (list m n) :element-type 'double-float))))
    
    (declare (type dmat c))
  
    (dotimes (i m c)
      (dotimes (j n)
	(setf (aref c i j) (a*b-i-j a b i j))))))


(declaim (ftype (function
                 (simple-array simple-array simple-array fixnum fixnum) double-float)
                aux-kl-1 aux-kl-2))


(defun nmf-kl (x k &optional (iteration 100))

  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type dmat x)
           (type fixnum k))
  ;; (declare (:explain :inlining))

  (let* ((m (array-dimension x 0))
	 (n (array-dimension x 1))
	 (u (make-test-matrix m k))
	 (v (make-test-matrix k n))
	 (q (make-array (list m n) :element-type 'double-float)))
    
    (declare (type dmat u v q))
    
    (dotimes (p iteration (values u v))
      
      (let ((uv (m-times-n u v q)))
	(dotimes (i m)
	  (dotimes (l k)
	    (declare (type fixnum i l))
	    (setf (aref u i l)
	      (* (aref u i l)
		 (aux-kl-1 x uv v i l))))))
      
      (let ((old-u (copy-matrix u)))
	
	(declare (type dmat old-u))
	
	(dotimes (i m)
	  (dotimes (l k)
            (declare (type fixnum i l))
	    (setf (aref u i l)
	      (/ (aref u i l)
		 (loop
		     for i below m
		     sum (aref old-u i l)))))))
      
      (let ((uv (m-times-n u v q)))
	(dotimes (l k)
	  (dotimes (j n)
	    (declare (type fixnum l j))
	    (setf (aref v l j)
	      (* (aref v l j)
		 (aux-kl-2 x uv u l j)))))))))


(defun aux-kl-1 (x uv v i l)
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type dmat x uv v))
  (loop
      for j fixnum below (array-dimension x 1) 
      sum (/ (* (aref x i j) (aref v l j))
	     (aref uv i j))
      of-type double-float))
	
	      
(defun aux-kl-2 (x uv u l j)
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type dmat x uv u))
  (loop 
      for i fixnum below (array-dimension x 0)
      sum (/ (* (aref x i j) (aref u i l))
	     (aref uv i j))
      of-type double-float))


(defun row-consensus-matrix (weight-matrix)
  (let* ((n (array-dimension weight-matrix 0))
	 (v (row-clustering-vector weight-matrix))
	 (c (make-array (list n n))))
    
    (dotimes (i n c)
      (dotimes (j n)
	(setf (aref c i j)
	  (if (= (aref v i) (aref v j))
	      1
	    0))))))


(defun column-consensus-matrix (feature-matrix)
  (let* ((n (array-dimension feature-matrix 1))
	 (v (column-clustering-vector feature-matrix))
	 (c (make-array (list n n))))
    
    (dotimes (i n c)
      (dotimes (j n)
	(setf (aref c i j)
	  (if (= (aref v i) (aref v j))
	      1
	    0))))))


(defun row-average-consensus-matrix (matrix k &key (cost-fn :euclidean) (iteration 100) (repeat 100))

  (let* ((m (array-dimension matrix 0))
	;(n (array-dimension matrix 1))
	 (average-c (make-array (list m m) :initial-element 0.0d0 :element-type 'double-float))
	 (c (make-array (list m m)))
	 (u (make-array (list m k))))
	;(v (make-array (list k n))))
    
    (dotimes (l repeat average-c)
      (setf u (nmf matrix k :cost-fn cost-fn :iteration iteration))
      (setf c (row-consensus-matrix u))
      (dotimes (i m)
	(dotimes (j m)
	  (setf (aref average-c i j)
	    (/ (+ (* l (aref average-c i j)) (aref c i j))
	       (+ l 1))))))))


(defun column-average-consensus-matrix (matrix k &key (cost-fn :euclidean) (iteration 100) (repeat 100))

  (let* ((m (array-dimension matrix 0))
	 (n (array-dimension matrix 1))
	 (average-c (make-array (list n n) :initial-element 0.0d0 :element-type 'double-float))
	 (c (make-array (list n n)))
	 (u (make-array (list m k)))
	 (v (make-array (list k n))))
    
    (dotimes (l repeat average-c)
      (multiple-value-setq (u v) (nmf matrix k :cost-fn cost-fn :iteration iteration))
      (setf u u)
      (setf c (column-consensus-matrix v))
      (dotimes (i n)
	(dotimes (j n)
	  (setf (aref average-c i j)
	    (/ (+ (* l (aref average-c i j)) (aref c i j))
	       (+ l 1))))))))


(defun average-consensus-matrix (matrix k &key (type :row) (cost-fn :euclidean) (iteration 100) (repeat 100))
  (cond ((eq type :row)
	 (row-average-consensus-matrix matrix k :cost-fn cost-fn :iteration iteration :repeat repeat))
	((eq type :column)
	 (column-average-consensus-matrix matrix k :cost-fn cost-fn :iteration iteration :repeat repeat))
	(t (error "illegal keyword parameter."))))


(defun numbering (lst)
  (mapcar #'list lst (loop for i below (length lst) collect i)))

(defun max-index (vector)
  (let* ((v (coerce vector 'list))
	 (w (numbering v)))
    (second (reduce #'(lambda (x y) (if (<= (first x) (first y))
				y
			      x)) w))))


(defun sim-mat->dis-mat (similarity-matrix)
  (let* ((n (array-dimension similarity-matrix 0))
	 (distance-matrix (make-array (list n n) :element-type 'double-float)))
    (dotimes (i n distance-matrix)
      (dotimes (j n)
	(setf (aref distance-matrix i j) (- 1.0 (aref similarity-matrix i j)))))))


(defun row-clustering-vector (weight-matrix)
  (let* ((m (array-dimension weight-matrix 0))
	 (v (make-array m)))
    (dotimes (i m v)
      (setf (aref v i)
	(max-index (pick-up-row weight-matrix i))))))


(defun column-clustering-vector (feature-matrix)
  (let* ((m (array-dimension feature-matrix 1))
	 (v (make-array m)))
    (dotimes (i m v)
      (setf (aref v i)
	(max-index (pick-up-column feature-matrix i))))))


(defun rho-k (matrix k &key (type :row) (cost-fn :euclidean) (iteration 100) (repeat 100))
  (let* ((avc (average-consensus-matrix matrix k :type type :cost-fn cost-fn :iteration iteration :repeat repeat))
	 (d (sim-mat->dis-mat avc))
	 (u (cophenetic-matrix d)))
    (cophenetic-cc d u)))


(defun nmf-clustering (matrix k &key (type :row) (cost-fn :euclidean) (iteration 100))
  (multiple-value-bind (weight feature) (nmf matrix k :cost-fn cost-fn :iteration iteration)
    (cond ((eq type :column) (column-clustering-vector feature))
	  ((eq type :row) (row-clustering-vector weight))
	  (t (error "illegal keyword parameter.")))))


(defun nmf (matrix k &key (cost-fn :euclidean) (iteration 100))
  (cond ((eq cost-fn :euclidean)
	 (nmf-euc matrix k iteration))
	((eq cost-fn :kl)
	 (nmf-kl matrix k iteration))
	(t (error "illegal keyword parameter."))))

(defun make-article-id (unspecialized-dataset)
  (let* ((data (dataset-points unspecialized-dataset))
	 (n (length data))
	 (x (make-array n)))
    (dotimes (k n x)
      (setf (aref x k) (aref (aref data k) 0)))))


(defun make-range (n)
  (loop for i below n collect i))


(defun make-data-types (n)
  (loop for i below n collect ':numeric))


(defun pick-up-column-data (feature-matrix n &key results)
  (assert (< -1 n (array-dimension feature-matrix 0)))
  (let ((v (numbering (coerce (pick-up-row feature-matrix n) 'list))))
   
    (setf v (sort v #'> :key  #'car))
    (dotimes (j results)
      (format t "~A   ~A~%" (car (cdr (nth j v))) (car (nth j v))))))


(defun result-column-data (feature-matrix &key results)
  (assert (< results (array-dimension feature-matrix 1)))
  (let ((n (array-dimension feature-matrix 0)))
    (dotimes (i n)
      (format t "~%Feature ~A~%" i)
      (pick-up-column-data feature-matrix i :results results))))


(defun pick-up-row-data (weight-matrix n &key results)
  (assert (< -1 n (array-dimension weight-matrix 1)))
  (let ((v (numbering (coerce (pick-up-column weight-matrix n) 'list))))
    
    (setf v (sort v #'> :key  #'car))
    (dotimes (j results)
      (format t "~A   ~A~%" (car (cdr (nth j v))) (car (nth j v))))))


(defun result-row-data (weight-matrix &key results)
  (assert (< results (array-dimension weight-matrix 0)))
  (let ((n (array-dimension weight-matrix 1)))
    (dotimes (i n)
      (format t "~%Feature ~A~%" i)
      (pick-up-row-data weight-matrix i :results results))))


(defun nmf-analysis (matrix k &key (cost-fn :euclidean) (iteration 100) (type :row) (results 10))
  (multiple-value-bind (weight feature) (nmf matrix k :cost-fn cost-fn :iteration iteration)
    (cond ((eq type :row)
	   (result-row-data weight :results results))
	  ((eq type :column)
	   (result-column-data feature :results results))
	  (t (error "illegal keyword parameter.")))))


(defun make-term-index (unspecialized-dataset)
  (let* ((dataset-length (length (dataset-dimensions unspecialized-dataset)))
         (range (rest (make-range dataset-length)))
         (data-types (make-data-types (length range)))
         (numeric-dataset (pick-and-specialize-data unspecialized-dataset
						    :range range
						    :data-types data-types))
         (numeric-dataset-dimensions (dataset-dimensions numeric-dataset))
         (numeric-dataset-length (length numeric-dataset-dimensions)))
    (loop with term-index-vector = (make-array numeric-dataset-length)
        for i below numeric-dataset-length
        do (setf (aref term-index-vector i)
             (dimension-name (aref numeric-dataset-dimensions i)))
        finally (return term-index-vector))))


(defun make-document-index (unspecialized-dataset)
  (let* ((data (dataset-points unspecialized-dataset))
	 (document-index-vector (make-array (length data))))
    (dotimes (i (length data) document-index-vector)
      (setf (aref document-index-vector i) (aref (aref data i) 0)))))


(defun nmf-corpus-analysis (corpus-dataset k &key (cost-fn :euclidean) (iteration 100) (results 10))
  (let ((term-index-vector (make-term-index corpus-dataset))
	(document-index-vector (make-document-index corpus-dataset))
	(tf*idf*cosine-matrix (make-document-term-matrix corpus-dataset)))
    
    (multiple-value-bind (weight feature) (nmf tf*idf*cosine-matrix k :cost-fn cost-fn :iteration iteration)
      (result-terms feature term-index-vector :results results)
      (result-documents weight document-index-vector :results results))))


(defun result-documents (weight-matrix document-index-vector &key results)
  (assert (< results (array-dimension weight-matrix 0)))
  (let ((n (array-dimension weight-matrix 1)))
    (dotimes (i n)
      (format t "~%Feature ~A~%" i)
      (pick-up-documents weight-matrix document-index-vector i :results results))))


(defun pick-up-documents (weight-matrix document-index-vector n &key results)
  (assert (< -1 n (array-dimension weight-matrix 1)))
  (let ((v (numbering (coerce (pick-up-column weight-matrix n) 'list))))
    
    (setf v (sort v #'> :key  #'car))
    (dotimes (j results)
      (format t "~A     ~A~%" (aref document-index-vector (car (cdr (nth j v)))) (car (nth j v))))))


(defun result-terms (feature-matrix term-index-vector &key results)
  (assert (< results (array-dimension feature-matrix 1)))
  (let ((n (array-dimension feature-matrix 0)))
    (dotimes (i n)
      (format t "~%Feature ~A~%" i)
      (pick-up-terms feature-matrix term-index-vector i :results results))))


(defun pick-up-terms (feature-matrix term-index-vector n &key results)
  (assert (< -1 n (array-dimension feature-matrix 0)))
  (let ((v (numbering (coerce (pick-up-row feature-matrix n) 'list))))
   
    (setf v (sort v #'> :key  #'car))
    (dotimes (j results)
      (format t "~A     ~A~%" (aref term-index-vector (car (cdr (nth j v)))) (car (nth j v))))))


(defun make-document-term-matrix (corpus-dataset)
  (let* ((dataset-length (length (dataset-dimensions corpus-dataset)))
         (range (rest (make-range dataset-length)))
         (data-types (make-data-types (length range)))
         (numeric-dataset (pick-and-specialize-data corpus-dataset
						    :range range
                                                    :data-types data-types))
	 (matrix (numeric-matrix numeric-dataset)))
    (normalization (tf*idf matrix))))
   ;; (tf*idf matrix)))

#+allegro
(eval-when (:compile-toplevel)
  (setf (get 'normalization-factor 'sys::immed-args-call)
    '((:lisp :lisp) double-float)))

(declaim (ftype (function (simple-array fixnum) double-float) normalization-factor))

(defun normalization (matrix)
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type dmat matrix))
  (let* ((m (array-dimension matrix 0))
	 (n (array-dimension matrix 1))
	 (new-matrix (make-array (list m n) :element-type 'double-float)))
    (dotimes (i m new-matrix)
      (dotimes (j n)
	(setf (aref new-matrix i j)
	  (+ double-float-epsilon
	     (* (aref matrix i j)
	     (normalization-factor matrix i))))))))

#+ignore
(defun normalization (matrix)
  "for experiment"
  matrix)


(defun normalization-factor (matrix i)
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type dmat matrix)
           (type fixnum i))
  ;; (declare (:explain :inlining))
  (let ((n (array-dimension matrix 1))
        (sum 0.0d0))
    (declare (type (double-float 0.0d0 *) sum))
    (dotimes (j n)
      (declare (type fixnum j))
      (incf sum (expt (aref matrix i j) 2)))
    (/ (sqrt sum))))

(declaim (ftype (function (simple-array fixnum) fixnum) dfreq))

(defun dfreq (matrix word-id)
  (assert (<= 0 word-id (- (array-dimension matrix 1) 1)))
  (let ((v (pick-up-column matrix word-id)))
    (number-of-non-zero-elements v)))


(defun number-of-non-zero-elements (vector)
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type dvec vector))
  (loop for i below (length vector)
      count (not (zerop (aref vector i)))))


(defun tf*idf (matrix)
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type dmat matrix))
  ;; (declare (:explain :inlining))
  (let* ((m (array-dimension matrix 0))
	 (n (array-dimension matrix 1))
	 (new-matrix (make-array (list m n) :element-type 'double-float)))
    (dotimes (i m new-matrix)
      (dotimes (j n)
	(setf (aref new-matrix i j)
	  (* (aref matrix i j) 
	     (log (/ (float m 0.0d0)
                     (float (dfreq matrix j) 0.0d0)))))))))

#+ignore
(defun tf*idf (matrix)
  "for experiment"
  matrix)


(defun c^3m-cluster-number (corpus-dataset)
  (let* ((dataset-length (length (dataset-dimensions corpus-dataset)))
         (range (rest (make-range dataset-length)))
         (data-types (make-data-types (length range)))
         (numeric-dataset (pick-and-specialize-data corpus-dataset
						    :range range
						    :data-types data-types))
	 (matrix (numeric-matrix numeric-dataset)))
    (loop for i below (array-dimension matrix 0) sum (delta matrix i i))))

(defun +phi (matrix i j)
  (/ (non-zero (aref matrix i j))
     (number-of-non-zero-elements (pick-up-row matrix i))))

(defun -phi (matrix i j)
  (/ (non-zero (aref matrix i j))
     (number-of-non-zero-elements (pick-up-column matrix j))))

(defun delta (matrix i h)
  (loop for j below (array-dimension matrix 1)
      sum (* (+phi matrix i j)
	     (-phi matrix h j))))


(defun column-power-vector (matrix)
  (let* ((n (array-dimension matrix 1))
  	 (column-power-vector (make-array n :element-type 'double-float)))
    (dotimes (j n column-power-vector)
      (setf (aref column-power-vector j)
	(loop
	    for i below (array-dimension matrix 0)
	    sum (aref matrix i j))))))
	

(defun row-power-vector (matrix)
  (let* ((m (array-dimension matrix 0))
	 (row-power-vector (make-array m :element-type 'double-float)))
    (dotimes (i m row-power-vector)
      (setf (aref row-power-vector i)
	(loop
	    for j below (array-dimension matrix 1)
	    sum (aref matrix i j))))))


(defun column-adjusting-factor (matrix column-number)
  (let* ((column-power-vector (column-power-vector matrix))
	 (max-column-power (max-vector column-power-vector))
	 (semantic-weight 1.7))
    (/ (* semantic-weight max-column-power)
       (aref column-power-vector column-number))))


(defun row-adjusting-factor (matrix row-number)
  (let* ((row-power-vector (row-power-vector matrix))
	 (max-row-power (max-vector row-power-vector))
	 (semantic-weight 1.7))
    (/ (* semantic-weight max-row-power)
       (aref row-power-vector row-number))))


(defun nmf-search (matrix row-or-column-number &key type (iteration 100) (results 10))
  (cond ((eq type :row)
	 (assert (< -1 row-or-column-number (array-dimension matrix 0)))
	 (nmf-analysis (row-theme-weighting matrix row-or-column-number) 1 
		       :type type :iteration iteration :results results))
	((eq type :column)
	 (assert (< -1 row-or-column-number (array-dimension matrix 1)))
	 (nmf-analysis (column-theme-weighting matrix row-or-column-number) 1 
		       :type type :iteration iteration :results results))
	(t (error "illegal keyword parameter."))))


(defun column-theme-weighting (matrix column-number)
  (let ((adjusting-factor (column-adjusting-factor matrix column-number))
	(new-matrix (copy-matrix matrix)))
    
    (dotimes (i (array-dimension matrix 0) new-matrix)
      (dotimes (j (array-dimension matrix 1))
	(when (= j column-number)
	  (setf (aref new-matrix i j) (* adjusting-factor (aref new-matrix i j))))))))


(defun row-theme-weighting (matrix row-number)
  (let ((adjusting-factor (row-adjusting-factor matrix row-number))
	(new-matrix (copy-matrix matrix)))
    
    (dotimes (i (array-dimension matrix 0) new-matrix)
      (dotimes (j (array-dimension matrix 1))
	(when (= i row-number)
	  (setf (aref new-matrix i j) (* adjusting-factor (aref new-matrix i j))))))))


(defun term->column-number (term term-index-vector)
  (dotimes (i (length term-index-vector) (error "such term does not exist."))
    (when (equal term (aref term-index-vector i))
      (return i))))


(defun document->row-number (document document-index-vector)
  (dotimes (i (length document-index-vector) (error "such document does not exist."))
    (when (equal document (aref document-index-vector i))
      (return i))))


(defun nmf-corpus-search (corpus-dataset term-or-document-name
			  &key type (iteration 100) (results 10))
  (cond ((eq type :document)
	 (let* ((term-index-vector (make-term-index corpus-dataset)) 
		(document-index-vector (make-document-index corpus-dataset)) 
		(tf*idf*cosine-matrix (make-document-term-matrix corpus-dataset))
		(row-number (document->row-number term-or-document-name document-index-vector))
		(matrix (row-theme-weighting tf*idf*cosine-matrix row-number)))
	
	   (multiple-value-bind (weight feature) (nmf matrix 1 
						      :iteration iteration)
	     (result-terms feature term-index-vector :results results)
	     (result-documents weight document-index-vector :results results))))
	
	((eq type :term) 
	 (let* ((term-index-vector (make-term-index corpus-dataset)) 
		(document-index-vector (make-document-index corpus-dataset)) 
		(tf*idf*cosine-matrix (make-document-term-matrix corpus-dataset))
		(column-number (term->column-number term-or-document-name term-index-vector))
		(matrix (column-theme-weighting tf*idf*cosine-matrix column-number)))
	   
	   (multiple-value-bind (weight feature) (nmf matrix 1 
						      :iteration iteration)
	     (result-terms feature term-index-vector :results results)
	     (result-documents weight document-index-vector :results results))))
	
	(t (error "illegal keyword parameter."))))




(defun sparseness (vector)
  (let ((n (length vector)))
    (/ (- (sqrt n)
	  (/ (vector-sum vector)
	     (sqrt (square-sum vector))))
       (- (sqrt n) 1))))

(defun average-sparseness (matrix)
  (let ((m (array-dimension matrix 0)))
    (/ (loop for i below m sum (sparseness (pick-up-row matrix i)))
       m)))

(defun non-negative-p (n)
  (not (minusp n)))

(defun non-negative-vector-p (vector)
  (let ((n (length vector)))
    (dotimes (i n t)
      (when (minusp (aref vector i))
	(return nil)))))

(defun l1 (vector)
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type dvec vector))
  ;; (declare (:explain :inlining))
  (let ((sum 0.0d0))
    (declare (type double-float sum))
    (dotimes (i (length vector) sum)
      (incf sum (abs (aref vector i))))))

(defun l2 (vector)
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type dvec vector))
  ;; (declare (:explain :inlining))
  (let ((sum 0.0d0))
    (declare (type double-float sum))
    (dotimes (i (length vector) sum)
      (incf sum (the double-float
                  (expt (aref vector i) 2))))))

(defun s-x (x)
  (/ (- (sqrt (length x))
	(/ (l1 x) (sqrt (l2 x))))
     (- (sqrt (length x)) 1)))

(defun dot (a b)
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type dvec a b))
  ;; (declare (:explain :inlining))
  (assert (= (length a) (length b)))
  (let ((sum 0.0d0))
    (declare (type double-float sum))
    (dotimes (i (length a) sum)
      (incf sum (* (aref a i) (aref b i))))))

(defun v- (a b)
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type dvec a b))
  ;; (declare (:explain :inlining))
  (assert (= (length a) (length b)))
  (let ((v (make-array (length a) :element-type 'double-float)))
    (declare (type dvec v))
    (dotimes (i (length a) v)
      (setf (aref v i) (- (aref a i) (aref b i))))))
	       
(defun alpha (x s m)
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (ftype (function (simple-array simple-array) double-float) dot)
           (ftype (function (simple-array) double-float) l2))
  ;; (declare (:explain :inlining))
  (/ (+ (- (dot m (v- s m)))
	(sqrt (the (double-float 0.0)
                (- (the double-float
                     (expt (dot m (v- s m)) 2))
                   (* (l2 (v- s m))
                      (- (l2 m) (l2 x)))))))
     (l2 (v- s m))))

(defun set-l1 (x sparseness)
  (assert (< 0.0 sparseness 1.0))
  (* (sqrt (l2 x))
     (- (sqrt (length x))
	(* sparseness
	   (- (sqrt (length x)) 1)))))

(defun set-m (x z sparseness)
  (assert (< 0.0 sparseness 1.0))
  (let* ((n (length x))
	 (size (length z))
	 (m (make-array n :element-type 'double-float))
	 (l1-norm (set-l1 x sparseness)))
    (dotimes (i n m)
      (setf (aref m i)
	(if (member i z)
	    0.0
	  (/ l1-norm
	     (- n size)))))))

(defun set-z (z s)
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type dvec s))
  ;; (declare (:explain :inlining))
  (dotimes (i (length s) z)
    (when (minusp (aref s i))
      (setf z (remove-duplicates (append z (list i)))))))

(defun set-s0 (x sparseness)
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type dvec x))
  ;; (declare (:explain :inlining))
  (assert (< 0.0 sparseness 1.0))
  (let ((l1-norm (set-l1 x sparseness))
	(sum (vector-sum x))
	(s (make-array (length x) :element-type 'double-float))
	(n (length x)))
    (declare (type double-float l1-norm sum)
             (type dvec s))
    (dotimes (i (length s) s)
      (setf (aref s i)
	(+ (aref x i)
	   (/ (- l1-norm sum) n))))))

(defun set-s1 (x s m)
  (v+ m (alpha-v (alpha x s m) (v- s m))))

(defun set-s2 (z s)
  (dotimes (i (length s) s)
    (when (member i z)
      (setf (aref s i) 0.0))))

(defun set-s3 (x z s sparseness)
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type dvec s))
  ;; (declare (:explain :inlining))
  (assert (< 0.0 sparseness 1.0))
  (let ((c (calculate-c x s z sparseness)))
    (declare (type double-float c))
    (dotimes (i (length s) s)
      (unless (member i z)
        (setf (aref s i) (- (aref s i) c))))))

(defun calculate-c (x s z sparseness)
  (assert (< 0.0 sparseness 1.0))
  (/ (- (l1 s) (set-l1 x sparseness))
     (- (length x) (length z))))

(defun v+ (a b)
  (assert (= (length a) (length b)))
  (let ((v (make-array (length a) :element-type 'double-float)))
    (dotimes (i (length a) v)
      (setf (aref v i) (+ (aref a i) (aref b i))))))

(defun alpha-v (alpha v)
  (let ((w (make-array (length v) :element-type 'double-float)))
    (dotimes (i (length v) w)
      (setf (aref w i) (* alpha (aref v i))))))
  
(defun projection-operator (x sparseness)
  (assert (< 0.0 sparseness 1.0))
  (let* ((n (length x))
	 (s (make-array n :element-type 'double-float))
	 (z '())
	 (m (make-array n :element-type 'double-float)))
    
    (setf s (set-s0 x sparseness))
    (setf m (set-m x z sparseness))
    (setf s (set-s1 x s m))
    
    (loop while (not (non-negative-vector-p s))
        do
          (setf z (set-z z s))
          (setf s (set-s2 z s))
          (setf s (set-s3 x z s sparseness))
          (setf s (set-s1 x s m))
          (setf m (set-m x z sparseness))
          (setf s (set-s1 x s m)))
    s))

(defun alpha-m (alpha a)
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type double-float alpha)
           (type dmat a))
  ;; (declare (:explain :inlining))
  (let ((m (make-array (list (array-dimension a 0)
			     (array-dimension a 1))
		       :element-type 'double-float)))
    (dotimes (i (array-dimension a 0) m)
      (dotimes (j (array-dimension a 1))
	(setf (aref m i j) (* alpha (aref a i j)))))))

(defun set-w (v w h iteration)
  (m- w
      (alpha-m (coerce (/ (expt 2 iteration)) 'double-float) 
	       (m-times-n (m- (m-times-n w h) v)
			  (m^t h)))))

(defun set-h (v w h iteration)
  (m- h
      (alpha-m (coerce (/ (expt 2 iteration)) 'double-float) 
	       (m-times-n (m^t w)
			  (m- (m-times-n w h) v)))))
		
(defun proj-w (w sparseness)
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type dmat w))
  ;; (declare (:explain :inlining))
  (assert (< 0.0 sparseness 1.0))
  (let ((new-w (make-array (list (array-dimension w 0)
				 (array-dimension w 1))
			   :element-type 'double-float)))
    
    (dotimes (j (array-dimension w 1) new-w)
      	(let ((w-j (projection-operator (pick-up-column w j) sparseness)))
          (declare (type dvec w-j))
	  (dotimes (i (array-dimension w 0))
	    (setf (aref new-w i j) (aref w-j i)))))))

(defun proj-h (h sparseness)
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type dmat h))
  (assert (< 0.0 sparseness 1.0))
  (let ((new-h (make-array (list (array-dimension h 0)
				 (array-dimension h 1))
			   :element-type 'double-float)))
    
    (dotimes (i (array-dimension h 0) new-h)
      	(let ((h-i (projection-operator (pick-up-row h i) sparseness)))
	  (dotimes (j (array-dimension h 1))
	    (setf (aref new-h i j) (aref h-i j)))))))

(defun nmf-sc-left (v k sparseness &key (iteration 100))
  "new-version"
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type dmat v))
  ;; (declare (:explain :inlining))
  
  (let* ((m (array-dimension v 0))
	 (n (array-dimension v 1))
	 (w (make-test-matrix m k))
	 (h (make-test-matrix k n))
	 (q (make-array (list k k) :element-type 'double-float)))
     
    (declare (type dmat w h))
    
    (setf w (proj-w w sparseness))
    
    (dotimes (p iteration)
     
      (setf w (set-w v w h p))
      (setf w (proj-w w sparseness))
    
      (let* ((w^t (m^t w))
	     (w^tw (m-times-n w^t w q)))
 
	(declare (type dmat w^t w^tw))
    	
	(dotimes (i k)
	  (dotimes (j n)
            (declare (type fixnum i j))
	    (setf (aref h i j)
              (/ (* (aref h i j)
                    (a*b-i-j w^t v i j))
                 (+ (a*b-i-j w^tw h i j) double-float-epsilon)))))))
					; avoid to divide by zero
    (values w h)))

(defun nmf-sc-right (v k sparseness &key (iteration 100))
  "new version"
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type dmat v))
  ;; (declare (:explain :inlining))

  (let* ((m (array-dimension v 0))
	 (n (array-dimension v 1))
	 (w (make-test-matrix m k))
	 (h (make-test-matrix k n))
	 (q (make-array (list m n) :element-type 'double-float)))
     
    (declare (type dmat w h))
    
    (setf h (proj-h h sparseness))
    
    (dotimes (p iteration)
      
      (let* ((h^t (m^t h))
	     (wh (m-times-n w h q)))
	
	(declare (type dmat h^t wh))
	
	(dotimes (i m)
	  (dotimes (j k)
	     (declare (type fixnum i j))
	    (setf (aref w i j)
              (/ (* (aref w i j)
                    (a*b-i-j v h^t i j))
		 (+ (a*b-i-j wh h^t i j) double-float-epsilon))))))
                                        ; avoid to divide by zero
      (setf h (set-h v w h p))
      (setf h (proj-h h sparseness)))
      
    (values w h)))

(defun nmf-sc (v k sparseness &key type (iteration 100))
  (cond ((eq type :left)
	 (nmf-sc-left v k sparseness :iteration iteration))
	((eq type :right)
	 (nmf-sc-right v k sparseness :iteration iteration))
	(t (error "illegal keyword parameter."))))

(defpackage :pre-nmf
  (:use :cl
	:hc
	:linear-regression
	:hjs.learn.read-data
	:hjs.util.matrix))

(in-package :pre-nmf)

(defun i-thvector (numeric-dataset i)
  (assert (eq (type-of numeric-dataset) 'numeric-dataset))
  (assert (<= 0 i (- (length (aref (dataset-numeric-points numeric-dataset) 0)) 1)))
  (assert (<= 3 (length (dataset-numeric-points numeric-dataset))))
  (let* ((data (dataset-numeric-points numeric-dataset))
	 (n (length data))
	 (x-i (make-array n :element-type 'double-float)))
    (dotimes (k n x-i)
      (setf (aref x-i k) (aref (aref data k) i)))))

(defun vector-sum (vector)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (type dvec vector)) 
  
  (let ((sum 0))
    (dotimes (i (length vector) sum)
      (setf sum (+ sum (aref vector i))))))     

(defun vector-square (vector)
  (let ((v-square (make-array (length vector) :element-type 'double-float)))
    (dotimes (i (length vector) v-square)
      (setf (aref v-square i) (* (aref vector i) (aref vector i))))))

(defun square-sum (vector)
  (vector-sum (vector-square vector)))

(defun vector-mean (vector)
  (/ (vector-sum vector) (length vector)))

(defun m^t (matrix)
  (let* ((m (array-dimension matrix 0))
	 (n (array-dimension matrix 1))
	 (a (make-array (list n m) :element-type 'double-float)))
    (dotimes (i n a)
      (dotimes (j m)
	(setf (aref a i j) (aref matrix j i))))))

(defun make-test-matrix (m n)
  (let ((a (make-array (list m n) :element-type 'double-float)))
    (dotimes (i m a)
      (dotimes (j n)
	(setf (aref a i j) (coerce (random 1.0) 'double-float))))))

(defun make-test-matrix2 (m n)
  (let ((a (make-array (list m n) :element-type 'double-float)))
    (dotimes (i m a)
      (dotimes (j n)
	(setf (aref a i j) (coerce (random 100) 'double-float))))))

(defun nmf (x k)
  (let* ((m (array-dimension x 0))
	 (n (array-dimension x 1))
	 (u (make-test-matrix m k))
	 (v (make-test-matrix k n)))
    (dotimes (p 100)
      (dotimes (i k)
	(dotimes (j n)
	  (setf (aref v i j)
	    (* (aref v i j)
	       (aref (m*m (m^t u) x) i j)
	       (/ (aref (m*m (m*m  (m^t u) u) v) i j))))))
      (dotimes (i m)
	(dotimes (j k)
	  (setf (aref u i j)
	    (* (aref u i j)
	       (aref (m*m x (m^t v)) i j)
	       (/ (aref (m*m (m*m u v) (m^t v)) i j)))))))
      (values u v)))

(defun norm (matrix)
  (let ((n (array-dimension matrix 0))
	(m (array-dimension matrix 1))
	(sum 0.0))
    (dotimes (i n (* 0.5 sum))
      (dotimes (j m)
	(setf sum (+ sum (* (aref matrix i j) (aref matrix i j))))))))
	   

(defun nmf2 (x k)
  (do* ((m (array-dimension x 0))
	(n (array-dimension x 1))
	(u (make-test-matrix m k))
	(v (make-test-matrix k n)))
      ((< (norm (m- x (m*m u v))) 0.001) (values u v))
    (dotimes (i k)
      (dotimes (j n)
	(setf (aref v i j)
	  (* (aref v i j)
	     (aref (m*m (m^t u) x) i j)
	     (/ (aref (m*m (m*m (m^t u) u) v) i j))))))
    (dotimes (i m)
      (dotimes (j k)
	(setf (aref u i j)
	  (* (aref u i j)
	     (aref (m*m x (m^t v)) i j)
	     (/ (aref (m*m (m*m u v) (m^t v)) i j))))))))

(defun nmf4 (x k)
  (let* ((m (array-dimension x 0))
	 (n (array-dimension x 1))
	 (u (make-test-matrix m k))
	 (v (make-test-matrix k n))
	 (sum1 0.0)
	 (sum2 0.0)
	 (sum3 0.0)
	 (sum4 0.0))
    
    (dotimes (p 100 (values u v))
      
      (let ((uu u)
	    (vv v)) 
	(dotimes (i k)
	  (dotimes (j n)
	    (setf (aref v i j)
	      (*  (aref vv i j)
		  (/ (dotimes (l m sum1)
		       (setf sum1 (+ sum1
				     (* (aref uu l j) (aref x l j)
					(/ (aref (m-times-n uu vv) l j))))))
		     (dotimes (l m sum2)
		       (setf sum2 (+ sum2 (aref uu l i))))))))))
        
      (let ((uu u)
	    (vv v))
	(dotimes (i m)
	  (dotimes (j k)
	    (setf (aref u i j)
	      (* (aref uu i j)
		 (/ (dotimes (l n sum3)
		      (setf sum3 (+ sum3
				    (* (aref vv j l) (aref x i l)
				       (/ (aref (m-times-n uu vv) i l))))))
		    (dotimes (l n sum4)
		      (setf sum4 (+ sum4 (aref vv j l)))))))))))))

(defun nmf-beta (x k)
 
  (let* ((m (array-dimension x 0))
	 (n (array-dimension x 1))
	 (u (make-test-matrix m k))
	 (v (make-test-matrix k n)))
    
    (dotimes (p 100)
      
      (let* ((u^t (m^t u))
	     (w (m*m u^t u)))
	(dotimes (i k)
	  (dotimes (j n)
	    (setf (aref v i j)
	      (* (aref v i j)
		 (a*b-i-j u^t x i j)
		 (/ (a*b-i-j w v i j)))))))                      
      
      (let* ((v^t (m^t v))
	     (ww (m*m u v)))
	(dotimes (i m)
	  (dotimes (j k)
	    (setf (aref u i j)
	      (* (aref u i j)
		 (a*b-i-j x v^t i j)	
		 (/ (a*b-i-j ww v^t i j))))))))
    
    (values u v)))

;(declaim (inline a*b-i-j))

(defun a*b-i-j (a b i j)
 
  (declare (type dmat a))
  (declare (type dmat b))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (assert (= (array-dimension a 1) (array-dimension b 0)))
  
  (let ((k (array-dimension a 1))
	(sum 0.0))
    (dotimes (l k sum)
      (setf sum (+ sum
		   (* (aref a i l) (aref b l j)))))))

(defun aux-gene (n)
  (dotimes (i n)
    (princ ":numeric ")))

(defun aux-gene2 (n)
  (dotimes (i n)
    (format t "~a " i)))

;(defun numeric-matrix (numeric-dataset)
;  (let* ((data (dataset-numeric-points numeric-dataset))
;	 (m (length data))
;	 (n (length (aref data 0)))
;	 (a (make-array (list m n) :element-type 'double-float)))
;    (dotimes (i m a)
;      (dotimes (j n)
;	(setf (aref a i j)
;	  (aref (i-thvector numeric-dataset j) i))))))

(defun pick-up-row (matrix n)
  
  (declare (type dmat matrix))
  
  (let* ((m (array-dimension matrix 1))
	 (v (make-array m :element-type 'double-float)))
    (dotimes (i m v)
      (setf (aref v i) (aref matrix n i)))))

(defun make-index (n)
  (let ((index nil))
    (dotimes (i n (reverse index))
      (setf index (cons i index)))))

(defun numbering (list)
  (let ((n (length list)))
    (mapcar #'list
	    list
	    (make-index n))))

(defun pick-up-words (feature-matrix words-vector n)
  (let ((v (numbering (coerce (pick-up-row feature-matrix n) 'list))))
    (setf v (sort v #'> :key  #'car))
    (dotimes (j 10)
      (format t "~A    ~A~%" (aref words-vector (car (cdr (nth j v)))) (car (nth j v))))))
		
(defun pick-up-column (matrix n)
  
  (declare (type dmat matrix))
  
  (let* ((m (array-dimension matrix 0))
	 (v (make-array m :element-type 'double-float)))
    (dotimes (i m v)
      (setf (aref v i) (aref matrix i n)))))  
	 
(defun pick-up-articles (weight-matrix articles-vector n)
  (let ((v (numbering (coerce (pick-up-column weight-matrix n) 'list))))
    (setf v (sort v #'> :key  #'car))
    (dotimes (j 10)
      (format t "~A   ~A~%" (aref articles-vector (car (cdr (nth j v)))) (car (nth j v))))))

(defun make-article-id (unspecialized-dataset)
  (let* ((data (dataset-points unspecialized-dataset))
	 (n (length data))
	 (x (make-array n)))
    (dotimes (k n x)
      (setf (aref x k) (aref (aref data k) 0)))))

(defun result-words (feature-matrix words-vector)
  (let ((n (array-dimension feature-matrix 0)))
    (dotimes (i n)
      (format t "~%Feature ~A~%" i)
      (pick-up-words feature-matrix words-vector i))))
     

(defun result-articles (weight-matrix articles-vector)
  (let ((n (array-dimension weight-matrix 1)))
    (dotimes (i n)
      (format t "~%Feature ~A~%" i)
      (pick-up-articles weight-matrix articles-vector i))))

(defun search-area (article-id article-area)
  (if (= article-id (aref (car article-area) 0))
      (aref (car article-area) 1)
    (search-area article-id (cdr article-area))))

  (defun pick-up-articles2 (weight-matrix articles-vector article-area n)
  (let ((v (numbering (coerce (pick-up-column weight-matrix n) 'list))))
    (setf v (sort v #'> :key  #'car))
    (dotimes (j 10)
      (format t "~A,~A~%" (aref articles-vector (car (cdr (nth j v))))
	      (search-area (parse-integer (aref articles-vector (car (cdr (nth j v)))))
			   article-area)))))
	      
(defun result-articles2 (weight-matrix articles-vector article-area)
  (let ((n (array-dimension weight-matrix 1)))
    (dotimes (i n)
      (format t "Feature ~A~%" i)
      (pick-up-articles2 weight-matrix articles-vector article-area i))))

(defun set-data (file-name)
  (progn
    (defparameter dataset (read-data-from-file file-name :external-format :utf-8))
    (defparameter article-id (make-article-id dataset))
    
    (defparameter numeric-dataset (pick-and-specialize-data dataset
							    :range 
							    (rest (make-range 
								   (length (dataset-dimensions dataset))))
							    :data-types
							    (make-data-types (length (rest 
										      (make-range (length (dataset-dimensions dataset))))))))
    (defparameter words (dataset-dimensions numeric-dataset))
    (defparameter matrix (numeric-matrix numeric-dataset))))


(defun nmf-analysis (matrix k)
  (progn
    (time (multiple-value-setq (weight feature) (nmf-gamma matrix k)))
    (result-words feature words)
    (result-articles weight article-id)
    (norm (m- matrix (m-times-n weight feature)))))

(defun make-range (n)
  (let ((range nil))
    (dotimes (i n (reverse range))
      (setf range (cons i range)))))

(defun make-data-types (n)
  (let ((data-types nil))
    (dotimes (i n data-types)
      (setf data-types (cons :numeric data-types)))))


(defun pick-up-no-feature-words (feature-matrix words-vector n)
  (let ((v (numbering (coerce (pick-up-row feature-matrix n) 'list))))
    (setf v (sort v #'< :key  #'car))
    (dotimes (j 10)
      (format t "~A~%" (aref words-vector (car (cdr (nth j v))))))))

(defun result-no-feature-words (feature-matrix words-vector)
  (let ((n (array-dimension feature-matrix 0)))
    (dotimes (i n)
      (format t "~%Feature ~A~%" i)
      (pick-up-no-feature-words feature-matrix words-vector i))))

(defun word-distance (feature-matrix word-index1 word-index2 &optional (distance-fn #'euclidean))
  (let ((v1 (pick-up-column feature-matrix word-index1))
	(v2 (pick-up-column feature-matrix word-index2)))
;	(d 0.0))
   ; (dotimes (i (array-dimension feature-matrix 0) (sqrt d))
					; (setf d (+ d (* (- (aref v1 i) (aref v2 i)) (- (aref v1 i) (aref v2 i))))))))
    (funcall distance-fn v1 v2)))

(defun make-word-distance-matrix (feature-matrix)
  (let* ((n (array-dimension feature-matrix 1))
	 (word-distance-matrix (make-array (list n n) :element-type 'double-float)))
    (dotimes (i n word-distance-matrix)
      (dotimes (j n)
	(setf (aref word-distance-matrix i j)
	  (word-distance feature-matrix i j))))))

(defun pick-up-similar-words (word-distance-matrix words-vector word-index)
  (let ((v (numbering (coerce (pick-up-row word-distance-matrix word-index) 'list))))
    (setf v (sort v #'< :key  #'car))
    (dotimes (j 20)
      (format t "~A    ~A~%" (aref words-vector (car (cdr (nth j v)))) (car (nth j v))))))

(defun article-distance (weight-matrix article-index1 article-index2 &optional (distance-fn #'cosine))
  (let ((v1 (pick-up-row weight-matrix article-index1))
	(v2 (pick-up-row weight-matrix article-index2)))
	;(d 0.0))
   ; (dotimes (i (array-dimension weight-matrix 1) (sqrt d))
					;(setf d (+ d (* (- (aref v1 i) (aref v2 i)) (- (aref v1 i) (aref v2 i))))))))
     (funcall distance-fn v1 v2)))

(defun make-article-distance-matrix (weight-matrix)
  (let* ((m (array-dimension weight-matrix 0))
	 (article-distance-matrix (make-array (list m m) :element-type 'double-float)))
    (dotimes (i m article-distance-matrix)
      (dotimes (j m)
	(setf (aref article-distance-matrix i j)
	  (article-distance weight-matrix i j))))))

(defun pick-up-similar-articles (article-distance-matrix articles-vector article-index)
  (let ((v (numbering (coerce (pick-up-column article-distance-matrix article-index) 'list))))
    (setf v (sort v #'< :key  #'car))
    (dotimes (j 10)
      (format t "~A    ~A~%" (aref articles-vector (car (cdr (nth j v)))) (car (nth j v))))))

(defun make-word-data-file (feature-matrix)
  (let ((n (array-dimension feature-matrix 1)))
    (dotimes (i n)
      (format t "~%~A    ~A    ~A~%" (aref feature-matrix 0 i) (aref feature-matrix 1 i)
	      (aref feature-matrix 2 i)))))

(defun make-article-data-file (weight-matrix)
  (let ((m (array-dimension weight-matrix 0)))
    (dotimes (i m)
      (format t "~%~A    ~A    ~A~%" (aref weight-matrix i 0) (aref weight-matrix i 1)
	      (aref weight-matrix i 2)))))

(defun pre-normalize (matrix)
  (let* ((m (array-dimension matrix 0)) 
	 (n (array-dimension matrix 1))  
	 (u (make-array (list m n) :element-type 'double-float)))
    
    (dotimes (i m)
      (dotimes (j n)
	(setf (aref u i j) (aref matrix i j))))
    
    (dotimes (i m u)
      (dotimes (j n)
	(let ((sum 0.0))
	  (setf (aref u i j)
	    (/ (aref matrix i j)
	       (dotimes (l n (sqrt sum))
		 (setf sum (+ sum (expt (aref matrix l j) 2)))))))))))

(defun normalize (weight feature)
  (let* ((k (array-dimension weight 1))
	 (n (array-dimension feature 1))
	 (v (make-array (list k n) :element-type 'double-float)))
    
    (dotimes (i k)
      (dotimes (j n)
	(let ((sum 0.0))
	  (setf (aref v i j)
	    (* (aref feature i j)
	       (dotimes (l k (sqrt sum))
		 (setf sum (+ sum (expt (aref weight l i) 2)))))))))
    
    (values (pre-normalize weight) v)))
    
	  
(defun nmf-gamma0 (x k)
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type dmat x))
  (let* ((m (array-dimension x 0))
	 (n (array-dimension x 1))
	 (u (make-test-matrix m k))
	 (v (make-test-matrix k n)))
    
    (dotimes (p 100)
      
      (let* ((u^t (m^t u))
	     (w (m*m u^t u)))
	(dotimes (i k)
	  (dotimes (j n)
	    (setf (aref v i j)
	      (* (aref v i j)
		 (a*b-i-j u^t x i j)
		 (/ (a*b-i-j w v i j)))))))                      
      
      (let* ((v^t (m^t v))
	     (ww (m*m u v)))
	(dotimes (i m)
	  (dotimes (j k)
	    (setf (aref u i j)
	      (* (aref u i j)
		 (a*b-i-j x v^t i j)	
		 (/ (a*b-i-j ww v^t i j))))))))
    
    (normalize u v)))

(defun nmf-gamma (x k)
 
  (declare (type dmat x))
 
  (let* ((m (array-dimension x 0))
	 (n (array-dimension x 1))
	 (u (make-test-matrix m k))
	 (v (make-test-matrix k n)))
     
    (declare (type dmat u))
    (declare (type dmat v))
    
    (dotimes (p 100)
      
      (let* ((u^t (m^t u))
	     (w (m-times-n u^t u)))
 
	(declare (type dmat u^t))
	(declare (type dmat w))
    	
	
	(dotimes (i k)
	  (dotimes (j n)
	    (setf (aref v i j)
	      (* (aref v i j)
		 (a*b-i-j u^t x i j)
		 (/ (a*b-i-j w v i j)))))))                      
      
      (let* ((v^t (m^t v))
	     (ww (m-times-n u v)))
	
	(declare (type dmat v^t))
	(declare (type dmat ww))	
	
	(dotimes (i m)
	  (dotimes (j k)
	    (setf (aref u i j)
	      (* (aref u i j)
		 (a*b-i-j x v^t i j)	
		 (/ (a*b-i-j ww v^t i j)))))))
      (print (norm (m- x (m-times-n u v)))))
    ;;(normalize u v)))
    (values u v)))

(defun nmf-gamma-norm (x k)
 
  (declare (type dmat x))
 
  (do* ((m (array-dimension x 0))
	(n (array-dimension x 1))
	(u (make-test-matrix m k))
	(v (make-test-matrix k n)))
      ((< (norm (m- x (m-times-n u v))) 0.1) (normalize u v))
   
    (print (norm (m- x (m-times-n u v))))
    (let* ((u^t (m^t u))
	   (w (m-times-n u^t u)))
      (dotimes (i k)
	(dotimes (j n)
	  (setf (aref v i j)
	    (* (aref v i j)
	       (a*b-i-j u^t x i j)
	       (/ (a*b-i-j w v i j)))))))                     
      
    (let* ((v^t (m^t v))
	   (ww (m-times-n u v)))
      (dotimes (i m)
	(dotimes (j k)
	  (setf (aref u i j)
	    (* (aref u i j)
	       (a*b-i-j x v^t i j)	
	       (/ (a*b-i-j ww v^t i j)))))))))

(defun word-clustering-vector (feature-matrix)
  (let* ((n (array-dimension feature-matrix 1))
	 (v (make-array n)))
    (dotimes (i n v)
      (setf (aref v i)
	(max-index (pick-up-column feature-matrix i))))))

(defun feature-vector (feature-matrix)
  (let* ((n (array-dimension feature-matrix 1))
	 (v (make-array n)))
    (dotimes (i n v)
      (setf (aref v i)
	(aref feature-matrix (aref (word-clustering-vector feature-matrix) i) i)))))
      
(defun max-index (vector)
  (let* ((v (coerce vector 'list))
	 (w (numbering v)))
    (setf w (sort w #'> :key  #'car))
    (second (car w))))

(defun word-clustering (feature-matrix words-vector)
  (mapcar #'list
	  (coerce (word-clustering-vector feature-matrix) 'list)
	  (coerce words-vector 'list)
	  (coerce (feature-vector feature-matrix) 'list)))

(defun result-word-clustering (feature-matrix words-vector)
  (let ((v (sort (sort (word-clustering feature-matrix words-vector) #'> :key #'third) #'< :key #'first)))
    (dotimes (i (length words-vector))
      (format t "~%~A   ~A   ~A~%" 
	      (first (nth i v)) (second (nth i v)) (third (nth i v))))))
	      

(defun article-clustering-vector (weight-matrix)
  (let* ((m (array-dimension weight-matrix 0))
	 (v (make-array m)))
    (dotimes (i m v)
      (setf (aref v i)
	(max-index (pick-up-row weight-matrix i))))))

(defun weight-vector (weight-matrix)
  (let* ((m (array-dimension weight-matrix 0))
	 (v (make-array m)))
    (dotimes (i m v)
      (setf (aref v i)
	(aref weight-matrix i (aref (article-clustering-vector weight-matrix) i))))))

(defun article-clustering (weight-matrix articles-vector)
  (mapcar #'list
	  (coerce (article-clustering-vector weight-matrix) 'list)
	  (coerce articles-vector 'list)
	  (coerce (weight-vector weight-matrix) 'list)))

(defun result-article-clustering (weight-matrix articles-vector)
  (let ((v (sort (sort (article-clustering weight-matrix articles-vector) #'> :key #'third) #'< :key #'first)))
    (dotimes (i (length articles-vector))
      (format t "~%~A   ~A   ~A~%" 
	      (first (nth i v)) (second (nth i v)) (third (nth i v))))))

(defun consensus-matrix0 (feature-matrix)
  (let* ((n (array-dimension feature-matrix 1))
	 (v (word-clustering-vector feature-matrix))
	 (c (make-array (list n n))))
    (dotimes (i n c)
      (dotimes (j n)
	(setf (aref c i j)
	  (if (= (aref v i) (aref v j))
	      1
	    0))))))

(defun consensus-matrix (weight-matrix)
  (let* ((n (array-dimension weight-matrix 0))
	 (v (article-clustering-vector weight-matrix))
	 (c (make-array (list n n))))
    (dotimes (i n c)
      (dotimes (j n)
	(setf (aref c i j)
	  (if (= (aref v i) (aref v j))
	      1
	    0))))))

(defun average-consensus-matrix (matrix k &optional (count 50))
 
  (let* ((m (array-dimension matrix 0))
	 (n (array-dimension matrix 1))
	 (average-c (make-array (list m m) :initial-element 0.0d0 :element-type 'double-float))
	 (c (make-array (list m m)))
	 (u (make-array (list m k)))
	 (v (make-array (list k n))))
    
    (dotimes (l count average-c)
      (multiple-value-setq (u v) (nmf-gamma matrix k))
      (setf c (consensus-matrix u))
      (dotimes (i m)
	(dotimes (j m)
	    (setf (aref average-c i j)
	      (/ (+ (* l (aref average-c i j)) (aref c i j))
		 (+ l 1))))))))

(defun make-distance-matrix (similarity-matrix)
  (let* ((n (array-dimension similarity-matrix 0))
	 (distance-matrix (make-array (list n n) :element-type 'double-float)))
    (dotimes (i n distance-matrix)
      (dotimes (j n)
	(setf (aref distance-matrix i j) (- 1.0 (aref similarity-matrix i j)))))))
      
(defun nmf-delta (x k)
  
  (declare (type dmat x))
 
  (let* ((m (array-dimension x 0))
	 (n (array-dimension x 1))
	 (u (make-test-matrix m k))
	 (v (make-test-matrix k n)))
    
    (declare (type dmat u))
    (declare (type dmat v))
    
    (dotimes (p 100 (values u v))
      
      (let* ((u^t (m^t u))
	     (w (m-times-n u^t u)))
	(dotimes (i k)
	  (dotimes (j n)
	    (setf (aref v i j)
	      (* (aref v i j)
		 (a*b-i-j u^t x i j)
		 (/ (a*b-i-j w v i j)))))))                      
      
      (let* ((v^t (m^t v))
	     (ww (m-times-n u v)))
	(dotimes (i m)
	  (dotimes (j k)
	    (setf (aref u i j)
	      (* (aref u i j)
		 (a*b-i-j x v^t i j)	
		 (/ (a*b-i-j ww v^t i j)))))))
    
      (multiple-value-setq (u v) (normalize u v)))))

(defun m-times-n (a b)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (type dmat a))
  (declare (type dmat b))
 
  (assert (= (array-dimension a 1) (array-dimension b 0)))
  (let* ((m (array-dimension a 0))
	 (n (array-dimension b 1))
	 (c (make-array (list m n) :element-type 'double-float)))
    (declare (type dmat c))
    (dotimes (i m c)
      (dotimes (j n)
	(setf (aref c i j) (a*b-i-j a b i j))))))

(defun rho-k (matrix k)
  (let* ((avc (average-consensus-matrix matrix k))
	 (d (make-distance-matrix avc))
	 (u (cophenetic-matrix d)))
    (cophenetic-cc d u)))

(defun norm-nmf (matrix k)
  (multiple-value-setq (u v) (nmf-gamma matrix k))
  (norm (m- matrix (m-times-n u v))))

(defun KL-nmf (x k)
  
  (declare (type dmat x))
 
  (let* ((m (array-dimension x 0))
	 (n (array-dimension x 1))
	 (u (make-test-matrix m k))
	 (v (make-test-matrix k n)))
    
    (dotimes (p 30 (normalize u v))
      
      (let ((uv (m-times-n u v)))
	(dotimes (i m)
	  (dotimes (l k)
	    (setf (aref u i l)
	      (* (aref u i l)
		 (aux-kl-1 x uv v i l))))))
      
      (let ((old-u (copy-matrix u)))
	(dotimes (i m)
	  (dotimes (l k)
	    (setf (aref u i l)
	      (* (aref u i l)
		 (/ (vector-sum (pick-up-column old-u l))))))))
      
      (let ((uv (m-times-n u v)))
	(dotimes (l k)
	  (dotimes (j n)
	    (setf (aref v l j)
	      (* (aref v l j)
		 (aux-kl-2 x uv u l j)))))))))

(defun aux-kl-1 (x uv v i l)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (type dmat x))
  (declare (type dmat uv))
  (declare (type dmat v))
  
  (let* ((n (array-dimension x 1))
	 (a (make-array n :element-type 'double-float)))
    
    (declare (type dvec a))  
    
    (dotimes (j n (vector-sum a))
      (setf (aref a j)
	(* (aref x i j)
	   (/ (aref uv i j))
	   (aref v l j))))))
	      
(defun aux-kl-2 (x uv u l j)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (type dmat x))
  (declare (type dmat uv))
  (declare (type dmat u)) 
    
  (let* ((m (array-dimension x 0))
	 (b (make-array m :element-type 'double-float)))
    
    (declare (type dvec b)) 
     
    (dotimes (i m (vector-sum b))
      (setf (aref b i)
	(* (aref x i j)
	   (/ (aref uv i j))
	   (aref u i l))))))

(defun copy-matrix (matrix)
  
  (declare (type dmat matrix))
  
  (let* ((m (array-dimension matrix 0))
	 (n (array-dimension matrix 1))
	 (w (make-array (list m n) :element-type 'double-float)))
    (dotimes (i m w)
      (dotimes (j n)
	(setf (aref w i j) (aref matrix i j))))))

(defun partition-matrix (matrix word-clustering-vector)
 
  (declare (type dmat matrix)) 
  (declare (type (SIMPLE-ARRAY T (*)) word-clustering-vector))
  
  (let* ((m (array-dimension matrix 0))
	 ;(n (array-dimension matrix 1))
	 (p (number-of-n word-clustering-vector 0))
	 (q (number-of-n word-clustering-vector 1))
	 (a (make-array (list m p) :element-type 'double-float))
	 (b (make-array (list m q) :element-type 'double-float)))
    
    (dotimes (i m)
      (dotimes (j p)
	(setf (aref a i j)
	  (aref (jth-n-column matrix word-clustering-vector j 0) i))))
    
    (dotimes (i m (values a b))
      (dotimes (j q)
	(setf (aref b i j)
	  (aref (jth-n-column matrix word-clustering-vector j 1) i))))))

(defun number-of-n (clustering-vector n)
  
  (declare (type (SIMPLE-ARRAY T (*)) clustering-vector))
  
  (let ((sum 0))
    (dotimes (i (length clustering-vector) sum)
      (when (= (svref clustering-vector i) n)
	(setf sum (+ 1 sum))))))

(defun correspond-column-number (word-clustering-vector j p)
  
  (declare (type (SIMPLE-ARRAY T (*)) word-clustering-vector))
  
  (let ((sum 0))
    (do ((i 0 (+ i 1)))
	((= (- sum 1) j) (- i 1))
      (when (= (svref word-clustering-vector i) p)
	(setf sum (+ 1 sum))))))

(defun jth-n-column (matrix word-clustering-vector j n)
  (pick-up-column matrix (correspond-column-number word-clustering-vector j n)))

(defun with-index (matrix)
  (let* ((m (array-dimension matrix 0))
	 (n (array-dimension matrix 1))
	 (matrix-with-index (make-array 
			     (list (+ m 1) (+ n 1))
			     :element-type 'double-float)))
    
    (setf (aref matrix-with-index 0 0) -1.0d0)
    
    (dotimes (j n)
      (setf (aref matrix-with-index 0 (+ j 1)) (coerce j 'double-float)))
    
    (dotimes (i m)
      (setf (aref matrix-with-index (+ i 1) 0) (coerce i 'double-float)))
    
    (dotimes (i m matrix-with-index)
      (dotimes (j n)
	(setf (aref matrix-with-index (+ i 1) (+ j 1))
	  (aref matrix i j))))))

(defun without-index (matrix-with-index)
  (let* ((m (- (array-dimension matrix-with-index 0) 1))
	 (n (- (array-dimension matrix-with-index 1) 1))
	 (matrix (make-array (list m n) :element-type 'double-float)))
    
    (dotimes (i m matrix)
      (dotimes (j n)
	(setf (aref matrix i j) (aref matrix-with-index (+ i 1) (+ j 1)))))))

(defun make-new-words-vector (words-vector word-clustering-vector p)
  (let* ((n (number-of-n word-clustering-vector p))
	 (new-words-vector (make-array n)))
    (dotimes (i n new-words-vector)
      (setf (aref new-words-vector i) 
	(aref words-vector (correspond-column-number word-clustering-vector i p))))))

(defun hierarchical-nmf-analysis (matrix words-vector)
  (progn
    (time (multiple-value-setq (weight feature) (nmf-gamma matrix 2)))
    (setf new-words-vector (make-new-words-vector words-vector (word-clustering-vector feature) 0))
    (multiple-value-setq (m0 m1) (partition-matrix matrix (word-clustering-vector feature)))
    (result-words feature words-vector)
    (values (array-dimension m0 1) (array-dimension m1 1))))

(defun enhanced-matrix0 (matrix)
  (let* ((m (array-dimension matrix 0))
	 (n (array-dimension matrix 1))
	 (new-matrix (make-array (list m n) :element-type 'double-float)))
    (dotimes (i m new-matrix)
      (dotimes (j n)
	(setf (aref new-matrix i j)
	  (expt (aref matrix i j) 2))))))


(defun enhanced-matrix1 (matrix)
  (let* ((m (array-dimension matrix 0))
	 (n (array-dimension matrix 1))
	 (new-matrix (make-array (list m n) :element-type 'double-float)))
    (dotimes (i m new-matrix)
      (dotimes (j n)
	(if (= (aref matrix i j) 0.0d0)
	    (setf (aref new-matrix i j) 0.01d0)
	  (setf (aref new-matrix i j)
	    (expt (aref matrix i j) 2)))))))

(defun enhanced-matrix (matrix)
  (let* ((m (array-dimension matrix 0))
	 (n (array-dimension matrix 1))
	 (new-matrix (make-array (list m n) :element-type 'double-float)))
    (dotimes (i m new-matrix)
      (dotimes (j n)
	(if (= (aref matrix i j) 0.0d0)
	    (setf (aref new-matrix i j) 0.01d0)
	  (setf (aref new-matrix i j) (aref matrix i j)))))))

(defun dfreq (matrix word-id)
  (assert (<= 0 word-id (- (array-dimension matrix 1) 1)))
  (let ((v (pick-up-column matrix word-id)))
    (number-of-non-zero-elements v)))

(defun number-of-non-zero-elements (vector)
  (let ((n (length vector))
	(sum 0))
    (dotimes (i n sum)
      (setf sum (if (zerop (aref vector i))
		    (+ sum 0)
		  (+ sum 1))))))

(defun tf*idf (matrix)
  (let* ((m (array-dimension matrix 0))
	 (n (array-dimension matrix 1))
	 (new-matrix (make-array (list m n) :element-type 'double-float)))
    
    (dotimes (i m new-matrix)
      (dotimes (j n)
	(setf (aref new-matrix i j)
	  (* (aref matrix i j) 
	     (log (/ m (dfreq matrix j)))))))))
		    
(defun normalized-tf*idf (matrix)
  (let* ((m (array-dimension matrix 0))
	 (n (array-dimension matrix 1))
	 (new-matrix (make-array (list m n) :element-type 'double-float)))
    
    (dotimes (i m new-matrix)
      (dotimes (j n)
	(setf (aref new-matrix i j)
	  (* (tf2 matrix i j) 
	     (log (/ m (dfreq matrix j)))))))))

(defun tf (matrix i j)
  (let ((v (pick-up-row matrix i)))
    (/ (log (+ 1.0 (aref matrix i j)))
       (log (vector-sum v)))))

(defun tf2 (matrix i j)
  (let ((v (pick-up-row matrix i)))
    (/ (log (+ 1.0 (aref matrix i j)))
       (log (number-of-non-zero-elements v)))))

(defun word-weighting (matrix)
  (let* ((m (array-dimension matrix 0))
	 (n (array-dimension matrix 1))
	 (new-matrix (make-array (list m n) :element-type 'double-float)))
    
    (dotimes (i m new-matrix)
      (dotimes (j n)
	(setf (aref new-matrix i j)
	  (* (local-weight matrix i j)
	     (global-weight matrix j)))))))
	 
(defun local-weight (matrix i j)
  (if (zerop (aref matrix i j))
      0
    (+ 1.0 (sqrt (- (aref matrix i j) 0.5)))))

(defun global-weight (matrix j)
  (sqrt (- (/ (vector-sum (pick-up-column matrix j))
	      (number-of-non-zero-elements (pick-up-column matrix j)))
	   0.90)))

(defun normalization (matrix)
  (let* ((m (array-dimension matrix 0))
	 (n (array-dimension matrix 1))
	 (new-matrix (make-array (list m n) :element-type 'double-float)))
    
    (dotimes (i m new-matrix)
      (dotimes (j n)
	(setf (aref new-matrix i j)
	  (* (aref matrix i j)
	     (normalization-factor matrix i)))))))

(defun normalization-factor (matrix i)
  (let ((n (array-dimension matrix 1))
	(sum 0.0))
    
    (dotimes (j n (/ (sqrt sum)))
      (setf sum (+ sum (expt (aref matrix i j) 2))))))

(defun scale-transformation (matrix scale)
  (let* ((m (array-dimension matrix 0))
	 (n (array-dimension matrix 1))
	 (new-matrix (make-array (list m n) :element-type 'double-float)))
    
    (dotimes (i m new-matrix)
      (dotimes (j n)
	(setf (aref new-matrix i j)
	  (* (aref matrix i j)
	     (expt 10 scale)))))))

(defun +phi (matrix i j)
  (/ (non-zero (aref matrix i j))
     (number-of-non-zero-elements (pick-up-row matrix i))))

(defun -phi (matrix i j)
  (/ (non-zero (aref matrix i j))
     (number-of-non-zero-elements (pick-up-column matrix j))))

(defun delta (matrix i h)
  (let ((n (array-dimension matrix 1))
	(sum 0.0))
    (dotimes (j n sum)
      (setf sum (+ sum 
		   (* (+phi matrix i j)
		      (-phi matrix h j)))))))

(defun cluster-number (matrix)
  (let ((m (array-dimension matrix 0))
	(sum 0.0))
    (dotimes (i m sum)
      (setf sum (+ sum (delta matrix i i))))))

(defun non-zero (number)
  (assert (numberp number))
  (if (zerop number)
      0.0d0
    1.0d0))

(defun feature-weighting-matrix (matrix word-id)
  (let* ((n (array-dimension matrix 1))
	 (feature-weighting-matrix (make-array (list n n) :initial-element 0.0d0
					       :element-type 'double-float)))
    (dotimes (i n feature-weighting-matrix)
      (setf (aref feature-weighting-matrix i i)
	(if (= i word-id)
	    (adjusting-factor matrix word-id)
	    1.0d0)))))

(defun word-power-vector (matrix)
  (let* ((n (array-dimension matrix 1))
	 (word-power-vector (make-array n :element-type 'double-float)))
    (dotimes (i n word-power-vector)
      (setf (aref word-power-vector i)
	(vector-sum (pick-up-column matrix i))))))

(defun adjusting-factor (matrix word-id)
  (let* ((word-power-vector (word-power-vector matrix))
	 (max-power (aref (sort word-power-vector #'>) 0)))
    (/ (* 1.7 max-power) 
       (aref (word-power-vector matrix) word-id))))

(defun unit-matrix (size)
  (let ((unit (make-array (list size size) :initial-element 0.0d0
			  :element-type 'double-float)))
    (dotimes (i size unit)
      (setf (aref unit i i) 1.0d0))))

(defun theme-weighting-matrix (matrix adjusting-vector)
  (let* ((n (array-dimension matrix 1))
	 (theme-weighting-matrix (unit-matrix n)))
    
    (dotimes (i n theme-weighting-matrix)
      (setf (aref theme-weighting-matrix i i)
	(aref adjusting-vector i)))))

(defun adjusting-vector (matrix theme-word-id-list)
  (let* ((n (array-dimension matrix 1))
	 (adjusting-vector (make-array n :initial-element 1.0d0
				       :element-type 'double-float)))
    (dotimes (i n adjusting-vector)
      (when (member i theme-word-id-list)
	(setf (aref adjusting-vector i)
	  (adjusting-factor matrix i))))))

(defun theme-weighted-matrix0 (matrix theme-word-id-list)
  (let* ((adjusting-vector (adjusting-vector matrix theme-word-id-list))
	 (theme-weighting-matrix (theme-weighting-matrix matrix adjusting-vector)))
    (m-times-n matrix theme-weighting-matrix)))

(defun nmf-search0 (word-id)
  (let ((theme-weighted-matrix (theme-weighted-matrix tf*idf*cosine-matrix (list word-id))))
    (nmf-analysis theme-weighted-matrix 1)))

(defun word->id (word)
  (do ((i 0 (+ i 1)))
      ((equal word (dimension-name (aref words i))) i)))
	 
(defun nmf-search (word)
  (let ((theme-weighted-matrix (theme-weighted-matrix tf*idf*cosine-matrix (list (word->id word)))))
    (nmf-analysis theme-weighted-matrix 1)))

(defun theme-weighted-matrix (matrix theme-word-id-list)
  (let* ((adjusting-vector (adjusting-vector matrix theme-word-id-list))
	 (theme-weighted-matrix (copy-matrix matrix)))
    (dotimes (i (array-dimension matrix 0) theme-weighted-matrix)
      (dotimes (j (array-dimension matrix 1))
	(when (/= (aref adjusting-vector j) 1.0d0)
	  (setf (aref theme-weighted-matrix i j)
	    (* (aref theme-weighted-matrix i j) (aref adjusting-vector j))))))))

(defun test ()
  (dotimes (i 1202)
    (nmf-search0 i)))

(defun article-power-vector (matrix)
  (let* ((m (array-dimension matrix 0))
	 (article-power-vector (make-array m :element-type 'double-float)))
    (dotimes (i m article-power-vector)
      (setf (aref article-power-vector i)
	(vector-sum (pick-up-row matrix i))))))

(defun article-adjusting-factor (matrix article-id) 
  (let* ((article-power-vector (article-power-vector matrix))
	 (max-power (aref (sort article-power-vector #'>) 0)))
    (/ (* 1.7 max-power) 
       (aref (article-power-vector matrix) article-id))))

(defun article-adjusting-vector (matrix theme-article-id-list)
  (let* ((m (array-dimension matrix 0))
	 (article-adjusting-vector (make-array m :initial-element 1.0d0
				       :element-type 'double-float)))
    (dotimes (i m article-adjusting-vector)
      (when (member i theme-article-id-list)
	(setf (aref article-adjusting-vector i)
	  (article-adjusting-factor matrix i))))))

(defun article-theme-weighted-matrix (matrix theme-article-id-list)
  (let* ((article-adjusting-vector (article-adjusting-vector matrix theme-article-id-list))
	 (article-theme-weighted-matrix (copy-matrix matrix)))
    (dotimes (i (array-dimension matrix 0) article-theme-weighted-matrix)
      (dotimes (j (array-dimension matrix 1))
	(when (/= (aref article-adjusting-vector i) 1.0d0)
	  (setf (aref article-theme-weighted-matrix i j)
	    (* (aref article-theme-weighted-matrix i j) (aref article-adjusting-vector i))))))))

(defun nmf-article-search0 (article-no)
   (let ((article-theme-weighted-matrix (article-theme-weighted-matrix tf*idf*cosine-matrix (list article-no))))
     (nmf-analysis article-theme-weighted-matrix 1)))

(defun test2 ()
  (dotimes (i 100)
    (nmf-article-search0 i)))

(defun article->id (article-no)
  (do ((i 0 (+ i 1)))
      ((equal article-no (aref article-id i)) i)))

(defun nmf-article-search (article)
  (let ((article-theme-weighted-matrix 
	 (article-theme-weighted-matrix tf*idf*cosine-matrix (list (article->id article)))))
    (nmf-analysis article-theme-weighted-matrix 1)))

(defun sum (f g)
  #'(lambda (x) (+ (funcall f x) (funcall g x))))

(defun f (n)
  #'(lambda (x) (+ x n)))

(defun g (n)
  #'(lambda (x) (* x n)))

(let ((y 7))
  (defun scope-test (x)
    (list x y)))

(let ((counter 0))
  (defun new-id () (incf counter))
  (defun reset-id () (setq counter 0)))

(defun last1 (lst)
  (car (last lst)))

(defun single-list (lst)
  (and (consp lst) (not (cdr lst))))

(defun append1 (lst obj)
  (append lst (list obj)))

(defun conc1 (lst obj)
  (nconc lst (list obj)))

(defmacro nil! (var)
  (list 'setq var nil))

(defun sparseness (vector)
  (let ((n (length vector)))
    (/ (- (sqrt n)
	  (/ (vector-sum vector)
	     (sqrt (vector-sum (vector-square vector)))))
       (- (sqrt n) 1))))

(defun average-sparseness (matrix)
  (let* ((m (array-dimension matrix 0))
	 (v (make-array m :element-type 'double-float)))
    (dotimes (i m (vector-mean v))
      (setf (aref v i) (sparseness (pick-up-row matrix i))))))

(defmacro while (test &rest body)
  `(do ()
       ((not ,test))
     ,@body))

(defun non-negative-p (n)
  (not (minusp n)))

(defun non-negative-vector-p (vector)
  (let ((n (length vector)))
    (dotimes (i n t)
      (when (minusp (aref vector i))
	(return nil)))))

(defun l1 (vector)
  (let ((sum 0.0))
    (dotimes (i (length vector) sum)
      (setf sum (+ sum (abs (aref vector i)))))))

(defun l2 (vector)
  (let ((sum 0.0))
    (dotimes (i (length vector) sum)
      (setf sum (+ sum (expt (aref vector i) 2))))))

(defun s-x (x)
  (/ (- (sqrt (length x))
	(/ (l1 x) (sqrt (l2 x))))
     (- (sqrt (length x)) 1)))

(defun dot (a b)
  (assert (= (length a) (length b)))
  (let ((sum 0.0d0))
    (dotimes (i (length a) sum)
      (setf sum (+ sum (* (aref a i) (aref b i)))))))

(defun v- (a b)
  (assert (= (length a) (length b)))
  (let ((v (make-array (length a) :element-type 'double-float)))
    (dotimes (i (length a) v)
      (setf (aref v i) (- (aref a i) (aref b i))))))
	       
(defun vector-shift (vector const)
  (let* ((n (length vector))
	 (w (make-array n :element-type 'double-float)))
    (dotimes (i n w)
      (setf (aref w i) (- (aref vector i) const)))))

(defun alpha (x s m)
  (/ (+ (- (dot m (v- s m)))
	(sqrt (- (expt (dot m (v- s m)) 2)
		 (* (l2 (v- s m))
		    (- (l2 m) (l2 x))))))
     (l2 (v- s m))))

(defun set-l1 (x sparseness)
  (* (sqrt (l2 x))
     (- (sqrt (length x))
	(* sparseness
	   (- (sqrt (length x)) 1)))))

(defun set-m (x z sparseness)
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
  (dotimes (i (length s) z)
    (if (minusp (aref s i))
	(setf z (remove-duplicates (append z (list i)))))))

(defun set-s0 (x sparseness)
  (let ((l1-norm (set-l1 x sparseness))
	(sum (vector-sum x))
	(s (make-array (length x) :element-type 'double-float))
	(n (length x)))
    (dotimes (i (length s) s)
      (setf (aref s i)
	(+ (aref x i)
	   (/ (- l1-norm sum) n))))))

(defun set-s1 (x s m)
  (v+ m (alpha-v (alpha x s m) (v- s m))))

(defun set-s2 (z s)
  (dotimes (i (length s) s)
    (if (member i z)
	(setf (aref s i) 0.0))))

(defun set-s3 (x z s sparseness)
  (let ((c (calculate-c x s z sparseness)))
	    
    (dotimes (i (length s) s)
      (if (not (member i z))
	  (setf (aref s i) (- (aref s i) c))))))

(defun calculate-c (x s z sparseness)
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
  (let* ((n (length x))
	 (s (make-array n :element-type 'double-float))
	 (z nil)
	 (m (make-array n :element-type 'double-float)))
    
    (setf s (set-s0 x sparseness))
    (setf m (set-m x z sparseness))
    (setf s (set-s1 x s m))
    
    (while (not (non-negative-vector-p s))
      (setf z (set-z z s))
      (setf s (set-s2 z s))
      (setf s (set-s3 x z s sparseness))
      (setf s (set-s1 x s m))
      (setf m (set-m x z sparseness))
      (setf s (set-s1 x s m)))
    s))

(defun nmf-sc0 (v k)
 
  (declare (type dmat v))
 
  (let* ((m (array-dimension v 0))
	 (n (array-dimension v 1))
	 (w (make-test-matrix m k))
	 (h (make-test-matrix k n)))
     
    (declare (type dmat w))
    (declare (type dmat h))
    
    (dotimes (p 100)
      
      (let* ((w^t (m^t w))
	     (w^tw (m-times-n w^t w)))
 
	(declare (type dmat w^t))
	(declare (type dmat w^tw))
    	
	
	(dotimes (i k)
	  (dotimes (j n)
	    (setf (aref h i j)
	      (* (aref h i j)
		 (a*b-i-j w^t v i j)
		 (/ (a*b-i-j w^tw h i j)))))))                      
      
      (let* ((h^t (m^t h))
	     (wh (m-times-n w h)))
	
	(declare (type dmat h^t))
	(declare (type dmat wh))	
	
	(dotimes (i m)
	  (dotimes (j k)
	    (setf (aref w i j)
	      (* (aref w i j)
		 (a*b-i-j v h^t i j)	
		 (/ (a*b-i-j wh h^t i j))))))))
    
    (values w h)))

(defun m- (a b)
  
  (declare (type dmat a))
  (declare (type dmat b))
  
  (assert (and (= (array-dimension a 0) (array-dimension b 0))
	       (= (array-dimension b 1) (array-dimension b 1))))
  (let ((m (make-array (list (array-dimension a 0)
			     (array-dimension a 1))
		       :element-type 'double-float)))
    (dotimes (i (array-dimension a 0) m)
      (dotimes (j (array-dimension a 1))
	(setf (aref m i j) (- (aref a i j) (aref b i j)))))))

(defun alpha-m (alpha a)
  
  (declare (type dmat a))
  
  (let ((m (make-array (list (array-dimension a 0)
			     (array-dimension a 1))
		       :element-type 'double-float)))
    (dotimes (i (array-dimension a 0) m)
      (dotimes (j (array-dimension a 1))
	(setf (aref m i j) (* alpha (aref a i j)))))))

(defun set-w (v w h iteration)
  (m- w
      (alpha-m (/ (expt 2 iteration)) 
	       (m-times-n (m- (m-times-n w h) v)
			  (m^t h)))))

(defun proj-w (w sparseness)
  (let ((new-w (make-array (list (array-dimension w 0)
				 (array-dimension w 1))
			   :element-type 'double-float)))
    
    (dotimes (j (array-dimension w 1) new-w)
      	(let ((w-j (projection-operator (pick-up-column w j) sparseness)))
	  (dotimes (i (array-dimension w 0))
	    (setf (aref new-w i j) (aref w-j i)))))))

(defun nmf-sc (v k &optional (iteration 100))
 
  (declare (type dmat v))
  
  (let* ((m (array-dimension v 0))
	 (n (array-dimension v 1))
	 (w (make-test-matrix m k))
	 (h (make-test-matrix k n)))
     
    (declare (type dmat w))
    (declare (type dmat h))
    
    (setf w (proj-w w 0.9))
    
    (dotimes (p iteration)
     
      (setf w (set-w v w h p))
      (setf w (proj-w w 0.9))
    
      (let* ((w^t (m^t w))
	     (w^tw (m-times-n w^t w)))
 
	(declare (type dmat w^t))
	(declare (type dmat w^tw))
    	
	(dotimes (i k)
	  (dotimes (j n)
	    (setf (aref h i j)
	      (* (aref h i j)
		 (a*b-i-j w^t v i j)
		 (/ (a*b-i-j w^tw h i j)))))))
      
      (print (norm (m- v (m-times-n w h)))))
    
    (values w h)))

(defpackage :linear-regression
  (:use :cl
        :hjs.learn.read-data
	:hjs.util.matrix
        :hjs.util.meta)
  (:import-from :hc 
		#:i-thvector
		#:square-sum
		#:product-sum
		#:vector-shift
		#:vector-mean)
  (:export #:mlr
	   #:residual-vector
	   #:residual-quantile-vector
	   #:residual-std-err
           #:std-err-vector
	   #:t-value-vector
	   #:d.f
	   #:pt
	   #:pt-value-vector
	   #:r^2
	   #:adjusted-r^2
	   #:f-value
	   #:pf
	   #:pf-value))

(in-package :linear-regression)

(defun aux-quantile (x n)
  (if (= n (floor n))
      (aref x (floor n))
    (+  (* (- (ceiling n) n) (aux-quantile x (floor n)))
	(* (- n (floor n)) (aux-quantile x (ceiling n))))))

(defun quantile (vector q)
  (let ((x (sort vector #'<=)))
    (aux-quantile x (* q (- (length vector) 1)))))

(defun s-i-j (x-i x-j)
  (product-sum (vector-shift x-i (vector-mean x-i))
	       (vector-shift x-j (vector-mean x-j))))

(defun make-matrix-s (numeric-dataset range)
  (assert (eq (type-of numeric-dataset) 'numeric-dataset))
  (assert (<= 2 (length range) (length (aref (dataset-numeric-points numeric-dataset) 0))))
  (let* ((n (- (length range) 1))
	 (s (make-array (list n n) :element-type 'double-float)))
    (dotimes (i n s)
      (dotimes (j n)
	(setf (aref s i j)
	  (s-i-j (i-thvector numeric-dataset (nth i range))
		 (i-thvector numeric-dataset (nth j range))))))))

(defun solve-system-of-equations (matrix vector)
  (m-times-v (m^-1 matrix) vector))

(defun m-times-v (matrix vector)
  (let* ((n (array-dimension vector 0))
	 (a (make-array n)))
    (dotimes (i n a)
      (setf (aref a i)
	(loop for k below n 
	    sum (* (aref matrix i k) (aref vector k)))))))
	  
(defun make-vector-c (numeric-dataset range)
  (assert (eq (type-of numeric-dataset) 'numeric-dataset))
  (let* ((n (- (length range) 1))
	 (c (make-array n :element-type 'double-float)))
    (dotimes (i n c)
      (setf (aref c i)
	(s-i-j (i-thvector numeric-dataset (nth i range))
	       (i-thvector numeric-dataset (first (last range))))))))
			 
(defun mlr-intercept (numeric-dataset range)
  (assert (eq (type-of numeric-dataset) 'numeric-dataset))
  (let* ((s (make-matrix-s numeric-dataset range))
	 (c (make-vector-c numeric-dataset range))
	 (coefficients (solve-system-of-equations s c))
	 (n (first (array-dimensions coefficients)))
	 (intercept (vector-mean (i-thvector numeric-dataset (first (last range))))))
    (dotimes (i n intercept)
      (setf intercept
	(- intercept
	   (* (aref coefficients i) (vector-mean (i-thvector numeric-dataset (nth i range)))))))))

(defun mlr (numeric-dataset range)
  (assert (eq (type-of numeric-dataset) 'numeric-dataset))
  (let* ((s (make-matrix-s numeric-dataset range))
	 (c (make-vector-c numeric-dataset range))
	 (coefficients (solve-system-of-equations s c))
	 (n (first (array-dimensions coefficients)))
	 (intercept (mlr-intercept numeric-dataset range))
	 (answer (make-array (+ n 1) :element-type 'double-float)))
    (dotimes (i (+ n 1) answer)
      (setf (aref answer i)
	(if (= i 0)
	    intercept
	  (aref coefficients (- i 1)))))))

(defun fitted (numeric-dataset range)
  (assert (eq (type-of numeric-dataset) 'numeric-dataset))
  (let* ((n (length (dataset-numeric-points numeric-dataset)))
	 (coef (mlr numeric-dataset range))
	 (y-hat (make-array n)))
    (dotimes (i n y-hat)
      (setf (aref y-hat i)
	(+ (aref coef 0)
	   (loop
	       for k below (1- (length range))
	       sum (* (aref coef (+ k 1))
		      (aref (i-thvector numeric-dataset (nth k range)) i))))))))

(defun residual-vector (numeric-dataset range)
  (assert (eq (type-of numeric-dataset) 'numeric-dataset))
  (let* ((n (length (dataset-numeric-points numeric-dataset)))
	 (y (i-thvector numeric-dataset (first (last range))))
	 (y-hat (fitted numeric-dataset range))
	 (e (make-array n)))
    (dotimes (i n e)
      (setf (aref e i) (- (aref y i) (aref y-hat i))))))

(defun residual-quantile-vector (numeric-dataset range)
  (let ((v (sort (residual-vector numeric-dataset range) #'<=))
	(w (make-array 5)))
    (dotimes (i 5 w)
      (setf (aref w i) (quantile v (* 0.25 i))))))

(defun capital-se (numeric-dataset range)
  (assert (eq (type-of numeric-dataset) 'numeric-dataset))
  (square-sum (residual-vector numeric-dataset range)))

(defun small-se (numeric-dataset range)
  (assert (eq (type-of numeric-dataset) 'numeric-dataset))
  (/ (capital-se numeric-dataset range)
     (- (length (dataset-numeric-points numeric-dataset)) (- (length range) 1) 1)))

(defun residual-std-err (numeric-dataset range)
  (sqrt (small-se numeric-dataset range)))

(defun std-err-intercept (numeric-dataset range)
  (assert (eq (type-of numeric-dataset) 'numeric-dataset))
  (let ((n (length (dataset-numeric-points numeric-dataset)))
	(s^-1 (m^-1 (make-matrix-s numeric-dataset range))))
    (sqrt (* (small-se numeric-dataset range)
	     (+ (/ 1 n)
		(loop 
		    for i below (1- (length range)) 
		    sum
		      (loop
			  for j below (1- (length range))
			  sum  (* (vector-mean (i-thvector numeric-dataset (nth i range)))
				  (vector-mean (i-thvector numeric-dataset (nth j range)))
				  (aref s^-1 i j)))))))))

(defun std-err-of-ith-coef (numeric-dataset range i)
  (assert (eq (type-of numeric-dataset) 'numeric-dataset))
  (sqrt (* (aref (m^-1 (make-matrix-s numeric-dataset range)) i i)
	   (small-se numeric-dataset range))))

(defun std-err-vector (numeric-dataset range)
  (assert (eq (type-of numeric-dataset) 'numeric-dataset))
  (let ((s (make-array (length range))))
    (dotimes (i (length range) s)
      (if (= i 0)
	  (setf (aref s i) (std-err-intercept numeric-dataset range))
	(setf (aref s i) (std-err-of-ith-coef numeric-dataset range (- i 1)))))))

(defun t-value-of-intercept (numeric-dataset range)
  (assert (eq (type-of numeric-dataset) 'numeric-dataset))
  (/ (mlr-intercept numeric-dataset range)
     (std-err-intercept numeric-dataset range)))

(defun t-value-of-ith-coef (numeric-dataset range i)
  (assert (eq (type-of numeric-dataset) 'numeric-dataset))
  (/ (aref (mlr numeric-dataset range) (+ i 1))
     (std-err-of-ith-coef numeric-dataset range i)))

(defun t-value-vector (numeric-dataset range)
  (assert (eq (type-of numeric-dataset) 'numeric-dataset))
  (let ((t-vector (make-array (length range))))
    (dotimes (i (length range) t-vector)
      (if (= i 0)
	  (setf (aref t-vector i) (t-value-of-intercept numeric-dataset range))
	(setf (aref t-vector i) (t-value-of-ith-coef numeric-dataset range (- i 1)))))))
		
(defun r^2 (numeric-dataset range)
  (assert (eq (type-of numeric-dataset) 'numeric-dataset))
  (/ (square-sum (vector-shift (fitted numeric-dataset range) 
			       (vector-mean (i-thvector numeric-dataset (first (last range))))))
     (+ (square-sum (vector-shift (fitted numeric-dataset range) 
				  (vector-mean (i-thvector numeric-dataset (first (last range))))))
	(capital-se numeric-dataset range))))

(defun adjusted-r^2 (numeric-dataset range)
  (assert (eq (type-of numeric-dataset) 'numeric-dataset))
  (- 1
     (/ (* (capital-se numeric-dataset range)
	   (- (length (dataset-numeric-points numeric-dataset)) 1))
	(* (square-sum (vector-shift (i-thvector numeric-dataset (first (last range))) 
				     (vector-mean (i-thvector numeric-dataset (first (last range))))))
	   (- (length (dataset-numeric-points numeric-dataset)) (- (length range) 1) 1)))))

(defun aic (numeric-dataset range)
  (+ (* (length (dataset-numeric-points numeric-dataset))
	(+ (log (/ (* 2 pi (capital-se numeric-dataset range))
		   (length (dataset-numeric-points numeric-dataset))))
	   1))
     (* 2 (+ (length range) 1))))

(defun even-i (x v)
  (if (= v 1)
      (sqrt x)
    (+ (even-i x (- v 1))
       (/ (even-u x (- v 1)) (- v 1)))))

(defun even-u (x v)
  (if (= v 1)
      (* 0.5 (sqrt x) (- 1 x))
    (* (even-u x (- v 1))
       (* (- (* 2 v) 1)
	  (- 1 x)
	  (/ 1 (- (* 2 v) 2))))))

(defun odd-i (x v)
  (if (= v 0.5)
	(- 1 (* (/ 2 pi)
		(atan (sqrt (/ (- 1 x) x)))))
						 
    (+ (odd-i x (- v 1))
       (/ (odd-u x (- v 1)) (- v 1)))))
	  
(defun odd-u (x v)
  (if (= v 0.5)
      (/ (sqrt (* x (- 1 x))) pi)
    (* (odd-u x (- v 1))
       (* (- (* 2 v) 1)
	  (- 1 x)
	  (/ 1 (- (* 2 v) 2))))))

(defun sign (x)
  (if (>= x 0)
      1
    -1))

(defun pt (t-value phi)
  (let ((x (/ (* t-value t-value) (+ phi (* t-value t-value))))
	(v (/ phi 2)))
    (if (evenp phi)
	(+ 0.5 (* (sign t-value) (even-i x v) 0.5))
      (+ 0.5 (* (sign t-value) (odd-i x v) 0.5)))))

(defun p-value (t-value phi)
  (* 2 (- 1 (pt (abs t-value) phi))))

(defun d.f (numeric-dataset range)
  (assert (eq (type-of numeric-dataset) 'numeric-dataset))
  (- (length (dataset-numeric-points numeric-dataset)) (length range)))

(defun pt-value-vector (numeric-dataset range)
  (assert (eq (type-of numeric-dataset) 'numeric-dataset))
  (let ((p-vector (make-array (length range)))
	(t-vector (t-value-vector numeric-dataset range)))
    (dotimes (i (length range) p-vector)
      (setf (aref p-vector i)
	(p-value (aref t-vector i) (d.f numeric-dataset range))))))

(defun f-value (numeric-dataset range)
  (/ (* (r^2 numeric-dataset range) (d.f numeric-dataset range))
     (* (- 1 (r^2 numeric-dataset range)) (- (length range) 1))))

(defun i-11 (x m n)
  (if (and (= m 0.5) (= n 0.5))
      (- 1 (* (/ 2 pi)
	      (atan (sqrt (/ (- 1 x) x)))))
    (if (= m 0.5)
	(+ (i-11 x 0.5 (- n 1))
	   (/ (u-11 x 0.5 (- n 1))
	      (- n 1)))
      (- (i-11 x (- m 1) n)
	 (/ (u-11 x (- m 1) n)
	    (- m 1))))))
	
(defun u-11 (x m n)
  (if (and (= m 0.5) (= n 0.5))
      (/ (sqrt (* x (- 1 x))) pi)
    (if (= m 0.5)
	(* (u-11 x 0.5 (- n 1))
	   (/ (- n 0.5)
	      (- n 1))
	   (- 1 x))
      (* (u-11 x (- m 1) n)
	 (/ (+ m n -1) (- m 1))
	 x))))


(defun i-10 (x m n)
  (if (and (= m 0.5) (= n 1.0))
      (sqrt x)
    (if (= m 0.5)
	(+ (i-10 x 0.5 (- n 1))
	   (/ (u-10 x 0.5 (- n 1))
	      (- n 1)))
      (- (i-10 x (- m 1) n)
	 (/ (u-10 x (- m 1) n)
	    (- m 1))))))
	
(defun u-10 (x m n)
  (if (and (= m 0.5) (= n 1.0))
      (* (- 1 x)
	 (sqrt x)
	 0.50)
    (if (= m 0.5)
	(* (u-10 x 0.5 (- n 1))
	   (/ (- n 0.5)
	      (- n 1))
	   (- 1 x))
      (* (u-10 x (- m 1) n)
	 (/ (+ m n -1) (- m 1))
	 x))))

(defun i-01 (x m n)
  (if (and (= m 1.0) (= n 0.5))
      (- 1
	 (sqrt (- 1 x)))
    (if (= m 1.0)
	(+ (i-01 x 1.0 (- n 1))
	   (/ (u-01 x 1.0 (- n 1))
	      (- n 1)))
      (- (i-01 x (- m 1) n)
	 (/ (u-01 x (- m 1) n)
	    (- m 1))))))

(defun u-01 (x m n)
  (if (and (= m 1.0) (= n 0.5))
      (* x
	 (sqrt (- 1 x))
	 0.50)
    (if (= m 1.0)
	(* (u-01 x 1.0 (- n 1))
	   (/  n
	       (- n 1))
	   (- 1 x))
      (* (u-01 x (- m 1) n)
	 (/ (+ m n -1) (- m 1))
	 x))))

(defun i-00 (x m n)
  (if (and (= m 1.0) (= n 1.0))
      x
    (if (= m 1.0)
	(+ (i-00 x 1.0 (- n 1))
	   (/ (u-00 x 1.0 (- n 1))
	      (- n 1)))
      (- (i-00 x (- m 1) n)
	 (/ (u-00 x (- m 1) n)
	    (- m 1))))))

(defun u-00 (x m n)
  (if (and (= m 1.0) (= n 1.0))
      (* x (- 1 x))
    (if (= m 1.0)
	(* (u-00 x 1.0 (- n 1))
	   (/  n
	       (- n 1))
	   (- 1 x))
      (* (u-00 x (- m 1) n)
	 (/ (+ m n -1) (- m 1))
	 x))))

(defun pf (f phi-1 phi-2)
  (let ((x (/ (* phi-1 f)
	      (+ phi-2 (* phi-1 f))))
	(m (/ phi-1 2))
	(n (/ phi-2 2)))
    (cond ((and (oddp phi-1) (oddp phi-2))
	   (i-11 x m n))
	  ((and (oddp phi-1) (evenp phi-2))
	   (i-10 x m n))
	  ((and (evenp phi-1) (oddp phi-2))
	   (i-01 x m n))
	  ((and (evenp phi-1) (evenp phi-2))
	   (i-00 x m n)))))

(defun pf-value (numeric-dataset range)
  (- 1.0
     (pf (f-value numeric-dataset range)
	 (- (length range) 1)
	 (d.f numeric-dataset range))))


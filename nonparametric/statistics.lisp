;; base statistics and small utility for nonparametric Bayes
(defpackage :nonparameteric.statistics
  (:nicknames :nonpara.stat)
  (:use :cl :hjs.util.matrix :hjs.util.vector :hjs.util.meta)
  (:export :make-adarray
   
	   :unit-random
	   :bernoulli
	   :exp-random
	   :beta-random
	   :gamma-random
	   :normal-random
	   :normal-density
	   :chi-square-random
	   :randomize-choice
	   :randomize-slice
	   :jackup-logged-prob
	   :shuffle-vector
	   :random-elt
	   :normalize!
	   :get-n-best
	   :safe-exp
	   :safe-expt
	   :stirling-number
	   :dirichlet-random
	   :binomial-random
	   :cauchy-random

	   :gamma-function
	   :beta-function
	   :loggamma
	   :digamma
	   :trigamma
	   
	   :multivariate-normal-density
	   :%multivariate-normal-density
	   :multivariate-normal-logged-density
	   :%multivariate-normal-logged-density
	   :multivariate-normal-random
	   :LUed-wishart-random

	   :outer-product
	   :map-matrix-cell
	   :map-matrix-cell!
	   :crossproduct
	   :cholesky-decomp
	   
	   :*most-negative-exp-able-float*
	   :*most-positive-exp-able-float*
	   
	   :*randomize-trace*))

(in-package :nonparameteric.statistics)

(defmacro make-adarray (dim &rest args)
  `(make-array ,dim :fill-pointer t :adjustable t ,@args))

(defconstant *most-negative-exp-able-float* #.(log least-positive-double-float))
(defconstant *most-positive-exp-able-float* #.(log most-positive-double-float))

(defmacro safe-exp (x)
  (let ((y (gensym)))
    `(let ((,y ,x))
       (exp (cond ((< ,y *most-negative-exp-able-float*)
		   #+ignore
		   (format t "underflow ~S~%" ,y)
		   *most-negative-exp-able-float*)
		  ((> ,y *most-positive-exp-able-float*)
		   #+ignore
		   (format t "overflow ~S~%" ,y)
		   *most-positive-exp-able-float*)
		  (t ,y))))))

(defmacro safe-expt (base power)
  `(handler-case (expt ,base ,power)
     (floating-point-underflow () least-positive-double-float)
     (floating-point-overflow  () most-positive-double-float)))

(defun random-elt (array)
  (aref array (random (length array))))

(defun shuffle-vector (v)
  "Return vector with elements shuffled"
  (loop
      finally (return v)
      for i from (length v) downto 1
      as j = (random i)
      do (rotatef (aref v j) (aref v (1- i)))))

;; sampling of random variables
(defun bernoulli (base-p)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float base-p))
  (assert (<= 0 base-p 1))
  (if (>= (the double-float (unit-random)) base-p)
      0d0
    1d0))

(defun unit-random ()
  "A random number in the range \(0, 1]."
  (let ((rand (random 1.0d0)))
    (declare (type double-float rand))
    (if (zerop rand) 1.0d0 rand)))

(defun exp-random ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (* -1.0d0 (the double-float (log (the double-float (unit-random))))))

#+ignore
(defun normal-random (average std)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float average std))
  (let ((fn (if (zerop (random 2)) #'cos #'sin)))
    (declare (type function fn))
    (+ (the double-float average)
       (the double-float
	 (* std
	    (the double-float (sqrt (* -2.0d0 (the double-float (log (the double-float (unit-random)))))))
	    (the double-float (funcall fn (* 2.0d0 pi (the double-float (unit-random))))))))))

;; Box-Muller transform -- make 2 standard-normal-random variable at 1 time by 2 unit-random
(let (tmp-value)
  (declare (type (or null double-float) tmp-value))
  (defun normal-random (average std)
    (declare (optimize (speed 3) (safety 0) (debug 0))
	     (type double-float average std))
    (let ((gauss
	   (if tmp-value
	       (prog1 tmp-value (setf tmp-value nil))
	     (let* ((a (unit-random))
		    (b (unit-random))
		    (sqr (sqrt (* -2.0d0 (the double-float (log a)))))
		    (angle (* 2.0d0 pi b)))
	       (declare (type double-float a b sqr angle))
	       (setf tmp-value
		 (* sqr (the double-float (sin angle))))
	       (* sqr (the double-float (cos angle)))))))
      (declare (type double-float gauss))
      (+ (the double-float average)
	 (the double-float
	   (* std gauss))))))

(defun normal-density (average std x)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float average std x))
  (let* ((norm (/ (the double-float (- x average)) std))
	 (sq (* norm norm)))
    (declare (type double-float norm sq))
    (max least-positive-double-float
	 (/ (the double-float (/ (safe-exp (/ sq -2.0d0)) #.(sqrt (* 2.0d0 pi)))) std))))

(defun gamma-random (a rate)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float a rate))
  (assert (> a 0d0))
  (/ (the double-float
       (cond ((< a 1d0)
	      (loop with b double-float = (/ (+ a #.(exp 1.0d0)) #.(exp 1.0d0))
		  for u1 double-float = (unit-random) and u2 double-float = (unit-random)
		  for y double-float = (* b u1)
		  do (if (<= y 1.0d0)
			 (let ((z (handler-case (expt y (/ 1d0 a))
				    (floating-point-underflow () (return-from gamma-random
								   least-positive-double-float)))))
			   (declare (type double-float z))
			   (when (< u2 (the double-float (exp (- z))))
			     (return (* a z))))
		       (let ((z (- (the double-float (log (/ (- b y) a))))))
			 (declare (type double-float z))
			 (when (<= u2 (the double-float (expt z (1- a))))
			   (return (* a z)))))))
	     ((= a 1d0)
	      ;; equivalent to exp-random
	      (exp-random))
	     (t (let* ((a1 (- a 1d0))
		       (b (/ (- a (/ (* a 6d0))) a1))
		       (c (/ 2d0 a1))
		       (d (+ c 2d0)))
		  (declare (type double-float a1 b c d))
		  (loop for u1 double-float = (unit-random) and u2 double-float = (unit-random)
		      for w double-float = (* b (/ u1 u2)) do
			(when (or (<= (+ (* c u2) (- d) w (/ w)) 0d0)
				  (< (+ (* c (the double-float (log u2))) (- (the double-float (log w))) w -1d0) 0d0))
			  (return (* a1 w))))))))
       rate))

(defun beta-random (a b)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float a b))
  (let ((swapp (> a b)))
    (when swapp (rotatef a b))
    (let ((result
	   (cond ((> a 1.0d0)
		  (let* ((alpha (+ a b))
			 (beta (sqrt (/ (the double-float (- alpha 2.0d0))
					(the double-float (- (the double-float (* 2.0d0 a b)) alpha)))))
			 (gamma (+ a (/ beta))))
		    (declare (type double-float alpha beta gamma))
		    (loop for u1 double-float = (unit-random) and u2 double-float = (unit-random)
			for v double-float = (* beta (log (/ u1 (- 1.0d0 u1))))
			for w double-float = (* a (exp v))
			for z double-float = (* u1 u1 u2)
			for r double-float = (- (* gamma v) #.(log 4.0d0))
			for s double-float = (- (+ a r) w)
			for tt double-float = (log z)
			while (and (< (+ s #.(+ 1.0d0 (log 5.0d0))) (* 5.0d0 z))
				   (< s tt)
				   (< (+ r (* alpha (log (/ alpha (+ b w))))) tt))
			finally (return (/ w (+ b w))))))
		 (t ;; (<= (min a b) 1.0d0)
		  (let* ((alpha (+ a b))
			 (beta (/ b))
			 (delta (- (+ 1.0d0 a) b))
			 (kappa1 (/ (* delta (+ #.(/ 72.0d0) (* #.(/ 24.0d0) b)))
				    (- (* a beta) #.(/ 7.0d0 9.0d0))))
			 (kappa2 (if (zerop delta)
				     most-positive-double-float
				   (+ 0.25d0 (* (+ 0.5d0 (/ 0.25d0 delta)) b)))))
		    (declare (type double-float alpha beta delta kappa1 kappa2))
		    (loop for u1 double-float = (unit-random) and u2 double-float = (unit-random)
			for v double-float = (* beta (log (/ u1 (- 1.0d0 u1))))
			for w double-float = (* a (exp v))
			for z double-float = (* u1 u1 u2)
			for status symbol =
					  (if (>= u1 0.5d0)
					      (cond ((<= z 0.25d0) 'done)
						    ((< z kappa2) 'check)
						    (t 'back))
					    (if (>= (- (+ (* 0.25d0 u2) z) (* u1 u2)) kappa1)
						'back
					      'check))
			while (or (eq status 'back)
				  (and (eq status 'check)
				       (< (- (* alpha (+ (log (/ alpha (+ b w))) v))
					     #.(log 4.0d0))
					  (log z))))
			finally (return (/ w (+ b w)))))))))
      (declare (type double-float result))
      (if swapp (- 1.0d0 result) result))))

;; chi-square(k) == gamma(k/2,1/2)
(defun chi-square-random (k)
  (gamma-random (dfloat (/ k 2)) 0.5d0))

(defvar *randomize-trace* nil)

(defun randomize-choice (count-array sum-with-new-table)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float sum-with-new-table)
	   (type (array double-float (*)) count-array))
  (when *randomize-trace*
    (format t "~A ~A~%" count-array sum-with-new-table))
  (let ((rand (if (zerop sum-with-new-table)
		  0d0
		(max least-positive-double-float
		     (* (the double-float (unit-random)) sum-with-new-table)))))
    (declare (type double-float rand))
    (loop
	for prob double-float across count-array
	for i fixnum from 0 do
	  (decf rand prob)
	  (when (<= rand 0d0)
	    (return i))
	finally (return -1))))

(defun randomize-slice (count-array sum-with-new-table end)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float sum-with-new-table)
	   (type (array double-float (*)) count-array)
	   (type fixnum end))
  (when *randomize-trace*
    (format t "~A ~A~%" count-array sum-with-new-table))
  (let ((rand (if (zerop sum-with-new-table)
		  0d0
		(max least-positive-double-float
		     (* (the double-float (unit-random)) sum-with-new-table)))))
    (declare (type double-float rand))
    (loop
      	for i fixnum from 0 upto end
	for prob double-float = (aref count-array i) do				
	  (decf rand prob)
	  (when (<= rand 0d0)
	    (return i))
	finally (return -1))))

(defun jackup-logged-prob (logged-prob p-max &optional (index (length logged-prob)))
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type (array double-float (*)) logged-prob)
	   (type double-float p-max)
	   (type fixnum index))
  (let ((jack (- #.(/ (log most-positive-double-float) 2) p-max))
	(sum 0d0))
    (declare (type double-float jack sum))
    (loop for i fixnum from 0 below index
	for upp double-float = (if (zerop (aref logged-prob i))
				   0d0
				 (safe-exp (+ (aref logged-prob i) jack))) do
	  (incf sum upp)
	  (setf (aref logged-prob i) upp))
    sum))

(defun normalize! (vector)
  (let ((sum (reduce #'+ vector)))
    (map-into vector #'(lambda (x) (/ x sum)) vector))
  vector)

(defun get-n-best (row n)
  (let ((ans (make-array n :element-type t))
	(tmp (make-array n :element-type 'double-float :initial-element 0d0)))
    (flet ((update (pos data)
	     (let (change changep changed)
	       (loop for j from 0 to (1- n)
		   for comp = (aref tmp j) do
		     (cond (change
			    (when (null (aref ans j))
			      (setf (aref ans j) changep)
			      (setf (aref tmp j) changed)
			      (return-from update))
			    (rotatef changep (aref ans j))
			    (rotatef changed (aref tmp j)))
			   ((> data comp)
			    (setf change t)
			    (setf changep (aref ans j)
				  (aref ans j) pos)
			    (setf changed (aref tmp j)
				  (aref tmp j) data)))))))
      (loop for i from 0 to (1- (length row))
	  for data = (aref row i) do
	    (update i data)
	  finally (return (values ans tmp))))))

;;; multivariate randoms
(defun crossproduct (mat &optional result)
  (declare (type dmat mat))
  (let* ((m (array-dimension mat 0))
	 (n (array-dimension mat 1))
	 (ans (or result (make-dmat m m))))
    (declare (type fixnum m n)
	     (type dmat ans))
    (loop for i of-type array-index from 0 below m do
	  (loop for j of-type array-index from 0 upto i do
		(let ((prod 0d0))
		  (declare (type double-float prod))
		  (loop for k from 0 below n do
			(incf prod (* (aref mat i k)
				      (aref mat j k))))
		  (setf (aref ans i j) prod)
		  (setf (aref ans j i) prod))))
    ans))

(defun fact-L (A)
  (let ((LU (LU-factorization A))
	(m (array-dimension A 1))
	(n (array-dimension A 0)))
    (loop for i from 0 below m do
	  (loop
	      initially (setf (aref LU i i) 1d0)
	      for j from (1+ i) below n do
		(setf (aref LU i j) 0d0)))
    LU))

(defun fact-U (A)
  (let ((LU (LU-factorization A))
	(m (array-dimension A 1)))
    (loop for i from 0 below m do
	  (loop 
	      for j from 0 below i do
		(setf (aref LU i j) 0d0)))
    LU))

(defun LU-factorization (A)
  (declare (type dmat A))
  (let* ((Ar #+mkl (transpose A)
             #-mkl (hjs.util.matrix::mat2array (transpose A)))
         (m (array-dimension A 1))
         (n (array-dimension A 0))
         (lda (max 1 m))
         (ipiv (make-array (min m n) :element-type '(unsigned-byte 32)))
         (info 0))
    (assert (= m n))
    (setq info 
      (car (last
            (multiple-value-list
             #+mkl (mkl.lapack::dgetrf m n Ar lda ipiv info)
             #-mkl (lapack::dgetrf m n Ar lda ipiv info)))))
    (assert (= 0 info))
    #+mkl (transpose Ar)
    #-mkl (transpose (hjs.util.matrix::array2mat Ar m))))
  
(defun cholesky-decomp (mat &optional (result (make-array (array-dimensions mat)
							  :element-type 'double-float
							  :initial-element 0d0)
					      result-passed-p))
  (declare (type dmat mat result))
  (let ((m (array-dimension mat 0)))
    (declare (type fixnum m))
    (when result-passed-p
      (loop for i of-type array-index from 0 below m do
	    (loop for j of-type array-index from (1+ i) below m do
		  (setf (aref result i j) 0d0))))
    (loop for i of-type array-index from 0 below m do
	  (loop for j of-type array-index from 0 below i do
		;;; sym check
		(setf (aref result i j)
		  (/ (loop with a_ij double-float = (aref mat i j)
			 for k from 0 below j do
			   (decf a_ij (* (aref result i k)
					 (aref result j k)))
			 finally (return a_ij))
		     (aref result j j)))
	      finally (setf (aref result i i)
			(sqrt (loop with a_ii double-float = (aref mat i i)
				  for k from 0 below i do
				    (decf a_ii (expt (aref result i k) 2))
				  finally (return a_ii))))))
    result))

(defun multivariate-normal-density  (averages sqrt-sigma dvec)
  (%multivariate-normal-density averages (m^-1 sqrt-sigma) dvec))

(defun %multivariate-normal-density (averages inv-sqrt-sigma dvec)
  (declare (type dvec averages dvec)
	   (type dmat inv-sqrt-sigma))
  ;; inv-sqrt-sigma is triagle matrix -- so determinant is just a product of diag
  (let ((base (loop with ans double-float = 1d0
		  for i of-type array-index from 0 below (array-dimension inv-sqrt-sigma 0) do
		    (setf ans (* ans (aref inv-sqrt-sigma i i)))
		  finally (return ans)))
	(sqrted (m*v inv-sqrt-sigma
		     (vcv dvec averages :c #'-))))
    (declare (type dvec sqrted))
    (let ((l (length averages))
	  (ans (* base (safe-exp (* (vdotv sqrted sqrted) -0.5d0)))))
      (declare (type fixnum l)
	       (type double-float ans))
      (max least-positive-double-float
	   (loop
	     (when (< l 770)
	       (return (/ ans (expt #.(sqrt (* 2.0d0 pi)) l))))
	     (setf ans (/ ans #.(expt (sqrt (* 2.0d0 pi)) 770)))
	     (when (zerop ans)
	       (return-from %multivariate-normal-density least-positive-double-float))
	     (decf l 770))))))

(defun multivariate-normal-logged-density  (averages sqrt-sigma dvec)
  (%multivariate-normal-logged-density averages (m^-1 sqrt-sigma) dvec))

(defun %multivariate-normal-logged-density (averages inv-sqrt-sigma dvec)
  (declare (type dvec averages dvec)
	   (type dmat inv-sqrt-sigma))
  ;; inv-sqrt-sigma is triagle matrix -- so determinant is just a product of diag
  (let* ((base (loop with ans double-float = 0d0
		   for i of-type array-index from 0 below (array-dimension inv-sqrt-sigma 0) do
		     (incf ans (log (aref inv-sqrt-sigma i i)))
		   finally (return ans)))
	 (tmp (vcv dvec averages :c #'-))
	 (sqrted (m*v inv-sqrt-sigma tmp)))
    (declare (type dvec sqrted)
	     (type double-float base))
    (let ((l (length averages))
	  (ans (+ base (* (the double-float (vdotv sqrted sqrted)) -0.5d0))))
      (declare (type fixnum l)
	       (type double-float ans))
      (- ans (* l #.(log (sqrt (* 2.0d0 pi)))))
      )))

(defun multivariate-normal-random (averages sqrt-sigma &optional result)
  (let ((dice (or result (make-dvec (length averages)))))
    (loop for i from 0 below (length averages) do
	  (setf (aref dice i) (normal-random 0d0 1d0)))
    (v+ averages (m*v sqrt-sigma dice) dice)))

;; wishart distribution: inverse of variance matrix of gauss distribution
(defun LUed-wishart-random (df dim)
  (declare (type fixnum df dim))
  (assert (> df dim))
  (let ((Z (make-dmat dim dim)))
    (declare (type dmat Z))
    (loop for i of-type array-index from 0 below dim do
	  (loop for j of-type array-index from 0 below dim do
		(setf (aref Z i j)
		  (cond ((= i j) (sqrt (chi-square-random (- df i))))
			((< i j) 0d0)
			(t (normal-random 0d0 1d0))))))
    Z))

(defun outer-product (x y)
  (declare (type dvec x y))
  (let* ((s (length x))
	 (ans (make-dmat s s)))
    (loop for i of-type array-index from 0 below s do
	  (loop for j of-type array-index from 0 below s do
		(setf (aref ans i j) (* (aref x i) (aref y j)))))
    ans))
    
(defun map-matrix-cell (fn dmat)
  (declare (type dmat dmat))
  (let ((ans (make-dmat (array-dimension dmat 0) (array-dimension dmat 1))))
    (declare (type dmat ans))
    (loop for i of-type array-index from 0 below (array-dimension ans 0) do
	  (loop for j of-type array-index from 0 below (array-dimension ans 1) do
		(setf (aref ans i j) (funcall fn (aref dmat i j)))))
    ans))

(defun map-matrix-cell! (fn dmat)
  (declare (type dmat dmat))
  (loop for i of-type array-index from 0 below (array-dimension dmat 0) do
	(loop for j of-type array-index from 0 below (array-dimension dmat 1) do
	      (setf (aref dmat i j) (funcall fn (aref dmat i j)))))
  dmat)

(let ((memo (make-hash-table :test #'equal)))
  (setf (gethash (list 1 1) memo) 1)
  (setf (gethash (list 0 0) memo) 1)
  (defun stirling-number (n m)
    (assert (and (>= n 0) (>= m 0)))
    (cond ((> m n) 0)
	  ((zerop m) 0)
	  (t (let* ((key (list n m))
		    (found (gethash key memo)))
	       (if found
		   found
		 (let ((new (+ (stirling-number (1- n) (1- m))
			       (* (1- n) (stirling-number (1- n) m)))))
		   (setf (gethash key memo) new)
		   new)))))))

(defun dirichlet-random (vector &optional result)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type (array double-float (*)) vector))
  (let ((ans (or result (make-array (length vector) :element-type 'double-float)))
	(sum 0d0)
	(l (length vector)))
    (declare (type double-float sum)
	     (type (array double-float (*)) ans)
	     (type fixnum l))
    (loop for i fixnum from 0 below l
	for rnd double-float = (gamma-random (aref vector i) 1d0) do
	  (incf sum rnd)
	  (setf (aref ans i) rnd))
    (loop for i fixnum from 0 below l
	for tmp double-float = (/ (aref ans i) sum) do
	  (setf (aref ans i) (if (< tmp least-positive-double-float) least-positive-double-float tmp))
	finally (return ans))))

(defun binomial-random (times p)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type fixnum times)
	   (type double-float p))
  (let ((ans 0))
    (declare (type fixnum ans))
    (dotimes (i times)
      (when (< (the double-float (random 1.0d0)) p)
	(incf ans)))
    ans))

(defun cauchy-random (average scale)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float average scale))
  (let ((u (unit-random)))
    (declare (type double-float u))
    (+ average (* scale
		  (tan (* pi (- u 0.5d0)))))))
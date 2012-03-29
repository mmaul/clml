;;; -*- mode: lisp; syntax: common-lisp -*-

;;; Peter Salvi, 2008

;;; Notes
;;; -----
;;; * Numbers are not converted to (double) floats, for better accuracy with
;;;   whole number data. This should be OK, since double data will generate
;;;   double results (the number type is preserved).
;;; * Some functions that operate on sorted sequences have two versions, FUN
;;;   and FUN-ON-SORTED, the latter of which assumes that the sequence is
;;;   already sorted.
;;; * Distributions are implemented as functions that return CLOS objects,
;;;   that have methods like CDF (cumulative distribution function),
;;;   DENSITY, QUANTILE and RAND (gives a random number according to the
;;;   given distribution).
;;; * Places marked with TODO are not optimal or not finished.

(in-package :statistics)


;;;;;;;;;;;;;;;;;;;;;
;;; Data analysis ;;;
;;;;;;;;;;;;;;;;;;;;;

;;; Functions on one-valued data

(defgeneric mean (obj))
(defmethod mean ((sequence sequence))
  (/ (reduce #'+ sequence) (length sequence)))

(def-on-sorted median (sequence)
  (let ((n (length sequence)))
    (if (evenp n)
	(let ((n/2 (/ n 2)))
	  (/ (+ (elt sequence (1- n/2)) (elt sequence n/2)) 2))
	(elt sequence (/ (1- n) 2)))))

(def-on-sorted discrete-quantile (sequence cuts)
  "The function gives the mean of the two numbers closest to the given ratio
if the ratio does not give an exact (whole) position. This is what LISP-STAT
does, but returning
  \(LINEAR-COMBINATION \(ELT SEQUENCE Q) R \(ELT SEQUENCE (1+ Q)))
may be better. More on this at http://mathworld.wolfram.com/Quantile.html.

CUTS is a single number or a list of numbers, each in the interval [0,1]."
  (let ((n (length sequence)))
    (flet ((section (cut)
	     (multiple-value-bind (q r)
		 (floor (* (1- n) cut))
	       (if (zerop r)
		   (elt sequence q)
		   (/ (+ (elt sequence q) (elt sequence (1+ q))) 2)))))
      (if (listp cuts)
	  (mapcar #'section cuts)
	  (section cuts)))))

(def-on-sorted five-number-summary (sequence)
  (discrete-quantile-on-sorted sequence '(0 1/4 1/2 3/4 1)))

(defun range (sequence)
  (- (reduce #'max sequence) (reduce #'min sequence)))

(def-on-sorted interquartile-range (sequence)
  (- (discrete-quantile-on-sorted sequence 3/4)
     (discrete-quantile-on-sorted sequence 1/4)))

(defun sum-on-deviation (function sequence)
  (let ((mean (mean sequence)))
    (reduce (lambda (sum x) (+ sum (funcall function (- x mean)))) sequence
	    :initial-value 0)))

(defun mean-deviation (sequence)
  (/ (sum-on-deviation #'abs sequence) (length sequence)))

(defgeneric variance (obj))
(defmethod variance ((sequence sequence))
  (/ (sum-on-deviation #'sqr sequence) (length sequence)))

(defun standard-deviation (sequence &key populationp)
  "Sample standard deviation; or population standard deviation if POPULATIONP."
  (sqrt (coerce (/ (sum-on-deviation #'sqr sequence)
		   (if populationp (length sequence) (1- (length sequence))))
		'double-float)))


;;; Functions on two-valued data

(defun covariance (seq1 seq2)
  (let ((mean1 (mean seq1))
	(mean2 (mean seq2))
	(n1 (length seq1))
	(n2 (length seq2)))
    (assert (= n1 n2) (seq1 seq2)
	    "The two sequences must have the same length.")
    (/ (apply #'+ (map 'list (lambda (x y) (* (- x mean1) (- y mean2)))
		       seq1 seq2))
       (1- n1))))

(defun linear-regression (seq1 seq2)
  "Fits a line y = A + Bx on the data points from SEQ1 x SEQ2. Returns (A B)."
  (let ((b (/ (covariance seq1 seq2) (sqr (standard-deviation seq1)))))
    (list (- (mean seq2) (* b (mean seq1))) b)))

(defun correlation-coefficient (seq1 seq2)
  (/ (covariance seq1 seq2)
     (* (standard-deviation seq1)
	(standard-deviation seq2))))

(defun rank-list (sequence)
  "Returns the indices of the values as if sorted in ascending order.

TODO: This could be done more efficiently."
  (let ((sorted (sort (remove-duplicates sequence) #'<)))
    (map 'list (lambda (x) (1+ (position x sorted))) sequence)))

;;;this fcn is added by naganuma@msi
(defun rank-list-with-tie (sequence)
  "Returns the indices of the values as if sorted in ascending order
with considering tie of rank."
  (let ((sorted (sort (copy-seq sequence) #'<)))
    (map 'list
      (lambda (x)
        (let ((x-begin (1+ (position x sorted :from-end nil)))
              (x-end (1+ (position x sorted :from-end t))))
          (cond ((= x-begin x-end)
                 x-begin)
                ((> x-end x-begin)
                 (/ (+ x-begin x-end) 2)) ; mean of rank
                (t (error "illegal sorted seq.")))))
      sequence)))

(defun spearman-rank-correlation (seq1 seq2)
  "Gives the correlation coefficient based on just the relative size of the
given values."
  (let ((n1 (length seq1))
        (n2 (length seq1)))
    (assert (= n1 n2) (seq1 seq2)
	    "The two sequences must have the same length.")
    ;;;  below is edited by naganuma@msi
    ;;;  ref: http://aoki2.si.gunma-u.ac.jp/lecture/Soukan/spearman.html
    (let* ((rank1 (rank-list-with-tie seq1))
           (rank2 (rank-list-with-tie seq2))
           (n3-n (- (expt n1 3) n1))
           (t1 (/ (- n3-n
                     (loop for i in (remove-duplicates rank1 :test #'=)
                         as n = (count i rank1 :test #'=)
                         sum (- (expt n 3) n))) 12))
           (t2 (/ (- n3-n 
                     (loop for i in (remove-duplicates rank2 :test #'=)
                         as n = (count i rank2 :test #'=)
                         sum (- (expt n 3) n))) 12))
           (sum-d (apply #'+ (mapcar (lambda (x y) (sqr (- x y)))
                                     rank1 rank2))))
      (if (and (= t1 0) (= t2 0))
          (- 1 (/ (* 6 sum-d) n3-n))
        (/ (- (+ t1 t2) sum-d) (* 2 (sqrt (* t1 t2))))))))

(defun kendall-rank-correlation (seq1 seq2)
  (let ((n1 (length seq1))
        (n2 (length seq1)))
    (assert (= n1 n2) (seq1 seq2)
	    "The two sequences must have the same length.")
    ;;;  below is edited by naganuma@msi
    ;;;  ref: http://aoki2.si.gunma-u.ac.jp/lecture/Soukan/kendall.html
    (let* ((rank1 (rank-list-with-tie seq1))
           (rank2 (rank-list-with-tie seq2))
           (denom (* n1 (1- n1)))
           (t1 (loop for i in (remove-duplicates rank1 :test #'=)
                   as n = (count i rank1 :test #'=)
                   sum (/ (* n (1- n)) 2)))
           (t2 (loop for i in (remove-duplicates rank2 :test #'=)
                   as n = (count i rank2 :test #'=)
                   sum (/ (* n (1- n)) 2)))
           (sigma-p (loop for i from 0 below n1
                        sum 
                          (loop
                              for j from 0 below n1
                              when (/= i j)
                              sum (if (or 
                                       (and (> (elt seq1 i) (elt seq1 j))
                                            (> (elt seq2 i) (elt seq2 j)))
                                       (and (< (elt seq1 i) (elt seq1 j))
                                            (< (elt seq2 i) (elt seq2 j))))
                                      1
                                    0))))
           (sigma-q (loop for i from 0 below n1
                        sum 
                          (loop
                              for j from 0 below n1
                              when (/= i j)
                              sum (if (or 
                                       (and (> (elt seq1 i) (elt seq1 j))
                                            (< (elt seq2 i) (elt seq2 j)))
                                       (and (< (elt seq1 i) (elt seq1 j))
                                            (> (elt seq2 i) (elt seq2 j))))
                                      1
                                    0)))))
      (if (and (= t1 0) (= t2 0))
          (/ (- sigma-p sigma-q)
             denom)
        (/ (/ (- sigma-p sigma-q) 2)
           (* (sqrt (- (/ denom 2) t1))
              (sqrt (- (/ denom 2) t2))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Probability distribution ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; For reference see Wikipedia and
;;; Statistical Distributions, 2nd Edition, Merran Evans et al., Wiley 1993.

(defclass distribution () ())
(defclass discrete-distribution (distribution) ())
(defclass continuous-distribution (distribution) ())
(defclass gamma-like-distribution (continuous-distribution) ())
(defgeneric cdf (distribution x)
  (:documentation "Cumulative distribution function of DISTRIBUTION at X."))
(defgeneric density (continuous-distribution x)
  (:documentation "Density function of DISTRIBUTION at X."))
(defgeneric mass (discrete-distribution k)
  (:documentation "Probability mass function of DISTRIBUTION at X.")
  (:method (distribution k)
    (- (cdf distribution k) (cdf distribution (1- k)))))
(defgeneric quantile (distribution p)
  (:documentation "Quantile of P according to DISTRIBUTION."))
(defgeneric rand (distribution)
  (:documentation "Gives a random number according to DISTRIBUTION."))

;;; Note also the generic functions MEAN and VARIANCE defined above.

(defmethod cdf :around ((distribution discrete-distribution) x)
  "The CDF of a discrete distribution is \(CDF \(FLOOR X\)\)."
  (call-next-method distribution (floor x)))
(defun rand-n (distribution n)
  "N random numbers according to DISTRIBUTION."
  (loop repeat n collect (rand distribution)))


;;; Quantile guess/search functions (using moments)

(defun cornish-fisher-guess-with-central-moments (mean moment-fn p)
  "MOMENT-FN takes an integer parameter N and gives the Nth central moment."
  (let ((deviation (sqrt (funcall moment-fn 2))))
    (flet ((moment (r) (/ (funcall moment-fn r) (expt deviation r))))
      (let* ((z (quantile (standard-normal-distribution) p))
	     (z2 (* z z))
	     (z3 (* z2 z))
	     (z4 (* z3 z))
	     (k2 (moment 2))
	     (k3 (moment 3))
	     (k4 (- (moment 4) (* 3 k2 k2)))
	     (k5 (- (moment 5) (* 10 k2 k3)))
	     (cornish-fisher
	      (+ z
		 (* k3 (/ (- z2 1) 6))
		 (* k4 (/ (- z3 (* 3 z)) 24))
		 (- (* k3 k3 (/ (- (* 2 z3) (* 5 z)) 36)))
		 (* k5 (/ (- z4 (* 6 z2) -3) 120))
		 (- (* k3 k4 (/ (- z4 (* 5 z2) -2) 24)))
		 (* k3 k3 k3 (/ (- (* 12 z4) (* 53 z2) -17) 324)))))
	(+ mean (* deviation cornish-fisher))))))

(defun central-moment-fn-from-moment-fn (moment-fn)
  "Converts moments around the origin to central moments"
  (let ((mean (funcall moment-fn 1)))
    (lambda (n)
      (loop for j from 0 to n
	 sum (* (binomial n j) (expt -1 (- n j)) (funcall moment-fn j)
		(expt mean (- n j)))))))

(defun cornish-fisher-guess-with-moments-around-origin (moment-fn p)
  (cornish-fisher-guess-with-central-moments
   (funcall moment-fn 1) (central-moment-fn-from-moment-fn moment-fn) p))

(defun search-for-quantile (distribution p guess &key min max)
  (labels ((evaluate (x) (abs (- (cdf distribution x) p)))
	   (search-around (plus max less)
	     (loop
		for prev = guess then next
		for next = (funcall plus prev)
		while (and (or (null max) (funcall less next max) (= next max))
			   (funcall less (cdf distribution next) p))
		finally
		(return
		  (if (or (not (or (null max)
				   (funcall less next max) (= next max)))
			  (< (evaluate prev) (evaluate next)))
		      prev
		      next)))))
    (let ((guessed-value (cdf distribution guess)))
      (if (= guessed-value p)
	  guess
	  (if (< guessed-value p)
	      (search-around #'1+ max #'<)
	      (search-around #'1- min #'>))))))


;;; 1. Normal Distribution

(defclass normal-distribution (continuous-distribution)
  ((expected-value :initarg :expected-value :reader expected-value)
   (deviation :initarg :deviation :reader deviation)))

(defmethod print-object ((obj normal-distribution) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream ": EXPECTED-VALUE = ~a, DEVIATION = ~a"
	    (expected-value obj) (deviation obj))))

(defun normal-distribution (expected-value deviation)
  (assert (realp expected-value) (expected-value)
	  "EXPECTED-VALUE should be a real number.")
  (assert (and (realp deviation) (> deviation 0)) (deviation)
	  "DEVIATION should be a positive real number.")
  (make-instance 'normal-distribution
		 :expected-value expected-value :deviation deviation))

(let ((standard (normal-distribution 0.0d0 1.0d0)))
  (defun standard-normal-distribution ()
    standard))

(defmethod cdf ((distribution normal-distribution) x)
  (flet ((phi (x) (/ (1+ (erf (/ x (sqrt 2.0d0)))) 2.0d0)))
    (phi (/ (- x (expected-value distribution)) (deviation distribution)))))

(defmethod density ((distribution normal-distribution) x)
  (flet ((phi (x) (/ (exp (/ (sqr x) -2.0d0)) (sqrt (* 2.0d0 pi)))))
    (let ((deviation (deviation distribution)))
      (/ (phi (/ (- x (expected-value distribution)) deviation)) deviation))))

(defmethod quantile ((distribution normal-distribution) p)
  (+ (expected-value distribution)
     (* (deviation distribution) (sqrt 2.0d0)
	(let ((x (1- (* 2.0d0 p))))
	  (cond ((= x -1.0d0) #.(erf-inverse (1- double-float-epsilon)))
		((= x 1.0d0) #.(erf-inverse (- 1.0d0 double-float-epsilon)))
		(t (erf-inverse x)))))))

(defmethod rand ((distribution normal-distribution))
  (let ((fn (if (zerop (random 2)) #'cos #'sin)))
    (+ (expected-value distribution)
       (* (deviation distribution)
	  (sqrt (* -2.0d0 (log (unit-random))))
	  (funcall fn (* 2.0d0 pi (unit-random)))))))

(defmethod mean ((distribution normal-distribution))
  (expected-value distribution))

(defmethod variance ((distribution normal-distribution))
  (sqr (deviation distribution)))

(defun normal-distribution-estimate-unbiased (sequence)
  (normal-distribution (mean sequence)
		       (standard-deviation sequence :populationp nil)))

(defun normal-distribution-estimate-maximum-likelihood (sequence)
  (normal-distribution (mean sequence)
		       (standard-deviation sequence :populationp t)))


;;; 2. Lognormal Distribution

(defclass log-normal-distribution (continuous-distribution)
  ((expected-value :initarg :expected-value :reader expected-value)
   (deviation :initarg :deviation :reader deviation)))

(defmethod print-object ((obj log-normal-distribution) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream ": EXPECTED-VALUE = ~a, DEVIATION = ~a"
	    (expected-value obj) (deviation obj))))

(defun log-normal-distribution (expected-value deviation)
  (assert (realp expected-value) (expected-value)
	  "EXPECTED-VALUE should be a real number.")
  (assert (and (realp deviation) (> deviation 0)) (deviation)
	  "DEVIATION should be a positive real number.")
  (make-instance 'log-normal-distribution
		 :expected-value expected-value :deviation deviation))

(defmethod cdf ((distribution log-normal-distribution) x)
  (+ (* (erf (/ (- (log x) (expected-value distribution))
		(* (sqrt 2.0d0) (deviation distribution))))
	0.5d0)
     0.5d0))

(defmethod density ((distribution log-normal-distribution) x)
  (let ((deviation (deviation distribution)))
    (/ (exp (/ (sqr (- (log x) (expected-value distribution)))
	       (* -2.0d0 (sqr deviation))))
       (* x deviation (sqrt (* 2.0d0 pi))))))

(defmethod quantile ((distribution log-normal-distribution) p)
  (exp (+ (expected-value distribution)
	  (* (deviation distribution) (sqrt 2.0d0)
	     (let ((x (1- (* 2.0d0 p))))
	       (cond ((= x -1.0d0)
		      #.(erf-inverse (1- double-float-epsilon)))
		     ((= x 1.0d0)
		      #.(erf-inverse (- 1.0d0 double-float-epsilon)))
		     (t (erf-inverse x))))))))

(defmethod rand ((distribution log-normal-distribution))
  (* (exp (* (deviation distribution) (rand (standard-normal-distribution))))
     (expected-value distribution)))

(defmethod mean ((distribution log-normal-distribution))
  (* (expected-value distribution)
     (exp (/ (sqr (deviation distribution)) 2.0d0))))

(defmethod variance ((distribution log-normal-distribution))
  (let ((o (exp (sqr (deviation distribution)))))
    (* (sqr (expected-value distribution)) o (- o 1.0d0))))

(defun log-normal-distribution-estimate-unbiased (lst)
  (let* ((n (length lst))
	 (mu (/ (loop for x in lst sum (log x)) n))
	 (sigma2 (/ (loop for x in lst sum (sqr (- (log x) mu))) (1- n))))
    (log-normal-distribution (exp mu) (sqrt sigma2))))

(defun log-normal-distribution-estimate-maximum-likelihood (lst)
  (let* ((n (length lst))
	 (mu (/ (loop for x in lst sum (log x)) n))
	 (sigma2 (/ (loop for x in lst sum (sqr (- (log x) mu))) n)))
    (log-normal-distribution (exp mu) (sqrt sigma2))))


;;; 3. Uniform Distribution

(defclass uniform-distribution (continuous-distribution)
  ((from :initarg :from :reader uniform-from)
   (to :initarg :to :reader uniform-to)
   (denominator :accessor uniform-denom)))

(defmethod print-object ((obj uniform-distribution) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream ": [~a, ~a]" (uniform-from obj) (uniform-to obj))))

(defmethod initialize-instance :after ((distribution uniform-distribution) &key)
  (setf (uniform-denom distribution)
	(/ (- (uniform-to distribution) (uniform-from distribution)))))

(defun uniform-distribution (from to)
  (assert (and (realp from) (realp to) (> to from)) (from to)
	  "FROM and TO should be real numbers such that TO > FROM.")
  (make-instance 'uniform-distribution :from from :to to))

(let ((standard (make-instance 'uniform-distribution :from 0.0d0 :to 1.0d0)))
  (defun standard-uniform-distribution ()
    standard))

(defmethod cdf ((distribution uniform-distribution) x)
  (cond ((< x (uniform-from distribution)) 0)
	((< x (uniform-to distribution))
	 (* (- x (uniform-from distribution)) (uniform-denom distribution)))
	(t 1)))

(defmethod density ((distribution uniform-distribution) x)
  (if (<= (uniform-from distribution) x (uniform-to distribution))
      (uniform-denom distribution)
      0))

(defmethod quantile ((distribution uniform-distribution) p)
  (+ (uniform-from distribution)
     (* p (- (uniform-to distribution) (uniform-from distribution)))))

(defmethod rand ((distribution uniform-distribution))
  "TODO: This will never take the maximum value."
  (linear-combination (uniform-from distribution)
		      (unit-random)
		      (uniform-to distribution)))

(defmethod mean ((distribution uniform-distribution))
  (/ (+ (uniform-from distribution) (uniform-to distribution)) 2.0d0))

(defmethod variance ((distribution uniform-distribution))
  (/ (sqr (- (uniform-to distribution) (uniform-from distribution))) 12.0d0))

(defun uniform-distribution-estimate-moments (sequence)
  (let ((mu (mean sequence))
	(s3 (sqrt (* 3.0d0 (variance sequence)))))
    (uniform-distribution (- mu s3) (+ mu s3))))

(defun uniform-distribution-estimate-maximum-likelihood (sequence)
  (uniform-distribution (reduce #'min sequence) (reduce #'max sequence)))


;;; 4. Erlang Distribution

(defclass erlang-distribution (gamma-like-distribution)
  ((scale :initarg :scale :reader scale)
   (shape :initarg :shape :reader shape)))

(defmethod print-object ((obj erlang-distribution) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream ": SCALE = ~a, SHAPE = ~a" (scale obj) (shape obj))))

(defun erlang-distribution (scale shape)
  (assert (and (realp scale) (> scale 0)) (scale)
	  "SCALE should be a positive real number.")
  (assert (real-integer-p shape) (shape) "SHAPE must be an integer.")
  (if (= shape 1)
      (exponential-distribution scale nil)
      (make-instance 'erlang-distribution :scale scale :shape shape)))

(defmethod cdf ((distribution erlang-distribution) x)
  (let ((scale (scale distribution))
	(shape (shape distribution)))
    (- 1.0d0
       (* (exp (/ (- x) scale))
	  (loop for i from 0 below shape
	     sum (/ (expt (/ x scale) i) (gamma (1+ i))))))))

(defmethod rand ((distribution erlang-distribution))
  (* (- (scale distribution))
     (log (loop with result = 1 repeat (shape distribution)
	     do (setf result (* result (unit-random)))
	     finally (return result)))))

(defmethod mean ((distribution erlang-distribution))
  (* (scale distribution) (shape distribution)))

(defmethod variance ((distribution erlang-distribution))
  (* (sqr (scale distribution)) (shape distribution)))

(defun erlang-distribution-estimate (sequence)
  "Estimates by matching moments."
  (destructuring-bind (scale shape) (gamma-like-distribution-estimate sequence)
    (erlang-distribution scale (round shape))))


;;; 5. Exponential Distribution

(defclass exponential-distribution (continuous-distribution)
  ((hazard :initarg :hazard :reader hazard)))

(defmethod scale ((distribution exponential-distribution))
  (/ (hazard distribution)))

(defmethod print-object ((obj exponential-distribution) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream ": HAZARD = ~a" (hazard obj))))

(defun exponential-distribution (scale-or-hazard &optional (hazardp t))
  "\(EXPONENTIAL-DISTRIBUTION SCALE T) or \(EXPONENTIAL-DISTRIBUTION HAZARD)."
  (assert (and (realp scale-or-hazard) (> scale-or-hazard 0)) (scale-or-hazard)
	  "~:[SCALE~;HAZARD~] should be a positive real number." hazardp)
  (make-instance 'exponential-distribution
		 :hazard (if hazardp scale-or-hazard (/ scale-or-hazard))))

(defmethod cdf ((distribution exponential-distribution) x)
  (- 1.0d0 (exp (* (- x) (hazard distribution)))))

(defmethod density ((distribution exponential-distribution) x)
  (* (hazard distribution) (exp (* (- x) (hazard distribution)))))

(defmethod quantile ((distribution exponential-distribution) p)
  (* (- (scale distribution)) (log (- 1.0d0 p))))

(defmethod rand ((distribution exponential-distribution))
  (* (- (scale distribution)) (log (unit-random))))

(defmethod mean ((distribution exponential-distribution))
  (scale distribution))

(defmethod variance ((distribution exponential-distribution))
  (sqr (scale distribution)))

(defun exponential-distribution-estimate (sequence)
  "Unbiased maximum likelihood estimate."
  (exponential-distribution (mean sequence) nil))


;;; 6. Gamma Distribution

(defclass gamma-distribution (gamma-like-distribution)
  ((scale :initarg :scale :reader scale)
   (shape :initarg :shape :reader shape)))

(defmethod print-object ((obj gamma-distribution) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream ": SCALE = ~a, SHAPE = ~a" (scale obj) (shape obj))))

(defun gamma-distribution (scale shape)
  (assert (and (realp scale) (> scale 0)) (scale)
	  "SCALE should be a positive real number.")
  (assert (and (realp shape) (> shape 0)) (shape)
	  "SHAPE should be a positive real number.")
  (if (real-integer-p shape)
      (erlang-distribution scale shape)
      (make-instance 'gamma-distribution :scale scale :shape shape)))

(defmethod cdf ((distribution gamma-distribution) x)
  (let ((scale (scale distribution))
	(shape (shape distribution)))
    (/ (lower-incomplete-gamma shape (/ x scale))
       (gamma shape))))

(defmethod density ((distribution gamma-like-distribution) x)
  (let ((scale (scale distribution))
	(shape (shape distribution)))
    (/ (* (expt (/ x scale) (1- shape))
	  (exp (/ (- x) scale)))
       (* scale (gamma shape)))))

(defmethod quantile ((distribution gamma-like-distribution) p)
  "Uses the Wilson-Hilferty guess and Newton-Raphson approximation.
TODO: For small numbers the WH estimate doesn't really work.
WH:    KT = 2/G * \(\(\(ZT - G/6) * G/6 + 1)^3 - 1)
Kite:  KT = ZT + \(ZT^2 - 1) * G/6 + 1/3 * \(ZT^3 - 6 * ZT) * \(G/6)^2
            - \(ZT^2 - 1) * \(G/6)^3 + ZT * \(G/6)^4 + 1/3 * \(G/6)^5."
  (let* ((scale (scale distribution))
         (shape (shape distribution))
         (zt (quantile (standard-normal-distribution) p))
         (g (/ 2.0d0 (sqrt shape)))     ; skew
         ;; Wilson-Hilferty
         (kt (* (/ 2.0d0 g)
                (1- (expt (1+ (* (/ g 6.0d0) (- zt (/ g 6.0d0)))) 3))))
         ;; Kite
         ;; 	 (kt (+ zt (* (1- (sqr zt)) (/ g 6.0d0))
         ;; 		(* 1/3 zt (- (sqr zt) 6.0d0) (sqr (/ g 6.0d0)))
         ;; 		(- (* (1- (sqr zt)) (expt (/ g 6.0d0) 3)))
         ;; 		(* zt (expt (/ g 6.0d0) 4)) (* 1/3 (expt (/ g 6.0d0) 5))))
         (guess (* scale (+ shape (* (sqrt shape) kt)))))
    (newton-raphson (lambda (x) (- (cdf distribution x) p))
                    (lambda (x) (density distribution x))
                    :initial-guess guess)))
(defmethod quantile-ili ((distribution gamma-like-distribution) p)
  "Use the method of inverse-linear-interpolation for numerical calculation.
If there is a numerical problem with quantile of gamma-like-distribution, 
this method would be solve it. However this is slower than Newton-Raphson."
  (assert (< 0d0 p 1d0))
  (loop with cdf-p = (lambda (x) 
                        (- (gammp (shape distribution) (/ x (scale distribution))) p))
      with lower = *inv-lin-interp-precision*
      with upper = 1d0
      as fl = (funcall cdf-p lower)
      as fu = (funcall cdf-p upper)
      while (= (signum fl) (signum fu))
      do (cond ((plusp fl) (return 0d0))
               ((minusp fu) (let ((next (* upper 10d0)))
                              (if (> most-positive-double-float next)
                                  (setf upper next)
                                (return most-positive-double-float))))
               (t (return lower)))
      finally (return (inverse-linear-interpolation cdf-p `(,lower ,upper)))))

(let ((ud (uniform-distribution 0.0d0 1.0d0))
      (xd (exponential-distribution 1.0d0)))
  (defmethod rand ((distribution gamma-distribution))
    "When SHAPE >= 1, we use the sampling method by George S. Fishman:
`Sampling from the Gamma Distribution on a Computer'.
When SHAPE < 1, we use the algorithm by Ahrens & Dieter, `Computer methods
for sampling from gamma, beta, poisson and binomial distributions.'"
    (let ((a (shape distribution)))
      (* (if (< a 1)
	     (loop with b = (/ (+ a (exp 1.0d0)) (exp 1.0d0))
		for u1 = (rand ud) and u2 = (rand ud)
		for y = (* b u1)
		do (if (<= y 1.0d0)
		       (let ((z (expt y (/ a))))
			 (when (< u2 (exp (- z)))
			   (return (* a z))))
		       (let ((z (- (log (/ (- b y) a)))))
			 (when (<= u2 (expt z (1- a)))
			   (return (* a z))))))
	     (loop
		for x = (rand xd) and u = (rand ud)
		while (> u (expt (/ x (exp (1+ x))) (1- a)))
		finally (return (* a x))))
	 (scale distribution)))))

(defmethod mean ((distribution gamma-distribution))
  (* (scale distribution) (shape distribution)))

(defmethod variance ((distribution gamma-distribution))
  (* (sqr (scale distribution)) (shape distribution)))

(defun gamma-like-distribution-estimate (sequence)
  (let ((mu (mean sequence))
	(s2 (variance sequence)))
    (list (/ s2 mu) (/ (sqr mu) s2))))

(defun gamma-distribution-estimate (sequence)
  "Estimates by matching moments."
  (apply #'gamma-distribution (gamma-like-distribution-estimate sequence)))


;;; 7. Chi-squared Distribution

(defclass chi-square-distribution (continuous-distribution)
  ((degree :initarg :degree :reader degree)))

(defmethod print-object ((obj chi-square-distribution) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream ": DEGREE = ~d" (degree obj))))

(defun chi-square-distribution (degree)
  (assert (and (real-integer-p degree) (> degree 0)) (degree)
	  "DEGREE should be a positive integer.")
  (make-instance 'chi-square-distribution :degree degree))

(defmethod cdf ((distribution chi-square-distribution) x)
  (let ((k (degree distribution)))
    (/ (lower-incomplete-gamma-half k (/ x 2.0d0)) (gamma-half k))))

(defmethod density ((distribution chi-square-distribution) x)
  (let ((k (degree distribution)))
    (if (<= x 0)
	0
	(/ (* (expt x (1- (/ k 2))) (exp (/ x -2.0d0)))
	   (* (expt 2 (/ k 2)) (gamma-half k))))))

(defmethod quantile ((distribution chi-square-distribution) p)
  (quantile (gamma-distribution 2.0d0 (* 0.5d0 (degree distribution))) p))

(defmethod rand ((distribution chi-square-distribution))
  (loop
     with standard = (standard-normal-distribution)
     for i from 1 to (degree distribution)
     sum (sqr (rand standard))))

(defmethod mean ((distribution chi-square-distribution))
  (degree distribution))

(defmethod variance ((distribution chi-square-distribution))
  (* 2 (degree distribution)))


;;; 8. Student's t Distribution

(defclass t-distribution (continuous-distribution)
  ((degree :initarg :degree :reader degree)
   (precalc :accessor t-precalc)))

(defmethod print-object ((obj t-distribution) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream ": DEGREE = ~d" (degree obj))))

(defmethod initialize-instance :after ((distribution t-distribution) &key)
  "TODO: This can be done once in a global variable that contains an
adjustable vector, that is adjusted if the new distribution has a larger
degree than the one precalculated."
  (let ((k (degree distribution)))
    (with-accessors ((v t-precalc)) distribution
      (setf v (make-array (floor k 2)))
      (setf (elt v 0) 1)
      (loop
	 for j from 1 below (floor k 2)
	 do (setf (elt v j)
		  (* (elt v (1- j))
		     (if (evenp k)
			 (/ (1- (* 2 j)) (* 2 j))
			 (/ (* 2 j) (1+ (* 2 j))))))))))

(defun t-distribution (degree)
  (assert (and (real-integer-p degree) (> degree 0)) (degree)
	  "DEGREE should be a positive integer.")
  (make-instance 't-distribution :degree degree))

(defmethod cdf ((distribution t-distribution) x)
  (let* ((k (degree distribution))
	 (v (t-precalc distribution)))
    (if (evenp k)
	(+ 0.5d0
	   (* (/ x (* 2.0d0 (sqrt (+ k (sqr x)))))
	      (loop
		 for j from 0 below (floor k 2)
		 sum (/ (elt v j) (expt (1+ (/ (sqr x) k)) j)))))
	(+ 0.5d0
	   (/ (atan (/ x (sqrt (coerce k 'double-float)))) pi)
	   (* (/ (* x (sqrt (coerce k 'double-float))) (* pi (+ k (sqr x))))
	      (loop
		 for j from 0 below (floor k 2)
		 sum (/ (elt v j) (expt (1+ (/ (sqr x) k)) j))))))))

(defmethod density ((distribution t-distribution) x)
  (let ((k (degree distribution)))
    (* (/ (gamma-half (1+ k)) (* (sqrt (* k pi)) (gamma-half k)))
       (expt (1+ (/ (sqr x) k)) (/ (1+ k) -2)))))

;;; Another version that doesn't use the incomplete beta inverse can also be
;;; found here: Shaw, W.T., 2006, "Sampling Student's T distribution - use of
;;; the inverse cumulative distribution function."
;;; Journal of Computational Finance, Vol 9 Issue 4, pp 37-73, Summer 2006p
(defmethod quantile ((distribution t-distribution) p)
  "This implementation is quite expensive: it uses Newton-Raphson numerical
integration twice, since the inverse of the incomplete beta function also
uses it."
  (let* ((n (degree distribution))
	 (x (* 2.0d0 (if (< p 0.5d0) p (- 1.0d0 p))))
	 (guess (* (if (< p 0.5d0) -1.0d0 1.0d0)
		   (sqrt (* n (1- (/ (incomplete-beta-inverse
				      (/ n 2.0d0) 0.5d0 x))))))))
    (newton-raphson (lambda (x) (- (cdf distribution x) p))
		    (lambda (x) (density distribution x))
		    :initial-guess guess)))

(defmethod rand ((distribution t-distribution))
  (let ((k (degree distribution)))
    (/ (rand (standard-normal-distribution))
       (sqrt (/ (rand (chi-square-distribution k)) k)))))

(defmethod mean ((distribution t-distribution))
  (assert (> (degree distribution) 1) ()
	  "MEAN is undefined when DEGREE is 1.")
  0)

(defmethod variance ((distribution t-distribution))
  (let ((degree (degree distribution)))
    (assert (> (degree distribution) 2) ()
	    "VARIANCE is undefined when DEGREE is less than 3.")
    (/ degree (- degree 2))))


;;; 9. Beta Distribution

(defclass beta-distribution (continuous-distribution)
  ((shape1 :initarg :shape1 :reader shape1)
   (shape2 :initarg :shape2 :reader shape2)))

(defmethod print-object ((obj beta-distribution) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream ": SHAPE = (~d, ~d)" (shape1 obj) (shape2 obj))))

(defun beta-distribution (shape1 shape2)
  (assert (and (realp shape1) (> shape1 0)) (shape1)
	  "SHAPE1 should be a positive real number.")
  (assert (and (realp shape2) (> shape2 0)) (shape2)
	  "SHAPE2 should be a positive real number.")
  (make-instance 'beta-distribution :shape1 shape1 :shape2 shape2))

(defmethod cdf ((distribution beta-distribution) x)
  (regularized-incomplete-beta (shape1 distribution) (shape2 distribution) x))

(defmethod density ((distribution beta-distribution) x)
  (let ((shape1 (shape1 distribution))
	(shape2 (shape2 distribution)))
    (/ (* (expt x (1- shape1))
	  (expt (- 1.0 x) (1- shape2)))
       (beta shape1 shape2))))

(defmethod quantile ((distribution beta-distribution) p)
  (let* ((n (shape1 distribution))
	 (o (shape2 distribution))
	 (guess (cornish-fisher-guess-with-moments-around-origin
		 (lambda (r) (/ (beta (+ n r) o) (beta n o))) p)))
    (if (< 0 guess 1)
	(newton-raphson (lambda (x) (- (cdf distribution x) p))
			(lambda (x) (density distribution x))
			:initial-guess guess)
	(newton-raphson (lambda (x) (- (cdf distribution x) p))
			(lambda (x) (density distribution x))
			:range '(0.0d0 1.0d0)))))

;;; For non-integer shape parameters, use the algorithm described in
;;; Generating beta variates with nonintegral shape parameters,
;;; R. C. H. Cheng, Communications of the ACM 21, 317-322, 1978.
(defmethod rand ((distribution beta-distribution))
  (let ((a (shape1 distribution))
	(b (shape2 distribution)))
    (cond ((and (integerp a) (integerp b))
	   (let ((tmp (rand (erlang-distribution 1 a))))
	     (/ tmp (+ tmp (rand (erlang-distribution 1 b))))))
	  ((> (min a b) 1.0d0)
	   (conditional-swap-let (> a b) (lambda (x) (- 1.0d0 x)) ((a b))
	     (let* ((alpha (+ a b))
		    (beta (sqrt (/ (- alpha 2.0d0) (- (* 2.0d0 a b) alpha))))
		    (gamma (+ a (/ beta))))
	       (loop for u1 = (unit-random) and u2 = (unit-random)
		  for v = (* beta (log (/ u1 (- 1.0d0 u1))))
		  for w = (* a (exp v))
		  for z = (* u1 u1 u2)
		  for r = (- (* gamma v) #.(log 4.0d0))
		  for s = (- (+ a r) w)
		  for tt = (log z)
		  while (and (< (+ s #.(+ 1.0d0 (log 5.0d0))) (* 5.0d0 z))
			     (< s tt)
			     (< (+ r (* alpha (log (/ alpha (+ b w))))) tt))
		  finally (return (/ w (+ b w)))))))
	  (t ;; (<= (min a b) 1.0d0)
	   (conditional-swap-let (< a b) (lambda (x) (- 1.0d0 x)) ((a b))
	     (let* ((alpha (+ a b))
		    (beta (/ b))
		    (delta (- (+ 1.0d0 a) b))
		    (kappa1 (/ (* delta (+ #.(/ 72.0d0) (* #.(/ 24.0d0) b)))
			       (- (* a beta) #.(/ 7.0d0 9.0d0))))
		    (kappa2 (+ 0.25d0 (* (+ 0.5d0 (/ 0.25d0 delta)) b))))
	       (loop for u1 = (unit-random) and u2 = (unit-random)
		  for v = (* beta (log (/ u1 (- 1.0d0 u1))))
		  for w = (* a (exp v))
		  for z = (* u1 u1 u2)
		  for status =
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
		  finally (return (/ w (+ b w))))))))))

(defmethod mean ((distribution beta-distribution))
  (/ (shape1 distribution)
     (+ (shape1 distribution) (shape2 distribution))))

(defmethod variance ((distribution beta-distribution))
  (let ((n (shape1 distribution))
	(o (shape2 distribution)))
    (/ (* n o) (* (sqr (+ n o)) (+ n o 1)))))

(defun beta-distribution-estimate (sequence)
  "Estimates by matching moments."
  (let ((mu (mean sequence))
	(s2 (variance sequence)))
    (beta-distribution (* mu (- (/ (* mu (- 1.0d0 mu)) s2) 1.0d0))
		       (* (- 1.0d0 mu) (- (/ (* mu (- 1.0d0 mu)) s2) 1.0d0)))))


;;; 10. F Distribution

(defclass f-distribution (continuous-distribution)
  ((degree1 :initarg :degree1 :reader degree1)
   (degree2 :initarg :degree2 :reader degree2)))

(defmethod print-object ((obj f-distribution) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream ": DEGREES = (~d, ~d)" (degree1 obj) (degree2 obj))))

(defun f-distribution (degree1 degree2)
  (assert (and (real-integer-p degree1) (> degree1 0)) (degree1)
	  "DEGREE1 should be a positive integer.")
  (assert (and (real-integer-p degree2) (> degree2 0)) (degree2)
	  "DEGREE2 should be a positive integer.")
  (make-instance 'f-distribution :degree1 degree1 :degree2 degree2))

(defmethod cdf ((distribution f-distribution) x)
  (let ((d1 (degree1 distribution))
	(d2 (degree2 distribution)))
    (regularized-incomplete-beta (/ d1 2) (/ d2 2)
				 (/ (* d1 x) (+ (* d1 x) d2)))))

(defmethod density ((distribution f-distribution) x)
  (if (<= x 0)
      0
      (let ((d1 (degree1 distribution))
	    (d2 (degree2 distribution)))
	(/ (* (expt (coerce (/ d1 d2) 'double-float) (/ d1 2))
	      (expt x (1- (/ d1 2)))
	      (expt (1+ (* (/ d1 d2) x)) (/ (+ d1 d2) -2)))
	   (beta-half d1 d2)))))

;;; The regularized incomplete beta inverse is too unstable for this.
;; (defmethod quantile ((distribution f-distribution) p)
;;   (let* ((d1 (degree1 distribution))
;; 	 (d2 (degree2 distribution))
;; 	 (x (regularized-incomplete-beta-inverse (/ d1 2) (/ d2 2) p)))
;;     (/ (* d2 x) (* (- 1.0d0 x) d1))))

(defmethod quantile ((distribution f-distribution) p)
  "If one degree is large, it uses the chi-square quantile,
otherwise it uses the beta distribution quantile."
  (let* ((d1 (degree1 distribution))
	 (d2 (degree2 distribution)))
    (cond ((and (<= d1 d2) (> d2 400000))
	   (/ (quantile (chi-square-distribution d1) p) d1))
	  ((> d1 400000)
	   (/ d2 (quantile (chi-square-distribution d2) (- 1.0 p))))
	  (t (let ((y (quantile (beta-distribution (/ d1 2) (/ d2 2)) p)))
	       (/ (* d2 y) (* (- 1.0d0 y) d1)))))))

(defmethod rand ((distribution f-distribution))
  (let ((d1 (degree1 distribution))
	(d2 (degree2 distribution)))
    (/ (/ (rand (chi-square-distribution d1)) d1)
       (/ (rand (chi-square-distribution d2)) d2))))

(defmethod mean ((distribution f-distribution))
  (let ((d2 (degree2 distribution)))
    (assert (> d2 2) () "MEAN is undefined when DEGREE2 <= 2.")
    (/ d2 (- d2 2))))

(defmethod variance ((distribution f-distribution))
  (let ((d1 (degree1 distribution))
	(d2 (degree2 distribution)))
    (assert (> d2 4) () "VARIANCE is undefined when DEGREE2 <= 4.")
    (/ (* 2 d2 d2 (+ d1 d2 -2))
       (* d1 (sqr (- d2 2)) (- d2 4)))))


;;; 11. Binomial Distribution

(defclass binomial-distribution (discrete-distribution)
  ((size :initarg :size :reader size)
   (probability :initarg :probability :reader probability)))

(defmethod print-object ((obj binomial-distribution) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream ": SIZE = ~d, PROBABILITY = ~a"
	    (size obj) (probability obj))))

(defun binomial-distribution (size probability)
  (assert (and (real-integer-p size) (>= size 0)) (size)
	  "SIZE should be a nonnegative integer.")
  (assert (and (realp probability) (<= 0 probability 1)) (probability)
	  "PROBABILITY should be a real number between 0 and 1.")
  (make-instance 'binomial-distribution
		 :size size :probability probability))

(defmethod cdf ((distribution binomial-distribution) k)
  (regularized-incomplete-beta (+ (- (size distribution) k) 0.0d0)
			       (+ k 1.0d0)
			       (- 1.0d0 (probability distribution))))

(defmethod mass ((distribution binomial-distribution) k)
  (let ((n (size distribution))
	(p (probability distribution)))
    (* (binomial n k) (expt p k) (expt (- 1 p) (- n k)))))

;;; As in R:
;;; Compute an educated guess by the 2nd-order Cornish-Fisher expansion:
;;; x = round(mu + sigma * (z + k3 * (z^2 - 1) / 6), where k3 is the third
;;; cumulant (= the third central moment) of the normalized binomial
;;; distribution: X* = (X - mu) / sigma, and z is the quantile of the
;;; standard normal distribution at p.
;;; As for the cumulant, we can calculate the moments of the binomial
;;; distribution from its moment-generating function (1 - p + p * e^t)^n,
;;; its kth derivative is the kth moment. Thus we have
;;; M1 = mu, M2 = mu^2 + sigma^2, M3 = mu^3 + sigma^2 * (3mu - 2p + 1).
;;; With a little calculation it can be shown that k3 = (1 - 2p) / sigma.
;;; After we have a guess, just do a search in the proximity.
(defmethod quantile ((distribution binomial-distribution) p)
  "TODO: The search part could be more efficient."
  (cond ((= p 0.0d0) 0)
	((= p 1.0d0) (size distribution))
	(t (let* ((n (size distribution))
		  (pr (probability distribution))
		  (mu (* n pr))
		  (sigma (sqrt (* n pr (- 1.0d0 pr))))
		  (k3 (/ (- 1.0d0 (* 2.0d0 pr)) sigma))
		  (z (quantile (standard-normal-distribution) p))
		  (x (+ z (/ (* k3 (1- (sqr z))) 6.0d0)))
		  (guess (min (max (round (+ mu (* sigma x))) 0) n)))
	     (search-for-quantile distribution p guess :min 0 :max n)))))

(defmethod rand ((distribution binomial-distribution))
  (loop repeat (size distribution)
     with p = (probability distribution)
     count (< (unit-random) p)))

(defmethod mean ((distribution binomial-distribution))
  (* (size distribution) (probability distribution)))

(defmethod variance ((distribution binomial-distribution))
  (let ((p (probability distribution)))
    (* (size distribution) p (- 1.0d0 p))))

(defun binomial-distribution-estimate (size successes)
  "Maximum likelihood estimate."
  (binomial-distribution size (/ successes size)))


;;; 12. Geometric Distribution

;;; This is a Pascal distribution with SUCCESSES = 1.
;;; TODO: Maybe it should be merged?

(defclass geometric-distribution (discrete-distribution)
  ((probability :initarg :probability :reader probability)))

(defmethod print-object ((obj geometric-distribution) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream ": PROBABILITY = ~a" (probability obj))))

(defun geometric-distribution (probability)
  "Supported on k = 1, 2, ... \(the # of trials until a success, inclusive\)"
  (assert (and (realp probability) (< 0 probability) (<= probability 1))
	  (probability) "PROBABILITY should be a positive real number <= 1.")
  (make-instance 'geometric-distribution :probability probability))

(defmethod cdf ((distribution geometric-distribution) k)
  (- 1.0d0 (expt (- 1.0d0 (probability distribution)) k)))

(defmethod mass ((distribution geometric-distribution) k)
  (let ((p (probability distribution)))
    (* (expt (- 1.0d0 p) (1- k)) p)))

(defmethod quantile ((distribution geometric-distribution) p)
  (values (round (log (- 1.0 p) (- 1.0d0 (probability distribution))))))

(defmethod rand ((distribution geometric-distribution))
  (ceiling (/ (log (unit-random)) (log (- 1.0d0 (probability distribution))))))

(defmethod mean ((distribution geometric-distribution))
  (let ((p (probability distribution)))
    (/ p)))

(defmethod variance ((distribution geometric-distribution))
  (let ((p (probability distribution)))
    (/ (- 1.0d0 p) p p)))

(defun geometric-distribution-estimate (trials)
  "Maximum likelihood estimate."
  (geometric-distribution (/ trials)))


;;; 13. Hypergeometric Distribution

(defclass hypergeometric-distribution (discrete-distribution)
  ((elements :initarg :elements :reader elements)
   (successes :initarg :successes :reader successes)
   (samples :initarg :samples :reader samples)))

(defmethod print-object ((obj hypergeometric-distribution) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream ": N = ~a, m = ~a, n = ~a"
	    (elements obj) (successes obj) (samples obj))))

(defun hypergeometric-distribution (elements successes samples)
  (assert (and (real-integer-p elements) (>= elements 0)) (elements)
	  "ELEMENTS should be a nonnegative integer.")
  (assert (and (real-integer-p successes) (>= successes 0)) (successes)
	  "SUCCESSES should be a nonnegative integer.")
  (assert (and (real-integer-p samples) (>= samples 0)) (samples)
	  "SAMPLES should be a nonnegative integer.")
  (make-instance 'hypergeometric-distribution
		 :elements elements :successes successes :samples samples))

(defmethod cdf ((distribution hypergeometric-distribution) k)
  "TODO: Trivial implementation - ineffective and there may be cancellation."
  (let ((all (elements distribution))
	(m (successes distribution))
	(n (samples distribution)))
    (if (>= k (min m n))
	1
	(loop for i from (max 0 (- (+ n m) all)) to k
	   sum (mass distribution i)))))

(defmethod mass ((distribution hypergeometric-distribution) k)
  (let ((all (elements distribution))
	(m (successes distribution))
	(n (samples distribution)))
    (/ (* (binomial m k) (binomial (- all m) (- n k))) (binomial all n))))

(defmethod quantile ((distribution hypergeometric-distribution) p)
  "TODO: Trivial implementation - ineffective."
  (let ((all (elements distribution))
	(m (successes distribution))
	(n (samples distribution)))
    (binary-search p (lambda (k) (cdf distribution k))
		   (max 0 (- (+ n m) all)) (min m n))))

(defmethod rand ((distribution hypergeometric-distribution))
  (loop with m = (successes distribution) and all = (elements distribution)
     for i from 1 to (samples distribution)
     for r = (unit-random)
     for p = (/ m all) then (/ (- (* (1+ (- all i)) p) (if (< r p) 1 0))
			       (- all i))
     count (< r p)))

(defmethod mean ((distribution hypergeometric-distribution))
  (/ (* (samples distribution) (successes distribution))
     (elements distribution)))

(defmethod variance ((distribution hypergeometric-distribution))
  (let ((all (elements distribution))
	(m (successes distribution))
	(n (samples distribution)))
    (/ (* (/ (* n m) all) (- 1 (/ m all)) (- all n)) (1- all))))

(defun hypergeometric-distribution-estimate-successes-unbiased
    (elements samples sample-successes)
  (hypergeometric-distribution elements
			       (round (/ (* elements sample-successes) samples))
			       samples))

(defun hypergeometric-distribution-estimate-successes-maximum-likelihood
    (elements samples sample-successes)
  (hypergeometric-distribution elements
			       (floor (/ (* (1+ elements) sample-successes)
					 samples))
			       samples))

(defun hypergeometric-distribution-estimate-elements
    (successes samples sample-successes)
  "Maximum likelihood estimation."
  (hypergeometric-distribution (floor (/ (* samples successes)
					 sample-successes))
			       successes samples))


;;; 14. Cauchy Distribution

(defclass cauchy-distribution (continuous-distribution)
  ((location :initarg :location :reader location)
   (scale :initarg :scale :reader scale)))

(defmethod print-object ((obj cauchy-distribution) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream ": LOCATION = ~a, SCALE = ~a" (location obj) (scale obj))))

(defun cauchy-distribution (location scale)
  (assert (realp location) (location) "LOCATION should be a real number.")
  (assert (and (realp scale) (> scale 0)) (scale)
	  "SCALE should be a positive real number.")
  (make-instance 'cauchy-distribution :location location :scale scale))

(defmethod cdf ((distribution cauchy-distribution) x)
  (+ 1/2 (/ (atan (- x (location distribution)) (scale distribution)) pi)))

(defmethod density ((distribution cauchy-distribution) x)
  (let ((location (location distribution))
	(scale (scale distribution)))
    (/ (* pi scale (1+ (sqr (/ (- x location) scale)))))))

(defmethod quantile ((distribution cauchy-distribution) p)
  (+ (location distribution)
     (* (scale distribution)
	(tan (* pi (- p 0.5d0))))))

(defmethod rand ((distribution cauchy-distribution))
  (+ (* (tan (* pi (- (unit-random) 0.5d0)))
	(scale distribution))
     (location distribution)))

(defmethod mean ((distribution cauchy-distribution))
  (error "The mean of the Cauchy distribution is undefined."))

(defmethod variance ((distribution cauchy-distribution))
  (error "The variance of the Cauchy distribution is undefined."))

;;; Iterative numerical method as described in
;;; F. Nagy, Parameter Estimation of the Cauchy Distribution in
;;; Information Theory Approach, Journal of Universal Computer Science,
;;; vol. 12, no. 9, pp. 1332-1344, 2006.
;;; TODO: The exact solution can be computed by solving an equation
;;; system with two unknowns. See the same paper for details.
(defun cauchy-distribution-estimate (lst &optional (iterations 100))
  (loop repeat iterations
     with n = (length lst)
     for c = 1.0d0 then (+ c (* s (/ e1 e0)))
     for s = 1.0d0 then (* s (sqrt (- (/ e0) 1.0d0)))
     for u = (loop for a in lst collect (/ (- a c) s))
     for e0 = (/ (loop for ui in u sum (/ (+ (sqr ui) 1.0d0))) n)
     for e1 = (/ (loop for ui in u sum (/ ui (+ (sqr ui) 1.0d0))) n)
     finally (return (cauchy-distribution c s))))


;;; 15. Logistic Distribution

(defclass logistic-distribution (continuous-distribution)
  ((location :initarg :location :reader location)
   (scale :initarg :scale :reader scale)))

(defmethod print-object ((obj logistic-distribution) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream ": LOCATION = ~a, SCALE = ~a" (location obj) (scale obj))))

(defun logistic-distribution (location scale)
  (assert (realp location) (location) "LOCATION should be a real number.")
  (assert (and (realp scale) (> scale 0)) (scale)
	  "SCALE should be a positive real number.")
  (make-instance 'logistic-distribution :location location :scale scale))

(defmethod cdf ((distribution logistic-distribution) x)
  (/ (1+ (exp (- (/ (- x (location distribution)) (scale distribution)))))))

(defmethod density ((distribution logistic-distribution) x)
  (let ((e (exp (- (/ (- x (location distribution)) (scale distribution))))))
    (/ e (* (scale distribution) (sqr (1+ e))))))

(defmethod quantile ((distribution logistic-distribution) p)
  (- (location distribution) (* (scale distribution) (log (1- (/ p))))))

(defmethod rand ((distribution logistic-distribution))
  (let ((r (unit-random)))
    (+ (location distribution)
       (* (scale distribution)
	  (log (/ r (- 1.0d0 r)))))))

(defmethod mean ((distribution logistic-distribution))
  (location distribution))

(defmethod variance ((distribution logistic-distribution))
  (/ (* (sqr pi) (sqr (scale distribution))) 3.0d0))

;;; This fixpoint method and several others are described in detail in
;;; M. R. Alkasasbeh, M. Z. Raqab, Estimation of the generalized
;;; logistic distribution parameters: Comparative study,
;;; Statistical Methodology (2008), doi: 10.1016/j.stamet.2008.10.001
(defun logistic-distribution-estimate (sequence &optional (iteration 100)
				       (tolerance 1.0d-10))
  "Maximal likelihood estimate."
  (let ((mu (mean sequence))
	(n (length sequence)))
    (loop repeat iteration
       for last = 0.0d0 then s
       for s = 1.0d0 then
       (/ (loop for xi in sequence
	     for di = (exp (/ (- mu xi) s))
	     sum (/ (* xi (- 1.0d0 di)) (+ 1.0d0 di)))
	  n)
       while (> (abs (- last s)) tolerance)
       finally (return (logistic-distribution mu s)))))


;;; 17. Negative Binomial Distribution

(defclass negative-binomial-distribution (discrete-distribution)
  ((successes :initarg :successes :reader successes)
   (probability :initarg :probability :reader probability)
   (failuresp :initarg :failuresp :reader failuresp)))

(defmethod print-object ((obj negative-binomial-distribution) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream ": SUCCESSES = ~a, PROBABILITY = ~a~:[ [TR]~;~]"
	    (successes obj) (probability obj) (failuresp obj))))

(defun negative-binomial-distribution (successes probability
				       &optional (failuresp t))
  "Number of failures until a given number of successes,
extended to real numbers.
If FAILURESP is NIL, we look at the number of all trials, not just the
failures."
  (assert (and (realp successes) (> successes 0)) (successes)
	  "SUCCESSES should be a positive real number.")
  (assert (and (realp probability) (< 0 probability 1)) (probability)
	  "PROBABILITY should be a real number between 0 and 1.")
  (make-instance 'negative-binomial-distribution
		 :successes successes :probability probability
		 :failuresp failuresp))

(defmethod cdf ((distribution negative-binomial-distribution) k)
  (let ((failures (if (failuresp distribution)
		      k
		    (- k (successes distribution)))))
    (if (< failures 0)
	0
	(regularized-incomplete-beta (successes distribution)
				     (+ failures 1.0d0)
				     (probability distribution)))))

(defmethod mass ((distribution negative-binomial-distribution) k)
  (let ((s (successes distribution))
	(p (probability distribution))
	(failures (if (failuresp distribution)
		      k
		    (- k (successes distribution)))))
    (if (< failures 0)
	0
      (* (/ (gamma (+ s failures)) (* (gamma (+ failures 1.0d0)) (gamma s)))
	 (expt p s) (expt (- 1.0d0 p) failures)))))

;;; Compute a guess by the 2nd-order Cornish-Fisher expansion.
;;; See the notes at the binomial distribution quantile.
(defmethod quantile ((distribution negative-binomial-distribution) p)
  (let* ((prob (probability distribution))
	 (q (- 1.0d0 prob))
	 (s (successes distribution))
	 (mean (/ (* s q) prob))
	 (sigma (sqrt (/ (* s q) (sqr prob))))
	 (z (quantile (standard-normal-distribution) p))
	 (k3 (/ (* s q (1+ q)) (expt (* sigma prob) 3)))
	 (guess (round (+ mean
			  (* sigma (+ z (* k3 (/ (1- (* z z)) 6.0d0))))
			  (if (failuresp distribution) 0 s)))))
    (search-for-quantile distribution p guess :min s)))

(defun negative-binomial-rand-integer (distribution)
  (let ((p (probability distribution)))
    (- (if (> p 0.5d0)
	   ;; by rejection (faster for large p)
	   (let ((s (successes distribution))
		 (success 0))
	     (loop for n upfrom 0
		   while (< success s)
		   for r = (unit-random)
		   do (when (< r p) (incf success))
		   finally (return n)))
	 ;; by geometric distribution
	 (let ((d (geometric-distribution (probability distribution))))
	   (loop repeat (successes distribution) sum (rand d))))
       (if (failuresp distribution) (successes distribution) 0))))

;;; As in S. H. Ong, Wen-Jau Lee:
;;; "Computer generation of negative binomial variates by envelope rejection",
;;; In: Computation Statistics & Data Analysis 52, pp. 4175-4183, 2008.
;;; (Note that in the paper (and so in this algorithm) P is the probability
;;;  of failure, not success, that's why the enveloping distribution is
;;;  created with probability 1-PHI instead of PHI.
(defun negative-binomial-rand-real (distribution)
  "TODO: This implementation is practical when 1 <= SUCCESSES < 20.
For other cases, it may be faster to use Poisson and gamma distributions.
Also see the speedup part in the paper."
  (let* ((q (probability distribution))
	 (p (- 1.0d0 q))
	 (alpha (successes distribution))
	 (m (truncate alpha))
	 (theta (- alpha m))
	 (a (- (/ p) 1.0d0))
	 (phi (/ (+ 1.0d0 (/ (* a m) alpha))))
	 (e #.(exp 1.0d0))
	 (s (negative-binomial-distribution m (- 1.0d0 phi))))
    (flet ((T1 (x) (* (expt (* (/ (- 1.0d0 p) alpha) e) theta)
		      (expt (/ p phi) (+ m x))
		      (expt (+ m x) theta)
		      (+ 1.0d0 (/ (* theta (- theta 1.0d0))
				  (* 2.0d0 (+ m x))))))
	   (LB (x) (* (/ (* q e (+ (* 2 (+ m x)) (* theta (- theta 1.0d0))))
			 (- (* 2.0d0 alpha theta)
			    (* 2.0d0 q e (+ m x) (- theta 1.0d0))))
		      (expt (/ p phi) (+ m x)))))
      (+ (loop for u = (unit-random)
	       for x = (negative-binomial-rand-integer s)
	       while (and (>= u (LB x)) (> u (T1 x)))
	       finally (return x))
	 (if (failuresp distribution) 0 alpha)))))

(defmethod rand ((distribution negative-binomial-distribution))
  (if (real-integer-p (successes distribution))
      (negative-binomial-rand-integer distribution)
    (negative-binomial-rand-real distribution)))

(defmethod mean ((distribution negative-binomial-distribution))
  (let ((p (probability distribution)))
    (+ (/ (* (successes distribution) (- 1.0d0 p)) p)
       (if (failuresp distribution) 0 (successes distribution)))))

(defmethod variance ((distribution negative-binomial-distribution))
  (let ((p (probability distribution)))
    (/ (* (successes distribution) (- 1.0d0 p)) p p)))

(defun negative-binomial-distribution-estimate-maximum-likelihood
    (successes trials &optional (failuresp t))
    "Estimate based on the number of successes in a given number of trials.
FAILURESP works as in NEGATIVE-BINOMIAL-DISTRIBUTION."
  (negative-binomial-distribution successes (/ successes trials) failuresp))

(defun negative-binomial-distribution-estimate-unbiased
    (successes trials &optional (failuresp t))
    "Estimate based on the number of successes in a given number of trials.
FAILURESP works as in NEGATIVE-BINOMIAL-DISTRIBUTION."
  (negative-binomial-distribution
   successes (/ (1- successes) (1- trials)) failuresp))


;;; 18. Poisson Distribution

(defclass poisson-distribution (discrete-distribution)
  ((rate :initarg :rate :reader rate)))

(defmethod print-object ((obj poisson-distribution) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream ": RATE = ~a" (rate obj))))

(defun poisson-distribution (rate)
  (assert (and (realp rate) (> rate 0)) (rate)
	  "RATE should be a positive real number.")
  (make-instance 'poisson-distribution :rate rate))

(defmethod cdf ((distribution poisson-distribution) k)
  (regularized-gamma (1+ k) (rate distribution)))

(defmethod mass ((distribution poisson-distribution) k)
  (let ((l (rate distribution)))
    (/ (* (exp (- l)) (expt l k)) (gamma (1+ k)))))

;;; Compute a guess by the 2nd-order Cornish-Fisher expansion.
;;; See the notes at the binomial distribution quantile.
(defmethod quantile ((distribution poisson-distribution) p)
  (let* ((l (rate distribution))	; = mean
	 (variance (sqrt l))
	 (k3 (/ variance))
	 (z (quantile (standard-normal-distribution) p))
	 (guess (round (+ l (* variance (+ z (* k3 (/ (1- (* z z)) 6.0d0))))))))
    (print guess)
    (search-for-quantile distribution p guess :min 0)))

(defmethod rand ((distribution poisson-distribution))
  "TODO: Inefficient implementation. Maybe we should store some of the data."
  (let* ((l (rate distribution))
	 (u (/ (unit-random) (exp (- l)))))
    (loop for i upfrom 0
       sum (/ (expt l i) (gamma (1+ i))) into s
       while (>= u s)
       finally (return i))))

(defmethod mean ((distribution poisson-distribution))
  (rate distribution))

(defmethod variance ((distribution poisson-distribution))
  (rate distribution))

(defun poisson-distribution-estimate (sequence)
  "Maximum likelihood estimate, also unbiased and minimum variance."
  (poisson-distribution (mean sequence)))


;;; 19. Weibull Distribution

(defclass weibull-distribution (continuous-distribution)
  ((scale :initarg :scale :reader scale)
   (shape :initarg :shape :reader shape)))

(defmethod print-object ((obj weibull-distribution) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream ": SCALE = ~a, SHAPE = ~a" (scale obj) (shape obj))))

(defun weibull-distribution (scale shape)
  (assert (and (realp scale) (> scale 0)) (scale)
	  "SCALE should be a positive real number.")
  (assert (and (realp shape) (> shape 0)) (shape)
	  "SHAPE should be a positive real number.")
  (make-instance 'weibull-distribution :scale scale :shape shape))

(defmethod cdf ((distribution weibull-distribution) x)
  (- 1.0d0 (exp (- (expt (/ x (scale distribution)) (shape distribution))))))

(defmethod density ((distribution weibull-distribution) x)
  (if (< x 0)
      0
      (let ((l (scale distribution))
	    (k (shape distribution)))
	(* (/ k l) (expt (/ x l) (1- k)) (exp (- (expt (/ x l) k)))))))

(defmethod quantile ((distribution weibull-distribution) p)
  (* (expt (- (log (- 1.0d0 p))) (/ (shape distribution)))
     (scale distribution)))

(defmethod rand ((distribution weibull-distribution))
  (* (scale distribution)
     (expt (- (log (unit-random))) (/ (shape distribution)))))

(defmethod mean ((distribution weibull-distribution))
  (let ((shape (shape distribution)))
    (* (scale distribution) (gamma (/ (+ shape 1.0d0) shape)))))

(defmethod variance ((distribution weibull-distribution))
  (let ((shape (shape distribution)))
    (* (sqr (scale distribution))
       (- (gamma (/ (+ shape 2.0d0) shape))
	  (sqr (gamma (/ (+ shape 1.0d0) shape)))))))

;;; Simple binary search implementation.
(defun weibull-distribution-estimate (sequence)
  "Maximum likelihood estimate."
  (flet ((sumx (beta &optional lnxp)
	   (loop for xi in sequence
	      sum (if lnxp
		      (* (expt xi beta) (log xi))
		      (expt xi beta)))))
    (let* ((n (length sequence))
	   (r (/ (sumx 0 t) n)))
      (flet ((fn (beta) (- (/ (sumx beta t) (sumx beta)) (/ beta) r)))
	(let* ((min (loop for x = 1.0d0 then (/ x 2.0d0)
		       while (> (fn x) 0) finally (return x)))
	       (max (loop for x = 1.0d0 then (* x 2.0d0)
		       while (< (fn x) 0) finally (return x)))
	       (shape (real-binary-search #'fn min max)))
	  (weibull-distribution (expt (/ (sumx shape) n) (/ shape)) shape))))))

;;; get the most frequent object in sequence
;;; Usually, this is for Categorical valued data
;;; return: object, count
(defun mode (seq)
  (let* ((alist (count-values seq))
         (val-num (when alist (car (sort alist #'> :key #'cdr)))))
    (when val-num
      (values (car val-num) (cdr val-num)))))



;;;;;;;;;;;;;;;;;;;;;
; distribution-test ;
;;;;;;;;;;;;;;;;;;;;;
;;; ref. 統計学入門(東京大学出版)
;;; 適合度のカイ二乗検定 chi-square-test of goodness of fit
(defun chi-square-test (freq-list prob-list significance-level)
  (assert (eql (length freq-list) (length prob-list)))
  (assert (notevery #'zerop freq-list))  
  (assert (< 0 significance-level 1))
  (if (notany #'zerop prob-list)
      (let* ((np-list (loop with n = (apply #'+ freq-list)
                          for f in freq-list
                          for p in prob-list
                          as np = (* n p) collect np))
             (chi-square (apply #'+ (mapcar (lambda (f np) (/ (expt (- f np) 2) np))
                                            freq-list np-list)))
             (chi-dist (chi-square-distribution (1- (length freq-list))))
             (lower-cd (- 1 significance-level))
             (percentile (handler-case (quantile chi-dist lower-cd)
                           (FLOATING-POINT-OVERFLOW (c) (declare (ignore c))
                             (quantile-ili (gamma-distribution 2.0d0 (* 0.5d0 (degree chi-dist)))
                                           lower-cd)))))
        (values (if (> chi-square percentile)
                    :rejected
                  :not-rejected)
                np-list
                chi-square
                percentile))
    (values :na (make-list (length freq-list) :initial-element :na) :na :na)))
;;; 得られたヒストグラムのある確率分布に対する適合度をカイ二乗検定
;;; input: distribution, ある確率分布
;;; freq-list, 度数リスト
;;; range-list, 階級範囲リスト, 値の小さい方から順番に連続して並んでいなければならない
;;; significance-list, 有意水準 ( 0 < 1 )
(defun chi-square-test-with-distribution (distribution freq-list range-list significance-level)
  (assert (eql (length freq-list) (length range-list)))
  (flet ((range-prob (range)
           (let ((inf (car range)) (sup (cdr range)))
             (- (cdf distribution sup) (cdf distribution inf)))))
    (let ((init-p-list (mapcar #'range-prob range-list)))
      (when (every #'zerop init-p-list) (error "The distribution is completely mischoice"))
      (multiple-value-bind (p-list freq-list range-list)
          (merge-class-prob init-p-list freq-list range-list)
        (multiple-value-bind (res np-list chi^2 percentile)
            (chi-square-test freq-list p-list significance-level)
          (values res (loop for freq in freq-list
                          for range in range-list
                          for prob in p-list
                          for np in np-list
                          collect `(:range ,range :probability ,prob :freq ,freq :expected-freq ,np))
                  chi^2 percentile))))))
(defun merge-class-prob (p-list freq-list range-list)
  (let ((total-freq (apply #'+ freq-list)))
    (assert (> total-freq 0) (freq-list))
    (cond ((eql 2 (length p-list)) (values p-list freq-list range-list))
          ((some (lambda (p) (< (* p total-freq) 1d0)) p-list)
           (let* ((pos (position-if (lambda (p) (< (* p total-freq) 1d0)) p-list :from-end nil))
                  (next-pos (if (eql pos (1- (length p-list))) (1- pos) (1+ pos)))
                  (pos-range (nth pos range-list))
                  (next-range (nth next-pos range-list))
                  (new-range (cons (apply #'min `(,(car pos-range) ,(car next-range)))
                                   (apply #'max `(,(cdr pos-range) ,(cdr next-range)))))
                  (new-freq (+ (nth pos freq-list) (nth next-pos freq-list)))
                  (new-p (+ (nth pos p-list) (nth next-pos p-list))))
             (loop for i from 0 for p in p-list
                 for freq in freq-list
                 for range in range-list
                 if (eql i pos)
                 collect new-p into merged-p-list and
                 collect new-freq into merged-freq-list and
                 collect new-range into merged-range-list
                 else if (not (eql i next-pos))
                 collect p into merged-p-list and
                 collect freq into merged-freq-list and
                 collect range into merged-range-list
                 finally (return (merge-class-prob merged-p-list merged-freq-list merged-range-list)))))
          (t (values p-list freq-list range-list)))))

    
;;; refer R-code at http://aoki2.si.gunma-u.ac.jp/R/
#||
normal-dist-test (正規分布検定)

OUTPUT( 3 values of property-list )
 result (:total :mean :variance :SD)
 table (:MID 各階級の中心値
        :FREQ 各級の度数
        :Z 級限界の標準化得点
        :CDF 累積確率
        :EXPECTATION 期待値)
 result2 (:CHI-SQ カイ二乗統計量 :D.F. 自由度 :P-VALUE P-値)
||#
(defun normal-dist-test (freq-seq       ; sequence of frequency
                         inf            ; infimum of the first class
                         width          ; class width
                         precision)     ; precision of measurement
  (assert (notevery #'zerop freq-seq))
  (assert (> width 0))
  (assert (>= precision 0))
  (let* ((n (reduce #'+ freq-seq))
         (x `(0 ,@(coerce freq-seq 'list) 0))
         (k (length x))
         (mid (mapcar #'(lambda (val)
                     (- val (/ precision 2)))
                 (loop for i from 0 below k
                     with init-val = (- inf (/ width 2))
                     collect (+ init-val (* i width)))))
         (xbar (/ (apply #'+ (mapcar #'* x mid)) n))
         (variance (/ 
                    (apply #'+ 
                           (mapcar #'* x
                                   (mapcar #'(lambda (a)
                                               (sqr (- a xbar)))
                                           mid)))
                    n))
         (sd (sqrt variance))
         (result `(:total ,n :mean ,xbar :variance ,variance :sd ,sd))
         
         (z (mapcar #'(lambda (a)
                        (/ (- (+ a (/ width 2)) xbar) sd))
                    mid))
         (p (mapcar #'(lambda (a)
                        (cdf (standard-normal-distribution) a))
                    z)))
    (setf (nth (1- k) p) 1
          p (mapcar #'- p
                    `(0 ,@(subseq p 0 (1- k)))))
    (let* ((expectation (mapcar #'(lambda (a) (* n a)) p))
           (table `(:mid ,mid :freq ,x 
                         :z ,z :cdf ,p
                         :expectation ,expectation)))
      (setq x (copy-seq x)
            expectation (copy-seq expectation))
      (loop while (< (first expectation) 1)
          do (incf (nth 1 x) (nth 0 x))
             (incf (nth 1 expectation) (nth 0 expectation))
             (decf k)
             (setf x (cdr x)
                   expectation (cdr expectation)))
      (loop while (< (nth (1- k) expectation) 1)
          do (decf k)
             (incf (nth (1- k) x) (nth k x))
             (incf (nth (1- k) expectation) (nth k expectation))
             (setf x (subseq x 0 k)
                   expectation (subseq expectation 0 k)))
      (let* ((chisq (apply #'+
                           (mapcar #'(lambda (x-val exp-val)
                                       (/ (expt (- x-val exp-val) 2)
                                          exp-val))
                                   x expectation)))
             result2)
        (decf k 3)
        (setf p (- 1 (cdf (chi-square-distribution k) chisq))
              result2 `(:chi-sq ,chisq :d.f. ,k :p-value ,p))
        
        (values result table result2)))))
#||
STAT(6): (normal-dist-test '(4 19 86 177 105 33 2) 40 5 0.1)
(:TOTAL 426 :MEAN 57.931225 :VARIANCE 26.352928 :SD 5.13351)
(:MID (37.45 42.45 47.45 52.45 57.45 62.45 67.45 72.45 77.45) :FREQ
(0 4 19 86 177 105 33 2 0) :Z
(-3.5027153 -2.5287228 -1.5547304 -0.58073795 0.3932545 1.3672462
 2.3412387 3.315231 4.2892237)
:CDF
(2.3027066827641107d-4 0.005493650023016494d0 0.0542812231219722d0
 0.2207033969433026d0 0.3722256949242654d0 0.2612916822967053d0
 0.07616414571442975d0 0.009152099332533692d0 4.578369754981715d-4)
:EXPECTATION
(0.09809530468575112d0 2.4383902144907776d0 23.123801049960157d0
 94.01964709784691d0 158.56814603773705d0 111.31025665839645d0
 32.44592607434708d0 4.093832867221574d0 0.19503855156222105d0))
(:CHI-SQ 6.000187256825313d0 :D.F. 4 :P-VALUE 0.19913428945535006d0)
||#


#||
poisson-dist-test (ポアソン分布検定)

OUTPUT( 3 values of p-list )
result (:N 全度数
        :MEAN 平均)
table (:C-ID 仮の階級値
       :FREQ 度数
       :P 確率
       :E 期待値)
result2 (:CHI-SQ カイ二乗統計量 
         :D.F. 自由度
         :P-VALUE P-値)
||#
(defun poisson-dist-test (d)            ; sequence of frequency
  (assert (> (length d) 1))
  (let* ((k (length d))
         (n (apply #'+ (coerce d 'list)))
         (x (loop for i from 0 below k
                collect i))
         (mean (coerce (/ (apply #'+ (map 'list #'* d x)) n)
                       *read-default-float-format*))
         (result `(:n ,n :mean ,mean))
         (p (mapcar #'(lambda (num)
                        (/ (* (exp (- 0 mean))
                              (expt mean num))
                           (do ((i 1 (1+ i))
                                (fact 1 (* fact i)))
                               ((> i num) fact))))
                    x)))
    (setf (nth (1- k) p)
      (- 1 (apply #'+ (subseq p 0 (1- k)))))
    (let* ((e (mapcar #'(lambda (num)
                          (* n num)) p))
           (table `(:c-id ,x :freq ,d :p ,p :e ,e)))
      (setq d (copy-seq d)
            e (copy-seq e))
      (loop while (< (nth 0 e) 1)
          do (setf (nth 1 d) (+ (nth 1 d) (nth 0 d))
                   (nth 1 e) (+ (nth 1 e) (nth 0 e))
                   d (cdr d)
                   e (cdr e))
             (decf k))
      (loop while (< (nth (1- k) e) 1)
          do (setf (nth (- k 2) d) (+ (nth (- k 2) d) (nth (1- k) d))
                   (nth (- k 2) e) (+ (nth (- k 2) e) (nth (1- k) e))
                   d (subseq d 0 (1- (length d)))
                   e (subseq e 0 (1- (length e))))
             (decf k))
      (let* ((chisq (apply #'+
                           (mapcar #'(lambda (d-val e-val)
                                       (/ (expt (- d-val e-val) 2)
                                          e-val))
                                   d e)))
             result2)
        (decf k 2)
        (setf p (- 1 (cdf (chi-square-distribution k) chisq))
              result2 `(:chi-sq ,chisq :d.f. ,k :p-value ,p))
        (values result table result2)))))
#||
STAT(10): (poisson-dist-test '(27 61 77 71 54 35 20 11 6 2 1))
(:N 365 :MEAN 1092/365)
(:C-ID (0 1 2 3 4 5 6 7 8 9 ...) :FREQ (27 61 77 71 54 35 20 11 6 2 ...)
 :P
 (0.050197963 0.1501813 0.22465476 0.22403927 0.1675691 0.100266
  0.04999565 0.021368004 0.0079910485 0.002656385 ...)
 :E
 (18.322256 54.816174 81.998985 81.77434 61.162724 36.59709 18.248411
  7.7993217 2.9167328 0.96958053 ...))
(:CHI-SQ 14.143778 :D.F. 8 :P-VALUE 0.07809402061210624d0)
||#

#||
二項分布検定(binom-dist-test)

OUTPUT( 3 values of p-list )
result (:D-SIZE サンプルサイズ
        :PROBABILITY) 母比率
table (:FREQ 度数分布
       :P 確率
       :E) 期待値
result2 (:CHI-SQ カイ二乗統計量
         :D.F. 自由度
         :P-VALUE) P-値
||#
(defun binom-dist-test (d               ; sequence of frequency
                        x               ; sequence of class-value
                        size)           ; size of Bernoulli trials
  (let ((k (length d)))
    (assert (= k (length x)))
    (assert (notany #'minusp d))
    (assert (notany #'minusp x))
    (assert (every #'integerp d))
    (assert (every #'integerp x))
    (assert (every #'(lambda (a) (>= size a)) x))

    (let* ((n (apply #'+ (coerce d 'list)))
           (prob 
            (coerce (/ (/ (apply #'+ (map 'list #'* d x)) n) size)
                    *read-default-float-format*))
           (binom-dist (binomial-distribution size prob))
           (p (map 'list #'(lambda (val)
                             (coerce (mass binom-dist val)
                                     *read-default-float-format*)) x))
           (e (mapcar #'(lambda (val)
                          (* val n)) p))
           (result `(:size ,k :probability ,prob))
           (table `(:freq ,d :p ,p :e ,e)))
      (setq d (copy-seq d)
            e (copy-seq e))
      (loop while (< (nth 0 e) 1)
          do (setf (nth 1 d) (+ (nth 1 d) (nth 0 d))
                   (nth 1 e) (+ (nth 1 e) (nth 0 e))
                   d (cdr d)
                   e (cdr e))
             (decf k))
      (loop while (< (nth (1- k) e) 1)
          do (setf (nth (- k 2) d) (+ (nth (- k 2) d) (nth (1- k) d))
                   (nth (- k 2) e) (+ (nth (- k 2) e) (nth (1- k) e))
                   d (subseq d 0 (1- k))
                   e (subseq e 0 (1- k)))
             (decf k))
      (let* ((chisq (apply #'+
                           (mapcar #'(lambda (d-val e-val)
                                       (/ (expt (- d-val e-val) 2)
                                          e-val))
                                   d e)))
             result2)
        (decf k 2)
        (setf p (- 1 (cdf (chi-square-distribution k) chisq))
              result2 `(:chi-sq ,chisq :d.f. ,k :p-value ,p))
        (values result table result2)))))


;;;;;;;;;;;;;;;;;;;;;;;;
; Outlier verification ;
;;;;;;;;;;;;;;;;;;;;;;;;
;;; Smirnov-Grubbs test
;;; http://aoki2.si.gunma-u.ac.jp/lecture/Grubbs/Grubbs.html
(defun smirnov-grubbs (seq alpha &key (type :max) (recursive nil) (sig-p-hash nil))
  "length of seq must be more than 4"
  (assert (> 1 alpha 0))
  (assert (every #'numberp seq))
  (if (>= (length seq) 4)
      (let* ((target (case type 
                       (:max (reduce #'max seq))
                       (:min (reduce #'min seq))))
             (target-pos (position target seq :test #'=)))
        (multiple-value-bind (ok t_i sig-p n)
            (smirnov-grubbs-p seq target-pos alpha :sig-p-hash sig-p-hash)
          (cond ((and recursive ok) seq)
                ((and recursive (not ok))
                 (multiple-value-bind (%seq removed-poss)
                     (smirnov-grubbs (loop for i below (length seq)
                                         unless (eql i target-pos)
                                         collect (elt seq i))
                                     alpha :type type :recursive recursive
                                     :sig-p-hash sig-p-hash)
                   (values %seq
                           (if removed-poss 
                               (cons target-pos 
                                     (mapcar (lambda (pos) (if (>= pos target-pos) (1+ pos) pos))
                                             removed-poss))
                             `(,target-pos)))))
                (t
                 (princ (format nil "~&Data: ~A = ~,3F~%" type target))
                 (princ (format nil "~&t= ~,3F, p-value = ~,3F, df = ~A~%"
                                t_i sig-p (- n 2)))
                 nil))))
    (error "The sequence is too short. It should be more than 4.")))
;;; input: sequence, target position, alpha
;;; return: boolean( t -> o.k. nil -> outlier )
(defun smirnov-grubbs-p (seq position alpha &key (sig-p-hash nil))
  (assert (> 1 alpha 0))
  (assert (every #'numberp seq))
  (when (and (>= (length seq) 4) position)
    (let* ((target (elt seq position))
           (n (length seq))
           (m (/ (reduce #'+ seq) n))
           (u-dev (sqrt
                   (/ (loop for i below n as val = (elt seq i)
                          sum (expt (- val m) 2))
                      (1- n))))
           (sig-p (when (hash-table-p sig-p-hash)
                    (cdr (assoc alpha (gethash n sig-p-hash) :test #'=)))))
      (unless sig-p (setf sig-p (get-sig-p n alpha)))
      (if (zerop u-dev)
          (values t nil sig-p n)
        (let ((t_i (/ (abs (- target m)) u-dev)))
          (values (< t_i sig-p) t_i sig-p n))))))
(defun get-sig-p (n alpha)
  (let* ((dist (t-distribution (- n 2)))
         (t_alpha^2 (expt 
                     (quantile
                      dist
                      (- 1 (/ (/ (* 100 alpha) n)
                              100))) 2)))
    (* (1- n)
       (sqrt (/ t_alpha^2
                (+ (* n (- n 2))
                   (* n t_alpha^2)))))))
(defun make-sig-p-hash (n alpha)
  (assert (>= n 4))
  (let ((hash (make-hash-table :test #'eql)))
    (loop for i from 4 to n
        do (setf (gethash i hash) 
             `(,(cons alpha (get-sig-p i alpha))))
        finally (return hash))))


;; update-distribution call timing?
;; Main Focus: change strategy to 'reuse' a dist instance for changing parameter

(in-package :statistics.distribution)

(defclass distribution ()
  ((mean)
   (variance)
   (skewness)
   (kurtosis)
   (mode)))

(defgeneric update-distribution (distribution)
  (:method (distribution)
	   distribution))

(defmethod initialize-instance ((instance distribution) &rest initargs)
  (declare (ignore initargs))
  (call-next-method)
  (update-distribution instance))

(defclass discrete-distribution (distribution) ())
(defclass continuous-distribution (distribution) ())

(defdistribution gamma-like-distribution (continuous-distribution)
  ((scale :initarg :scale :accessor scale)
   (shape :initarg :shape :accessor shape)))
(defmethod update-distribution ((distribution gamma-like-distribution))
  (with-slots (scale shape mean variance skewness kurtosis mode) distribution
    (assert (and (realp scale) (> scale 0)) (scale)
      "SCALE should be a positive real number.")
    (assert (and (realp shape) (> shape 0)) (shape)
      "SHAPE should be a positive real number.")
    (setf mean (* shape scale))
    (setf variance (* mean scale))
    (setf skewness (/ 2d0 (sqrt shape)))
    (setf kurtosis (/ (* 3d0 (+ shape 2d0)) shape))
    (if (> shape 1d0)
	(setf mode (* (- shape 1d0) scale))
      (slot-makunbound distribution 'mode)))
  distribution)

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

;;; statistics measures

(defgeneric mean (obj))
(defmethod mean ((sequence sequence))
  (/ (reduce #'+ sequence) (length sequence)))
(defmethod mean ((distribution distribution))
  (if (slot-boundp distribution 'mean)
      (slot-value distribution 'mean)
    (error "Mean is undefined for distribution ~S" distribution)))

(defgeneric variance (obj))
(defmethod variance ((sequence sequence))
  (/ (sum-on-deviation #'sqr sequence) (length sequence)))
(defmethod variance ((distribution distribution))
  (if (slot-boundp distribution 'variance)
      (slot-value distribution 'variance)
    (error "Variance is undefined for distribution ~S" distribution)))

(defun standard-deviation (sequence &key populationp)
  "Sample standard deviation; or population standard deviation if POPULATIONP."
  (sqrt (coerce (/ (sum-on-deviation #'sqr sequence)
		   (if populationp (length sequence) (1- (length sequence))))
		'double-float)))

(defun sum-on-deviation (function sequence)
  (let ((mean (mean sequence)))
    (reduce (lambda (sum x) (+ sum (funcall function (- x mean)))) sequence
	    :initial-value 0)))

(defgeneric skewness (obj))
(defmethod skewness ((sequence sequence))
  (let ((ave (mean sequence))
	(var (variance sequence)))
    (/ (reduce #'+ sequence :key #'(lambda (x) (int-power (- x ave) 3)))
       (* (length sequence) (half-integer-power var #.(/ 3 2))))))
(defmethod skewness ((distribution distribution))
  (if (slot-boundp distribution 'skewness)
      (slot-value distribution 'skewness)
    (error "Skewness is undefined for distribution ~S" distribution)))

;; note: gauss ditribution's kurtosis = 3 (not 3-minused kurtosis)

(defgeneric kurtosis (obj))
(defmethod kurtosis ((sequence sequence))
  (let ((ave (mean sequence))
	(var (variance sequence)))
    (/ (reduce #'+ sequence :key #'(lambda (x) (int-power (- x ave) 4)))
       (* (length sequence) (int-power var 2)))))
(defmethod kurtosis ((distribution distribution))
  (if (slot-boundp distribution 'kurtosis)
      (slot-value distribution 'kurtosis)
    (error "Kurtosis is undefined for distribution ~S" distribution)))

(defgeneric mode (obj &optional test))
(defmethod mode ((seq sequence) &optional (test #'eql))
  (let ((hash (make-hash-table :test test))
	(max 0)
	value)
    (do ((i 0 (+ i 1))
         (n (length seq)))
        ((= i n) (values value max))
      (declare (type integer i n))
      (let* ((val (elt seq i))
	     (count (incf (gethash val hash 0))))
	(when (> count max)
	  (setf max count value val))))))
(defmethod mode ((distribution distribution) &optional test)
  (declare (ignore test))
  (if (slot-boundp distribution 'mode)
      (slot-value distribution 'mode)
    (error "Mode is undefined for distribution ~S" distribution)))

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

(defdistribution normal-distribution (continuous-distribution)
  ((average :initarg :average :accessor average)
   (std     :initarg :std     :accessor std)
   (skewness :initform 0d0)
   (kurtosis :initform 3d0)))

(defmethod update-distribution ((distribution normal-distribution))
  (with-slots (average std mean variance mode) distribution
    (assert (realp average) (average)
      "AVERAGE should be a real number.")
    (assert (and (realp std) (> std 0)) (std)
      "STD should be a positive real number.")
    (setf mean average)
    (setf variance (* std std))
    (setf mode average)
    )
  distribution)

(defmethod print-object ((obj normal-distribution) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream ": AVERAGE = ~a, STD = ~a"
	    (average obj) (std obj))))

(defmethod cdf ((distribution normal-distribution) x)
  (flet ((phi (x) (/ (1+ (erf (/ x #.(sqrt 2.0d0)))) 2.0d0)))
    (phi (/ (- x (average distribution)) (std distribution)))))

(defmethod density ((distribution normal-distribution) x)
  (flet ((phi (x) (/ (exp (/ (sqr x) -2.0d0)) (sqrt (* 2.0d0 pi)))))
    (let ((std (std distribution)))
      (/ (phi (/ (- x (average distribution)) std)) std))))

(defmethod quantile ((distribution normal-distribution) p)
  (+ (average distribution)
     (* (std distribution) (sqrt 2.0d0)
	(let ((x (1- (* 2.0d0 p))))
	  (cond ((= x -1.0d0) #.(erf-inverse (1- double-float-epsilon)))
		((= x 1.0d0) #.(erf-inverse (- 1.0d0 double-float-epsilon)))
		(t (erf-inverse x)))))))

(defmethod rand ((distribution normal-distribution))
  (with-slots (average std) distribution
    (declare (optimize (speed 3) (safety 0) (debug 0))
	     (type double-float average std))
    (normal-random average std)))
  
(defun normal-distribution-estimate-unbiased (sequence)
  (normal-distribution (mean sequence)
		       (standard-deviation sequence :populationp nil)))

(defun normal-distribution-estimate-maximum-likelihood (sequence)
  (normal-distribution (mean sequence)
		       (standard-deviation sequence :populationp t)))

(defun normal-distribution (average std)
  (assert (realp average) (average)
    "AVERAGE should be a real number.")
  (assert (and (realp std) (> std 0)) (std)
	  "STD should be a positive real number.")
  (make-instance 'normal-distribution
		 :average average :std std))

(let ((standard (normal-distribution 0.0d0 1.0d0)))
  (defun standard-normal-distribution ()
    standard))

;;; 2. Lognormal Distribution

(defdistribution log-normal-distribution (continuous-distribution)
  ((average :initarg :average :accessor average)
   (std     :initarg :std     :accessor std)))

(defmethod update-distribution ((distribution log-normal-distribution))
  (with-slots (average std mean variance skewness kurtosis mode) distribution
    (assert (realp average) (average)
      "AVERAGE should be a real number.")
    (assert (and (realp std) (> std 0)) (std)
      "STD should be a positive real number.")
    (let ((std2 (* std std)))
      (setf mean (exp (+ (average (/ std2 2d0)))))
      (setf variance (* (exp (+ (* 2d0 average) std2))
			(- (exp std2) 1d0)))
      (setf skewness (* (+ (exp std2) 2d0)
			(sqrt (- (exp std2) 1d0))))
      (setf kurtosis (+ (exp (* 4d0 std2))
			(* 2d0 (exp (* 3d0 std2)))
			(* 3d0 (exp (* 2d0 std2)))
			-3d0))
      (setf mode (exp (- average std2)))))
  distribution)

(defmethod print-object ((obj log-normal-distribution) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream ": AVERAGE = ~a, STD = ~a"
	    (average obj) (std obj))))

(defun log-normal-distribution (average std)
  (assert (realp average) (average)
	  "AVERAGE should be a real number.")
  (assert (and (realp std) (> std 0)) (std)
	  "STD should be a positive real number.")
  (make-instance 'log-normal-distribution
		 :average average :std std))

(defmethod cdf ((distribution log-normal-distribution) x)
  (+ (* (erf (/ (- (log x) (average distribution))
		(* (sqrt 2.0d0) (std distribution))))
	0.5d0)
     0.5d0))

(defmethod density ((distribution log-normal-distribution) x)
  (let ((std (std distribution)))
    (/ (exp (/ (sqr (- (log x) (average distribution)))
	       (* -2.0d0 (sqr std))))
       (* x std (sqrt (* 2.0d0 pi))))))

(defmethod quantile ((distribution log-normal-distribution) p)
  (exp (+ (average distribution)
	  (* (std distribution) (sqrt 2.0d0)
	     (let ((x (1- (* 2.0d0 p))))
	       (cond ((= x -1.0d0)
		      #.(erf-inverse (1- double-float-epsilon)))
		     ((= x 1.0d0)
		      #.(erf-inverse (- 1.0d0 double-float-epsilon)))
		     (t (erf-inverse x))))))))

(defmethod rand ((distribution log-normal-distribution))
  (exp (normal-random (average distribution) (std distribution))))

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

(defdistribution uniform-distribution (continuous-distribution)
  ((from :initarg :from :accessor uniform-from)
   (to :initarg :to :accessor uniform-to)
   (width :reader uniform-width)
   (denominator :reader uniform-denom)
   (skewness :initform 0d0)
   (kurtosis :initform 1.8d0)))

(defmethod print-object ((obj uniform-distribution) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream ": [~a, ~a]" (uniform-from obj) (uniform-to obj))))

(defmethod update-distribution ((distribution uniform-distribution))
  (with-slots (from to width denominator mean variance skewness kurtosis) distribution
    (assert (and (realp from) (realp to) (> to from)) (from to)
      "FROM and TO should be real numbers such that TO > FROM.")
    (setf width (dfloat (- to from)))
    (setf denominator (/ width))
    (setf mean (/ (+ from to) 2d0))
    (setf variance (/ (* width width) 12.0d0)))
  distribution)

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
     (* p (uniform-width distribution))))

(defmethod rand ((distribution uniform-distribution))
  (+ (uniform-from distribution)
     (* (uniform-width distribution) (unit-random :[]))))

(defun uniform-distribution-estimate-moments (sequence)
  (let ((mu (mean sequence))
	(s3 (sqrt (* 3.0d0 (variance sequence)))))
    (uniform-distribution (- mu s3) (+ mu s3))))

(defun uniform-distribution-estimate-maximum-likelihood (sequence)
  (uniform-distribution (reduce #'min sequence) (reduce #'max sequence)))

;;; 4. Erlang Distribution

(defdistribution erlang-distribution (gamma-like-distribution)
  ((include-zero :initarg :include-zero :initform nil :accessor include-zero)))

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
  (erlang-random (shape distribution) (scale distribution) (include-zero distribution)))

(defun erlang-distribution-estimate (sequence)
  "Estimates by matching moments."
  (destructuring-bind (scale shape) (gamma-like-distribution-estimate sequence)
    (erlang-distribution scale (round shape))))

;;; 5. Exponential Distribution

(defdistribution exponential-distribution (continuous-distribution)
  ((hazard :initarg :hazard :accessor hazard)
   (scale  :reader scale)
   (include-zero :initarg :include-zero :initform nil :accessor include-zero)
   (skewness :initform 2d0)
   (kurtosis :initform 9d0)
   (mode :initform 0d0)))

(defmethod update-distribution ((distribution exponential-distribution))
  (with-slots (hazard scale mean variance) distribution
    (assert (and (realp hazard) (> hazard 0)) (hazard)
      "SCALE should be a positive real number.")
    (setf scale (dfloat (/ hazard)))
    (setf mean scale)
    (setf variance (* scale scale)))
  distribution)

#+ignore
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
  (exp-random (scale distribution) (include-zero distribution)))

(defun exponential-distribution-estimate (sequence)
  "Unbiased maximum likelihood estimate."
  (exponential-distribution (mean sequence) nil))

;;; 6. Gamma Distribution

(defdistribution gamma-distribution (gamma-like-distribution)
  ((gamma-factor :reader gamma-factor)
   (shape-inv)
   (d)
   (c)))

(defmethod update-distribution ((distribution gamma-distribution))
  (call-next-method)
  (with-slots (scale shape gamma-factor shape-inv d c) distribution
    (setf gamma-factor (gamma shape))
    (setf shape-inv (/ shape))
    (cond ((> shape 1d0)
	   (setf d (- shape #.(dfloat 1/3))))
	  ((< shape 1d0)
	   (setf d (- (+ shape 1d0) #.(dfloat 1/3)))))
    (unless (= shape 1d0)
      (setf c (/ (sqrt (* 9d0 d)))))
    )
  distribution)

(defmethod print-object ((obj gamma-distribution) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream ": SCALE = ~a, SHAPE = ~a" (scale obj) (shape obj))))

(defun gamma-distribution (scale shape)
  (assert (and (realp scale) (> scale 0)) (scale)
	  "SCALE should be a positive real number.")
  (assert (and (realp shape) (> shape 0)) (shape)
	  "SHAPE should be a positive real number.")
  (make-instance 'gamma-distribution :scale scale :shape shape))

(defmethod cdf ((distribution gamma-distribution) x)
  (let ((scale (scale distribution))
	(shape (shape distribution)))
    (/ (lower-incomplete-gamma shape (/ x scale))
       (gamma-factor distribution))))

(defmethod density ((distribution gamma-distribution) x)
  (let ((scale (scale distribution))
	(shape (shape distribution)))
    (/ (* (expt (/ x scale) (1- shape)) ;; cache enable
	  (exp (/ (- x) scale)))
       (* scale (gamma-factor distribution)))))

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

(defmethod rand ((distribution gamma-distribution))
  (with-slots (scale shape d c shape-inv) distribution
    (* scale
       (the double-float
	 (cond ((> shape 1d0) (gamma-compression-shape-big-cached shape d c))
	       ((= shape 1d0) (exp-random 1d0))
	       (t (gamma-compression-shape-small-cached shape shape-inv d c)))))))

(defun gamma-like-distribution-estimate (sequence)
  (let ((mu (mean sequence))
	(s2 (variance sequence)))
    (list (/ s2 mu) (/ (sqr mu) s2))))

(defun gamma-distribution-estimate (sequence)
  "Estimates by matching moments."
  (apply #'gamma-distribution (gamma-like-distribution-estimate sequence)))

;;; 7. Chi-squared Distribution

(defdistribution chi-square-distribution (continuous-distribution)
  ((freedom :initarg :freedom :accessor freedom)
   (eq-gamma :initform (make-instance 'gamma-distribution :shape 2d0 :scale 2d0) :reader eq-gamma)))

(defmethod print-object ((obj chi-square-distribution) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream ": FREEDOM = ~d" (freedom obj))))

(defmethod update-distribution ((distribution chi-square-distribution))
  (with-slots (freedom eq-gamma mean variance skewness kurtosis mode) distribution
    (assert (and (integerp freedom) (> freedom 0)) (freedom)
      "FREEDOM should be a positive integer.")
    (setf (shape eq-gamma) (dfloat (/ freedom 2)))
    (setf mean (slot-value eq-gamma 'mean))
    (setf variance (slot-value eq-gamma 'variance))
    (setf skewness (slot-value eq-gamma 'skewness))
    (setf kurtosis (slot-value eq-gamma 'kurtosis))
    (if (slot-boundp eq-gamma 'mode)
	(setf mode (slot-value eq-gamma 'mode))
      (slot-makunbound distribution 'mode)))
  distribution)

(defun chi-square-distribution (freedom)
  (assert (and (real-integer-p freedom) (> freedom 0)) (freedom)
	  "FREEDOM should be a positive integer.")
  (make-instance 'chi-square-distribution :freedom freedom))

(defmethod cdf ((distribution chi-square-distribution) x)
  (cdf (eq-gamma distribution) x))

(defmethod density ((distribution chi-square-distribution) x)
  (density (eq-gamma distribution) x))

(defmethod quantile ((distribution chi-square-distribution) p)
  (quantile (eq-gamma distribution) p))

(defmethod rand ((distribution chi-square-distribution))
  (case (freedom distribution)
    (1 (let ((u (half-normal-random 1d0)))
	 (declare (type double-float u))
	 (* u u)))
    (2 (exp-random 2d0 nil))
    (t (rand (eq-gamma distribution)))))

;;; 8. Student's t Distribution

(defdistribution t-distribution (continuous-distribution)
  ((freedom :initarg :freedom :accessor freedom)
   (precalc :reader t-precalc)
   (r)
   (b)
   (c)
   (a)
   (d)
   (k)
   (w)
   (s)
   (p)
   (q)
   (t1)
   (t2)
   (v1)
   (v2)))

(defmethod print-object ((obj t-distribution) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream ": FREEDOM = ~d" (freedom obj))))

(defmethod update-distribution ((distribution t-distribution))
  "TODO: This can be done once in a global variable that contains an
adjustable vector, that is adjusted if the new distribution has a larger
freedom than the one precalculated."
  (let ((k (freedom distribution)))
    (with-slots (t-precalc) distribution
      (setf t-precalc (make-array (floor k 2)))
      (setf (elt t-precalc 0) 1)
      (loop
	 for j from 1 below (floor k 2)
	 do (setf (elt t-precalc j)
		  (* (elt t-precalc (1- j))
		     (if (evenp k)
			 (/ (1- (* 2 j)) (* 2 j))
		       (/ (* 2 j) (1+ (* 2 j)))))))))
  (with-slots (freedom r b c a d k w s p q t1 t2 v1 v2) distribution
    (when (> freedom 2)
      (setf r (dfloat freedom))
      (setf b (case freedom
		(3 3.142d0)
		(4 2.968d0)
		(5 2.868d0)
		(6 2.783d0)
		(7 2.756d0)
		(8 2.724d0)
		(t
		 (assert (> freedom 8))
		 (+ 2.5074d0 (expt (* 1.876d0 r) -1.042d0)))))
      (setf c (multiple-value-bind (quotient remainder) (floor freedom 2)
		(declare (type fixnum quotient remainder))
		(if (= remainder 0)
		    ;;; even
		    (/ (loop with ans = 1d0
			   for i fixnum from 1 below quotient do
			     (setf ans (* ans (+ (/ (* 2d0 i)) 1d0)))
			   finally (return ans))
		       (* 2d0 (the double-float (sqrt r))))
		  ;;; odd
		  (/ (loop with ans = 1d0
			 for i fixnum from 1 upto quotient do
			   (setf ans (* ans (+ (/ (- (* 2d0 i) 1d0)) 1d0)))
			 finally (return ans))
		     (* pi (the double-float (sqrt r)))))))
      (setf a (sqrt (* r (- (expt (* 2 b c) (/ 2 (+ freedom 1))) 1d0))))
      (setf d (/ (* b (- (expt 2 (floor +bit-operation-m+ 2)) 2))))
      (setf k (floor (/ (* a (- (expt 2 (floor +bit-operation-m+ 2)) 1)) b)))
      (setf w (/ b (- (expt 2 (floor +bit-operation-m+ 2)) 1)))
      (setf s (/ a (- b a)))
      (setf p (/ r))
      (setf q (/ (+ freedom 1) 2))
      (setf t1 (/ s))
      (setf t2 (/ (+ t1 1d0) 2 b))
      (setf v1 (+ r (* b b)))
      (setf v2 (/ 2d0 (- r 1d0)))))
  (with-slots (freedom r mean variance skewness kurtosis mode) distribution
    (declare (type fixnum freedom)
	     (type double-float r))
    (cond ((> freedom 1)
	   (setf mean 0d0 mode 0d0))
	  (t
	   (slot-makunbound distribution 'mean)
	   (slot-makunbound distribution 'mode)))
    (cond ((> freedom 2)
	   (setf variance (/ r (- r 2d0))))
	  (t
	   (slot-makunbound distribution 'variance)))
    (cond ((> freedom 3)
	   (setf skewness 0d0))
	  (t
	   (slot-makunbound distribution 'skewness)))
    (cond ((> freedom 4)
	   (setf kurtosis (/ (* 3d0 (- r 2d0))
			     (- r 4d0))))
	  (t
	   (slot-makunbound distribution 'kurtosis)))))

(defun t-distribution (freedom)
  (assert (and (real-integer-p freedom) (> freedom 0)) (freedom)
	  "FREEDOM should be a positive integer.")
  (make-instance 't-distribution :freedom freedom))

(defmethod cdf ((distribution t-distribution) x)
  (let* ((k (freedom distribution))
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
  (let ((k (freedom distribution)))
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
  (let* ((n (freedom distribution))
	 (x (* 2.0d0 (if (< p 0.5d0) p (- 1.0d0 p))))
	 (guess (* (if (< p 0.5d0) -1.0d0 1.0d0)
		   (sqrt (* n (1- (/ (incomplete-beta-inverse
				      (/ n 2.0d0) 0.5d0 x))))))))
    (newton-raphson (lambda (x) (- (cdf distribution x) p))
		    (lambda (x) (density distribution x))
		    :initial-guess guess)))

(defmethod rand ((distribution t-distribution))
  (with-slots (freedom r b c a d k w s p q t1 t2 v1 v2) distribution
    (case freedom
      (1 (cauchy-random 0d0 1d0))
      (2 (/ (the double-float (normal-random 0d0 1d0))
	    (the double-float (sqrt (exp-random 1d0)))))
      (t (t-monty-python-bit-cached freedom r b c a d k w s p q t1 t2 v1 v2)))))

(defmethod mean ((distribution t-distribution))
  (assert (> (freedom distribution) 1) ()
	  "MEAN is undefined when FREEDOM is 1.")
  0)

(defmethod variance ((distribution t-distribution))
  (let ((freedom (freedom distribution)))
    (assert (> (freedom distribution) 2) ()
	    "VARIANCE is undefined when FREEDOM is less than 3.")
    (/ freedom (- freedom 2))))

;;; 9. Beta Distribution

(defdistribution beta-distribution (continuous-distribution)
  ((shape1 :initarg :shape1 :accessor shape1)
   (shape2 :initarg :shape2 :accessor shape2)
   (alpha-gamma :initform (make-instance 'gamma-distribution :shape 1d0 :scale 1d0))
   (beta-gamma :initform (make-instance 'gamma-distribution :shape 1d0 :scale 1d0))))

(defmethod print-object ((obj beta-distribution) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream ": SHAPE = (~d, ~d)" (shape1 obj) (shape2 obj))))

(defmethod update-distribution ((distribution beta-distribution))
  (with-slots (shape1 shape2 alpha-gamma beta-gamma mean variance skewness kurtosis mode) distribution
    (assert (and (realp shape1) (> shape1 0)) (shape1)
      "SHAPE1 should be a positive real number.")
    (assert (and (realp shape2) (> shape2 0)) (shape2)
      "SHAPE2 should be a positive real number.")
    (setf (shape alpha-gamma) shape1)
    (setf (shape beta-gamma)  shape2)
    (let ((sum (+ shape1 shape2))
	  (prod (* shape1 shape2)))
      (setf mean (/ shape1 sum))
      (setf variance (/ prod (* sum sum (+ sum 1d0))))
      (setf skewness (/ (* 2d0 (- shape2 shape1) (sqrt (+ sum 1d0)))
			(* (+ sum 2d0) (sqrt prod))))
      (setf kurtosis (/ (* 3d0 (+ sum 1d0) (+ (* prod (- sum 6d0)) (* 2d0 sum sum)))
			(* prod (+ sum 2d0) (+ sum 3d0))))
      (if (and (> shape1 1d0) (> shape2 1d0))
	  (setf mode (/ (- shape1 1d0) (- sum 2d0)))
	(slot-makunbound distribution 'mode))))
  distribution)

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

(defmethod rand ((distribution beta-distribution))
  (let ((alpha (shape1 distribution))
	(beta (shape2 distribution)))
    (cond ((= alpha beta 1d0)
	   (unit-random))
	  ((= alpha beta 0.5d0)
	   (arcsine-random))
	  ((= beta 1d0)
	   (if (= alpha 2d0)
	       (right-triangular-random 0d0 1d0)
	     (power-function-random alpha 0d0 1d0)))
	  ((= alpha 1d0)
	   (if (= beta 2d0)
	       (left-triangular-random 0d0 1d0)
	     (let ((y (power-function-random beta 0d0 1d0)))
	       (declare (type double-float y))
	       (- 1d0 y))))
	  (t
	   (with-slots (alpha-gamma beta-gamma) distribution
	     (let ((y1 (rand alpha-gamma))
		   (y2 (rand beta-gamma)))
	       (declare (type double-float y1 y2))
	       (/ y1 (+ y1 y2))))))))

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

(defdistribution f-distribution (continuous-distribution)
  ((freedom1 :initarg :freedom1 :accessor freedom1)
   (freedom2 :initarg :freedom2 :accessor freedom2)
   (chi1 :initform (make-instance 'chi-square-distribution :freedom 1))
   (chi2 :initform (make-instance 'chi-square-distribution :freedom 1))
   (f)))

(defmethod print-object ((obj f-distribution) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream ": FREEDOMS = (~d, ~d)" (freedom1 obj) (freedom2 obj))))

(defmethod update-distribution ((distribution f-distribution))
  (with-slots (freedom1 freedom2 chi1 chi2 f) distribution
    (assert (and (real-integer-p freedom1) (> freedom1 0)) (freedom1)
	  "FREEDOM1 should be a positive integer.")
    (assert (and (real-integer-p freedom2) (> freedom2 0)) (freedom2)
      "FREEDOM2 should be a positive integer.")
    (setf (freedom chi1) freedom1)
    (setf (freedom chi2) freedom2)
    (setf f (/ (dfloat freedom2) (dfloat freedom1))))
  (with-slots (freedom1 freedom2 chi1 chi2 mean variance skewness kurtosis mode) distribution
    (declare (type fixnum freedom1 freedom2))
    (let* ((r1 (slot-value chi1 'r))
	   (r2 (slot-value chi2 'r))
	   (sum (+ r1 r2)))
      (cond ((> freedom2 2)
	     (setf mean (/ r2 (- r2 2d0))))
	    (t
	     (slot-makunbound distribution 'mean)))
      (cond ((> freedom1 2)
      	     (setf mode (/ (* r2 (- r1 2d0))
			   (* r1 (+ r2 2d0)))))
	    (t
	     (slot-makunbound distribution 'mode)))
      (cond ((> freedom2 4)
	     (setf variance (/ (* 2d0 r2 r2 (- sum 2d0))
			       (* r1 (sqr (- r2 2d0)) (- r2 4d0)))))
	    (t
	     (slot-makunbound distribution 'variance)))
      (cond ((> freedom2 6)
	     (setf skewness (/ (* (+ (* 2d0 r1) r2 -2d0) (sqrt (* 8d0 (- r2 4d0))))
			       (* (- r2 6d0) (sqrt (* r1 (- sum 2d0)))))))
	    (t
	     (slot-makunbound distribution 'skewness)))
      (cond ((> freedom2 8)
	     (setf kurtosis (/ (* 3d0 (- r2 4d0)
				  (+ (* 10d0 r1 (- r1 2d0))
				     (* 4d0 (sqr (- r2 2d0)))
				     (* r1 r2 (+ sum 8d0))))
			       (* r1 (- r2 6d0) (- r2 8d0) (- sum 2d0)))))
	    (t
	     (slot-makunbound distribution 'kurtosis)))))
  distribution)

(defun f-distribution (freedom1 freedom2)
  (assert (and (real-integer-p freedom1) (> freedom1 0)) (freedom1)
	  "FREEDOM1 should be a positive integer.")
  (assert (and (real-integer-p freedom2) (> freedom2 0)) (freedom2)
	  "FREEDOM2 should be a positive integer.")
  (make-instance 'f-distribution :freedom1 freedom1 :freedom2 freedom2))

(defmethod cdf ((distribution f-distribution) x)
  (let ((d1 (freedom1 distribution))
	(d2 (freedom2 distribution)))
    (regularized-incomplete-beta (/ d1 2) (/ d2 2)
				 (/ (* d1 x) (+ (* d1 x) d2)))))

(defmethod density ((distribution f-distribution) x)
  (if (<= x 0)
      0
      (with-slots (freedom1 freedom2 f) distribution
	(/ (* (half-integer-power f (/ freedom1 2))
	      (half-integer-power x (1- (/ freedom2 2))))
	   (* (half-integer-power (1+ (* (/ freedom1 freedom2) x)) (/ (+ freedom1 freedom2) 2))
	      (beta-half freedom1 freedom2))))))

;;; The regularized incomplete beta inverse is too unstable for this.
;; (defmethod quantile ((distribution f-distribution) p)
;;   (let* ((d1 (freedom1 distribution))
;; 	 (d2 (freedom2 distribution))
;; 	 (x (regularized-incomplete-beta-inverse (/ d1 2) (/ d2 2) p)))
;;     (/ (* d2 x) (* (- 1.0d0 x) d1))))

(defmethod quantile ((distribution f-distribution) p)
  "If one freedom is large, it uses the chi-square quantile,
otherwise it uses the beta distribution quantile."
  (let* ((d1 (freedom1 distribution))
	 (d2 (freedom2 distribution)))
    (cond ((and (<= d1 d2) (> d2 400000))
	   (/ (quantile (chi-square-distribution d1) p) d1))
	  ((> d1 400000)
	   (/ d2 (quantile (chi-square-distribution d2) (- 1.0 p))))
	  (t (let ((y (quantile (beta-distribution (/ d1 2) (/ d2 2)) p)))
	       (/ (* d2 y) (* (- 1.0d0 y) d1)))))))

(defmethod rand ((distribution f-distribution))
  (with-slots (chi1 chi2 f) distribution
    (/ (* f (rand chi1))
       (rand chi2))))

(defmethod mean ((distribution f-distribution))
  (let ((d2 (freedom2 distribution)))
    (assert (> d2 2) () "MEAN is undefined when FREEDOM2 <= 2.")
    (/ d2 (- d2 2))))

(defmethod variance ((distribution f-distribution))
  (let ((d1 (freedom1 distribution))
	(d2 (freedom2 distribution)))
    (assert (> d2 4) () "VARIANCE is undefined when FREEDOM2 <= 4.")
    (/ (* 2 d2 d2 (+ d1 d2 -2))
       (* d1 (sqr (- d2 2)) (- d2 4)))))

;;; bernoulli related distribution
(defdistribution bernoulli-related-distribution (discrete-distribution)
  ((probability :initarg :probability :accessor probability)))

(defmethod update-distribution ((distribution bernoulli-related-distribution))
  (let ((probability (probability distribution)))
    (assert (and (realp probability) (<= 0 probability 1)) (probability)
      "PROBABILITY should be a real number between 0 and 1.")))

;;; 11. Binomial Distribution

(defdistribution binomial-distribution (bernoulli-related-distribution)
  ((size :initarg :size :accessor size)
   (table)
   (ki)
   (vi)
   (b)
   (k)
   (w)
   (nsq)))

(defmethod print-object ((obj binomial-distribution) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream ": SIZE = ~d, PROBABILITY = ~a"
	    (size obj) (probability obj))))

(defmethod update-distribution ((distribution binomial-distribution))
  (call-next-method)
  (with-slots (size probability table ki vi b k w nsq) distribution
    (assert (and (real-integer-p size) (>= size 0)) (size)
      "SIZE should be a nonnegative integer.")
    (multiple-value-setq (table ki vi b k w nsq) (binomial-table-histogram size probability)))
  (with-slots (size probability mean variance skewness kurtosis mode) distribution
    (setf mean (* size probability))
    (if (> size 1)
	(setf variance (* mean (- 1d0 probability)))
      (slot-makunbound distribution 'variance))
    (if (> size 2)
	(setf skewness (/ (- 1d0 (* 2d0 probability))
			  (sqrt variance)))
      (slot-makunbound distribution 'skewness))
    (if (> size 3)
	(setf kurtosis (/ (+ 1d0 (* 3d0 (- size 2d0) probability (- 1d0 probability)))
			  variance))
      (slot-makunbound distribution 'kurtosis))
    (setf mode (floor (* probability (+ size 1)))))
  distribution)

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
  (with-slots (table ki vi b k w nsq) distribution
    (binomial-table-histogram-lookup table ki vi b k w nsq)))

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

(defdistribution geometric-distribution (bernoulli-related-distribution)
  ((table)
   (ki)
   (vi)
   (b)
   (k)
   (w)
   (nsq)
   (psq)
   (q)
   (r)
   (c)))

(defmethod print-object ((obj geometric-distribution) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream ": PROBABILITY = ~a" (probability obj))))

(defmethod update-distribution ((distribution geometric-distribution))
  (call-next-method)
  (with-slots (probability table ki vi b k w nsq psq q r c) distribution
    (multiple-value-setq (table ki vi b k w nsq psq q r c) (geometric-table-histogram probability)))
  (with-slots (probability mean variance skewness kurtosis mode) distribution
    (let ((fail (- 1d0 probability)))
      (setf mean (/ probability))
      (setf variance (* mean mean fail))
      (setf skewness (/ (- 2d0 probability) (sqrt fail)))
      (setf kurtosis (+ (/ (* probability probability) fail) 9d0))
      (setf mode 1)))
  distribution)

(defun geometric-distribution (probability)
  "Supported on k = 1, 2, ... \(the # of trials until a success, inclusive\)"
  (assert (and (realp probability) (< 0 probability) (<= probability 1))
	  (probability) "PROBABILITY should be a positive real number <= 1.")
  (make-instance 'geometric-distribution :probability probability))

(defmethod cdf ((distribution geometric-distribution) k)
  (- 1.0d0 (int-power (- 1.0d0 (probability distribution)) k)))

(defmethod mass ((distribution geometric-distribution) k)
  (let ((p (probability distribution)))
    (* (int-power (- 1.0d0 p) (1- k)) p)))

(defmethod quantile ((distribution geometric-distribution) p)
  (values (round (log (- 1.0 p) (- 1.0d0 (probability distribution))))))

(defmethod rand ((distribution geometric-distribution))
  (with-slots (table ki vi b k w nsq psq q r c) distribution
    (geometric-table-histogram-lookup table ki vi b k w nsq psq q r c)))

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

(defdistribution hypergeometric-distribution (discrete-distribution)
  ((elements :initarg :elements :accessor elements)
   (successes :initarg :successes :accessor successes)
   (samples :initarg :samples :accessor samples)
   (table)
   (ki)
   (vi)
   (b)
   (k)
   (w)
   (nsq)
   (a1)))

(defmethod print-object ((obj hypergeometric-distribution) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream ": N = ~a, m = ~a, n = ~a"
	    (elements obj) (successes obj) (samples obj))))

(defmethod update-distribution ((distribution hypergeometric-distribution))
  (with-slots (elements successes samples table ki vi b k w nsq a1) distribution
    (assert (and (real-integer-p elements) (>= elements 0)) (elements)
      "ELEMENTS should be a nonnegative integer.")
    (assert (and (real-integer-p successes) (>= successes 0)) (successes)
      "SUCCESSES should be a nonnegative integer.")
    (assert (and (real-integer-p samples) (>= samples 0)) (samples)
      "SAMPLES should be a nonnegative integer.")
    (multiple-value-setq (table ki vi b k w nsq a1) (hypergeometric-table-histogram elements successes samples)))
  (with-slots (elements successes samples mean variance skewness kurtosis mode) distribution
    (let ((enu (dfloat elements))
	  (m (dfloat successes))
	  (n (dfloat samples)))
      (setf mean (/ (* n m) enu))
      (setf variance (/ (* n m (- enu n) (- enu m))
			(* enu enu (- enu 1d0))))
      (setf skewness (* (/ (* (- enu (* 2 n)) (- enu (* 2 m)))
			   (- enu 2d0))
			(sqrt (/ (- enu 1d0)
				 (* n m (- enu n) (- enu m))))))
      (setf kurtosis (/ (* (- enu 1d0)
			   (+ (- (* enu enu enu (+ enu 1d0))
				 (* 6d0 enu enu n (- enu n))
				 (* 6d0 enu enu m (- enu m)))
			      (* 3d0 n m (- enu n) (- enu m) (- (* enu enu) (* 4d0 enu) -12d0))))
			(* n m (- enu n) (- enu m) (- enu 2d0) (- enu 3d0))))
      (setf mode (floor (/ (* (+ m 1) (+ n 1))
			   (+ enu 2))))))
  distribution)

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
  (with-slots (table ki vi b k w nsq a1) distribution
    (hypergeometric-table-histogram-lookup table ki vi b k w nsq a1)))

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

(defdistribution cauchy-distribution (continuous-distribution)
  ((location :initarg :location :accessor location)
   (scale :initarg :scale :accessor scale)))

(defmethod print-object ((obj cauchy-distribution) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream ": LOCATION = ~a, SCALE = ~a" (location obj) (scale obj))))

(defmethod update-distribution ((distribution cauchy-distribution))
  (with-slots (location scale mode) distribution
    (assert (realp location) (location) "LOCATION should be a real number.")
    (assert (and (realp scale) (> scale 0)) (scale)
      "SCALE should be a positive real number.")
    (setf mode location))
  distribution)

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
  (cauchy-random (location distribution) (scale distribution)))

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

(defdistribution logistic-distribution (continuous-distribution)
  ((location :initarg :location :accessor location)
   (scale :initarg :scale :accessor scale)
   (skewness :initform 0d0)
   (kurtosis :initform #.(dfloat (/ 21 5)))))

(defmethod print-object ((obj logistic-distribution) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream ": LOCATION = ~a, SCALE = ~a" (location obj) (scale obj))))

(defmethod update-distribution ((distribution logistic-distribution))
  (with-slots (location scale mean variance skewness kurtosis mode) distribution
    (assert (realp location) (location) "LOCATION should be a real number.")
    (assert (and (realp scale) (> scale 0)) (scale)
      "SCALE should be a positive real number.")
    (setf mean location)
    (setf variance (/ (sqr (* location scale)) 3d0))
    (setf mode location))
  distribution)
    
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
  (logistic-random (location distribution) (scale distribution)))

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

(defdistribution negative-binomial-distribution (bernoulli-related-distribution)
  ((success-r :initarg :success-r :accessor success-r)
   (table)
   (ki)
   (vi)
   (b)
   (k)
   (w)
   (nsq)
   (psq)
   (q)
   (r)
   (xl)
   (xu)
   (pl)
   (pu)
   (que)
   (s)
   (tee)))

(defmethod print-object ((obj negative-binomial-distribution) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream ": SUCCESS-R = ~a, PROBABILITY = ~a~"
	    (success-r obj) (probability obj))))

(defmethod update-distribution ((distribution negative-binomial-distribution))
  (call-next-method)
  (with-slots (probability success-r table ki vi b k w nsq psq q r xl xu pl pu que s tee) distribution
    (assert (and (realp success-r) (> success-r 0)) (success-r)
      "SUCCESS-R should be a positive real number.")
    (multiple-value-setq (table ki vi b k w nsq psq q r xl xu pl pu que s tee)
      (negative-binomial-table-histogram success-r probability)))
  (with-slots (probability success-r mean variance skewness kurtosis mode) distribution
    (let ((fail (- 1d0 probability)))
      (setf mean (/ (* fail success-r) probability))
      (setf variance (/ mean probability))
      (setf skewness (/ (- 2d0 probability) (sqrt (* fail success-r))))
      (setf kurtosis (+ 3d0 (/ 6d0 success-r) (/ variance)))
      (setf mode (if (> success-r 1d0)
		     (floor (/ (* (1- success-r) fail) probability))
		   0d0))))
  distribution)

(defun negative-binomial-distribution (successes probability)
  "Number of failures until a given number of successes,
extended to real numbers.
If FAILURESP is NIL, we look at the number of all trials, not just the
failures."
  (assert (and (realp successes) (> successes 0)) (successes)
	  "SUCCESSES should be a positive real number.")
  (assert (and (realp probability) (< 0 probability 1)) (probability)
	  "PROBABILITY should be a real number between 0 and 1.")
  (make-instance 'negative-binomial-distribution
		 :success-r successes :probability probability))

(defmethod cdf ((distribution negative-binomial-distribution) k)
  (if (< k 0)
      0
    (regularized-incomplete-beta (success-r distribution)
				 (+ k 1.0d0)
				 (probability distribution))))

(defmethod mass ((distribution negative-binomial-distribution) k)
  (let ((s (success-r distribution))
	(p (probability distribution)))
    (if (< k 0)
	0
      (* (/ (gamma (+ s k)) (* (gamma (+ k 1.0d0)) (gamma s)))
	 (expt p s) (int-power (- 1.0d0 p) k)))))

;;; Compute a guess by the 2nd-order Cornish-Fisher expansion.
;;; See the notes at the binomial distribution quantile.
(defmethod quantile ((distribution negative-binomial-distribution) p)
  (let* ((prob (probability distribution))
	 (q (- 1.0d0 prob))
	 (s (success-r distribution))
	 (mean (/ (* s q) prob))
	 (sigma (sqrt (/ (* s q) (sqr prob))))
	 (z (quantile (standard-normal-distribution) p))
	 (k3 (/ (* s q (1+ q)) (expt (* sigma prob) 3)))
	 (guess (round (+ mean
			  (* sigma (+ z (* k3 (/ (1- (* z z)) 6.0d0))))))))
    (search-for-quantile distribution p guess :min s)))

#+ignore
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
#+ignore
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
  (with-slots (table ki vi b k w nsq psq q r xl xu pl pu que s tee) distribution
    (negative-binomial-table-histogram-lookup table ki vi b k w nsq psq q r xl xu pl pu que s tee)))

(defmethod mean ((distribution negative-binomial-distribution))
  (let ((p (probability distribution)))
    (/ (* (success-r distribution) (- 1.0d0 p)) p)))

(defmethod variance ((distribution negative-binomial-distribution))
  (let ((p (probability distribution)))
    (/ (* (success-r distribution) (- 1.0d0 p)) p p)))

(defun negative-binomial-distribution-estimate-maximum-likelihood (successes trials)
    "Estimate based on the number of successes in a given number of trials.
FAILURESP works as in NEGATIVE-BINOMIAL-DISTRIBUTION."
  (negative-binomial-distribution successes (/ successes trials)))

(defun negative-binomial-distribution-estimate-unbiased (successes trials)
    "Estimate based on the number of successes in a given number of trials.
FAILURESP works as in NEGATIVE-BINOMIAL-DISTRIBUTION."
  (negative-binomial-distribution
   successes (/ (1- successes) (1- trials))))

;;; 18. Poisson Distribution

(defdistribution poisson-distribution (discrete-distribution)
  ((rate :initarg :rate :accessor rate)
   (table)
   (ki)
   (vi)
   (b)
   (k)
   (w)
   (nsq)
   (psq)
   (q)
   (r)
   (xl)
   (xu)
   (pl)
   (pu)
   (c)
   ))

(defmethod print-object ((obj poisson-distribution) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream ": RATE = ~a" (rate obj))))

(defmethod update-distribution ((distribution poisson-distribution))
  (with-slots (rate table ki vi b k w nsq psq q r xl xu pl pu c) distribution
    (assert (and (realp rate) (> rate 0)) (rate)
      "RATE should be a positive real number.")
    (multiple-value-setq (table ki vi b k w nsq psq q r xl xu pl pu c rate)
      (poisson-table-histogram rate)))
  (with-slots (rate mean variance skewness kurtosis mode) distribution
    (setf mean rate)
    (setf variance rate)
    (setf skewness (/ (sqrt rate)))
    (setf kurtosis (+ (/ rate) 3d0))
    (setf mode (floor rate))) ;; if rate is integer, (- rate 1) is also mode -- in case of def of mode, take average of 2
  distribution)

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
  (with-slots (table ki vi b k w nsq psq q r xl xu pl pu c rate) distribution
    (poisson-table-histogram-lookup table ki vi b k w nsq psq q r xl xu pl pu c rate)))

(defmethod mean ((distribution poisson-distribution))
  (rate distribution))

(defmethod variance ((distribution poisson-distribution))
  (rate distribution))

(defun poisson-distribution-estimate (sequence)
  "Maximum likelihood estimate, also unbiased and minimum variance."
  (poisson-distribution (mean sequence)))

;;; 19. Weibull Distribution

;;; todo -- weibull dist is not gamma distribution; bad definition
(defdistribution weibull-distribution (gamma-like-distribution)
  ((include-zero :initarg :include-zero :initform nil :accessor include-zero)
   (r-inv)))

(defmethod print-object ((obj weibull-distribution) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream ": SCALE = ~a, SHAPE = ~a" (scale obj) (shape obj))))

(defmethod update-distribution ((distribution weibull-distribution))
  (with-slots (shape scale r-inv) distribution
    (assert (and (realp scale) (> scale 0)) (scale)
      "SCALE should be a positive real number.")
    (assert (and (realp shape) (> shape 0)) (shape)
      "SHAPE should be a positive real number.")
    (setf r-inv (/ shape)))
  (with-slots (shape scale mean variance skewness kurtosis mode) distribution
    (setf mean (* scale (gamma (+ (/ shape) 1d0))))
    (setf variance (* scale scale
		      (- (gamma (+ (/ 2d0 shape) 1d0))
			 (sqr (gamma (+ (/ shape) 1d0))))))
    (setf skewness (/ (+ (gamma (+ (/ 3d0 shape) 1d0))
			 (- (* 3d0 (gamma (+ (/ 2d0 shape) 1d0))
			       (gamma (+ (/ shape) 1d0))))
			 (* 2d0 (int-power (gamma (+ (/ shape) 1d0)) 3)))
		      (half-integer-power (- (gamma (+ (/ 2d0 shape) 1d0))
					     (sqr (gamma (+ (/ shape) 1d0))))
					  3/2)))
    (setf kurtosis (/ (+ (gamma (+ (/ 4d0 shape) 1d0))
			 (- (* 4d0 (gamma (+ (/ 3d0 shape) 1d0))
			       (gamma (+ (/ shape) 1d0))))
			 (* 6d0 (gamma (+ (/ 2d0 shape) 1d0))
			    (sqr (gamma (+ (/ shape) 1d0))))
			 (- (* 3d0 (int-power (gamma (+ (/ shape) 1d0)) 4))))
		      (sqr (- (gamma (+ (/ 2d0 shape) 1d0))
			      (sqr (gamma (+ (/ shape) 1d0))))))))
  distribution)

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
  (with-slots (scale r-inv include-zero) distribution
    (* scale
       (the double-float (expt (the double-float (exp-random 1d0 include-zero)) r-inv)))))

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

;;; debug utility

(defun unit-test-distribution (dist &optional (sample 10000))
  (let ((rands (rand-n dist sample)))
    (ignore-errors (format t "MEAN:      sample=~A   predict=~A~%" (mean rands) (mean dist)))
    (ignore-errors (format t "VAR:       sample=~A   predict=~A~%" (variance rands) (variance dist)))
    (ignore-errors (format t "MODE:      sample=~A   predict=~A~%" (mode rands) (mode dist)))
    (ignore-errors (format t "SKEWNESS:  sample=~A   predict=~A~%" (skewness  rands) (skewness dist)))
    (ignore-errors (format t "KURTOSIS:  sample=~A   predict=~A~%" (kurtosis  rands) (kurtosis dist)))))
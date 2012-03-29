;;; -*- mode: lisp; syntax: common-lisp -*-

;;; Peter Salvi 2008

;;; (mainly translated from various sources)

(in-package :statistics)

;;; Contents:
;;; ---------
;;; chebyshev, chebyshev-terms
;;; log-gamma, log-gamma-correction
;;; stirling-error
;;; gamma, gamma-half, digamma
;;; erf, erf-inverse
;;; *max-series-iterations*, sum-series
;;; lower-incomplete-gamma, lower-incomplete-gamma-half, regularized-gamma
;;; generalized-continued-fraction simple-continued-fraction
;;; beta, beta-half, incomplete-beta, regularized-incomplete-beta
;;;   incomplete-beta-inverse
;;; *newton-raphson-precision*, *newton-raphson-initial-divisions*,
;;;   newton-raphson
;;; numerical-derivative

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +ln-sqrt-2pi+ (log (sqrt (* 2.0d0 pi))))
  (defconstant +euler-gamma+ 0.5772156649015328606065120900824024d0)
  (defconstant +least-negative-exponent+ (log least-positive-double-float))
  (defconstant +most-positive-exponent+ (log most-positive-double-float)))

(defun chebyshev-terms (coefficients tolerance)
  "The maximum number of Chebyshev terms that the error is within tolerance."
  (let ((n (length coefficients)))
    (if (zerop n)
	0
	(loop for i from (1- n) downto 0
	   for err = 0.0d0 then (+ err (abs (elt coefficients i)))
	   while (<= err tolerance)
	   finally (return i)))))

(defun chebyshev (x coefficients n)
  "N-term shifted Chebyshev series at X."
  (assert (< 1 n 1000) (n)
	  "Invalid N = ~a for Chebyshev series evaluation." n)
  (assert (< -1.1d0 x 1.1d0) (x)
	  "Invalid X = ~a for Chebyshev series evaluation." x)
  (loop with twox = (* 2 x)
     for i from 1 to n
     for b2 = 0 then b1
     for b1 = 0 then b0
     for b0 = 0 then (+ (* twox b1) (- b2) (elt coefficients (- n i)))
     finally (return (* (- b0 b2) 0.5d0))))

(let ((log-gamma-chebyshev #(+.1666389480451863247205729650822d+0
			     -.1384948176067563840732986059135d-4
			     +.9810825646924729426157171547487d-8
			     -.1809129475572494194263306266719d-10
			     +.6221098041892605227126015543416d-13
			     -.3399615005417721944303330599666d-15
			     +.2683181998482698748957538846666d-17
			     -.2868042435334643284144622399999d-19
			     +.3962837061046434803679306666666d-21
			     -.6831888753985766870111999999999d-23
			     +.1429227355942498147573333333333d-24
			     -.3547598158101070547199999999999d-26
			     +.1025680058010470912000000000000d-27
			     -.3401102254316748799999999999999d-29
			     +.1276642195630062933333333333333d-30))
      (xbig (expt 2 26.5d0))
      (xmax (/ most-positive-double-float 48.0d0)))
  (defun log-gamma-correction (x)
    (assert (<= 10 x xmax) (x)
	    "X = ~a is not in the domain of the log-gamma correction function."
	    x)
    (if (< x xbig)
	(/ (chebyshev (1- (* (sqr (/ 10 x)) 2)) log-gamma-chebyshev 5) x)
	(/ (* x 12)))))

(let ((xmax (/ most-positive-double-float (log most-positive-double-float)))
      (near-zero (sqrt double-float-epsilon)))
  (defun log-gamma (x)
    (assert (and (<= (abs x) xmax) (not (and (<= x 0) (integerp x)))) (x)
	    "X = ~a is not in the domain of the log-gamma function." x)
    (let ((y (abs x)))
      (cond ((<= y 10) (log (abs (gamma x))))
	    ((< x 0)
	     (let* ((sin-pi-y (abs (sin (* pi y))))
		    (value (- (log (sqrt (/ pi 2.0d0))) (* (- 0.5d0 x) (log y))
			      x (log sin-pi-y) (log-gamma-correction y))))
	       (when (< (abs (/ (* (- x (truncate (- x 0.5d0))) value) x))
			near-zero)
		 (warn "Precision loss because the argument to the log-gamma ~
                        function is too close to a negative integer."))
	       value))
	    ((> x 1.0d17) (* x (- (log x) 1.0d0)))
	    (t (+ +ln-sqrt-2pi+ (* (- x 0.5d0) (log x)) (- x)
		  (if (> x 4934720.0d0) 0.0d0 (log-gamma-correction x))))))))

(let ((halves #(0.0d0
		0.1534264097200273452913848d0
		0.0810614667953272582196702d0
		0.0548141210519176538961390d0
		0.0413406959554092940938221d0
		0.03316287351993628748511048d0
		0.02767792568499833914878929d0
		0.02374616365629749597132920d0
		0.02079067210376509311152277d0
		0.01848845053267318523077934d0
		0.01664469118982119216319487d0
		0.01513497322191737887351255d0
		0.01387612882307074799874573d0
		0.01281046524292022692424986d0
		0.01189670994589177009505572d0
		0.01110455975820691732662991d0
		0.010411265261972096497478567d0
		0.009799416126158803298389475d0
		0.009255462182712732917728637d0
		0.008768700134139385462952823d0
		0.008330563433362871256469318d0
		0.007934114564314020547248100d0
		0.007573675487951840794972024d0
		0.007244554301320383179543912d0
		0.006942840107209529865664152d0
		0.006665247032707682442354394d0
		0.006408994188004207068439631d0
		0.006171712263039457647532867d0
		0.005951370112758847735624416d0
		0.005746216513010115682023589d0
		0.005554733551962801371038690d0))
      (s0 1/12)
      (s1 1/360)
      (s2 1/1260)
      (s3 1/1680)
      (s4 1/1188))
  (defun stirling-error (n)
    (if (<= n 15)
	(let ((nn (+ n n)))
	  (if (integerp nn)
	      (elt halves nn)
	      (- (log-gamma (1+ n)) (* (+ n 0.5d0) (log n)) (- n)
		 +ln-sqrt-2pi+)))
	(let ((nn (* n n)))
	  (cond ((> n 500) (/ (- s0 (/ s1 nn)) n))
		((> n 80) (/ (- s0 (/ (- s1 (/ s2 nn)) nn)) n))
		((> n 35) (/ (- s0 (/ (- s1 (/ (- s2 (/ s3 nn)) nn)) nn)) n))
		(t (/ (- s0 (/ (- s1 (/ (- s2 (/ (- s3 (/ s4 nn)) nn)) nn)) nn)) n)))))))

(defparameter *gamma-limits*
  (let ((log-min (- (log least-positive-normalized-double-float)))
	(log-max (log most-positive-double-float)))
    (labels ((next-min (x ln)
	       (- x (* x (/ (- (* (+ x .5d0) ln) x .2258d0 log-min)
			    (+ (* x ln) .5d0)))))
	     (next-max (x ln)
	       (- x (* x (/ (- (* (- x .5d0) ln) x -.9189d0 log-max)
			    (- (* x ln) .5d0)))))
	     (limits (start next)
	       (loop repeat 10
		  for xold = start then x
		  for xln = (log xold)
		  for x = (funcall next start xln) then (funcall next x xln)
		  when (< (abs (- x xold)) .005d0) do (return (- .01d0 x))
		  finally (error "Unable to find the gamma function limits."))))
      (let ((xmin (limits log-min #'next-min))
	    (xmax (limits log-max #'next-max)))	; negated
	(list (max xmin (1+ xmax)) (- xmax)))))
  "For IEEE_754 this should be \(-170.56749727266123d0 171.61447887182297d0).")

(let* ((gamma-chebyshev #(+.8571195590989331421920062399942d-2
			  +.4415381324841006757191315771652d-2
			  +.5685043681599363378632664588789d-1
			  -.4219835396418560501012500186624d-2
			  +.1326808181212460220584006796352d-2
			  -.1893024529798880432523947023886d-3
			  +.3606925327441245256578082217225d-4
			  -.6056761904460864218485548290365d-5
			  +.1055829546302283344731823509093d-5
			  -.1811967365542384048291855891166d-6
			  +.3117724964715322277790254593169d-7
			  -.5354219639019687140874081024347d-8
			  +.9193275519859588946887786825940d-9
			  -.1577941280288339761767423273953d-9
			  +.2707980622934954543266540433089d-10
			  -.4646818653825730144081661058933d-11
			  +.7973350192007419656460767175359d-12
			  -.1368078209830916025799499172309d-12
			  +.2347319486563800657233471771688d-13
			  -.4027432614949066932766570534699d-14
			  +.6910051747372100912138336975257d-15
			  -.1185584500221992907052387126192d-15
			  +.2034148542496373955201026051932d-16
			  -.3490054341717405849274012949108d-17
			  +.5987993856485305567135051066026d-18
			  -.1027378057872228074490069778431d-18
			  +.1762702816060529824942759660748d-19
			  -.3024320653735306260958772112042d-20
			  +.5188914660218397839717833550506d-21
			  -.8902770842456576692449251601066d-22
			  +.1527474068493342602274596891306d-22
			  -.2620731256187362900257328332799d-23
			  +.4496464047830538670331046570666d-24
			  -.7714712731336877911703901525333d-25
			  +.1323635453126044036486572714666d-25
			  -.2270999412942928816702313813333d-26
			  +.3896418998003991449320816639999d-27
			  -.6685198115125953327792127999999d-28
			  +.1146998663140024384347613866666d-28
			  -.1967938586345134677295103999999d-29
			  +.3376448816585338090334890666666d-30
			  -.5793070335782135784625493333333d-31))
       (terms (chebyshev-terms gamma-chebyshev (/ double-float-epsilon 20.0d0)))
       (near-zero (exp (+ (max (log least-positive-normalized-double-float)
			       (- (log most-positive-double-float)))
			  0.01d0))))
  (defun gamma (x)
    (assert (not (or (= x 0) (and (< x 0) (integerp x))
		     (< x (first *gamma-limits*))
		     (> x (second *gamma-limits*))))
	    (x) "X = ~a is not in the domain of the gamma function." x)
    (if (<= (abs x) 10)
	(let* ((n (- (truncate x) (if (< x 0) 2 1)))
	       (y (- x (1+ n)))
	       (value (+ (chebyshev (1- (* 2 y)) gamma-chebyshev terms)
			 .9375d0)))
	  (cond ((< n 0)
		 (when (and (< x -0.5d0)
			    (< (abs (/ (- x (truncate (- x 0.5d0))) x))
			       (sqrt double-float-epsilon)))
		   (warn "Precision loss because the argument to the gamma ~
                          function is too close to a negative integer."))
		 (assert (>= y near-zero) ()
			 "Near zero argument to the gamma function causes ~
                          ~:[overflow~;underflow~]." (< x 0))
		 (loop for i from 0 below (- n)
		    do (setf value (/ value (+ x i)))))
		((> n 0)
		 (loop for i from 1 to n
		    do (setf value (* value (+ y i))))))
	  value)
	(let* ((y (abs x))
	       (value (if (and (< y 50) (integerp y))
			  (reduce #'* (loop for i from 2 below y collect i))
			  (exp (+ (* (- y 0.5d0) (log y)) (- y) +ln-sqrt-2pi+
				  (if (integerp (* 2 y))
				      (stirling-error y)
				      (log-gamma-correction y)))))))
	  (if (> x 0)
	      value
	      (let ((sin-pi-y (sin (* pi y))))
		(when (< (abs (/ (- x (truncate (- x 0.5d0))) x))
			 (sqrt double-float-epsilon))
		  (warn "Precision loss because the argument to the gamma ~
                         function is too close to a negative integer."))
		(assert (/= sin-pi-y 0) ()
			"Large negative integer argument to the gamma ~
                         function causes overflow.")
		(/ (- pi) (* y sin-pi-y value))))))))

(defun gamma-half (n)
  "The gamma function for N/2 (where N is an integer)."
  (gamma (/ n 2)))

;;; as in the implementation of R. J. Mathar:
;;; http://www.strw.leidenuniv.nl/~mathar/progs/digamma.c
(let ((k '(.30459198558715155634315638246624251d0
	   .72037977439182833573548891941219706d0
	   -.12454959243861367729528855995001087d0
	   .27769457331927827002810119567456810d-1
	   -.67762371439822456447373550186163070d-2
	   .17238755142247705209823876688592170d-2
	   -.44817699064252933515310345718960928d-3
	   .11793660000155572716272710617753373d-3
	   -.31253894280980134452125172274246963d-4
	   .83173997012173283398932708991137488d-5
	   -.22191427643780045431149221890172210d-5
	   .59302266729329346291029599913617915d-6
	   -.15863051191470655433559920279603632d-6
	   .42459203983193603241777510648681429d-7
	   -.11369129616951114238848106591780146d-7
	   .304502217295931698401459168423403510d-8
	   -.81568455080753152802915013641723686d-9
	   .21852324749975455125936715817306383d-9
	   -.58546491441689515680751900276454407d-10
	   .15686348450871204869813586459513648d-10
	   -.42029496273143231373796179302482033d-11
	   .11261435719264907097227520956710754d-11
	   -.30174353636860279765375177200637590d-12
	   .80850955256389526647406571868193768d-13
	   -.21663779809421233144009565199997351d-13
	   .58047634271339391495076374966835526d-14
	   -.15553767189204733561108869588173845d-14
	   .41676108598040807753707828039353330d-15
	   -.11167065064221317094734023242188463d-15)))
  (defun digamma (x)
    "TODO: This is a kind of Chebyshev series, should be implemented with
CHEBYSHEV and CHEBYSHEV-TERMS."
    (cond ((< x 0.0d0) (- (digamma (1+ x)) (/ x)))
	  ((= x 1.0d0) (- +euler-gamma+))
	  ((= x 2.0d0) (- 1.0d0 +euler-gamma+))
	  ((= x 3.0d0) (- 1.5d0 +euler-gamma+))
	  ((> x 3.0d0) (+ (log 2.0d0)
			  (* 0.5d0
			     (+ (digamma (/ x 2.0d0))
				(digamma (/ (1+ x) 2.0d0))))))
	  (t (let ((y (- x 2.0d0)))
	       (+ (loop
		     for n from 1 below (length k)
		     for tmp = (- (* 2.0d0 y y) 1.0d0)
		     then (- (* 2.0d0 y tn) tn-prev)
		     for tn-prev = 1.0d0 then tn
		     for tn = y then tmp
		     sum (* (elt k n) tn))
		  (elt k 0)))))))


;;; ------------
;;; Erf Function
;;; ------------

;;; Translated from ACM TOMS 708 [ERF], http://www.netlib.org/toms/708
(let ((c .564189583547756d0)
      (a '(7.7105849500132d-5 -.00133733772997339d0 .0323076579225834d0
	   .0479137145607681d0 .128379167095513d0))
      (b '(.00301048631703895d0 .0538971687740286d0 .375795757275549d0))
      (p '(-1.36864857382717d-7 .564195517478974d0
	   7.21175825088309d0 43.1622272220567d0 152.98928504694d0
	   339.320816734344d0 451.918953711873d0 300.459261020162d0))
      (q '(1.0d0 12.7827273196294d0 77.0001529352295d0 277.585444743988d0
	   638.980264465631d0 931.35409485061d0 790.950925327898d0
	   300.459260956983d0))
      (r '(2.10144126479064d0 26.2370141675169d0 21.3688200555087d0
	   4.6580782871847d0 .282094791773523d0))
      (s '(94.153775055546d0 187.11481179959d0 99.0191814623914d0
	   18.0124575948747d0)))
  (defun erf (x)
    (let ((ax (abs x)))
      (cond ((<= ax 0.5d0)
	     (let ((x2 (* x x)))
	       (* x (/ (1+ (polynomial a x2))
		       (1+ (* x2 (polynomial b x2)))))))
	    ((<= ax 4.0d0)
	     (* (- 1.0d0
		   (* (exp (* (- x) x))
		      (/ (polynomial p ax)
			 (polynomial q ax))))
		(if (< x 0) -1.0d0 1.0d0)))
	    ((>= ax 5.8d0)
	     (if (< x 0) -1 1))
	    (t (let* ((x2 (* x x))
		      (1/x2 (/ x2)))
		 (* (- 1.0d0
		       (* (exp (* (- x) x))
			  (/ (- c (/ (polynomial r 1/x2)
				     (* x2 (1+ (* 1/x2 (polynomial s 1/x2))))))
			     ax)))
		    (if (< x 0) -1.0d0 1.0d0))))))))

;;; Translated from ACM TOMS 602 [MERFI], http://www.netlib.org/toms/602
(let ((a '(-.5751703d0 -1.896513d0 -.5496261d-1))
      (b '(-.1137730d0 -3.293474d0 -2.374996d0 -1.187515d0))
      (c '(-.1146666d0 .5073975d-1 -.2368201d0 -.1314774d0))
      (d '(1.0d0 -7.586103d0 21.98546d0 -44.27977d0))
      (e '(-.5668422d-1 .6208963d-1 -.3166501d0 .3937021d0))
      (f '(-2.962883d0 4.666263d0 -6.266786d0))
      (g '(.1851159d-3 .1078639d-1 -.1498384d0 -.2028152d-2))
      (h '(1.0d0 -.6888301d-1 .5211733d0 .9952975d-1)))
  (defun erf-inverse (x)
    (assert (< -1 x 1) (x) "X = ~a is not in the range \(-1, 1)" x)
    (let ((z (abs x)))
      (* (if (< z 0.85d0)
	     (let ((z2 (* z z)))
	       (+ z (* z (+ (first b)
			    (/ (* (first a) z2)
			       (+ (second b) z2
				  (/ (second a)
				     (+ (third b) z2
					(/ (third a)
					   (+ (fourth b) z2))))))))))
	     (let* ((1-z (- 1.0d0 z))
		    (w (sqrt (- (log (+ 1-z (* 1-z z)))))))
	       (cond ((< w 2.5d0)
		      (+ w (* w (+ (first c)
				   (/ (* w (polynomial (rest c) w))
				      (polynomial d w))))))
		     ((< w 4.0d0)
		      (+ w (* w (+ (first e)
				   (/ (* w (polynomial (rest e) w))
				      (polynomial f w))))))
		     (t (let ((1/w (/ w)))
			  (+ w (* w (+ (first g)
				       (/ (* 1/w (polynomial (rest g) 1/w))
					  (polynomial h 1/w))))))))))
	 (if (< x 0) -1.0d0 1.0d0)))))


;;; ---------------------------
;;; Gamma function, as in Boost
;;; ---------------------------

(defparameter *max-series-iterations* 100)
(defun sum-series (fn bits)
  "FN generates the series, the error will be less than sum/2^BITS."
  (loop with factor = (expt 2 bits)
     for next = (funcall fn)
     sum next into result
     while (< (abs result) (abs (* factor next)))
     finally (return result)))

(defun %lower-gamma-series (a x)
  (sum-series (let ((a0 a)
		    (result 1.0d0))
		(lambda ()
		  (incf a0)
		  (prog1 result (setf result (* result (/ x a0))))))
	      *max-series-iterations*))

(defun %upper-gamma-fraction (a x &optional (bits 32))
  (/ (generalized-continued-fraction (lambda (k) (* k (- a k)))
				     (lambda (k) (+ x (- a) 1 (* 2 k)))
				     :pred (lambda (new old)
					     (> (abs (- new old))
						(expt 2.0d0 (- bits)))))))

(defun lower-incomplete-gamma (a x)
  (* (%lower-gamma-series a x)
     (/ (* (expt x a) (exp (- x))) a)))

(defun lower-incomplete-gamma-half (n x)
  "The lower incomplete gamma function for N/2 (where N is an integer)."
  (lower-incomplete-gamma (/ n 2) x))

(defun regularized-gamma (a x)
  (if (integerp a)
      (* (exp (- x))
	 (loop for i from 0 below a
	    sum (/ (expt x i) (gamma (1+ i)))))
      (/ (lower-incomplete-gamma a x) (gamma a))))


;;; --------------------------
;;; Beta function, as in Boost
;;; --------------------------

(defun beta (a b)
  (assert (and (> a 0) (> b 0)) (a b) "A and B should both be positive.")
  (cond ((< b double-float-epsilon) (gamma b))
	((< a double-float-epsilon) (gamma a))
	((= b 1) (/ a))
	((= a 1) (/ b))
	((< a 1) (* (/ (+ a b) a) (beta (1+ a) b)))
	((< b 1) (* (/ (+ a b) b) (beta a (1+ b))))
	((< a b) (beta b a))
	(t (flet ((lanczos-series (d limit)
		    (+ (/ (%lower-gamma-series d limit) d)
		       (%upper-gamma-fraction d limit))))
	     (let* ((la (max 10 a))
		    (lb (max 10 b))
		    (lc (max 10 (+ a b)))
		    (sa (lanczos-series a la))
		    (sb (lanczos-series b lb))
		    (sc (lanczos-series (+ a b) lc))
		    (result (* (exp (- lc la lb))
			       (expt (/ la lc) a) (expt (/ lb lc) b)
			       (/ (* sa sb) sc))))
	       (if (= result 0)
		   least-positive-normalized-double-float
		   result))))))

(defun beta-half (a b)
  "Beta function for A/2, B/2."
  (beta (/ a 2) (/ b 2)))

;;; As in Boost (but only the simplest case, for details see ACM TOMS 708):
;;; Maybe this should be (* (regularized-incomplete-beta a b x) (beta a b)
(defun incomplete-beta (a b x)
  (sum-series (let ((result (expt x a))
		    (apn a)
		    (n 1.0d0)
		    (poch (- 1.0d0 b)))
		(lambda ()
		  (prog1 (/ result apn)
		    (incf apn)
		    (setf result (/ (* result poch x) n))
		    (incf n)
		    (incf poch))))
	      *max-series-iterations*))


;;; --------------------------------------------------------------------
;;; Regularized incomplete beta function, as in ACM TOMS 708 / R / Boost
;;; --------------------------------------------------------------------

;;; Some of these algorithms were simplified. Please refer to the original
;;; sources in case of bugs.

(defun generalized-continued-fraction (a b &key
				       (a1 (funcall a 1)) (b0 (funcall b 0))
				       (tolerance double-float-epsilon)
				       (pred (lambda (new old)
					       (> (abs (- new old))
						  (* tolerance new)))))
  "Calculates the infinite continued fraction with coefficients given by the
functions A and B, with relative tolerance TOLERANCE. Initial values \(for 
the n=0 case) can b given as keys.
If PRED is given, it should be a function of two arguments, the new and old
value in the iteration, that returns NIL when the iterations should stop."
  (loop for n upfrom 1
     for an = a1 then (funcall a n)
     for bn = (funcall b n)
     for alpha-prev2 = 0 then alpha-prev
     for alpha-prev = b0 then alpha-n
     for alpha-n = (+ a1 (* b0 bn)) then
       (+ (* bn alpha-prev) (* an alpha-prev2))
     for beta-prev2 = 0 then beta-prev
     for beta-prev = 1 then beta-n
     for beta-n = bn then
       (+ (* bn beta-prev) (* an beta-prev2))
     for prev-fraction = 0 then fraction
     for fraction = (/ alpha-n beta-n)
     while (funcall pred fraction prev-fraction)
     finally (return fraction)))

(defun simple-continued-fraction (a &key (a0 (funcall a 0))
				  (tolerance double-float-epsilon)
				  (pred (lambda (new old)
					  (> (abs (- new old))
					     (* tolerance new)))))
  (generalized-continued-fraction (lambda (n) (declare (ignore n)) 1) a
				  :b0 a0 :tolerance tolerance
				  :pred pred))

;;; Usage example:
;;; Pi:
;; (+ (generalized-continued-fraction (lambda (n) (sqr (1- n)))
;; 				   (lambda (n) (1- (* 2 n)))
;; 				   :a1 4 :b0 0)
;;    0.0d0)
;;; Golden ratio:
;; (+ (simple-continued-fraction (lambda (n) (declare (ignore n)) 1)) 0.0d0)

(defun %reg-inc-beta-fpser (a b x)
  "When A is very small and X <= 1/2.
Series: x^a * b / a * \(1 + a * sum_{i=1} x^i / \(a + i)."
  (loop with tolerance = (/ double-float-epsilon a)
     for xi = x then (* x xi)
     for ai upfrom (1+ a)
     for change = (/ xi ai)
     sum change into s
     while (> (abs change) tolerance)
     finally (return (* (expt x a) (/ b a) (1+ (* a s))))))

(defun %reg-inc-beta-apser (a b x)
  "When A is very small, B*X <= 1 and X <= 1/2.
Series \(large B):
-a * \(log\(b*x) + gamma + sum_{i=1} \(prod_{j=1}^i x - bx/j) / i).
Series \(not very large B):
-a * \(log\(x) + digamma\(b) + gamma + sum_{i=1} \(prod_{j=1}^i x - bx/j) / i)."
  (let* ((bx (* b x))
	 (tt (- x bx))
	 (c (if (< (* b double-float-epsilon) 0.02d0)
		(+ (log x) (digamma b) +euler-gamma+)
		(+ (log bx) +euler-gamma+)))
	 (tolerance (* double-float-epsilon 5.0d0 (abs (+ c tt)))))
    (loop
       for tt = tt then (* tt (/ (- x bx) j))
       for j upfrom 1
       for change = (/ tt j)
       sum change into s
       while (> (abs change) tolerance)
       finally (return (* (- a) (+ c s))))))

(defun %reg-inc-beta-bpser (a b x)
  "When B <= 1 or B*X <= 0.7.
Series:
x^a / \(a * beta\(a, b)) * \(1 + a * sum_{i=1} \(prod_{j=1}^i x - bx/j) / i))."
  (loop with tolerance = (/ double-float-epsilon a) and bx = (* b x)
     for n upfrom 1
     for c = (- x bx) then (* c (- x (/ bx n)))
     for change = (/ c (+ a n))
     sum change into s
     while (> (abs change) tolerance)
     finally (return (* (/ (expt x a) (* a (beta a b))) (1+ (* a s))))))

(defun %reg-inc-beta-gam1 (x)
  "For X in [-0.5, 1.5]."
  (1- (/ (gamma (1+ x)))))
(defun %reg-inc-beta-gamln1 (x)
  "For X in [-0.2, 1.25]."
  (log-gamma (1+ x)))
(defun %reg-inc-beta-algdiv (a b)
  "For B >= 8."
  (- (log-gamma b) (log-gamma (+ a b))))
(declaim (inline %reg-inc-beta-gam1 %reg-inc-beta-gamln1 %reg-inc-beta-algdiv))

(defun %reg-inc-beta-brcmp1 (mu a b x y)
  "Computes (/ (* (exp mu) (expt x a) (expt y b)) (beta a b))."
  (cond ((> a b) (%reg-inc-beta-brcmp1 mu b a y x))
	((> a 8)
	 (flet ((rlog1m (e c)
		  (if (> (abs e) 0.6d0)
		      (- e (log c))
		      (- e (log (1+ e))))))
	   (let ((l (- a (* (+ a b) x))))
	     (* (/ #.(sqrt (* 2.0d0 pi)))
		(sqrt (/ (* a b) (+ a b)))
		(exp (- mu
			(* a (rlog1m (/ (- l) a) (* x (/ (+ a b) a))))
			(* b (rlog1m (/ l b) (* y (/ (+ a b) b))))))
		(exp (- (%reg-inc-beta-bcorr a b)))))))
	((< a 1)
	 (cond ((>= b 8)
		(* a (exp (- (+ mu (* a (log x)) (* b (log y)))
			     (%reg-inc-beta-gamln1 a)
			     (%reg-inc-beta-algdiv a b)))))
	       ((> b 1)
		(let ((z (- (+ (* a (log x)) (* b (log y)))
			    (loop for i from b downto 1
			       for c = 1 then (* c (/ i (+ a i)))
			       finally (return (+ (%reg-inc-beta-gamln1 a)
						  (log c))))))
		      (b (- b (floor b))))
		  (/ (* a (exp (+ mu z)) (1+ (%reg-inc-beta-gam1 b)))
		     (if (> (+ a b) 1)
			 (/ (1+ (%reg-inc-beta-gam1 (+ a b -1))) (+ a b))
			 (1+ (%reg-inc-beta-gam1 (+ a b)))))))
	       (t (let ((result (exp (+ mu (* a (log x)) (* b (log y))))))
		    (if (= result 0.0d0)
			result
			(/ (* result a (1+ (%reg-inc-beta-gam1 a))
			      (1+ (%reg-inc-beta-gam1 b)))
			   (* (1+ (/ a b))
			      (if (> (+ a b) 1)
				  (/ (1+ (%reg-inc-beta-gam1 (+ a b -1)))
				     (+ a b))
				  (1+ (%reg-inc-beta-gam1 (+ a b)))))))))))
	(t (exp (+ mu (* a (log x)) (* b (log y)) (- (log (beta a b))))))))

(defun %reg-inc-beta-bup (a b x y n)
  "I_X\(A, B) - I_X\(A+N, B)."
  (let* ((mu (if (or (= n 1) (< a 1.0d0) (< (+ a b) (* (1+ a) 1.1d0)))
		 0
		 #.(* (min (- +least-negative-exponent+)
			   +most-positive-exponent+)
		      0.69314718055995d0)))
	 (d (exp (- mu)))
	 (brc (/ (%reg-inc-beta-brcmp1 mu a b x y) a)))
    (if (or (= n 1) (= brc 0.0d0))
	brc
	(let* ((r (- (* (1- b) (/ x y)) a))
	       (k (if (or (<= b 1) (< r 1.0d0)) 0 (min (1- n) (floor r)))))
	  (* brc
	     (loop with w = 0
		for i from 0 to (1- n)
		for next = d then (* (/ (+ a b i -1) (+ a i)) x next)
		while (or (<= i k) (> next (* w double-float-epsilon)))
		when (= i k) do (setf w next)
		sum next))))))

(defun %reg-inc-beta-bgrat (a b x y w)
  "Asymptotic expansion when A > B, A >= 15, B <= 1.
TODO: The computation of the incomplete gamma ratio should be done more safely.
See ACM TOMS 708 for a sample safe implementation."
  (declare (ignore y))
  (let* ((nu (+ a (/ (1- b) 2.0d0)))
	 (lnx (log x))
	 (z (* (- nu) lnx)))		; e^(-z) = x^(a+(b-1)/2)
    (assert (/= (* b z) 0) () "The asymptotic expansion cannot be computed.")
    (let* ((r (/ (* (exp (- z)) (expt z b)) (gamma b)))
	   (u (* r (exp (- (+ (%reg-inc-beta-algdiv b a) (* b (log nu))))))))
      (assert (/= u 0) () "The asymptotic expansion cannot be computed.")
      (let ((ratio (- 1.0d0 (/ (lower-incomplete-gamma b z) (gamma b))))
	    (v (/ 0.25d0 (sqr nu)))
	    (t2 (/ lnx 4.0d0))
	    (l (/ w u)))
	(+ w (* u (loop with c = (make-array 30) and d = (make-array 30)
		     with result = (/ ratio r)
		     for n from 1 to 30
		     for 2n from 2 to 60 by 2
		     for j = (* v (+ (* (+ b 2n -2) (+ b 2n -1) (/ ratio r))
				     (+ b 2n -1 z)))
		     then (* v (+ (* (+ b 2n -2) (+ b 2n -1) j)
				  (* (+ b 2n -1 z) t2n)))
		     for t2n = t2 then (* t2n t2)
		     for cn = 1/6 then (/ cn (* 2n (1+ 2n)))
		     for s = (loop for i from 1 below n
				sum (* (- (* b i) n)
				       (elt c (1- i)) (elt d (- n i 1))))
		     do (let* ((dn (+ (* (1- b) cn) (/ s n)))
			       (dj (* dn j)))
			  (setf (elt c (1- n)) cn
				(elt d (1- n)) dn)
			  (incf result dj)
			  (assert (> result 0.0d0) () "Negative result.")
			  (when (> (abs dj) (* 15.0d0 double-float-epsilon
					       (+ result l)))
			    (return result)))
		     finally (return result))))))))

(defun %reg-inc-beta-bfrac (a b x y l)
  "Continued fraction expansion when A, B > 1, L = \(A + B) * Y - B."
  (let ((brc (%reg-inc-beta-brcmp1 0.0d0 a b x y)))
    (if (= brc 0.0d0)
	0.0d0
	(flet ((alpha (m)
		 (let ((n (1- m)))
		   (/ (* (+ n a -1) (+ n a b -1) n (- b n) (sqr x))
		      (sqr (+ a (* 2 n) -1)))))
	       (beta (m)
		 (let ((n (1- m)))
		   (+ n (/ (* n (- b n) x) (+ a (* 2 n) -1))
		      (/ (* (+ n a) (+ l 1 (* n (1+ y))))
			 (+ a (* 2 n) 1))))))
	  (* brc (generalized-continued-fraction
		  #'alpha #'beta :a1 1.0d0 :b0 0.0d0
		  :tolerance (* 15.0d0 double-float-epsilon)))))))

(defun %reg-inc-beta-bcorr (a0 b0)
  "Evaluate del\(A0) + del\(B0) - del\(A0 + B0), where
del\(a) = ln\(gamma\(a)) - \(a - 0.5) * ln\(a) + a - 0.5 * ln\(2pi).
TODO: This should be implemented in a more stable way, see ACM TOMS 708."
  (flet ((del (a)
	   (+ (log-gamma a) (* (- 0.5d0 a) (log a)) a
	      (* -0.5d0 (log (* 2.0d0 pi))))))
    (- (+ (del a0) (del b0)) (del (+ a0 b0)))))

(let* ((e0 (/ 2 (sqrt pi)))
       (e1 (expt 2.0d0 -3/2)))
  (defun %reg-inc-beta-basym (a b l &optional (iterations 20))
    "Asymptotic expansion for large A and B, L = \(A + B) * Y - B.
ITERATIONS should be even.
TODO: Spaghetti code."
    (flet ((rlog1 (x) (- x (log (1+ x)))))
      (let* ((f (+ (* a (rlog1 (/ (- l) a))) (* b (rlog1 (/ (- l) b)))))
	     (e-f (exp (- f))))
	(if (= e-f 0.0d0)
	    0.0d0
	    (let* ((z0 (sqrt f))
		   (z (* (sqrt 2.0d0) z0))
		   (z2 (+ f f))
		   (size (+ iterations 2))
		   (a0 (make-array size :element-type 'double-float))
		   (b0 (make-array size :element-type 'double-float))
		   (c (make-array size :element-type 'double-float))
		   (d (make-array size :element-type 'double-float)))
	      (setf (elt b0 0) -1.0d0)
	      (conditional-let* (< a b) #'identity
		  ((h (/ a b) (/ b a))
		   (r0 (/ (1+ h)) (/ (1+ h)))
		   (r1 (/ (- b a) b) (/ (- b a) a))
		   (w0sq (/ (* a (1+ h))) (/ (* b (1+ h))))
		   (w0 (sqrt w0sq)))
		(loop with h2 = (* h h)
		   for n from 0 to iterations by 2
		   for hn = 1 then (* hn h2)
		   for zn-1 = (/ z) then (* zn-1 z2)
		   for zn = 1 then (* zn z2)
		   for w = 1 then (* w w0sq)
		   for s = 1 then (+ s hn)
		   for j0 = (/ (* 2.0d0 e0 (exp (* z0 z0)) (- 1.0d0 (erf z0))))
		   then (+ (* e1 zn-1) (* (1- n) j0))
		   for j1 = e1 then (+ (* e1 zn) (* n j1))
		   for (d1 d2) =
		     (progn
		       (setf (elt a0 n) (/ (* r0 2 (1+ (* h hn))) (+ n 2)))
		       (setf (elt a0 (1+ n)) (/ (* r1 2 s) (+ n 3)))
		       (loop for i from n to (1+ n)
			  for r = (* (1+ i) -1/2)
			  do
			    (setf (elt b0 1) (* r (elt a0 1)))
			    (loop for m from 2 to i
			       do (setf (elt b0 m)
					(+ (* r (elt a0 m))
					   (/ (loop for j from 1 below m
						 sum (* (- (* j (1+ r)) m)
							(elt a0 j)
							(elt b0 (- m j))))
					      m))))
			    (setf (elt c i) (/ (elt b0 i) (1+ i)))
			    (setf (elt d i)
				  (- (+ (elt c i)
					(loop for j from 1 below i
					   sum (* (elt d (- i j)) (elt c j))))))
			  finally (return (list (elt d n) (elt d (1+ n))))))
		   for t0 = (* d1 w j0)
		   for t1 = (* d2 (* w w0) j1)
		   for change = (+ t0 t1)
		   while (> (abs change) (* 100.0d0 double-float-epsilon sum))
		   sum change into sum
		   finally
		   (return (* e0 e-f sum
			      (exp (- (%reg-inc-beta-bcorr a b)))))))))))))

(defun regularized-incomplete-beta (a b x)
  (assert (and (>= a 0) (>= b 0) (<= 0 x 1)) (a b x)
	  "A and B should be nonnegative and X should be less than 1.")
  (assert (not (= a x 0)) (a x) "One of A and X should not be 0.")
  (assert (not (and (= b 0) (= x 1))) (b x) "X cannot be 1 when B is 0.")
  (let ((y (- 1.0d0 x)))
    (cond ((or (= b 0.0d0) (= x 0.0d0)) 0.0d0)
	  ((or (= a 0.0d0) (= y 0.0d0)) 1.0d0)
	  ((< (max a b) (* 0.001d0 #.(max double-float-epsilon 1.0d-15)))
	   (/ b (+ a b)))
	  ((<= (min a b) 1.0d0)
	   (conditional-swap-let (> x 0.5d0) (lambda (x) (- 1.0d0 x))
	       ((a b) (x y))
	     (cond ((< b (min double-float-epsilon
			      (* double-float-epsilon a)))
		    (%reg-inc-beta-fpser a b x))
		   ((and (< a (min double-float-epsilon
				   (* double-float-epsilon b)))
			 (<= (* b x) 1.0d0))
		    (- 1.0d0 (%reg-inc-beta-apser a b x)))
		   ((> (max a b) 1.0d0)
		    (cond ((or (<= b 1.0d0)
			       (and (< x 0.1d0)
				    (<= (expt (* x b) a) 0.7d0)))
			   (%reg-inc-beta-bpser a b x))
			  ((>= x 0.3d0)
			   (- 1.0d0 (%reg-inc-beta-bpser b a y)))
			  ((> b 15.0d0)
			   (- 1.0d0 (%reg-inc-beta-bgrat b a y x 0)))
			  (t (let ((w1 (%reg-inc-beta-bup b a y x 20)))
			       (- 1.0d0 (%reg-inc-beta-bgrat
					 (+ b 20) a y x w1))))))
		   (t (cond ((or (> a (min 0.2d0 b))
				 (< (expt x a) 0.9d0))
			     (%reg-inc-beta-bpser a b x))
			    ((>= x 0.3d0)
			     (- 1.0d0 (%reg-inc-beta-bpser b a y)))
			    (t (let ((w1 (%reg-inc-beta-bup b a y x 20)))
				 (- 1.0d0 (%reg-inc-beta-bgrat
					   (+ b 20) a y x w1)))))))))
	  (t (let* ((l (if (> a b) (- (* (+ a b) y) b) (- a (* (+ a b) x))))
		    (lneg (- l)))
	       (conditional-swap-let (< l 0.0d0) (lambda (x) (- 1.0d0 x))
		   ((l lneg) (a b) (x y))
		 (cond ((< b 40.0d0)
			(if (<= (* b x) 0.7d0)
			    (%reg-inc-beta-bpser a b x)
			    (multiple-value-bind (n b) (floor b)
			      (when (= b 0.0d0) (decf n) (setf b 1.0d0))
			      (let ((w (%reg-inc-beta-bup b a y x n)))
				(cond ((<= x 0.7d0)
				       (+ w (%reg-inc-beta-bpser a b x)))
				      ((<= a 15.0d0)
				       (let ((w (+ w (%reg-inc-beta-bup
						      a b x y 20))))
					 (%reg-inc-beta-bgrat
					  (+ a 20) b x y w)))
				      (t (%reg-inc-beta-bgrat a b x y w)))))))
		       ((or (and (> a b)
				 (or (<= b 100.0d0) (> l (* b 0.03d0))))
			    (<= a 100.0d0)
			    (> l (* a 0.03d0)))
			(%reg-inc-beta-bfrac a b x y l))
		       (t (%reg-inc-beta-basym a b l)))))))))

(defun incomplete-beta-inverse (a b x)
  "TODO: a much better guess should be computed. A very detailed treatise
of all the cases can be found in the Boost library documentation."
  (newton-raphson (lambda (y) (- (incomplete-beta a b y) x))
		  (lambda (y) (* (expt y (1- a)) (expt (- 1.0d0 y) (1- b))))
		  :initial-guess (expt (* a x (beta a b)) (/ a))))

(defun regularized-incomplete-beta-inverse (a b x)
  (incomplete-beta-inverse a b (* x (beta a b))))


;;; Numerical methods: Inverse-linear-interpolation, Newton-Raphson, numerical derivative

(defparameter *inv-lin-interp-precision* 1d-12)
(defparameter *inv-lin-interp-max-iteration* 1000)
(defun inverse-linear-interpolation (fn range)
  (assert (< (first range) (second range)))
  (loop repeat *inv-lin-interp-max-iteration*
      with a = (first range)
      with b = (second range)
      as fa = (funcall fn (first range))
      as fb = (funcall fn (second range))
      as lean = (progn (assert (/= (signum fa) (signum fb)))
                       (/ (- fb fa) (- b a)))
      as x-intercept = (let ((y-intercept (- fa (* lean a)))) 
               (/ (- y-intercept) lean))
      as fx = (funcall fn x-intercept)
      do (cond ((>= *inv-lin-interp-precision* (abs fx)) (return x-intercept))
               ((plusp lean) (cond ((plusp fx) (setf b x-intercept))
                                   ((minusp fx) (setf a x-intercept))))
               ((minusp lean) (cond ((plusp fx) (setf a x-intercept))
                                    ((minusp fx) (setf b x-intercept))))
               (t (error "illegal range for method of inverse-linear-interpolation range = ~A, lean = ~A"
                         `(,a ,b) lean)))
      finally (return x-intercept)))

(defparameter *newton-raphson-precision* 1.0d-12)
(defparameter *newton-raphson-maximal-iteration-count* 100)
(defparameter *newton-raphson-initial-divisions* 100)
(defun newton-raphson-guess (fn range)
  (loop with min = (abs (funcall fn (first range))) and minx = (first range)
     for k from 1 below *newton-raphson-initial-divisions*
     for x = (linear-combination
	      (first range)
	      (/ k (1- *newton-raphson-initial-divisions*))
	      (second range))
     for fnx = (abs (funcall fn x))
     when (< fnx min) do (setf min fnx minx x)
     finally (return minx)))
(defun newton-raphson (fn derivative &key range initial-guess)
  (assert (or range initial-guess) (range initial-guess)
	  "You have to supply a range or an initial guess.")
  (let ((initial-guess (or initial-guess (newton-raphson-guess fn range))))
    (loop repeat *newton-raphson-maximal-iteration-count*
       for x = initial-guess then (- x (/ fx dfx))
       for fx = (funcall fn x)
       while (>= (abs fx) *newton-raphson-precision*)
       for dfx = (funcall derivative x)
       finally (return x))))

(defun numerical-derivative (fn x &key range)
  (let ((h (* (sqrt double-float-epsilon) x)))
    (if (or (null range) (< (first range) (- x h) (+ x h) (second range)))
	(/ (- (funcall fn (+ x h)) (funcall fn (- x h))) (* 2 h))
	(let ((h1 (max (first range) (- x h)))
	      (h2 (min (+ x h) (second range))))
	  (/ (- (funcall fn (+ x h2)) (funcall fn (- x h1))) (+ h1 h2))))))

;; incomplete gamma function
;; ref: Numerical Recipes
(defun gammp (a x)
  (declare (type double-float a x))
  (check-type a double-float)
  (check-type x double-float)
  (let ((gamser 0d0) (gammcf 0d0) (gammp 0d0))
    (declare (type double-float gamser gammcf gammp))

    (if (or (< x 0d0) (<= a 0d0)) (error " invalid arguments for gammp : x = ~A, shape = ~A" x a))
    (cond 
     ((< x (+ a 1d0))
      (setq gamser (gser a x)) 
      (setf gammp gamser)) 
     (t
      (setq gammcf (gcf a x)) 
      (setf gammp (- 1d0 gammcf)))) 
    gammp))

(defun gammln (xx)
  (declare (type double-float xx))
  (check-type xx double-float)
  (let* ((cof (make-array 6 :element-type 'double-float :initial-contents
                          '(76.18009173d0 -86.50532033d0 24.01409822d0 
                            -1.231739516d0 0.120858003d-2 -0.536382d-5)))
         (stp 2.50662827465d0) 
         (half 0.5d0)
         (one 1.0d0)
         (fpf 5.5d0)
         (x (1- xx))
         (buff (+ x fpf))
         (tmp (- (* (+ x half) (log buff)) buff))
         (ser one))
    (declare (type (simple-array double-float (*)) cof)) 
    (declare (type double-float stp half one fpf tmp ser x))
 
    (do ((j 0 (+ j 1)))
        ((> j 5) t)
      (declare (type fixnum j))
      (setf x (1+ x))
      (setf ser (+ ser (/ (aref cof j) x))))
    (+ tmp (log (* stp ser)))))

(defun gser (a x &key (itmax 10000) (eps 3.0d-7))
  (declare (type double-float a x eps))
  (declare (type fixnum itmax))

  (prog ((gamser 0d0) (gln 0d0) (ap 0d0) 
                      (del 0d0) (sum 0d0))
    (declare (type double-float gamser gln ap del sum))


    (setf gln (gammln a)) 
    (when 
        (<= x 0)  
      (if (< x 0d0) (error " invalid argument to gser "))
      (setf gamser 0d0) 
      (return (values gamser gln))) 

    (setf ap a) 
    (setf sum (/ 1d0 a)) 
    (setf del sum) 
    (do ((n 1 (+ n 1)))
        ((> n itmax) t)
      (declare (type fixnum n))
      (setf ap (+ ap 1))
      (setf del (/ (* del x) ap))
      (setf sum (+ sum del))
      (if (< (abs del) (* (abs sum) eps)) (go label1))) 

    (error " a too large , itmax too small in gser ") 
   label1 
    (setf gamser (* sum
                    (handler-case (exp (+ (- x) (* a (log x)) (- gln)))
                      (FLOATING-POINT-UNDERFLOW (c) (declare (ignore c)) 0d0))))
   
    (return (values gamser gln))))


(defun gcf (a x &key (itmax 10000) (eps 3.0d-7))
  (declare (type double-float a x eps))
  (declare (type fixnum itmax))

  (prog ((gammcf 0d0) (gln 0d0) (gold 0d0) (a0 0d0) (a1 0d0) 
                      (b0 0d0) (b1 0d0) (fac 0d0) (an 0d0) (ana 0d0) (anf 0d0) (g 0d0))
    (declare (type double-float gln gammcf gold a0 a0 b0 b1 fac an ana anf g))

    (setf gln (gammln a)) 
    (setf gold 0d0) 
    (setf a0 1d0) 
    (setf a1 x) 
    (setf b0 0d0) 
    (setf b1 1d0) 
    (setf fac 1d0) 
    (do ((n 1 (+ n 1)))
        ((> n itmax) t)
      (setf an (coerce n 'double-float))
      (setf ana (- an a))
      (setf a0 (* (+ a1 (* a0 ana)) fac))
      (setf b0 (* (+ b1 (* b0 ana)) fac))
      (setf anf (* an fac))
      (setf a1 (+ (* x a0) (* anf a1)))
      (setf b1 (+ (* x b0) (* anf b1)))
      (when 
          (not (= a1 0d0))
        (setf fac (/ 1d0 a1))
        (setf g (* b1 fac)) 
        (if (< (abs (/ (- g gold) g)) eps) (go label1))
        (setf gold g))) 
   
    (error " a too large , itmax too small in gcf ") 
   label1 
    (setf gammcf (* (handler-case (exp (+ (- x) (* a (log x)) (- gln)))
                      (FLOATING-POINT-UNDERFLOW (c) (declare (ignore c)) 0d0))
                    g))
   
    (return (values gammcf gln))))
(in-package :clml.statistics)


(eval-when (:compile-toplevel :load-toplevel)
  (defclass distribution ()
  ((mean)
   (variance)
   (skewness)
   (kurtosis)
   (mode)))


  (defclass discrete-distribution (distribution) ())
  (defclass continuous-distribution (distribution) ()))


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
(defgeneric mode (distribution &optional test)
  (:documentation "Gives a mode according to DISTRIBUTION."))
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

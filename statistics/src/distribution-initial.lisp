(in-package :clml.statistics)

(eval-when (:compile-toplevel :load-toplevel)
  (defun parse-dist-slots (distribution-slots class)
    (let (class-slots methods)
      (dolist (slot distribution-slots)
        (let* ((def (cdr slot))
               (updater (getf def :accessor)))
          (if updater
              (let ((writer (intern (format nil "~A-~A" 'set updater)))
                    (s (car slot)))
                (setf def (copy-list def))
                (remf def :accessor)
                (push `(,s :reader ,updater
                           :writer ,writer ,@def) class-slots)
                ;; use (defmethod (setf foo))
                (push `(defmethod (setf ,updater) (new-value (obj ,class))
                         (setf (slot-value obj ',s) new-value)
                         (update-distribution obj))
                      methods))
              (push slot class-slots))))
      (values (nreverse class-slots)
              (nreverse methods)))))

;; easy definition of parameter cache class
(defmacro defdistribution (name direct-supers direct-slots &rest options)
  (multiple-value-bind (new-slot-definitions methods) (parse-dist-slots direct-slots name)
    `(prog1 (defclass ,name ,direct-supers ,new-slot-definitions ,@(when options
                                                                     `(&rest . ,options)))
       ,@methods)))


(defclass distribution ()
  ((mean)
   (variance)
   (skewness)
   (kurtosis)
   (mode)))


(defclass discrete-distribution (distribution) ())
(defclass continuous-distribution (distribution)
  ((mean :initarg :mean :accessor mean :initform 0d0)))

(defclass gamma-like-distribution (continuous-distribution)
                 ((scale :reader scale :writer set-scale :initarg :scale)
                  (shape :reader shape :writer set-shape :initarg :shape)))

(defclass log-normal-distribution (continuous-distribution)
                 ((average :reader average :writer set-average :initarg
                           :average)
                  (std :reader std :writer set-std :initarg :std)))

(defclass uniform-distribution (continuous-distribution)
  ((from :reader uniform-from :writer set-uniform-from :initarg
         :from)
   (to :reader uniform-to :writer set-uniform-to :initarg :to)
   (width :reader uniform-width)
   (denominator :reader uniform-denom :writer set-uniform-denom)
   (skewness :initform 0.0) (kurtosis :initform 1.8)))
(defclass gamma-distribution (gamma-like-distribution)
                 ((gamma-factor :reader gamma-factor) (shape-inv) (d) (c)))
(defclass erlang-distribution (gamma-like-distribution)
                 ((include-zero :reader include-zero :writer set-include-zero
                                :initarg :include-zero :initform nil)))
(defclass exponential-distribution (continuous-distribution)
                 ((hazard :reader hazard :writer set-hazard :initarg :hazard)
                  (scale :reader scale)
                  (include-zero :reader include-zero :writer set-include-zero
                                :initarg :include-zero :initform nil)
                  (skewness :initform 2.0) (kurtosis :initform 9.0)
                  (mode :initform 0.0)))
(defclass normal-distribution (continuous-distribution)
                 ((average :reader average :writer set-average :initarg
                           :average :initform 0.0)
                  (mode :initform 0.0)
                  (std :reader std :writer set-std :initarg :std :initform 0.0)
                  (skewness :initform 0.0) (kurtosis :initform 3.0)))
(defclass chi-square-distribution (continuous-distribution)
                 ((freedom :reader freedom :writer set-freedom :initarg
                           :freedom)
                  (eq-gamma :initform
                            (make-instance 'gamma-distribution :shape 2.0
                                           :scale 2.0)
                            :reader eq-gamma)))
(defclass t-distribution (continuous-distribution)
                 ((freedom :reader freedom :writer set-freedom :initarg
                           :freedom)
                  (t-precalc :reader t-precalc :writer set-t-precalc) (r) (b)
                  (c) (a) (d) (k) (w) (s) (p) (q) (t1) (t2) (v1) (v2)))
(defclass beta-distribution (continuous-distribution)
                 ((shape1 :reader shape1 :writer set-shape1 :initarg :shape1)
                  (shape2 :reader shape2 :writer set-shape2 :initarg :shape2)
                  (alpha-gamma :initform
                   (make-instance 'gamma-distribution :shape 1.0 :scale 1.0))
                  (beta-gamma :initform
                   (make-instance 'gamma-distribution :shape 1.0 :scale
                                  1.0))))
(defclass f-distribution (continuous-distribution)
                 ((freedom1 :reader freedom1 :writer set-freedom1 :initarg
                            :freedom1)
                  (freedom2 :reader freedom2 :writer set-freedom2 :initarg
                            :freedom2)
                  (chi1 :initform
                   (make-instance 'chi-square-distribution :freedom 1))
                  (chi2 :initform
                   (make-instance 'chi-square-distribution :freedom 1))
                  (f)))
(defclass bernoulli-related-distribution (discrete-distribution)
                 ((probability :reader probability :writer set-probability
                               :initarg :probability)))
(defclass binomial-distribution (bernoulli-related-distribution)
                 ((size :reader size :writer set-size :initarg :size) (table)
                  (ki) (vi) (b) (k) (w) (nsq)))
(defclass geometric-distribution (bernoulli-related-distribution)
  ((table) (ki) (vi) (b) (k) (w) (nsq) (psq) (q) (r) (c)))
(defclass hypergeometric-distribution (discrete-distribution)
                 ((elements :reader elements :writer set-elements :initarg
                            :elements)
                  (successes :reader successes :writer set-successes :initarg
                             :successes)
                  (samples :reader samples :writer set-samples :initarg
                           :samples)
                  (table) (ki) (vi) (b) (k) (w) (nsq) (a1)))
(defclass cauchy-distribution (continuous-distribution)
                 ((location :reader location :writer set-location :initarg
                            :location)
                  (scale :reader scale :writer set-scale :initarg :scale)))
(defclass logistic-distribution (continuous-distribution)
                 ((location :reader location :writer set-location :initarg
                            :location)
                  (scale :reader scale :writer set-scale :initarg :scale)
                  (skewness :initform 0.0) (kurtosis :initform 4.2)))
(defclass negative-binomial-distribution
                 (bernoulli-related-distribution)
                 ((success-r :reader success-r :writer set-success-r :initarg
                             :success-r)
                  (table) (ki) (vi) (b) (k) (w) (nsq) (psq) (q) (r) (xl) (xu)
                  (pl) (pu) (que) (s) (tee)))
(defclass poisson-distribution (discrete-distribution)
                 ((rate :reader rate :writer set-rate :initarg :rate) (table)
                  (ki) (vi) (b) (k) (w) (nsq) (psq) (q) (r) (xl) (xu) (pl) (pu)
                  (c)))
;;; todo -- weibull dist is not gamma distribution; bad definition
(defclass weibull-distribution (gamma-like-distribution)
                 ((include-zero :reader include-zero :writer set-include-zero
                                :initarg :include-zero :initform nil)
                  (r-inv)))

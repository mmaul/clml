;; implement sticky-HDP-HMM as mix-in
(defpackage :nonparametric.sticky-hdp-hmm
;  (:nicknames :sticky)
  (:use :cl :nonpara.stat :hjs.util.meta
	:nonparametric.dpm
	:nonparametric.hdp
	:nonparametric.hdp-hmm)
  (:export :sticky-hdp-hmm
	   :sticky-hidden-state
	   :sticky-state-uniform
	   
	   :sticky-kappa
	   
	   :*rho-base-c*
	   :*rho-base-d*))

(in-package :nonparametric.sticky-hdp-hmm)

(defparameter *rho-base-c* 1d0)
(defparameter *rho-base-d* 1d0)

(defclass sticky-hdp-hmm (hdp-hmm)
  ((base-distribution :initform (make-instance 'sticky-state-uniform))
   (kappa :initform (beta-random *rho-base-c* *rho-base-d*) :accessor sticky-kappa)))

(defclass sticky-hidden-state (hidden-state)
  ((sticky-wj  :initform 0 :accessor sticky-wj)
   (sticky-mjj :initform 0 :accessor sticky-mjj)))

(defclass sticky-state-uniform (state-uniform)
  ((cluster-class :initform 'sticky-hidden-state)))

(defmethod sample-latent-table ((cluster sticky-hidden-state) &rest args &key franchise alpha kappa)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float alpha kappa))
  (let ((n (gethash franchise (cluster-tmp-table cluster) 0))
	(ab (* alpha (the double-float (cluster-beta cluster)))))
    (declare (type fixnum n)
	     (type double-float ab))
    (cond ((eq cluster franchise)
	   (incf ab kappa)
	   (unless (zerop (bernoulli (/ ab (the double-float (+ n ab)))))
	     (incf (the fixnum (sticky-mjj cluster)))))
	  (t
	   (unless (zerop (bernoulli (/ ab (the double-float (+ n ab)))))
	     (incf (the fixnum (cluster-latent-table cluster))))))))

(defmethod sample-cluster-parameters ((cluster sticky-hidden-state) dist (dpm sticky-hdp-hmm))
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (ignore dist))
  (let* ((alpha (dpm-hyper dpm))
	 (kappa (sticky-kappa dpm))
	 (rho (/ kappa (+ alpha kappa)))
	 (wj (binomial-random (sticky-mjj cluster)
			      (/ rho
				 (+ rho (* (the double-float (cluster-beta cluster))
					   (the double-float (- 1d0 rho))))))))
    (declare (type double-float alpha kappa rho)
	     (type fixnum wj))
    (setf (sticky-wj cluster) wj)
    (setf (cluster-latent-table cluster)
      (+ (the fixnum (cluster-latent-table cluster))
	 (the fixnum (max 1 (- (the fixnum (sticky-mjj cluster)) wj))))) ;;; ugly for safety
    (call-next-method)))

(defmethod hypers-sampling ((dpm sticky-hdp-hmm))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (multiple-value-bind (alpha* gamma) (call-next-method) ;; sampling (alpha + kappa) as alpha  
    (let ((w__ 0)
	  (m__ 0)
	  (clusters (dpm-clusters dpm)))
      (declare (type fixnum w__ m__)
	       (type vector clusters)
	       (type double-float alpha* gamma))
      ;; duplication calc of m__ -- but very cheap, so choice cleary
      (loop for i fixnum from 0 below (dpm-k dpm)
	  for c = (aref clusters i) do
	  (incf w__ (sticky-wj c))
	  (incf m__ (sticky-mjj c))
	  ;; now discard sticky counts
	  (setf (sticky-wj c) 0)
	  (setf (sticky-mjj c) 0))
      (let* ((new-rho (beta-random (+ w__ (the double-float *rho-base-c*))
				   (+ (the fixnum (- m__ w__)) (the double-float *rho-base-d*))))
	     (kappa (* new-rho alpha*))
	     (alpha (- alpha* kappa)))
	(declare (type double-float new-rho kappa alpha))
	(setf (sticky-kappa dpm) kappa)
	(setf (dpm-hyper dpm) alpha)
	(values alpha gamma kappa)))))

(defmethod add-customer :around ((dpm sticky-hdp-hmm) customer old &rest args)
  (apply #'call-next-method dpm customer old :kappa (sticky-kappa dpm) args))

;; sample
(defclass gaussian-sticky (sticky-hidden-state gaussian-state) ())

(defclass gauss-sticky-hmm (sticky-hdp-hmm gauss-hdp-hmm)
  ((base-distribution :initform (make-instance 'state-gaussian-sticky))))

(defclass state-gaussian-sticky (sticky-state-uniform state-gaussian)
  ((cluster-class :initform 'gaussian-sticky)))
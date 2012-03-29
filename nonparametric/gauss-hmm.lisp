(defpackage :nonparametric.hdp-hmm
  (:export :gaussian-state
	   :gauss-hdp-hmm
	   :state-gaussian
	   
	   :make-sticky-test))

(in-package :nonparametric.hdp-hmm)

(defclass gaussian-state (gaussian-cluster hidden-state) ())

(defclass gauss-hdp-hmm (gauss-dpm hdp-hmm)
  ((base-distribution :initform (make-instance 'state-gaussian))))

(defclass state-gaussian (dp-gaussian state-uniform)
  ((cluster-class :initform 'gaussian-state)))

(defmethod emission-prob ((state gaussian-state) data &rest args &key v)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (ignore v))
  (normal-density (cluster-center state) (cluster-std state) data))

(defmethod initialize ((dpm gauss-hdp-hmm))
  (when (estimate-base? dpm)
    (setf (estimate-base? dpm) nil))
  (let ((data (dpm-data dpm)))
    (loop for i from 0 below (length data)
	for before = (if (zerop i) (hdp-hmm-eos dpm) (point-cluster (aref data (1- i)))) do
	  (add-customer dpm (aref data i) least-positive-double-float :franchise before)))
  (parameters-sampling dpm)
  (hypers-sampling dpm))

(defun make-gauss-example (k length &optional (std 3d0))
  (let ((states (coerce (loop repeat k collect (normal-random 0d0 std)) 'vector))
	(trans (make-array length :element-type 'fixnum))
	(ans (make-array length)))
    (loop for i from 0 below length
	for s = 0 then (let ((new (1+ s)))
			 (if (= new k) 0 new))
	for d = (normal-random (aref states s) 1d0) do
	  (setf (aref trans i) s)
	  (setf (aref ans   i) (make-point :data d)))
    (format t "states~%")
    (loop for s across states do
	  (format t "~S~%" s))
    (values trans ans)))

(defun make-sticky-test (length states &optional (stds (map 'vector #'(lambda (x) (declare (ignore x)) 1d0) states)))
  (let ((trans (make-array length :element-type 'fixnum))
	(ans (make-array length)))
    (loop for i from 0 below length
	for s = 0 then (case (random 100)
			 (98 (ecase s
			       (0 1)
			       (1 2)
			       (2 0)))
			 (99 (ecase s
			       (0 2)
			       (1 0)
			       (2 1)))
			 (t s))
	for d = (normal-random (aref states s) (aref stds s)) do
	  (setf (aref trans i) s)
	  (setf (aref ans   i) (make-point :data d)))
    (values trans ans)))
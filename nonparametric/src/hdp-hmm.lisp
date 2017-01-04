;-*- coding: utf-8 -*-
;; HDP-HMM (or infinite HMM)
(in-package :clml.nonparametric.hdp-hmm)


(defparameter *smooth-beta* 1d-2)


;;;;;;


(defmethod initialize-instance ((instance hdp-hmm) &rest initargs)
  (declare (ignore initargs))
  (call-next-method)
  (setf (hdp-hmm-eos instance) (make-instance (cluster-class (dpm-base instance))))
  instance)



;; specific methods for distribution
(defmethod add-to-cluster ((cluster hidden-state) data &rest args &key franchise)
  #+sbcl (declare (ignorable args))
  (incf (gethash data (emission cluster) 0))
  (incf (the fixnum (gethash franchise (cluster-dist-table cluster) 0)))
  (call-next-method))

(defmethod remove-from-cluster ((cluster hidden-state) data &rest args &key franchise)
  #+sbcl (declare (ignorable args))
  (decf (gethash data (emission cluster)))
  (decf (gethash franchise (cluster-dist-table cluster)))
  (call-next-method))

(defmethod density-to-cluster ((dpm hdp-hmm) (cluster hidden-state) data &rest args &key franchise)
  #+sbcl (declare (ignorable args))
  (let ((v (vocabulary dpm))
        (k (dpm-k dpm)))
    (* (trans-prob franchise cluster :k k)
       (emission-prob cluster data :v v))))

(defgeneric trans-prob (before after &rest args &key &allow-other-keys)
  (:documentation "transition probability between before and after"))

(defmethod trans-prob ((before hidden-state) (after hidden-state) &rest args &key k)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type fixnum k)
           #+sbcl (ignorable args))
  (let ((table (cluster-dist-table after)))
    (/ (the double-float (+ (the fixnum (gethash before table 0)) (the double-float *smooth-beta*)))
       (the double-float (+ (the fixnum (cluster-size after)) (* (the double-float *smooth-beta*) k))))))

(defgeneric emission-prob (state data &rest args &key &allow-other-keys)
  (:documentation "data emission probability from state"))

(defmethod emission-prob ((state hidden-state) data &rest args &key v)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type fixnum v)
           #+sbcl (ignorable args))
  (let ((table (emission state)))
    (/ (the double-float (+ (the fixnum (gethash data table 0)) (the double-float *smooth-beta*)))
       (the double-float (+ (the fixnum (cluster-size state))  (* (the double-float *smooth-beta*) v))))))

(defmethod make-new-cluster ((dpm hdp-hmm) (dist state-uniform) data &optional discarded-cluster)
  (declare (ignore data discarded-cluster))
  (let ((new (call-next-method)))
    (clrhash (emission new))
    (clrhash (cluster-dist-table new))
    new))

(defmethod base-distribution ((dpm hdp-hmm) (dist state-uniform) data &rest args)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (ignore data args))
  (/ 1d0 (the fixnum (+ (the fixnum (dpm-k dpm)) 1))))

(defmethod initialize ((dpm hdp-hmm))
  (when (estimate-base? dpm)
    (let ((memo (make-hash-table :test #'equal)))
      (loop for d across (dpm-data dpm) do
            (setf (gethash (point-data d) memo) t))
      (setf (vocabulary dpm) (hash-table-count memo))))
  (setf (estimate-base? dpm) nil) ;; safety
  (let ((data (dpm-data dpm)))
    (loop for i from 0 below (length data)
        for before = (if (zerop i) (hdp-hmm-eos dpm) (point-cluster (aref data (1- i)))) do
          (add-customer dpm (aref data i) 1 :franchise before)))
  (parameters-sampling dpm)
  (hypers-sampling dpm))

(defmethod seatings-sampling ((dpm hdp-hmm))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((data (dpm-data dpm))
        (old-before (hdp-hmm-eos dpm))
        (new-before (hdp-hmm-eos dpm)))
    (declare (type vector data))
    (loop for i fixnum from 0 below (the fixnum (length data))
        for d = (aref data i)
        for old-state = (point-cluster d)
        for new-state = (add-customer dpm d
                                      (remove-customer dpm d :franchise old-before)
                                      :franchise new-before) do
          (setf new-before new-state old-before old-state))))

;;; util for test
(defun make-repeat-pattern (pattern times)
  (with-output-to-string (s)
    (dotimes (i times)
      (format s pattern))))

(defun make-pattern-data (pattern times)
  (map 'vector #'(lambda (x) (make-instance 'point :data (intern (string-upcase (make-string 1 :initial-element x)) :keyword)))
       (make-repeat-pattern pattern times)))

(defgeneric show-hidden-states (hdp-hmm))
(defmethod show-hidden-states ((hdp-hmm hdp-hmm))
  (map 'vector #'(lambda (x) (position (point-cluster x) (dpm-clusters hdp-hmm)))
       (dpm-data hdp-hmm)))

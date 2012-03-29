;; Dirichlet Process Mixture
;; with simple one-dimension gauss distribution definition
(defpackage :nonparametric.dpm
  ; (:nicknames :dpm)
  (:use :cl :hjs.util.meta :nonpara.stat)
  (:export :dpm
	   :dpm-k
	   :dpm-p
	   :dpm-base
	   :dpm-clusters
	   :dpm-hyper
	   :dpm-data
	   :dpm-cluster-layers
	   :estimate-base?
	   
	   :logged-dpm
	   
	   :point
	   :make-point
	   :point-data
	   :point-cluster
	   
	   :cluster
	   :gaussian-cluster
	   :cluster-size
	   :cluster-center
	   :cluster-std
	   
	   :dp-distribution
	   :dp-gaussian
	   :cluster-class
	   :average-of-average
	   :std-of-average
	   
	   :gauss-dpm
	   
	   :density-to-cluster
	   :base-distribution
	   :make-new-cluster
	   :sample-cluster-parameters
	   :sample-distribution
	   
	   :add-customer
	   :remove-customer
	   :add-to-cluster
	   :remove-from-cluster
	   
	   :cluster-rotation
	   
	   :initialize
	   :sampling
	   :seatings-sampling
	   :parameters-sampling
	   :hypers-sampling
	   
	   :make-cluster-result
	   :head-clusters
	   
	   :*hyper-base-a*
	   :*hyper-base-b*))

(in-package :nonparametric.dpm)

(defparameter *hyper-base-a* 1d0)
(defparameter *hyper-base-b* 1d0)

(defclass point ()
  ((data :initarg :data :accessor point-data)
   (cluster :initform nil :initarg :cluster :accessor point-cluster)))

(defun make-point (&key data cluster)
  (make-instance 'point :data data :cluster cluster))

(defclass cluster ()
  ((num :initform 0 :accessor cluster-size)))

(defclass gaussian-cluster (cluster)
  ((center :initarg :center :initform 0d0 :accessor cluster-center)
   (std :initform 1d0 :initarg :std :accessor cluster-std)
   (acc :initform 0d0 :accessor cluster-acc)
   (points :initform (make-array 0 :fill-pointer t :adjustable t :element-type 'double-float)
	   :accessor cluster-points)))

(defclass dpm ()
  ((dpm-k :initform 0 :accessor dpm-k)
   (base-distribution :initarg :base-distribution :accessor dpm-base)
   (clusteres :initform (make-adarray 0) :accessor dpm-clusters)
   (layers :initform (make-adarray 1 :element-type 'fixnum :initial-element 0) :accessor dpm-cluster-layers) 
   (dpm-hyper :initform (gamma-random *hyper-base-a* *hyper-base-b*) :accessor dpm-hyper)
   (p :initform (make-array 0 :fill-pointer t :adjustable t :element-type 'double-float) :accessor dpm-p)
   (estimate-base? :initform nil :accessor estimate-base?)
   (data :initarg :data :accessor dpm-data)))

;; use logged density for double-float 
(defclass logged-dpm (dpm) ())

(defclass gauss-dpm (dpm)
  ((base-distribution :initform (make-instance 'dp-gaussian))))

(defclass dp-distribution () 
  ((cluster-class :initform 'cluster :accessor cluster-class)))

(defclass dp-gaussian (dp-distribution)
  ((cluster-class :initform 'gaussian-cluster)
   (ave :initarg :ave :initform 0d0 :accessor average-of-average)
   (std :initarg :std :initform 1d0 :accessor std-of-average)))

(defmethod print-object ((object cluster) stream)
  (print-unreadable-object (object stream :identity t)
    (format stream "~S size ~S" (type-of object) (cluster-size object))))

(defmethod print-object ((object gaussian-cluster) stream)
  (print-unreadable-object (object stream :identity t)
    (format stream "~S size/~S ave/~S std/~S"
	    (type-of object) (cluster-size object) (cluster-center object) (cluster-std object))))

(defgeneric density-to-cluster (dpm cluster data &rest other-data &key &allow-other-keys)
  (:documentation "density of data to cluster"))

(defmethod density-to-cluster ((dpm gauss-dpm) (cluster gaussian-cluster) data &rest args)
  (declare (ignore args))
  (with-slots (center std) cluster
    (normal-density center std data)))

(defgeneric sample-cluster-parameters (cluster dist dpm)
  (:documentation "update cluster parameters if exists"))

(defmethod sample-cluster-parameters ((cluster cluster) (dist dp-distribution) (dpm dpm))
  cluster)

(defmethod sample-cluster-parameters ((cluster gaussian-cluster) (dist dp-gaussian) (dpm gauss-dpm))
  (let* ((size (cluster-size cluster))
	 (mean (/ (+ (average-of-average dist) (cluster-acc cluster)) (1+ size)))
	 (std (/ (cluster-std cluster) (sqrt (1+ size))))
	 (new-center (normal-random mean std)))
    (when (> size 2)
      (setf (cluster-center cluster) new-center)
      (setf (cluster-acc cluster) 0d0)
      (let ((acc 0d0))
	(loop for data across (cluster-points cluster) do
	      (incf acc (expt (- new-center data) 2)))
	(setf (cluster-std cluster) (sqrt (/ acc (chi-square-random size)))))
      (setf (fill-pointer (cluster-points cluster)) 0))
    (call-next-method)))

(defgeneric sample-distribution (dpm distribution)
  (:documentation "update distribution parameters if exists"))

(defmethod sample-distribution ((dpm dpm) (distribution dp-distribution))
  distribution)

(defmethod sample-distribution ((dpm gauss-dpm) (dist dp-gaussian))
  (let ((clusters (dpm-clusters dpm))
	(k (dpm-k dpm))
	(sum (average-of-average dist)))
    (loop for i from 0 below k
	for c across clusters do
	  (incf sum (cluster-center c)))
    (let ((new (normal-random (/ sum (1+ k)) (sqrt (1+ k)))))
      (setf (average-of-average dist) new)
      (setf sum 0d0)
      (loop for i from 0 below k
	  for c across clusters do
	    (incf sum (expt (- (cluster-center c) new) 2)))
      (setf (std-of-average dist) (sqrt (/ sum (chi-square-random k))))
      )))

(defgeneric make-new-cluster (dpm distribution data &optional discarded-cluster)
  (:documentation "make new cluster of passed distribution"))

(defmethod make-new-cluster ((dpm dpm) (distribution dp-distribution) data &optional result)
  (declare (ignore data))
  (or result (make-instance (cluster-class distribution))))

(defmethod make-new-cluster ((dpm gauss-dpm) (distribution dp-gaussian) data &optional result)
  (declare (ignore result))
  (let ((new (call-next-method))
	(ave (normal-random (/ (+ data (average-of-average distribution)) 2)
			    #.(dfloat (sqrt 1/2)))))
    (setf (cluster-center new) ave)
    (setf (cluster-std new) (sqrt (/ (expt (- data ave) 2)
				     (chi-square-random 1))))
    new))

(defgeneric base-distribution (dpm distribution data &rest args &key &allow-other-keys)
  (:documentation "prior of fresh cluster of data in distribution"))

(defmethod base-distribution ((dpm gauss-dpm) (dist dp-gaussian) data &rest args)
  (declare (ignore args))
  (let ((aoa (average-of-average dist))
	(std (std-of-average dist)))
    (/ (safe-exp (/ (expt (- data aoa) 2)
		    -4d0 std std))
       (* std #.(* 2 (sqrt pi))))))

(defgeneric add-to-cluster (cluster data &rest args &key &allow-other-keys)
  (:documentation "add data to cluster"))

(defmethod add-to-cluster ((cluster cluster) data &rest args)
  (declare (ignore data args))
  (incf (cluster-size cluster)))

(defmethod add-to-cluster ((cluster gaussian-cluster) data &rest args)
  (declare (ignore args))
  (call-next-method)
  (incf (cluster-acc cluster) data)
  (vector-push-extend data (cluster-points cluster)))

(defgeneric add-customer (dpm customer old &rest args &key &allow-other-keys)
  (:documentation "add data to model randomly"))

(defmethod add-customer ((dpm dpm) customer old &rest args)
  (let ((p (dpm-p dpm))
	(clusters (dpm-clusters dpm))
	(layers (dpm-cluster-layers dpm))
	(slice (random old))
	(data (point-data customer))
	(dist (dpm-base dpm)))
    (declare (type (array double-float (*)) p)
	     (type vector clusters layers)
	     (type fixnum slice))
    (loop with limit fixnum = (if (zerop slice)
				  (1- (aref layers 0))
				(1- (aref layers (1- slice))))
	with sum double-float = 0d0
	for i from 0 upto limit
	for den double-float = (apply #'density-to-cluster dpm (aref clusters i) data args) do
	  (setf (aref p i) den)
	  (incf sum den)
	finally
	  (when (>= (dpm-hyper dpm) slice)
	    (incf sum (the double-float (apply #'base-distribution dpm dist data args))))
	  (let ((ref (randomize-slice p sum limit)))
	    (declare (type fixnum ref))
	    (when (= ref -1) ;; new cluster
	      (incf (dpm-k dpm))
	      (setf ref (aref layers 0))
	      (let ((new-cluster (make-new-cluster dpm dist data (unless (= ref (length clusters))
							       (aref clusters ref))))) ;; cluster recycling
		(when (= ref (the fixnum (length clusters)))
		  ;; extend array related to tables
		  (vector-push-extend new-cluster clusters)
		  (vector-push-extend 0d0 p))))
	    (let* ((cluster (aref clusters ref))
		   (old (cluster-size cluster))
		   (new-position (aref layers old)))
	      (apply #'add-to-cluster cluster data args)
	      (setf (point-cluster customer) (aref clusters ref))
	      ;;; cluster rotation!!!
	      (rotatef (aref clusters ref)
		       (aref clusters new-position))
	      (incf (aref layers old))
	      (when (= (length layers) (1+ old))
		(vector-push-extend 0 layers))
	      (return cluster))))))

(defmethod add-customer ((dpm logged-dpm) customer old &rest args)
  (let ((p (dpm-p dpm))
	(clusters (dpm-clusters dpm))
	(layers (dpm-cluster-layers dpm))
	(slice (random old))
	(data (point-data customer))
	(dist (dpm-base dpm)))
    (declare (type (array double-float (*)) p)
	     (type vector clusters layers)
	     (type fixnum slice))
    (loop with limit fixnum = (if (zerop slice)
				  (1- (aref layers 0))
				(1- (aref layers (1- slice))))
	with max double-float = most-negative-double-float
	for i from 0 upto limit
	for den double-float = (apply #'density-to-cluster dpm (aref clusters i) data args) do
	  (setf (aref p i) den)
	  (setf max (max max den))
	finally
	  (let ((new 0d0))
	    (declare (type double-float new))
	    (when (>= (dpm-hyper dpm) slice)
	      (setf new (the double-float (apply #'base-distribution dpm dist data args)))
	      (setf max (max max new))
	      (setf new (safe-exp (+ new (- #.(/ (log most-positive-double-float) 2) max)))))
	    (let* ((sum (+ new (jackup-logged-prob p max (1+ limit))))
		   (ref (randomize-slice p sum limit)))
	      (declare (type double-float sum)
		       (type fixnum ref))
	      (when (= ref -1) ;; new cluster
		(incf (dpm-k dpm))
		(setf ref (aref layers 0))
		(let ((new-cluster (make-new-cluster dpm dist data (unless (= ref (length clusters))
								 (aref clusters ref))))) ;; cluster recycling
		  (when (= ref (the fixnum (length clusters)))
		    ;; extend array related to tables
		    (vector-push-extend new-cluster clusters)
		    (vector-push-extend 0d0 p))))
	      (let* ((cluster (aref clusters ref))
		     (old (cluster-size cluster))
		     (new-position (aref layers old)))
		(apply #'add-to-cluster cluster data args)
		(setf (point-cluster customer) (aref clusters ref))
		;;; cluster rotation!!!
		(rotatef (aref clusters ref)
			 (aref clusters new-position))
		(incf (aref layers old))
		(when (= (length layers) (1+ old))
		  (vector-push-extend 0 layers))
		(return cluster)))))))

;; useful utitlity (especially for random initialize)
(defun cluster-rotation (ref clusters layers old-size)
  (let ((new-position (aref layers old-size)))
    (declare (type fixnum new-position))
    (rotatef (aref clusters ref)
	     (aref clusters new-position))
    (incf (aref layers old-size))
    (when (= (length layers) (1+ old-size))
      (vector-push-extend 0 layers))
    new-position))

(defgeneric remove-from-cluster (cluster data &rest args &key &allow-other-keys)
  (:documentation "remove data from cluster"))

(defmethod remove-from-cluster ((cluster cluster) data &rest args)
  (declare (ignore data args))
  (decf (cluster-size cluster)))

(defgeneric remove-customer (dpm customer &rest args &key &allow-other-keys)
  (:documentation "remove data from model"))

(defmethod remove-customer ((dpm dpm) customer &rest args)
  (let* ((clusters (dpm-clusters dpm))
	 (layers (dpm-cluster-layers dpm))
	 (c (point-cluster customer))
	 (old (cluster-size c))
	 (ref (position c clusters :start (aref layers old)))
	 (new (apply #'remove-from-cluster c (point-data customer) args))
	 (new-position (1- (the fixnum (aref layers new)))))
    (when (zerop new)
      (decf (dpm-k dpm)))
    (rotatef (aref clusters ref)
	     (aref clusters new-position))
    (decf (aref layers new))
    old))

(defgeneric initialize (dpm)
  (:documentation "initialize dpm slots and first seating sampling"))

(defgeneric sampling (dpm)
  (:documentation "samlpling seatings and other parameters"))

(defgeneric seatings-samling (dpm)
  (:documentation "sampling all customers seatings"))

(defgeneric parameters-sampling (dpm)
  (:documentation "sampling without seatings but other parameters"))

(defgeneric hypers-sampling (dpm)
  (:documentation "hyperparameter sampling"))

(defmethod initialize ((dpm dpm))
  (loop for point across (shuffle-vector (dpm-data dpm)) do
	(add-customer dpm point 1))
  (parameters-sampling dpm)
  (when (estimate-base? dpm)
    (sample-distribution dpm (dpm-base dpm)))
  (hypers-sampling dpm))

(defmethod sampling ((dpm dpm))
  (seatings-sampling dpm)
  (parameters-sampling dpm)
  (when (estimate-base? dpm)
    (sample-distribution dpm (dpm-base dpm)))
  (hypers-sampling dpm))

(defmethod seatings-sampling ((dpm dpm))
  (loop for point across (shuffle-vector (dpm-data dpm)) do
	(add-customer dpm point (remove-customer dpm point))))

(defmethod parameters-sampling ((dpm dpm))
  (loop with clusters = (dpm-clusters dpm)
      with dist = (dpm-base dpm)
      for i from 0 below (dpm-k dpm)
      for c = (aref clusters i) do
	(sample-cluster-parameters c dist dpm)))

(defmethod hypers-sampling ((dpm dpm))
  (let ((old-hyper (dpm-hyper dpm))
	(k (dpm-k dpm))
	(l (length (dpm-data dpm))))
    (setf (dpm-hyper dpm)
      (gamma-random (- (the double-float (dfloat (+ *hyper-base-a* k)))
		       (the double-float (bernoulli (/ l (+ old-hyper l)))))
		    (- (the double-float *hyper-base-b*)
		       (the double-float (log (beta-random (1+ old-hyper) (dfloat l)))))))))

;;; utility for result
(defmethod make-cluster-result ((dpm dpm))
  (let ((ans (coerce (loop repeat (dpm-k dpm) collect (make-adarray 0)) 'vector))
	(clusters (dpm-clusters dpm))
	(layers (dpm-cluster-layers dpm)))
    (loop for data across (dpm-data dpm)
	for c = (point-cluster data)
	for d = (point-data data)
	for p = (position c clusters :start (aref layers (cluster-size c))) do
	  (vector-push-extend d (aref ans p)))
    ans))

;;; functions for test
(defun make-real-gauss-dataset (mean k num &optional (stds (loop repeat k collect 1d0))
				     (mixtures (loop repeat k collect (dfloat (/ k)))))
  (let ((ans (make-array num))
	(dists (coerce (loop for std in stds collect (list (normal-random mean 10d0) std 0)) 'vector))
	(mix (coerce mixtures 'vector)))
    (loop repeat num
	  for i from 0
	  for dist = (randomize-choice mix 1d0) do
	  (let ((place (aref dists dist)))
	    (incf (caddr place))
	    (setf (aref ans i) (make-point :data (normal-random (car place) (cadr place))))))
    (setf dists (sort dists #'> :key #'caddr))
    (format t "distributions~%")
    (loop for dist across dists do
	  (format t "~S~%" dist))
    ans))

(defun head-clusters (dpm &optional (n most-positive-fixnum))
  (let ((vector (dpm-clusters dpm)))
    (dotimes (i (min n (length vector)))
      (format t "~A:~% ~S~%" i (aref vector i)))
    (format t "~%")))
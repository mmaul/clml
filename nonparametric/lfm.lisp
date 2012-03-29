;; Latent Feature Model using Indian Buffet Process

(defpackage :nonparametric.lfm
;  (:nicknames :lfm)
  (:use :cl :nonpara.stat :hjs.util.meta
	:hjs.util.matrix :hjs.util.vector
	:nonparametric.dpm)
  (:export :ibp
	   :ibp-row
	   :ibp-distribution
	   
	   :lfm
	   
	   :lfm-row
	   :row-weight
	   
	   :lfm-distribution
	   
	   ))

(in-package :nonparametric.lfm)

(defclass ibp (dpm)
  ((base-distribution :initform (make-instance 'ibp-distribution))
   (past-assign :initform (make-hash-table) :accessor ibp-past-assign)))

(defclass ibp-row (cluster) ())

(defclass ibp-distribution (dp-distribution)
  ((cluster-class :initform 'ibp-row)))

(defclass lfm (ibp)
  ((base-distribution :initform (make-instance 'lfm-distribution))
   (feature-ave :initarg :feat-ave :accessor lfm-feature-average)
   (tmp-A :initform nil :accessor lfm-tmp-A)
   (tmp-z :initform nil :accessor lfm-tmp-z)
   (tmp-data :accessor lfm-tmp-data)))

(defclass lfm-row (ibp-row)
  ((weight :accessor row-weight)))

(defclass lfm-distribution (ibp-distribution)
  ((cluster-class :initform 'lfm-row)
   (std-of-data :initform 1d0 :accessor data-std)
   (std-matrix :accessor std-matrix)
   (feature-average :initform 0d0 :accessor feature-average)
   (feature-std :initform 1d0 :accessor feature-std)))

;; assume (point-cluster customer) is adarray
(defmethod add-customer ((dpm ibp) customer N &rest args &key &allow-other-keys)
  (let ((k (dpm-k dpm))	
	(clusters (dpm-clusters dpm))
	(layers (dpm-cluster-layers dpm))
	(data (point-data customer))
	(dist (dpm-base dpm))
	(flip (point-cluster customer))
	(table (ibp-past-assign dpm)))
    (clrhash table)
    (loop until (zerop (fill-pointer flip))
	for c = (vector-pop flip) do
	  (setf (gethash c table) t))
    (loop for i from 0 below k 
	for c = (aref clusters i) do
	  (let ((coin (bernoulli (apply #'density-to-cluster dpm c data :past table :n n args))))
	    (if (zerop coin)
		(setf (gethash c table) nil)
	      (progn
		(setf (gethash c table) t)
		(vector-push-extend c flip))))
	finally
	  (loop for c across flip
	      for old = (cluster-size c)
	      for ref = (position c clusters :start (aref layers old)) do
		(apply #'add-to-cluster c data args)
		(cluster-rotation ref clusters layers old)))
    ;; try to add new row
    (let ((new (sample-new-row dpm customer table n)))
      (dotimes (i new)
	(incf (dpm-k dpm))
	(let* ((ref (aref layers 0))
	       (new-cluster (make-new-cluster dpm dist customer (unless (= ref (length clusters))
							      (aref clusters ref)))))
	  (when (= ref (the fixnum (length clusters)))
	    ;; extend array related to tables
	    (vector-push-extend new-cluster clusters))
	  (apply #'add-to-cluster new-cluster data args)
	  (vector-push-extend new-cluster flip)
	  (when (= (length layers) 1)
	    ;;; first addition
	    (vector-push-extend 0 layers))
	  (incf (aref layers 0)) ;; this is all to do for rotation
	  )))
    flip))

;; passed customer is <struct point> !!
(defmethod make-new-cluster ((dpm lfm) (dist lfm-distribution) customer &optional discarded-cluster)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (ignore discarded-cluster))
  (let* ((new (call-next-method))
	 (pdata (copy-seq (point-data customer)))
	 (clusters (point-cluster customer))
	 (ave (feature-average dist))
	 (std (let ((s (feature-std dist)))
		(sqrt (+ 1d0 (* s s)))))
	 (m (std-matrix dist))
	 (l (length pdata)))
    (declare (type double-float ave std))
    (loop for c across clusters
	for feat = (row-weight c) do
	  (loop for i from 0 below l do
		(decf (aref pdata i) (aref feat i))))
    (loop for i from 0 below l do
	  (setf (aref m i i) std))
    (setf (row-weight new)
      (multivariate-normal-random
       (map-into pdata #'(lambda (x)
			   (declare (type double-float x))
			   (/ (+ x ave) 2d0)) pdata)
       m))
    new
    ))

(defmethod sample-cluster-parameters ((cluster ibp-row) (dist ibp-distribution) (dpm ibp))
  cluster)

;; sample from conditional distribution
(defmethod sample-cluster-parameters ((cluster lfm-row) (dist lfm-distribution) (dpm lfm))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((data (dpm-data dpm))
	 (w (row-weight cluster))
	 (l (length w))
	 (size (cluster-size cluster))
	 (std (std-matrix (dpm-base dpm)))
	 (tmp (lfm-tmp-data dpm)))
    (declare (type fixnum size))
    (fill tmp 0d0)
    (loop for d across data do
	(when (find cluster (point-cluster d))
	  (v+ tmp (point-data d) tmp)
	  (loop for c across (point-cluster d) do
		(unless (eq c cluster)
		  (v- tmp (row-weight c) tmp)))))
    (map-into tmp #'(lambda (x) (declare (type double-float x)) (/ x size)) tmp)
    ;; edit std
    (loop with s double-float = (/ (data-std dist) (sqrt l))
	for i from 0 below l do
	  (setf (aref std i i) s))
    (multivariate-normal-random tmp std w)))

(defun density-to-z (z+ A data std tmp)
  (%multivariate-normal-logged-density (m*v A z+ tmp) std data))

(defun make-z (k &optional result)
  (declare (type fixnum k)
	   (type (or null dvec) result))
  (if (and result (= (the fixnum (length result)) k))
      (fill result 0d0)
    (make-dvec k 0d0)))

(defun make-A (clusters d k &optional result)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type fixnum d k)
	   (type (or null dmat) result))
  (unless (and result (= k (array-dimension result 1)))
    (setf result nil))
  (let ((ans (or result (make-dmat d k))))
    (declare (type dmat ans))
    (loop for i from 0 below k
	for w = (row-weight (aref clusters i)) do
	  (loop for j from 0 below d do
		(setf (aref ans j i) (aref w j))))
    ans))

(defmethod density-to-cluster ((dpm lfm) (cluster lfm-row) data &rest args &key past n)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type dvec data))
  (let* ((k (dpm-k dpm))
	 (clusters (dpm-clusters dpm))
	 (z+ (setf (lfm-tmp-z dpm) (make-z k (lfm-tmp-z dpm))))
	 (A (setf (lfm-tmp-A dpm) (make-A clusters (length data) k (lfm-tmp-A dpm))))
	 (std (std-matrix (dpm-base dpm)))
	 (ref (position cluster clusters :start (aref (dpm-cluster-layers dpm) (cluster-size cluster))))
	 (buffet (/ (cluster-size cluster) N))
	 (tmp (lfm-tmp-data dpm)))
    (declare (type dvec z+))
    ;; edit std
    (loop with s = (/ (data-std (dpm-base dpm)))
	for i from 0 below (length tmp) do
	  (setf (aref std i i) s))
    ;; construct z+
    (loop for i from 0 below k
	for c = (aref clusters i)
	when (or (= i ref) (gethash c past)) do
	  (incf (aref z+ i)))
    (let ((positive (+ (log buffet) (density-to-z z+ A data std tmp)))
	  (negative (+ (log (- 1 buffet))
		       (progn (decf (aref z+ ref)) (density-to-z z+ A data std tmp)))))
      (/ (1+ (the double-float (safe-exp (- negative positive))))))))

(defmethod remove-customer ((dpm ibp) customer &rest args &key &allow-other-keys)
  (let ((clusters (dpm-clusters dpm))
	(layers (dpm-cluster-layers dpm)))
    (loop for c across (point-cluster customer) do
	  (let* ((old (cluster-size c))
		 (ref (position c clusters :start (aref layers old)))
		 (new (apply #'remove-from-cluster c (point-data customer) args))
		 (new-position (1- (the fixnum (aref layers new)))))
	    (when (zerop new)
	      (decf (dpm-k dpm)))
	    (rotatef (aref clusters ref)
		     (aref clusters new-position))
	    (decf (aref layers new))))))

(defparameter *ibp-sample-threshold* 10)

(defun sample-new-row (dpm customer table n)
  (let* ((p (dpm-p dpm))
	 (dist (dpm-base dpm))
	 (alpha (dpm-hyper dpm))
	 (lambda (/ alpha n))
	 (poisson-base (max least-positive-double-float (expt #.(exp 1) (- lambda))))
	 (max most-negative-double-float)
	 (pdata (copy-seq (point-data customer)))
	 (clusters (dpm-clusters dpm))
	 (l (length pdata))
	 (k (dpm-k dpm))
	 (z+ (make-z k (lfm-tmp-z dpm)))
	 (A (make-A clusters l k (lfm-tmp-A dpm)))
	 c-ave)
    (if (zerop k)
	(setf c-ave (make-dvec l 0d0))
      (progn
	(loop for i from 0 below k
	    for c = (aref clusters i)
	    when (gethash c table) do
	      (incf (aref z+ i)))
	(setf c-ave (m*v A z+))))
    (setf (fill-pointer p) 0)
    (let ((den (+ (log poisson-base) (base-distribution dpm dist customer :n 0 :ave c-ave))))
      (vector-push-extend den p)
      (setf max (max max den)))
    (setf poisson-base (max least-positive-double-float (* poisson-base lambda)))
    (loop 
	while (>= poisson-base least-positive-double-float)
	for i from 1 below *ibp-sample-threshold*
	for den = (+ (log poisson-base) (base-distribution dpm dist customer :n i :ave c-ave)) do
	  (vector-push-extend den p)
	  (setf max (max max den))
	  (setf poisson-base (* (/ poisson-base (1+ i)) lambda)))
    (let ((sum (jackup-logged-prob p max)))
      (randomize-choice p sum))))

(defmethod base-distribution ((dpm lfm) (dist lfm-distribution) customer &rest args &key n ave)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type fixnum n)
	   (type dvec ave))
  (let* ((k (dpm-k dpm))
	 (k+ (+ k n)))
    (if (zerop k+)
	most-negative-double-float ;;; should make new cluster
      (let* ((new (lfm-tmp-data dpm))
	     (std (std-matrix dist))
	     (bstd (data-std dist))
	     (fa (feature-average dist))
	     (fs (feature-std dist))
	     (data (point-data customer))
	     (l (length (dpm-data dpm))))
	(declare (type double-float bstd fa fs)
		 (type fixnum l)
		 (type dvec new)
		 (type dmat std))
	(map-into new #'(lambda (x) (+ x (* n fa))) ave)
	(loop with s double-float = (/ (sqrt (+ (* bstd bstd)
					     (* n (* fs fs)))))
	    for i from 0 below l do
	      (setf (aref std i i) s))
	(+ (the double-float (%multivariate-normal-logged-density new std data))
	   (* n #.(log 0.5d0)))))))

(defmethod initialize ((dpm ibp))
  (let ((d (length (point-data (aref (dpm-data dpm) 0))))
	(dist (dpm-base dpm)))
    (setf (lfm-tmp-data dpm) (make-dvec d))
    (setf (std-matrix dist) (diag d (data-std dist))))
  (loop for point across (shuffle-vector (dpm-data dpm))
      for i from 1 do
	(add-customer dpm point i))
  (parameters-sampling dpm)
  (when (estimate-base? dpm)
    (sample-distribution dpm (dpm-base dpm)))
  (hypers-sampling dpm))

(defmethod initialize ((dpm lfm))
  (setf (feature-average (dpm-base dpm)) (lfm-feature-average dpm))
  (call-next-method))

(defmethod seatings-sampling ((dpm ibp))
  (loop
      with n = (length (dpm-data dpm))
      for point across (shuffle-vector (dpm-data dpm)) do
	(remove-customer dpm point)
	(add-customer dpm point n)))

(defun make-feat-data (size features)
  (let ((data (make-array size)))
    (loop for i from 0 below size
	for s = (make-array (length (aref features 0)) :element-type 'double-float :initial-element 0d0) do
	  (loop for feat across features
	      when (zerop (random 2)) do
		(loop for f across feat
		    for j from 0 do
		      (incf (aref s j) f)))
	  (setf (aref data i) s))
    data))

;; hypers-sampling -- test implementation
#+ignore
(defmethod hypers-sampling ((dpm ibp))
  "skip")
;; generic Hierachical Dirichlet Process by direct assignment
(defpackage :nonparametric.hdp
;  (:nicknames :hdp)
  (:use :cl :hjs.util.meta :nonpara.stat :nonparametric.dpm)
  (:export :hdp-cluster
	   :cluster-latent-table
	   :cluster-tmp-table
	   :cluster-beta
	   
	   :hdp
	   :hdp-gamma
	   :hdp-beta
	   
	   :sample-latent-table
	   
	   :hdp-distribution
	   
	   :sliced-hdp
	   ))

(in-package :nonparametric.hdp)

;; clusters have beta and 'latent-table'
(defclass hdp-cluster (cluster)
  ((latent-table :initform 0 :accessor cluster-latent-table)
   (beta :initform (gamma-random 1d0 1d0) :accessor cluster-beta)
   (tmp-table  :initform (make-hash-table) :accessor cluster-tmp-table)))

;; head dpm has only clusters but have beta_new
(defclass hdp (dpm)
  ((gamma :initform (gamma-random *hyper-base-a* *hyper-base-b*) :accessor hdp-gamma)
   (beta_new :initform (gamma-random 1d0 1d0) :accessor hdp-beta)
   (beta-tmp :initform (make-adarray 0 :element-type 'double-float) :accessor hdp-beta-tmp)
   (abm-tmp  :initform (make-adarray 1 :element-type 'double-float) :accessor hdp-abm-tmp)
   (table-p  :initform (make-adarray 1 :element-type 'double-float) :accessor hdp-table-p)
   ))

(defclass hdp-distribution (dp-distribution) 
  ((cluster-class :initform 'hdp-cluster)))

(defmethod add-to-cluster ((cluster hdp-cluster) data &rest args &key franchise)
  (declare (ignore data))
  (apply #'sample-latent-table cluster args)
  (incf (the fixnum (gethash franchise (cluster-tmp-table cluster) 0)))
  (call-next-method))

(defgeneric sample-latent-table (cluster &rest args &key &allow-other-keys)
  (:documentation "sample \"latent\" table for HDP direct assignment"))

(defmethod sample-latent-table ((cluster hdp-cluster) &rest args &key alpha franchise)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float alpha))
  (let ((n (gethash franchise (cluster-tmp-table cluster) 0))
	(ab (* alpha (the double-float (cluster-beta cluster)))))
    (declare (type fixnum n)
	     (type double-float ab))
    (unless (zerop (bernoulli (/ ab (the double-float (+ n ab)))))
      (incf (the fixnum (cluster-latent-table cluster))))))

;; gibbs version
(defmethod add-customer ((dpm hdp) customer old &rest args)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (ignore old))
  (let* ((p (dpm-p dpm))
	 (clusters (dpm-clusters dpm))
	 (layers (dpm-cluster-layers dpm))
	 (data (point-data customer))
	 (dist (dpm-base dpm))
	 (alpha (dpm-hyper dpm))
	 (beta_new (hdp-beta dpm)))
    (declare (type (array double-float (*)) p)
	     (type (vector fixnum) layers)
	     (type vector clusters)
	     (type double-float alpha beta_new))
    (loop with limit fixnum = (1- (aref layers 0))
	with sum double-float = 0d0
	for i fixnum from 0 upto limit
	for c = (aref clusters i)
	for beta double-float = (cluster-beta c)		   
	for den double-float = (* (+ (the fixnum (cluster-size c))
				     (* alpha beta))
				  (the double-float (apply #'density-to-cluster dpm c data args)))
	do
	  (setf (aref p i) den)
	  (incf sum den)
	finally
	  (incf sum (* alpha beta_new (the double-float (apply #'base-distribution dpm dist data args))))
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
	      (declare (type fixnum old new-position))
	      (apply #'add-to-cluster cluster data :alpha alpha args)
	      (setf (point-cluster customer) (aref clusters ref))
	      ;;; cluster rotation!!!
	      (rotatef (aref clusters ref)
		       (aref clusters new-position))
	      (incf (the fixnum (aref layers old)))
	      (when (= (the fixnum (length layers)) (1+ old))
		(vector-push-extend 0 layers))
	      (return cluster))))))

(defmethod hypers-sampling ((dpm hdp))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((clusters (dpm-clusters dpm))
	 (old-hyper (dpm-hyper dpm))
	 (ntables 0))
    (declare (type double-float old-hyper)
	     (type fixnum ntables))
    (loop
	with limit fixnum  = (aref (dpm-cluster-layers dpm) 0)
	for i fixnum from 0 below limit
	for cluster = (aref clusters i)
	for nj__ fixnum = (cluster-size cluster)
	summing (the double-float (log (beta-random (1+ old-hyper) (dfloat nj__)))) into wj double-float
	summing (the double-float (bernoulli (/ nj__ (+ old-hyper nj__)))) into sj double-float
	do (incf ntables (the fixnum (prog1 (cluster-latent-table cluster)
				       (setf (cluster-latent-table cluster) 0)
				       (clrhash (cluster-tmp-table cluster)))))
	finally (setf (dpm-hyper dpm)
		  (gamma-random (- (+ (the double-float *hyper-base-a*) ntables) sj)
				(- (the double-float *hyper-base-b*) wj))))
    (let ((old-gamma (hdp-gamma dpm))
	  (k (dpm-k dpm)))
      (declare (type double-float old-gamma)
	       (type fixnum k))
      (setf (hdp-gamma dpm)
	(gamma-random (- (the double-float (+ (the double-float *hyper-base-a*) k))
			 (the double-float (bernoulli (/ ntables (+ old-gamma ntables)))))
		      (- (the double-float (the double-float *hyper-base-b*))
			 (the double-float (log (beta-random (1+ old-gamma) (dfloat ntables))))))))
    (values (dpm-hyper dpm)
	    (hdp-gamma dpm))))

(defmethod parameters-sampling ((dpm hdp))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((tmp (hdp-beta-tmp dpm))
	 (k (dpm-k dpm))
	 (dist (dpm-base dpm))
	 (clusters (dpm-clusters dpm))
	 (k1 (1+ k)))
    (declare (type fixnum k k1)
	     (type vector clusters)
	     (type (vector double-float) tmp))
    (when (< (the fixnum (array-dimension tmp 0)) k1)
      (adjust-array tmp k1))
    (setf (fill-pointer tmp) k1)
    (loop for i fixnum from 0 below k
	for s = (aref clusters i) do
	  (sample-cluster-parameters s dist dpm)
	  (setf (aref tmp i) (dfloat (cluster-latent-table s))))
    (setf (aref tmp k) (the double-float (hdp-gamma dpm)))
    (dirichlet-random tmp tmp)
    (loop for i fixnum from 0 below k do
	  (setf (cluster-beta (aref clusters i)) (aref tmp i)))
    (setf (hdp-beta dpm) (aref tmp k)))
  (hdp-beta dpm))

(defclass sliced-hdp (hdp) ())

;; slice sampling for HDP
(defmethod add-customer ((dpm sliced-hdp) customer old &rest args)
  (let* ((p (dpm-p dpm))
	 (clusters (dpm-clusters dpm))
	 (slice (random old))
	 (layers (dpm-cluster-layers dpm))
	 (data (point-data customer))
	 (dist (dpm-base dpm))
	 (alpha (dpm-hyper dpm))
	 (beta_new (hdp-beta dpm)))
    (declare (type (array double-float (*)) p)
	     (type vector clusters layers))
    (loop with limit fixnum = ;(if (zerop lslice)
				  (1- (aref layers 0))
				;(1- (aref layers (1- lslice))))
	with sum double-float = 0d0
	for i from 0 upto limit
	for c = (aref clusters i)
	for beta = (cluster-beta c)
	for fx = (+ (the fixnum (cluster-size c))
		    (* alpha beta))
	for den double-float = (if (> fx slice)
				   (apply #'density-to-cluster dpm c data args)
				 0d0)
	do
	  (setf (aref p i) den)
	  (incf sum den)
	finally
	  (when (> (* alpha beta_new) slice)
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
	      (apply #'add-to-cluster cluster data :alpha alpha args)
	      (setf (point-cluster customer) (aref clusters ref))
	      ;;; cluster rotation!!!
	      (rotatef (aref clusters ref)
		       (aref clusters new-position))
	      (incf (aref layers old))
	      (when (= (length layers) (1+ old))
		(vector-push-extend 0 layers))
	      (return cluster))))))

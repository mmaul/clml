;; sample usage of DPM -- multivariate Gaussian mixture
(defpackage :nonparametric.dpm
  (:use :cl :hjs.util.matrix :hjs.util.vector)
  (:export :multivar-gaussian-cluster
	   :multivar-gauss-dpm
	   :multivar-dp-gaussian))

(in-package :nonparametric.dpm)

(defclass multivar-gaussian-cluster (gaussian-cluster)
  ((points :initform (make-array 0 :element-type 'dvec :fill-pointer t :adjustable t))
   (acc :initarg :acc)))

(defclass multivar-gauss-dpm (logged-dpm gauss-dpm)
  ((dimension :initform 2 :initarg :dim :accessor dpm-dim)))

(defclass multivar-dp-gaussian (dp-gaussian)
  ((dimension :initform 2 :initarg :dim :accessor dist-dim)
   (ave-of-std :initarg :aos :accessor average-of-std)))

(defmethod initialize-instance ((instance multivar-gauss-dpm) &rest initargs)
  (declare (ignore initargs))
  (call-next-method)
  (setf (dpm-base instance)
    (make-instance 'multivar-dp-gaussian :dim (dpm-dim instance))))

(defmethod initialize-instance ((instance multivar-dp-gaussian) &rest initargs)
  (declare (ignore initargs))
  (call-next-method)
  (unless (typep (average-of-average instance) 'dvec)
    (setf (average-of-average instance) (make-dvec (dist-dim instance) 0d0)))
  (unless (typep (std-of-average instance) 'dmat)
    (setf (std-of-average instance) (diag (dist-dim instance) 0.1d0)))
  (unless (and (slot-boundp instance 'ave-of-std) (typep (average-of-std instance) 'dmat))
    (setf (average-of-std instance) (diag (dist-dim instance) 1d0))))

(defmethod density-to-cluster ((dpm multivar-gauss-dpm) (cluster multivar-gaussian-cluster) data &rest args)
  (declare (ignore args))
  (with-slots (center std) cluster
    (%multivariate-normal-logged-density center std data)))

(defun matrix-symp (mat)
  (let ((m (array-dimension mat 0))
	(n (array-dimension mat 1)))
    (and (= m n)
	 (loop for i from 0 below m always
	       (loop for j from 0 below i always
		     (= (aref mat i j)
			(aref mat j i)))))))

(defun make-Q0 (trials mean)
  (declare (type vector trials)
	   (type dvec mean))
  (let* ((dim (length (aref trials 0)))
	 (tmp (make-dvec dim))
	 (ans (make-array (list dim dim) :element-type 'double-float :initial-element 0d0)))
    (loop for p across trials do
	  (v- p mean tmp)
	  (loop for i of-type array-index below dim do
		(loop for j of-type array-index below dim do
		      (incf (aref ans i j)
			    (* (aref tmp i)
			       (aref tmp j))))))
    ans))

(define-condition non-positive-definite-error (simple-error) ())

(defmacro try-cho (mat &optional return-from)
  `(handler-case (progn
		   (assert (matrix-symp ,mat))
		   (assert (loop for i from 0 below (array-dimension ,mat 0) always (plusp (aref ,mat i i)))
		       () 'non-positive-definite-error)
		   (cholesky-decomp ,mat))
     (error () (return-from ,return-from))))

(defmethod sample-cluster-parameters ((cluster multivar-gaussian-cluster) (dist multivar-dp-gaussian)
				      (dpm multivar-gauss-dpm))
  (let* ((size (cluster-size cluster))
	 (trials (cluster-points cluster))
	 (acc (cluster-acc cluster))
	 (mean (map-into acc #'(lambda (x) (/ x size)) acc))
	 (dim (dist-dim dist)))
    (when (> (- size 1) dim)
      (let* ((q0 (ignore-errors (make-Q0 trials mean)))
	     (chq0 (try-cho q0 sample-cluster-parameters))
	     (inv-chq0 (m^-1 chq0))
	     (inv-q0 (crossproduct (transpose inv-chq0)))
	     (inv-sqrt-q0 (try-cho inv-q0 sample-cluster-parameters))
	     (df (1- size))
	     (W (LUed-wishart-random df dim))
	     (inv-sqrt-sigma (setf (cluster-std cluster) (M*M inv-sqrt-q0 W))) ;;; kor
	     (inv-sigma (crossproduct inv-sqrt-sigma))
	     (sigma (m^-1 inv-sigma))
	     (sqrt-sigma (try-cho sigma sample-cluster-parameters))
	     (norm-sqrt-sigma (let ((sqdf (sqrt df)))
				(map-matrix-cell!
				 #'(lambda (x) (/ x sqdf))
				 sqrt-sigma))) ;;; kore
	     (rand (multivariate-normal-random (make-dvec dim 0d0) (diag dim 1d0)))
	     (new (m*v norm-sqrt-sigma rand)))
	(setf (cluster-center cluster)
	  (v+ mean new new))))
    (setf (fill-pointer (cluster-points cluster)) 0) ;; must move to parent method
    (fill (cluster-acc cluster) 0d0)
    cluster))

#+ignore
(defmethod sample-cluster-parameters ((cluster multivar-gaussian-cluster) (dist multivar-dp-gaussian)
				      (dpm multivar-gauss-dpm))
  (let* ((size (cluster-size cluster))
	 (trials (cluster-points cluster))
	 (std (cluster-std cluster))
	 (acc (cluster-acc cluster))
	 (mean (map-into acc #'(lambda (x) (/ x size)) acc))
	 (dim (dist-dim dist)))
    (when (> (- size 1) dim)
      (let* ((q0 (prog1 (make-Q0 trials mean) (print "q0!")))
	     (inv-sqrt-q0 (cholesky-decomp (m^-1 q0))
			  #+ignore 
			  (if (zerop (det q0))
			      (return-from sample-cluster-parameters);; abort
			      (m^-1 (transpose (cholesky-decomp q0)))))
	     (df (prog1 (1- size) (print "df!")))
	     (W (LUed-wishart-random df dim))
	     (inv-sqrt-sigma (M*M inv-sqrt-q0 W std)) ;; updated std directly!!
	     (inv-sigma (crossproduct inv-sqrt-sigma W))
	     (sqrt-sigma #+ignore (cholesky-decomp
				   (map-matrix-cell!
				    #'(lambda (x) (/ x df))
				    (m^-1 inv-sigma))
				   inv-sigma)
			 (let ((sqdf (sqrt df)))
			   (map-matrix-cell!
			    #'(lambda (x) (/ x sqdf))
			    (m^-1 (transpose (cholesky-decomp inv-sigma))))))
	     (rand (multivariate-normal-random (make-dvec dim 0d0) (diag dim 1d0)))
	     (new (m*v sqrt-sigma rand)))
	(setf (cluster-center cluster)
	  (v+ mean new new))))
    (setf (fill-pointer (cluster-points cluster)) 0) ;; must move to parent method
    (fill (cluster-acc cluster) 0d0)
    cluster))

(defmethod make-new-cluster ((dpm multivar-gauss-dpm) (distribution multivar-dp-gaussian) data &optional result)
  (let* ((dim (dist-dim distribution))
	 (new (or result (make-instance 'multivar-gaussian-cluster :acc (make-dvec dim 0d0))))
	 (sqrt-sigma (map-matrix-cell!
		      #'(lambda (x) (/ x (sqrt dim)))
		      (lued-wishart-random (1+ dim) dim))))
    (setf (cluster-center new)
      (multivariate-normal-random (map '(simple-array double-float (*))
				    #'(lambda (d aoa) (/ (+ d aoa) 2d0))
				    data
				    (average-of-average distribution))
				  (map-matrix-cell
				   #'(lambda (x) (/ x #.(sqrt 2d0)))
				   (average-of-std distribution))))
    (setf (cluster-std new) sqrt-sigma)
    new))

(defmethod base-distribution ((dpm multivar-gauss-dpm) (dist multivar-dp-gaussian) data &rest args)
  (declare (ignore args))
  (let* ((aoa (average-of-average dist))
	 (inv-sqrt-std (std-of-average dist))
	 (aos (average-of-std dist))
	 (inv-std (crossproduct inv-sqrt-std))
	 (std (m^-1 inv-std))
	 (tmp (m^-1 (crossproduct aos inv-std))))
    (multivariate-normal-logged-density aoa
					(cholesky-decomp (map-matrix-cell!
							   #'(lambda (x) (/ x 2))
							   (mcm std tmp))
							 std)
					data)))

(defmethod add-to-cluster ((cluster multivar-gaussian-cluster) data &rest args)
  (declare (ignore args))
  (incf (cluster-size cluster))
  (v+ (cluster-acc cluster) data (cluster-acc cluster))
  (vector-push-extend data (cluster-points cluster)))

(defmethod print-object ((object multivar-gaussian-cluster) stream)
  (print-unreadable-object (object stream :identity nil)
    (format stream "size: ~S~% ave: ~S~% std: ~S"
	    (cluster-size object) (cluster-center object) (cluster-std object))))

(defmethod sample-distribution ((dpm multivar-gauss-dpm) (dist multivar-dp-gaussian))
  (let* ((clusters (dpm-clusters dpm))
	 (size (dpm-k dpm))
	 (trials (map '(vector dvec) #'(lambda (c) (cluster-center c)) clusters))
	 (std (std-of-average dist))
	 (mean (mean-points trials))
	 (dim (dist-dim dist)))
    (when (> (- size 1) dim)
      (let* ((q0 (make-Q0 trials mean))
	     (inv-sqrt-q0 (if (zerop (det q0))
			      (return-from sample-distribution) ;; abort!!!
			    (cholesky-decomp (m^-1 q0) q0)))
	     (df (1- size))
	     (W (LUed-wishart-random df dim))
	     (inv-sqrt-sigma (M*M inv-sqrt-q0 W std)) ;; updated std directly!!
	     (inv-sigma (crossproduct inv-sqrt-sigma W))
	     (sqrt-sigma (cholesky-decomp
			  (map-matrix-cell!
			   #'(lambda (x) (/ x df))
			   (m^-1 inv-sigma))
			  inv-sigma))
	     (rand (multivariate-normal-random (make-dvec dim 0d0) (diag dim 1d0)))
	     (new (m*v sqrt-sigma rand)))
	(setf (average-of-average dist)
	  (v+ mean new new))))
    dist))


(defun make-multivar-gauss-dataset (dim k num)
  (let ((ans (make-array num))
	(dists (make-array k))
	(zeros (make-dvec dim 0d0))
	(tens (diag dim 10d0)))
    (loop for i from 0 below k do
	  (setf (aref dists i) (list (multivariate-normal-random zeros tens)
				     (transpose (m^-1 (LUed-wishart-random (1+ dim)
									   dim)))
				     0)))
    (loop repeat num for i from 0
	for dist = (random-elt dists)
	for data = (multivariate-normal-random (car dist) (cadr dist)) do
	  (incf (caddr dist))
	  (setf (aref ans i) (make-point :data data)))
    (setf dists (sort dists #'> :key #'caddr))
    (loop for dist across dists do
	  (setf (cadr dist) (transpose (m^-1 (cadr dist)))))
    (format t "distributions~%")
    (loop for dist across dists do
	  (format t "~S~%" dist))
    ans))

(defun bench (dpm)
  (dotimes (i 5)
    (format t "---------- iteration ~A ----------~%" (1+ i))
    (dotimes (j 50)
      (sampling dpm))
    (head-clusters dpm 10)))

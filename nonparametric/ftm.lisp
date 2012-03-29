;; Focused Topic Model
;; Sinead Williamson, et al. The IBP Compound Dirichlet Process and its Application to Focused Topic Modeling

(defpackage :nonparametric.ftm
;  (:nicknames :ftm)
  (:use :cl :nonpara.stat :nonparametric.dpm)
  (:export :ftm-topic
	   :topic-pi
	   :topic-phi
	   
	   :document
	   
	   :ftm
	   :ftm-ibp-alpha	  
	   
	   :ftm-uniform

	   :get-top-n-words
   ))

(in-package :nonparametric.ftm)

(defparameter *smooth-beta* 1d-1)

(defclass ftm-topic (cluster)
  ((topic-pi :accessor topic-pi)
   (topic-phi :accessor topic-phi)
   (dist  :initform (make-hash-table) :accessor cluster-dist-table)
   (ibp-dish :initform (make-hash-table) :accessor topic-ibp-table)
   (emission :initform (make-hash-table) :accessor topic-emission)))

(defclass document ()
  ((id :initarg :id :accessor document-id)
   (words :initarg :words :accessor document-words)
   (thetas :accessor document-thetas)))

(defclass ftm (dpm)
  ;; dpm-hyper assigns gamma in paper
  ((dpm-k :initarg :init-k :initform 1)
   ;; paper assumption: (gamma-random 5d0 0.1d0)
   (dpm-hyper :initform (gamma-random 2d0 10d0))
   (base-distribution :initform (make-instance 'ftm-uniform))
   (word-table :initform (make-hash-table :test #'equal) :accessor word-table)
   (revert-table :accessor revert-table)
   (id :initform -1 :accessor vocabulary)
   (ftm-hyper :initform 5d0 :accessor ftm-ibp-alpha) ;;; initform according to paper
   ))

(defclass ftm-uniform (dp-distribution)
  ((cluster-class :initform 'ftm-topic)))

(defmethod add-to-cluster ((cluster ftm-topic) data &rest args &key franchise)
  (incf (gethash data (topic-emission cluster) 0))
  (incf (the fixnum (gethash franchise (cluster-dist-table cluster) 0)))
  (setf (gethash franchise (topic-ibp-table cluster)) 1d0) ;; safety
  (call-next-method))

(defmethod remove-from-cluster ((cluster ftm-topic) data &rest args &key franchise)
  (assert (>= (decf (gethash data (topic-emission cluster))) 0))
  (decf (gethash franchise (cluster-dist-table cluster)))
  (call-next-method))

;; following 2 methods referenced to paper's appendix
(defmethod density-to-cluster ((dpm ftm) (cluster ftm-topic) data &rest args &key franchise X EY VY)
  #+ignore  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((doc-topic-count (gethash franchise (cluster-dist-table cluster) 0))
	 (word-topic-count (gethash data (topic-emission cluster) 0))
	 (emit-prob (/ (+ word-topic-count *smooth-beta*)
		       (+ (* *smooth-beta* (vocabulary dpm)) (cluster-size cluster))))
	 (doc-words (length (document-words franchise))))
    (if (not (zerop doc-topic-count)) ;; found this topic in this franchise
      ;;; 'case 1'
	(let* ((coef (+ doc-words -1 x ey))
	       (E (+ (/ (* (expt 2d0 (+ x ey))
			   coef))
		     (/ (* vy (+ (* coef coef)
				 (* #.(log 2) coef)
				 2))
			(* (expt 2 (+ x ey))
			   coef)))))
	  (* E (+ (topic-phi cluster) doc-topic-count) emit-prob))
      ;;; 'case 2'
      (let* ((tpi (topic-pi cluster))
	     (tphi (topic-phi cluster))
	     (EY-tmp (- EY (* tphi tpi)))
	     (VY-tmp (- VY (* tphi tphi tpi (- 1 tpi))))
	     (power (expt 2d0 (+ x ey-tmp tphi)))
	     (coef (+ doc-words -1 x ey-tmp tphi))
	     (E (+ (/ 1d0 
		      (* power coef))
		   (/ (* vy-tmp (+ (* coef coef)
				   (* #.(log 2d0) coef)
				   2))
		      (* power coef)))))
	(* E tphi tpi emit-prob))
      )))

;; appendix 'case 3'
(defmethod base-distribution ((dpm ftm) (dist ftm-uniform) data &rest args &key franchise
										X
										EYstar EYdagger
										VYstar VYdagger)
  (declare (ignore data))
  (let* ((doc-words (length (document-words franchise)))
	 (ey (+ EYstar EYdagger))
	 (coef (+ doc-words -1 x ey))
	 (power (expt 2d0 (+ x ey))))
    (/ (+ (/ EYdagger (* power coef))
	  (/ (* vystar eydagger (+ (* coef coef)
				   (* #.(log 2d0) coef)
				   1))
	     (* power coef coef coef))
	  (/ (* vydagger (+ (* (- eydagger #.(log 2d0)) coef coef)
			    EYdagger
			    (* coef (- (* eydagger #.(log 2d0)) 1d0))))
	     (* power coef coef coef)))
       (vocabulary dpm))))

;; gibbs version
(defmethod add-customer ((dpm ftm) customer old &rest args &key franchise)
  (declare (ignore old))
  (let* ((p (dpm-p dpm))
	 (k (dpm-k dpm))
	 (clusters (dpm-clusters dpm))
	 (layers (dpm-cluster-layers dpm))
	 (data (point-data customer))
	 (dist (dpm-base dpm))
	 (gamma (dpm-hyper dpm))
	 (alpha (ftm-ibp-alpha dpm))
	 (X 0d0)
	 (EYstar 0d0)
	 (VYstar 0d0)
	 (EYdagger (* alpha gamma))
	 (VYdagger (- (* (+ gamma 1) gamma alpha)
		      (/ (* alpha alpha gamma gamma)
			 (+ 1d0 (* 2d0 alpha))))))
    (declare (type (array double-float (*)) p)
	     (type (vector fixnum) layers)
	     (type vector clusters)
	     (type double-float gamma alpha X EYstar VYstar EYdagger VYdagger))
    (loop for i from 0 below k
	for c = (aref clusters i) do
	  (let ((tphi (topic-phi c))
		(tpi  (topic-pi  c)))
	    (declare (type double-float tphi tpi))
	    (if (> (gethash franchise (topic-ibp-table c) 0d0) 0d0)
		(progn (incf X tphi))
	      (progn (incf EYstar (* tpi tphi))
		     (incf VYstar (* tpi tphi tphi (- 1d0 tpi)))))))
    (loop
	with limit = (1- (aref layers 0))
	with sum double-float = 0d0
	with EY = (+ EYstar EYdagger)
	with VY = (+ VYstar VYdagger)
	for i fixnum from 0 upto limit
	for c = (aref clusters i)
	for den double-float = (the double-float (apply #'density-to-cluster dpm c data
							:X X :EY EY :VY VY args))
	do
	  (setf (aref p i) den)
	  (incf sum den)
	finally
	  (incf sum (the double-float (apply #'base-distribution dpm dist data
					     :X X :EYstar EYstar :VYstar VYstar
					     :EYdagger EYdagger :VYdagger VYdagger args)))
	  (let ((ref (randomize-slice p sum limit)))
	    (declare (type fixnum ref))
	    (when (= ref -1) ;; new cluster
	      (setf ref (aref layers 0))
	      (let ((new-cluster (make-new-cluster dpm dist data (unless (= ref (length clusters))
							       (aref clusters ref))))) ;; cluster recycling
		(when (= ref (the fixnum (length clusters)))
		  ;; extend array related to tables
		  (vector-push-extend new-cluster clusters)
		  (vector-push-extend 0d0 p)))
	      (incf (dpm-k dpm)))
	    (let* ((cluster (aref clusters ref))
		   (old (cluster-size cluster))
		   (new-position (aref layers old)))
	      (declare (type fixnum old new-position))
	      (apply #'add-to-cluster cluster data args)
	      (setf (point-cluster customer) (aref clusters ref))
	      ;;; cluster rotation!!!
	      (rotatef (aref clusters ref)
		       (aref clusters new-position))
	      (incf (the fixnum (aref layers old)))
	      (when (= (the fixnum (length layers)) (1+ old))
		(vector-push-extend 0 layers))
	      (return cluster))))))

(defmethod make-new-cluster ((dpm ftm) (dist ftm-uniform) data &optional discarded-cluster)
  (declare (ignore data discarded-cluster))
  (let ((new (call-next-method))
	(minpi 1d0))
    (loop 
	with clusters = (dpm-clusters dpm)
	for i from 0 below (dpm-k dpm) do
	  (setf minpi (min minpi (topic-pi (aref clusters i)))))    
    (setf (topic-pi new) (* minpi (beta-random (ftm-ibp-alpha dpm) 1d0)))
    (setf (topic-phi new) (gamma-random (dpm-hyper dpm) 1d0))
    (clrhash (topic-emission new))
    (clrhash (cluster-dist-table new))
    (clrhash (topic-ibp-table new))
    new))

(defmethod ensure-word ((ftm ftm) string)
  (let ((memo (word-table ftm)))
    (multiple-value-bind (oldid found) (gethash string memo)
      (unless found
	(setf oldid (incf (vocabulary ftm)))
	(setf (gethash string memo) oldid))
      (make-point :data oldid))))  

(defmethod parameters-sampling ((dpm ftm))
  (let ((docs (dpm-data dpm))
	(clusters (dpm-clusters dpm))
	(k (dpm-k dpm)))
    ;; sample phi and gamma
    (sample-phi-and-gamma dpm)
    ;; sampling pi
    (loop 
	with m = (length docs)
	for i from 0 below k do
	  (let* ((topic (aref clusters i))
		 (bmk 0d0)
		 (table (topic-ibp-table topic)))
	    (maphash #'(lambda (k v)
			 (declare (ignore k))
			 (incf bmk v)) table)
	    (setf (topic-pi topic) (beta-random bmk (- (+ 1 m) bmk))))) ;;; ?????
    ;; sampling b
    (loop for i from 0 below k do
	  (loop
	      with topic = (aref clusters i)
	      for d across docs do
		(if (zerop (gethash d (cluster-dist-table topic) 0))
		    (let ((ibp-table (topic-ibp-table topic))
			  (tphi (topic-phi topic))
			  (tpi (topic-pi topic)))
		      (setf (gethash d ibp-table)
			(bernoulli (let ((tmp (* (expt 2 tphi) (- 1 tpi))))
				     (/ tpi (+ tpi tmp))))))
		  ;; always active
		  ;; for safety
		  (setf (gethash d (topic-ibp-table topic)) 1d0)
		  )))
    (values)))

(defmethod hypers-sampling ((dpm ftm))
  ;; just sample alpha
  ;; do nothing
  )

(defmethod seatings-sampling ((dpm ftm))
  (loop for doc across (shuffle-vector (dpm-data dpm)) do
	(loop for w across (document-words doc) do
	      (add-customer dpm w
			    (remove-customer dpm w :franchise doc)
			    :franchise doc))))

(defmethod initialize ((dpm ftm))
  (loop for doc across (dpm-data dpm) do
	(loop with words = (document-words doc)
	    for i from 0 below (length words)
	    for w = (ensure-word dpm (aref words i)) do
	      (setf (aref words i) w)))
  (let ((k (dpm-k dpm))
	(clusters (dpm-clusters dpm))
	(dist (dpm-base dpm))
	(layers (dpm-cluster-layers dpm))
	(p (dpm-p dpm)))
    (setf (dpm-k dpm) 0)
    (dotimes (i k)
      (vector-push-extend
       (make-new-cluster dpm dist
			 (point-data (random-elt (document-words (random-elt (dpm-data dpm))))))
       clusters)
      (incf (dpm-k dpm)))
    (when (< (the fixnum (array-dimension p 0)) k)
      (adjust-array p k))
    (setf (fill-pointer p) k)

    (loop for doc across (dpm-data dpm) do
		 (loop for w across (document-words doc) do
		       (let* ((ref (random k))
			      (c (aref clusters ref))
		       (old (cluster-size c)))
			 (add-to-cluster c (point-data w) :franchise doc)
			 (setf (point-cluster w) c)
			 (cluster-rotation ref clusters layers old))))
    (setf (dpm-k dpm) (aref layers 0)))
  (loop for doc across (dpm-data dpm) do
	  (loop for w across (document-words doc) do
		(add-customer dpm w 0 :franchise doc)))
  (parameters-sampling dpm)
  (hypers-sampling dpm))

;; HMC for phi and gamma
(defparameter *hmc-epsilon* 1d-5)
(defparameter *hmc-leapfrog* 10)
(defparameter *hmc-iteration* 10)

(defun sample-phi-and-gamma (dpm)
  (let* ((clusters (dpm-clusters dpm))
	 (k (dpm-k dpm))
	 (pp (make-array (1+ k) :element-type 'double-float))
	 (xx (copy-seq pp))
	 (EE 0d0)
	 (gg (copy-seq xx)))
    (loop for i from 0 below k
	for c = (aref clusters i) do
	  (setf (aref xx i) (log (topic-phi c))))  ;; omega
    (setf (aref xx k) (log (dpm-hyper dpm))) ;; tau
    (setf EE (energy-x xx k clusters))
    (setf gg (grad-x xx k clusters gg))
    (dotimes (iter *hmc-iteration*)
      ;; make pp
      (loop for i from 0 to k do
	    (setf (aref pp i) (normal-random 0d0 1d0)))
      (let* ((HH (+ EE (/ (loop for i from 0 to k summing (* (aref pp i) (aref pp i))) 2)))
	     (xx_new (copy-seq xx))
	     (gg_new (grad-x xx k clusters)))
	(loop for i from 0 to k do	
	      (decf (aref pp i) (* *hmc-epsilon* (/ (aref gg_new i) 2))))
	(dotimes (leap *hmc-leapfrog*)
	  (loop for i from 0 to k do
		(incf (aref xx_new i) (* *hmc-epsilon* (aref pp i))))
	  (setf gg_new (grad-x xx_new k clusters gg_new))
	  (loop for i from 0 to k do
		(decf (aref pp i) (* *hmc-epsilon* (aref gg_new i)))))
	(loop for i from 0 to k do
	      (decf (aref pp i) (* *hmc-epsilon* (/ (aref gg_new i) 2))))
	(let* ((EE_new (energy-x xx_new k clusters))
	       (HH_new (+ EE_new (/ (loop for i from 0 to k summing (* (aref pp i) (aref pp i))) 2)))
	       (dh (safe-exp (- HH HH_new))))
	  (when (or (> dh 0d0) (= (bernoulli dh) 1d0))
	    (setf xx xx_new gg gg_new EE EE_new)))))
    ;; update all
    (loop for i from 0 below k do
	  (setf (topic-phi (aref clusters i)) (safe-exp (aref xx i))))
    (setf (dpm-hyper dpm) (safe-exp (aref xx k))))
  (values))

(defun energy-x (xx k clusters)
#+ignore  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type (array double-float (*)) xx)
	   (type array clusters)
	   (type fixnum k))
  (let ((gamma (safe-exp (aref xx k))))
    (declare (type double-float gamma))
    (loop for i fixnum from 0 below k summing
	  (let* ((topic (aref clusters i))
		 (phi (safe-exp (aref xx i)))
		 (gammatop 0d0)
		 (gammabottom1 0d0)
		 (gammabottom2 0d0)
		 (power 0d0)
		 (dist (cluster-dist-table topic)))
	    (declare (type double-float phi gammatop gammabottom1 gammabottom2 power))
	    (maphash #'(lambda (doc b)
			 (declare (type double-float b))
			 (unless (zerop b)
			   (let ((n (gethash doc dist 0)))
			     (declare (type fixnum n))
			     (incf power (+ phi n))
			     (incf gammatop (the double-float (loggamma (+ phi n))))
			     (incf gammabottom1 (the double-float (loggamma phi)))
			     (incf gammabottom2 (the double-float (loggamma (+ n 1d0)))))))
		     (topic-ibp-table topic))
	    #+ignore
	    ;; author version
	    (+ (* power #.(log 2d0))
	       gammabottom1
	       gammatop
	       (* (- gamma 1d0) (the double-float (log phi)))
	       (- phi))
	    ;; different from author's matlab code
	    (- (+ (* (- gamma 1d0) (the double-float (log phi)))
		  (- phi)
		  gammatop)
	       (+ (the double-float (loggamma gamma))
		  gammabottom1
		  gammabottom2
		  (* power #.(log 2d0)))))
	  )))

(defun grad-x (xx k clusters &optional result)
#+ignore  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type (array double-float (*)) xx)
	   (type array clusters)
	   (type fixnum k))
  (let ((gg (or result (copy-seq xx)))
	(gamma (safe-exp (aref xx k))))
    (declare (type (array double-float (*)) gg)
	     (type double-float gamma))
    (loop for i fixnum from 0 below k do
	  (setf (aref gg i) (the double-float (grad-phi gamma (safe-exp (aref xx i)) (aref clusters i)))))
    (setf (aref gg k)
      #+ignore
      ;; author version
      (* (- gamma) (loop for i fixnum from 0 below k summing (aref xx i)))
      ;; handwrite version
      (- (the double-float (loop for i fixnum from 0 below k summing (aref xx i)))
	 (* k (the double-float (digamma (aref gg k))))))
    gg))

(defun grad-phi (gamma phi topic)
#+ignore  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float gamma phi))
  (let ((ans (+ (/ (- gamma 1d0) phi) 1d0))
	(dist (cluster-dist-table topic)))
    (declare (type double-float ans))
    (maphash #'(lambda (doc b)
		 (declare (type double-float b))
		 (unless (zerop b)
		   (let ((n (gethash doc dist 0)))
		     (declare (type fixnum n))
		     (incf ans (- (the double-float (digamma (+ phi n)))
				  (the double-float (digamma phi))
				  #.(log 2d0))))))
	     (topic-ibp-table topic))
    ans))

;; result utilities
;; copied from hdp-lda
(defmethod assign-theta ((dpm ftm))
  (loop with k = (dpm-k dpm)
      for doc across (dpm-data dpm) do
	(loop with new = (make-array k :initial-element #+ignore (hdp-gamma dpm) *smooth-beta*)
	    for i from 0 below (dpm-k dpm)
	    for topic = (aref (dpm-clusters dpm) i) do
	      (incf (aref new i) (gethash doc (cluster-dist-table topic) 0))
	    finally (setf (document-thetas doc) (normalize! new)))))

(defmethod get-phi ((dpm ftm))
  (let* ((k (dpm-k dpm))
	 (phi (make-array k))
	 (v (vocabulary dpm)))
    (loop
	for i from 0 below k
	for topic = (aref (dpm-clusters dpm) i) do
	  (let ((new (make-array v :element-type 'double-float)))
	    (loop for j from 0 below v do
		  (setf (aref new j) (+ *smooth-beta* (gethash j (topic-emission topic) 0))))
	    (setf (aref phi i) (normalize! new))))
    phi))

(defun revert-word (hdp-lda word-id)
  (unless (slot-boundp hdp-lda 'revert-table)
    (let* ((wt (word-table hdp-lda))
	   (rt (make-array (hash-table-count wt))))
      (maphash #'(lambda (k v)
		   (setf (aref rt v) k)) wt)
      (setf (revert-table hdp-lda) rt)))
  (aref (revert-table hdp-lda) word-id))

(defmethod get-top-n-words ((model ftm) n)
  (let ((phi (get-phi model)))
    (loop for topic across phi
	for i from 0
	for n-best = (get-n-best topic n) do
	  (loop for x across n-best
	      for j from 0 do
		(setf (aref n-best j) (revert-word model x)))
	  (setf (aref phi i) n-best))
    phi))

;; util
(defun show-document-assign (ftm)
  (let ((data (dpm-data ftm))
	(topic (dpm-clusters ftm))
	(k (dpm-k ftm)))
    (map 'vector #'(lambda (doc)
		     (let ((new (make-array k)))
		       (loop for i from 0 below k do
			     (setf (aref new i) (gethash doc (topic-ibp-table (aref topic i)))))
		       new))
	 data)))
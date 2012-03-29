;; blocked HDP-HMM
;; faster version
(defpackage :nonparametric.blocked-hdp-hmm
;  (:nicknames :blocked-hdp-hmm)
  (:use :cl :nonpara.stat :hjs.util.meta
	:nonparametric.dpm 
	:nonparametric.hdp
	:nonparametric.hdp-hmm)
  (:export :blocked-hidden-state
	   :blocked-hdp-hmm
	   :block-uniform
	   
	   :point-sequence
	   :sequence-data
	   :seq-point
	   
	   :sampling-pi
	   
	   :sorted-before
	   :hdp-hmm-l
	   :state-pi))

(in-package :nonparametric.blocked-hdp-hmm)

(defclass blocked-hidden-state (hidden-state)
  ((state-pi :initform (make-adarray 0 :element-type 'double-float) :accessor state-pi)
   (sorted-before :initform (make-adarray 0) :accessor sorted-before)))

(defclass blocked-hdp-hmm (hdp-hmm)
  ((dpm-hyper :initform 1d0)
   (base-distribution :initform (make-instance 'block-uniform))
   (eos-state :initform (make-instance 'blocked-hidden-state))
   (current-state-limit :initarg :L :accessor hdp-hmm-L)
   (l-step :initform 1 :initarg :l-step :accessor inc-l-step)))

(defclass block-uniform (state-uniform) 
  ((cluster-class :initform 'blocked-hidden-state)))

(defclass point-sequence ()
  ((data :initarg :seq :accessor sequence-data)
   (slice :accessor sequence-slice)))

(defclass seq-point (point)
  ((tmp :initform (make-adarray 0 :element-type 'double-float) :accessor point-tmp-p)))

(defmethod initialize-instance ((instance point-sequence) &rest initargs)
  (declare (ignore initargs))
  (call-next-method)
  (setf (sequence-slice instance)
    (make-array (length (sequence-data instance)) :element-type 'double-float :initial-element 0d0))
  instance)

;;; promise
;;; (point-tmp-p (aref data i)) is sorted same as (sorted-before (old-state (aref data (1+ i))))
;;; so density-to-cluster avoid position search
;;;                             and sampling slice be fast too
;;; (old-state (aref data (length data)) defined as (hdp-hmm-eos dpm)
;;; 
;;; when sampling backwards, (old-state (aref data (1+ i))) is disappeared
;;; so copy (old-state (aref data (1+ i))) to (old-state (aref data i)) while forward filtering

(defmethod add-customer ((dpm blocked-hdp-hmm) (seq point-sequence) slice &rest args)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type (simple-array double-float (*)) slice))
  (let ((data (sequence-data seq))
	(L (hdp-hmm-l dpm))
	(layers (dpm-cluster-layers dpm))
	(alpha (dpm-hyper dpm)))
    (declare (type vector data layers)
	     (type fixnum L)
	     (type double-float alpha))
    (macrolet ((old-state (p)
		 `(point-cluster ,p))
	       (adjust-adarray (ad)
		 `(progn
		    (when (< (the fixnum (array-dimension ,ad 0)) l)
		      (adjust-array ,ad l))
		    (setf (fill-pointer ,ad) l)
		    ,ad)))
      ;; initialize position 0
      (let* ((point (aref data 0))
	     (tmp (adjust-adarray (point-tmp-p point)))
	     (old-s (if (= (length data) 1)
			nil
		      (old-state (aref data 1))))
	     (states (if old-s
			 (sorted-before old-s)
		       (dpm-clusters dpm))))
	(declare (type (vector double-float) tmp)
		 (type vector states))
	(apply #'density-to-cluster dpm states (point-data point) :slice (aref slice 0) :ans tmp args)
	(setf (point-cluster point) old-s))
      ;; forward-filtering
      (loop for i fixnum from 1 below (1- (length data)) do
	    (let* ((point (aref data i))
		   (before (point-tmp-p (aref data (1- i))))
		   (before-sorted (sorted-before (old-state point)))
		   (tmp (adjust-adarray (point-tmp-p point)))
		   (old-s (old-state (aref data (1+ i))))
		   (states (sorted-before old-s))
		   (slice (aref slice i)))
	      (declare (type (array double-float (*)) tmp)
		       (type vector states))
	      (apply #'density-to-cluster dpm states (point-data point)
		     :before before :slice slice :ans tmp
		     :before-sorted before-sorted
		     args)
	      (setf (point-cluster point) old-s)))
      ;; tail filtering
      (unless (= (length data) 1)
	(let* ((dl (length data))
	       (point (aref data (1- dl)))
	       (before (point-tmp-p (aref data (- dl 2))))
	       (before-sorted (sorted-before (old-state point)))
	       (tmp (adjust-adarray (point-tmp-p point)))
	       (states (dpm-clusters dpm))
	       (slice (aref slice (1- dl))))
	  (declare (type (vector double-float) tmp)
		   (type vector states))
	  (apply #'density-to-cluster dpm states (point-data point)
		 :before before :slice slice :ans tmp
		 :before-sorted before-sorted
		 args)))
      ;; sample tail
      (let ((p (dpm-p dpm))
	    (tail (1- (the fixnum (length data)))))
	(declare (type (array double-float (*)) p)
		 (type fixnum tail))
	(let* ((point (aref data tail))
	       (tmp (point-tmp-p point))
	       (states (dpm-clusters dpm))
	       (max most-negative-double-float))
	  (declare (type (vector double-float) tmp)
		   (type double-float max)
		   (type vector states))
	    (loop for j fixnum from 0 below L
		for logp double-float across tmp do
		  (setf (aref p j) logp)
		  (setf max (max max logp)))
	    (let* ((sum (jackup-logged-prob p max))
		   (ref (randomize-choice p sum)))
	      (declare (type fixnum ref)
		       (type double-float sum))
	      (setf (point-cluster point) (aref states ref))	      
	      ))
	;; backward-sampling loop
	(loop for i fixnum from (1- tail) downto 0 do
	      (let* ((point (aref data i))
		     (after (aref data (1+ i)))
		     (slice-state (point-cluster point))
		     (states (sorted-before slice-state))
		     (tmp (point-tmp-p point))
		     (after-state (point-cluster after))
		     (slice (aref slice (1+ i)))
		     (max most-negative-double-float)
		     (ref -1))
		(declare (type (vector double-float) tmp)
			 (type double-float max)
			 (type vector states)
			 (type fixnum ref))
		;; calc backward prob
		(if (eq after-state slice-state)
		    ;; quick slice sampler
		    (let ((spi (state-pi slice-state)))
		      (declare (type (vector double-float) spi))
		      (loop for j fixnum from 0 below L
			  for tp double-float = (aref spi j) do
			    (unless (<= slice tp)
			      (setf ref (the fixnum (randomize-slice p (jackup-logged-prob p max j) (1- j))))
			      (return))
			    (let ((logp (+ (the double-float (aref tmp j))
					   (the double-float (log tp)))))
			      (declare (type double-float logp))
			      (setf (aref p j) logp)
			      (setf max (max max logp)))
			  finally
			    (setf ref (the fixnum (randomize-slice p (jackup-logged-prob p max j) (1- j))))))
		  ;; normal but sliced sampler
		  (loop for j fixnum from 0 below L
		      for s = (aref states j)
		      for tp double-float = (trans-prob s after-state) do
			(if (<= slice tp)
			    (let ((logp (+ (the double-float (aref tmp j))
					   (the double-float (log tp)))))
			      (declare (type double-float logp))
			      (setf (aref p j) logp)
			      (setf max (max max logp)))
			  (setf (aref p j) 0d0))
		      finally
			(setf ref (the fixnum (randomize-choice p (jackup-logged-prob p max))))))
		;; update
		(setf (point-cluster point) (aref states ref))
		(let* ((new-state (point-cluster point))
		       (old (cluster-size after-state))
		       (states (dpm-clusters dpm))
		       (new-position (aref layers old))
		       (oldref (position after-state states :start new-position)))
		  (declare (type fixnum old new-position oldref)
			   (type vector states))
		  (when (zerop old)
		    (incf (the fixnum (dpm-k dpm))))
		  (apply #'add-to-cluster after-state (point-data after) :franchise new-state :alpha alpha args)
		  (rotatef (aref states oldref)
			   (aref states new-position))
		  (incf (the fixnum (aref layers old)))
		    (when (= (the fixnum (length layers)) (1+ old))
		      (vector-push-extend 0 layers))
		    )))))
    ;;; final addition
    (let* ((head (aref data 0))
	   (states (dpm-clusters dpm))
	   (s (point-cluster head))
	   (old (cluster-size s))
	   (new-position (aref layers old))
	   (oldref (position s states :start new-position)))
      (declare (type fixnum old new-position)
	       (type vector states))
      (when (zerop old)
	(incf (the fixnum (dpm-k dpm))))
      (apply #'add-to-cluster s (point-data head) :franchise (hdp-hmm-eos dpm) :alpha alpha args)
      ;; rotation
	(rotatef (aref states oldref)
		 (aref states new-position))
	(incf (the fixnum (aref layers old)))
	(when (= (the fixnum (length layers)) (1+ old))
	  (vector-push-extend 0 layers)))
    seq
    ))

(defmethod remove-customer ((dpm blocked-hdp-hmm) (seq point-sequence) &rest args)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  ;; return slice vector
  (let ((slice (sequence-slice seq))
	(data  (sequence-data  seq)))
    (declare (type (simple-array double-float (*)) slice)
	     (type vector data))
    (loop for i fixnum from 0 below (the fixnum (length data))
	for before = (hdp-hmm-eos dpm) then (point-cluster (aref data (1- i))) do
	  (setf (aref slice i)
	    (the double-float (random (apply #'remove-customer dpm (aref data i) :franchise before args)))))
    slice))

(defmethod remove-customer ((dpm blocked-hdp-hmm) (customer seq-point) &rest args &key franchise)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (call-next-method) ;; remove and rotation
  ;; return slice limit
  (let ((s (point-cluster customer)))
    (if (eq franchise (hdp-hmm-eos dpm))
	least-positive-double-float
      (aref (state-pi s) (the fixnum (position franchise (sorted-before s)))))))


(defmethod density-to-cluster ((dpm blocked-hdp-hmm) states data &rest args
			       &key before slice before-sorted ans)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float slice)
	   (type vector before-sorted)
	   (type (vector double-float) ans)
	   (type (or null (vector double-float)) before))
  (if before
      (loop
	  for j fixnum from 0 below (length states)
	  for cluster = (aref states j) do
	    (let ((v (vocabulary dpm))
		  (states (sorted-before cluster))
		  (p (dpm-p dpm))
		  (spi (state-pi cluster))
		  (L (hdp-hmm-l dpm))
		  (max most-negative-double-float))
	      (declare (type fixnum v l)
		       (type double-float max)
		       (type (vector double-float) p spi))
	      (block iter
		(loop for i fixnum from 0 below L
		    for s = (aref states i)
		    for subpi double-float = (aref spi i) do
		      (unless (<= slice subpi)
			;; break
			(setf L i)
			(return))
		      (let ((logp (aref before (the fixnum (position s before-sorted)))))
			(declare (type double-float logp))
			(setf (aref p i) logp)
			(setf max (max max logp))))
		(if (zerop L)
		    ;; no effective transition (over the slice) to this cluster
		    (setf (aref ans j) most-negative-double-float)
		  (loop
		      with jack double-float = (- #.(/ *most-positive-exp-able-float* 2) max)
		      for i fixnum from 0 below L
		      summing (the double-float (safe-exp (+ (aref p i) jack))) into tmp double-float
		      finally (setf (aref ans j)
				(the double-float
				  (- (the double-float
				       (log (* tmp (the double-float (emission-prob cluster data :v v)))))
				     jack))))))))
    (loop
	for j fixnum from 0 below (length states)
	for cluster = (aref states j) do
	  (setf (aref ans j)
	    (the double-float		    
	      (log (the double-float (emission-prob cluster data :v (vocabulary dpm)))))))))

#+ignore
(defmethod density-to-cluster ((dpm blocked-hdp-hmm) (cluster blocked-hidden-state) data &rest args
			       &key before slice before-sorted)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float slice)
	   (type vector before-sorted)
	   (type (or null (vector double-float)) before))
  (if before
      (let ((v (vocabulary dpm))
	    (states (sorted-before cluster))
	    (p (dpm-p dpm))
	    (spi (state-pi cluster))
	    (L (hdp-hmm-l dpm))
	    (max most-negative-double-float))
	(declare (type fixnum v l)
		 (type double-float max)
		 (type (vector double-float) p spi))
	(loop for i fixnum from 0 below L
	    for s = (aref states i)
	    for subpi double-float = (aref spi i) do
	      (unless (<= slice subpi)
		;; break
		(setf L i)
		(return))
	      (let ((logp (aref before (the fixnum (position s before-sorted)))))
		(declare (type double-float logp))
		(setf (aref p i) logp)
		(setf max (max max logp))))
	(when (zerop L)
	  ;; no effective transition (over the slice) to this cluster
	  (return-from density-to-cluster most-negative-double-float))
	(loop
	    with jack double-float = (- #.(/ *most-positive-exp-able-float* 2) max)
	    for i fixnum from 0 below L
	    summing (the double-float (safe-exp (+ (aref p i) jack))) into tmp double-float
	    finally (return (- (the double-float
				 (log (* tmp (the double-float (emission-prob cluster data :v v)))))
			       jack))))
    (log (the double-float (emission-prob cluster data :v (vocabulary dpm)))))
  )

(defmethod trans-prob ((before blocked-hidden-state) (after blocked-hidden-state) &rest args)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (ignore args))
  ;;; just return
  (aref (state-pi after) (the fixnum (position before (sorted-before after)))))


(defmethod initialize ((dpm blocked-hdp-hmm))
  ;; set some start condition
  ;; assume first L is passed
  ;; all arrays have to have length L
  (let ((L (hdp-hmm-L dpm))
	(states (dpm-clusters dpm))
	(dist (dpm-base dpm))
	(tmp (dpm-p dpm)))
    (dotimes (i l)
      (vector-push-extend
       (make-new-cluster dpm dist
			 (point-data (random-elt (sequence-data (random-elt (dpm-data dpm))))))
       states))
    (when (< (the fixnum (array-dimension tmp 0)) l)
      (adjust-array tmp l))
    (setf (fill-pointer tmp) l)
    (loop
	with beta = (dfloat (/ L))
	for i from 0 below L do
	  (setf (cluster-beta (aref states i)) beta))
    (loop for i fixnum from 0 below l
	for s = (aref states i) do
	  (sampling-pi s dpm))
    (let ((memo (make-hash-table :test #'equal)))
      (loop for seq across (dpm-data dpm) do
	    (loop for d across (sequence-data seq) do
		  (setf (gethash (point-data d) memo) t)
		  (setf (point-cluster d) (random-elt states))))
      (setf (vocabulary dpm) (hash-table-count memo)))
    (setf (estimate-base? dpm) nil)) ;; safety
  ;;; first adding
  (let ((data (dpm-data dpm)))
    (loop for seq across data do
	  (add-customer dpm seq (fill (sequence-slice seq) 0d0)))) ;; slice initialize
  (parameters-sampling dpm)
  (hypers-sampling dpm))

(defmethod seatings-sampling ((dpm blocked-hdp-hmm))
  (declare (optimize (speed 3) (safety 0) (debug 0)))  
  (let ((data (dpm-data dpm)))
    (declare (type vector data))
    (loop for i fixnum from 0 below (the fixnum (length data))
	for seq = (aref data i) do
	  (add-customer dpm seq (remove-customer dpm seq)))))

(defmethod parameters-sampling ((dpm blocked-hdp-hmm))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((tmp (dpm-p dpm))
	 (k (dpm-k dpm))
	 (l (hdp-hmm-l dpm))
	 (dist (dpm-base dpm))
	 (clusters (dpm-clusters dpm))
	 (gamma (/ (the double-float (hdp-gamma dpm)) l)))
    (declare (type fixnum l k)
	     (type double-float gamma)
	     (type vector clusters)
	     (type (array double-float (*)) tmp))
    ;; update L
    (when (= l k)
      (let ((new-l (+ l (the fixnum (inc-l-step dpm))))
	    (dummy (point-data (aref (sequence-data (aref (dpm-data dpm) 0)) 0))))
	(declare (type fixnum new-l))
	(setf (hdp-hmm-l dpm) new-l)
	(when (< (the fixnum (array-dimension tmp 0)) new-l)
	  (adjust-array tmp new-l))
	(setf (fill-pointer tmp) new-l)
	(loop for i fixnum from l below new-l do
	      (vector-push-extend (make-new-cluster dpm dist dummy) clusters))
	(setf l new-l)))
    (loop for i fixnum from 0 below l
	for s = (aref clusters i) do
	  (sample-cluster-parameters s dist dpm)
	  (setf (aref tmp i) (the double-float (+ gamma (the double-float (dfloat (cluster-latent-table s)))))))
    (dirichlet-random tmp tmp)
    (loop for i fixnum from 0 below l do
	  (setf (cluster-beta (aref clusters i)) (aref tmp i)))
    (loop for i fixnum from 0 below l
	for s = (aref clusters i) do
	  (sampling-pi s dpm))
    l))

(defmethod sampling-pi ((state blocked-hidden-state) (dpm blocked-hdp-hmm))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((spi (state-pi state))
	(states (dpm-clusters dpm))
	(L (hdp-hmm-L dpm))
	(p (dpm-p dpm))
	(alpha (dpm-hyper dpm))
	(table (cluster-dist-table state))
	(before (sorted-before state)))
    (declare (type hash-table table)
	     (type fixnum L)
	     (type double-float alpha)
	     (type vector before)
	     (type (vector double-float) p spi))
    (when (< (the fixnum (array-dimension before 0)) l)
      (adjust-array before l)
      (adjust-array spi l))
    (setf (fill-pointer before) l)
    (setf (fill-pointer spi) l)
    (loop for i fixnum from 0 below L
	for s = (aref states i) do
	  (setf (aref p i) (max least-positive-double-float
				(the double-float (+ (* alpha (the double-float (cluster-beta s))) ;; this is prior
						     (the fixnum (gethash s table 0))))))
	  (setf (aref before i) s))
    (dirichlet-random p p)
    ;; copy
    (loop for i fixnum from 0 below L do
	  (setf (aref spi i) (aref p i))
	  ;; sort spi
	finally (sort spi #'(lambda (x y) (declare (type double-float x y)) (> x y))))
    ;; sort before
    (sort before #'(lambda (x y) (declare (type fixnum x y)) (< x y))
	  :key #'(lambda (x) (position (aref p (position x states)) spi)))
      ))

(defun make-pattern-seq (pattern times)
  (make-instance 'point-sequence
    :seq (map 'vector
	   #'(lambda (x)
	       (make-instance 'seq-point :data (intern (string-upcase (make-string 1 :initial-element x)) :keyword)))
	      (make-repeat-pattern pattern times))))

(defmethod show-hidden-states ((hdp-hmm blocked-hdp-hmm))
  (map 'vector #'(lambda (y)
		   (map 'vector #'(lambda (x) (position (point-cluster x) (dpm-clusters hdp-hmm))) (sequence-data y)))
       (dpm-data hdp-hmm)))
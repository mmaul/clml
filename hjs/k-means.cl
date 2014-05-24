;;;; Use Elkan's algorithm to avoid excessive distance computation.

(defpackage :hjs.learn.k-means
  (:use :cl :hjs.util.vector :hjs.util.meta :hjs.learn.read-data
	:statistics :hjs.util.matrix :iterate
	:hjs.learn.vars)
  (:nicknames :k-means)
  (:export #:k-means

	   #:make-cluster
	   #:c-center
	   #:c-size
	   #:c-points
	   #:cluster

	   #:pw-points
	   #:pw-clusters

	   #:p-pos
	   #:p-owner
	   #:point
     
     #:get-cluster-centroids
     #:get-cluster-points
	   ))

(in-package :hjs.learn.k-means)

(declaim (optimize (speed 3) (debug 1) (safety 1)))

;;;; global variables
(defparameter *num-of-trials* 10)
(defparameter *distance-function* #'euclid-distance)
(defparameter *k-means-random-state* (make-random-state t))
(defparameter *max-iteration* 1000)

(declaim (type fixnum *num-of-trials* *max-iteration*))

;;;; helper function
(defmacro distance (a b)
  ;;; NOTE: hack, avoid boxings
  `(let ((distance-fn *distance-function*))
     (let ((result
	    (cond ((or (eq #'euclid-distance distance-fn)
		       (eq 'euclid-distance distance-fn))
		   (euclid-distance ,a ,b))
		  ((or (eq #'manhattan-distance distance-fn)
		       (eq 'manhattan-distance distance-fn))
		   (manhattan-distance ,a ,b))
		  ((or (eq #'cosine-distance distance-fn)
		       (eq 'cosine-distance distance-fn))
		   (cosine-distance ,a ,b))
		  (t
		   (error "distance function not recognized: ~a" distance-fn)))))
       (declare (type double-float result))
       result)))

(defun make-random-state-with-seed (seed) ;; make random-state with seed.
  (assert (and (integerp seed) (>= seed 0)))
  #+allegro (make-random-state t (1+ seed))
  #+sbcl (make-random-state (sb-ext:seed-random-state seed))
  #-(or allegro sbcl)
  (error "I don't know how to make random-state by seed in this lisp.~%
If you know it, add line in make-random-state-with-seed.~%
Otherwise, you must use :auto for random-seed."))


;;;; data and type definition
(deftype id () 'fixnum)

(defstruct (cluster (:conc-name c-)
		    (:constructor %make-cluster (id center))
		    (:copier copy-cluster))
  (id -1 :type id)
  (center #.(make-dvec 0) :type dvec)
  (old-center #.(make-dvec 0) :type dvec)
  (size 0 :type fixnum)
  (points nil :type list)
  )

(defun make-cluster (id center)
  (let ((center (coerce center 'dvec)))
    (check-type id id)
    (check-type center dvec)
    (let ((result (%make-cluster id (copy-seq center))))
      (setf (c-old-center result) (copy-seq center))
      result)))

(defstruct (point (:conc-name p-)
		  (:constructor %make-point (id pos))
		  (:copier copy-point))
  (id -1 :type id)
  (pos #.(make-dvec 0) :type dvec)
  (owner nil)                           ; :type cluster
  )

(defun make-point (id pos)
  (let ((pos (coerce pos 'dvec)))
    (check-type id id)
    (check-type pos dvec)
    (%make-point id pos)))

(defstruct (problem-workspace (:conc-name pw-)
			      (:constructor %make-problem-space (points clusters))
            (:print-object print-workspace))
  (points #() :type (simple-array point))
  (clusters #() :type (simple-array cluster))
  (distance-between-clusters #.(make-dmat 0 0) :type dmat)
  (distance-between-point-and-owner #.(make-dvec 0) :type dvec)
  (lower-bounds #.(make-dvec 0) :type dvec)
  )

(defun print-workspace (object stream)
  (print-unreadable-object (object stream :type t :identity t)
    (let ((*print-length* (length (pw-clusters object))))
      (format stream "~D Clusters (ID size): ~S"
              (length (pw-clusters object))
              (loop for clstr across (pw-clusters object) collect `(,(c-id clstr) ,(c-size clstr)))))))

(defmethod get-cluster-centroids ((object problem-workspace))
  (loop for cl across (pw-clusters object) collect (cons (c-id cl) (c-center cl))))

(defmethod get-cluster-points ((object problem-workspace) cid)
  (let ((cluster (find cid (pw-clusters object) :test #'eql :key #'c-id)))
    (when cluster
      (coerce (mapcar #'p-pos (c-points cluster)) 'vector))))

(defun make-problem-space (points clusters)
  (check-type points (simple-array point))
  (check-type clusters (simple-array cluster))
  (let* ((result (%make-problem-space points clusters))
	 (nclusters (length clusters)))
    (declare (type array-index nclusters))
    (setf (pw-distance-between-clusters result)
	  (make-array (list nclusters nclusters)
		      :element-type 'double-float
		      :initial-element most-positive-double-float))
    (setf (pw-distance-between-point-and-owner result)
	  (make-array (list (length points))
		      :element-type 'double-float
		      :initial-element most-positive-double-float))
    (setf (pw-lower-bounds result)
	  (make-dvec nclusters))
    ;; initialize
    (update-lower-bounds result)
    ;; assign initial clusters to pointers
    (let ((distance-between-point-and-owner  (pw-distance-between-point-and-owner result)))
      (do-vec (p points :type point :index-var pid)
	(loop
	   with p-pos of-type dvec = (p-pos p)
	   with min-c of-type cluster = (aref clusters 0)
	   with min-dis of-type double-float = (distance (c-center min-c) p-pos)
	   for i of-type array-index from 1 below nclusters
	   for c of-type cluster = (aref clusters i)
	   do (let ((d (distance (c-center c) p-pos)))
		(declare (double-float d))
		(when (< d min-dis)
		  (setf min-c c)
		  (setf min-dis d)))
	   finally (progn
		     (setf (p-owner p) min-c)
		     (setf (aref distance-between-point-and-owner pid) min-dis))))) 
    result))

;;;; mainbody
;;; find-closest-cluster
(defun find-closest-cluster (point clusters &key (distance-fn *distance-function*))
  (cond ((or (eq 'euclid-distance distance-fn)
	     (eq #'euclid-distance distance-fn))
	 (%find-closest-cluster-euclid-distance point clusters))
	((or (eq 'manhattan-distance distance-fn)
	     (eq #'manhattan-distance distance-fn))
	 (%find-closest-cluster-manhattan-distance point clusters))
	((or (eq 'cosine-distance distance-fn)
	     (eq #'cosine-distance distance-fn))
	 (%find-closest-cluster-cosine-distance point clusters))
	(t
	 (%find-closest-cluster-euclid-distance point clusters))))

(defun %find-closest-cluster-euclid-distance (point clusters)
  (declare (type dvec point)
	   (type (simple-array cluster (*)) clusters))
  (let ((nearest-distance most-positive-double-float)
	(nearest-cluster nil))
    (declare (double-float nearest-distance))
    (dotimes (i (length clusters))
      (declare (fixnum i))
      (let* ((c (aref clusters i))
	     (d (euclid-distance (c-center c) point)))
	(declare (double-float d))
	(when (< d nearest-distance)
	  (setf nearest-distance d)
	  (setf nearest-cluster c))))
    (values nearest-cluster
	    nearest-distance)))

(defun %find-closest-cluster-manhattan-distance (point clusters)
  (declare (type dvec point)
	   (type (simple-array cluster (*)) clusters))
  (let ((nearest-distance most-positive-double-float)
	(nearest-cluster nil))
    (declare (double-float nearest-distance))
    (dotimes (i (length clusters))
      (declare (fixnum i))
      (let* ((c (aref clusters i))
	     (d (manhattan-distance (c-center c) point)))
	(declare (double-float d))
	(when (< d nearest-distance)
	  (setf nearest-distance d)
	  (setf nearest-cluster c))))
    (values nearest-cluster
	    nearest-distance)))

(defun %find-closest-cluster-cosine-distance (point clusters)
  (declare (type dvec point)
	   (type (simple-array cluster (*)) clusters))
  (let ((nearest-distance most-positive-double-float)
	(nearest-cluster nil))
    (declare (double-float nearest-distance))
    (dotimes (i (length clusters))
      (declare (fixnum i))
      (let* ((c (aref clusters i))
	     (d (cosine-distance (c-center c) point)))
	(declare (double-float d))
	(when (< d nearest-distance)
	  (setf nearest-distance d)
	  (setf nearest-cluster c))))
    (values nearest-cluster
	    nearest-distance)))

;;; not useful here.
(define-compiler-macro find-closest-cluster (&whole form point clusters &key (distance-fn *distance-function*))
  (if  (and (listp distance-fn)
	    (or (eq (first distance-fn) 'quote)
		(eq (first distance-fn) 'function)))
       (case (second distance-fn)
	 (euclid-distance
	  `(%find-closest-cluster-euclid-distance ,point ,clusters))
	 (manhattan-distance
	  `(%find-closest-cluster-manhattan-distance ,point ,clusters))
	 (cosine-distance
	  `(%find-closest-cluster-cosine-distance ,point ,clusters))
	 (otherwise
	  form))
       form))



;;;; k-means logic

;;; pick initial points for clusters
;;@ function-type: integer -> #(point) -> #(cluster)
;;@ precondition:
;;@  - points must not be empty,
;;@  - point can't be zero dimension,
;;@  - number of clusters must be positive integer
;;@  - number of clusters must be less than the number of points
;;@ postcondition: 
;;@ depends-on:
;;@  - *distance-function*
(defun pick-initial-clusters (num datapoints &key (distance-fn *distance-function*))
  (declare (ignorable distance-fn))
  (assert (and (vectorp datapoints)
	       (not (zerop (length datapoints))))
	  (datapoints)
	  "Datapoints must not be empty")
  (assert (and (vectorp (aref datapoints 0))
	       (not (equal (array-dimensions (aref datapoints 0)) '(0))))
	  (datapoints)
	  "Point can't be zero dimension")
  (assert (> num 0)
	  (num)
	  "number of clusters must be a positive integer")
  (assert (and (integerp num)
	       (<= num (length datapoints)))
	  (num)
	  "number of clusters must be less than the number of datapoints")
  (%pick-initial-clusters-randomly num datapoints))

;;@ depends-on:
;;@  - *random-state*
(defun %pick-initial-clusters-randomly (num datapoints)
  (iter (generate count from 0 below num)
	(with size = (length datapoints))
	(for n = (random size *k-means-random-state*))
	(when (not (find n selected))
	  (next count)
	  (collect n into selected)
	  (let ((c (make-cluster count (aref datapoints n)))) 
	    (collect c into result result-type vector)))
	(finally (return result))))

(defun update-lower-bounds (problem-workspace)
  (let* ((clusters (pw-clusters problem-workspace))
	 (nclusters (length clusters))
	 (distance-between-clusters (pw-distance-between-clusters problem-workspace))
	 (lower-bounds (pw-lower-bounds problem-workspace)))
    (declare (type dmat distance-between-clusters) 
	     (type dvec lower-bounds)
	     (type array-index nclusters)
	     (optimize speed (safety 0)))
    (assert (= (length lower-bounds) nclusters))
    ;; compute d(c,c') and s(c)
    ;; ref: elkan's paper on k-means using triangle inequality
    (do-vec (c1 clusters :type cluster :index-var ic1)
      (do-vec (c2 clusters :type cluster :index-var ic2)
	(if (= ic1 ic2)
	    (setf (aref distance-between-clusters ic1 ic2)
		  most-positive-double-float) ; avoid mistake even ic1 = ic2
	    (when (> ic1 ic2)
	      (let ((distance (distance (c-center c1) (c-center c2))))
		(setf (aref distance-between-clusters ic1 ic2) distance)
		(setf (aref distance-between-clusters ic2 ic1) distance))))))
    (loop  
       with len of-type array-index = (length lower-bounds)
       for i of-type array-index below len
       do (setf (aref lower-bounds i)
		(let ((min 0d0))	; trick, otherwise ACL will try to box the float...
		  (declare (type double-float min))
		  (loop
		     for j of-type array-index below len
		     minimize (aref distance-between-clusters i j) into result of-type double-float
		     finally (setf min (/ result 2d0)))
		  min)))))


;;; find the cluster once
;;@ function-type: #(cluster) -> #(point) -> #(cluster)
(defmethod trial ((problem-workspace problem-workspace) &key
		  (max-iteration *max-iteration*)
		  debug)
  (assert (and (integerp max-iteration)
	       (> max-iteration 0)))
  (let* ((rest-iteration max-iteration)
	 (clusters (pw-clusters problem-workspace))
	 (points (pw-points problem-workspace))
	 (nclusters (length clusters))
	 (npoints (length points))
	 (distance-between-clusters (pw-distance-between-clusters problem-workspace))
	 (distance-between-point-and-owner (pw-distance-between-point-and-owner problem-workspace))
	 (lower-bounds (pw-lower-bounds problem-workspace))
	 (point-mark (make-array npoints :element-type 'bit :initial-element 0)))
    (declare (type dmat distance-between-clusters)
	     (type array-index nclusters npoints)
	     (type dvec lower-bounds distance-between-point-and-owner)
	     (type (simple-array bit (*)) point-mark)
	     (type simple-vector points clusters)
             (ignorable distance-between-clusters nclusters))
    (check-type clusters simple-vector)
    (assert (> (length clusters) 0))
    (check-type (aref clusters 0) cluster)
    (check-type points simple-vector)
    (assert (> (length points) 0))
    (check-type (aref points 0) point)
    (labels ((stop-p ()
	       (or (<= rest-iteration 0)
		   (loop
		      for c across clusters
		      always (equalp (c-center c) (c-old-center c)))))
	     (next-iter ()
	       ;; update lower-bounds
	       (update-lower-bounds problem-workspace)
	       ;; select possible points for update
	       (do-vec (p points :type point :index-var pid)
		 ;; mark all candidate point
		 (if (> (aref distance-between-point-and-owner pid)
			(aref lower-bounds (the array-index (c-id (p-owner p)))))
		     (setf (aref point-mark pid) 1)
		     (setf (aref point-mark pid) 0)))
	       ;; update center
	       (do-vec (markedp point-mark :type bit :index-var pid)
		 (when (= markedp 1)
		   (let* ((p (aref points pid)) 
			  (distance-to-owner (aref distance-between-point-and-owner pid)))
		     (declare (type double-float distance-to-owner))
		     (do-vec (c clusters :type cluster)
		       (let ((d (distance (c-center c) (p-pos p))))
			 (declare (type double-float d))
			 (when (< d distance-to-owner)
			   (setf (p-owner p) c)))))))
	       ;; update centers and save old-centers
	       (do-vec (c clusters :type cluster)
		 (replace (c-old-center c) (c-center c))
		 (fill-vec (c-center c) 0d0)
		 (setf (c-size c) 0))
	       (do-vec (p points :type point)
		 (let* ((c (p-owner p)))
		   (v+ (c-center c) (p-pos p) (c-center c))
		   (incf (c-size c))))
	       (do-vec (c clusters :type cluster)
		 (when (not (zerop (c-size c)))
		   (v-scale (c-center c) (the double-float (/ 1d0 (c-size c))) (c-center c))))
	       ;; update distance-between-point-and-owner
	       (do-vec (p points :type point :index-var pid)
		 (setf (aref distance-between-point-and-owner pid)
		       (distance (p-pos p) (c-center (p-owner p)))))
	       ;; and reduce the iteration remained
	       (decf rest-iteration)))
      ;; main loop 
      (loop
	 do (next-iter)
	 until (stop-p))
      ;; debug output
      (when debug
	(format t "~&Iter: ~A, Total Distance: ~A~%"
		(- max-iteration rest-iteration)
		(reduce #'+ (pw-distance-between-point-and-owner problem-workspace))))
      ;; return value
      problem-workspace)))

(defun pw-update-clusters (pw)
  (iter (for c in-sequence (pw-clusters pw))
	(setf (c-points c) nil))
  (iter (for p in-sequence (pw-points pw))
	(push p (c-points (p-owner p))))
  pw)

;;; main solver
;; (defgeneric k-means (k dataset &key distance-fn max-iteration num-of-trials random-state))
(defmethod k-means ((k integer) (dataset numeric-dataset) &key
		    (distance-fn *distance-function*)
		    standardization
		    (max-iteration *max-iteration*)
		    (num-of-trials *num-of-trials*)
		    (random-state *k-means-random-state*)
		    debug)
  (assert (and (integerp max-iteration)
	       (> max-iteration 0)))
  (assert (> k 0))
  (assert (>= (length (dataset-numeric-points dataset)) k))
  (let ((*distance-function* distance-fn)
	(*max-iteration* max-iteration)
	(*num-of-trials* num-of-trials)
	(*k-means-random-state* random-state))
    (let* ((orig-data (dataset-numeric-points dataset))
	   (data (if (not standardization)
		     orig-data
		     (standardize orig-data)))
	   (table (when standardization
		    (let ((table (make-hash-table :size (length data))))
		      (declare (type (simple-array dvec (*)) orig-data data))
		      (do-vecs ((orig orig-data :type dvec)
				(new data :type dvec))
			(setf (gethash new table) orig))
		      table)))
	   (minimal-distance-sum most-positive-double-float)
	   (best-result))
      (loop
	 repeat num-of-trials
	 do (let* ((clusters (pick-initial-clusters k data))
		   (points (let ((id-count -1))
			     (map 'vector (lambda (d) (make-point (incf id-count) d)) data))) 
		   (problem-workspace (make-problem-space points clusters)))
	      (declare (type simple-vector clusters points))
	      ;; trial
	      (trial problem-workspace :debug debug)
	      ;; record the best trial
	      (let ((distance-sum (reduce #'+ (pw-distance-between-point-and-owner problem-workspace))))
		(when (< distance-sum minimal-distance-sum)
		  (setf minimal-distance-sum distance-sum)
		  (setf best-result problem-workspace)))))
      ;; return value
      (pw-update-clusters best-result)
      (values best-result
	      table))))

#+ignore
(defun k-means-hss 
    (infile outdatafile outclusterfile label-strings cluster-num
     &key
     (infile-type :csv)
     (distance :euclid)
     (max-iter *max-iteration*)
     (num-of-trials *num-of-trials*)
     (random-seed :auto)
     (normalize nil)
     (external-format *ml-default-external-format*)
     csv-type-spec
     (csv-header-p t)
     (line-id-pos 0)	;; NOTE: column 0 is filename
     data-types
     debug)
  (declare (optimize safety (speed 0))
           (ignorable line-id-pos))
  ;; FIXME:
  (when normalize
    (error "Currently normalize is not correctly implemented."))
  (let ((distance-fn (case distance
                       ((nil :euclid)
                        #'euclid-distance)
                       (:manhattan
                        #'manhattan-distance)
                       (:cosine
                        #'cosine-distance)))
        (random-state (if (eq random-seed :auto)
                          *k-means-random-state*
                        (make-random-state-with-seed random-seed))))
    (let* ((orig (read-data-from-file infile
                                      :type infile-type
                                      :external-format external-format
                                      :csv-type-spec csv-type-spec
                                      :csv-header-p csv-header-p))
           (dims (dataset-dimensions orig))
           (range (mapcar (lambda (label)
                            (position label dims :test #'string-equal :key #'dimension-name))
                          label-strings))
           (data-types (if data-types
                           data-types
                         (make-list (length range) :initial-element :numeric)))
           (dataset (pick-and-specialize-data orig :range range :data-types data-types)))
      (multiple-value-bind (best-result table)
          (k-means cluster-num dataset
                   :distance-fn distance-fn
                   :standardization normalize
                   :max-iteration max-iter
                   :num-of-trials num-of-trials
                   :random-state random-state
                   :debug debug)
        (declare (ignorable table))
        ;; csv output
        ;; TODO: need clean up and speed up
        ;; FIXME: for standardized result
        (let* ((points (pw-points best-result))
               (clusters (pw-clusters best-result))
               (distance (pw-distance-between-point-and-owner best-result)))
          ;; clusters
          (when outclusterfile
            (let ((csv-records
                   (list* (coerce
                           (append '("id" "size")
                                   (map 'list #'dimension-name (dataset-dimensions dataset))
                                   '("residual"))
                           'vector)
                          (map 'list
                            (lambda (c)
                              (append (list (1+ (c-id c)) (c-size c))
                                      (coerce (c-center c) 'list)
                                      (list (aref distance (c-id c)))))
                            clusters))))
              (assert (every (lambda (l) (= (length l) (length (first csv-records)))) csv-records)
                  nil
                "The csv record is malformed.")
              (csv:write-csv-file outclusterfile
                                  csv-records
                                  :external-format external-format)))
          ;; points
          (when outdatafile
            (let* ((orig-points (dataset-points orig))
                   (csv-records
                    (list* (coerce
                            `(,@(map 'list #'dimension-name (dataset-dimensions orig))
                                "ClusterID")
                            'vector)
                           ;; NOTE: depends on the fact that points and data-point has same order
                           (iter (for (the array-index i) below (length points))
                                 (for orig-point = (aref orig-points i))
                                 (for point = (aref points i))
                                 (collect
                                  (coerce
                                   `(,@(coerce orig-point 'list)
                                       ,(1+ (c-id (p-owner point))))
                                   'vector))))))
              #+ignore
              (assert (every (lambda (l) (= (length l) (length (first csv-records)))) csv-records)
                  nil
                "The csv record is malformed.")
              (csv:write-csv-file outdatafile
                                  csv-records
                                  :external-format external-format))))
        (values best-result table)))))

(defpackage :learn.nearest
  (:use :cl
	:hjs.learn.read-data
	:hjs.util.vector
	:hjs.util.meta
	:nonpara.stat
	:hjs.util.matrix
	:pca
	:priority-que)
  (:export :nearest-search
	   
	   :exact-nearest-search
	   :stochastic-nearest-search
	   
	   :naive-nearest-search
	   
	   :kd-tree-search
	   
	   :m-tree-search
	   
	   :locality-sensitive-hashing
	   :p-stable-locality-sensitive-hashing
	   :euclid-locality-sensitive-hashing
	   :manhattan-locality-sensitive-hashing
	   :cosine-locality-sensitive-hashing
	   
	   :nns-input-data
	   :nns-input-key
	   :nns-distance
	   
	   :initialize-search
	   
	   :find-nearest
	   :find-nearest-k
	   :find-nearest-epsilon
	   
	   :stochastic-validation
	   ))

(in-package :learn.nearest)

(defclass nearest-search ()
  ((input-data :initarg :input-data :accessor nns-input-data)
   (input-key  :initarg :input-key  :accessor nns-input-key :initform #'identity)
   (distance   :initarg :distance   :accessor nns-distance  :initform #'euclid-distance)))

(defclass exact-nearest-search (nearest-search) ())
(defclass stochastic-nearest-search (nearest-search) ())

(defclass naive-nearest-search (exact-nearest-search) ())

(defclass kd-tree-search (exact-nearest-search)
  ((root-node :accessor root-node)
   (compare-v :accessor compare-v)
   (upper-bounds :initarg :upper-bounds :initform nil :accessor upper-bounds)
   (lower-bounds :initarg :lower-bounds :initform nil :accessor lower-bounds)))

(defmethod initialize-instance ((instance kd-tree-search) &rest args)
  (declare (ignore args))
  (call-next-method)
  (with-slots (compare-v input-data input-key) instance
    (setf compare-v (make-array (length (funcall input-key (aref input-data 0))) :element-type 'double-float))))

(defclass m-tree-search (exact-nearest-search)
  ((root-node :accessor root-node)
   (m         :initarg :m :accessor m-tree-size)
   (pivot     :initarg :pivot)
   (priority-queue :initarg :priority-queue :initform :binomial)))

(defmethod initialize-instance ((instance m-tree-search) &rest args)
  (declare (ignore args))
  (call-next-method)
  (with-slots (root-node m pivot) instance
    (setf root-node (make-m-tree-node :pivot pivot :children (make-array m :fill-pointer 0)))))

(defclass locality-sensitive-hashing (stochastic-nearest-search)
  ((hash-length :initarg :L :accessor hash-length)
   (hash-bit :initarg :k :accessor hash-bit)
   (hash-fns :accessor hash-fns)
   (candidates :initform (make-array 0 :fill-pointer t :adjustable t) :accessor candidates)))

(defmethod initialize-instance ((instance locality-sensitive-hashing) &rest args)
  (declare (ignore args))
  (call-next-method)
  (with-slots (hash-length hash-fns) instance
    (setf hash-fns (make-array hash-length)))
  instance)

(defgeneric find-nearest (nearest-search data)
  (:documentation "find a nearest point from data"))

(defgeneric find-nearest-k (nearest-search data k &optional result tmp-distances)
  (:documentation "find nearest k points from data"))

(defmethod find-nearest-k :around ((nearest-search nearest-search) data k &optional result tmp)
  (unless result
    (setf result (make-array k)))
  (unless tmp
    (setf tmp (make-array k :element-type 'double-float)))
  (fill result nil)
  (fill tmp most-positive-double-float)
  (call-next-method nearest-search data k result tmp))

(defgeneric find-nearest-epsilon (nearest-search data epsilon &optional result)
  (:documentation "find points within distance epsilon from data"))

(defmethod find-nearest-epsilon :around ((nearest-search nearest-search) data epsilon &optional result)
  (unless result
    (setf result (make-array 0 :fill-pointer t :adjustable t)))
  (setf (fill-pointer result) 0)
  (call-next-method nearest-search data epsilon result))

(defgeneric initialize-search (nearest-search)
  (:documentation "learn or construct data structure of nearest-search"))

(defmethod initialize-search ((search nearest-search))
  search)

(defmethod initialize-instance :after ((instance nearest-search) &rest initargs)
  (declare (ignore initargs))
  (initialize-search instance))

(defun nearest (candidates from distance &optional (key #'identity))
  (loop
      with n = nil
      with nd = most-positive-double-float
      for p across candidates
      for d = (funcall distance from (funcall key p))
      when (< d nd) do
	(setf nd d)
	(setf n p)
      finally (return (values n nd))))

(defun nearest-k (candidates from distance &optional (key #'identity) result tmp)
  (loop
      for p across candidates
      for d = (funcall distance from (funcall key p)) do
	(let ((flag nil))
	  (do-vecs ((_ tmp :setf-var td :type double-float)
		    (__ result :setf-var tp :type t))
	    (declare (ignorable _ __))
	    (when (or flag (< d td))
	      (setf flag t)
	      (rotatef d td)
	      (rotatef p tp))
	    (unless p			; if no next-point, update stop.
	      (return))))
      finally (return (values result tmp))))

(defun nearest-epsilon (candidates from distance epsilon &optional (key #'identity) result)
  (loop
      for p across candidates
      for d = (funcall distance from (funcall key p))
      when (<= d epsilon) do	
	(vector-push-extend p result) ;into res
      finally (return result)))

;; naive
(defmethod find-nearest ((search naive-nearest-search) data)
  (with-slots (input-data input-key distance) search
    (nearest input-data (funcall input-key data) distance input-key)))

(defmethod find-nearest-k ((search naive-nearest-search) data k &optional result tmp)
  (declare (ignore k))
  (with-slots (input-data input-key distance) search
    (nearest-k input-data (funcall input-key data) distance input-key result tmp)))

(defmethod find-nearest-epsilon ((search naive-nearest-search) data epsilon &optional result)
  (with-slots (input-data input-key distance) search
    (nearest-epsilon input-data (funcall input-key data) distance epsilon input-key result)))

;; Sliding-midpoint split kd-tree
;; "It’s okay to be skinny, if your friends are fat", Songrit Maneewongvatana and David M. Mount, December 18, 1999
(defstruct kd-node left right dim pivot lower upper)

(defmethod initialize-search ((search kd-tree-search))
  (with-slots (input-data input-key root-node upper-bounds lower-bounds) search
    (let ((k (length (funcall input-key (aref input-data 0)))))
      (unless upper-bounds
	(setf upper-bounds (make-dvec k most-negative-double-float))
	(loop for d across input-data
	    for v = (funcall input-key d) do
	      (do-vecs ((x v :type double-float)
			(_ upper-bounds :type double-float :setf-var u))
		(declare (ignorable _))
		(setf u (max u x)))))
      (unless lower-bounds
	(setf lower-bounds (make-dvec k most-positive-double-float))
	(loop for d across input-data
	    for v = (funcall input-key d) do
	      (do-vecs ((x v :type double-float)
			(_ lower-bounds :type double-float :setf-var l))
		(declare (ignorable _))
		(setf l (min l x)))))      
      (setf root-node (make-kd-tree input-data input-key lower-bounds upper-bounds))))
  search)

(defun make-kd-tree (vector key lower-bounds upper-bounds &optional (dims (make-array (length lower-bounds))))
  (if (= (length vector) 1)
      ;; leaf
      (make-kd-node :dim nil :pivot vector :lower lower-bounds :upper upper-bounds)
    (let (left-data right-data)
      (do-vecs ((u upper-bounds :type double-float :index-var i)
		(l lower-bounds :type double-float)
		(_ dims         :type t :setf-var d))
	(declare (ignorable _))
	(setf d (cons (- u l) i)))
      (sort dims #'> :key #'car)
      (loop for pair across dims
	  for b   = (car pair)
	  for dim = (cdr pair) do
	    ;; midopint split
	    (let ((pivot (+ (/ b 2) (aref lower-bounds dim))))
	      (declare (type double-float pivot))
	      (flet ((divide! ()
		       (setf left-data (remove-if-not #'(lambda (x) (< x pivot)) vector
						      :key #'(lambda (p) (aref (funcall key p) dim))))
		       (setf right-data (remove-if-not #'(lambda (x) (>= x pivot)) vector
						       :key #'(lambda (p) (aref (funcall key p) dim))))))
		(divide!)
		;; sliding
		(when (zerop (length left-data))
		  (loop
		      with min = most-positive-double-float
		      with second = most-positive-double-float
		      for p across vector
		      for elt = (aref (funcall key p) dim) do
			(cond ((< elt min)
			       (setf second min)
			       (setf min elt))
			      ((= elt min)
			       nil)
			      ((< elt second)
			       (setf second elt)))
		      finally 
			(setf pivot second)
			(divide!)))
		(when (zerop (length right-data))
		  (loop
		      with max = most-negative-double-float
		      for p across vector
		      for elt = (aref (funcall key p) dim)
		      when (> elt max) do
			(setf max elt)
		      finally 
			(setf pivot max)
			(divide!)))
		(when (and (not (zerop (length left-data)))
			   (not (zerop (length right-data))))
		  ;; divide success
		  (return (make-kd-node :dim dim :pivot pivot
					:left (make-kd-tree left-data
							    key
					lower-bounds
					(let ((new (copy-seq upper-bounds)))
					  (setf (aref new dim) pivot)
					  new))
					:right (make-kd-tree right-data
							     key
							     (let ((new (copy-seq lower-bounds)))
							       (setf (aref new dim) pivot)
							       new)
							     upper-bounds)
					:lower lower-bounds
					:upper upper-bounds)))))
	  finally
	    ;; all data are equal!!! -- same to leaf
	    (return (make-kd-node :dim nil :pivot vector :lower lower-bounds :upper upper-bounds))))))

(defmethod find-nearest ((search kd-tree-search) data)
  (with-slots (root-node distance input-key compare-v) search
    (let ((d (funcall input-key data)))
      (kd-nearest root-node d distance compare-v input-key))))

(defmethod find-nearest-k ((search kd-tree-search) data k &optional result tmp)
  (declare (ignore k))
  (with-slots (root-node distance input-key compare-v) search
    (let ((d (funcall input-key data)))
      (kd-nearest-k root-node d distance compare-v result tmp input-key))))

(defmethod find-nearest-epsilon ((search kd-tree-search) data epsilon &optional result)
  (with-slots (root-node input-key distance compare-v) search
    (kd-nearest-epsilon root-node (funcall input-key data) distance compare-v epsilon result input-key)))

(defun nearest-vertex (data lower upper result)
  (do-vecs ((x data :type t)
	    (l lower :type double-float)
	    (u upper :type double-float)
	    (_ result :type double-float :setf-var ans))
    (declare (ignorable _))
    (cond ((>= u x l)
	   (setf ans x))
	  ((< x l)
	   (setf ans l))
	  ((> x u)
	   (setf ans u))))
  result)

(defun kd-nearest (node x distance-fn compare &optional (key #'identity)
							tmp-point
							(tmp-distance most-positive-double-float))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((dim (kd-node-dim node)))
    (if (not dim)
	;; leaf
	(let* ((v (kd-node-pivot node))
	       (p (aref v 0))
	       (dis (funcall distance-fn x (funcall key p))))
	  (declare (type double-float dis))
	  (if (< dis tmp-distance)
	      (values p dis)
	    (values tmp-point tmp-distance)))
      (let ((pivot (kd-node-pivot node))
	    (elt (aref x dim))
	    (vertex (nearest-vertex x (kd-node-lower node) (kd-node-upper node) compare))
	    search-side
	    other-side)
	(declare (type double-float elt pivot))
	(when (> (funcall distance-fn x vertex) tmp-distance)
	  ;; cut branch
	  (return-from kd-nearest (values tmp-point tmp-distance)))
	(if (< elt pivot)
	    (setf search-side (kd-node-left node) other-side (kd-node-right node))
	  (setf search-side (kd-node-right node) other-side (kd-node-left node)))
	(multiple-value-bind (tmp-p tmp-d) (kd-nearest search-side x distance-fn compare key tmp-point tmp-distance)
	  (kd-nearest other-side x distance-fn compare key tmp-p tmp-d))))))

(defun kd-nearest-k (node x distance-fn compare result tmp &optional (key #'identity))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((dim (kd-node-dim node)))
    (if (not dim)
	;; leaf
	(let* ((v (kd-node-pivot node))
	       (p (aref v 0))
	       (dis (funcall distance-fn x (funcall key p))))
	  (declare (type double-float dis))
	  (loop for p across v
	      for flag = nil do
		(do-vecs ((_ tmp :setf-var td :type double-float)
			  (__ result :setf-var tp :type t))
		  (declare (ignorable _ __))
		  (when (or flag (< dis td))
		    (setf flag t)
		    (rotatef dis td)
		    (rotatef p tp))
		  (unless p		; if no next-point, update stop.
		    (return))))
	  (values result tmp))
      (let ((pivot (kd-node-pivot node))
	    (elt (aref x dim))
	    (vertex (nearest-vertex x (kd-node-lower node) (kd-node-upper node) compare))
	    search-side
	    other-side)
	(declare (type double-float elt pivot))
	(when (> (the double-float (funcall distance-fn x vertex)) (the double-float (aref tmp (1- (length tmp)))))
	  ;; cut branch
	  (return-from kd-nearest-k (values result tmp)))
	(if (< elt pivot)
	    (setf search-side (kd-node-left node) other-side (kd-node-right node))
	  (setf search-side (kd-node-right node) other-side (kd-node-left node)))
	(multiple-value-bind (tmp-p tmp-d) (kd-nearest-k search-side x distance-fn compare result tmp key)
	  (kd-nearest-k other-side x distance-fn compare tmp-p tmp-d key))))))

(defun kd-nearest-epsilon (node x distance-fn compare epsilon result &optional (key #'identity))
  (declare (optimize (speed 3) (debug 0) (safety 0))
	   (type double-float epsilon))
  (let ((dim (kd-node-dim node)))
    (if (not dim)
	;; leaf
	(let* ((v (kd-node-pivot node))
	       (p (aref v 0))
	       (dis (funcall distance-fn x (funcall key p))))
	  (declare (type double-float dis))
	  (if (<= dis epsilon)
	      (loop for p across v do
		    (vector-push-extend p result)
		  finally (return result))
	    result))
      (let ((pivot (kd-node-pivot node))
	    (elt (aref x dim))
	    search-side
	    other-side)
	(declare (type double-float elt pivot))
	(if (< elt pivot)
	    (setf search-side (kd-node-left node) other-side (kd-node-right node))
	  (setf search-side (kd-node-right node) other-side (kd-node-left node)))
	(let ((result (kd-nearest-epsilon search-side x distance-fn compare epsilon result key)))
	  (let ((diff (funcall distance-fn x
			       (nearest-vertex x (kd-node-lower other-side) (kd-node-upper other-side) compare))))
	    (declare (type double-float diff))
	    (if (> diff epsilon)
	      result
	    (kd-nearest-epsilon other-side x distance-fn compare epsilon
				result
				key))))))))

;; M-tree
;; pivot of root node is a passed parameter!
(defstruct m-tree-node pivot parent (radius 0d0) d-parent children)

(defun m-tree-leaf-p (node)
  (not (typep (aref (m-tree-node-children node) 0) 'm-tree-node)))

(defmethod initialize-search ((search m-tree-search))
  (with-slots (input-data input-key distance root-node m) search
    (loop for data across input-data do
	  (insert-m-tree root-node data m input-key distance)))
  search)

(defmethod find-nearest ((search m-tree-search) data)
  (with-slots (root-node input-key distance priority-queue) search
    (m-nearest root-node (funcall input-key data) distance input-key priority-queue)))

(defmethod find-nearest-k ((search m-tree-search) data k &optional result tmp)
  (declare (ignore k))
  (with-slots (root-node input-key distance priority-queue) search
    (m-nearest-k root-node (funcall input-key data) distance result tmp input-key priority-queue)))

(defmethod find-nearest-epsilon ((search m-tree-search) data epsilon &optional result)
  (with-slots (root-node input-key distance) search
    (m-nearest-epsilon root-node (funcall input-key data) distance epsilon result input-key)))

(defun insert-m-tree (node point m input-key distance-fn &optional d-parent)
  (let ((v (funcall input-key point))
	(children (m-tree-node-children node))
	(r (m-tree-node-radius node)))
    (setf (m-tree-node-radius node) (max r (or d-parent
					       (funcall distance-fn (m-tree-node-pivot node)
							(funcall input-key point)))))
    (if (m-tree-leaf-p node)
	;; leaf
	(if (= m (fill-pointer children))
	    ;; full
	    (split-m-tree node (let ((points (make-array (1+ m))))
				 (replace points children)
				 (setf (aref points m) point)
				 points)
			  m input-key distance-fn)
	  (vector-push point children))
      (loop with nearest = nil
	  with flag = nil ;; true if N_in found
	  with min = most-positive-double-float
	  with dp-new = most-positive-double-float
	  for c across children do
	    (let ((d (funcall distance-fn v (m-tree-node-pivot c)))
		  (r (m-tree-node-radius c)))
	      (if (> d r)
		  (unless flag
		    (let ((diff (- d r)))
		      (when (< diff min)
			(setf min diff)
			(setf nearest c dp-new d))))
		(if flag
		    (when (< d min)
		      (setf min d)
		      (setf nearest c dp-new d))
		  (setf flag t min d nearest c dp-new d))))
	  finally (insert-m-tree nearest point m input-key distance-fn dp-new)))))

(defun split-m-tree (node points m input-key distance-fn)
  (declare (optimize (speed 0) (safety 3) (debug 3)))
  (let ((p (m-tree-node-parent node)))
    (when (null p)
      ;; root
      (let ((new (copy-structure node))
	    (array (make-array m :fill-pointer 0)))
	;; new is now old node
	(setf (m-tree-node-parent new) node)
	(setf (m-tree-node-children new) array)
	(setf (fill-pointer (m-tree-node-children node)) 0)
	(vector-push new (m-tree-node-children node))
	(setf p node)
	(setf node new)))
    (let* ((node2 (make-m-tree-node :parent p))
	   (vs (map 'vector input-key points))
	   (pivot (aref vs 0))
	   (d1 (map 'dvec #'(lambda (x) (funcall distance-fn x pivot)) vs))
	   pivot2)
      (setf (m-tree-node-pivot node) pivot)
      (setf (m-tree-node-d-parent node) 0d0)
      (loop with cand = nil
	  with max = most-negative-double-float
	  for d across d1
	  for v across vs
	  when (> d max) do
	    (setf max d)
	    (setf cand v)
	  finally (setf (m-tree-node-pivot node2) cand pivot2 cand))
	(let ((d2 (map 'dvec #'(lambda (x) (funcall distance-fn x pivot2)) vs))
	      (c1 (m-tree-node-children node))
	      (c2 (make-array m :fill-pointer 0))
	      (r1 0d0)
	      (r2 0d0))
	  (setf (fill-pointer c1) 0)
	  (setf (m-tree-node-children node2) c2)
	  ;; split
	  (do-vecs ((p points :type t)
		    (dis1 d1 :type double-float)
		    (dis2 d2 :type double-float))
	    (cond ((> dis2 dis1)
		   ;; belong to one
		   (vector-push p c1)
		   (setf r1 (max r1 dis1)))
		  (t
		   ;; belong to two
		   (vector-push p c2)
		   (setf r2 (max r2 dis2)))))
	  ;; set radius
	  (setf (m-tree-node-radius node) r1)
	  (setf (m-tree-node-radius node2) r2)
	  ;; d-parent and radius of parent
	  (let* ((pp (m-tree-node-pivot p))
		 (dp1 (funcall distance-fn pp pivot))
		 (dp2 (funcall distance-fn pp pivot2)))
	    (setf (m-tree-node-d-parent node) dp1)
	    (setf (m-tree-node-d-parent node2) dp2)
	    (setf (m-tree-node-radius p) (max (m-tree-node-radius p) (+ dp1 r1) (+ dp2 r2))))
	  ;; final addition
	  (let ((p-children (m-tree-node-children p)))
	    (if (= (fill-pointer p-children) m)
		(split-m-tree-recursive p (let ((points (make-array (1+ m)))) ;; maybe recycle available
					    (replace points p-children)
					    (setf (aref points m) node2)
					    points)
					m distance-fn)
	      (vector-push node2 p-children)))))))

(defun split-m-tree-recursive (node points m distance-fn)
  (let ((p (m-tree-node-parent node)))
    (when (null p)
      ;; root
      (let ((new (copy-structure node))
	    (array (make-array m :fill-pointer 0)))
	;; new is now old node
	(setf (m-tree-node-parent new) node)
	(setf (m-tree-node-children new) array)
	(setf (fill-pointer (m-tree-node-children node)) 0)
	(vector-push new (m-tree-node-children node))
	(setf p node)
	(setf node new)))
    (let* ((node2 (make-m-tree-node :parent p))
	   (vs (map 'vector #'m-tree-node-pivot points))
	   (pivot (random-elt vs))
	   (d1 (map 'dvec #'(lambda (x) (funcall distance-fn x pivot)) vs))
	   pivot2)
      (setf (m-tree-node-pivot node) pivot)
      (setf (m-tree-node-d-parent node) 0d0)
      (loop with cand = nil
	  with max = most-negative-double-float
	  for d across d1
	  for v across vs
	  when (> d max) do
	    (setf max d)
	    (setf cand v)
	  finally (setf (m-tree-node-pivot node2) cand pivot2 cand))
	(let ((d2 (map 'dvec #'(lambda (x) (funcall distance-fn x pivot2)) vs))
	      (c1 (m-tree-node-children node))
	      (c2 (make-array m :fill-pointer 0))
	      (r1 0d0)
	      (r2 0d0))
	  (setf (fill-pointer c1) 0)
	  (setf (m-tree-node-children node2) c2)
	  ;; split
	  (do-vecs ((p points :type t)
		    (dis1 d1 :type double-float)
		    (dis2 d2 :type double-float))
	    (cond ((> dis2 dis1)
		   ;; belong to one
		   (vector-push p c1)
		   (setf (m-tree-node-parent p) node)
		   (setf (m-tree-node-d-parent p) dis1)
		   (setf r1 (max r1 (+ dis1 (m-tree-node-radius p)))))
		  (t
		   ;; belong to two
		   (vector-push p c2)
		   (setf (m-tree-node-parent p) node2)
		   (setf (m-tree-node-d-parent p) dis2)
		   (setf r2 (max r2 (+ dis2 (m-tree-node-radius p)))))))
	  ;; set radius
	  (setf (m-tree-node-radius node) r1)
	  (setf (m-tree-node-radius node2) r2)
	  ;; d-parent and radius of parent
	  (let* ((pp (m-tree-node-pivot p))
		 (dp1 (funcall distance-fn pp pivot))
		 (dp2 (funcall distance-fn pp pivot2)))
	    (setf (m-tree-node-d-parent node) dp1)
	    (setf (m-tree-node-d-parent node2) dp2)
	    (setf (m-tree-node-radius p) (max (m-tree-node-radius p) (+ dp1 r1) (+ dp2 r2))))
	  ;; final addition
	  (let ((p-children (m-tree-node-children p)))
	    (if (= (fill-pointer p-children) m)
		(split-m-tree-recursive p (let ((points (make-array (1+ m)))) ;; maybe recycle available
					    (replace points p-children)
					    (setf (aref points m) node2)
					    points)
					m distance-fn)
	      (vector-push node2 p-children)))))))

#+ignore
(defun m-nearest (node x distance-fn
		  &optional (key #'identity)
			    tmp
			    (tmp-d (funcall distance-fn x (m-tree-node-pivot node)))
			    (d-pivot tmp-d))
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type (or null double-float) tmp-d d-pivot))
  (if (m-tree-leaf-p node)
      (loop for p across (m-tree-node-children node)
	  for d double-float = (funcall distance-fn (funcall key p) x)
	  when (<= d tmp-d) do
	    (setf tmp p tmp-d d)
	  finally (return (values tmp tmp-d)))
    (let* ((children (m-tree-node-children node))
	   (tmp2 (map 'vector #'(lambda (c) (cons c (- (the double-float
							 (abs (- d-pivot (the double-float (m-tree-node-d-parent c)))))
						       (the double-float (m-tree-node-radius c))))) children)))
      (sort tmp2 #'(lambda (x y) (declare (type double-float x y)) (< x y)) :key #'cdr)
      (loop for pair across tmp2 do
	    (destructuring-bind (c . d) pair
	      (declare (type double-float d))
	      (if (<= d tmp-d)
		  (let* ((r (m-tree-node-radius c))
			 (dis (funcall distance-fn (m-tree-node-pivot c) x))
			 (d-min (max (- dis r) 0d0)))
		    (declare (type double-float r dis d-min))
		    (when (<= d-min tmp-d)
		      (multiple-value-bind (p d2)
			  (m-nearest c x distance-fn key tmp (min (+ dis r) tmp-d) dis)
			(declare (type double-float d2))
			(setf tmp p)
			(setf tmp-d d2))))
		(return (values tmp tmp-d))))
	  finally (return (values tmp tmp-d))))))

(defun m-nearest (root x distance-fn &optional (key #'identity) (implementation :binomial))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((pq (make-prique implementation
			 :lessp #'(lambda (x y) (declare (type double-float x y)) (< x y))
			 :key #'car
			 :maxcount 10000)))
    (multiple-value-bind (tmp tmp-d)
	(let ((d-pivot (funcall distance-fn x (m-tree-node-pivot root))))
	  (m-nearest-pq-iter root x distance-fn nil (+ d-pivot (m-tree-node-radius root)) pq key d-pivot))
      (loop
	  for (d-k d-pivot node) = (delete-min-prique pq)
	  while (<= d-k tmp-d) do
	    (multiple-value-bind (p2 d2) (m-nearest-pq-iter node x distance-fn tmp tmp-d pq key d-pivot)
	      (setf tmp p2 tmp-d d2))
	  when (prique-empty-p pq) do (return))
      (values tmp tmp-d))))

(defun m-nearest-pq-iter (node x distance-fn tmp tmp-d pq key d-pivot)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float d-pivot tmp-d))
  (if (m-tree-leaf-p node)
      (loop for p across (m-tree-node-children node)
	  for d double-float = (funcall distance-fn (funcall key p) x)
	  when (<= d tmp-d) do
	    (setf tmp p tmp-d d)
	  finally (return (values tmp tmp-d)))
    (let ((children (m-tree-node-children node)))
      (loop for c across children do
	    (let ((d-abs (- (abs (- d-pivot (the double-float (m-tree-node-d-parent c))))
			    (the double-float (m-tree-node-radius c)))))
	      (when (<= d-abs tmp-d)
		(let* ((r (m-tree-node-radius c))
		       (dis (funcall distance-fn (m-tree-node-pivot c) x))
		       (d-min (max (- dis r) 0d0)))
		  (declare (type double-float r dis d-min))
		  (when (<= d-min tmp-d)
		    ;; insertion
		    (insert-prique pq (list d-min dis c))
		    (let ((d-max (+ dis r)))
		      (declare (type double-float d-max))
		      ;;; update
		      (when (< d-max tmp-d)
			(setf tmp c tmp-d d-max)))))))
	  finally (return (values tmp tmp-d))))))

#+ignore
(defun m-nearest-k (node x distance-fn result tmp
		    &optional (key #'identity) (d-pivot (funcall distance-fn x (m-tree-node-pivot node))))
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type (or null double-float) d-pivot)
	   (type vector result tmp))
  (if (m-tree-leaf-p node)
      (loop for p across (m-tree-node-children node)
	  for d double-float = (funcall distance-fn (funcall key p) x) do
	    (let ((flag nil))
	      (do-vecs ((_ tmp :setf-var td :type double-float)
			(__ result :setf-var tp :type t))
		(declare (ignorable _ __))
		(when (or (not tp) flag (<= d td))
		  (setf flag t)
		  (rotatef d td)
		  (rotatef p tp)))))
    (let* ((children (m-tree-node-children node))
	   (tmp2 (map 'vector #'(lambda (c) (cons c (- (the double-float
							 (abs (- d-pivot (the double-float (m-tree-node-d-parent c)))))
						       (the double-float (m-tree-node-radius c))))) children)))
      (sort tmp2 #'(lambda (x y) (declare (type double-float x y)) (< x y)) :key #'cdr)
      (loop for pair across tmp2 do
	    (destructuring-bind (c . d) pair
	      (declare (type double-float d))
	      (let ((max (aref tmp (1- (length tmp)))))
		(declare (type double-float max))
		(if (<= d max)
		    (let* ((r (m-tree-node-radius c))
			   (dis (funcall distance-fn (m-tree-node-pivot c) x))
			   (d-min (max (- dis r) 0d0)))
		      (declare (type double-float r dis d-min))
		      (when (<= d-min max)
			(let ((d-max (+ dis r))
			      (flag nil))
			  (declare (type double-float d-max))
			  (when (< d-max max)
			    (do-vecs ((_ tmp :setf-var td :type double-float)
				      (__ result :setf-var tp :type t))
			      (declare (ignorable _ __))
			      (when (and (not tp) (or flag (< d-max td)))
				(setf flag t)
				(rotatef d-max td)))))
			(m-nearest-k c x distance-fn result tmp key dis)))
		  (return)))))))
  (values result tmp))

(defun m-nearest-k (root x distance-fn result tmp &optional (key #'identity) (implementation :binomial))
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type vector result tmp))
  (let ((pq (make-prique implementation
			 :lessp #'(lambda (x y) (declare (type double-float x y)) (< x y))
			 :key #'car
			 :maxcount 10000))
	(k (1- (length tmp))))
    (m-nearest-k-pq-iter root x distance-fn result tmp pq key (funcall distance-fn x (m-tree-node-pivot root)))
    (loop
	for (d-k d-pivot node) = (delete-min-prique pq)
	while (<= d-k (aref tmp k)) do
	  (m-nearest-k-pq-iter node x distance-fn result tmp pq key d-pivot)
	when (prique-empty-p pq) do (return))
    (values result tmp)))

(defun m-nearest-k-pq-iter (node x distance-fn result tmp pq key d-pivot)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float d-pivot)
	   (type vector result tmp))
  (if (m-tree-leaf-p node)
      (loop for p across (m-tree-node-children node)
	  for d double-float = (funcall distance-fn (funcall key p) x) do
	    (let ((flag nil))
	      (do-vecs ((_ tmp :setf-var td :type double-float)
			(__ result :setf-var tp :type t))
		(declare (ignorable _ __))
		(when (or flag (<= d td))
		  (setf flag t)
		  (rotatef d td)
		  (rotatef p tp)
		  (when (eq node p)
		    (return))))))
    (let ((children (m-tree-node-children node))
	  (k (1- (length tmp))))
      (loop for c across children do
	    (let ((d-abs (- (abs (- d-pivot (the double-float (m-tree-node-d-parent c))))
			    (the double-float (m-tree-node-radius c))))
		  (max (aref tmp k)))
	      (when (<= d-abs max)
		(let* ((r (m-tree-node-radius c))
		       (dis (funcall distance-fn (m-tree-node-pivot c) x))
		       (d-min (max (- dis r) 0d0)))
		  (declare (type double-float r dis d-min))
		  (when (<= d-min max)
		    ;; insertion
		    (insert-prique pq (list d-min dis c))
		    (let ((d-max (+ dis r))
			  (flag nil))
		      (declare (type double-float d-max))
		      ;;; nn-update
		      (when (< d-max max)
			(do-vecs ((_ tmp :setf-var td :type double-float)
				  (__ result :setf-var tp :type t))
			  (declare (ignorable _ __))
			    ;;; parent remove check
			  (cond ((or flag (<= d-max td))
				 (setf flag t)
				 (rotatef tp c)
				 (rotatef d-max td)
				 (when (eq node c)
				   (return)))))))))))))))
				  
(defun m-nearest-epsilon (node x distance-fn epsilon result
			  &optional (key #'identity) (d-pivot (funcall distance-fn x (m-tree-node-pivot node))))
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float epsilon)
	   (type (or null double-float) d-pivot)
	   (type vector result))
  (if (m-tree-leaf-p node)
      (loop for p across (m-tree-node-children node)
	  for d double-float = (funcall distance-fn (funcall key p) x)
	  when (<= d epsilon) do
	    (vector-push-extend p result))
    (let ((children (m-tree-node-children node)))
      (loop for c across children
	  for d-parent double-float = (m-tree-node-d-parent c)
	  for r double-float = (m-tree-node-radius c)
	  for er double-float = (+ epsilon r)
	  when (<= (abs (- d-pivot d-parent)) er) do
	    (let ((dis (funcall distance-fn (m-tree-node-pivot c) x)))
	      (declare (type double-float dis))
	      (when (<= dis er)
		(m-nearest-epsilon c x distance-fn epsilon result key dis))))))
  result)

;; LSH
(defclass lsh ()
  ((local-hash :initform (make-hash-table :test #'eql) :accessor local-hash)
   (fns :initarg :fns :accessor lsh-fns)))

(defgeneric lsh-bit (lsh x)
  (:documentation "Locality Sensitive Hash Function -- return bit array"))

(defmethod lsh-bit ((lsh lsh) x)
  (let ((fns (lsh-fns lsh))
	(ans 0))
    (declare (type fixnum ans)) ;; fixnum limit bit length!!!
    (do-vec (fn fns :type t)
      (setf ans (* ans 2))
      (incf ans (if (plusp (funcall fn x))
		  1
		0)))
    ans))

(defgeneric make-lsh-fn (locality-sensitive-hashing)
  (:documentation "make LSH function object"))

(defgeneric lsh-map (lsh x)
  (:documentation "Locality Sensitive Hash Function Mapper"))

(defmethod lsh-map ((lsh lsh) x)
  (let ((bit (lsh-bit lsh x))
	(lh (local-hash lsh)))
    (multiple-value-bind (array found) (gethash bit lh)
      (unless found
	(let ((new (make-array 0 :fill-pointer t :adjustable t)))
	  (setf (gethash bit lh) new)
	  (setf array new)))
      array)))

(defmethod add-hashed-data ((lsh lsh) point bit)
  (let ((lh (local-hash lsh)))
    (multiple-value-bind (array found) (gethash bit lh)
      (unless found
	(let ((new (make-array 0 :fill-pointer t :adjustable t)))
	  (setf (gethash bit lh) new)
	  (setf array new)))
      (vector-push-extend point array))))

(defmethod find-candidates ((search locality-sensitive-hashing) data)
  (with-slots (hash-fns candidates hash-length) search
    (setf (fill-pointer candidates) 0)
    (loop
	while (> (* hash-length 2) (fill-pointer candidates)) ;; find amount upper bound: 2L
	for hash across hash-fns do
	  (loop for p across (lsh-map hash data)
	      unless (find p candidates) do
		(vector-push-extend p candidates)))
    candidates))

(defmethod find-nearest ((search locality-sensitive-hashing) data)
  (with-slots (input-key distance) search
    (let* ((d (funcall input-key data))
	   (candidates (find-candidates search d)))
      (nearest candidates d distance input-key))))

(defmethod find-nearest-k ((search locality-sensitive-hashing) data k &optional result tmp)
  (declare (ignore k))
  (with-slots (input-data input-key distance) search
    (let* ((d (funcall input-key data))
	   (candidates (find-candidates search d)))
      (nearest-k candidates d distance input-key result tmp))))

(defmethod find-nearest-epsilon ((search locality-sensitive-hashing) data epsilon &optional result)
  (with-slots (input-data input-key distance) search
    (let* ((d (funcall input-key data))
	   (candidates (find-candidates search d)))
      (nearest-epsilon candidates d distance epsilon input-key result))))

(defmethod initialize-search ((search locality-sensitive-hashing))
  (with-slots (input-data input-key hash-fns) search
    (loop
	for i from 0 below (length hash-fns) do
	  (setf (aref hash-fns i) (make-lsh-fn search)))
    (loop for p across input-data
	for d = (funcall input-key p) do
	  (loop for hash across hash-fns
	      for b = (lsh-bit hash d) do
		(add-hashed-data hash p b))))
  search)

;; p-stable distribution hash families
(defclass p-stable-locality-sensitive-hashing (locality-sensitive-hashing)
  ((w :initarg :w :accessor plsh-w)))

(defun p-stable-lsh-closure (a b w)
  (declare (type double-float b w)
	   (type dvec a)
           (optimize speed (safety 0) (debug 1)))
  #'(lambda (x)
      (declare (type dvec x))
      (floor (/ (+ (the double-float (inner-product a x)) b) w))))

;; euclid (L2-norm) hash
(defclass euclid-locality-sensitive-hashing (p-stable-locality-sensitive-hashing) ())

(defmethod make-lsh-fn ((lsh euclid-locality-sensitive-hashing))
  (with-slots (w input-data input-key hash-bit) lsh
    (let ((fns (make-array hash-bit))
	  (dim (length (funcall input-key (aref input-data 0)))))
      (do-vec (_ fns :setf-var fn)
	(declare (ignorable _))
	(setf fn
	  (let ((a (make-dvec dim)))
	    (do-vec (__ a :setf-var ai :type double-float)
	      (declare (ignorable __))
	      (setf ai (normal-random 0d0 1d0)))
	  (p-stable-lsh-closure a (random w) w))))
      (make-instance 'lsh :fns fns))))

(defmethod initialize-instance ((instance euclid-locality-sensitive-hashing) &rest args)
  (declare (ignore args))
  (call-next-method)
  (setf (nns-distance instance) #'euclid-distance)
  instance)

;; manhattan (L1 norm) hash
(defclass manhattan-locality-sensitive-hashing (p-stable-locality-sensitive-hashing) ())

(defmethod make-lsh-fn ((lsh manhattan-locality-sensitive-hashing))
  (with-slots (w input-data input-key hash-bit) lsh
    (let ((fns (make-array hash-bit))
	  (dim (length (funcall input-key (aref input-data 0)))))
      (do-vec (_ fns :setf-var fn)
	(declare (ignorable _))
	(setf fn
	  (let ((a (make-dvec dim)))
	    (do-vec (__ a :setf-var ai :type double-float)
	      (declare (ignorable __))
	      (setf ai (cauchy-random 0d0 1d0)))
	  (p-stable-lsh-closure a (random w) w))))
      (make-instance 'lsh :fns fns))))

(defmethod initialize-instance ((instance manhattan-locality-sensitive-hashing) &rest args)
  (declare (ignore args))
  (call-next-method)
  (setf (nns-distance instance) #'manhattan-distance)
  instance)

;; cosine hash
;; this is not 'simHash' -- bad definition
(defclass cosine-locality-sensitive-hashing (locality-sensitive-hashing) ())

(defun cosine-lsh-closure (u)
  (declare (type dvec u)
           (optimize speed (safety 0) (debug 1)))
  #'(lambda (x)
      (declare (type dvec x))
      (if (>= (the double-float (inner-product u x)) 0d0)
	  1
	0)))

(defmethod make-lsh-fn ((lsh cosine-locality-sensitive-hashing))
  (with-slots (input-data input-key hash-bit) lsh
    (let ((fns (make-array hash-bit))
	  (dim (length (funcall input-key (aref input-data 0)))))
      (do-vec (_ fns :setf-var fn)
	(declare (ignorable _))
	(setf fn
	  (let ((a (make-dvec dim)))
	    (do-vec (__ a :setf-var ai :type double-float)
	      (declare (ignorable __))
	      (setf ai (normal-random 0d0 1d0)))
	  (cosine-lsh-closure a))))
      (make-instance 'lsh :fns fns))))

(defmethod initialize-instance ((instance cosine-locality-sensitive-hashing) &rest args)
  (declare (ignore args))
  (call-next-method)
  (setf (nns-distance instance) #'cosine-distance)
  instance)

;; spectral hash
(defclass spectral-hashing (locality-sensitive-hashing)
  ((hash-length :initform 1)))

(defparameter *sh-epsilon* (expt 2d0 -52))

(defun vmax (dvec)
  (let ((m most-negative-double-float))
    (do-vec (p dvec :type double-float)
      (setf m (max m p)))
    m))

(defmethod initialize-search ((search spectral-hashing))
  (with-slots (input-data input-key hash-fns) search
    (setf hash-fns (make-array 1 :initial-element (make-lsh-fn search)))
    (loop for p across input-data
	for d = (funcall input-key p) do
	  (loop for hash across hash-fns
	      for b = (lsh-bit hash d) do
		(add-hashed-data hash p b))))
  search)

(defmethod make-lsh-fn ((lsh spectral-hashing))
  (with-slots (w input-data input-key hash-bit) lsh
    (let* ((X (map 'vector input-key input-data))
	   (input-l (length (aref X 0))))
      (assert (<= hash-bit input-l))
      (let ((dataset (make-numeric-dataset (loop for i from 0 below input-l collect (format nil "~A" i)) X)))
	(multiple-value-bind (result model) (princomp dataset :method :covariance)
	  (declare (ignore result))
	  (let ((proj (princomp-projection dataset model))
		(min (make-dvec input-l most-positive-double-float))
		(max (make-dvec input-l most-negative-double-float)))
	    (loop for point across proj do
		  (do-vecs ((p point :type double-float)
			    (_ min   :type double-float :setf-var n)
			    (__ max  :type double-float :setf-var x))
		    (declare (ignorable _ __))
		    (setf n (min n p))
		    (setf x (max x p))))
	    (do-vecs ((_ min   :type double-float :setf-var n)
		      (__ max  :type double-float :setf-var x))
	      (declare (ignorable _ __))
	      (setf n (- n *sh-epsilon*))
	      (setf x (+ x *sh-epsilon*)))
	    (let* ((R (v- max min max))
		   (maxR (vmax R))
		   (maxMode (map 'vector #'(lambda (x) (ceiling (/ (* (1+ hash-bit) x) maxR))) R))
		   (nModes (1+ (- (loop for m across maxMode summing m) (length maxMode))))
		   (modes (coerce
			   (loop repeat input-l collect (make-dvec nModes 1d0))
			   'vector)))
	      #+ignore (declare)
	      ;;; tricky! please check
	      (let ((diff -1))
		(loop for i from 0 below input-l do
		      (let ((mo (aref modes i)))
			(loop with mm = (aref maxMode i)
			    for j from 2 upto mm do
			      (setf (aref mo (+ j diff)) (dfloat j))
			    finally (setf diff (+ diff (1- mm)))))))
	      (loop for mode across modes do
		    (loop for i from 0 below nModes do
			  (decf (aref mode i))))
	      (let* ((omega0 (map 'dvec #'(lambda (x) (/ pi x)) R))		
		     (omegas (map 'vector #'(lambda (x om)
					      (map 'dvec #'(lambda (z) (* z om)) x)) modes omega0))
		     (eigVal (map 'dvec #'(lambda (x) (reduce #'+ x :key #'(lambda (y) (* y y)))) omegas))
		     (tmp (map 'vector #'cons eigval omegas)))
		(sort tmp #'< :key #'car)
		;;; construct hash fn
		(let ((ans (make-array hash-bit))
		      (pc (loading-factors model)))
		  (loop for i from 0 below hash-bit do
			(setf (aref ans i)
			  (let ((os (cdr (aref tmp i))))
			    #'(lambda (x)
				(declare (type dvec x))
				(let ((prj (make-dvec (length x))))
				  (do-vec (v pc :type dvec :index-var iv)
				    (setf (aref prj iv)
					  (inner-product x v)))
				  (v- prj min prj)
				  (let ((ans 1d0))
				    (do-vecs ((p prj :type double-float)
						  (s os  :type double-float))
				      (setf ans (* ans (sin (+ (* p s) #.(/ pi 2))))))
				    (if (> ans 0) 1 0)))))))
		  (make-instance 'lsh :fns ans))))))))))

;; simhash -- broken?
(defclass simhash (locality-sensitive-hashing)
  ((hash-length :initform 0)
   (v :accessor simhash-tmp-v)
   (lsh :initform (make-instance 'lsh))))

(defun data-to-bit (data elt-hash-array v)
  (fill v 0d0)
  (loop for elt across data
      for elt-hash across elt-hash-array do
	(let ((int (funcall elt-hash elt)))
	  (loop for i from 0
	      while (zerop int) do
		(multiple-value-bind (n bit) (floor int 2)
		  (if (= bit 1)
		      (incf (aref v i) elt)
		    (decf (aref v i) elt))
		  (setf int n)))))
  (loop
      with ans = 0
      for flag across v
      for i from 0 do
	(setf ans (* ans 2))
	(incf ans (if (plusp flag)
		      1
		    0))
      finally (return ans)))

(defun make-elt-hash-closure (min max f)
  (let ((bit-step (/ (- max min) f)))
    #'(lambda (elt)
	(declare (type double-float elt))
	(floor (/ (- elt min) bit-step)))))

(defmethod initialize-instance ((instance simhash) &rest args)
  (declare (ignore args))
  (call-next-method)
  (setf (nns-distance instance) #'cosine-distance)
  (with-slots (input-data input-key v hash-length hash-bit hash-fns) instance
    (setf hash-length (length (funcall input-key (aref input-data 0))))
    (setf v (make-dvec hash-length))
    (setf hash-fns (make-array hash-length)))
  instance)

(defmethod initialize-search ((search simhash))
  (with-slots (input-data hash-bit input-key hash-fns v lsh) search
    (let ((mins (make-dvec (length v) most-positive-double-float))
	  (maxs (make-dvec (length v) most-negative-double-float)))
    (loop
	for p across input-data
	for vec = (funcall input-key p) do
	  (do-vecs ((x vec :type double-float)
		    (_ mins  :type double-float :setf-var min)
		    (__ maxs :type double-float :setf-var max))
	    (declare (ignore _ __))
	    (setf min (min min x) max (max max x))))
    (loop
	for i from 0
	for max across maxs
	for min across mins do
	  (setf (aref hash-fns i) (make-elt-hash-closure min max hash-bit)))
    (loop for p across input-data
	for d = (funcall input-key p) do
	  (add-hashed-data lsh p (data-to-bit d hash-fns v)))))
  search)

(defmethod find-candidates ((search simhash) data)
  (with-slots (lsh hash-fns candidates v) search
    (gethash (data-to-bit data hash-fns v) (local-hash lsh) #())))

;; stochastic validation utility
(defmethod stochastic-validation ((search nearest-search) test-dataset &optional (k 5) (epsilon 1d-2))
  (let ((naive
	 (with-slots (input-data input-key distance) search
	   (make-instance 'naive-nearest-search :input-data input-data :input-key input-key :distance distance)))
	(total (length test-dataset))
	(nearest-match 0)
	(k-total 0)
	(k-match 0)
	(k-exact-match 0)
	(e-total 0)
	(e-match 0)
	(e-exact-match 0)
	)
    (loop for test across test-dataset
	for nearest = (find-nearest naive test)
	for est     = (find-nearest search test)
	when (eq nearest est) do (incf nearest-match))
    (loop for test across test-dataset
	for nearest-k = (find-nearest-k naive test k)
	for est-k     = (find-nearest-k search test k) do
	  (loop
	      with flag = t
	      for nk across nearest-k do
		(incf k-total)
		(if (find nk est-k :test #'equalp)
		    (incf k-match)
		  (setf flag nil))
	      finally (when flag (incf k-exact-match))))
    (loop for test across test-dataset
	for nearest-e = (find-nearest-epsilon naive test epsilon)
	for est-e     = (find-nearest-epsilon search test epsilon) do
	  (loop
	      with flag = t
	      for ne across nearest-e do
		(incf e-total)
		(if (find ne est-e :test #'equalp)
		    (incf e-match)
		  (setf flag nil))
	      finally (when flag (incf e-exact-match))))
    (format t "nearest: ~A~%" (dfloat (/ nearest-match total)))
    (format t "k-exact: ~A~%" (dfloat (/ k-exact-match total)))
    (format t "k-sub  : ~A~%" (dfloat (/ k-match k-total)))
    (format t "e-exact: ~A~%" (dfloat (/ e-exact-match total)))
    (format t "e-sub  : ~A~%" (dfloat (/ e-match e-total)))))
(defpackage :hjs.util.vector
  (:use :cl
	:hjs.util.meta)
  (:nicknames :vector)
  (:export #:make-dvec
	   #:fill-vec
	   #:do-vec
	   #:do-vecs
           #+future #:par-do-vec
           #:copy-vec
	   #:v+
	   #:v-
	   #:v-scale
	   #:inner-product
	   #:inner-product-unsafe
	   #:distance-to-origin
	   #:euclid-distance
	   #:manhattan-distance
	   #:cosine-distance
     #:hausdorff-distance
	   #:normalize-vec
	   #:reorder-vec
	   #:reorder-dvec
	   #:specialize-vec
     #:mean-points
	   ))

(in-package :hjs.util.vector)

(declaim (optimize (speed 3) (safety 1) (space 0) (debug 1)))


;;@ function-type: integer -> dvec
;;@ precondition:
;;@  - size must be non-negative
;;@  - initial-element must be a double float
(defun make-dvec (size &optional initial-element)
  (assert (>= size 0))
  (if initial-element
      (#+mkl ffi-utils:make-static-array #-mkl make-array
       size :element-type 'double-float :initial-element initial-element)
    (#+mkl ffi-utils:make-static-array #-mkl make-array size :element-type 'double-float)))


;;; macro: do-vec
;;@ precondition:
;;@  - var, setf-var, index-var must be a symbol
;;@ return: content specified by return, or nil
;;@ note:
;;@  - type means the type of var (element in the vector)
;;@  - if return specified, return the expression, otherwise return nil (like dolist)
(defmacro do-vec ((var vector &key (type t) (start 0) end from-end setf-var index-var return)
		  &body body &environment env)
  "Iterate on array that is a kind of simple-array.
e.g.
\(defun distance-to-origin (x)
  (declare (type dvec x)
	   #+allegro (:faslmode :immediate))
  (let ((result 0.0))
    (declare (type (double-float 0.0) result))
    (do-vec (ex x :type double-float)
      (incf result (* ex ex)))
    (sqrt result)))"
  (check-type var symbol)
  (check-type setf-var symbol)
  (check-type index-var symbol)
  (let ((vec-var (gensym "VEC-"))
	(index (or index-var (gensym "INDEX-"))))
    (labels ((build-index-var-clause ()
	       (if (not from-end)
		   `(for ,index of-type array-index from ,start below (or ,end (length ,vec-var)))
		   `(for ,index of-type array-index from (1- (or ,end (length ,vec-var))) downto ,start)))
	     (build-data-var-clause ()
	       `(for ,var of-type ,type = (aref ,vec-var ,index)))
	     (build-setf-var-clause ()
	       (and setf-var `((,setf-var (aref ,vec-var ,index)))))
	     (build-decl-clause ()
	       `(declare 
		 ,@(let ((vector-type-decl
			  (or (second
			       (assoc 'type
				      #+allegro
                                      (nth-value 2 (sys:variable-information vector env))
                                      #+sbcl
                                      (nth-value 3 (sb-cltl2:variable-information vector env))
                                      #+lispworks
                                      (nth-value 3 (cl::variable-information vector env))
                                      ))
			      (when type
				`(simple-array ,type (*)))
			      `(simple-array T (*)))))
			`((type ,vector-type-decl ,vec-var)
			  ,@(when (symbolp vector)
				  `((type ,vector-type-decl ,vector))))))))
      `(let ((,vec-var ,vector))
	 (locally ,(build-decl-clause)
	   (loop
	      ,@(build-index-var-clause)
	      ,@(build-data-var-clause)
	      do (symbol-macrolet ,(build-setf-var-clause)
		   ,@body)
	      finally (return ,return)))))))

#+future
(defun next-end (total-size n-processors start)
  (declare (type fixnum total-size n-processors start)
           (optimize speed (safety 0) (debug 1)))
  (if (>= start total-size)
      nil
      (the fixnum (+ start (the fixnum (round (the fixnum (- total-size start)) n-processors))))))

#+future
(defmacro par-do-vec ((var vector &key (type t) (start 0) end setf-var index-var return)
                      &body body)
  (check-type var symbol)
  (once-only (start end vector)
    (with-unique-names (inner-start inner-end)
      `(labels ((do-it (,inner-start ,inner-end)
                  (declare (type array-index ,inner-start ,inner-end))
                  (do-vec (,var ,vector :type ,type :start ,inner-start :end ,inner-end
                                :setf-var ,setf-var :index-var ,index-var)
                    ,@body)))
         (declare (type (simple-array ,type (*)) ,vector))
         (let* ((,start (or ,start 0))
                (,end (or ,end (length ,vector)))
                (length (- ,end ,start)))
           (declare (type array-index ,start ,end length))
           (future:with-new-environment ()
             (future:wait-for-all-futures
              (loop for processors of-type array-index downfrom (future:future-max-threads)
                    for start = ,start then end
                    for end = (next-end length processors start)
                    for future-id of-type array-index from 0
                    while end
                    collect
                 (let ((start start)
                       (end end))
                   (future:future (do-it start end))))))
           ,return)))))


;;; macro do-vecs

(defmacro do-vecs ((&rest binding-clauses) &body body &environment env)
  "Parallelly iterate on multiple vectors. Accept parameters in do-vec except :return.
e.g.
\(defun euclid-distance (x y)
  (declare (type dvec x y)
	   #+allegro (:faslmode :immediate))
  (assert (= (length x) (length y)))
  (let ((result 0.0))
    (declare (type (double-float 0.0) result))
    (do-vecs ((ex x :type double-float)
	      (ey y :type double-float))
      (let ((diff (- ex ey)))
	(incf result (* diff diff))))
    (sqrt result)))"
  (labels ((build-loop-clause (var vector &key (type t) (start 0) end
				   from-end setf-var ((:index-var index-var-outer) nil))
	     (check-type var symbol)
	     (check-type setf-var symbol)
	     (check-type index-var-outer symbol)
	     (let ((vec-var (gensym "VEC-"))
		   (index-var (or index-var-outer (gensym "INDEX-"))))
	       (labels ((build-index-var-clause ()
			  (when (or index-var-outer from-end)
			    (if (not from-end)
				`(for ,index-var of-type array-index from ,start below (or ,end (length ,vec-var)))
				`(for ,index-var of-type array-index
				      from (1- (or ,end (length ,vec-var))) downto ,start))))
			(build-data-var-clause ()
			  `(for ,var of-type ,type = (aref ,vec-var ,index-var)))
			(build-setf-var-clause ()
			  (and setf-var `((,setf-var (aref ,vec-var ,index-var)))))
			(build-declaration-clause ()
			  (when (symbolp vector)
			    (let ((vector-type-decl
				   (second
				    (assoc 'type
					   #+allegro
                                           (nth-value 2 (sys:variable-information vector env))
                                           #+sbcl
                                           (nth-value 3 (sb-cltl2:variable-information vector env))
                                           #+lispworks
                                           (nth-value 3 (cl::variable-information vector env))
                                           ))))
			      (when vector-type-decl
				`((type ,vector-type-decl ,vec-var)))))))
		 (list :vec-var vec-var
		       :index-var index-var
		       :declaration-clause (build-declaration-clause)
		       :index-var-clause (build-index-var-clause)
		       :data-var-clause (build-data-var-clause)
		       :setf-var-clause (build-setf-var-clause)
		       :orig-vec vector
		       )))))
    (let* ((building-blocks (mapcar (lambda (c) (apply #'build-loop-clause c)) binding-clauses))
	   (main-index-var (gensym "INDEX-"))
	   (has-index-var-clause-p
	    (dolist (b building-blocks)
	      (when (getf b :index-var-clause)
		(return t)))))
      `(let (,@(mapcar (lambda (b) (list (getf b :vec-var) (getf b :orig-vec))) building-blocks))
	 (locally
	     (declare ,@(mapcan (lambda (b) (getf b :declaration-clause)) building-blocks))
	   (loop
	      ,@(if has-index-var-clause-p
		    `(for ,main-index-var of-type array-index from 0)
		    `(for ,main-index-var of-type array-index
			  from 0 below (length ,(getf (first building-blocks) :vec-var))))
	      ,@(mapcan
		 (lambda (b)
		   (append (getf b :index-var-clause)
			   (subst main-index-var
				  (getf b :index-var)
				  (getf b :data-var-clause))))
		 building-blocks)
	      do (symbol-macrolet ,(mapcan (lambda (b)
					     (subst main-index-var
						    (getf b :index-var)
						    (getf b :setf-var-clause)))
					   building-blocks)
		   (declare (ignorable _ __ ___ ____))
		   ,@body)))))))


;;@ function-type: dvec -> &optional dvec -> dvec
(defun copy-vec (vec &optional target)
  (declare (type dvec vec))
  (if (not target)
      (copy-seq vec)
      (progn
        (assert (typep target 'dvec))
        (replace target vec))))

;;@ function-type: dvec -> double-float -> dvec
(defun fill-vec (vec default)
  (declare (double-float default)
	   (type dvec vec))
  (do-vec (ev vec :type double-float :index-var iv :return vec)
    (declare (ignorable ev))
    (setf (aref vec iv) default)))


;;@ precondition: x, y, and result are of same length
;;@ postcondition:
;;@ side-effect: result will be destructively updated
(declaim (ftype (function (dvec dvec dvec) dvec) v+))
(defun v+ (x y result)
  (declare (type dvec x y result))
  (assert (= (length x) (length y) (length result)))
  (do-vecs ((ex x :type double-float)
	    (ey y :type double-float)
	    (_ result :type double-float :setf-var sr))
    (setf sr (+ ex ey)))
  result)


;;@ function-type: dvec -> dvec -> dvec -> dvec
;;@ precondition: x, y, and result are of same length
;;@ postcondition:
;;@ side-effect: result will be destructively updated
(declaim (ftype (function (dvec dvec dvec) dvec) v-))
(defun v- (x y result)
  (declare (type dvec x y result))
  (assert (= (length x) (length y) (length result)))
  (do-vecs ((ex x :type double-float)
	    (ey y :type double-float)
	    (_ result :type double-float :setf-var sr))
    (setf sr (- ex ey)))
  result)

;;@ function-type: dvec -> double-float -> dvec -> dvec
;;@ precondition: vec and result are of same length
;;@ postcondition:
;;@ side-effect: result will be destructively updated
(declaim (ftype (function (dvec double-float dvec) dvec) v-scale))
(defun v-scale (vec n result)
  (declare (double-float n)
	   (type dvec vec result))
  (assert (= (length vec) (length result)))
  (do-vecs ((ev vec :type double-float)
	    (_ result :type double-float :setf-var sr))
    (setf sr (* n ev)))
  result)

;;@ function-type: dvec -> dvec -> double-float
;;@ precondition: x and y are of same length
;;@ postcondition:
#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; avoid unboxing in the return value
  ;; non-official
  (setf (get 'inner-product 'sys::immed-args-call)
	'((:lisp :lisp) double-float)))

(declaim (ftype (function (dvec dvec) double-float) inner-product))
(defun inner-product (x y)
  (declare (type dvec x y)
	   #+allegro (:faslmode :immediate))
  (assert (= (length x) (length y)))
  (let ((result 0.0))
    (declare (type double-float result))
    (do-vecs ((ex x :type double-float)
	      (ey y :type double-float))
      (incf result (* ex ey)))
    result))


;;@ function-type: dvec -> dvec -> double-float
;;@ precondition: x and y are of same length
;;@ postcondition:
#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; avoid unboxing in the return value
  ;; non-official
  (setf (get 'inner-product-unsafe 'sys::immed-args-call)
	'((:lisp :lisp) double-float)))

(declaim (ftype (function (dvec dvec) double-float) inner-product-unsafe))
(defun inner-product-unsafe (x y)
  (declare (type dvec x y)
	   (optimize (safety 0))
	   #+allegro (:faslmode :immediate))
  (let ((result 0.0))
    (declare (type double-float result))
    (do-vecs ((ex x :type double-float)
	      (ey y :type double-float))
      (incf result (* ex ey)))
    result))


;;@ function-type: dvec -> double-float
#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; avoid unboxing in the return value
  ;; non-official
  (setf (get 'distance-to-origin 'sys::immed-args-call)
	'((:lisp) double-float)))

(declaim (ftype (function (dvec) (double-float 0.0)) distance-to-origin))
(defun distance-to-origin (x)
  (declare (type dvec x)
	   #+allegro (:faslmode :immediate))
  (let ((result 0.0))
    (declare (type (double-float 0.0) result))
    (do-vec (ex x :type double-float)
      (incf result (* ex ex)))
    (sqrt result)))

;;@ function-type: dvec -> dvec -> (or dvec null)
;;@ precondition: vec and result are of same length
;;@ postcondition:
;;@ exception: error when distance is 0
(declaim (ftype (function (dvec dvec) dvec) normalize-vec))
(defun normalize-vec (vec result)
  (declare (type dvec vec result))
  (assert (= (length vec) (length result)))
  (let ((distance (distance-to-origin vec)))
    (when (zerop distance)
      (error "Cannot scale a zero vector to unit vector."))
    (do-vecs ((ev vec :type double-float)
	      (_ result :type double-float :setf-var sr))
      (setf sr (/ ev distance)))
    result))

;;@ function-type: dvec -> dvec -> double-float
;;@ precondition: x and y are of same length
;;@ postcondition:
#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; avoid unboxing in the return value
  ;; non-official
  (setf (get 'euclid-distance 'sys::immed-args-call)
	'((:lisp :lisp) double-float)))

(declaim (ftype (function (dvec dvec) (double-float 0.0)) euclid-distance))
(defun euclid-distance (x y)
  (declare (type dvec x y)
	   #+allegro (:faslmode :immediate))
  (assert (= (length x) (length y)))
  (let ((result 0.0))
    (declare (type (double-float 0.0) result))
    (do-vecs ((ex x :type double-float)
	      (ey y :type double-float))
      (let ((diff (- ex ey)))
	(incf result (* diff diff))))
    (sqrt result)))

;;@ function-type: dvec -> dvec -> double-float
;;@ precondition: x and y are of same length
;;@ postcondition:
#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; avoid unboxing in the return value
  ;; non-official
  (setf (get 'manhattan-distance 'sys::immed-args-call)
	'((:lisp :lisp) double-float)))

(declaim (ftype (function (dvec dvec) (double-float 0.0)) manhattan-distance))
(defun manhattan-distance (x y)
  (declare (optimize speed (safety 0) (debug 0))
           (type dvec x y)
	   #+allegro (:faslmode :immediate))
  (assert (= (length x) (length y)))
  (let ((result 0.0d0))
    (declare (type double-float result))
    (do-vecs ((ex x :type double-float)
	      (ey y :type double-float))
      (incf result (abs (- ex ey))))
    result))

;;@ function-type: dvec -> dvec -> double-float
;;@ precondition: x and y are of same length
;;@ postcondition:
#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; avoid unboxing in the return value
  ;; non-official
  (setf (get 'cosine-distance 'sys::immed-args-call)
	'((:lisp :lisp) double-float)))

(declaim (ftype (function (dvec dvec) (double-float 0.0)) cosine-distance))
(defun cosine-distance (x y)
  (declare (optimize speed (safety 0) (debug 0))
           (type dvec x y)
	   #+allegro (:faslmode :immediate))
  (assert (= (length x) (length y)))
  (let ((a (* (distance-to-origin x)
	      (distance-to-origin y))))
    (when (zerop a)
      (error "Cannot determine the cosine distance of a zero vector."))
    (- 1.0d0
       (/ (inner-product x y)
	  a))))

;;@ function-type: (points points) -> double-float
;;@ precondition: points in xpts and ypts are of same length
(defun hausdorff-distance (xpts ypts &key (norm #'euclid-distance))
  (declare (type (simple-array dvec (*)) xpts ypts))
  (assert (= (length (aref xpts 0)) (length (aref ypts 0))))
  (flet ((h-d (pts1 pts2) (declare (type (simple-array dvec (*)) pts1 pts2))
              (loop for v1 across pts1
                  maximize (loop for v2 across pts2
                               minimize (funcall norm v1 v2)))))
    (max (h-d xpts ypts) (h-d ypts xpts))))
  
;;@ function-type: vector -> (simple-array fixnum (*)) -> vector
;;@ precondition: length of vector is the same as length of indices and result
;;@ postcondition: return type is the same as input type
(defun reorder-vec (vector indices &optional (result (copy-seq vector)))
  (declare (type vector vector result)
	   (type (simple-array fixnum (*)) indices))
  (assert (= (length vector) (length indices) (length result)))
  (do-vec (v indices :type fixnum :index-var i :return result)
    (setf (aref result i) (aref vector v))))

;;@ function-type: dvec -> (simple-array fixnum (*)) -> dvec
;;@ precondition: length of vector is the same as length of indices and result
;;@ postcondition: return type is the same as input type
(defun reorder-dvec (vector indices &optional (result (copy-seq vector)))
  (declare (type dvec vector result)
	   (type (simple-array fixnum (*)) indices))
  (assert (= (length vector) (length indices) (length result)))
  (do-vec (v indices :type fixnum :index-var i :return result)
    (setf (aref result i) (aref vector v))))


;;@ function-type: vector -> dvec
(defun specialize-vec (vector &key check)
  (declare (type vector vector))
  (when check
    (assert (every #'realp vector)))
  (coerce vector 'dvec))

;;@ function-type: (simple-array dvec (*)) -> dvec
;;@ precondition:
;;@  - result is of the same length to any point
;;@  - points are not empty
(defun mean-points (points &optional result)
  (declare (type (vector dvec) points))
  (assert (> (length points) 0))
  (let* ((size (length (aref points 0)))
	 (scale (the double-float (/ 1.0 (length points)))))
    (assert (or (null result)
		(= (length result) size)))
    (setf result
      (if result
          (fill-vec result 0.0)
        (let ((sum (make-dvec size 0.0)))
          (declare (type dvec sum))
          (do-vec (p points :type dvec :return sum)
            (v+ sum p sum)))))
    (v-scale result scale result)))

(in-package :mkl.blas-lapack.test)

(defun one-value (element-type rmax rmin)
  (cond ((member element-type '(single-float double-float integer fixnum))
	 (coerce (+ rmin (random (- rmax rmin))) element-type))
	((and (listp element-type)
	      (eq (first element-type) 'complex))
	 (complex (coerce (+ rmin (random (- rmax rmin))) (second element-type))
		  (coerce (+ rmin (random (- rmax rmin))) (second element-type))))
	(t
	 (error "element-type (~a) not recognized." element-type))))

(defun make-random-array (dimensions
			  &key
			  (element-type 'double-float)
			  (rmax 100)
			  (rmin 0))
  (let* ((arr (make-static-array dimensions :element-type element-type))
	 (total-size (array-total-size arr)))
    (loop for i fixnum below total-size
	  do (setf (row-major-aref arr i) (one-value element-type rmax rmin)))
    arr))

(defun make-random-symmetric-matrix (n &key
				     (element-type 'double-float)
				     (rmax 100)
				     (rmin 0))
  (let ((a (make-static-array (list n n) :element-type element-type)))
    (loop for i fixnum from 0 to (- n 1) do
      (loop for j fixnum from i to (- n 1) do
	(setf (aref a i j) (one-value element-type rmax rmin))
	(unless (= i j)
	  (setf (aref a j i) (aref a i j)))))
    a))

(defun setup-array (array &key (step 1) (rstart 0) (rstep 1))
  (loop for i fixnum below (array-total-size array) by step
	do (progn
	     (setf (row-major-aref array i)
		   (coerce rstart (array-element-type array)))
	     (incf rstart rstep)))
  array)

;;; defcached
(defvar *cache* (make-hash-table))

(defmacro defcached (var value)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (progn
       (setf (gethash ',var *cache*) ',value)
       (defparameter ,var ,value))))

(defmacro reset-value (var)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (symbol-value ',var) ,(gethash var *cache*))))


;;; similar
(defun ~= (a b)
  (etypecase a
    (number
       (or (= a b)
           (<= (/ (abs (- a b))
                  (+ (abs a) (abs b)))
               0.0001)))
    ((vector list)
       (every #'~= a b))
    (array
       (ecase (array-rank a)
         (1 (every #'~= a b))
         (2 (and (equalp (array-dimensions a) (array-dimensions b))
                 (loop for i below (array-dimension a 0)
                       do
                    (loop for j below (array-dimension a 1)
                          do
                       (when (not (~= (aref a i j) (aref b i j)))
                         (return-from ~= nil)))
                       finally (return t))))))))


;;; coerce-to-double
(defun coerce-to-double (v)
  (etypecase v
    (number
       (float v 1d0))
    (vector
       (map 'vector (lambda (v) (coerce-to-double v)) v))
    (list
       (map 'list (lambda (v) (coerce-to-double v)) v))))

;;; 2d-array slice
(defun slice (array x y)
  (assert (not (and (eq x :any) (eq y :any))))
  (cond ((eq x :any)
         (coerce
          (loop for i below (array-dimension array 0)
                collect (aref array i y))
          'vector))
        ((eq y :any)
         (coerce
          (loop for j below (array-dimension array 1)
                collect (aref array x j))
          'vector))
        (t
         (aref array x y))))

;;; round-array
(defun round-to (val precision)
  (let ((precision (float precision 1d0)))
    (* (round val precision) precision)))


(defun round-array (array precision)
  (ecase (array-rank array)
    (1
       (map 'vector (lambda (v) (round-to v precision))
            array))
    (2
       (let ((arr (alexandria:copy-array array)))
         (loop for i below (array-dimension arr 0)
               do
            (loop for j below (array-dimension arr 1)
                  do
               (setf (aref arr i j)
                     (round-to (aref arr i j) precision))))
         arr))))

;;; transpose-list-array
(defun transpose-list-array (list-array &optional (default 0d0))
  (cond ((null list-array) '())
        ((every #'null list-array) '())
        (t
         (cons (mapcar (lambda (x) (or (car x) default)) list-array)
               (transpose-list-array (mapcar #'cdr list-array))))))


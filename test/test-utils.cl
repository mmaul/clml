
(in-package "TEST")

(defun epsilon> (value1 value2 &optional (epsilon *epsilon*))
  (assert (and (numberp value1) (numberp value2)))
  (> epsilon (abs (- value1 value2))))

(defun point-equal (pt1 pt2 &key (test #'epsilon>))
  (every (lambda (v1 v2) (funcall test v1 v2)) pt1 pt2))

(defun assert-a-point-equal (expected target &key (test #'epsilon>))
  (assert-equality (lambda (p1 p2) (point-equal p1 p2 :test test)) expected target))

(defun assert-points-equal (expected target &key (test #'epsilon>))
  (assert-true (loop for pt1 across expected for pt2 across target
                   always (point-equal pt1 pt2 :test test))))

(defun run-all-test-sample ()
  (run-tests test-sample-assoc test-sample-cluster-validation
             test-decision-tree test-sample-expl-smthing
             test-hc test-sample-k-means
             test-sample-k-nn test-sample-linear-regression
             test-nmf test-sample-optics
             test-sample-pca test-random-forest
             test-sample-read-data test-sample-som
             test-sample-spectral-clustering test-sample-stat
             test-sample-svm wss3.kernels wss3.svm test-svr test-one-class-svm test-sample-ts-ar
             test-sample-ts-read-data test-sample-ts-stat
             test-sample-ts-stsp 
             test-sample-hdp-lda
             test-sample-dpm
             test-sample-text-utils))


;;;

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
  (let* ((arr (make-array dimensions :element-type element-type))
	 (total-size (array-total-size arr)))
    (loop for i fixnum below total-size
	  do (setf (row-major-aref arr i) (one-value element-type rmax rmin)))
    arr))

(defun make-random-symmetric-matrix (n &key
				     (element-type 'double-float)
				     (rmax 100)
				     (rmin 0))
  (let ((a (make-array (list n n) :element-type element-type)))
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

;;; similar
(defun ~= (a b &optional (epsilon 0.001))
  (etypecase a
    (number
       (or (= a b)
           (<= (/ (abs (- a b))
                  (+ (abs a) (abs b)))
               epsilon)))
    ((vector list)
       (every (lambda (x y) (~= x y epsilon)) a b))
    (array
       (ecase (array-rank a)
         (1 (every (lambda (x y) (~= x y epsilon)) a b))
         (2 (and (equalp (array-dimensions a) (array-dimensions b))
                 (loop for i below (array-dimension a 0)
                       do
                    (loop for j below (array-dimension a 1)
                          do
                       (when (not (~= (aref a i j) (aref b i j) epsilon))
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
       (let ((arr (matrix:copy-mat array)))
         (loop for i below (array-dimension arr 0)
               do
            (loop for j below (array-dimension arr 1)
                  do
               (setf (aref arr i j)
                     (round-to (aref arr i j) precision))))
         arr))))


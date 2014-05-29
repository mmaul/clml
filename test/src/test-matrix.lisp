(in-package "TEST")

(defvar eyes)
(defvar vecs)
(defvar mat)
(defvar flatmat)
(defvar mean)
(defvar ident)
(defvar avg-scale)
(defvar tmp)

(defun setup-test-matrix-data ()
  (setq eyes
    (hjs.learn.read-data:read-data-from-file "sample/eyes200.sexp"))
  (setf vecs
        (hjs.learn.read-data:dataset-points 
         (hjs.learn.read-data:pick-and-specialize-data
          eyes
          :except '(0 1)
          :data-types (make-list 1680 :initial-element :numeric))))
  (setf mat (make-array (list 200 1680) :element-type 'double-float))
  (setf flatmat (make-array (* 200 1680) :element-type 'double-float))
  (vecs2mat vecs mat)
  (setf mean (let ((mean (vector:make-dvec 1680)))
               #+mkl
               (mkl.blas:dgemv
                "N" 1680 200 1d0 mat 1680 (vector:make-dvec 200 (/ 1d0 200)) 1 0d0 mean 1)
               #-mkl
               (vector:mean-points vecs mean)
               mean))
  (setf ident (vector:make-dvec 200 1d0))
  (setf avg-scale (vector:make-dvec 200 (/ 1d0 200d0)))
  (setf tmp (make-array (list 200 1680) :element-type 'double-float))
  nil)


(define-test matrix-vecs-conversion-test
  (setup-test-matrix-data)
  (assert-true (equalp (mat2vecs (vecs2mat vecs)) vecs))
  (assert-true (equalp (flatmat2vecs (vecs2flatmat vecs) 200) vecs)))


(define-test matrix-transpose-test
  (let* (a b)
    (labels ((reset ()
               (setf a (make-random-array '(10 10)))
               (setf b (copy-mat a))))
      (reset)
      (assert-true (equalp (matrix::transpose a) (matrix::transpose b)))
      (assert-false (equalp (matrix::transpose a) a))
      (reset)
      (assert-false (equalp (matrix::transpose a a) b))
      (assert-true (equalp (matrix::transpose b b) a))
      (assert-true (equalp a b))
      (reset)
      (assert-true (equalp (matrix::transpose (matrix::transpose a a) a) b)))))

(defun m*m-reference (A B &optional result)
  (declare (type dmat A B))
  (assert (= (array-dimension A 1) (array-dimension B 0)))
  (let* ((n (array-dimension A 1))
         (row (array-dimension A 0))
         (col (array-dimension B 1))
         (rm (or result
                 (make-array `(,row ,col) :element-type 'double-float))))
    (declare (type dmat rm) (type fixnum n row col))
    (assert (and (eql row (array-dimension rm 0))
                 (eql col (array-dimension rm 1))))
    (loop for r of-type fixnum below row
          do (loop for c of-type fixnum below col
                   do (setf (aref rm r c)
                            (loop for i of-type fixnum below n
                                  sum (* (the double-float (aref A r i))
                                                    (the double-float (aref B i c)))
                                    into result of-type double-float
                                  finally (return result))))
          finally (return rm))))

(define-test matrix-m*m-test
  (let ((a (make-random-array '(100 200)))
        (b (make-random-array '(200 150)))
        (c (make-random-array '(100 150))))
    (assert-true (equalp (m*m a b) (m*m-reference a b)))
    (assert-true (equalp (m*m a b c) (m*m-reference a b c)))
    (assert-true (equalp (m*m a b) (m*m-reference a b c)))))


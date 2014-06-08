(in-package :mkl.blas.test)

(in-suite root-suite)

(defsuite* blas-test)

;;;; data

;;; 1d array
(defparameter *sx10* (setup-array (make-static-array 10 :element-type 'single-float :warning nil)))
(defparameter *dx10* (setup-array (make-static-array 10 :element-type 'double-float :warning nil)))
(defparameter *cx10* (setup-array (make-static-array 10 :element-type '(complex single-float) :warning nil)
				  :rstep #c(1 2)))
(defparameter *zx10* (setup-array (make-static-array 10 :element-type '(complex double-float) :warning nil)
				  :rstep #c(1 2)))
(defparameter *sy10* (setup-array (make-static-array 10 :element-type 'single-float :warning nil)
				  :rstep -2 :rstart 18))
(defparameter *dy10* (setup-array (make-static-array 10 :element-type 'double-float :warning nil)
				  :rstep -2 :rstart 18))
(defparameter *cy10* (setup-array (make-static-array 10 :element-type '(complex single-float) :warning nil)
				  :rstart #c(18 0) :rstep #c(-2 -1)))
(defparameter *zy10* (setup-array (make-static-array 10 :element-type '(complex double-float) :warning nil)
				  :rstart #c(18 0) :rstep #c(-2 -1)))
(defparameter m 200)
(defparameter n 100)
(defparameter A (setup-array (make-static-array '(100 200) :element-type 'double-float)))
(defparameter B (setup-array (make-static-array '(200 100) :element-type 'double-float)))
(defparameter C (setup-array (make-static-array '(200 200) :element-type 'double-float)))
(defparameter V (setup-array (make-static-array 200 :element-type 'double-float)))

(defixture test-data
  (setup-array *sx10*)
  (setup-array *dx10*)
  (setup-array *cx10* :rstep #c (1 2))
  (setup-array *zx10* :rstep #c (1 2))
  (setup-array *sy10* :rstep -2 :rstart 18)
  (setup-array *dy10* :rstep -2 :rstart 18)
  (setup-array *cy10* :rstep #c (-2 -1) :rstart #c (18 0))
  (setup-array *zy10* :rstep #c (-2 -1) :rstart #c (18 0))
  (setup-array A)
  (setup-array B)
  (setup-array C)
  (setup-array V))


;;; blas1 test
(deftest test-asum ()
  (with-fixture test-data
    (is (= 45 (sasum 10 *sx10* 1)))
    (is (= 45 (dasum 10 *dx10* 1)))
    (is (= 135 (scasum 10 *cx10* 1)))
    (is (= 135 (dzasum 10 *zx10* 1)))
    (is (= 20 (sasum 5 *sx10* 2)))
    (is (= 10 (dasum 5 *dx10* 1)))))

(deftest test-axpy ()
  (with-fixture test-data
    (is (equalp #(18 17 16 15 14 13 12 11 10 9)
                (saxpy 10 1.0f0 *sx10* 1 *sy10* 1))))
  (with-fixture test-data
    (is (equalp #(18 17 16 15 14 13 12 11 10 9)
                (daxpy 10 1.0d0 *dx10* 1 *dy10* 1))))
  (with-fixture test-data
    (is (equalp #(18 #c(17 1) #c(16 2) #c(15 3) #c(14 4) #c(13 5) #c(12 6) #c(11 7) #c(10 8) #c(9 9))
                (caxpy 10 #c(1.0f0 0.0f0) *cx10* 1 *cy10* 1))))
  (with-fixture test-data
    (is (equalp #(18 #c(17 1) #c(16 2) #c(15 3) #c(14 4) #c(13 5) #c(12 6) #c(11 7) #c(10 8) #c(9 9))
                (zaxpy 10 #c(1d0 0d0) *zx10* 1 *zy10* 1))))
  (with-fixture test-data
    (is (equalp #(18 18 18 18 18 18 18 18 2 0)
                (daxpy 8 2.0d0 *dx10* 1 *dy10* 1))))
  (with-fixture test-data
    (is (equalp #(18 16 18 12 18 8 18 4 2 0)
                (daxpy 4 2.0d0 *dx10* 2 *dy10* 2)))))


;;; blas2 tests

(deftest test-dgemv ()
  (with-fixture test-data
    (let ((y (dgemv "N" m n 1d0 a m (make-array m :element-type 'double-float :initial-element (/ 1d0 m)) 1 0d0 v 1)))
      (is (~= 4950.0 (aref y 0)))
      (is (~= 5050.0 (aref y (1- m)))))))


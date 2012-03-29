(in-package :mkl.lapack.test)


(in-suite root-suite)

(defsuite* lapack-test)


;;;
(defun setup-test-data ()
  nil)


;; dgeev
(deftest test-dgeev () 
  (let* ((n 4)
         (a (make-array (list n n) :element-type 'double-float
                        :initial-contents '((0.35d0 0.09d0 -0.44d0 0.25d0)
                                            (0.45d0 0.07d0 -0.33d0 -0.32d0)
                                            (-0.14d0 -0.54d0 -0.03d0 -0.13d0)
                                            (-0.17d0 0.35d0 0.17d0 0.11d0))))
         (wr (make-array n :element-type 'double-float))
         (wi (make-array n :element-type 'double-float))
         (vl (make-array 0 :element-type 'double-float))
         (vr (make-array (* n n) :element-type 'double-float))
         (lwork 136)
         (work (make-array lwork :element-type 'double-float)))
    (multiple-value-bind (a wr wi vl vr work info)
        (dgeev "N" "V" n a n wr wi vl n vr n work lwork 0)
      (declare (ignorable a vl work))
      (is (= info 0))
      (is (~= (aref wr 0) 0.79948d0))
      (is (= (aref wi 0) 0d0))
      (is (~= (subseq vr 0 4)
              #(-0.6550887675124073d0
                -0.5236294609021241d0
                0.5362184613722346d0
                -0.09560677820122966d0)))
      )))

;; dgesv
;;; reference: view-source:http://software.intel.com/sites/products/documentation/hpc/mkl/lapack/mkl_lapack_examples/dgesv_ex.f.htm
(deftest test-dgesv ()
  (let* ((n 5)
         (nrhs 3)
         (a (make-array (list n n) :element-type 'double-float
                        :initial-contents (coerce-to-double 
                                           '((6.8 -2.11 5.66 5.97 8.23)
                                             (-6.05 -3.3 5.36 -4.44 1.08)
                                             (-0.45 2.58 -2.7 0.27 9.04)
                                             (8.32 2.71 4.35 -7.17 2.14)
                                             (-9.67 -5.14 -7.26 6.08 -6.87)))))
         (b (make-array (list 3 n) :element-type 'double-float
                        :initial-contents (coerce-to-double 
                                           '((4.02 6.19 -8.22 -7.57 -3.03)
                                             (-1.56 4.0 -8.67 1.75 2.86)
                                             (9.81 -4.09 -4.57 -8.61 8.99)))))
         (lda n)
         (ldb n)
         (ipiv (make-array n :element-type '(unsigned-byte 32))))
    (multiple-value-bind (a ipiv b info)
        (dgesv n nrhs a lda ipiv b ldb 0)
      (is (= info 0))
      (is (~= (round-array b 0.01d0)
              (make-array (array-dimensions b)
                          :element-type 'double-float
                          :initial-contents
                          (coerce-to-double 
                           '((-0.8 -0.7 0.59 1.32 0.57)
                             (-0.39 -0.55 0.84 -0.1 0.11)
                             (0.96 0.22 1.9 5.36 4.04))))))
      (is (~= (round-array a 0.01d0)
              (make-array (array-dimensions a)
                          :element-type 'double-float
                          :initial-contents
                          (coerce-to-double 
                           '((8.23 0.83 0.69 0.73 -0.26)
                             (1.08 -6.94 -0.67 0.75 0.44) 
                             (9.04 -7.92 -14.18 0.02 -0.59)
                             (2.14 6.55 7.24 -13.82 -0.34)
                             (-6.87 -3.99 -5.19 14.19 -3.43))))))
      (is (equalp ipiv #(5 5 3 4 5))))))

;;; dstevd
;;; program: http://www.nag.co.uk/lapack-ex/examples/source/dstevd-ex.f
;;; data: http://www.nag.co.uk/lapack-ex/examples/data/dstevd-ex.d
;;; result: http://www.nag.co.uk/lapack-ex/examples/results/dstevd-ex.r
(deftest test-dstevd ()
  (let* ((nmax 4)
         (ldz nmax)
         (lwork (+ (* nmax nmax) (* 4 nmax) 1))
         (liwork (+ (* 5 nmax) 3))
         (info 0)
         (n 4)
         (jobz "V")
         (d (make-array nmax
                        :element-type 'double-float
                        :initial-contents
                        (coerce-to-double 
                         '(1 4 9 16))))
         (e (make-array nmax
                        :element-type 'double-float
                        :initial-contents
                        (coerce-to-double
                         '(1 2 3 0)))) 
         (work (make-array lwork :element-type 'double-float))
         (z (make-array (list ldz nmax) :element-type 'double-float))
         (iwork (make-array liwork :element-type '(unsigned-byte 32))))
    (multiple-value-bind (d e z work iwork info)
        (dstevd jobz n d e z ldz work lwork iwork liwork info)
      (declare (ignorable work iwork e))
      (is (= info 0))
      (is (~= (round-array d 0.0001)
              #(0.6476  3.5470  8.6578 17.1477)))
      (is (~= (round-array z 0.0001)
              (make-array (array-dimensions z)
                          :element-type 'double-float
                          :initial-contents
                          (transpose-list-array 
                           (coerce-to-double 
                            '((0.9396  0.3388  -0.0494  0.0034)
                              (-0.3311  0.8628  -0.3781  0.0545)
                              (0.0853 -0.3648  -0.8558  0.3568)
                              (-0.0167  0.0879 0.3497  0.9326))))))))))

;;; dsyevx
;;; program: http://www.nag.co.uk/lapack-ex/examples/source/dsyevx-ex.f
;;; data: http://www.nag.co.uk/lapack-ex/examples/data/dsyevx-ex.d
;;; result: http://www.nag.co.uk/lapack-ex/examples/results/dsyevx-ex.r
(deftest test-dsyevx ()
  (let* ((jobz "V")
         (range "V")
         (vl -1d0)
         (vu 1d0)
         (uplo "U")
         (n 4)
         (il 1)
         (iu 4)
         (ldz (max 1 n))
         (lwork (max 1 (* 8 n)))
         (work (make-array lwork :element-type 'double-float))
         (iwork (make-array (max 1 (* 5 n)) :initial-element 0 :element-type '(unsigned-byte 32)))
         (m 0)
         (w (make-array (max 1 n) :element-type 'double-float))
         (z (make-array `(,n ,ldz) :element-type 'double-float))
         (info 0)
         (ifail (make-array (max 1 n) :initial-element 0 :element-type '(unsigned-byte 32)))
         (lda 4)
         (abstol 0d0)
         (a (make-array (list 4 4) :element-type 'double-float
                        :initial-contents (coerce-to-double 
                                           '((1.0 2.0 3.0 4.0)
                                             (2.0 2.0 3.0 4.0)
                                             (3.0 3.0 3.0 4.0)
                                             (4.0 4.0 4.0 4.0))))))
    (multiple-value-bind (a m w z work ifail info)
        (mkl.lapack:dsyevx jobz range uplo n a lda vl vu il iu abstol m w z
                           ldz work lwork iwork ifail info)
      (setq w (subseq w 0 m)
            z (make-array `(,m ,n) :initial-contents (loop for i below m
                                                         collect (loop for j below n
                                                                     collect (aref z i j)))
                          :element-type 'double-float))
      (is (= info 0))
      (is (~= (round-array w 0.0001)
              #(-0.5146 -0.2943)))
      (is (~= (round-array z 0.0001)
              (make-array (array-dimensions z)
                          :element-type 'double-float
                          :initial-contents
                          (transpose-list-array 
                           (coerce-to-double 
                            '((-0.5144  0.2767)
                              (0.4851 -0.6634)
                              (0.5420  0.6504)
                              (-0.4543 -0.2457))))))))))


(in-package "TEST")

(define-test test-nmf
    (let (matrix w h sports-corpus politics-corpus)
      (setf matrix (sample-matrix 4 5))
      (assert-eql 4 (array-dimension matrix 0))
      (assert-eql 5 (array-dimension matrix 1))
      (multiple-value-setq (w h) (nmf matrix 3 :iteration 80))
      (assert-eql 4 (array-dimension w 0))
      (assert-eql 3 (array-dimension w 1))
      (assert-eql 3 (array-dimension h 0))
      (assert-eql 5 (array-dimension h 1))
      (multiple-value-setq (w h) (nmf matrix 3 :cost-fn :kl))
      (assert-eql 4 (array-dimension w 0))
      (assert-eql 3 (array-dimension w 1))
      (assert-eql 3 (array-dimension h 0))
      (assert-eql 5 (array-dimension h 1))
      (multiple-value-setq (w h) (nmf-sc matrix 3 0.7 :type :left))
      (assert-true (< 0.69 (sparseness (pick-up-column w 0)) 0.71))
      (multiple-value-setq (w h) (nmf-sc matrix 3 0.9 :type :right))
      (assert-true (< 0.89 (sparseness (pick-up-row h 0)) 0.91))
      (assert-eql 4 (length (nmf-clustering matrix 3)))
      (assert-eql 5 (length (nmf-clustering matrix 4 :type :column)))
      (assert-true (<= 0.0 (rho-k matrix 2) 1.0))
      (assert-true (<= 0.0 (rho-k matrix 2 :cost-fn :kl) 1.0))
      (setf matrix (sample-matrix 100 200))
      (assert-false (nmf-analysis matrix 5))
      (assert-false (nmf-analysis matrix 3 :type :column :results 5))
      (setf sports-corpus
	(read-data-from-file "sample/sports-corpus-data" :external-format :utf-8))
      (assert-false (nmf-corpus-analysis sports-corpus 4 :results 5))
      (assert-true (< 20.974 (c^3m-cluster-number sports-corpus) 20.975))
      (setf politics-corpus
	(read-data-from-file "sample/politics-corpus-data" :external-format :utf-8))
      (assert-true (< 15.29 (c^3m-cluster-number politics-corpus) 15.291))
      (assert-false (nmf-search matrix 113 :type :column))
      (assert-false (nmf-corpus-search sports-corpus "¼•" :type :term :results 5))
      (assert-false 
       (nmf-corpus-search sports-corpus "00267800" :type :document :results 5))
      (assert-false
       (nmf-corpus-search politics-corpus "ƒNƒŠƒ“ƒgƒ“" :type :term :results 5))
      ))


#||
;;; Strange sbcl behavior

TEST(11): (loop
           initially (sb-int:set-floating-point-modes :traps '(:overflow :divide-by-zero))
           while (getf (sb-int:get-floating-point-modes) :traps)
           do (run-tests test-sample-nmf)
              (print (sb-int:get-floating-point-modes)))

.......
TEST-SAMPLE-NMF: 30 assertions passed, 0 failed.
(:TRAPS (:OVERFLOW :DIVIDE-BY-ZERO) :ROUNDING-MODE :NEAREST :CURRENT-EXCEPTIONS
 (:UNDERFLOW :INEXACT) :ACCRUED-EXCEPTIONS (:UNDERFLOW :INEXACT) :FAST-MODE NIL) 
.......
TEST-SAMPLE-NMF: 30 assertions passed, 0 failed.
(:TRAPS (:OVERFLOW :DIVIDE-BY-ZERO) :ROUNDING-MODE :NEAREST :CURRENT-EXCEPTIONS
 (:UNDERFLOW :INEXACT) :ACCRUED-EXCEPTIONS (:UNDERFLOW :INEXACT) :FAST-MODE NIL) 
.......
TEST-SAMPLE-NMF: 30 assertions passed, 0 failed.
(:TRAPS (:OVERFLOW :DIVIDE-BY-ZERO) :ROUNDING-MODE :NEAREST :CURRENT-EXCEPTIONS
 (:UNDERFLOW :INEXACT) :ACCRUED-EXCEPTIONS (:UNDERFLOW :INEXACT) :FAST-MODE NIL) 
.......
TEST-SAMPLE-NMF: 30 assertions passed, 0 failed.
(:TRAPS (:OVERFLOW :DIVIDE-BY-ZERO) :ROUNDING-MODE :NEAREST :CURRENT-EXCEPTIONS
 (:UNDERFLOW :INEXACT) :ACCRUED-EXCEPTIONS (:UNDERFLOW :INEXACT) :FAST-MODE NIL) 
.......
TEST-SAMPLE-NMF: 30 assertions passed, 0 failed.
(:TRAPS (:OVERFLOW :DIVIDE-BY-ZERO) :ROUNDING-MODE :NEAREST :CURRENT-EXCEPTIONS
 (:UNDERFLOW :INEXACT) :ACCRUED-EXCEPTIONS (:UNDERFLOW :INEXACT) :FAST-MODE NIL) 
.......
TEST-SAMPLE-NMF: 30 assertions passed, 0 failed.
(:TRAPS (:OVERFLOW :DIVIDE-BY-ZERO) :ROUNDING-MODE :NEAREST :CURRENT-EXCEPTIONS
 (:UNDERFLOW :INEXACT) :ACCRUED-EXCEPTIONS (:UNDERFLOW :INEXACT) :FAST-MODE NIL) 
.......
TEST-SAMPLE-NMF: 30 assertions passed, 0 failed.
(:TRAPS (:OVERFLOW :DIVIDE-BY-ZERO) :ROUNDING-MODE :NEAREST :CURRENT-EXCEPTIONS
 (:UNDERFLOW :INEXACT) :ACCRUED-EXCEPTIONS (:UNDERFLOW :INEXACT) :FAST-MODE NIL) 
.......
TEST-SAMPLE-NMF: 30 assertions passed, 0 failed.
(:TRAPS (:OVERFLOW :DIVIDE-BY-ZERO) :ROUNDING-MODE :NEAREST :CURRENT-EXCEPTIONS
 (:UNDERFLOW :INEXACT) :ACCRUED-EXCEPTIONS (:UNDERFLOW :INEXACT) :FAST-MODE NIL) 
.......
TEST-SAMPLE-NMF: 30 assertions passed, 0 failed.
(:TRAPS NIL :ROUNDING-MODE :NEAREST :CURRENT-EXCEPTIONS (:INEXACT)
 :ACCRUED-EXCEPTIONS (:INEXACT) :FAST-MODE NIL) 
NIL
TEST(12): (lisp-implementation-version)

"1.0.28"
TEST(13): (lisp-implementation-type)

"SBCL"

||#
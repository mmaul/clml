		
(in-package "TEST")

(define-test test-hc
    (let (seiseki d-matrix cophenetic-matrix merge-matrix)
      (setf seiseki (pick-and-specialize-data 
		     (read-data-from-file "sample/seiseki.csv" :type :csv :csv-type-spec
					  '(string double-float double-float double-float double-float double-float))
		     :range '(1 2 3 4 5) :data-types '(:numeric :numeric :numeric :numeric :numeric)))
      (setf d-matrix (distance-matrix (numeric-matrix seiseki)))
      (assert-eql 68.65857557508748d0 (aref d-matrix 0 1))
      ;;average
      (assert-true (multiple-value-setq (cophenetic-matrix merge-matrix) (cophenetic-matrix d-matrix #'hc-average)))
      (assert-eql 69.92295649225116d0 (aref cophenetic-matrix 0 1))
      (assert-equalp #2A((-5 -1) (-4 -2) (2 0) (-6 1) (-3 3) (4 5)) merge-matrix)
      ;;ward
      (multiple-value-setq (cophenetic-matrix merge-matrix) (cophenetic-matrix d-matrix #'hc-ward))
      (assert-eql 150.95171411164776d0 (aref cophenetic-matrix 0 1))
      (assert-equalp #2A((-5 -1) (-4 -2) (2 0) (-6 1) (-3 3) (4 5)) merge-matrix)
      ;;single
      (multiple-value-setq (cophenetic-matrix merge-matrix) (cophenetic-matrix d-matrix #'hc-single))
      (assert-eql 54.31390245600109d0 (aref cophenetic-matrix 0 1))
      (assert-equalp #2A((-5 -1) (-4 -2) (2 0) (-6 1) (-3 3) (4 5)) merge-matrix)
      ;;complete
      (multiple-value-setq (cophenetic-matrix merge-matrix) (cophenetic-matrix d-matrix #'hc-complete))
      (assert-eql 91.53141537199127d0 (aref cophenetic-matrix 0 1))
      (assert-equalp #2A((-5 -1) (-4 -2) (2 0) (-6 1) (-3 3) (4 5)) merge-matrix)
      ;;centroid
      (multiple-value-setq (cophenetic-matrix merge-matrix) (cophenetic-matrix d-matrix #'hc-centroid))
      (assert-eql 44.02758328256393d0 (aref cophenetic-matrix 0 1))
      (assert-equalp #2A((-5 -1) (-4 -2) (2 0) (-6 1) (-3 3) (4 5)) merge-matrix)
      ;;median
      (multiple-value-setq (cophenetic-matrix merge-matrix) (cophenetic-matrix d-matrix #'hc-median))
      (assert-eql 45.42216730738696d0 (aref cophenetic-matrix 0 1))
      (assert-equalp #2A((-5 -1) (-4 -2) (2 0) (-6 1) (-3 3) (4 5)) merge-matrix)
      (assert-equalp #(1 2 1 3 1 2 2) (cutree 3 merge-matrix))
      ))

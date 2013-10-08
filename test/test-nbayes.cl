(in-package "TEST")

(define-test test-nbayes
    
    (setf bow-train (dataset-points (read-data-from-file "sample/bow-train.csv"
							 :type :csv :csv-type-spec `(,@(loop repeat 928 collect 'double-float) string))))
  
  (setf bow-test (dataset-points (read-data-from-file "sample/bow-test.csv"
						      :type :csv :csv-type-spec `(,@(loop repeat 928 collect 'double-float) string))))
  
  (assert-eql 84 (length bow-train))
  
  (assert-eql 70 (length bow-test))
  
  (setf mbnb-learner (apply #'make-mbnb-learner (mbnb-learn bow-train)))
  
  (assert-equal "cobol" (funcall mbnb-learner (svref bow-test 0)))
  
  (assert-equal "cobol" (funcall mbnb-learner (svref bow-test 1)))
  
  (assert-equal "cobol" (funcall mbnb-learner (svref bow-test 2)))
  
  (assert-equal "lisp" (funcall mbnb-learner (svref bow-test 40)))
  
  (assert-equal "lisp" (funcall mbnb-learner (svref bow-test 41)))
  
  (assert-equal "lisp" (funcall mbnb-learner (svref bow-test 42)))
  
  (setf mnb-learner (apply #'make-mnb-learner (mnb-learn bow-train)))
  
  (assert-equal "cobol" (funcall mnb-learner (svref bow-test 0)))
  
  (assert-equal "cobol" (funcall mnb-learner (svref bow-test 1)))
  
  (assert-equal "cobol" (funcall mnb-learner (svref bow-test 2)))
  
  (assert-equal "lisp" (funcall mnb-learner (svref bow-test 40)))
  
  (assert-equal "lisp" (funcall mnb-learner (svref bow-test 41)))
  
  (assert-equal "lisp" (funcall mnb-learner (svref bow-test 42)))
  )
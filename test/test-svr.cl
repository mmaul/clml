
(in-package "TEST")

(define-test test-svr
    (let (svm-bc-train svm-bc-test training-vector test-vector linear-svr rbf-kernel rbf-svr poly-svr)
       (setf svm-bc-train
	(pick-and-specialize-data
	 (read-data-from-file "sample/bc-train-for-svm.csv"
			      :type :csv
			      :csv-type-spec 
			      (make-list 10 :initial-element 'double-float))
	 :data-types (make-list 10 :initial-element :numeric)))
  
      (setf svm-bc-test
	(pick-and-specialize-data
	 (read-data-from-file "sample/bc-test-for-svm.csv"
			      :type :csv
			      :csv-type-spec 
			      (make-list 10 :initial-element 'double-float))
	 :data-types (make-list 10 :initial-element :numeric)))
  
      (setf training-vector (dataset-points svm-bc-train))
      
      (setf test-vector (dataset-points svm-bc-test))
      
      (setf linear-svr (make-svr-learner training-vector (make-linear-kernel) :c 0.1d0 :epsilon 0.01d0))
      
      (assert-eql  0.9828423479679607d0 (funcall linear-svr (svref test-vector 0)))
      
      (assert-eql -0.8730554456654958d0 (funcall linear-svr (svref test-vector 7)))
      
      (assert-eql 0.172803340477296d0 (svr-validation linear-svr test-vector))
      
      (setf rbf-kernel (make-rbf-kernel :gamma 0.05d0))
      
      (setf rbf-svr (make-svr-learner training-vector rbf-kernel :c 1 :epsilon 0.01d0))
      
      (assert-equality '~= 1.007050489405549d0 (funcall rbf-svr (svref test-vector 0)))
      
      (assert-equality '~= -0.9562934949702806d0 (funcall rbf-svr (svref test-vector 7)))
      
      (assert-equality '~= 0.1143902841901142d0 (svr-validation rbf-svr test-vector))
      ))

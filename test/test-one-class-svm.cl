
(in-package "TEST")

(define-test test-one-class-svm
    (let (svm-bc-train data-vector one-class-svm)
       (setf svm-bc-train
	(pick-and-specialize-data
	 (read-data-from-file "sample/bc-train-for-svm.csv"
			      :type :csv
			      :csv-type-spec 
			      (make-list 10 :initial-element 'double-float))
	 :data-types (make-list 10 :initial-element :numeric)))
  
       (setf data-vector (dataset-points svm-bc-train))
       
       (setf one-class-svm (one-class-svm data-vector :nu 0.01d0 :gamma 0.005d0))
       
       (assert-eql 1.0d0 (funcall one-class-svm (svref data-vector 0)))

       (assert-eql 13 (loop for data across data-vector count (= -1.0d0 (funcall one-class-svm data))))
       
       (setf one-class-svm (one-class-svm data-vector :nu 0.1d0 :gamma 0.05d0))
       
       (assert-eql 66 (loop for data across data-vector count (= -1.0d0 (funcall one-class-svm data))))
       
       (setf one-class-svm (one-class-svm data-vector :nu 0.9d0 :gamma 0.5d0))
       
       (assert-eql 304 (loop for data across data-vector count (= -1.0d0 (funcall one-class-svm data))))
      
      ))
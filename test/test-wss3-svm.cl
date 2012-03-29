
(in-package "TEST")

(define-test wss3.kernels
    (let (z-1 z-2 rbf-kernel poly-kernel)
      
      (setf z-1 (make-array 4 :element-type 'double-float
			    :initial-contents #(-1.0d0 7.0d0 4.0d0 1.0d0))
	    
	    z-2 (make-array 4 :element-type 'double-float
			    :initial-contents #(2.0d0 5.0d0 3.0d0 -1.0d0)))
      
      (assert-true (= 45.0d0 (svm.wss3::call-kernel-function-uncached (make-linear-kernel) z-1 z-2)))
      
      (setf rbf-kernel (make-rbf-kernel :gamma 1.0d0))
      
      (assert-true (> 0.00001d0 (- 8.315287191035679e-7 (svm.wss3::call-kernel-function-uncached rbf-kernel z-1 z-2))))
      
      (setf poly-kernel (make-polynomial-kernel :gamma 1.0d0 :r 0.0d0 :d 2))
      
      (assert-true (= 2025.0d0 (svm.wss3::call-kernel-function-uncached poly-kernel z-1 z-2)))
      
      (setf poly-kernel (make-polynomial-kernel :gamma 1.0d0 :r 1.0d0 :d 2))
      
      (assert-true (= 2116.0d0 (svm.wss3::call-kernel-function-uncached poly-kernel z-1 z-2)))
      
      (setf poly-kernel (make-polynomial-kernel :gamma 2.0d0 :r -89.0d0 :d 2))
      
      (assert-true (= 1.0d0 (svm.wss3::call-kernel-function-uncached poly-kernel z-1 z-2)))
      
      (setf poly-kernel (make-polynomial-kernel :gamma 3.0d0 :r -133.0d0 :d 3))
      
      (assert-true (= 8.0d0 (svm.wss3::call-kernel-function-uncached poly-kernel z-1 z-2)))
      ))


(define-test wss3.svm
    (let (svm-bc-train svm-bc-test training-vector test-vector linear-svm rbf-kernel rbf-svm poly-svm)
      
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
      
      (setf linear-svm (make-svm-learner training-vector (make-linear-kernel) :c 1))
      
      (assert-true (= 1.0d0 (funcall linear-svm (svref test-vector 0))))
      
      (assert-true (= -1.0d0 (funcall linear-svm (svref test-vector 7))))
      
      (assert-true (= 4 (length (svm-validation linear-svm test-vector))))
      
      (setf rbf-kernel (make-rbf-kernel :gamma 0.05d0))
      
      (setf rbf-svm (make-svm-learner training-vector rbf-kernel :c 100 :weight 0.5))
      
      (assert-true (= 1.0d0 (funcall rbf-svm (svref test-vector 0))))
      
      (assert-true (= -1.0d0 (funcall rbf-svm (svref test-vector 7))))
      
      (assert-true (= 4 (length (svm-validation rbf-svm test-vector))))
      
      (setf poly-svm (make-svm-learner training-vector (make-polynomial-kernel :gamma 1.0d0 :r 0.0d0 :d 2) :c 1))
      
      (assert-true (= 1.0 (funcall poly-svm (svref test-vector 0))))
      
      (assert-true (= -1.0 (funcall poly-svm (svref test-vector 7))))
      
      (assert-true (= 4 (length (svm-validation poly-svm test-vector))))
      ))
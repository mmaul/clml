
(in-package "TEST")

(define-test svm.smo.kernels
  (assert-true (setf z-1 (make-array 4 :element-type 'double-float
                                     :initial-contents #(-1.0d0 7.0d0 4.0d0 1.0d0))
                     z-2 (make-array 4 :element-type 'double-float
                                     :initial-contents #(2.0d0 5.0d0 3.0d0 -1.0d0))))
  (assert-true (= 45.0d0 (svm.smo::call-kernel-function-with-vectors #'svm.smo::linear-kernel z-1 z-2)))
  (assert-true (setf rbf-kernel (svm.smo::make-rbf-kernel :gamma 1.0d0)))
  (assert-true (> 0.00001d0 (- 8.315287191035679e-7 (svm.smo::call-kernel-function-with-vectors rbf-kernel z-1 z-2))))
  (assert-true (setf poly-kernel (svm.smo::make-polynomial-kernel :gamma 1.0d0 :r 0.0d0 :d 2)))
  (assert-true (= 2025.0d0 (svm.smo::call-kernel-function-with-vectors poly-kernel z-1 z-2)))
  (assert-true (setf poly-kernel (svm.smo::make-polynomial-kernel :gamma 1.0d0 :r 1.0d0 :d 2)))
  (assert-true (= 2116.0d0 (svm.smo::call-kernel-function-with-vectors poly-kernel z-1 z-2)))
  (assert-true (setf poly-kernel (svm.smo::make-polynomial-kernel :gamma 2.0d0 :r -89.0d0 :d 2)))
  (assert-true (= 1.0d0 (svm.smo::call-kernel-function-with-vectors poly-kernel z-1 z-2)))
  (assert-true (setf poly-kernel (svm.smo::make-polynomial-kernel :gamma 3.0d0 :r -133.0d0 :d 3)))
  (assert-true (= 8.0d0 (svm.smo::call-kernel-function-with-vectors poly-kernel z-1 z-2)))
  )


(define-test smo.svm
  (assert-true (setf svm-bc-train
                     (pick-and-specialize-data
                      (read-data-from-file "sample/bc-train-for-svm.csv"
                                           :type :csv
                                           :csv-type-spec 
                                           (make-list 10 :initial-element 'double-float))
                      :data-types (make-list 10 :initial-element :numeric))))
  
  (assert-true (setf svm-bc-test
                     (pick-and-specialize-data
                      (read-data-from-file "sample/bc-test-for-svm.csv"
                                           :type :csv
                                           :csv-type-spec 
                                           (make-list 10 :initial-element 'double-float))
                      :data-types (make-list 10 :initial-element :numeric))))
  
  (assert-true (setf training-vector (dataset-points svm-bc-train)))
  (assert-true (setf test-vector (dataset-points svm-bc-test)))
  (assert-true (setf linear-svm (svm.smo::make-svm-learner training-vector #'svm.smo::linear-kernel 1)))
  (assert-true (= 1.0 (funcall linear-svm (svref test-vector 0))))
  (assert-true (= -1.0 (funcall linear-svm (svref test-vector 7))))
  (assert-true (= 4 (length (svm.smo::svm-validation linear-svm test-vector))))
  (assert-true (setf rbf-kernel (svm.smo::make-rbf-kernel :gamma 0.05)))
  (assert-true (setf rbf-svm (svm.smo::make-svm-learner training-vector rbf-kernel 100)))
  (assert-true (= 1.0 (funcall rbf-svm (svref test-vector 0))))
  (assert-true (= -1.0 (funcall rbf-svm (svref test-vector 7))))
  (assert-true (= 4 (length (svm.smo::svm-validation rbf-svm test-vector))))
  (assert-true (setf poly-svm (svm.smo::make-svm-learner training-vector (svm.smo::make-polynomial-kernel :gamma 1.0d0 :r 0.0d0 :d 2) 1)))
  (assert-true (= 1.0 (funcall poly-svm (svref test-vector 0))))
  (assert-true (= -1.0 (funcall poly-svm (svref test-vector 7))))
  (assert-true (= 4 (length (svm.smo::svm-validation poly-svm test-vector))))
  )

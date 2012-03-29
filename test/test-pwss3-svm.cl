(defpackage :test-pwss3-svm
  (:use :common-lisp
	:lisp-unit
	:hjs.learn.read-data
	:pwss3-svm))

(in-package test-pwss3-svm)

(define-test kernels
  (assert-true (setf z-1 (make-array 4 :element-type 'double-float
                                     :initial-contents #(-1.0d0 7.0d0 4.0d0 1.0d0))
                     z-2 (make-array 4 :element-type 'double-float
                                     :initial-contents #(2.0d0 5.0d0 3.0d0 -1.0d0))))
  (assert-true (= 45.0d0 (pwss3-svm::call-kernel-function-uncached (make-linear-kernel) z-1 z-2)))
  (assert-true (setf rbf-kernel (make-rbf-kernel :gamma 1.0d0)))
  (assert-true (> 0.00001d0 (- 8.315287191035679e-7 (pwss3-svm::call-kernel-function-uncached rbf-kernel z-1 z-2))))
  (assert-true (setf poly-kernel (make-polynomial-kernel :gamma 1.0d0 :r 0.0d0 :d 2)))
  (assert-true (= 2025.0d0 (pwss3-svm::call-kernel-function-uncached poly-kernel z-1 z-2)))
  (assert-true (setf poly-kernel (make-polynomial-kernel :gamma 1.0d0 :r 1.0d0 :d 2)))
  (assert-true (= 2116.0d0 (pwss3-svm::call-kernel-function-uncached poly-kernel z-1 z-2)))
  (assert-true (setf poly-kernel (make-polynomial-kernel :gamma 2.0d0 :r -89.0d0 :d 2)))
  (assert-true (= 1.0d0 (pwss3-svm::call-kernel-function-uncached poly-kernel z-1 z-2)))
  (assert-true (setf poly-kernel (make-polynomial-kernel :gamma 3.0d0 :r -133.0d0 :d 3)))
  (assert-true (= 8.0d0 (pwss3-svm::call-kernel-function-uncached poly-kernel z-1 z-2)))
  )


(define-test svm
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
  (assert-true (setf linear-svm (make-svm-learner training-vector (make-linear-kernel) 1)))
  (assert-true (= 1.0d0 (funcall linear-svm (svref test-vector 0))))
  (assert-true (= -1.0d0 (funcall linear-svm (svref test-vector 7))))
  (assert-true (= 4 (length (svm-validation linear-svm test-vector))))
  (assert-true (setf rbf-kernel (make-rbf-kernel :gamma 0.05d0)))
  (assert-true (setf rbf-svm (make-svm-learner training-vector rbf-kernel 100)))
  (assert-true (= 1.0d0 (funcall rbf-svm (svref test-vector 0))))
  (assert-true (= -1.0d0 (funcall rbf-svm (svref test-vector 7))))
  (assert-true (= 4 (length (svm-validation rbf-svm test-vector))))
  (assert-true (setf poly-svm (make-svm-learner training-vector (make-polynomial-kernel :gamma 1.0d0 :r 0.0d0 :d 2) 1)))
  (assert-true (= 1.0 (funcall poly-svm (svref test-vector 0))))
  (assert-true (= -1.0 (funcall poly-svm (svref test-vector 7))))
  (assert-true (= 4 (length (svm-validation poly-svm test-vector))))
  (assert-true (setf train-data (pick-and-specialize-data (read-data-from-file "sample/svm-benchmark-train.csv" :type :csv :csv-type-spec (make-list 24 :initial-element 'double-float)
									       :external-format :utf-8) :data-types (make-list 24 :initial-element :numeric))))
  (assert-true (setf test-data (pick-and-specialize-data (read-data-from-file "sample/svm-benchmark-test.csv" :type :csv :csv-type-spec (make-list 24 :initial-element 'double-float)
									      :external-format :utf-8) :data-types (make-list 24 :initial-element :numeric))))
  (assert-true (setf training-vector (dataset-points train-data)))
  (assert-true (setf test-vector (dataset-points test-data)))
  (assert-true (setf rbf (make-rbf-kernel :gamma 0.5d0)))
  (assert-true (setf svm (make-svm-learner training-vector rbf 10)))
  (assert-true (multiple-value-setq (results accuracy) (svm-validation svm test-vector)))
  (assert-true (= accuracy 94.85150853161717d0))
  )

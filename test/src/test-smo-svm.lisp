
(in-package :clml.test)

(define-test svm.smo.kernels
  (let (poly-kernel rbf-kernel z-1 z-2)
    (assert (setf z-1 (make-array 4 :element-type 'double-float
                                       :initial-contents #(-1.0d0 7.0d0 4.0d0 1.0d0))
                       z-2 (make-array 4 :element-type 'double-float
                                       :initial-contents #(2.0d0 5.0d0 3.0d0 -1.0d0))))
    (assert-true (= 45.0d0 (clml.svm.smo::call-kernel-function-with-vectors #'clml.svm.smo::linear-kernel z-1 z-2)))
    (assert (setf rbf-kernel (clml.svm.smo::make-rbf-kernel :gamma 1.0d0)))
    (assert-true (> 0.00001d0 (- 8.315287191035679e-7 (clml.svm.smo::call-kernel-function-with-vectors rbf-kernel z-1 z-2))))
    (assert (setf poly-kernel (clml.svm.smo::make-polynomial-kernel :gamma 1.0d0 :r 0.0d0 :d 2)))
    (assert-true (= 2025.0d0 (clml.svm.smo::call-kernel-function-with-vectors poly-kernel z-1 z-2)))
    (assert (setf poly-kernel (clml.svm.smo::make-polynomial-kernel :gamma 1.0d0 :r 1.0d0 :d 2)))
    (assert-true (= 2116.0d0 (clml.svm.smo::call-kernel-function-with-vectors poly-kernel z-1 z-2)))
    (assert (setf poly-kernel (clml.svm.smo::make-polynomial-kernel :gamma 2.0d0 :r -89.0d0 :d 2)))
    (assert-true (= 1.0d0 (clml.svm.smo::call-kernel-function-with-vectors poly-kernel z-1 z-2)))
    (assert (setf poly-kernel (clml.svm.smo::make-polynomial-kernel :gamma 3.0d0 :r -133.0d0 :d 3)))
    (assert-true (= 8.0d0 (clml.svm.smo::call-kernel-function-with-vectors poly-kernel z-1 z-2))))
  )


(define-test smo.svm
  (let (linear-svm poly-svm rbf-kernel rbf-svm svm-bc-test svm-bc-train
                   test-vector training-vector)
    (assert (setf svm-bc-train
                       (pick-and-specialize-data
                        (read-data-from-file (clml.utility.data:fetch "https://mmaul.github.io/clml.data/sample/bc-train-for-svm.csv")
                                             :type :csv
                                             :csv-type-spec
                                             (make-list 10 :initial-element 'double-float)
                                             :external-format :utf-8)
                        :data-types (make-list 10 :initial-element :numeric))))

    (assert (setf svm-bc-test
                       (pick-and-specialize-data
                        (read-data-from-file (clml.utility.data:fetch "https://mmaul.github.io/clml.data/sample/bc-test-for-svm.csv")
                                             :type :csv
                                             :csv-type-spec
                                             (make-list 10 :initial-element 'double-float)
                                             :external-format :utf-8)
                        :data-types (make-list 10 :initial-element :numeric))))

    (assert (setf training-vector (dataset-points svm-bc-train)))
    (assert (setf test-vector (dataset-points svm-bc-test)))
    (assert (setf linear-svm (clml.svm.smo::make-svm-learner training-vector #'clml.svm.smo::linear-kernel 1)))
    (assert-true (= 1.0 (funcall linear-svm (svref test-vector 0))))
    (assert-true (= -1.0 (funcall linear-svm (svref test-vector 7))))
    (assert-true (= 4 (length (clml.svm.smo::svm-validation linear-svm test-vector))))
    (assert (setf rbf-kernel (clml.svm.smo::make-rbf-kernel :gamma 0.05)))
    (assert (setf rbf-svm (clml.svm.smo::make-svm-learner training-vector rbf-kernel 100)))
    (assert (= 1.0 (funcall rbf-svm (svref test-vector 0))))
    (assert-true (= -1.0 (funcall rbf-svm (svref test-vector 7))))
    (assert-true (= 4 (length (clml.svm.smo::svm-validation rbf-svm test-vector))))
    (assert (setf poly-svm (clml.svm.smo::make-svm-learner training-vector (clml.svm.smo::make-polynomial-kernel :gamma 1.0d0 :r 0.0d0 :d 2) 1)))
    (assert-true (= 1.0 (funcall poly-svm (svref test-vector 0))))
    (assert-true (= -1.0 (funcall poly-svm (svref test-vector 7))))
    (assert-true (= 4 (length (clml.svm.smo::svm-validation poly-svm test-vector)))))
  )

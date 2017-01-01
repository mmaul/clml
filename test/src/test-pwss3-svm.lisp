(in-package :clml.test)
(define-test test-pwss3-kernels
  (let (poly-kernel rbf-kernel z-1 z-2)
    (assert (setf z-1 (make-array 4 :element-type 'double-float
                                       :initial-contents #(-1.0d0 7.0d0 4.0d0 1.0d0))
                       z-2 (make-array 4 :element-type 'double-float
                                       :initial-contents #(2.0d0 5.0d0 3.0d0 -1.0d0))))
    (assert-true (= 45.0d0 (clml.svm.pwss3::call-kernel-function-uncached (clml.svm.pwss3:make-linear-kernel) z-1 z-2)))
    (assert (setf rbf-kernel (clml.svm.pwss3::make-rbf-kernel :gamma 1.0d0)))
    (assert-true (> 0.00001d0 (- 8.315287191035679e-7 (clml.svm.pwss3::call-kernel-function-uncached rbf-kernel z-1 z-2))))
    (assert (setf poly-kernel (clml.svm.pwss3::make-polynomial-kernel :gamma 1.0d0 :r 0.0d0 :d 2)))
    (assert-true (= 2025.0d0 (clml.svm.pwss3::call-kernel-function-uncached poly-kernel z-1 z-2)))
    (assert (setf poly-kernel (clml.svm.pwss3::make-polynomial-kernel :gamma 1.0d0 :r 1.0d0 :d 2)))
    (assert-true (= 2116.0d0 (clml.svm.pwss3::call-kernel-function-uncached poly-kernel z-1 z-2)))
    (assert (setf poly-kernel (clml.svm.pwss3::make-polynomial-kernel :gamma 2.0d0 :r -89.0d0 :d 2)))
    (assert-true (= 1.0d0 (clml.svm.pwss3::call-kernel-function-uncached poly-kernel z-1 z-2)))
    (assert (setf poly-kernel (clml.svm.pwss3::make-polynomial-kernel :gamma 3.0d0 :r -133.0d0 :d 3)))
    (assert-true (= 8.0d0 (clml.svm.pwss3::call-kernel-function-uncached poly-kernel z-1 z-2))))
  )


(define-test test-pwss3-svm
  (let (accuracy linear-svm poly-svm rbf rbf-kernel rbf-svm results
        svm svm-bc-test svm-bc-train test-data test-vector train-data
        training-vector)
    (assert (setf svm-bc-train
                       (pick-and-specialize-data
                        (read-data-from-file (clml.utility.data:fetch "https://mmaul.github.io/clml.data/sample/bc-train-for-svm.csv")
                                             :type :csv
                                             :csv-type-spec
                                             (make-list 10 :initial-element 'double-float))
                        :data-types (make-list 10 :initial-element :numeric))))

    (assert (setf svm-bc-test
                       (pick-and-specialize-data
                        (read-data-from-file (clml.utility.data:fetch "https://mmaul.github.io/clml.data/sample/bc-test-for-svm.csv")
                                             :type :csv
                                             :csv-type-spec
                                             (make-list 10 :initial-element 'double-float))
                        :data-types (make-list 10 :initial-element :numeric))))

    (assert (setf training-vector (dataset-points svm-bc-train)))
    (assert (setf test-vector (dataset-points svm-bc-test)))

    (assert (setf linear-svm (clml.svm.pwss3::make-svm-learner training-vector (clml.svm.pwss3:make-linear-kernel) :c 1))) ;Not sure whether is c or weight
    (assert-true (= 1.0d0 (funcall linear-svm (svref test-vector 0))))
    (assert-true (= -1.0d0 (funcall linear-svm (svref test-vector 7))))
    (assert-true (= 4 (length (clml.svm.pwss3::svm-validation linear-svm test-vector))))

    (assert (setf rbf-kernel (clml.svm.pwss3::make-rbf-kernel :gamma 0.05d0)))
    (assert (setf rbf-svm (clml.svm.pwss3:make-svm-learner training-vector rbf-kernel :c 100)))
    (assert-true (= 1.0d0 (funcall rbf-svm (svref test-vector 0))))
    (assert-true (= -1.0d0 (funcall rbf-svm (svref test-vector 7))))
    (assert-true (= 4 (length (clml.svm.pwss3:svm-validation rbf-svm test-vector))))
    (assert (setf poly-svm (clml.svm.pwss3::make-svm-learner training-vector (clml.svm.pwss3:make-polynomial-kernel :gamma 1.0d0 :r 0.0d0 :d 2) :c 1)))
    (assert-true (= 1.0 (funcall poly-svm (svref test-vector 0))))
    (assert-true (= -1.0 (funcall poly-svm (svref test-vector 7))))
    (assert-true (= 4 (length (clml.svm.pwss3:svm-validation poly-svm test-vector))))
    (assert (setf train-data (pick-and-specialize-data (read-data-from-file (clml.utility.data:fetch "https://mmaul.github.io/clml.data/sample/svm-benchmark-train.csv") :type :csv :csv-type-spec (make-list 24 :initial-element 'double-float)
                                                                                 :external-format :utf-8) :data-types (make-list 24 :initial-element :numeric))))
    (assert (setf test-data (pick-and-specialize-data (read-data-from-file (clml.utility.data:fetch "https://mmaul.github.io/clml.data/sample/svm-benchmark-test.csv") :type :csv :csv-type-spec (make-list 24 :initial-element 'double-float)
                                                                                :external-format :utf-8) :data-types (make-list 24 :initial-element :numeric))))
    (assert (setf training-vector (dataset-points train-data)))
    (assert (setf test-vector (dataset-points test-data)))
    (assert (setf rbf (clml.svm.pwss3:make-rbf-kernel :gamma 0.5d0)))
    (assert (setf svm (clml.svm.pwss3:make-svm-learner training-vector rbf :c 10)))
    (assert (multiple-value-setq (results accuracy) (clml.svm.pwss3:svm-validation svm test-vector)))
    (assert-true (= accuracy 94.85150853161717d0))

    )
  )

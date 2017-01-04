;-*- coding: utf-8 -*-
(in-package :clml.test)

(define-test test-random-forest
    (let (syobu bc-train bc-test cars forest query)

      (setf syobu (read-data-from-file (clml.utility.data:fetch "https://mmaul.github.io/clml.data/sample/syobu.csv") :type :csv :csv-type-spec '(string integer integer integer integer)))

      (setf bc-train (read-data-from-file (clml.utility.data:fetch "https://mmaul.github.io/clml.data/sample/bc.train.csv") :type :csv
                                          :csv-type-spec (append (loop for i below 9 collect 'double-float) '(string))))

      (setf bc-test (read-data-from-file (clml.utility.data:fetch "https://mmaul.github.io/clml.data/sample/bc.test.csv") :type :csv
                                         :csv-type-spec (append (loop for i below 9 collect 'double-float) '(string))))

      (setf cars (read-data-from-file (clml.utility.data:fetch "https://mmaul.github.io/clml.data/sample/cars.csv") :type :csv :csv-type-spec '(double-float double-float)))
      (let ((lparallel:*kernel* (lparallel:make-kernel 4)))
        (setf forest (make-random-forest syobu "種類" :feature-bag-size :quarter-set-size :data-bag-size :quarter-set-size))

        (assert-eql 500 (length forest))
        (let ((stream (make-string-output-stream)))
          (print-decision-tree (aref forest 0) stream 0)
          (assert-equal "[" (subseq (get-output-stream-string stream) 0 1) ));;random decision-tree

        (setf query #("?" 53.0d0 30.0d0 33.0d0 10.0d0))

        (assert-true (string= "Versicolor" (predict-forest query syobu forest)))

        (setf forest (make-random-forest bc-train "Class" :balance t))

        (assert-eql 4 (length (forest-validation bc-test "Class" forest)))

        (setf forest (make-regression-forest bc-train "Cell.size" :tree-number 400))

        (assert-eql 400 (length forest))
        (let ((stream (make-string-output-stream)))
          (print-regression-tree (aref forest 0) stream)
          (assert-equal "[" (subseq (get-output-stream-string stream) 0 1)));;random regression-tree

        (assert-true (< 1.0d0 (predict-regression-forest (svref (dataset-points bc-test) 0) bc-train forest) 1.1d0))

        (assert-true (< 1.5d0 (regression-forest-validation bc-test "Cell.size" forest) 1.7d0)))
      ))

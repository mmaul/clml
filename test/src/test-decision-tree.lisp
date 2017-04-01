;-*- coding: utf-8 -*-
(in-package :clml.test)
(define-test test-decision-tree
    (let (syobu bc-train bc-test cars tree query)

      (setf syobu (read-data-from-file (clml.utility.data:fetch "https://mmaul.github.io/clml.data/sample/syobu.csv") :type :csv
                                                     :csv-type-spec
                                                    '(string integer integer integer integer)))
       (setf bc-train (read-data-from-file (clml.utility.data:fetch "https://mmaul.github.io/clml.data/sample/bc.train.csv") :type :csv
                                          :csv-type-spec (append (loop for i below 9 collect 'double-float) '(string))))

       (setf bc-test (read-data-from-file (clml.utility.data:fetch "https://mmaul.github.io/clml.data/sample/bc.test.csv") :type :csv
                                         :csv-type-spec (append (loop for i below 9 collect 'double-float) '(string))))

       (setf cars (read-data-from-file (clml.utility.data:fetch "https://mmaul.github.io/clml.data/sample/cars.csv") :type :csv :csv-type-spec '(double-float double-float)))

      (setf tree (make-decision-tree syobu "種類"))

      (assert-false (print-decision-tree tree *dev-null*))

      (assert-false (print-decision-tree (make-decision-tree syobu "種類" :epsilon 0.1) *dev-null*))

      (setf query #("?" 53.0 30.0 33.0 10.0))

      (assert-true (string= "Versicolor" (predict-decision-tree query syobu tree)))

      (setf tree (make-decision-tree bc-train "Class"))

      (assert-false (print-decision-tree tree *dev-null*))

      (assert-equalp '((("benign" . "malignant") . 4) (("malignant" . "malignant") . 118) (("malignant" . "benign") . 9) (("benign" . "benign") . 214))
                     (decision-tree-validation bc-test "Class" tree))

      (setf tree (make-regression-tree cars "distance" :epsilon 35))

      (assert-false (print-regression-tree tree *dev-null*))

      (setf query #(24.1 "?"))

      (assert-eql 92.0d0 (predict-regression-tree query cars tree))

      (setf tree (make-regression-tree bc-train "Cell.size"))
      
      (assert-eql #- (or lispworks allegro) 2.5736457326892106d0 #+ (or lispworks allegro) 2.356254428341385d0 (regression-tree-validation bc-test "Cell.size" tree))
      ))

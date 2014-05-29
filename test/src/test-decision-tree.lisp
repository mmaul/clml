
(in-package "TEST")

(define-test test-decision-tree
    (let (syobu bc-train bc-test cars tree query)
      (setf syobu (read-data-from-file "sample/syobu.csv" :type :csv :csv-type-spec '(string integer integer integer integer)))
      
      (setf bc-train (read-data-from-file "sample/bc.train.csv" :type :csv 
					  :csv-type-spec (append (loop for i below 9 collect 'double-float) '(string))))
      (setf bc-test (read-data-from-file "sample/bc.test.csv" :type :csv 
					 :csv-type-spec (append (loop for i below 9 collect 'double-float) '(string))))
      
      (setf cars (read-data-from-file "sample/cars.csv" :type :csv :csv-type-spec '(double-float double-float)))
      
      (setf tree (make-decision-tree syobu "Ží—Þ"))
      
      (assert-false (print-decision-tree tree))

      (assert-false (print-decision-tree (make-decision-tree syobu "Ží—Þ" :epsilon 0.1)))
      
      (setf query #("?" 53.0 30.0 33.0 10.0))
      
      (assert-true (string= "Versicolor" (predict-decision-tree query syobu tree)))
      
      (setf tree (make-decision-tree bc-train "Class"))
  
      (assert-false (print-decision-tree tree))

      (assert-equalp '((("benign" . "malignant") . 4) (("malignant" . "malignant") . 118) (("malignant" . "benign") . 9) (("benign" . "benign") . 214))
		     (decision-tree-validation bc-test "Class" tree))

      (setf tree (make-regression-tree cars "distance" :epsilon 35))
      
      (assert-false (print-regression-tree tree))

      (setf query #(24.1 "?"))
      
      (assert-eql 92.0d0 (predict-regression-tree query cars tree))
  
      (setf tree (make-regression-tree bc-train "Cell.size"))
  
      (assert-eql 2.356254428341385d0 (regression-tree-validation bc-test "Cell.size" tree))
      ))
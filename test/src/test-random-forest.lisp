
(in-package "TEST")

(define-test test-random-forest
    (let (syobu bc-train bc-test cars forest query)
      
      (setf syobu (read-data-from-file "sample/syobu.csv" :type :csv :csv-type-spec '(string integer integer integer integer)))
      
      (setf bc-train (read-data-from-file "sample/bc.train.csv" :type :csv 
					  :csv-type-spec (append (loop for i below 9 collect 'double-float) '(string))))
      
      (setf bc-test (read-data-from-file "sample/bc.test.csv" :type :csv 
					 :csv-type-spec (append (loop for i below 9 collect 'double-float) '(string))))
      
      (setf cars (read-data-from-file "sample/cars.csv" :type :csv :csv-type-spec '(double-float double-float)))
      
      (setf forest (make-random-forest syobu "Ží—Þ"))
      
      (assert-eql 500 (length forest))
      
      (assert-false (print-decision-tree (aref forest 0)));;random decision-tree
  
      (setf query #("?" 53.0d0 30.0d0 33.0d0 10.0d0))
      
      (assert-true (string= "Versicolor" (predict-forest query syobu forest)))
      
      (setf forest (make-random-forest bc-train "Class"))
  
      (assert-eql 4 (length (forest-validation bc-test "Class" forest)))

      (setf forest (make-regression-forest bc-train "Cell.size" :tree-number 400))
      
      (assert-eql 400 (length forest))
      
      (assert-false (print-regression-tree (aref forest 0)));;random regression-tree
      
      (assert-true (< 1.0d0 (predict-regression-forest (svref (dataset-points bc-test) 0) bc-train forest) 1.1d0))
  
      (assert-true (< 1.5d0 (regression-forest-validation bc-test "Cell.size" forest) 1.7d0))
      ))
;;;

(in-package "TEST")

(defun classifier-test (type)
  (format t "classifier test [type : ~a]~%" type)
  (let ((train (ecase type
                 (bc (read-data-from-file
                      "sample/bc.train.csv"
                      :type :csv
                      :csv-type-spec (append (make-list 9 :initial-element 'double-float)
                                             '(symbol))))
                 (spam (read-data-from-file
                        "sample/spam.train.csv"
                        :type :csv
                        :csv-type-spec (append (make-list 55 :initial-element 'double-float)
                                               '(double-float double-float symbol))))
                 (german (read-data-from-file
                          "sample/german-credit-train.csv"
                          :type :csv
                          :external-format #+allegro :932 #-allegro :sjis
                          :csv-type-spec '(string integer string string integer string string integer string string integer string integer string string integer string integer string string string)))))
        (test (ecase type
                (bc (read-data-from-file
                     "sample/bc.test.csv"
                     :type :csv
                     :csv-type-spec (append (make-list 9 :initial-element 'double-float)
                                            '(symbol))))
                (spam (read-data-from-file
                       "sample/spam.test.csv"
                       :type :csv
                       :csv-type-spec (append (make-list 55 :initial-element 'double-float)
                                              '(double-float double-float symbol))))
                (german (read-data-from-file
                         "sample/german-credit-test.csv"
                         :type :csv
                         :external-format #+allegro :932 #-allegro :sjis
                         :csv-type-spec '(string integer string string integer string string integer string string integer string integer string string integer string integer string string string)))))
        (objective-param-name (ecase type
                                (bc "Class")
                                (spam "type")
                                (german "ŒÚ‹qŽí•Ê")))
        (manhattan (ecase type
                     (bc :double-manhattan)
                     (spam :double-manhattan)
                     (german :manhattan)))
        original-data-column-length)
    (setq original-data-column-length 
      (length (aref (read-data:dataset-points train) 0)))
    (format t "Executing k-nn...~%")
    (let* ((k 5)
           (k-nn-estimator
            (k-nn-analyze train k objective-param-name :all :distance manhattan :normalize t)))
      (loop for data across (dataset-points (k-nn-estimate k-nn-estimator test))
          with true = 0
          with false = 0
          if (equal (aref data 0) (aref data original-data-column-length))
          do (incf true)
          else do (incf false)
          finally (format t "[k-nn(k=~d) result]  true ~d | false ~d | precision :: ~f ~%"
                          k true false (/ true (+ true false)))))
    (format t "Executing decision tree...~%")
    (let ((dc-result (decision-tree-validation test
                                               objective-param-name
                                               (make-decision-tree train
                                                                   objective-param-name))))
      (loop for ((pred . orig) . c) in dc-result
          with true = 0
          with false = 0
          if (equal pred orig)
          do (incf true c)
          else do (incf false c)
          finally (format t "[decision tree result]  true ~d | false ~d | precision :: ~f ~%"
                          true false (/ true (+ true false)))))
    (format t "Executing svm...~%")
    (let ((positive-class-label (ecase type
                                  (bc '|malignant|)
                                  (spam '|spam|)
                                  (german (error "SVM not yet supported for ~A." type))))
          svm-learn-positive-ds
          svm-learn-negative-ds
          svm-test-positive-ds 
          svm-test-negative-ds
          svm-classifier
          (last-column-index (1- original-data-column-length))
          (kernel-info `(("linear" . ,+linear-kernel+)
                         ("gaussian-0.1" . ,(gaussian-kernel 0.1)))))
      (loop for v across (dataset-points train)
          if (equal (aref v last-column-index) positive-class-label)
          do (push (coerce (subseq v 0 (1- last-column-index)) 'list) svm-learn-positive-ds)
          else do (push (coerce (subseq v 0 (1- last-column-index)) 'list) svm-learn-negative-ds))
      (loop for v across (dataset-points test)
          if (equal (aref v last-column-index) positive-class-label)
          do (push (coerce (subseq v 0 (1- last-column-index)) 'list) svm-test-positive-ds)
          else do (push (coerce (subseq v 0 (1- last-column-index)) 'list) svm-test-negative-ds))
      (loop for (kernel-name . kernel-fn) in kernel-info
          do  
            (setq svm-classifier
              (svm kernel-fn svm-learn-positive-ds svm-learn-negative-ds))
            (let ((true 0)
                  (false 0))
              (loop for data in svm-test-positive-ds
                  if (funcall svm-classifier data)
                  do (incf true)
                  else do (incf false))
              (loop for data in svm-test-negative-ds
                  if (funcall svm-classifier data)
                  do (incf false)
                  else do (incf true))
              (format t "[svm ~a result]  true ~d | false ~d | precision :: ~f ~%"
                      kernel-name true false (/ true (+ true false))))))))

#|
CL-USER(3): (classifier-test 'bc)
classifier test [type : BC]
Executing k-nn...
Number of self-misjudgement : 13
[k-nn(k=5) result]  true 337 | false 8 | precision :: 0.9768116 
Executing decision tree...
[decision tree result]  true 332 | false 13 | precision :: 0.96231884 
Executing svm...
[svm linear result]  true 311 | false 34 | precision :: 0.90144926 
[svm gaussian-0.1 result]  true 334 | false 11 | precision :: 0.9681159 

CL-USER(3): (classifier-test 'spam)

CL-USER(3): (classifier-test 'german)
|#

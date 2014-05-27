(load "initial.cl")

(setf train-data
      (pick-and-specialize-data
       (read-data-from-file
        "sample/svm-benchmark-train.csv"
        :type :csv
        :csv-type-spec (make-list 24 :initial-element 'double-float)
        :external-format :utf-8)
       :data-types (make-list 24 :initial-element :numeric)))

(setf train-data-2
      (pick-and-specialize-data
       (read-data-from-file
        "sample/svm-benchmark2.csv"
        :type :csv
        :csv-type-spec (make-list 24 :initial-element 'double-float)
        :external-format :utf-8)
       :data-types (make-list 24 :initial-element :numeric)))

(setf train-data-3
      (pick-and-specialize-data
       (read-data-from-file
        "sample/svm-benchmark3.csv"
        :type :csv
        :csv-type-spec (make-list 14 :initial-element 'double-float)
        :external-format :utf-8)
       :data-types (make-list 14 :initial-element :numeric)))

(setf training-vector (dataset-points train-data))
(setf training-vector-2 (dataset-points train-data-2))
(setf training-vector-3 (dataset-points train-data-3))

(setf rbf (make-rbf-kernel :gamma 0.5d0))

(time (setf svm (make-svm-learner training-vector rbf :c 10 :cache-size-in-mb 100)))


#| libsvm format convertor

(defun data-converter (data-vector pathname)
  "for classification"
  (let ((label-index (1- (length (svref data-vector 0))))
	(path (make-pathname :name pathname)))
    (with-open-file (stream path :direction :output
                            :if-exists :supersede)
      (loop
        for row across data-vector
        as label = (if (plusp (aref row label-index))
                       1
		       -1)
        do (progn (format stream "~@,0D " label)
                  (loop 
                    for i below label-index
                    as value = (aref row i)
                    do (format stream "~A:~A " (1+ i) value)
                    finally (format stream "~%")))))))

$ time svm-train -s 0 -t 2 -g 0.5 -c 10 -m 0.00001 -h 0 svm-benchmark-train 

|#


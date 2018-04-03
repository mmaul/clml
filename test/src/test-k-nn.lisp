
(in-package :clml.test)

(define-test test-sample-k-nn
    (let (data-for-learn estimator data-for-estimate result
          (expected-maxs '(0.6d0 0.0d0 0.0d0 1.0d0 0.0d0 0.0d0 0.8d0 0.0d0 0.0d0 1.0d0 0.0d0
                           0.0d0 0.875d0 0.0d0 0.0d0 0.46666667d0 1.0d0 1.0d0 1.0d0 1.0d0 1.0d0
                           0.0d0 0.0d0 0.0d0 0.0d0 0.76d0 0.08d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0
                           0.0d0 0.06666667d0 1.0d0 0.0d0 0.08d0 0.0d0 0.73333335d0 1.0d0 0.8d0
                           0.0d0 0.067567565d0 0.026666667d0 0.8d0 0.0d0 0.21333334d0 0.0d0
                           0.06779661d0 0.7866667d0 0.014705882d0 0.0d0 0.0d0 0.18666667d0 0.0d0
                           0.625d0 1.0d0 0.0d0 1.0d0 0.0d0 1.0d0 0.5d0 1.0d0 0.70666665d0 0.0d0
                           0.0d0 0.0d0 0.7627119d0 1.0d0 1.0d0 0.58666664d0 1.0d0 0.0d0 1.0d0
                           0.0d0 1.0d0 1.0d0 0.5945946d0 0.46666667d0 1.0d0 0.85333335d0 0.0d0
                           0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.7866667d0 0.7733333d0
                           0.0d0 0.4074074d0 1.0d0 0.0d0 0.0d0 0.0d0 0.74666667d0 0.6666667d0
                           0.0d0 0.0d0 0.2739726d0 0.0d0 0.0d0 0.0d0)))
      (assert
       (setf data-for-learn
         (read-data-from-file
          (clml.utility.data:fetch "https://mmaul.github.io/clml.data/sample/learn.csv")
          :type :csv
          :csv-type-spec (cons 'string (make-list 105 :initial-element 'double-float))
          :external-format :utf-8
          )))


      (assert
       (setf estimator
         (clml.nearest-search.k-nn:k-nn-analyze data-for-learn 2 "id" :all :distance :manhattan :normalize t)))
      (destructuring-bind (&key k target explanatories distance mins maxs
                                vec-weight vecs vec-profiles teachers esttype &allow-other-keys)
          (clml.nearest-search.k-nn:estimator-properties estimator :verbose t)
        (assert-true (string= "id" target))
        (assert (set-equal (loop for dim across (dataset-dimensions data-for-learn)
                                    as name = (dimension-name dim)
                                    unless (string= "id" name) collect name)
                                explanatories :test #'string=))
        (assert-true (every (lambda (pro) (eq :numeric pro)) vec-profiles))
        (assert-false vec-weight)
        (assert-true (every #'zerop mins))
        (let ((*epsilon* 1d-7))
          (assert-true (set-equal expected-maxs (coerce maxs 'list) :test #'epsilon>)))
        (loop for i from 1
            for teacher across teachers
            do (assert-equality #'string= (format nil "~D" i) teacher))

        (assert-points-equal
         (map 'vector (lambda (pts) (map 'clml.hjs.meta:dvec
                                      (lambda (val m)
                                        #+(or sbcl ccl)
                                        (if (zerop m)
                                            0d0
                                            (/ val m))
                                        #-(or sbcl ccl)
                                        (handler-case (/ val m)
                                          (division-by-zero (c) (declare (ignore c)) 0d0)))
                                      (subseq pts 1)
                                      maxs))
              (dataset-points data-for-learn))
         vecs)

        (assert-eq :manhattan distance)
        (assert-eql 2 k)
        (assert-eq esttype :classify)

        (assert
         (setf data-for-estimate
           (read-data-from-file (clml.utility.data:fetch "https://mmaul.github.io/clml.data/sample/estimate.csv")
                                :type :csv
                                :csv-type-spec (make-list 105 :initial-element 'double-float)
                                :external-format :utf-8))
         )

        (assert (setf result (clml.nearest-search.k-nn:k-nn-estimate estimator data-for-estimate)))
        (assert-true (set-equal
                      '("24" "27" "31" "17" "110" "49" "58" "30" "58" "71" "96" "96" "152"
                        "116" "80" "128" "188" "97" "167" "197" "196" "196" "196")
                      (map 'list (lambda (vec) (svref vec 0)) (dataset-points result))
                      :test #'string=)))))

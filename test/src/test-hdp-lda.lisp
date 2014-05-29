
(in-package "TEST")

(define-test test-sample-hdp-lda
    (let ((dataset (pick-and-specialize-data
                    (read-data-from-file "sample/sports-corpus-data" :external-format :utf-8)
                    :except '(0) :data-types (make-list 1202 :initial-element :numeric))))
      (multiple-value-bind (res1 res2 model) (hdp-lda dataset)
        (assert-eql (topic-count model) (length (dataset-dimensions res1)))
        (assert-eql (length (dataset-dimensions dataset)) (1- (length (dataset-dimensions res2))))
        (assert-eql (topic-count model) (length (dataset-category-points res2)))
        (loop for pt across (dataset-numeric-points res1)
            do (assert-equality #'epsilon> 1d0 (reduce #'+ pt)))
        (loop for pt across (dataset-numeric-points res2)
            do (assert-equality #'epsilon> 1d0 (reduce #'+ pt)))
        (let ((hot-topics (get-trend-topics model :trend :hot))
              (topic-names (map 'list #'dimension-name (dataset-dimensions res1)))
              (bow (cdr (map 'list #'dimension-name (dataset-dimensions res2))))
              (mean-thetas (loop for thetas across (transposeV (dataset-numeric-points res1))
                               collect (mean thetas))))
          (loop for (topic-id . (words . mtheta)) in hot-topics
              as pos = (position topic-id topic-names :test #'string=)
              as expected-mtheta = (nth pos mean-thetas)
              as probabilities = (map 'list #'cons bow (aref (dataset-numeric-points res2) pos))
              as expected-words = (mapcar #'car (sort probabilities #'> :key #'cdr))
              do (assert-equality #'epsilon> expected-mtheta mtheta)
                 (loop for word across words
                     for expected-word in expected-words
                     do (assert-equality #'string= expected-word word)))))))






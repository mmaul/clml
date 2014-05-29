;;;

(in-package "TEST")

(define-test test-sample-text-utils
    (let ((data (read-data-from-file "sample/equivalence-class.csv" :type :csv :csv-type-spec
                                     '(string string double-float) :external-format :utf-8))
          result)
      (assert-equality #'epsilon> 0.5714285714285714d0
                       (calculate-string-similarity "kitten" "sitting" :type :lev))
      (assert-equality #'epsilon> 0.6153846153846154d0
                       (calculate-string-similarity "kitten" "sitting" :type :lcs))
      (assert-true (setq result (equivalence-clustering (dataset-points data))))
      (assert-true (loop for cluster in '(("e") ("f" "z" "y" "x") ("c") ("b" "a") ("w"))
                       always (member cluster result :test (lambda (l1 l2)
                                                             (set-equal l1 l2 :test #'string=)))))))
      

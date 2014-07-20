
(in-package :clml.test)

(define-test test-sample-ts-read-data
    (let (d msi-access)
      (assert-true 
       (setf d (read-data-from-file (clml.utility.data:fetch "https://mmaul.github.io/clml.data/sample/msi-access-stat/access-log-stat.sexp"))))
      (assert-true (setf msi-access (time-series-data d :range '(1) :time-label 0
                                                      :frequency 24 :start '(18 3))))
      (assert-true (setf msi-access (time-series-data d :range '(1) :time-label 0
                                                      :frequency 24 :start '(18 3) :end '(18 24))))
      (assert-true
       (setf msi-access (time-series-data d :range '(1) :time-label 0 :frequency 3)))
      (assert-true (ts-points msi-access))))
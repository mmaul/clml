
(in-package "TEST")

(define-test test-sample-ts-stat
    (let (ukgas useco)
      (assert-true 
       (setq ukgas 
         (time-series-data (read-data-from-file "sample/UKgas.sexp")
                           :range '(1) :time-label 0)
         useco
         (time-series-data (read-data-from-file "sample/USeconomic.sexp"))))
      (assert-false (acf useco))
      (assert-false (ccf (sub-ts useco :range '(0)) (sub-ts useco :range '(1))))
      (assert-false (periodgram ukgas))))
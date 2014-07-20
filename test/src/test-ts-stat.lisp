
(in-package :clml.test)

(define-test test-sample-ts-stat
    (let (ukgas useco)
      (assert-true 
       (setq ukgas 
             (time-series-data (read-data-from-file (clml.utility.data:fetch "https://mmaul.github.io/clml.data/sample/UKgas.sexp"))
                           :range '(1) :time-label 0)
         useco
         (time-series-data (read-data-from-file (clml.utility.data:fetch "https://mmaul.github.io/clml.data/sample/USeconomic.sexp")))))
      (assert-false (acf useco))
      (assert-false (ccf (sub-ts useco :range '(0)) (sub-ts useco :range '(1))))
      (assert-false (periodgram ukgas))))

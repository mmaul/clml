
(in-package :clml.test)

(define-test test-sample-ts-stat
    (let (ukgas useco)
      (assert
       (setq ukgas
             (time-series-data (read-data-from-file (clml.utility.data:fetch "https://mmaul.github.io/clml.data/sample/UKgas.sexp"))
                           :range '(1) :time-label 0)
         useco
         (time-series-data (read-data-from-file (clml.utility.data:fetch "https://mmaul.github.io/clml.data/sample/USeconomic.sexp")))))
      #+lispworks (assert-false (acf useco))
      #+lispworks (assert-false (ccf (sub-ts useco :range '(0)) (sub-ts useco :range '(1))))
      #+lispworks (assert-false (periodgram ukgas))))

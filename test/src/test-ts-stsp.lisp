
(in-package :clml.test)

(define-test test-sample-ts-stsp
    (let (model)
      (defparameter tokyo
          (time-series-data
           (read-data-from-file (clml.utility.data:fetch "https://mmaul.github.io/clml.data/sample/tokyo-temperature.sexp"))))
      (assert-true (setq model (trend tokyo :k 2 :opt-t^2 t)))
      (assert-true (predict model :n-ahead 10))
      ))
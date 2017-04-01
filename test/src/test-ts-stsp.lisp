
(in-package :clml.test)

(define-test test-sample-ts-stsp
  (let ((model)
        (tokyo
         (time-series-data
          (read-data-from-file (clml.utility.data:fetch "https://mmaul.github.io/clml.data/sample/tokyo-temperature.sexp")
                               :external-format :utf-8))))

    (assert (setq model (trend tokyo :k 2 :opt-t^2 t)))
    (assert-true (predict model :n-ahead 10))))

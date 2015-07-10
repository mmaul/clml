(defpackage :clml.pca.examples.pca-bench
  
  (:use :cl
        :clml.hjs.meta
        ;:clml.hjs.matrix
        ;:clml.hjs.vector
        clml.hjs.read-data
        ;:clml.statistics
        ;:clml.hjs.vars
        :clml.pca
        )
  (:export #:faces-example #:eyes-example)
)

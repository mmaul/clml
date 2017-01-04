
(defpackage :clml.nearest-search.k-nn

  (:use :cl
        :clml.hjs.vector
        :clml.hjs.read-data
        :clml.hjs.meta)
  (:export :k-nn-analyze
           :k-nn-estimate
           :estimator-properties)
    (:documentation "*** sample usage
#+INCLUDE: \"../sample/random-forest.org\" example lisp

*** note
When target, the objective variable's type is string, discriminant
analysis is used, when type is number, regression analysis is used. In
the case of discriminant analysis, the number of self-misjudgement from
self analysis is displayed.
")
  )

(defpackage :clml.nearest-search.nearest

  (:use :cl
        :clml.hjs.read-data
        :clml.hjs.vector
        :clml.hjs.meta
        :clml.nonparametric.statistics
        :clml.hjs.matrix
        :clml.pca
        :clml.utility.priority-que)
  (:export :nearest-search
                   :find-nearest-epsilon
           :exact-nearest-search
           :stochastic-nearest-search

           :naive-nearest-search

           :kd-tree-search

           :m-tree-search

           :locality-sensitive-hashing
           :p-stable-locality-sensitive-hashing
           :euclid-locality-sensitive-hashing
           :manhattan-locality-sensitive-hashing
           :cosine-locality-sensitive-hashing

           :nns-input-data
           :nns-input-key
           :nns-distance

           :initialize-search

           :find-nearest
           :find-nearest-k


           :stochastic-validation
           ))

(defpackage :clml.nearest-search.k-nn-new

  (:use :cl
        :clml.hjs.vector
        :clml.hjs.read-data
        :clml.hjs.meta
        :clml.nearest-search.nearest)
  (:export :k-nn-analyze
           :k-nn-estimate
           :estimator-properties)
      (:documentation "*** note
When target, the objective variable's type is string, discriminant
analysis is used, when type is number, regression analysis is used. In
the case of discriminant analysis, the number of self-misjudgement from
self analysis is displayed.
"))



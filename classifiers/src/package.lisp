;-*- coding: utf-8 -*-
(defpackage :clml.classifiers.linear-regression
  ;
  (:use :cl
        :clml.hjs.read-data
        :clml.hjs.matrix
        :clml.hjs.meta)
  (:import-from :clml.clustering.hc
                #:i-thvector
                #:square-sum
                #:product-sum
                #:vector-shift
                #:vector-mean)
  (:export #:mlr
           #:residual-vector
           #:residual-quantile-vector
           #:residual-std-err
           #:std-err-vector
           #:t-value-vector
           #:d.f
           #:pt
           #:pt-value-vector
           #:r^2
           #:adjusted-r^2
           #:f-value
           #:pf
           #:pf-value)
  (:documentation "linear regression package

*** sample usage
#+INCLUDE: \"../sample/linear-regression.org\" example lisp"))

(defpackage :clml.classifiers.logistic-regression
  (:use :cl
        :clml.svm.wss3;比較実験用
        :clml.hjs.read-data
        :clml.hjs.vector
        :clml.hjs.matrix))

(defpackage :clml.classifiers.nbayes

  (:use :cl
        :clml.hjs.read-data)
  (:export
   :mbnb-learn
   :make-mbnb-learner
   :mnb-learn
   :make-mnb-learner)
  (:documentation "Naive-Bayes

   Naive-Bayes package (Multivariate Bernoulli and Multinomial Naive Bayes)

*** sample usage
#+INCLUDE: \"../sample/nbayes.org\" example lisp
")
  )

;;;

(defpackage :clml.test
  (:use
   :cl
   :clml.hjs.vars
   :clml.hjs.read-data
   :clml.statistics
   :clml.statistics.math
   :clml.clustering.CLUSTER-VALIDATION
   :clml.clustering.optics
   :clml.clustering.nmf
   :clml.clustering.hc
   :clml.time-series.util
   :clml.time-series.read-data
   :clml.time-series.statistics
   :clml.time-series.state-space
   :clml.time-series.autoregression
   :clml.time-series.anomaly-detection
   :clml.time-series.exponential-smoothing
   :clml.time-series.burst-detection
   :clml.time-series.changefinder
   :clml.clustering.hc
   :clml.hjs.matrix
   :clml.hjs.vector
   :clml.hjs.k-means
   :clml.hjs.missing-value
   :clml.clustering.nmf
   :clml.clustering.optics
   :clml.clustering.spectral-clustering
   :clml.svm.mu
   :clml.svm.smo
   :clml.svm.wss3
   :clml.svm.pwss3 ; make-polynomial-kernel conflict
   :clml.svm.one-class
   :clml.svm.svr
   :clml.classifiers.linear-regression
   :clml.classifiers.nbayes
   :clml.nonparametric.hdp-lda
   :clml.decision-tree.random-forest
   :clml.text.utilities
   :clml.text.hdp-lda
   :clml.nonparametric.dpm-interface
   :clml.association-rule
   :clml.som
   :clml.pca
   :clml.utility.arff
   )
  (:shadow :make-polynomial-kernel :make-svm-learner :make-rbf-kernel :make-svm-validation
           :svm-validation :load-svm-learner :make-svm-kernel :make-linear-kernel
           :make-one-class-svm-kernel)
  (:shadow :centroid)

  (:import-from :LISP-UNIT
               :define-test
               :run-tests
               :assert-true
               :assert-equalp
               :assert-equal
               :assert-false
               :assert-eql
               :assert-eq
               :print-errors
               :print-failures
               :print-summary
               :ASSERT-EQUALITY
               :ASSERT-PRINTS
               :set-equal
               )

  (:import-from :clml.decision-tree.decision-tree :make-decision-tree :print-decision-tree
               :decision-tree-validation :predict-decision-tree :make-regression-tree
               :predict-regression-tree :print-regression-tree :regression-tree-validation)
  ;(:import-from "SPECTRAL-CLUSTERING" "*SAMPLE-W*")
  ;(:import-from "K-MEANS" "MANHATTAN-DISTANCE")
  ;(:import-from "HC" "NUMERIC-MATRIX" "PICK-UP-COLUMN" "PICK-UP-ROW")
  (:export :run-test
           :run-all-tests
           *statistics-tests*
           *decision-tree-tests*
           *clustering-tests*
           *time-series-tests*
           *svm-tests*
           *classifiers-tests*
           *association-rule-tests*
           *nonparametric-tests*
           *som-tests*
           *text-tests*
           *pca-tests*
           *hjs-tests*)
  )

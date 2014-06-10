;;;

(defpackage "TEST"
  (:use
   :cl
   :vars
   :read-data
   :statistics
   ;"ASSOC"
   :clml.clustering.CLUSTER-VALIDATION
   ;"DECISION-TREE"
   :clml.time-series.util
   :clml.time-series.read-data
   :clml.time-series.statistics
   :clml.time-series.state-space
   :clml.time-series.autoregression
   :clml.time-series.anomaly-detection
   :clml.time-series.exponential-smoothing
   :clml.time-series.burst-detection
   :clml.pca
   :CHANGEFINDER
   :hc
   ;"K-MEANS"
                                        ;"K-NN"
   :hjs.learn.k-means
   :hjs.util.missing-value
   :nmf
   :optics
   ;"PCA"
   ;"SOM"
   :spectral-clustering
   :clml.svm.mu
   :clml.svm.smo
   :clml.svm.wss3
   ;:clml.svm.pwss3
   :clml.svm.one-class
   :clml.classifiers.linear-regression
   ;"SVR"
   ;"ONE-CLASS-SVM"
   ;"NBAYES"
   ;"MATRIX"
   :random-forest
   ;"UTIL"
   ;"TS-BURST-DETECTION"
   ;"HDP-LDA"
   ;"DPM"
   ;"TEXT-UTILITIES"
   )
  (:import-from :LISP-UNIT
               :define-test
               :run-tests
               :assert-true
               :assert-equalp
               :assert-false
               :assert-eql
               :print-errors
               :print-failures
               :print-summary
               :ASSERT-EQUALITY
               :ASSERT-PRINTS
               :set-equal
               )
  
  (:import-from :decision-tree :make-decision-tree :print-decision-tree
               :decision-tree-validation :predict-decision-tree :make-regression-tree
               :predict-regression-tree :print-regression-tree :regression-tree-validation)
  ;(:import-from "SPECTRAL-CLUSTERING" "*SAMPLE-W*")
  ;(:import-from "K-MEANS" "MANHATTAN-DISTANCE")
  ;(:import-from "HC" "NUMERIC-MATRIX" "PICK-UP-COLUMN" "PICK-UP-ROW")
  )


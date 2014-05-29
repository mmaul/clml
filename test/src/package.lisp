;;;

(defpackage "TEST"
  (:use
   "CL" 
    "VARS" "READ-DATA" "STATISTICS"
    ;"ASSOC"
    ;"CLUSTER-VALIDATION"
    ;"DECISION-TREE"
    ;"TS-READ-DATA"
    ;"TS-AUTOREGRESSION"
    ;"TS-UTIL"
    ;"TS-STAT"
    ;"TS-STATE-SPACE"
    ;"EXPL-SMTHING"
    ;"CHANGEFINDER"
    ;"TS-ANOMALY-DETECTION" 
    ;"HC"
    ;"K-MEANS"
    ;"K-NN"
    ;"LINEAR-REGRESSION"
    ;"MISSING-VAL"
    ;"NMF"
    ;"OPTICS"
    ;"PCA"
    ;"SOM"
    ;"SPECTRAL-CLUSTERING"
    ;"SVM.MU"
    ;"SVM.SMO"
    ;"SVM.WSS3"
    ;"SVR"
    ;"ONE-CLASS-SVM"
    ;"NBAYES"
    ;"MATRIX"
    ;"RANDOM-FOREST"
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
               :print-errors)
  (:import-from :decision-tree :make-decision-tree :print-decision-tree
               :decision-tree-validation :predict-decision-tree :make-regression-tree
               :predict-regression-tree :print-regression-tree :regression-tree-validation)
  ;(:import-from "SPECTRAL-CLUSTERING" "*SAMPLE-W*")
  ;(:import-from "K-MEANS" "MANHATTAN-DISTANCE")
  ;(:import-from "HC" "NUMERIC-MATRIX" "PICK-UP-COLUMN" "PICK-UP-ROW")
  )


(in-package :clml.nonparametric.hdp-hmm)

(defclass hdp-hmm (hdp)
  ((base-distribution :initform (make-instance 'state-uniform))
   (vocabulary :initarg :v :initform 27 :accessor vocabulary)
   (eos-state :accessor hdp-hmm-eos)))

(defclass hidden-state (hdp-cluster)
    ((dist  :initform (make-hash-table) :accessor cluster-dist-table)
     (emission :initform (make-hash-table :test #'equal) :accessor emission)))

(defclass state-uniform (hdp-distribution)
  ((cluster-class :initform 'hidden-state)))

(in-package :clml.time-series.util)

(defclass timeseries-model ()
  ((observed-ts
    :initarg :observed-ts
    :accessor observed-ts
    :type time-series-dataset
    :initform (error "Must specify the observed timeseries data"))))

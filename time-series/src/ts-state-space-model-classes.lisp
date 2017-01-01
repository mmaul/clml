(in-package :clml.time-series.state-space)

(defclass state-space-model (timeseries-model)
  ((F-matrices :initarg :F-matrices
               :initform (error "Must specify the F matrices"))
   (G-matrices :initarg :G-matrices
               :initform (error "Must specify the G matrices"))
   (H-matrices :initarg :H-matrices
               :initform (error "Must specify the H matrices"))
   (Q-matrices :initarg :Q-matrices :initform nil)
   (R-matrices :initarg :R-matrices :initform nil)
   )
  (:documentation "- state space model
- accessors:
  - ts-data : observed time series data"))

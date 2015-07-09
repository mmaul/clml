#+sbcl
(declaim (sb-ext:muffle-conditions sb-kernel:character-decoding-error-in-comment))

(defpackage :clml.time-series-environment (:use :cl :asdf))
(in-package :clml.time-series-environment)

(defun call-with-environment (fun)
  (let ((*read-default-float-format* 'double-float))
    (funcall fun)))

(asdf:defsystem :clml.time-series
                :pathname "src/"
                :serial t
                :around-compile call-with-environment  
                :depends-on (:clml.hjs
                             :iterate
                             :clml.numeric
                             :uiop
                             :array-operations
                             )
                :components ((:file "package")
                             (:file "ts-util-classes")
                             (:file "ts-state-space-model-classes")
                             (:file "ts-ar-classes")
                             (:file "ts-read-data")
                             (:file "ts-util")
                             (:file "ts-stat")
                             (:file "ts-state-space-model")
                             (:file "ts-ar")
                             (:file "changefinder")
                             (:file "ts-anomaly-detection")
                             (:file "ts-burst-detection")
                             (:file "exponential-smoothing")
                             
                             ))

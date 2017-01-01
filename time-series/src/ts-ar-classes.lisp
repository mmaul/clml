;-*- coding: utf-8 -*-
(in-package :clml.time-series.autoregression)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ar-model (1-dimensional) ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
( eval-when (:compile-toplevel :load-toplevel)


  (defclass ar-model (clml.time-series.state-space::gaussian-stsp-model)
    ((ar-coefficients :initarg :ar-coefficients
                      :accessor ar-coefficients
                      :type list
                      :initform '())
     (sigma^2 :initarg :sigma^2
              :accessor sigma^2
              :type number
              :initform 0.0d0)
     (aic :initarg :aic
          :type list
          :initform '())
     (demean :initarg :demean
             :accessor demean
             :initform nil)
     (ar-method :initarg :ar-method
                :accessor ar-method
                :type symbol
                :initform nil))
    (:documentation "- parent: gaussian-stsp-model
- accessors:
  - ar-coefficients : AR parameters
  - sigma^2 : Variance for AR model
  - aic : AIC for AR model
  - ar-method : Method of constructing AR model")))

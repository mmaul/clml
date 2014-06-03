;-*- coding: utf-8 -*-

(defpackage :clml.time-series.read-data
  (:use :cl :util :vector :vars :read-data :missing-val)
  (:nicknames :ts-read-data)
  (:export 
   #:time-series-dataset
   #:time-series-data
   #:make-constant-time-series-data
   #:make-ts-point
   #:ts-p-pos
   #:ts-p-freq
   #:ts-p-label
   #:ts-p-time
   #:ts-points
   #:time-label-name
   #:copy-ts
   #:ts-start
   #:ts-end
   #:ts-freq
   #:ts-type
   #:tf-incl
   #:tf-gap
   #:ts-cleaning)
  (:import-from :read-data #:clean-points #:dimension)
  )

(defpackage :clml.time-series.util
  (:documentation "
    Utility generally relating to
      * Time conversion
      * String manip
      * External Program invocation

    Regarding external program invocation, work needs to be done, nameley converting alisp
    specific calls to uiop. Also external program invocation is used to spawn R for
    graph generation. Would be better to use 
  ")
  (:use :cl 
        
        :hjs.learn.read-data :hjs.util.meta :hjs.util.vector
        :hjs.util.matrix :statistics
        :clml.time-series.read-data)
  #+allegro
  (:use :excl)
  (:nicknames :ts-util)
  (:export #:ts-to-sta
           #:ts-
           #:sub-ts
           #:compose-ts
           #:merge-ts
           
           #:timeseries-model
           #:observed-ts
           #:predict
           #:statvis
           #:draw-ppm
           #:*r-stream*
           #:with-r
           #:open-eps-file
           #:date-time-to-ut
           #:ut-to-date-time))

(defpackage :clml.time-series.statistics
  (:use :cl :hjs.learn.read-data :hjs.util.meta :hjs.util.vector
        :hjs.util.matrix :statistics :hjs.learn.vars
        :clml.time-series.read-data
        :clml.time-series.util
        :clml.numeric.fast-fourier-transform)
  (:nicknames :ts-stat)
  (:export
   #:lag
   #:diff
   #:ts-ratio
   #:ts-log                             ; include logit transformation
   #:ts-min
   #:ts-max
   #:ts-mean
   #:ts-median
   #:ts-demean
   #:ts-covariance
   #:ts-correlation
   #:ma
   #:acf
   #:ccf
   #:periodgram))

(defpackage :clml.time-series.state-space
  (:documentation  "
    classes and methods for state-space-model
    reference: 時系列解析入門 著:北川源四郎 岩波書店 9 章以降")
  (:use :cl :read-data :util :vector :matrix
        :statistics :ts-util :ts-stat :ts-read-data
        :handling-missing-value)
  (:nicknames :ts-stsp)
  (:export 
   #:trend #:trend-prediction
   #:seasonal #:seasonal-adj))

(defpackage :clml.time-series.autoregression
  (:use :cl
        :hjs.learn.read-data
        :hjs.util.meta
        :hjs.util.vector
        :hjs.util.matrix
        :hjs.learn.vars
        :statistics
        :clml.time-series.util
        :clml.time-series.statistics
        :clml.time-series.read-data
        :clml.time-series.state-space)
  (:nicknames :ts-ar :ts-autoregression)
  (:export
   #:ar #:ar-prediction
   #:parcor #:parcor-filtering))

(defpackage :clml.time-series.changefinder
  (:use :cl
        :hjs.learn.read-data
        :hjs.util.meta
        :hjs.util.vector
        :hjs.util.matrix
        :statistics
        :clml.time-series.util
        :clml.time-series.statistics
        :clml.time-series.read-data
        :hjs.util.missing-value
        )
  (:nicknames :changefinder)
  (:export :init-changefinder
           :update-changefinder))


(defpackage :clml.time-series.anomaly-detection
  (:use :cl 
        :hjs.learn.read-data
        :hjs.util.meta
        :hjs.util.vector
        :hjs.util.matrix
        :hjs.learn.vars
        :hjs.util.missing-value
        :statistics
        :clml.utility.csv
        :clml.time-series.util
        :clml.time-series.statistics
        :clml.time-series.read-data
        :clml.time-series.state-space
        :clml.time-series.autoregression
        )
  (:export :make-db-detector
           :make-periodic-detector
           :make-eec-detector
           :make-snn
           :e-scores)
  #+allegro
  (:use :excl)
  )

(defpackage :clml.time-series.exponential-smoothing
  (:use :cl
        :iter
        :clml.time-series.util
        :clml.time-series.statistics
        :hjs.util.meta
        :hjs.util.vector
        :hjs.learn.vars
        :clml.time-series.read-data)
  (:nicknames :expl-smoothing :exponential-smoothing)
  (:export
   #:best-single-exp-parameters
   #:best-double-exp-parameters
   #:best-triple-exp-parameters
   #:holtwinters
   #:holtwinters-prediction))

(defpackage :clml.time-series.burst-detection
  (:use :cl :read-data :handling-missing-value
        :ts-util :ts-stat :ts-read-data)
  (:nicknames :ts-burst-detection)
  (:export
   #:continuous-kleinberg
   #:print-burst-indices
   #:enumerate-kleinberg))


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
  (:documentation
   "Time-Series-Read-Data

package for reading time series data

*** sample usage
#+INCLUDE: \"../sample/svm-validation.org\"  example lisp "))

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
        :hjs.util.matrix :clml.statistics
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
        :hjs.util.matrix :clml.statistics :hjs.learn.vars
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
   #:periodgram)
  (:documentation "Time-Series-Statistics
Package for statistic utils for /time-series-dataset/.

*** sample usage
#+INCLUDE: \"../sample/svm-validation.org\"  example lisp")
  )

(defpackage :clml.time-series.state-space
  (:documentation  "Package for state space model.
Classes and methods for representing various time series model.
Reference: 時系列解析入門 著:北川源四郎 岩波書店 9 章以降

*** sample usage
#+INCLUDE: \"../sample/state-space-model.org\" example lisp 
")
  (:use :cl :read-data :util :vector :matrix
        :clml.statistics :ts-util :ts-stat :ts-read-data
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
        :clml.statistics
        :clml.time-series.util
        :clml.time-series.statistics
        :clml.time-series.read-data
        :clml.time-series.state-space)
  (:nicknames :ts-ar :ts-autoregression)
  (:export
   #:ar #:ar-prediction
   #:parcor #:parcor-filtering)
  (:documentation "Package for AutoRegression model
*** sample usage
#+INCLUDE: \"../sample/time-series-autoregression.org\"  example lisp"))

(defpackage :clml.time-series.changefinder
  (:use :cl
        :hjs.learn.read-data
        :hjs.util.meta
        :hjs.util.vector
        :hjs.util.matrix
        :clml.statistics
        :clml.time-series.util
        :clml.time-series.statistics
        :clml.time-series.read-data
        :hjs.util.missing-value
        )
  (:nicknames :changefinder)
  (:export :init-changefinder
           :update-changefinder)
  (:documentation "ChangeFinder
Package for "ChangeFinder"

**** Comments
- A value of 0.01 has been added to the diagonal elements of the covariance matrix
  for the stability of the inverse matrix calculation.
  User can edit this value by the special variable named *stabilizer*.
**** Reference
- J. Takeuchi, K. Yamanishi\"A Unifying framework for detecting outliers and change points from time series\" 
- K. Yamanishi "データマイニングによる異常検知" p.45-58
")
  )


(defpackage :clml.time-series.anomaly-detection
  (:use :cl 
        :hjs.learn.read-data
        :hjs.util.meta
        :hjs.util.vector
        :hjs.util.matrix
        :hjs.learn.vars
        :hjs.util.missing-value
        :clml.statistics
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
  (:documentation "Direction-based anomaly detector
 *** Reference
 T.Ide and H.Kashima \"Eigenspace-based Anomaly Detection in Computer Systems\" sec.5
*** sample usage for make-db-detector and make-periodic-detector
#+INCLUDE: \"../sample/perodic-and-db-detector.org\"  example lisp

*** sample usage for SNN and EEC
#+INCLUDE: \"../sample/time-series-snn-eec.org\"  example lisp
"))

(defpackage :clml.time-series.exponential-smoothing
  (:use :cl
        :iter
        :clml.time-series.util
        :clml.time-series.statistics
        :hjs.util.meta
        :hjs.util.vector
        :hjs.learn.vars
        :hjs.learn.read-data
        :clml.time-series.read-data)
  (:nicknames :expl-smoothing :exponential-smoothing)
  (:export
   #:best-single-exp-parameters
   #:best-double-exp-parameters
   #:best-triple-exp-parameters
   #:holtwinters
   #:holtwinters-prediction
   )
  (:documentation "*** sample usage
#+INCLUDE: \"../sample/time-series-holtwinters.org\"  example lisp "))

(defpackage :clml.time-series.burst-detection
  (:use :cl :read-data :handling-missing-value
        :ts-util :ts-stat :ts-read-data)
  (:nicknames :ts-burst-detection)
  (:export
   #:continuous-kleinberg
   #:print-burst-indices
   #:enumerate-kleinberg))


;-*- coding: utf-8 -*-

(defpackage :clml.time-series.read-data
  (:use :cl :clml.hjs.meta :clml.hjs.vector :clml.hjs.vars :clml.hjs.read-data :clml.hjs.missing-value)

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
   #:ts-cleaning
   #:choice-a-dimension
   #:choice-a-dimensions
)
  (:import-from :clml.hjs.read-data #:clean-points #:dimension)
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

        :clml.hjs.read-data :clml.hjs.meta :clml.hjs.vector
        :clml.hjs.matrix :clml.statistics
        :clml.time-series.read-data)
  #-lispworks
  (:import-from :uiop/run-program #:run-program)
  #+allegro
  (:use :excl)

  (:export #:ts-to-sta
           #:timeseries-model
           #:sub-ts
           #:compose-ts
           #:merge-ts
           #:observed-ts
           #:predict
           #:statvis
           #:draw-ppm
           #:open-eps-file
           #:date-time-to-ut
           #:ut-to-date-time
           #:ts-)
  #+lispworks
  (:export            #:*r-stream*
                      #:with-r)
  )

(defpackage :clml.time-series.statistics
  (:use :cl :clml.hjs.read-data :clml.hjs.meta :clml.hjs.vector
        :clml.hjs.matrix :clml.statistics :clml.hjs.vars
        :clml.time-series.read-data
        :clml.time-series.util
        :clml.numeric.fast-fourier-transform)

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
   )
  #+lispworks
  (:export
   ;; These functions are dependant on R interface which is currently
   ;; dependant on lispworks
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
  (:use :cl :clml.hjs.read-data :clml.hjs.meta :clml.hjs.vector :clml.hjs.matrix
   :clml.statistics :clml.time-series.util :clml.time-series.statistics
        :clml.time-series.read-data
        :clml.hjs.missing-value)


  (:export
   #:trend #:trend-prediction
   #:seasonal #:seasonal-adj
   #:state-space-model
   #:trend
   ))

(defpackage :clml.time-series.autoregression
  (:use :cl
        :clml.hjs.read-data
        :clml.hjs.meta
        :clml.hjs.vector
        :clml.hjs.matrix
        :clml.hjs.vars
        :clml.statistics
        :clml.time-series.util
        :clml.time-series.statistics
        :clml.time-series.read-data
        :clml.time-series.state-space)
  ;(:shadow :predict)

  (:export
   #:ar #:ar-prediction
   #:parcor #:parcor-filtering
   ;#:predict
   #:ar-model)
  (:documentation "Package for AutoRegression model
*** sample usage
#+INCLUDE: \"../sample/time-series-autoregression.org\"  example lisp"))

(defpackage :clml.time-series.changefinder
  (:use :cl
        :clml.hjs.read-data
        :clml.hjs.meta
        :clml.hjs.vector
        :clml.hjs.matrix
        :clml.statistics
        :clml.time-series.util
        :clml.time-series.statistics
        :clml.time-series.read-data
        :clml.hjs.missing-value
        )
  (:shadow :predict)

  (:export :init-changefinder
           :update-changefinder)
  (:documentation "ChangeFinder
Package for \"ChangeFinder\"

**** Comments
- A value of 0.01 has been added to the diagonal elements of the covariance matrix
  for the stability of the inverse matrix calculation.
  User can edit this value by the special variable named *stabilizer*.
**** Reference
- J. Takeuchi, K. Yamanishi \"A Unifying framework for detecting outliers and change points from time series\"
- K. Yamanishi \"データマイニングによる異常検知\" p.45-58
")
  )


(defpackage :clml.time-series.anomaly-detection
  (:use :cl
        :clml.hjs.read-data
        :clml.hjs.meta
        :clml.hjs.vector
        :clml.hjs.matrix
        :clml.hjs.vars
        :clml.hjs.missing-value
        ::clml.hjs.eigensystems
        :clml.utility.csv
        :clml.statistics
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
  (:shadow :predict)
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
        :clml.hjs.meta
        :clml.hjs.vector
        :clml.hjs.vars
        :clml.hjs.read-data
        :clml.time-series.read-data)

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
  (:use :cl :clml.hjs.read-data :clml.hjs.missing-value
        :clml.time-series.util :clml.time-series.statistics :clml.time-series.read-data)

  (:export
   #:continuous-kleinberg
   #:print-burst-indices
   #:enumerate-kleinberg))

(defpackage :clml.time-series.finance
  (:use :cl)
  (:import-from :clml.hjs.read-data #:read-data-from-file #:dataset-points
                #:pick-and-specialize-data #:dataset-dimensions #:dimension-name)
  (:import-from :clml.hjs.meta #:v2dvec)
  (:import-from :clml.time-series.read-data #:time-series-data #:ts-points #:time-series-dataset
                #:ts-p-pos :ts-start #:ts-end #:ts-freq #:make-constant-time-series-data
                #:ts-p-label)
  (:import-from :clml.hjs.missing-value #:+nan+ )

  (:export
   #:atr)

  (:documentation "Financial Analysis Algorithims for time-series finance data

#+BEGIN_SRC lisp

; Calulate tr and atr for daily tick data for QQQ from 1 April 2010 to 13 May 2010
(defparameter cs-atr-csv
  (read-data-from-file
   (clml.utility.data:fetch \"https://mmaul.github.io/clml.data/sample/cs-atr.csv\")
   :type :csv :csv-type-spec '(string double-float double-float double-float double-float
                               double-float double-float double-float double-float)
   :missing-values-list '(\NA\")))

(defparameter cs-atr-ts (time-series-data cs-atr-csv :time-label 0 :range '(1 2 3)))
(defparameter qqq-atr-ts (atr cs-atr-ts :high 0 :low 1 :close 2))

(format t \"~9A \" (or (clml.time-series.read-data:time-label-name qqq-atr-ts) \"Time\"))
(loop for d across (dataset-dimensions qqq-atr-ts)
      do (format t \"~8a  \" (dimension-name d)))
(loop for line across (ts-points qqq-atr-ts)
      do (format t \"~&~9A ~{~5$~^ ~}~%\" (ts-p-label line) (coerce  (ts-p-pos line) 'list)))

#+END_SRC
"
))


(defpackage :linear-regression
  (:use :cl
        :hjs.learn.read-data
	:hjs.util.matrix
        :hjs.util.meta)
  (:import-from :hc 
		#:i-thvector
		#:square-sum
		#:product-sum
		#:vector-shift
		#:vector-mean)
  (:export #:mlr
	   #:residual-vector
	   #:residual-quantile-vector
	   #:residual-std-err
           #:std-err-vector
	   #:t-value-vector
	   #:d.f
	   #:pt
	   #:pt-value-vector
	   #:r^2
	   #:adjusted-r^2
	   #:f-value
	   #:pf
	   #:pf-value))

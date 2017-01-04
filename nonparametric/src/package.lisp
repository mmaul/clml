
(defpackage :clml.nonparametric.statistics

  (:use :cl :clml.hjs.matrix :clml.hjs.vector :clml.hjs.meta)
  (:export :make-adarray

           :unit-random
           :bernoulli
           :exp-random
           :beta-random
           :gamma-random
           :normal-random
           :normal-density
           :chi-square-random
           :randomize-choice
           :randomize-slice
           :jackup-logged-prob
           :shuffle-vector
           :random-elt
           :normalize!
           :get-n-best
           :safe-exp
           :safe-expt
           :stirling-number
           :dirichlet-random
           :binomial-random
           :cauchy-random

           :gamma-function
           :beta-function
           :loggamma
           :digamma
           :trigamma

           :multivariate-normal-density
           :%multivariate-normal-density
           :multivariate-normal-logged-density
           :%multivariate-normal-logged-density
           :multivariate-normal-random
           :LUed-wishart-random

           :outer-product
           :map-matrix-cell
           :map-matrix-cell!
           :crossproduct
           :cholesky-decomp

           :+most-negative-exp-able-float+
           :+most-positive-exp-able-float+

           :*randomize-trace*))


(defpackage :clml.nonparametric.dpm

  (:use :cl
        :clml.hjs.meta
        :clml.hjs.matrix
        :clml.hjs.vector
        :clml.nonparametric.statistics)
  (:export :dpm
           :dpm-k
           :dpm-p
           :dpm-base
           :dpm-clusters
           :dpm-hyper
           :dpm-data
           :dpm-cluster-layers
           :estimate-base?

           :logged-dpm

           :point
           :make-point
           :point-data
           :point-cluster

           :cluster
           :gaussian-cluster
           :cluster-size
           :cluster-center
           :cluster-std

           :dp-distribution
           :dp-gaussian
           :cluster-class
           :average-of-average
           :std-of-average
           :average-of-std
           :gauss-dpm

           :density-to-cluster
           :base-distribution
           :make-new-cluster
           :sample-cluster-parameters
           :sample-distribution

           :add-customer
           :remove-customer
           :add-to-cluster
           :remove-from-cluster

           :cluster-rotation

           :initialize
           :sampling
           :seatings-sampling
           :parameters-sampling
           :hypers-sampling

           :make-cluster-result
           :head-clusters

           :*hyper-base-a*
           :*hyper-base-b*

        :multivar-gaussian-cluster
           :multivar-gauss-dpm
           :multivar-dp-gaussian
       ))

(defpackage :clml.nonparametric.hdp-lda
  (:use :cl :clml.nonparametric.statistics :clml.hjs.meta)
  (:export :hdp-lda
           :word
           :document
           :table

           :document-id
           :document-words
           :document-thetas

           :word-id

           :topic-count
           :hdp-lda-data
           :vocabulary

           :add-customer
           :remove-customer
           :sample-new-topic
           :hypers-sampling

           :initialize
           :sampling
           :assign-theta
           :get-phi

           :get-top-n-words
           :revert-word

           :*alpha-base-a*
           :*alpha-base-b*
           :*gamma-base-a*
           :*gamma-base-b*

           :*default-beta*

       )
  (:documentation "Package for Latent-Dirichlet-Allocation by Hierarchical-Dirichlet-Process

*** sample usage
#+INCLUDE: \"../sample/svm-validation.org\"  example lisp
")
  )

(defpackage :clml.nonparametric.hdp

  (:use :cl :clml.hjs.meta :clml.nonparametric.statistics :clml.nonparametric.dpm)
  (:export :hdp-cluster
           :cluster-latent-table
           :cluster-tmp-table
           :cluster-beta

           :hdp
           :hdp-gamma
           :hdp-beta

           :sample-latent-table

           :hdp-distribution

           :sliced-hdp
           ))

(defpackage :clml.nonparametric.hdp-hmm

  (:use :cl
        :clml.hjs.meta
        :clml.nonparametric.statistics
        :clml.nonparametric.dpm
        :clml.nonparametric.hdp)
  (:export :hidden-state
           :emission

           :hdp-hmm
           :vocabulary
           :hdp-hmm-eos

           :cluster-dist-table
           :trans-prob
           :emission-prob

           :state-uniform

           :make-pattern-data
           :make-repeat-pattern
           :show-hidden-states

           :*smooth-beta*
           ))

(defpackage :clml.nonparametric.gauss-hdp-hmm

  (:use :cl)
  (:import-from :clml.nonparametric.dpm
                :gaussian-cluster
                )
  (:export :gaussian-state
           :gauss-hdp-hmm
           :state-gaussian

           :make-sticky-test))

(defpackage :clml.nonparametric.sticky-hdp-hmm

  (:use :cl :clml.nonparametric.statistics :clml.hjs.meta
        :clml.nonparametric.dpm
        :clml.nonparametric.hdp
        :clml.nonparametric.hdp-hmm)
  (:export :sticky-hdp-hmm
           :sticky-hidden-state
           :sticky-state-uniform

           :sticky-kappa

           :*rho-base-c*
           :*rho-base-d*))

(defpackage :clml.nonparametric.blocked-hdp-hmm

  (:use :cl :clml.nonparametric.statistics :clml.hjs.meta
        :clml.nonparametric.hdp-hmm
        :clml.nonparametric.dpm
        :clml.nonparametric.hdp
        )
  (:export :blocked-hidden-state
           :blocked-hdp-hmm
           :block-uniform

           :point-sequence
           :sequence-data
           :seq-point

           :sampling-pi

           :sorted-before
           :hdp-hmm-l
           :state-pi))

(defpackage :clml.nonparametric.ihmm

  (:use :cl :clml.hjs.meta
        :clml.nonparametric.statistics
        :clml.nonparametric.dpm
        :clml.nonparametric.hdp
        :clml.nonparametric.hdp-hmm
        :clml.nonparametric.sticky-hdp-hmm
        :clml.nonparametric.blocked-hdp-hmm)
  (:export :ihmm
           :ihmm-state
           :ihmm-state-uniform))




(defpackage :clml.nonparametric.ftm

  (:use :cl
        :clml.nonparametric.statistics
        :clml.nonparametric.dpm)
  (:export :ftm-topic
           :topic-pi
           :topic-phi

           :document

           :ftm
           :ftm-ibp-alpha

           :ftm-uniform

           :get-top-n-words
   ))

(defpackage :clml.nonparametric.lfm

  (:use :cl :clml.nonparametric.statistics :clml.hjs.meta
        :clml.hjs.matrix :clml.hjs.vector
        :clml.nonparametric.dpm)
  (:export :ibp
           :ibp-row
           :ibp-distribution

           :lfm

           :lfm-row
           :row-weight

           :lfm-distribution

           ))
(defpackage :clml.nonparametric.dpm-interface

  (:use :cl :clml.hjs.read-data :clml.nonparametric.dpm
        :clml.hjs.meta :clml.hjs.vector :clml.hjs.matrix)
  (:import-from :clml.nonparametric.dpm
                #:average-of-average
                #:std-of-average
                #:average-of-std
                )
  (:export
   #:dpm-k
   #:dpm-hyper
   #:dpm-base
   #:multivar-gauss-dpm
   #:multivar-dp-gaussian
   #:average-of-average
   #:std-of-average
   #:average-of-std
   #:gaussian-dpm
   #:get-cluster-info
   #:get-cluster-parameter
   ;; #:make-workspace-for-validation
   ))

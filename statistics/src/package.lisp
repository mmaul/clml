;;; -*- mode: lisp; syntax: common-lisp -*-

;;; Peter Salvi, 2008
;;; Ryo Fujii, 2011

(in-package :cl-user)

(defpackage :clml.statistics
  (:nicknames :distribution :statistics)
  (:use :cl :clml.statistics.rand)
  (:export
   ;; DATA ANALYSIS
   ;; (Functions on 1-valued data)
   :mean :mode :count-values
   :median :median-on-sorted
   :discrete-quantile :discrete-quantile-on-sorted
   :five-number-summary :five-number-summary-on-sorted
   :range
   :interquartile-range :interquartile-range-on-sorted
   :mean-deviation
   :variance
   :standard-deviation
   ;; (Functions on 2-valued data)
   :covariance
   :linear-regression
   :correlation-coefficient
   :spearman-rank-correlation
   :kendall-rank-correlation
   ;; PROBABILITY DISTRIBUTION
   :cdf :density :quantile :rand :rand-n
   :normal-distribution :standard-normal-distribution
   :normal-distribution-estimate-unbiased
   :normal-distribution-estimate-maximum-likelihood
   :log-normal-distribution
   :log-normal-distribution-estimate-unbiased
   :log-normal-distribution-estimate-maximum-likelihood
   :uniform-distribution :standard-uniform-distribution
   :uniform-distribution-estimate-moments
   :uniform-distribution-estimate-maximum-likelihood
   :erlang-distribution :erlang-distribution-estimate
   :exponential-distribution :exponential-distribution-estimate
   :gamma-distribution :gamma-distribution-estimate
   :quantile-ili
   :chi-square-distribution
   :t-distribution
   :beta-distribution :beta-distribution-estimate
   :f-distribution
   :binomial-distribution :binomial-distribution-estimate
   :geometric-distribution :geometric-distribution-estimate
   :hypergeometric-distribution
   :hypergeometric-distribution-estimate-successes-unbiased
   :hypergeometric-distribution-estimate-successes-maximum-likelihood
   :hypergeometric-distribution-estimate-elements
   :cauchy-distribution :cauchy-distribution-estimate
   :pascal-distribution
   :pascal-distribution-estimate-maximum-likelihood
   :pascal-distribution-estimate-unbiased
   :negative-binomial-distribution
   :negative-binomial-distribution-estimate-unbiased
   :negative-binomial-distribution-estimate-maximum-likelihood
   :logistic-distribution :logistic-distribution-estimate
   :poisson-distribution :poisson-distribution-estimate
   :weibull-distribution :weibull-distribution-estimate

   ;; DISTRIBUTION tests
   :normal-dist-test :poisson-dist-test
   :binom-dist-test 
   ;; Outlier verification
   :smirnov-grubbs
   :smirnov-grubbs-p
   )
  (:documentation "
** Notes
- Numbers are not converted to (double) floats, for better accuracy with
  whole number data. This should be OK, since double data will generate
  double results (the number type is preserved).
- Places marked with TODO are not optimal or not finished (see the TODO
  file for more details).


*** Distributions
Distributions are CLOS objects, and they are created by the constructor
of the same name. The objects support the methods CDF (cumulative
distribution function), DENSITY (MASS for discrete distributions),
QUANTILE, RAND (gives a random number according to the given distribution),
RAND-N (convenience function that gives n random numbers), MEAN and
VARIANCE (giving the distribution's mean and variance, respectively).
These take the distribution as their first parameter.

Most distributions can also be created with an estimator constructor.
The estimator function has the form <distribution>-ESTIMATE, unless noted.
")

  )















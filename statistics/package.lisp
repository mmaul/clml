;;; -*- mode: lisp; syntax: common-lisp -*-

;;; Peter Salvi, 2008

(in-package :cl-user)

(defpackage :statistics
  (:nicknames :stat)
  (:use :common-lisp)
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
   ))















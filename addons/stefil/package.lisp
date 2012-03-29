;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-user)

(defpackage :stefil
  (:shadow #:log)

  (:use
   :common-lisp
   :swank
   :metabang-bind
   :alexandria
   :iterate
   :stefil-system
   )

  (:export
   #:find-test
   #:deftest
   #:is
   #:signals
   #:finishes
   #:with-expected-failures
   #:in-suite
   #:root-suite
   #:in-root-suite
   #:defsuite
   #:defsuite*
   #:run-child-tests
   #:defixture
   #:with-fixture
   #:with-fixtures
   #:runs-without-failure?
   #:without-debugging
   #:without-test-progress-printing
   #:funcall-test-with-feedback-message
   #:run-failed-tests

   #:*display-all-slots-in-inspector* ; to also display the raw slots, disabled by default
   #:*test-progress-print-right-margin*
   #:*test-result-history*
   #:*last-test-result*
 
   ;; these are the defaults from which the test context is initialized
   #:*print-test-run-progress*
   #:*debug-on-unexpected-error*
   #:*debug-on-assertion-failure*
   ))



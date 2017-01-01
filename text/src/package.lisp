

(defpackage :clml.text.utilities

  (:use :common-lisp
        :clml.hjs.read-data)
  (:export :calculate-string-similarity
           :equivalence-clustering
           :calculate-lcs-distance
           :calculate-levenshtein-similarity
           :equivalence-clustering

           )
  (:documentation "Text Utilities
*** sample usage
#+INCLUDE: \"../sample/text-utils.org\"  example lisp "))

;; package of interfaces for :text.hdp-lda
(defpackage :clml.text.hdp-lda

  (:use :cl :clml.hjs.read-data :clml.hjs.vector :clml.nonparametric.hdp-lda)

  (:export #:hdp-lda
           #:get-trend-topics
           #:extract-words
           #:topic-names
           #:hdp-lda-gamma
           #:make-document-theta-result
           #:make-topic-beta-result
           #:make-docs
           #:make-bow-hash))


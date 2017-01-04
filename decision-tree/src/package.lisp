(defpackage :clml.decision-tree.decision-tree

  (:use :cl
        :clml.hjs.read-data
        :clml.hjs.matrix)
  (:export
   #:make-decision-tree
   #:make-regression-tree
   #:print-decision-tree
   #:print-regression-tree
   #:predict-decision-tree
   #:predict-regression-tree
   #:decision-tree-validation
   #:regression-tree-validation
   #:delta-variance
   #:delta-entropy
   #:delta-gini
   #:split
   #:whole-row-numbers-list
   #:variance
   #:mean
   #:entropy
   #:gini-index
   #:sum-up-results
   #:make-split-predicate
   #:total
   #:column-name->column-number
   #:sum-up
   #:make-variable-index-hash)
  (:documentation "decision tree package"))

(defpackage :clml.decision-tree.random-forest

  (:use :cl
        :clml.hjs.read-data
        :clml.decision-tree.decision-tree)
  (:import-from :clml.decision-tree.decision-tree
                #:make-variable-index-hash
                #:sum-up
                #:column-name->column-number
                #:total
                #:make-split-predicate
                #:sum-up-results
                #:gini-index
                #:entropy
                #:mean
                #:variance
                #:whole-row-numbers-list
                #:split
                #:delta-gini
                #:delta-entropy
                #:delta-variance)
  (:export
   #:make-random-forest
   #:make-regression-forest
   #:predict-forest
   #:importance
   #:predict-regression-forest
   #:forest-validation
   #:regression-forest-validation)
  (:documentation "random forest package

*** sample usage
#+INCLUDE: \"../sample/random-forest.org\" example lisp"))

(defpackage :clml.association-rule

  (:use :cl
        :clml.hjs.vector
        :clml.hjs.read-data)
  (:export :association-analyze
           :association-analyze-file
           :apply-rules
           :read-association-list
           :%association-analyze-apriori
           :%association-analyze-da
           :%association-analyze-ap-genrule
           :%association-analyze-da-ap-genrule
           :%association-analyze-fp-growth
           :%association-analyze-eclat
           :%association-analyze-lcm
           :%association-analyze-eclat
           :assoc-result-rules
           :assoc-result-header
           :association-rule
           :write-assoc-result-to-stream
           :read-assoc-result-from-stream
           )
  (:documentation "Package for association rule analysis

Implementation of association rule learning, association rule learing is a method of discovering relationships between specific values of categorical variables in a dataset. These relationships are are expressed as rules.

*** sample usage
#+INCLUDE: \"../sample/association-analyze.org\" example lisp"))

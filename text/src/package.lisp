

(defpackage :clml.text.utilities
  (:nicknames :text-utils :text-utilities)
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


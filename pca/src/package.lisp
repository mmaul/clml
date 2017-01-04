(defpackage :clml.pca

  (:use :cl
        :clml.hjs.meta
        :clml.hjs.matrix
        :clml.hjs.eigensystems
        :clml.hjs.vector
        :clml.hjs.read-data
        :clml.statistics
        :clml.hjs.vars
        :clml.hjs.missing-value)

  (:export
   #:pca-result
   #:princomp
   #:princomp-projection
   #:sub-princomp
   #:kernel-princomp
   #:make-face-estimator
   #:face-estimate
   #:components
   #:contributions
   #:loading-factors
   #:pca-method
   #:centroid
   )
  (:documentation "*** Note
- when using princomp and sub-princomp, if there exists two columns
  that are of same value, the result for :correlation
  method will not be converged. Therefore pick-and-specialize-data or
  divide-dataset must be used to remove one column.

*** sample usage
#+INCLUDE: \"../sample/face-estimate.org\" example lisp" )
  )

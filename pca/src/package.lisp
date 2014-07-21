(defpackage :clml.pca
  (:nicknames :hjs.learn.pca)
  (:use :cl
        :hjs.util.meta
        :hjs.util.matrix
        :hjs.util.eigensystems
        :hjs.util.vector
        :hjs.learn.read-data
        :clml.statistics
        :hjs.learn.vars)
  (:nicknames :pca)
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

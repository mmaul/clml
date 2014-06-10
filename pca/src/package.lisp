(defpackage :clml.pca
  (:nicknames :hjs.learn.pca)
  (:use :cl
        :hjs.util.meta
        :hjs.util.matrix
        :hjs.util.eigensystems
        :hjs.util.vector
        :hjs.learn.read-data
        :statistics
        :hjs.learn.vars)
  (:nicknames :pca)
  (:export 
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
   ))

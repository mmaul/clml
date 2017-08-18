;;;

(defpackage :clml.clustering.hc

  (:use :cl
        :clml.hjs.read-data
        :clml.hjs.vector
        :clml.hjs.matrix
        :clml.hjs.meta)
  (:export #:cophenetic-matrix
           #:cophenetic-cc
           #:distance-matrix
           #:hc-average
           #:hc-ward
           #:hc-single
           #:hc-complete
           #:hc-centroid
           #:hc-median
           #:cutree
       #:vector-sum
       #:vector-mean
       #:vector-shift
       #:product-sum
       #:square-sum
       #:i-thvector
       #:numeric-matrix
       #:max-vector
       #:min-vector
       #:pick-up-column
       #:pick-up-row)
  (:documentation "hierarchical-clustering package

*** sample usage
#+INCLUDE: \"../sample/hierarchial-clustering.org\" example lisp")
  )

(defpackage :clml.clustering.nmf

  (:use :cl
        :clml.clustering.hc
        :clml.blas
        :clml.lapack
        :clml.hjs.read-data
        :clml.hjs.matrix
    :clml.hjs.meta)
  (:import-from :clml.clustering.hc
                #:i-thvector
                #:vector-sum
                #:vector-mean
                #:vector-shift
                #:product-sum
                #:square-sum
                #:max-vector
                #:min-vector
                #:pick-up-row
                #:pick-up-column
                #:numeric-matrix)

  (:export #:nmf
           #:nmf-clustering
           #:rho-k
           #:nmf-analysis
           #:nmf-corpus-analysis
           #:c^3m-cluster-number
           #:nmf-search
           #:nmf-corpus-search
           #:nmf-sc
           #:sparseness
           #:sample-matrix))

(defpackage :clml.clustering.optics

  (:use :cl
        :clml.hjs.read-data
        :clml.hjs.matrix
        :clml.statistics
        :clml.nearest-search.nearest)
  (:export :optics
           :optics-main
           :make-optics-input)
  (:documentation "OPTICS -- density-based clustering package ")
  )

(defpackage :clml.clustering.optics-speed

  (:use
   :clml.nearest-search.nearest
   :clml.clustering.optics
   )
  (:export
   :optics-input-speed
   :get-neighbors
   :optics-speed
   :%optics-speec
   :make-optics-input-speed))

(defpackage :clml.clustering.spectral-clustering

    (:use :cl :clml.hjs.matrix :clml.hjs.meta :clml.hjs.eigensystems)
    (:export #:spectral-clustering-mcut
             #:*sample-w*)
    (:import-from #+allegro :excl #+sbcl "SB-INT" #+lispworks "LISPWORKS" #+ccl "CCL"
                  #:fixnump)
    (:documentation "   Package for undirected graph clustering"))

(defpackage :clml.clustering.cluster-validation

  (:use :cl
        :clml.hjs.k-means
        :clml.hjs.vector
        :clml.hjs.meta
        :iterate)
  (:import-from :clml.hjs.k-means :point)
  #+ccl
  (:import-from :ccl :fixnump)
  (:export
   :default-init-workspace
   :*workspace*
   :dunn-index
   :davies-bouldin-index
   :calinski
   :hartigan
   :ball-and-hall
   :global-silhouette-value
   :centroid)
  (:documentation "*** sample usage
#+INCLUDE: \"../sample/cluster-validation.org\" example lisp
*** reference
- [[http://www.msi.co.jp/vmstudio/materials/tech/index.html][VMS Technical Reference]]
- [[http://www.cs.tcd.ie/publications/tech-reports/reports.02/TCD-CS-2002-33.pdf][Cluster validation techniques for genome expression data]]"))


(defpackage :clml.clustering.k-means2
    (:use :cl :clml.hjs.vector :clml.hjs.meta
          :clml.statistics :clml.hjs.matrix)

  (:export #:k-means
           #:make-cluster
           #:c-center
           #:c-size
           #:c-points
           #:cluster

           #:pw-points
           #:pw-clusters

           #:p-pos
           #:p-owner
           #:point

           #:get-cluster-centroids
           #:get-cluster-points
           ))
#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-int::set-floating-point-modes
   :traps
   (remove :invalid (getf (sb-int:get-floating-point-modes) :traps))))
#+ccl
(eval-when (:compile-toplevel :load-toplevel :execute)
 (ccl::set-fpu-mode :invalid nil))

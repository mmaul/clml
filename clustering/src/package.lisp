;;;

(defpackage :clml.clustering.hc
  (:nicknames :hc)
  (:use :cl
	:hjs.learn.read-data
	:hjs.util.vector
	:hjs.util.matrix
        :hjs.util.meta)
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
       #:pick-up-row))

(defpackage :clml.clustering.nmf
  (:nicknames :nmf)
  (:use :cl
	:hc
	:blas
	:lapack
	:hjs.learn.read-data
	:hjs.util.matrix
        :hjs.util.meta)
  (:import-from :hc 
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
  (:nicknames :optics)
  (:use :cl
        :hjs.learn.read-data
        :hjs.util.matrix
        :statistics)
  (:export :optics
           :optics-main
           :make-optics-input))

(defpackage :clml.clustering.optics-speed
  (:nicknames :optics-speed)
  (:use
   :optics
   :learn.nearest)
  (:export
   :optics-input-speed
   :get-neighbors
   :optics-speed
   :%optics-speec
   :make-optics-input-speed))

(defpackage :clml.clustering.spectral-clustering
  (:nicknames :spectral-clustering)
    (:use :cl :hjs.util.matrix :hjs.util.meta)
    (:export #:spectral-clustering-mcut
             #:*sample-w*)
    (:import-from #+allegro "EXCL" #+sbcl "SB-INT" #+lispworks "LISPWORKS" #+ccl "CCL"
                  "FIXNUMP"))

(defpackage :clml.clustering.cluster-validation
  (:nicknames :cluster-validation)
  (:use :cl
        :hjs.learn.k-means
        :hjs.util.vector
        :hjs.util.meta
        :iterate)
  (:import-from :hjs.learn.k-means :point)
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
   :centroid))

(defpackage :clml.clustering.k-means2
    (:use :cl :hjs.util.vector :hjs.util.meta
          :statistics :hjs.util.matrix)
  (:nicknames :k-means2)
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

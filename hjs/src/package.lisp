(defpackage :hjs.util.meta
  (:use :cl)
  (:nicknames :util)
  (:export #:with-unique-names
	   #:once-only
           #:defun-speedy
           #:defdoublefunc
           #:gethash-or-set
           #:dmat
	   #:dvec
	   #:cvec
	   #:make-dvec
	   #:array-index
           #:dfloat
	   #:+fl
	   #:-fl
	   #:*fl
	   #:/fl
           #:safe-/
	   #:batch-elt
           #:split-seq-odd-even
           #:d-expt
           #:d-exp
           #:get-underlying-1d-array
           #:vecs2mat
           #:vecs2flatmat
           #:mat2vecs
           #:flatmat2vecs
	   ))
(defpackage :hjs.learn.vars
  (:use :cl)
  (:nicknames :vars)
  (:import-from :alexandria #:define-constant)
  (:export #:*epsilon*
           #:*workers*
           #:*most-negative-exp-able-float*
           #:*most-positive-exp-able-float*
           ))


(defpackage :hjs.util.vector
  (:use :cl
	:hjs.util.meta)
  (:nicknames :vector)
  (:export #:make-dvec
           #:fill-vec
           #:do-vec
           #:do-vecs
           #+future #:par-do-vec
           #:copy-vec
           #:v+
           #:v-
           #:v-scale
           #:inner-product
           #:inner-product-unsafe
           #:distance-to-origin
           #:euclid-distance
           #:manhattan-distance
           #:cosine-distance
           #:hausdorff-distance
           #:normalize-vec
           #:reorder-vec
           #:reorder-dvec
           #:specialize-vec
           #:mean-points
           ))

;; --
(defpackage :hjs.util.matrix
  (:use :cl :hjs.util.meta :hjs.util.vector :blas :lapack)
  (:nicknames :matrix)
  (:export #:sum-mat
	   #:copy-mat
	   #:nrow
	   #:ncol
	   #:transpose
	   #:transposeV
           #:trans
	   #:dmat
	   #:specialize-mat
           #:diag
           #:vcv
           #:mcm
           #:vdotv
           #:m*v
           #:m*m
           #+mkl #:symat-ev
           #:m^-1
           #:tr
           #:det
           #+mkl #:solve-linear-eq
           #:make-dmat
           #:append-mat
           #:standard-deviations-from-covariance
           #:standard-deviations
           #:covariance-matrix
           #:correlation-matrix
           #:standardize
           #:regularize-covariance
           #:vecs2mat
           #:vecs2flatmat
           #:mat2vecs
           #:flatmat2vecs
           #:row-aref
           #:c*mat
	   ))

(defpackage :hjs.util.missing-value
  (:use :cl :util :vector :clml.statistics :clml.statistics.distribution hjs.util.meta)
  (:import-from :alexandria #:define-constant)
  (:nicknames :missing-val :handling-missing-value)
  (:export
   #:*missing-values* #:missing-value-p
   #:*na* #:*nan* #:*c-nan* #:*+inf* #:*-inf*
   #:fill-na #:na-p #:nan-p #:c-nan-p
   #:outlier-verification #:interpolate))


(defpackage :hjs.learn.read-data
  (:documentation "package for reading data for machine learning")
  (:use :cl :hjs.util.meta :hjs.util.vector :hjs.learn.vars :hjs.util.matrix
        :hjs.util.missing-value
        ;handling-missing-value
        )
  (:nicknames :read-data)
  (:import-from #:hjs.util.missing-value
                ;:handling-missing-value
                #:interpolate
                )
  (:import-from :alexandria #:define-constant)
  (:export 
   #:read-data-from-file
   #:pick-and-specialize-data
   #:divide-dataset
   #:dataset-dimensions
   #:dataset-points
   #:unspecialized-dataset
   #:specialized-dataset
   #:numeric-dataset
   #:numeric-matrix-dataset
   #:dataset-numeric-points
   #:numeric-and-category-dataset
   #:numeric-matrix-and-category-dataset
   #:dataset-category-points
   #:dimension-name
   #:dimension-type
   #:dimension-index
   #:dimension-metadata
   #:make-dimension
   #:copy-dimension
   #:make-unspecialized-dataset
   #:make-numeric-dataset
   #:make-numeric-matrix-dataset
   #:make-numeric-and-category-dataset
   #:make-numeric-matrix-and-category-dataset
   #:choice-a-dimension
   #:choice-dimensions
   #:dataset-cleaning
   #:copy-dataset
   #:make-bootstrap-sample-datasets))

(defpackage :hjs.learn.k-means
  (:use :cl :hjs.util.vector :hjs.util.meta :hjs.learn.read-data
	:clml.statistics :hjs.util.matrix :iterate
	:hjs.learn.vars)
  (:nicknames :k-means)
  (:export
   #:k-means
   #:make-cluster
   #:c-center
   #:c-size
   #:c-points
   #:cluster
   #:point
   #:p-point
   #:%make-point
   #:copy-point
   #:id
   #:pos
   #:owner
   #:pw-points
   #:pw-clusters
   #:make-random-state-with-seed 
   #:p-pos
   #:p-owner
   #:point
     
   #:get-cluster-centroids
   #:get-cluster-points
   )
  (:documentation "*** sample usage
#+INCLUDE: "../sample/k-means.org" example lisp"))

(defpackage :hjs.util.eigensystems
  (:use :cl
        :hjs.util.meta
        :hjs.util.vector
        :hjs.util.matrix
        :blas
        :lapack)
  (:nicknames :eigensystems)
  (:export
   #:jacobi
   #:eigsrt
   #:tred2
   #:tqli
   #:balanc
   #:elmhes
   #:eigen-by-jacobi
   #:eigen-by-householder-ql
   #:eigen-by-power

   ))

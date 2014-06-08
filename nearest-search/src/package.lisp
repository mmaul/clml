
(defpackage :clml.nearest-search.k-nn
  (:nicknames :k-nn :learn.k-nn)
  (:use :cl
        :hjs.util.vector
        :hjs.learn.read-data
        :hjs.util.meta)
  (:export :k-nn-analyze
           :k-nn-estimate
           :estimator-properties))

(defpackage :clml.nearest-search.nearest
  (:nicknames :learn.nearest)
  (:use :cl
	:hjs.learn.read-data
	:hjs.util.vector
	:hjs.util.meta
	:nonpara.stat
	:hjs.util.matrix
	:clml.pca
	:clml.utility.priority-que)
  (:export :nearest-search
	   
	   :exact-nearest-search
	   :stochastic-nearest-search
	   
	   :naive-nearest-search
	   
	   :kd-tree-search
	   
	   :m-tree-search
	   
	   :locality-sensitive-hashing
	   :p-stable-locality-sensitive-hashing
	   :euclid-locality-sensitive-hashing
	   :manhattan-locality-sensitive-hashing
	   :cosine-locality-sensitive-hashing
	   
	   :nns-input-data
	   :nns-input-key
	   :nns-distance
	   
	   :initialize-search
	   
	   :find-nearest
	   :find-nearest-k
	   :find-nearest-epsilon
	   
	   :stochastic-validation
	   ))

(defpackage :clml.nearest-search.k-nn-new
  (:nicknames :k-nn-new :learn.k-nn-new)
  (:use :cl
        :hjs.util.vector
        :hjs.learn.read-data
        :hjs.util.meta
        :clml.nearest-search.nearest)
  (:export :k-nn-analyze
           :k-nn-estimate
           :estimator-properties))



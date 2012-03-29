(defpackage :clml.k-means
    (:use :cl :hjs.util.vector :hjs.util.meta
          :statistics :hjs.util.matrix)
  (:nicknames :k-means)
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

(in-package :clml.k-means)

;;;; global variables
(defparameter *num-of-trials* 10)
(defparameter *distance-function* #'euclid-distance)
(defparameter *k-means-random-state* (make-random-state t))
(defparameter *max-iteration* 1000)

(declaim (type fixnum *num-of-trials* *max-iteration*))

(defstruct distance-function
  name 
  v.v
  m.v
  m.m)

(defun trial ()
  (loop
    do (progn
         (update-lower-bounds problem-workspace)
         (select-points-for-update)
         (update-center-for-selected-points)
         (update-all-centers-and-save-old-centers)
         (update-distance-between-point-and-owner)
         (decf rest-iteration))
    until (stop-p))
  )

(defun k-means ()
  (loop repeat num-of-trials
        do
     (trial)
     (update-best-trial))
  )

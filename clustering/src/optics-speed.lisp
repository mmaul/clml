;; optics with nearest-serach package
(defpackage :optics
  (:use :learn.nearest))

(in-package :optics)

(defclass optics-input-speed (optics-input)
  ((nns-args  :initform nil    :initarg :nns-args :accessor nns-args)
   (nns-type   :initform :naive :initarg :nns-type :accessor nns-type)
   (nn-search :initarg :nn-search :accessor nn-search)
   (neighbors :initform (make-array 0 :fill-pointer t :adjustable t) :accessor neighbors)))

(defmethod get-neighbors ((input optics-input-speed) object)
  (delete object (find-nearest-epsilon (nn-search input) object (epsilon input) (neighbors input))))

(defun optics-speed (input-path epsilon min-pts r-epsilon
		     target-cols &key (file-type :sexp) 
				      (csv-type-spec '(string double-float double-float))
				      (distance :manhattan)
				      (normalize nil)
				      (external-format :default)
				      (nns-type :naive)
				      (nns-args nil))
  (assert (plusp epsilon))
  (assert (plusp min-pts))
  (assert (and (plusp r-epsilon) (<= r-epsilon epsilon)))
  (assert target-cols)
  (let* ((dataset
          (read-data-from-file input-path 
                               :type file-type
                               :csv-type-spec csv-type-spec
                               :external-format external-format))
         (range (map 'list
                  #'(lambda (col-name)
                      (dimension-index
                       (find col-name (dataset-dimensions dataset)
                             :key #'dimension-name :test #'string-equal)))
                  target-cols)))
    (setq dataset
      (pick-and-specialize-data dataset
                                :range range
                                :data-types 
                                (make-list (length range)
                                           :initial-element :numeric)))
    (%optics-speed dataset epsilon min-pts r-epsilon
		   :distance distance :normalize normalize :nns-type nns-type :nns-args nns-args)))

(defun %optics-speed (numeric-dataset epsilon min-pts r-epsilon &key (distance :manhattan) normalize (nns-type :naive) nns-args)
  (when normalize
    (setf (dataset-numeric-points numeric-dataset) 
      (standardize (dataset-numeric-points numeric-dataset))))
  (let* ((optics-input (make-optics-input-speed
                        (dataset-numeric-points numeric-dataset)
                        epsilon min-pts r-epsilon
                        :distance distance :normalize normalize))
         (optics-output (make-instance 'optics-output))
	 (nns-class (case nns-type
		      (:naive 'naive-nearest-search)
		      (:kd-tree 'kd-tree-search)
		      (:m-tree  'm-tree-search)
		      (:lsh (ecase distance
			      (:manhattan 'manhattan-locality-sensitive-hashing)
			      (:euclid    'euclid-locality-sensitive-hashing)
			      (:cosine    'cosine-locality-sensitive-hashing)))
		      (otherwise
		       (check-type nns-type symbol)
		       nns-type)))
	 (point-objs (point-objs optics-input)))
    (setf (nn-search optics-input)
      (apply #'make-instance nns-class :input-data point-objs :input-key #'point
	     :distance (distance optics-input)
	     nns-args))
    (optics-main optics-input optics-output)
    optics-output))

(defun make-optics-input-speed (input-data epsilon min-pts r-epsilon 
                          &key (distance :manhattan)
                               (normalize nil))
  (assert (<= r-epsilon epsilon))
  (setq distance
    (case distance
      (:manhattan #'hjs.util.vector:manhattan-distance)
      (:euclid #'hjs.util.vector:euclid-distance)
      (:cosine #'hjs.util.vector:cosine-distance)
      (t (error "illegal name for distance | ~A" distance))))
  (let* ((optics-input (make-instance 'optics-input-speed
                         :input-data input-data :distance distance
                         :epsilon (hjs.util.meta:dfloat epsilon) :min-pts min-pts
                         :r-epsilon (hjs.util.meta:dfloat r-epsilon) :normalize normalize))
         (point-objs
          (coerce 
           (loop for point across input-data
               for id from 0
               collect (make-instance 'optics-point-object
                         :id id :optics-input optics-input))
           'vector)))
    (setf (point-objs optics-input) point-objs)
    optics-input))
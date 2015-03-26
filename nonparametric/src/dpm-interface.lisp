;; package of interfaces for :nonparametric.dpm
(in-package :clml.nonparametric.dpm-interface)
(defgeneric gaussian-dpm (dataset
                         &key sampling
                           estimate-base
                           average-of-average
                           std-of-average
                           average-of-std))
(defmethod gaussian-dpm ((dataset numeric-dataset)
                         &key (sampling 100)
                           (estimate-base nil)
                           average-of-average
                           std-of-average
                           average-of-std)
  (assert (and (numberp sampling) (not (minusp sampling))))
  (let* ((pts (map 'vector (lambda (pt) (make-point :data pt))
                   (dataset-numeric-points dataset)))
         (dim (length (dataset-dimensions dataset)))
         (model (make-instance 'multivar-gauss-dpm :data (copy-seq pts) :dim dim)))
    (when (>= dim #.(floor (log most-positive-double-float (sqrt (* 2 pi)))))
      (error "Dataset dimension is too large: ~D" dim))
    (setf (estimate-base? model) estimate-base)
    (unless estimate-base
      (when (typep average-of-average 'dvec) (setf (average-of-average model) average-of-average))
      (when (typep std-of-average 'dmat) (setf (std-of-average model) std-of-average))
      (when (typep average-of-std 'dmat) (setf (average-of-std model) average-of-std)))
    (loop repeat sampling
       initially (initialize model)
       do (sampling model))
    (values (make-dpm-result model pts
                             (map 'list #'dimension-name (dataset-dimensions dataset)))
            model)))

(defgeneric make-dpm-result (model org-order-pts column-names))
(defmethod make-dpm-result ((model dpm) org-order-pts column-names)
    (let* ((dim (length column-names))
           (cluster-id-alist (cluster-id-alist model))
           (pts (map 'vector (lambda (pt) (cons (cdr (assoc (point-cluster pt)
                                                       cluster-id-alist :test #'eq))
                                           (point-data pt))) org-order-pts))
           (num-data (map 'vector #'cdr pts))
           (cid-data (map 'vector (lambda (pt) `#(,(format nil "~D" (car pt)))) pts))
           (column-names `(,@column-names "ClusterID"))
           (cat-index `(,dim))
           (num-indices (loop for i below dim collect i)))
      (make-numeric-and-category-dataset column-names num-data num-indices cid-data cat-index)))

(defgeneric get-cluster-info (model))
(defmethod get-cluster-info ((model multivar-gauss-dpm))
  (let ((cluster-id-alist (cluster-id-alist model))
        (hash (make-cluster-points-hash model)))
    (loop for (cluster . id) in cluster-id-alist
       collect `(:cluster-id ,(format nil "~D" id)
                             :size ,(cluster-size cluster)
                             :center ,(mean-points (coerce (gethash id hash) 'vector))
                             :std ,(covariance-matrix (coerce (gethash id hash) 'vector))))))

(defgeneric get-cluster-parameter (model))
(defmethod get-cluster-parameter ((model multivar-gauss-dpm))
  (let ((cluster-id-alist (cluster-id-alist model)))
    (loop for (cluster . id) in cluster-id-alist
       collect `(:cluster-id ,(format nil "~D" id)
                             :center ,(cluster-center cluster)
                             :std ,(cluster-std cluster)))))

(defgeneric cluster-id-alist (model))
(defmethod cluster-id-alist ((model dpm))
    (loop with id = 0
       for cluster across (dpm-clusters model)
       as size = (cluster-size cluster)
       unless (zerop size)
       collect (cons cluster (incf id))))

(defgeneric make-cluster-points-hash (model))
(defmethod make-cluster-points-hash ((model dpm))
    (loop with cluster-id-alist = (cluster-id-alist model)
       with hash = (make-hash-table :test #'eql)
       for pt across (dpm-data model)
       as cluster-id = (cdr (assoc (point-cluster pt) cluster-id-alist :test #'eq))
       do (push (point-data pt) (gethash cluster-id hash))
       finally (return hash)))

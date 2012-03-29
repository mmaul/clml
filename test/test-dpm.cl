
(in-package "TEST")

(define-test test-sample-dpm 
    (let ((dataset (pick-and-specialize-data
                    (read-data-from-file "sample/k5-gaussian.sexp")
                    :data-types (make-list 5 :initial-element :numeric))))
      (multiple-value-bind (result model) (gaussian-dpm dataset :estimate-base t)
        (let ((cluster-infos (get-cluster-info model))
              (hash (make-hash-table :test #'equal)))
          (loop for org-pt across (dataset-numeric-points dataset)
              for res-pt across (dataset-numeric-points result)
              for cluster-id-pt across (dataset-category-points result)
              do (assert-a-point-equal org-pt res-pt :test #'=)
                 (push res-pt (gethash (aref cluster-id-pt 0) hash)))
          (loop for info in cluster-infos
              as id = (getf info :cluster-id)
              as size = (getf info :size)
              as center = (getf info :center)
              as std = (getf info :std)
              do (let (real-size real-center real-std)
                   (multiple-value-bind (pts pre-p) (gethash id hash)
                     (when (assert-true pre-p)
                       (setq real-size (length pts)
                             real-center (map 'vector (lambda (val) (/ val real-size))
                                              (apply #'map 'vector #'+ pts))
                             real-std (covariance-matrix (coerce pts 'vector)))
                       (assert-eql size real-size)
                       (assert-a-point-equal center real-center)
                       (when (assert-equal (array-dimensions std) (array-dimensions real-std))
                         (loop with dims = (array-dimensions std)
                             for i below (first dims)
                             do (loop for j below (second dims)
                                    do (assert-equality #'epsilon>
                                                        (aref std i j)
                                                        (aref real-std i j)))))))))
          ))))





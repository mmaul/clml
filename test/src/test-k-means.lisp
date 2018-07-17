
(in-package :clml.test)

(define-test test-sample-k-means
    (let (dataset centroids result)
      (assert
       (setf dataset (read-data-from-file (clml.utility.data:fetch "https://mmaul.github.io/clml.data/sample/pos.sexp") :external-format #+allegro :932 #+sbcl :sjis #+ccl :Windows-31j #+lispworks :sjisj)))
      (assert
       (setf dataset
         (pick-and-specialize-data dataset :range '(2 3) :data-types '(:numeric :numeric))))
      (assert
       (setf result
         (k-means 20 dataset :distance-fn #'manhattan-distance)))
      (assert (setf centroids (get-cluster-centroids result)))
      (assert-true (get-cluster-points result 0))
      (assert-eql (length centroids) 20)
      (loop for cid below 20
          as cluster = (find cid (pw-clusters result) :test #'eql :key #'clml.hjs.k-means::c-id)
          as pts = (get-cluster-points result cid)
          do (assert-eql (c-size cluster) (length pts))
             (loop for pt across pts
                 do (assert-true (find pt (mapcar #'p-pos (c-points cluster)) :test #'point-equal))))))

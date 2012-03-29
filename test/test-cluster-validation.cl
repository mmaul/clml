
(in-package "TEST")

(define-test test-sample-cluster-validation
    (let (*workspace*)
      (assert-true (setf *workspace*
                     (k-means:k-means
                      5
                      (read-data:pick-and-specialize-data
                       (read-data:read-data-from-file
                        "sample/syobu.csv" :type :csv
                        :csv-type-spec '(string integer integer integer integer)
                        :external-format #+allegro :932 #-allegro :sjis)
                       :except '(0) :data-types (make-list 4
                                                           :initial-element :numeric))
                      :random-state
                      (hjs.learn.k-means::make-random-state-with-seed 0))))
      (assert-true (epsilon> (calinski) 441.62300853729414))
      (assert-true (epsilon> (hartigan) 2.5069376578154543))
      (assert-true (epsilon> (ball-and-hall) 1128.3210196476964))
      (assert-true (epsilon> (dunn-index :distance :euclid
                                         :intercluster :hausdorff
                                         :intracluster :centroid)
                             1.218510772099368))
      (assert-true (epsilon> (davies-bouldin-index :distance :euclid
                                                   :intercluster :average
                                                   :intracluster :complete)
                             1.8817487113136586))
      (assert-true (epsilon> (global-silhouette-value :distance :euclid)
                             0.5793276055727323))))
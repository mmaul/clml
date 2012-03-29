(setf dataset
      (pick-and-specialize-data 
       (read-data-from-file "sample/norm-interp-feature.csv"
                            :csv-type-spec (cons 'string (make-list 19 :initial-element 'double-float))
                            :type :csv)
       :data-types (make-list 19 :initial-element :numeric)
       :except '(0)))

(time (k-means 20 dataset
               :distance-fn #'manhattan-distance
               :max-iteration 100
               :random-state (k-means::make-random-state-with-seed 3)
               :standardization t))



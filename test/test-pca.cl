
(in-package "TEST")

(define-test test-sample-pca
    (let (dataset proj-vecs pca-result pca-model for-learn for-estimate)
      (assert-true (setf dataset 
                     (read-data-from-file "sample/pos.sexp" 
                                          :external-format #+allegro :932 #-allegro :sjis)))
      (assert-true (setf dataset (pick-and-specialize-data
                                  dataset :range '(2 3) :data-types '(:numeric :numeric))))
      (assert-true (multiple-value-setq (pca-result pca-model)
                     (princomp dataset :method :correlation)))
      ;; check pca-result pca-model
      (assert-points-equal
       #(#(-0.18646787691278618d0 -0.5587877417431286d0) #(-0.2586922124306382d0 -0.6310120772609806d0)
         #(0.08929776779173992d0 -0.2830220970386028d0) #(-0.311219001898167d0 -0.6835388667285094d0)
         #(-0.19303372559622725d0 -0.5653535904265697d0) #(-0.19303372559622725d0 -0.5653535904265697d0)
         #(-0.19303372559622725d0 -0.5653535904265697d0) #(-1.9046466459275095d0 1.014942356235892d0)
         #(0.20748304409367965d0 -0.1648368207366632d0) #(0.161522103309592d0 -0.21079776152075083d0))
       (hjs.learn.pca::components pca-result)
       :test (lambda (v1 v2) (epsilon> (abs v1) (abs v2))))
      (assert-a-point-equal #(1.0766620035641237d0 0.9233379964358763d0)
                            (hjs.learn.pca::contributions pca-result)
                            :test (lambda (v1 v2) (epsilon> (abs v1) (abs v2))))
      (assert-points-equal #(#(-0.7071067811865472d0 0.7071067811865478d0)
                             #(0.7071067811865478d0 0.7071067811865472d0))
                           (hjs.learn.pca::loading-factors pca-result)
                           :test (lambda (v1 v2) (epsilon> (abs v1) (abs v2))))
      (assert-points-equal #(#(-0.7071067811865472d0 0.7071067811865478d0)
                             #(0.7071067811865478d0 0.7071067811865472d0))
                           (hjs.learn.pca::loading-factors pca-model)
                           :test (lambda (v1 v2) (epsilon> (abs v1) (abs v2))))
      (assert-a-point-equal #(1.2262030207235688d0 185.75242109488684d0)
                            (hjs.learn.pca::centroid pca-result))
      (assert-a-point-equal #(1.2262030207235688d0 185.75242109488684d0)
                            (hjs.learn.pca::centroid pca-model))
      (assert-eq :correlation (hjs.learn.pca::pca-method pca-result))
      (assert-eq :correlation (hjs.learn.pca::pca-method pca-model))
      
      (assert-true (setf proj-vecs (princomp-projection dataset pca-model)))
      (assert-points-equal (hjs.learn.pca::components pca-result) proj-vecs :test #'=)
      
      (let ((eyes (pick-and-specialize-data
                   (read-data-from-file "sample/eyes200.sexp")
                   :except '(0)
                   :data-types (append (make-list 1 :initial-element :category)
                                       (make-list 1680 :initial-element :numeric)))))
        ;; result check with no random
        (multiple-value-setq (for-learn for-estimate)
          (divide-dataset eyes :divide-ratio '(1 1) :range (loop for i from 0 to 51 collect i)))
        (multiple-value-setq (pca-result pca-model)
          (princomp (divide-dataset for-learn :except '(0)) :method :covariance))
        (loop with estimator = (make-face-estimator for-learn :dimension-thld 5 :method :eigenface
                                                    :pca-result pca-result :pca-model pca-model)
            for vec across (dataset-numeric-points for-estimate)
            for expected in 
              '("arthur" "arthur" "arthur" "arthur" "arthur" "arthur" "arthur" "arthur" "arthur" "arthur" 
                "arthur" "arthur" "arthur" "arthur" "arthur" "arthur" "arthur" "arthur" "arthur" "arthur"
                "arthur" "arthur" "arthur" "arthur" "arthur" "arthur" "arthur" "arthur" "arthur" "arthur"
                "arthur" "arthur" "arthur" "arthur" "arthur" "arthur" "arthur" "arthur" "arthur" "arthur"
                "arthur" "arthur" "arthur" "arthur" "arthur" "arthur" "scott" "max" "arthur" "arthur" 
                "arthur" "arthur" "max" "kevin" "arthur" "arthur" "arthur" "arthur" "arthur" "arthur" 
                "arthur" "arthur" "arthur" "arthur" "arthur" "arthur" "arthur" "arthur" "arthur" "arthur"
                "igor" "kevin" "kevin" "kevin" "kevin" "kevin" "kevin" "kevin" "kevin" "kevin" "arthur" 
                "arthur" "arthur" "arthur" "arthur" "arthur" "arthur" "arthur" "arthur" "arthur" "arthur" 
                "arthur" "arthur" "arthur" "arthur" "arthur" "arthur" "arthur" "arthur" "arthur")
            as ested = (funcall estimator vec)
            do (assert-equality #'string= expected ested))
        
        ;; with random
        (multiple-value-setq (for-learn for-estimate)
          (divide-dataset eyes :divide-ratio '(1 1) :random t)))
      (multiple-value-setq (pca-result pca-model)
        (princomp (divide-dataset for-learn :except '(0)) :method :covariance))

      (flet ((check-estimate-result (result estimator)
               (loop for pts across (dataset-numeric-points result)
                   for res across (dataset-category-points result)
                   as expected = (funcall estimator pts)
                   do (assert-equality #'string= expected (aref res 0)))))
        (loop for dimension in '(1 5 10 20 30)
            as estimator = (make-face-estimator for-learn :dimension-thld dimension :method :eigenface
                                                :pca-result pca-result :pca-model pca-model)
            as result = (face-estimate for-estimate estimator)
            do (check-estimate-result result estimator)
               (format t "hitting-ratio: ~,3F~%"
                       (/ (count-if (lambda (p) (string-equal (aref p 0) (aref p 1)))
                                    (dataset-category-points result))
                          (length (dataset-points result))))))
      ))
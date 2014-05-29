
(in-package "TEST")

(defparameter *sample* '(10 194 8.6d0 69 5 10 "" nil "NA" 3 4.5d0 6 5 5 5))
(defparameter *sample-1* `(10d0 3d0 194d0 8.6d0 69d0 5d0 10d0 3d0 3d0 3d0
                                ,*nan* 3d0 4.5d0 6d0 5d0 5d0 5d0))
(defparameter *sample-2* `(10 3 194 69 5 10 3 3 3 ,*c-nan* 3 6 5 5 5))

(define-test test-ps
    (progn
      (assert-equality #'=
                       (length *missing-values*)
                       (loop for val in *missing-values* count (missing-value-p val)))
      (assert-true (na-p *na*))
      (assert-true (na-p *nan* :type :numeric))
      (assert-true (na-p *c-nan* :type :category))
      (assert-true (na-p "my-na" :na-string "my-na"))
      (assert-false (na-p "not-my-na" :na-string "my-na"))
      (assert-false (nan-p *na*))
      (assert-false (c-nan-p *na*))
      (assert-false (nan-p *c-nan*))
      (assert-false (c-nan-p *nan*))))

(define-test test-fill-rem
    (progn
      (assert-true (tree-equal `(10 194 8.6d0 69 5 10 ,*na* ,*na* ,*na* 3 4.5d0 6 5 5 5)
                               (fill-na *sample*) :test #'equalp))
      (assert-equalp (length *missing-values*)
                     (loop for val in (handling-missing-value::na2nan (fill-na *sample*))
                         count (nan-p val)))
      (assert-equalp (length *missing-values*)
                     (loop for val in (handling-missing-value::na2c-nan (fill-na *sample*))
                         count (c-nan-p val)))
      (assert-true (tree-equal `(10 194 8.6d0 69 5 10 3 4.5d0 6 5 5 5)
                               (handling-missing-value::remove-na-nan (fill-na *sample*))
                               :test #'equalp))
      (assert-true (tree-equal `(10 194 8.6d0 69 5 10 ,*na* ,*na* ,*na* 3 4.5d0 6 5 5 5)
                               (handling-missing-value::remove-na-nan (fill-na *sample*) :remove-na nil)
                               :test #'equalp))
      (assert-true (loop for val in (handling-missing-value::remove-na-nan 
                                     (handling-missing-value::na2nan (fill-na *sample*)) 
                                     :seq-type :numeric)
                       always (numberp val)))
      (assert-true (loop for val in (handling-missing-value::remove-na-nan
                                     (handling-missing-value::na2c-nan (fill-na *sample*))
                                     :seq-type :category)
                       always (and (numberp val) (not (c-nan-p val)))))))

(define-test test-interp-outlier
    (let ((numeric-seq
           (map 'dvec (lambda (v) (dfloat v)) (handling-missing-value::na2nan (fill-na *sample*)))))
      (assert-true (tree-equal '(10.0d0 194.0d0 8.6d0 69.0d0 5.0d0 10.0d0 3.0d0
                                 3.0d0 3.0d0 3.0d0 4.5d0 6.0d0 5.0d0 5.0d0 5.0d0)
                               (coerce
                                (handling-missing-value::interpolate numeric-seq
                                                                     :interp :min
                                                                     :seq-type :numeric)
                                'list)
                               :test #'=))
      (assert-true (tree-equal '(10 194 8.6d0 69 5 10 5 5 5 3 4.5d0 6 5 5 5)
                               (handling-missing-value::interpolate
                                (handling-missing-value::na2c-nan (fill-na *sample*))
                                :interp :mode
                                :seq-type :category)
                               :test #'=))
      (assert-true (loop for val1 in '(10d0 194d0 8.6d0 69d0 5d0 10d0 26.28360779871214d0
                                       22.92285714285714d0 11.350677915573574d0 3d0 4.5d0
                                       6d0 5d0 5d0 5d0)
                       for val2 across (handling-missing-value::interpolate numeric-seq
                                                                            :interp :spline 
                                                                            :seq-type :numeric)
                       always (> *epsilon* (abs (- val1 val2)))))
      
      (assert-true (loop for val1 in `(10d0 3d0 ,*nan* 8.6d0 ,*nan* 5d0 10d0 3d0 3d0 3d0
                                            ,*nan* 3d0 4.5d0 6d0 5d0 5d0 5d0)
                       for val2 in (outlier-verification *sample-1* :seq-type :numeric)
                       always (or (= val1 val2) (and (nan-p val1) (nan-p val2)))))
      (assert-true (loop for val1 in `(10d0 3d0 ,*nan* 8.6d0 ,*nan* 5d0 10d0 3d0 3d0 3d0
                                            ,*nan* 3d0 4.5d0 6d0 5d0 5d0 5d0)
                       for val2 in (outlier-verification *sample-1* 
                                                         :type :std-dev
                                                         :outlier-value 0.9
                                                         :seq-type :numeric)
                       always (or (= val1 val2)
                                  (and (nan-p val1) (nan-p val2)))))
      (assert-true (loop for val1 in `(10 3 ,*c-nan* ,*c-nan* 5 10 3 3 3 ,*c-nan* 3 ,*c-nan* 5 5 5)
                       for val2 in (outlier-verification *sample-2* 
                                                         :type :freq
                                                         :outlier-value 0.075
                                                         :seq-type :category)
                       always (or (= val1 val2)
                                  (and (nan-p val1) (nan-p val2)))))))

(define-test test-spline
    (prog* ((xa (loop for i below 10 
                    collect (* pi (coerce (+ 0.05 (* 0.1 i)) 'double-float))))
            (ya (mapcar #'sin xa))
            (yp1 (cos (first xa)))
            (ypn (cos (car (last xa))))
            (xa-dvec (make-array (length xa) :initial-contents xa :element-type 'double-float))
            (ya-dvec (make-array (length ya) :initial-contents ya :element-type 'double-float))
            (y2 (handling-missing-value::spline xa-dvec ya-dvec yp1 ypn))
            (target-pos 6)
            (target (nth target-pos ya))
            (epsilon 1d-4))
      (assert-true (> *epsilon*
                      (abs (- (sin (* 0.05d0 pi))
                              (handling-missing-value::splint xa-dvec ya-dvec y2 (* 0.05d0 pi))))))
      (assert-true (> epsilon
                      (abs (- (sin (* 0.5d0 pi))
                              (handling-missing-value::splint xa-dvec ya-dvec y2 (* 0.5d0 pi))))))
      (setf (aref ya-dvec target-pos) *nan*)
      (assert-true (> (+ 7.285997538017952e-4 *epsilon*)
                      (abs (- target
                              (aref (handling-missing-value::3dim-spline-interp
                                     ya-dvec :x-dvec xa-dvec) 
                                    target-pos)))))
      (assert-true (loop for val1 across (handling-missing-value::3dim-spline-interp
                                          ya-dvec :x-dvec xa-dvec)
                       for val2 across (handling-missing-value::3dim-spline-interp ya-dvec)
                       always (> epsilon (abs (- val1 val2)))))))
#+ignore
(run-tests test-ps test-fill-rem test-interp-outlier test-spline)

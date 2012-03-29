
(in-package "TEST")

(define-test test-sample-stat
    (let ((height '(148 160 159 153 151 140 156 137 149 160 151 157 157 144))
          (weight '(41 49 45 43 42 29 49 31 47 47 42 39 48 36))
          (baseball-teams '((3 2 1 5 4 6) (2 6 3 5 1 4)))
          (normal-random (rand-n (standard-normal-distribution) 1000))
          (chi-random (rand-n (chi-square-distribution 10) 1000))
          (*sample* 
           '(133 134 134 134 135 135 139 140 140 140 141 142 142 144 144 147 147 149 150 164)))
      (assert-equality #'= 1061/7 (mean height))
      (assert-equality #'epsilon> 151.57142857142858d0 (+ (mean height) 0.0d0))
      (assert-eql 152 (median height))
      (assert-true 
       (set-equal '(137 297/2 152 157 160) (five-number-summary height) :test #'=))
      (assert-true 
       (set-equal '(137 297/2 152 157 160) 
                  (mapcar (lambda (x) (discrete-quantile height x)) '(0 1/4 1/2 3/4 1))
                  :test #'=))
      (assert-equality #'= 17/2 (interquartile-range height))
      (assert-equality #'epsilon> 5.857142857142857d0 (+ (mean-deviation height) 0.0d0))
      (assert-equality #'epsilon> 50.10204081632653d0 (+ (variance height) 0.0d0))
      (assert-equality #'epsilon> 7.345477789500419d0 (standard-deviation height))
      (assert-true (set-equal '(-70.15050916496945d0 0.7399185336048879d0)
                              (linear-regression height weight) :test #'epsilon>))
      (assert-equality #'epsilon> 39.92307692307692d0 (+ (covariance height weight) 0.0d0))
      (assert-equality #'epsilon> 0.851211920646571d0 (correlation-coefficient height weight))
      (assert-equality #'epsilon> 0.02857142873108387d0 
                       (+ (apply #'spearman-rank-correlation baseball-teams) 0.0d0))
      (assert-equality #'epsilon> -0.06666666666666667d0
                       (+ (apply #'kendall-rank-correlation baseball-teams) 0.0d0))
      (assert-equality #'epsilon> -1.9599639551896222d0
                       (quantile (standard-normal-distribution) 0.025d0))
      (assert-true (zerop (density (standard-uniform-distribution) 1.5d0)))
      (assert-equality #'epsilon> 0.3d0 (cdf (standard-uniform-distribution) 0.3d0))
      (assert-true (five-number-summary normal-random))
      (assert-true (mean normal-random))
      (assert-true (standard-deviation normal-random))
      (assert-equality #'epsilon> -2.0150483733330242d0 (quantile (t-distribution 5) 0.05d0))
      (assert-equality #'epsilon> 0.23036198922913856d0 (density (t-distribution 10) 1.0d0))
      (assert-true (mean chi-random))
      (assert-true (standard-deviation chi-random))
      
      (assert-true (normal-dist-test '(4 19 86 177 105 33 2) 40 5 0.1))
      (assert-true (poisson-dist-test '(27 61 77 71 54 35 20 11 6 2 1)))
      (assert-true (binom-dist-test '(2 14 20 34 22 8) '(0 1 2 3 4 5) 5))
      
      (assert-prints (format nil "Data: ~A = 164.000~%t= 3.005, p-value = 2.557, df = 18" :max)
                     (smirnov-grubbs *sample* 0.05 :type :max))
      (assert-prints (format nil "Data: ~A = 133.000~%t= 1.172, p-value = 2.557, df = 18" :min)
                     (smirnov-grubbs *sample* 0.05 :type :min))
      (assert-true 
       (set-equal 
        '(133 134 134 134 135 135 139 140 140 140 141 142 142 144 144 147 147 149 150)
        (smirnov-grubbs *sample* 0.05 :type :max :recursive t) :test #'eql))
      (assert-true 
       (set-equal
        '(164)
        (set-difference *sample* (smirnov-grubbs *sample* 0.05 :type :max :recursive t))
        :test #'eql))))
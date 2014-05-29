
(in-package "TEST")

#|cf. expected results by R

lm(formula = dist ~ speed, data = cars)

Residuals:
    Min      1Q  Median      3Q     Max 
-29.069  -9.525  -2.272   9.215  43.201 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) -17.5791     6.7584  -2.601   0.0123 *  
speed         3.9324     0.4155   9.464 1.49e-12 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

Residual standard error: 15.38 on 48 degrees of freedom
Multiple R-squared: 0.6511,	Adjusted R-squared: 0.6438 
F-statistic: 89.57 on 1 and 48 DF,  p-value: 1.490e-12 
|#

(define-test test-sample-linear-regression
    (let (dataset airquality)
      (assert-true
       (setf dataset 
         (read-data-from-file 
          "sample/airquality.csv"
          :type :csv
          :csv-type-spec 
          '(integer double-float double-float double-float double-float integer integer))))
      (assert-true
       (setf airquality
         (pick-and-specialize-data dataset :range '(0 1 2 3 4) 
                                   :data-types '(:numeric :numeric :numeric :numeric :numeric))))
      (assert-true (mlr airquality '(2 3 4 1)))))

(define-test simple-linear-regression-case
    (let* ((cars (read-data-from-file "sample/cars.csv" :type :csv :csv-type-spec '(double-float double-float)))
	   (cars (pick-and-specialize-data cars :range '(0 1) :data-types '(:numeric :numeric)))
	   (epsilon 0.001))
      (assert-true (< (abs (- (aref (mlr cars '(0 1)) 0) -17.579))
		      epsilon))
      (assert-true (< (abs (- (aref (mlr cars '(0 1)) 1) 3.9324))
		      epsilon))
      (assert-true (< (abs (- (aref (std-err-vector cars '(0 1)) 0) 6.7584))
		      epsilon))
      (assert-true (< (abs (- (aref (std-err-vector cars '(0 1)) 1) 0.4155))
		      epsilon))
      (assert-true (< (abs (- (aref (t-value-vector cars '(0 1)) 0) -2.601))
		      epsilon))
      (assert-true (< (abs (- (aref (t-value-vector cars '(0 1)) 1) 9.464))
		      epsilon))
      (assert-true (< (abs (- (R^2 cars '(0 1)) 0.6511))
		      epsilon))
      (assert-true (< (abs (- (adjusted-R^2 cars '(0 1)) 0.6438))
		      epsilon))))

#|cf. expected results by R
lm(formula = Ozone ~ Solar.R + Wind + Temp, data = airquality)

Residuals:
    Min      1Q  Median      3Q     Max 
-40.485 -14.219  -3.551  10.097  95.619 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) -64.34208   23.05472  -2.791  0.00623 ** 
Solar.R       0.05982    0.02319   2.580  0.01124 *  
Wind         -3.33359    0.65441  -5.094 1.52e-06 ***
Temp          1.65209    0.25353   6.516 2.42e-09 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

Residual standard error: 21.18 on 107 degrees of freedom
  (42 observations deleted due to missingness)
Multiple R-squared: 0.6059,	Adjusted R-squared: 0.5948 
F-statistic: 54.83 on 3 and 107 DF,  p-value: < 2.2e-16 
|#

(define-test multi-linear-regression-case
    (let* ((air (read-data-from-file "sample/airquality.csv"
				     :type :csv
				     :csv-type-spec '(integer double-float double-float double-float double-float integer integer)))
	   (air (pick-and-specialize-data air :range '(0 1 2 3 4) 
					  :data-types '(:numeric :numeric :numeric :numeric :numeric)))
	   (epsilon 0.001))
      (assert-true (< (abs (- (aref (mlr air '(2 3 4 1)) 0) -64.34208))
		      epsilon))
      (assert-true (< (abs (- (aref (mlr air '(2 3 4 1)) 1) 0.05982))
		      epsilon))
      (assert-true (< (abs (- (aref (mlr air '(2 3 4 1)) 2) -3.33359))
		      epsilon))
      (assert-true (< (abs (- (aref (mlr air '(2 3 4 1)) 3) 1.65209))
		      epsilon))
      (assert-true (< (abs (- (aref (std-err-vector air '(2 3 4 1)) 0) 23.05472))
		      epsilon))
      (assert-true (< (abs (- (aref (std-err-vector air '(2 3 4 1)) 1) 0.02319))
		      epsilon))
      (assert-true (< (abs (- (aref (std-err-vector air '(2 3 4 1)) 2) 0.65441))
		      epsilon))
      (assert-true (< (abs (- (aref (std-err-vector air '(2 3 4 1)) 3) 0.25353))
		      epsilon))
      (assert-true (< (abs (- (aref (t-value-vector air '(2 3 4 1)) 0) -2.791))
		      epsilon))
      (assert-true (< (abs (- (aref (t-value-vector air '(2 3 4 1)) 1) 2.580))
		      epsilon))
      (assert-true (< (abs (- (aref (t-value-vector air '(2 3 4 1)) 2) -5.094))
		      epsilon))
      (assert-true (< (abs (- (aref (t-value-vector air '(2 3 4 1)) 3) 6.516))
		      epsilon))
      (assert-true (< (abs (- (R^2 air '(2 3 4 1)) 0.6059))
		      epsilon))
      (assert-true (< (abs (- (adjusted-R^2 air '(2 3 4 1)) 0.5948))
		      epsilon))))

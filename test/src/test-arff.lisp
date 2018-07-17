
(in-package clml.test)

(define-test parse-arff
  (let* ((raw-data "% 1. Title: Iris Plants Database
%
% 2. Sources:
%      (a) Creator: R.A. Fisher
%      (b) Donor: Michael Marshall (MARSHALL%PLU@io.arc.nasa.gov)
  %    (c) Date: July, 1988

@RELATION iris

@ATTRIBUTE sepallength	numeric
@ATTRIBUTE sepalwidth 	REAL
@ATTRIBUTE petallength	reaL
@ATTRIBUTE petalwidth   Real
@ATTRIBUTE class	 {Iris-setosa,Iris-versicolor,Iris-virginica}

@DATA
4.8,3.0,1.4,0.3,Iris-setosa
5.1,3.8,1.6,0.2,Iris-setosa
4.6,3.2,1.4,0.2,Iris-setosa
5.3,3.7,1.5,0.2,Iris-setosa
5.0,3.3,1.4,0.2,Iris-setosa
5.7,3.0,4.2,1.2,Iris-versicolor
5.7,2.9,4.2,1.3,Iris-versicolor
6.2,2.9,4.3,1.3,Iris-versicolor
5.1,2.5,3.0,1.1,Iris-versicolor
5.7,2.8,4.1,1.3,Iris-versicolor
6.7,3.0,5.2,2.3,Iris-virginica
6.3,2.5,5.0,1.9,Iris-virginica
6.5,3.0,5.2,2.0,Iris-virginica
6.2,3.4,5.4,2.3,Iris-virginica
5.9,3.0,5.1,1.8,Iris-virginica
%")
         (parsed-data (read-arff-stream (make-string-input-stream raw-data))))
  (assert-equal 15 (length parsed-data))
  (loop for row across parsed-data
    do (assert-equal 5 (length row))
    do (assert-true (floatp (aref row 0)))
    do (assert-true (floatp (aref row 1)))
    do (assert-true (floatp (aref row 2)))
    do (assert-true (floatp (aref row 3)))
    do (assert-true (stringp (aref row 4))))))

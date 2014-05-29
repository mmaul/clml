
(in-package "TEST")

(define-test test-sample-svm
    (progn
      (defparameter *positive-set*
          '((8.0 8.0) (8.0 20.0) (8.0 44.0) (8.0 56.0) (12.0 32.0) (16.0 16.0) (16.0 48.0)
            (24.0 20.0) (24.0 32.0) (24.0 44.0) (28.0 8.0) (32.0 52.0) (36.0 16.0)))
      (defparameter *negative-set*
          '((36.0 24.0) (36.0 36.0) (44.0 8.0) (44.0 44.0) (44.0 56.0)
            (48.0 16.0) (48.0 28.0) (56.0 8.0) (56.0 44.0) (56.0 52.0)))
      (assert-true (setf linear-fcn
                     (svm +linear-kernel+ *positive-set* *negative-set*)))
      (assert-true (multiple-value-list (funcall linear-fcn (car (last *positive-set*)))))
      (assert-true (setf polynomial-fcn
                     (svm (polynomial-kernel 3 nil) *positive-set* *negative-set*)))
      (assert-true (funcall polynomial-fcn (car (last *positive-set*))))
      (assert-true (funcall polynomial-fcn '(30.0 20.0)))
      ))
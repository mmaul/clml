
(in-package "TEST")

#|
TEST-SPECTRAL-CLUSTERING(28): (run-tests)
RANDOM-W: 500 assertions passed, 0 failed.
SAMPLE-W: 7 assertions passed, 0 failed.
TOTAL: 507 assertions passed, 0 failed, 0 execution errors.
|#

(define-test test-sample-spectral-clustering
    (progn
      (assert-true *sample-w*)
      (assert-true (spectral-clustering-mcut *sample-w* 1))
      (assert-true (spectral-clustering-mcut *sample-w* 2))
      (assert-true (spectral-clustering-mcut *sample-w* 5))
      ))

(define-test sample-w
    (let ((*sample-w* spectral-clustering::*sample-w*))
      (assert-error 'error (spectral-clustering-mcut *sample-w* 0))
      (assert-equality #'set-equal 
                       '((0 1 2 3 4))
                       (spectral-clustering-mcut *sample-w* 1))
      (assert-equality #'set-equal 
                       '((2 3 4) (0 1))
                       (spectral-clustering-mcut *sample-w* 2))
      (assert-equality #'set-equal 
                       '((2 3) (0 1) (4))
                       (spectral-clustering-mcut *sample-w* 3))
      (assert-equality #'set-equal 
                       '((0 1) (3) (2) (4))
                       (spectral-clustering-mcut *sample-w* 4))
      (assert-equality #'set-equal 
                       '((1) (0) (3) (2) (4))
                       (spectral-clustering-mcut *sample-w* 5))
      (assert-false (spectral-clustering-mcut *sample-w* 6))
      ))

(define-test random-w
    (loop repeat 50
        as w = (spectral-clustering::make-random-symmetric-matrix
                100 0 most-positive-single-float)
        do (loop for c-num from 1 to 10
               do (assert-equality 
                   #'=
                   c-num
                   (length (spectral-clustering-mcut w c-num))))))


;; TODO make (define-test eigen) by using this fcn.
#+ignore
(defun test-eigen (ndim &key (method 'eigen-by-householder-ql)
                             (verify t))
  (let ((a (make-random-symmetric-matrix ndim 0 100))
        (time (get-internal-run-time)))
    (multiple-value-bind (vals vecs)
        (funcall method a)
      (setq time (/ (float (- (get-internal-run-time) time))
                    internal-time-units-per-second))
      (cond
       (verify
        (let ((norm (mat-norm (mat- (mat* a vecs)
                                    (mat* vecs (make-diag-mat vals))))))
          (when (< 0.0001 (abs norm))
            (format t "*** WARNING: error value seems too big !~%"))
          (format t "dim=~d  runtime=~,3f  error=~,4f  method=~a~%"
                  ndim time norm
                  (subseq (string-downcase (symbol-name method)) 9))))
       (t
        (format t "dim=~d  runtime=~,3f  :verify=nil  method=~a~%"
                ndim time
                (subseq (string-downcase (symbol-name method)) 9))))))
  (values))

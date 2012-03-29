(in-package :future-test)

(defun fib (x)
  (if (<= x 1)
      1
      (+ (fib (- x 1))
         (fib (- x 2)))))

(defun benchmark-parallel (x times)
  (kill-all-futures)
  (let (futures)
    (loop repeat times
          for f = (future (fib x))
          ;; collect f into fs
          ;; finally (setf futures fs)
          do (push f futures))
    (loop for f in futures
          for r = (touch f)
          collect r)))

(defun benchmark-sequential (x times)
  (loop repeat times
        for r = (fib x)
        collect r))

(defun benchmark-map-reduce (x threads times)
  (kill-all-futures)
  (let ((future-pool (make-array threads)))
    (loop for i below threads
          do
       (setf (aref future-pool i)
             (future::make-future)))
    (loop repeat times
          do
       (wait-for-all-futures
        (loop for i below threads
              collect
           (future-funcall 'fib (list x) (aref future-pool i)))))))


#|
(time (benchmark-map-reduce 22 32 1000))
|#

(in-package :future-test)

(in-suite root-suite)

(defsuite* stress-test)

(deftest simple-queue-push-pull-test ()
  (with-fixture queue
    (let ((queue (future::make-queue)))
      (future::join-threads
       (loop repeat 10
             collect
          (future::spawn-thread
           (lambda ()
             (loop repeat 1000
                   do (push-pull queue 0.01))))))
      (is (= 0 (future::queue-length *empty-queue*)))
      (is (future::queue-empty-p *empty-queue*))
      ;;
      (future::join-threads
       (loop repeat 10
             collect
          (future::spawn-thread
           (lambda ()
             (loop repeat 100
                   do (push-pull queue 0.1))))))
      (is (= 0 (future::queue-length *empty-queue*)))
      (is (future::queue-empty-p *empty-queue*))
      ;;
      (future::join-threads
       (loop repeat 100
             collect
          (future::spawn-thread
           (lambda ()
             (loop repeat 100000
                   do (push-pull queue nil))))))
      (is (= 0 (future::queue-length *empty-queue*)))
      (is (future::queue-empty-p *empty-queue*)))))


(deftest thread-pool-reset-test ()
  (with-fixture thread-pool
    (loop repeat 1000
          do
       (progn
         (loop repeat 10
               do
            (future::assign-task (future::make-future :function (lambda () (sleep 1))) *thread-pool*))
         (sleep 0.05)
         (is (future::thread-pool-full-p *thread-pool*))
         (future::reset-thread-pool *thread-pool*)
         (is (future::thread-pool-empty-p *thread-pool*))))))


(deftest 100p-100times ()
  (with-fixture thread-pool
    (loop repeat 100
          do
       (progn
         (kill-all-futures)
         (assert-no-futures) 
         (let ((futures (loop repeat 100
                              collect
                           (future (sleep 0.01) (+ 1 1)))))
           (is (= 100 (futures-count)))
           (is (= 200 (reduce '+ futures :key 'touch))))
         (assert-no-futures)))))

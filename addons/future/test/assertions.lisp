(in-package :future-test)

(defun running-futures-count ()
  (is (= (length (future::thread-pool-threads future::*thread-pool*))
         (future::thread-pool-thread-count future::*thread-pool*)))
  (- (future::thread-pool-thread-count future::*thread-pool*)
     (future::thread-pool-idle-thread-count future::*thread-pool*)))

(defun pending-futures-count ()
  (future::queue-length (future::thread-pool-task-queue future::*thread-pool*)))

(defun futures-count ()
  (+ (running-futures-count) (pending-futures-count)))

(defun assert-no-running-futures ()
  (is (= 0 (running-futures-count))))

(defun assert-no-pending-futures ()
  (is (= 0 (pending-futures-count))))

(defun assert-no-futures ()
  (assert-no-running-futures)
  (assert-no-pending-futures))


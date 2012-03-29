(in-package :future-test)

(in-suite root-suite)

(defsuite* unit-test)

;;; queue
(defvar *empty-queue*)
(defvar *sample-queue*)

(defixture queue
  (setf *empty-queue* (future::make-queue))
  (setf *sample-queue* (future::make-queue :initial-content #(1 2 3))))

(defun push-pull (queue pause-time)
  (future::enqueue 1 queue)
  (future::enqueue 2 queue)
  (future::enqueue 3 queue)
  (when pause-time
    (sleep (random pause-time)))
  (future::dequeue queue)
  (future::dequeue queue)
  (future::dequeue queue))

(deftest simple-queue-tests ()
  (with-fixture queue
    (is (= 0 (future::queue-length *empty-queue*)))
    (is (= 3 (future::queue-length *sample-queue*)))
    (is (future::queue-empty-p *empty-queue*))
    (is (not (future::queue-empty-p *sample-queue*)))
    ;;
    (future::enqueue 1 *empty-queue*)
    (is (= 1 (future::queue-length *empty-queue*)))
    (is (not (future::queue-empty-p *empty-queue*)))
    ;;
    (future::queue-empty! *empty-queue*)
    (is (= 0 (future::queue-length *empty-queue*)))
    (is (future::queue-empty-p *empty-queue*))
    ;;
    (push-pull *empty-queue* 0.01)
    (is (= 0 (future::queue-length *empty-queue*)))
    (is (future::queue-empty-p *empty-queue*))
    ;;
    (future::queue-delete-item 2 *sample-queue*)
    (is (= 2 (future::queue-length *sample-queue*)))
    (is (= 1 (future::dequeue *sample-queue*)))
    (is (= 3 (future::dequeue *sample-queue*)))
    (is (= 0 (future::queue-length *sample-queue*)))
    (is (future::queue-empty-p *sample-queue*))
    ;;
    (future::with-mutex ((future::queue-mutex *empty-queue*))
      (future::wait-for-new-items *empty-queue* :timeout 0.1)
      (is (eq nil (future::dequeue *empty-queue*))))
    ;;
    (future::join-threads
     (list
      (future::spawn-thread
       (lambda ()
         (future::with-mutex ((future::queue-mutex *empty-queue*))
           (future::wait-for-new-items *empty-queue* :timeout 1)
           (is (= 1 (future::dequeue *empty-queue*))))))
      (future::spawn-thread
       (lambda ()
         (future::with-mutex ((future::queue-mutex *empty-queue*))
           (future::enqueue 1 *empty-queue*)
           (future::queue-notify *empty-queue*))))))
    (is (= 0 (future::queue-length *empty-queue*)))
    (is (future::queue-empty-p *empty-queue*))
    ))


;;;
(defvar *thread-pool*)

(defixture thread-pool
  (setf *thread-pool* (future::make-thread-pool :limit 3 :keep-alive-seconds 1)))

(deftest thread-pool-test ()
  (with-fixture thread-pool
    (is (future::thread-pool-empty-p *thread-pool*))
    (is (= 0 (future::thread-pool-idle-thread-count *thread-pool*)))
    (is (= 0 (length (future::thread-pool-threads *thread-pool*))))
    (is (future::queue-empty-p (future::thread-pool-task-queue *thread-pool*)))
    (is (not (future::thread-pool-full-p *thread-pool*)))
    ;;
    (future::assign-task (future::make-task :function (lambda () (sleep 1))) *thread-pool*)
    (is (not (future::thread-pool-empty-p *thread-pool*)))
    (is (= 0 (future::thread-pool-idle-thread-count *thread-pool*)))
    (is (= 1 (length (future::thread-pool-threads *thread-pool*))))
    (is (future::queue-empty-p (future::thread-pool-task-queue *thread-pool*)))
    (is (not (future::thread-pool-full-p *thread-pool*)))
    (future::assign-task (future::make-task :function (lambda () (sleep 1))) *thread-pool*)
    (future::assign-task (future::make-task :function (lambda () (sleep 1))) *thread-pool*)
    (is (future::thread-pool-full-p *thread-pool*))
    (is (= 0 (future::thread-pool-idle-thread-count *thread-pool*)))
    (is (= 3 (length (future::thread-pool-threads *thread-pool*))))
    (is (future::queue-empty-p (future::thread-pool-task-queue *thread-pool*)))
    (future::assign-task (future::make-task :function (lambda () (sleep 1))) *thread-pool*)
    (is (future::thread-pool-full-p *thread-pool*))
    (is (= 0 (future::thread-pool-idle-thread-count *thread-pool*)))
    (is (= 3 (length (future::thread-pool-threads *thread-pool*))))
    (is (not (future::queue-empty-p (future::thread-pool-task-queue *thread-pool*))))
    ;;
    (future::join-threads (future::thread-pool-threads *thread-pool*))
    (is (future::thread-pool-empty-p *thread-pool*))
    (is (= 0 (future::thread-pool-idle-thread-count *thread-pool*)))
    (is (= 0 (length (future::thread-pool-threads *thread-pool*))))
    (is (future::queue-empty-p (future::thread-pool-task-queue *thread-pool*)))
    (is (not (future::thread-pool-full-p *thread-pool*)))
    ;;
    (future::assign-task (future::make-task :function (lambda () (sleep 1))) *thread-pool*)
    (sleep 0.5)
    (future::reset-thread-pool *thread-pool*)
    (is (future::thread-pool-empty-p *thread-pool*))
    (is (= 0 (future::thread-pool-idle-thread-count *thread-pool*)))
    (is (= 0 (length (future::thread-pool-threads *thread-pool*))))
    (is (future::queue-empty-p (future::thread-pool-task-queue *thread-pool*)))
    (is (not (future::thread-pool-full-p *thread-pool*)))
    ;;
    ))


;;; future
(defixture future-default-thread-pool
  (setf (future:future-max-threads) 2)
  (initialize-environment :kill-current-futures-p t))

(deftest 1+1-is-2 ()
  (with-fixture future-default-thread-pool
    (assert-no-futures)
    (is (= 2 (touch (future (+ 1 1)))))
    (is (= 2 (touch (future (print 'hello) (+ 1 1)))))
    (assert-no-futures)))

#+nil
(deftest error-test ()
  (with-fixture future-default-thread-pool
    (assert-no-futures)
    (signals error (touch (future (error "error test"))))
    (assert-no-futures)))

(deftest future-test ()
  (with-fixture future-default-thread-pool
    (assert-no-futures)
    (let ((f1 (future (sleep 0.1) (+ 1 1)))
          (f2 (future (sleep 0.2) (+ 1 1))))
      (is (= 2 (futures-count)))
      (is (= 4 (+ (touch f1) (touch f2)))))
    (assert-no-futures)))

(deftest wait-for-future-test ()
  (with-fixture future-default-thread-pool
    (assert-no-futures)
    (let ((f1 (future (sleep 0.1) (+ 1 1)))
          (f2 (future (sleep 0.2) (+ 1 1))))
      (is (= 2 (running-futures-count)))
      (is (= 0 (pending-futures-count)))
      (wait-for-future f1)
      (is (= 1 (running-futures-count)))
      (is (= 0 (pending-futures-count)))
      (sleep 0.2)
      (assert-no-futures)
      (wait-for-future f2)
      (assert-no-futures)
      (is (future::future-finished-p f1))
      (is (future::future-finished-p f2))
      (is (= 2 (touch f1)))
      (is (= 2 (touch f1))))))

(deftest wait-for-any-future-test ()
  (with-fixture future-default-thread-pool
    (assert-no-futures)
    (let ((f1 (future (sleep 0.2) (+ 2 2)))
          (f2 (future (sleep 0.1) (+ 1 1)))
          (f3 (future (sleep 0.3) (+ 3 3))))
      (is (= 2 (running-futures-count)))
      (is (= 1 (pending-futures-count)))
      (wait-for-any-future)
      (is (= 2 (running-futures-count)))
      (is (= 0 (pending-futures-count)))
      (is (future::future-finished-p f2))
      (wait-for-any-future)
      (is (= 1 (running-futures-count)))
      (is (= 0 (pending-futures-count)))
      (is (future::future-finished-p f1))
      (wait-for-any-future)
      (assert-no-futures)
      (is (= 4 (touch f1)))
      (is (= 2 (touch f2)))
      (is (= 6 (touch f3))))))

(deftest wait-for-all-futures-test ()
  (with-fixture future-default-thread-pool
    (assert-no-futures) 
    (let* ((f1 (future (sleep 0.1) (+ 1 1)))
           (f2 (future (sleep 0.2) (+ 2 2)))
           (f3 (future (sleep 0.3) (+ 3 3)))
           (futures (list f1 f2 f3)))
      (is (= 2 (running-futures-count)))
      (is (= 1 (pending-futures-count)))
      (wait-for-all-futures futures)
      (assert-no-futures)
      (is (every #'future::future-finished-p futures))
      (is (= 12 (+ (touch f1) (touch f2) (touch f3)))))))
 
(deftest kill-future-test ()
  (with-fixture future-default-thread-pool
    (assert-no-futures)
    (let* ((f (future (sleep 1) (+ 1 1))))
      (is (= 1 (running-futures-count)))
      (is (= 0 (pending-futures-count)))
      (sleep 0.1)
      (kill-future f)
      (assert-no-futures)
      (is (not (future::future-finished-p f)))
      (is (future::queue-empty-p (future::thread-pool-finished-tasks future::*thread-pool*))))
    (let* ((f1 (future (sleep 0.3) (+ 1 1)))
           (f1.5 (future (sleep 0.2) (+ 1 1)))
           (f2 (future (sleep 2) (+ 1 1)))
           (f3 (future (sleep 3) (+ 1 1))))
      (is (= 2 (running-futures-count)))
      (is (= 2 (pending-futures-count)))
      (sleep 0.1)
      (kill-future f1.5)
      (is (= 2 (running-futures-count)))
      (is (= 1 (pending-futures-count)))
      (sleep 0.1)
      (kill-future f3)
      (is (= 2 (running-futures-count)))
      (is (= 0 (pending-futures-count)))
      (sleep 0.2)
      (is (= 1 (running-futures-count)))
      (is (= 1 (future::queue-length (future::thread-pool-finished-tasks future::*thread-pool*))))
      (kill-future f1)
      (is (= 1 (running-futures-count)))
      (is (= 0 (future::queue-length (future::thread-pool-finished-tasks future::*thread-pool*))))
      (kill-future f2)
      (assert-no-futures)
      (is (future::future-finished-p f1))
      (is (not (future::future-finished-p f2)))
      (is (not (future::future-finished-p f3))))))

(deftest kill-all-futures-test ()
  (with-fixture future-default-thread-pool
    (assert-no-futures)
    (let* ((f1 (future (sleep 0.1) (+ 1 1)))
           (f2 (future (sleep 0.2) (+ 1 1)))
           (f3 (future (sleep 1) (+ 1 1))))
      (declare (ignorable f1 f2 f3))
      (is (= 2 (running-futures-count)))
      (is (= 1 (pending-futures-count)))
      (is (= 0 (future::queue-length (future::thread-pool-finished-tasks future::*thread-pool*))))
      (sleep 0.3)
      (is (= 2 (future::queue-length (future::thread-pool-finished-tasks future::*thread-pool*))))
      (kill-all-futures)
      (assert-no-futures)
      (is (= 0 (future::queue-length (future::thread-pool-finished-tasks future::*thread-pool*)))))
    (let* ((f1 (future (sleep 0.2) (+ 1 1)))
           (f2 (future (sleep 0.5) (+ 1 1)))
           (f3 (future (sleep 1) (+ 1 1))))
      (declare (ignorable f1 f2 f3))
      (is (= 2 (running-futures-count)))
      (is (= 1 (pending-futures-count)))
      (is (= 0 (future::queue-length (future::thread-pool-finished-tasks future::*thread-pool*))))
      (sleep 0.1)
      (kill-all-futures)
      (assert-no-futures)
      (is (= 0 (future::queue-length (future::thread-pool-finished-tasks future::*thread-pool*)))))))

#+nil
(deftest recursive-future-test ()
  (with-fixture future-default-thread-pool
    (assert-no-futures)
    (let ((f1
           (future
             (+
              (let ((f1 (future (+ 1 1)))
                    (f2 (future (+ 2 2))))
                (is (= 6 (+ (touch f1) (touch f2))))
                (+ (touch f1) (touch f2)))
              (let ((f1 (future (+ 1 1)))
                    (f2 (future (+ 2 2))))
                (is (= 6 (+ (touch f1) (touch f2))))
                (+ (touch f1) (touch f2)))))))
      (is (= 12 (touch f1)))
      (assert-no-futures))))

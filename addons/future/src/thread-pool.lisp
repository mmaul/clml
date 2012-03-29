(in-package :future)

(defstruct task
  function
  args
  (result 'unbound)
  thread)

(defun run-task (task)
  (with-slots (function args result) task
    (setf result
          (if args
              (apply function args)
              (funcall function)))))

(defstruct thread-pool
  (mutex (make-mutex :name "Thread pool mutex"))
  (limit 4)
  (thread-count 0)
  (idle-thread-count 0)
  threads
  (task-queue (make-queue))
  (finished-tasks (make-queue))
  (keep-alive-seconds 30))

(defmacro deftpfun (name args &body body)
  `(defun ,name ,args
     (with-slots (mutex limit thread-count idle-thread-count threads
                        task-queue finished-tasks keep-alive-seconds)
         thread-pool
       (with-mutex (mutex)
         ,@body))))

(deftpfun thread-pool-full-p (thread-pool)
  (>= thread-count limit))

(deftpfun thread-pool-empty-p (thread-pool)
  (= thread-count 0))


(deftpfun make-active-thread (new-task thread-pool &key record-finished-tasks)
  (labels ((self-regulation ()
             (tagbody
              :run-next
                (multiple-value-bind (task availabie-p) (dequeue task-queue)
                  (when availabie-p
                    (setf (task-thread task) (current-thread))
                    (setf (task-result task) (run-task task))
                    (setf (task-thread task) nil)
                    (when record-finished-tasks
                      (enqueue task finished-tasks))
                    (queue-notify finished-tasks)
                    (go :run-next)))
              :wait
                (with-mutex ((queue-mutex task-queue))
                  (incf idle-thread-count)
                  (when (eq :timeout
                            (prog1
                                (wait-for-new-items task-queue :timeout (thread-pool-keep-alive-seconds thread-pool))
                              (decf idle-thread-count)))
                    (go :quit)) 
                  (go :run-next))
              :quit
                ;; NOTE: be sure to check there's no task before quit
                (with-mutex ((queue-mutex task-queue))
                  (when (not (queue-empty-p (thread-pool-task-queue thread-pool)))
                    (go :run-next))
                  (setf threads (delete (current-thread) threads))
                  (decf thread-count)))))
    (assert (not (thread-pool-full-p thread-pool)))
    (let ((thread (spawn-thread (lambda ()
                                  (setf (task-thread new-task) (current-thread))
                                  (setf (task-result new-task) (run-task new-task))
                                  (setf (task-thread new-task) nil)
                                  (when record-finished-tasks
                                    (enqueue new-task finished-tasks))
                                  (queue-notify finished-tasks)
                                  (self-regulation))
                                :name (format nil "Thread #~a" thread-count))))
      (push thread threads)
      (incf thread-count)
      thread)))

(deftpfun assign-task (task thread-pool &key record-finished-tasks)
  (if (not (thread-pool-full-p thread-pool))
      (make-active-thread task thread-pool :record-finished-tasks record-finished-tasks)
      (progn
        (enqueue task task-queue)
        (queue-notify task-queue)))
  thread-pool)

(deftpfun reset-thread-pool (thread-pool)
  (queue-empty! task-queue)
  (mapc #'kill-thread threads)
  (queue-empty! finished-tasks)
  (setf threads '())
  (setf thread-count 0)
  (setf idle-thread-count 0)
  thread-pool)

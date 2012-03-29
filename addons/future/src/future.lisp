(in-package :future)

(defvar *future-max-threads* 4)
(defvar *thread-pool* (make-thread-pool :limit *future-max-threads*))

(defstruct (future (:include task)))

(defun future-max-threads ()
  *future-max-threads*)

(defun (setf future-max-threads) (newval)
  (setf (thread-pool-limit *thread-pool*) newval)
  (setf *future-max-threads* newval))

(defvar *after-finish-hooks* nil)
(defvar *before-start-hooks* nil)

(defun future-finished-p (future)
  (not (eq (future-result future) 'unbound)))

;;; 
(defun initialize-environment (&key kill-current-futures-p)
  (when kill-current-futures-p
    (kill-all-futures))
  (setf *thread-pool* (make-thread-pool :limit *future-max-threads*)))

(defmacro with-new-environment (() &body body)
  `(let (*thread-pool*
         (*future-max-threads* *future-max-threads*)
         *after-finish-hooks*
         *before-start-hooks*)
     (initialize-environment)
     (locally
         ,@body)))

;;;
(defun pop-all-finished-futures ()
  (let ((queue (thread-pool-finished-tasks *thread-pool*)))
    (loop while (nth-value 1 (dequeue queue)))))

(defun wait-for-future (future)
  (let ((queue (thread-pool-finished-tasks *thread-pool*)))
    (loop until (future-finished-p future)
          do
       (with-mutex ((queue-mutex queue))
         (multiple-value-bind (finished-future available-p) (dequeue queue)
           (if available-p
               (when (eq finished-future future)
                 (return))
               (wait-condition-variable (queue-cond-var queue) (queue-mutex queue))))))
    (pop-all-finished-futures)
    future))

(defun wait-for-any-future ()
  (let ((queue (thread-pool-finished-tasks *thread-pool*)))
    (with-mutex ((queue-mutex queue))
      (multiple-value-bind (finished-future available-p) (dequeue queue)
        (if available-p
            finished-future
            (progn
              (wait-condition-variable (queue-cond-var queue) (queue-mutex queue))
              (dequeue queue)))))))

(defun wait-for-all-futures (futures)
  (mapc #'wait-for-future futures))

(defun kill-future (future)
  (let ((thread-pool *thread-pool*))
    (unless (future-finished-p future)
      (with-mutex ((thread-pool-mutex thread-pool))
        (let ((future-thread (future-thread future)))
          (when (and future-thread
                     (not (future-finished-p future))
                     (thread-alive-p future-thread))
            (kill-thread future-thread)
            (setf (future-thread future) nil)
            (setf (thread-pool-threads thread-pool)
                  (delete future-thread (thread-pool-threads thread-pool)))
            (decf (thread-pool-thread-count thread-pool))
            (unless (queue-empty-p (thread-pool-task-queue thread-pool))
              (let ((task (dequeue (thread-pool-task-queue thread-pool))))
                (when task
                  (assign-task task thread-pool :record-finished-tasks t)))))
          (queue-delete-item future (thread-pool-task-queue thread-pool)))))
    ;; use pop-all-finished-futures is better
    (pop-all-finished-futures)
    nil)) 

(defun kill-all-futures ()
  ;; NOTE: this is special
  (reset-thread-pool *thread-pool*))

(defun eval-future (fn &optional args future)
  (let ((future (or future (make-future))))
    (declare (type future future))
    (setf (future-function future) fn)
    (setf (future-args future) args)
    (setf (future-result future) 'unbound)
    (assign-task future *thread-pool* :record-finished-tasks t)
    future))

(defmacro future (&body body)
  `(eval-future #'(lambda () ,@body)))

(defun future-funcall (function &optional args future)
  (eval-future function args future))

(defun touch (future)
  (wait-for-future future) 
  (future-result future))


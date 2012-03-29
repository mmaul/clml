(in-package :future)

(defstruct (queue (:constructor %make-queue))
  (mutex (make-mutex :name "queue-lock"))
  (cond-var (make-condition-variable))
  head
  tail)

(defun make-queue (&key initial-content)
  (let* ((tail (list '%end%))
         (head (append (coerce initial-content 'list) tail)))
    (%make-queue :head head :tail tail)))

(defmacro defqfun (name args &body body)
  `(defun ,name ,args
     (with-slots (head tail) queue
       (with-mutex ((queue-mutex queue))
         ,@body))))

;;
(defqfun queue-length (queue)
  (loop for i on head
        until (eq i tail)
        count i))

(defqfun queue-empty-p (queue)
  (eq head tail))

(defqfun queue-empty! (queue)
  (setf tail (list '%end%))
  (setf head tail)
  queue)

(defqfun enqueue (item queue)
  (setf (car tail) item)
  (setf (cdr tail) (list '%end%))
  (setf tail (cdr tail))
  item)
 
(defqfun dequeue (queue)
  (if (queue-empty-p queue)
      (values nil nil)
      (values (pop head) t)))

(defqfun queue-delete-item (item queue &key (test #'eql) key)
  (setf head (delete item head :test test :key key))
  queue)

(defmacro with-timeout ((seconds &body timeout-body) &body body)
  #+sbcl `(handler-case
              (sb-ext:with-timeout ,seconds
                ,@body)
            (sb-ext:timeout ()
              ,@timeout-body))
  #+allegro `(sys:with-timeout (,seconds ,@timeout-body)
               ,@body))

(defun wait-for-new-items (queue &key timeout)
  "NOTE: Like cond-var, it requires the current thread to hold the mutex of the queue."
  (unless timeout
    (setf timeout most-positive-fixnum))
  (with-timeout (timeout :timeout)
    (wait-condition-variable (queue-cond-var queue) (queue-mutex queue))))

(defqfun queue-notify (queue)
  (notify-condition-variable (queue-cond-var queue)))

#|  example

(with-mutex ((queue-mutex queue))
  (loop
    (wait-for-new-items queue)
    (let ((task (dequeue queue)))
      (dosomething task))))

|#

(in-package :fork-future)

(defstruct (queue (:constructor %make-queue))
  ;; (mutex
  ;;  #+allegro (mp:make-process-lock :name "queue-lock")
  ;;  #+sbcl (sb-thread:make-mutex :name "queue-lock"))
  head
  tail)

(defun make-queue (&key initial-content)
  (let* ((tail (list '%end%))
         (head (append (coerce initial-content 'list) tail)))
    (%make-queue :head head :tail tail)))

(defmacro defqfun (name args &body body)
  `(defun ,name ,args
     (with-slots (head tail) queue
       ,@body)))

;;
(defqfun queue-length (queue)
  (loop for i on head
        until (eq i tail)
        count i))

(defqfun queue-empty-p (queue)
  (eq head tail))

(defqfun queue-empty! (queue)
  (setf tail '(%end%))
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
  (setf head (remove item head :test test :key key))
  queue)


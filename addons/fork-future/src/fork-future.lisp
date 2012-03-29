(in-package :fork-future)

(defvar *future-result-file-template* "/tmp/future-result.~d.tmp~~")

(defvar *fork-future-max-processes* 4)

(defparameter *running-futures* (make-hash-table))
(defparameter *pending-futures* (make-queue))

(defvar *after-fork-hooks* nil)
(defvar *before-fork-hooks* nil)

(defclass future ()
  ((pid :initarg :pid
        :accessor pid-of
        :initform nil)                  ; nil means not started yet
   (code :reader code-of
         :initarg :code
         :initform (error "Must provide code for future"))
   (lambda :reader lambda-of
           :initarg :lambda
           :initform (error "Must provide lambda for future"))
   (result :reader result-of :initform 'unbound)
   (exit-status :reader exit-status-of :initform 'unknown)))

(defmethod print-object ((f future) stream)
  (with-accessors ((pid pid-of)
                   (exit-status exit-status-of)
                   (result result-of)
                   (code code-of)) f
    (print-unreadable-object (f stream :type t :identity t)
      (format stream "PID: ~A, CODE: ~A, EXIT-STATUS: ~A, RESULT: ~A"
	      pid code exit-status result))))

(defun initialize-environment (&key kill-current-futures-p force-p)
  (when kill-current-futures-p
    (kill-all-futures force-p))
  (setf *running-futures* (make-hash-table))
  (setf *pending-futures* (make-queue)))

(defmacro with-new-environment (() &body body)
  `(let (*running-futures* *pending-futures*
         (*future-result-file-template* *future-result-file-template*)
         (*fork-future-max-processes* *fork-future-max-processes*)
         *after-fork-hooks*
         *before-fork-hooks*)
     (initialize-environment)
     (locally
         ,@body)))

(defun maybe-start-next-available-future ()
  "When there's pending futures left, and the process pool is not
full, start the next one pending future.

Return the future started or nil for process pool is full."
  (when (and (not (queue-empty-p *pending-futures*))
             (< (hash-table-count *running-futures*) *fork-future-max-processes*))
    (let ((next-future (dequeue *pending-futures*)))
      (assert (null (pid-of next-future)))
      ;; before hook
      (mapc #'funcall *before-fork-hooks*)
      (let ((pid (fork)))
        (cond ((> pid 0)
               ;; parent process context
               (setf (pid-of next-future) pid)
               (setf (gethash pid *running-futures*) next-future))
              ((zerop pid)
               ;; child process context
               (let* ((in (make-string-input-stream ""))
                      (out (make-string-output-stream))
                      (tw (make-two-way-stream in out))
                      (*standard-input* in)
                      (*standard-output* out)
                      (*error-output* out)
                      (*trace-output* out)
                      (*terminal-io* tw)
                      (*debug-io* tw)
                      (*query-io* tw)) 
                 (let* ((output-pathname (format nil *future-result-file-template* (getpid))))
                   (handler-case 
                       (progn
                         (mapc #'funcall *after-fork-hooks*)
                         (let ((result (funcall (lambda-of next-future))))
                           (cl-store:store (list (get-output-stream-string out) result) output-pathname) 
                           (close tw)
                           (close in)
                           (close out)
                           (exit 0)))
                     (error (e)
                       (cl-store:store (list (get-output-stream-string out) e) output-pathname)
                       (close tw)
                       (close in)
                       (close out)
                       (exit 1))))))
              (t
               (error "Fork failed with error code: ~a" pid))))
      next-future)))

(defmethod read-result ((future future) status-code)
  "Read result from the serialization file of the future after it process finishes and cleanup.

Return the result when finished."
  (check-type status-code integer)
  (with-slots (pid result exit-status) future
    (assert pid)
    (setf exit-status status-code)
    (let* ((path (format nil *future-result-file-template* pid)))
      (if (not (probe-file path))
          (setf result (values))        ; nil
          (destructuring-bind (output stored-result)
              (cl-store:restore path)
            (when (and output (> (length output) 0))
              (format t "~&Child of PID ~a says ~a.~%" pid output))
            (setf result stored-result)
            (delete-file (probe-file path))))) 
    result))

(defun check-exit-status-or-raise-error (future)
  (with-slots (exit-status pid result) future
    (unless (zerop exit-status)
      (error "Future of PID ~a terminated with error: ~a"
             pid result))))

(defmethod wait-for-future ((future future))
  (if (not (eq (result-of future) 'unbound))
      future
      (loop for f = (wait-for-any-future)
            while f
            until (eq f future)
            finally
         (return (or f (error "Future seems to be already finished but the states are not updated."))))))

(defun wait-for-any-future (&optional error-p (warn-p t))
  (multiple-value-bind (maybe-pid status)
      (waitpid 0)
    (prog1
        (cond ((> maybe-pid 0)
               (let ((future (gethash maybe-pid *running-futures*)))
                 (when future
                   (unwind-protect 
                        (read-result future status) 
                     (remhash maybe-pid *running-futures*)) 
                   future)))
              ((< maybe-pid 0)
               (when error-p
                 (error "No more child process.")))
              ((= maybe-pid 0)
               (error "Child exit status shouldn't be 0."))
              (t
               (when warn-p
                 (warn "A child process of PID ~a has just been reaped but it's not among the futures." maybe-pid))))
      (loop while (maybe-start-next-available-future)))))

(defun wait-for-all-futures ()
  (loop while (or (not (queue-empty-p *pending-futures*))
                  (> (hash-table-count *running-futures*) 0))
        do (wait-for-any-future)))

(defmethod kill-future ((future future) &optional force) 
  (let* ((pid (pid-of future))
         (file (format nil *future-result-file-template* pid)))
    (if pid
        (progn
          (kill pid (if force 9 15))
          (when (probe-file file)
            (delete-file (probe-file file)))
          (remhash pid *running-futures*)
          (waitpid 0))
        (progn
          (queue-delete-item *pending-futures* future)))))

(defun kill-all-futures (&optional force)
  (queue-empty! *pending-futures*)
  (loop while (> (hash-table-count *running-futures*) 0)
        do (block only-once
             (maphash #'(lambda (key value)
                          (declare (ignore key))
                          (kill-future value force)
                          (return-from only-once))
                      *running-futures*)))
  (loop for pid = (waitpid 0)
        until (< pid 0))
  (clrhash *running-futures*))


(defun eval-future (fn code)
  (check-type fn function)
  (let ((future (make-instance 'future :code code :lambda fn)))
    ;; eval
    (enqueue future *pending-futures*)
    (loop while (maybe-start-next-available-future))
    future))

(defmacro future (&body body)
  "Evaluate expr in parallel using a forked child process. Returns a
'future' object whose value can be retrieved using touch. No
side-effects made in <expr> will be visible from the calling process."
  `(eval-future #'(lambda ()
                    (with-new-environment ()
                      ,@body))
                '(progn ,@body)))

(defmethod touch ((future future))
  "walk the list structure 'future', replacing any futures with their
evaluated values. Blocks if a future is still running."
  (with-slots (result) future
    (wait-for-future future)
    (check-exit-status-or-raise-error future)
    (result-of future)))



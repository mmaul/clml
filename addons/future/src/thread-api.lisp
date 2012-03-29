(in-package :future)

;;; portable helper function
(defun spawn-thread (function &key name)
  #+sbcl (sb-thread:make-thread function :name name)
  #+allegro (mp:process-run-function name function))

(defun current-thread ()
  #+sbcl sb-thread:*current-thread*
  #+allegro mp:*current-process*)

(defun thread-alive-p (thread)
  #+sbcl (sb-thread:thread-alive-p thread)
  #+allegro (mp:process-alive-p thread))

(defun kill-thread (thread)
  (when (thread-alive-p thread)
    #+sbcl (sb-thread:terminate-thread thread)
    #+allegro (mp:process-kill thread)))

(defun join-threads (threads)
  #+sbcl (loop for thread in threads
               do (sb-thread:join-thread thread))
  #+allegro (loop for thread in threads
                  do (mp:process-wait "join" (complement #'mp:process-alive-p))))

;;
(defun make-mutex (&key name owner)
  #+sbcl (sb-thread:make-mutex :name name :%owner owner)
  #+allegro (mp:make-process-lock :name name :locker owner))

(defun mutex-owner (mutex)
  #+sbcl (sb-thread:mutex-owner mutex)
  #+allegro (mp:process-lock-locker mutex))

(defmacro with-mutex ((mutex) &body body)
  #+sbcl
  `(sb-thread:with-recursive-lock (,mutex)
     ,@body)
  #+allegro
  `(mp:with-process-lock (,mutex)
     ,@body))

(defun release-mutex (mutex)
  #+sbcl (sb-thread:release-mutex mutex)
  #+allegro (mp:process-unlock mutex))

(defun get-mutex (mutex)
  #+sbcl (sb-thread:get-mutex mutex)
  #+allegro (mp:process-lock mutex))

(defun holding-mutex-p (mutex)
  #+sbcl (sb-thread:holding-mutex-p mutex)
  #+allegro (eq (mp:process-lock-locker mutex) mp:*current-process*))

;;
(defun make-condition-variable ()
  #+sbcl (sb-thread:make-waitqueue)
  #+allegro (mp:make-gate nil))

(defun wait-condition-variable (cond-var mutex)
  #+sbcl (sb-thread:condition-wait cond-var mutex)
  #+allegro (unwind-protect
                 (progn
                   (assert (holding-mutex-p mutex))
                   (release-mutex mutex)
                   (mp:process-wait "Wait cond var" #'mp:gate-open-p cond-var))
              (get-mutex mutex)))

(defun notify-condition-variable (cond-var)
  #+sbcl (sb-thread:condition-notify cond-var)
  #+allegro (mp:open-gate cond-var))


;;; waitqueue
;;; it's more sensible to group the condition-variable with a lock
(defstruct waitqueue
  (lock (make-mutex :name "waitqueue lock"))
  (condition-variable (make-condition-variable)))

(defun wait-waitqueue (waitqueue)
  (let* ((lock (waitqueue-lock waitqueue))
         (cond-var (waitqueue-condition-variable waitqueue)))
    (wait-condition-variable cond-var lock)))

(defun notify-waitqueue (waitqueue)
  (notify-condition-variable (waitqueue-condition-variable waitqueue)))

(defmacro with-waitqueue-mutex ((waitqueue) &body body)
  (alexandria:once-only (waitqueue)
    `(with-mutex ((waitqueue-lock ,waitqueue))
       ,@body)))


(in-package :clml.statistics)
(defclass distribution ()
  ((mean)
   (variance)
   (skewness)
   (kurtosis)
   (mode)))

(defgeneric update-distribution (distribution)
  (:method (distribution)
	   distribution))

(defmethod initialize-instance ((instance distribution) &rest initargs)
  (declare (ignore initargs))
  (call-next-method)
  (update-distribution instance))

(defclass discrete-distribution (distribution) ())
(defclass continuous-distribution (distribution)
  ((mean :initarg :mean :accessor mean :initform 0d0)))

(eval-when (:compile-toplevel :load-toplevel)
  (defun parse-dist-slots (distribution-slots class)
    (let (class-slots methods)
      (dolist (slot distribution-slots)
        (let* ((def (cdr slot))
               (updater (getf def :accessor)))
          (if updater
              (let ((writer (intern (format nil "~A-~A" 'set updater)))
                    (s (car slot)))
                (setf def (copy-list def))
                (remf def :accessor)
                (push `(,s :reader ,updater
                           :writer ,writer ,@def) class-slots)
                ;; use (defmethod (setf foo))
                (push `(defmethod (setf ,updater) (new-value (obj ,class))
                         (setf (slot-value obj ',s) new-value)
                         (update-distribution obj))
                      methods))
              (push slot class-slots))))
      (values (nreverse class-slots)
              (nreverse methods)))))

;; easy definition of parameter cache class
(defmacro defdistribution (name direct-supers direct-slots &rest options)
  (multiple-value-bind (new-slot-definitions methods) (parse-dist-slots direct-slots name)
    `(prog1 (defclass ,name ,direct-supers ,new-slot-definitions ,@(when options
								     `(&rest . ,options)))
       ,@methods)))


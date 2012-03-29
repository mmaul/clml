;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information 

;;  The framework where everything hangs together.
;; 

(in-package :cl-store)

(defvar *store-used-packages* nil
  "If non-nil will serialize each used package otherwise will
only store the package name")
(defvar *nuke-existing-packages* nil
  "Whether or not to overwrite existing packages on restoration.")
(defvar *nuke-existing-classes* nil
  "Do we overwrite existing class definitions on restoration.")
(defvar *store-class-superclasses* nil
  "Whether or not to store the superclasses of a stored class.")
(defvar *store-class-slots* t
  "Whether or not to serialize slots which are class allocated.")

(declaim (type backend *default-backend* *current-backend*))
(defvar *default-backend*)
(defvar *current-backend*)


;; conditions
;; From 0.2.3 all conditions which are signalled from 
;; store or restore will signal a store-error or a 
;; restore-error respectively inside a handler-bind.
(defun cl-store-report (condition stream)
  (declare (stream stream))
  (aif (caused-by condition)
       (format stream "~A" it)
       (apply #'format stream (format-string condition) 
              (format-args condition))))

(define-condition cl-store-error (error)
  ((caused-by :accessor caused-by :initarg :caused-by 
              :initform nil)
   (format-string :accessor format-string :initarg :format-string 
                  :initform "Unknown")
   (format-args :accessor format-args :initarg :format-args :initform nil))
  (:report cl-store-report)
  (:documentation "Root cl-store condition"))

(define-condition store-error (cl-store-error)
  ()
  (:documentation "Error thrown when storing an object fails."))

(define-condition restore-error (cl-store-error)
  ()
  (:documentation "Error thrown when restoring an object fails."))

(defun store-error (format-string &rest args)
  (error 'store-error :format-string format-string :format-args args))

(defun restore-error (format-string &rest args)
  (error 'restore-error :format-string format-string :format-args args))


;; entry points
(defun store-to-file (obj place backend)
  (declare (type backend backend)
           (optimize speed))
  (let ((element-type (stream-type backend)))
    (with-open-file (s place :element-type element-type
                       :direction :output :if-exists :supersede)
      (backend-store backend s obj))))

(defgeneric store (obj place &optional designator) 
  (:documentation "Store OBJ into Stream PLACE using backend BACKEND.")
  (:method ((obj t) (place t) &optional (designator *default-backend*))
   "Store OBJ into Stream PLACE using backend BACKEND."
   (declare (optimize speed))
   (let* ((backend (backend-designator->backend designator))
          (*current-backend* backend)
          (*read-eval* nil))
     (handler-bind ((error (lambda (c)
                             (signal 'store-error :caused-by c))))
       (backend-store backend place obj)))))


(defgeneric backend-store (backend place obj)
  (:method ((backend backend) (place stream) (obj t))
    "The default. Checks the streams element-type, stores the backend code
     and calls store-object."
    (declare (optimize speed))
    (store-backend-code backend place)
    (store-object obj place backend)
    obj)
  (:method ((backend backend) (place string) (obj t))
    "Store OBJ into file designator PLACE."
    (store-to-file obj place backend))
  (:method ((backend backend) (place pathname) (obj t))
    "Store OBJ into file designator PLACE."
    (store-to-file obj place backend))
  (:documentation "Method wrapped by store, override this method for 
    custom behaviour (see circularities.lisp)."))

(defgeneric store-backend-code (backend stream)
  (:method ((backend backend) (stream t))
    (declare (optimize speed))
    (when-let (magic (magic-number backend))
      (store-32-bit magic stream)))
  (:documentation 
   "Store magic-number of BACKEND, when present, into STREAM."))

(defun store-object (obj stream &optional (backend *current-backend*))
  "Store OBJ into STREAM. Not meant to be overridden, 
   use backend-store-object instead"
  (backend-store-object backend obj stream))

(defgeneric backend-store-object (backend obj stream)
  (:documentation
   "Wrapped by store-object, override this to do custom storing 
   (see circularities.lisp for an example).")
  (:method ((backend backend) (obj t) (stream t))
    "The default, just calls internal-store-object."
    (declare (optimize speed))
    (internal-store-object backend obj stream)))


(defgeneric internal-store-object (backend obj place)
  (:documentation "Method which is specialized by defstore-? macros.")
  (:method ((backend backend) (obj t) (place t))
    "If call falls back here then OBJ cannot be serialized with BACKEND."
    (store-error "Cannot store objects of type ~A with backend ~(~A~)."
                 (type-of obj) (name backend))))

;; restoration
(defgeneric restore (place &optional backend)
  (:documentation 
   "Restore and object FROM PLACE using BACKEND. Not meant to be 
   overridden, use backend-restore instead")
  (:method (place &optional (designator *default-backend*))
    "Entry point for restoring objects (setfable)."
    (declare (optimize speed))
    (let* ((backend (backend-designator->backend designator))
           (*current-backend* backend)
           (*read-eval* nil))
      (handler-bind ((error (lambda (c)
                              (signal 'restore-error :caused-by c))))
        (backend-restore backend place)))))

  
(defgeneric backend-restore (backend place)
  (:documentation "Wrapped by restore. Override this to do custom restoration")
  (:method ((backend backend) (place stream))
    "Restore the object found in stream PLACE using backend BACKEND.
     Checks the magic-number and invokes backend-restore-object"
    (declare (optimize speed))
    (check-magic-number backend place)
    (backend-restore-object backend place))
  (:method ((backend backend) (place string))
    "Restore the object found in file designator PLACE using backend BACKEND."
    (restore-from-file place backend))
  (:method ((backend backend) (place pathname))
    "Restore the object found in file designator PLACE using backend BACKEND."
    (restore-from-file place backend)))

(defun restore-from-file (place backend)
  (declare (optimize speed))
  (let ((element-type (stream-type backend)))
    (with-open-file (s place :element-type element-type :direction :input)
      (backend-restore backend s))))
     
(defun (setf restore) (new-val place &optional (backend *default-backend*))
  (store new-val place backend))

(defgeneric check-magic-number (backend stream)
  (:method ((backend backend) (stream t))
    (let ((magic-number (magic-number backend)))
      (declare (type (or null ub32) magic-number))
      (when magic-number
        (let ((val (read-32-bit stream nil)))
          (declare (type ub32 val))
          (cond ((= val magic-number) nil)
                ((member val (compatible-magic-numbers backend))
                 nil)
                ((member val (old-magic-numbers backend))
                 (restore-error "Stream contains an object stored with an ~
incompatible version of backend ~A." (name backend)))
                (t (restore-error "Stream does not contain a stored object~
 for backend ~A."
                                  (name backend))))))))
  (:documentation   
   "Check to see if STREAM actually contains a stored object for BACKEND."))

(defun lookup-reader (val readers)
  (gethash val readers))

(defgeneric get-next-reader (backend place)
  (:documentation 
   "Method which must be specialized for BACKEND to return 
   the next function to restore an object from PLACE.
   If no reader is found return a second value which will be included 
   in the error.")
  (:method ((backend backend) (place t))
   (declare (ignore place))
    "The default, throw an error."
    (restore-error "get-next-reader must be specialized for backend ~(~A~)."
                   (name backend))))

;; Wrapper for backend-restore-object so we don't have to pass
;; a backend object around all the time

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun restore-object (place &optional (backend *current-backend*))
    "Restore the object in PLACE using BACKEND"
    (backend-restore-object backend place)))

(defgeneric backend-restore-object (backend place)
  (:documentation
   "Find the next function to call with BACKEND and invoke it with PLACE.")
  (:method ((backend backend) (place t))
    "The default"
    (internal-restore-object backend (get-next-reader backend place) place)))

(defgeneric internal-restore-object (backend type place))


;; EOF

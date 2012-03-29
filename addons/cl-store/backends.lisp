;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.

;; CL-STORE now has a concept of backends.
;; store and restore now take an optional  backend as an
;;  argument to do the actual restoring. Examples of use are 
;; in default-backend.lisp and xml-backend.lisp

(in-package :cl-store)

(defun required-arg (name)
  (error "~S is a required argument" name))

(defclass backend ()
  ((name :accessor name :initform "Unknown" :initarg :name :type symbol)
   (magic-number :accessor magic-number :initarg :magic-number :type integer)
   (compatible-magic-numbers :accessor compatible-magic-numbers
                             :initarg :compatible-magic-numbers :type list)
   (old-magic-numbers :accessor old-magic-numbers :initarg :old-magic-numbers
                      :type list)
   (stream-type :accessor stream-type :initarg :stream-type :type (or symbol cons)
                :initform (required-arg :stream-type)))
  (:documentation "Core class which custom backends must extend"))

(deftype backend-designator ()
  `(or symbol backend))

(defparameter *registered-backends* nil 
  "An assoc list mapping backend-names to the backend objects")

(defun find-backend (name &optional errorp)
  (declare (type symbol name))
  "Return backup called NAME. If there is no such backend NIL is returned 
if ERRORP is false, otherwise an error is signalled."
  (or (cdr (assoc name *registered-backends*))
      (if errorp 
          (error "Backend named ~S does not exist." name)
          nil)))

(defun backend-designator->backend (designator)
  (check-type designator backend-designator)
  (etypecase designator
    (symbol (find-backend designator t))
    (backend designator)))


#+lispworks
(defun get-store-macro (name)
  "Return the defstore-? macro which will be used by a custom backend"
  (let ((macro-name (symbolicate 'defstore- name)))
    `(defmacro ,macro-name ((var type stream &optional qualifier) 
                            &body body)
       (with-gensyms (gbackend)
         `(dspec:def (,',macro-name (,var ,type ,stream))
            (defmethod internal-store-object ,@(if qualifier (list qualifier) nil)
              ((,gbackend ,',name) (,var ,type) ,stream)
              ,(format nil "Definition for storing an object of type ~A with ~
 backend ~A" type ',name)
              (declare (ignorable ,gbackend))
              ,@body))))))

#-lispworks
(defun get-store-macro (name)
  "Return the defstore-? macro which will be used by a custom backend"
  (let ((macro-name (symbolicate 'defstore- name)))
    `(defmacro ,macro-name ((var type stream &optional qualifier) 
                            &body body)
       (with-gensyms (gbackend)
         `(defmethod internal-store-object ,@(if qualifier (list qualifier) nil)
              ((,gbackend ,',name) (,var ,type) ,stream)
              ,(format nil "Definition for storing an object of type ~A with ~
 backend ~A" type ',name)
              (declare (ignorable ,gbackend))
              ,@body)))))

#+lispworks
(defun get-restore-macro (name)
  "Return the defrestore-? macro which will be used by a custom backend"
  (let ((macro-name (symbolicate 'defrestore- name)))
    `(defmacro ,macro-name ((type place &optional qualifier) &body body)
       (with-gensyms (gbackend gtype)
         `(dspec:def (,',macro-name (,type ,place))
            (defmethod internal-restore-object ,@(if qualifier (list qualifier) nil)
              ((,gbackend ,',name) (,gtype (eql ',type)) (,place t))
              (declare (ignorable ,gbackend ,gtype))
              ,@body))))))

#-lispworks
(defun get-restore-macro (name)
  "Return the defrestore-? macro which will be used by a custom backend"
  (let ((macro-name (symbolicate 'defrestore- name)))
    `(defmacro ,macro-name ((type place &optional qualifier) &body body)
       (with-gensyms (gbackend gtype)
         `(defmethod internal-restore-object ,@(if qualifier (list qualifier) nil)
            ((,gbackend ,',name) (,gtype (eql ',type)) (,place t))
            (declare (ignorable ,gbackend ,gtype))
            ,@body)))))


(defun register-backend (name class magic-number stream-type old-magic-numbers 
                              compatible-magic-numbers)
  (declare (type symbol name))
  (let ((instance (make-instance class
                                 :name name
                                 :magic-number magic-number
                                 :old-magic-numbers old-magic-numbers
                                 :compatible-magic-numbers compatible-magic-numbers
                                 :stream-type  stream-type)))
    (if (assoc name *registered-backends*)
        (cerror "Redefine backend" "Backend ~A is already defined." name)
        (push (cons name instance) *registered-backends*))
    instance))

(defun alias-backend (old alias)
  (let ((backend (find-backend old t)))
    (pushnew (cons alias backend) *registered-backends*
             :test #'equalp)
    t))

(defun get-class-form (name fields extends)
  `(defclass ,name ,extends
    ,fields
    (:documentation ,(format nil "Autogenerated cl-store class for backend ~(~A~)."
                             name))))


#+lispworks
(defun get-dspec-alias-and-parser (name)
  (let ((store-name (symbolicate 'defstore- name))
        (restore-name (symbolicate 'defrestore- name)))
    `( (dspec:define-dspec-alias ,store-name (arglist)
         `(method cl-store::internal-store-object ,arglist))
       (dspec:define-form-parser ,store-name (arglist)
         `(,,store-name ,arglist))

       (dspec:define-dspec-alias ,restore-name (arglist)
         `(method cl-store::internal-restore-object ,arglist))

       (dspec:define-form-parser ,restore-name (arglist)
         `(,,restore-name ,arglist)))))


(defmacro defbackend (name &key (stream-type ''(unsigned-byte 8))
                           (magic-number nil) fields (extends '(backend))
                           (old-magic-numbers nil) (compatible-magic-numbers nil))
  "Defines a new backend called NAME. Stream type must be either 'char or 'binary. 
FIELDS is a list of legal slots for defclass. MAGIC-NUMBER, when supplied, will
be written down stream as verification and checked on restoration.
EXTENDS is a class to extend, which must be backend or a class which extends
backend"
  (assert (symbolp name))
  `(eval-when (:load-toplevel :execute)
     (eval-when (:compile-toplevel :load-toplevel :execute)
       #+lispworks ,@(get-dspec-alias-and-parser name)
       ,(get-class-form name fields extends)
       ,(get-store-macro name)
       ,(get-restore-macro name))
     (register-backend ',name ',name ,magic-number 
                       ,stream-type ',old-magic-numbers ',compatible-magic-numbers)))

(defmacro with-backend (backend &body body)
  "Run BODY with *default-backend* bound to BACKEND"
  `(let* ((*default-backend* (backend-designator->backend ,backend)))
    ,@body))

;; EOF

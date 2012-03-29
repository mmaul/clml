;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.

(in-package :cl-store)

;; this is such a pain.

(defgeneric slot-definition-name (slot))
(defgeneric slot-definition-allocation (slot))

(defmethod slot-definition-name ((slot vector))
  (aref slot 0))

(defmethod slot-definition-allocation ((slot vector))
  (if (keywordp (aref slot 4))
      :instance
      :class))


(defun compute-slots (class)
  (std-compute-slots class))
                  
(defun slot-definition-x (val slot)
  (cadr (member val slot)))


(defmethod slot-definition-allocation ((slot cons))
  (or (slot-definition-x :allocation slot)
      :instance))

(defmethod slot-definition-initargs ((slot cons))
  (slot-definition-x :initargs slot))

(defmethod slot-definition-name ((slot cons))
  (slot-definition-x :name slot))

(defmethod slot-definition-readers ((slot cons))
  (slot-definition-x :readers slot))

(defmethod slot-definition-writers ((slot cons))
  (slot-definition-x :writers slot))

(defmethod slot-definition-type ((slot cons))
  (or (slot-definition-x :type slot)
      t))

(defun class-direct-superclasses (class)
  (or (clos::class-direct-superclasses class)
      (list (find-class 'standard-object))))


(defun add-methods-for-class (class vals)
  (let ((readers (mappend #'(lambda (x)
                              (second (member :readers x)))
                          vals))
        (writers (mappend #'(lambda (x)
                              (second (member :writers x)))
                          vals)))
    (loop for x in readers do
          (eval `(defmethod ,x ((clos::object ,class))
                  (slot-value clos::object ',x))))
    (loop for x in writers do
          (eval `(defmethod ,x (clos::new-value (clos::object ,class))
                  (setf (slot-value clos::object ',x) clos::new-value))))
    (find-class class)))

(defmethod generic-function-name ((gf generic-function))
  (multiple-value-bind (l cp name) (function-lambda-expression gf)
    (declare (ignore l cp))
    name))

;; EOF
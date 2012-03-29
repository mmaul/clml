(in-package :cl-store)

(defun slot-definition-name (slot)
  (nth 0 slot))

(defun slot-definition-allocation (slot)
  (nth 6 slot))

(defun slot-definition-initform (slot)
  (nth 2 slot))

(defun slot-definition-initargs (slot)
  (nth 1 slot))

(defun slot-accessors (slot)
  (nth 3 slot))

(defun slot-definition-writers (slot)
  (append (slot-accessors slot)
          (nth 5 slot)))

(defun slot-definition-readers (slot)
  (append (slot-accessors slot)
          (nth 4 slot)))

(defun slot-definition-type (slot)
  (nth 7 slot))

;; EOF
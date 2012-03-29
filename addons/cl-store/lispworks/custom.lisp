;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.

(in-package :cl-store)

;; Special float handling
(defun create-float-values (value &rest codes)
  (let ((neg-inf (expt value 3)))
    (mapcar 'cons
            (list (expt (abs value) 2)
                  neg-inf
                  (/ neg-inf neg-inf))
            codes)))

;; Custom structure storing from Alain Picard.
(defstore-cl-store (obj structure-object stream)
  (output-type-code +structure-object-code+ stream)
  (let* ((slot-names (structure:structure-class-slot-names (class-of obj))))
    (store-object (type-of obj) stream)
    (store-object (length slot-names) stream)
    (dolist (slot-name slot-names)
      (store-object slot-name stream)
      (store-object (slot-value obj slot-name) stream))))

(defrestore-cl-store (structure-object stream)
  (let* ((class (find-class (restore-object stream)))
         (length (restore-object stream))
         (new-instance (structure::allocate-instance class)))
    (loop repeat length do
          (let ((slot-name (restore-object stream)))
            ;; slot-names are always symbols so we don't
            ;; have to worry about circularities
            (resolving-object (obj new-instance)
              (setting (slot-value obj slot-name) (restore-object stream)))))
    new-instance))

;; EOF

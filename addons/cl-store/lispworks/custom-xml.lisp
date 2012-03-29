;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.

(in-package :cl-store-xml)

(defstore-xml (obj structure-object stream)
  (with-tag ("STRUCTURE-OBJECT" stream)
    (princ-and-store "CLASS" (type-of obj) stream)
    (let ((slots (structure:structure-class-slot-names (class-of obj))))
      (with-tag ("SLOTS" stream)
        (dolist (slot-name slots)
          (with-tag ("SLOT" stream)
            (princ-and-store "NAME" slot-name stream)
            (princ-and-store "VALUE" (slot-value obj slot-name) stream)))))))

(defrestore-xml (structure-object place)
  (let* ((class (find-class (restore-first (get-child "CLASS" place))))
         (new-instance (structure::allocate-instance class)))
    (resolving-object new-instance
      (dolist (slot (xmls:node-children (get-child "SLOTS" place)))
        (let ((slot-name (restore-first (get-child "NAME" slot))))
          (setting (slot-value slot-name) 
                   (restore-first (get-child "VALUE" slot))))))))



(defstore-xml (obj float stream)
  (block body
    (handler-bind ((simple-error
                    #'(lambda (err)
                        (declare (ignore err))
                        (cond
                         ((cl-store::positive-infinity-p obj)
                          (with-tag ("POSITIVE-INFINITY" stream))
                          (return-from body)) 
                         ((cl-store::negative-infinity-p obj)
                          (with-tag ("NEGATIVE-INFINITY" stream))
                          (return-from body))
                         ((cl-store::float-nan-p obj)
                          (with-tag ("FLOAT-NAN" stream))
                          (return-from body))
                         (t nil)))))
        (multiple-value-bind (signif exp sign) 
            (integer-decode-float obj)
          (with-tag ("FLOAT" stream)
            (princ-and-store "SIGNIFICAND" signif stream)
            (princ-and-store "EXPONENT" exp stream)
            (princ-and-store "SIGN" sign stream)
            (princ-and-store "TYPE" (float-type obj) stream))))))

(defrestore-xml (positive-infinity stream)
  (declare (ignore stream))
  cl-store::+positive-infinity+)

(defrestore-xml (negative-infinity stream)
  (declare (ignore stream))
  cl-store::+negative-infinity+)

(defrestore-xml (float-nan stream)
  (declare (ignore stream))
  cl-store::+nan-float+)

;; EOF

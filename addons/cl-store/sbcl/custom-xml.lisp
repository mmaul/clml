;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.

(in-package :cl-store-xml)


(defstore-xml (obj structure-object stream)
  (with-tag ("STRUCTURE-OBJECT" stream)
    (princ-and-store "CLASS" (type-of obj) stream)
    (xml-dump-type-object obj stream)))


(defrestore-xml (structure-object place)
  (restore-xml-type-object place))


(defstore-xml (obj single-float stream)
  (with-tag ("SINGLE-FLOAT" stream)
    (princ-and-store "BITS" (sb-kernel::single-float-bits obj)
                     stream)))

(defrestore-xml (single-float stream)
  (sb-kernel::make-single-float
   (restore-first (get-child "BITS" stream))))

(defstore-xml (obj double-float stream)
  (with-tag ("DOUBLE-FLOAT" stream)
    (princ-and-store "HIGH-BITS" (sb-kernel::double-float-high-bits obj)
                     stream)
    (princ-and-store "LOW-BITS" (sb-kernel::double-float-low-bits obj)
                     stream)))

(defrestore-xml (double-float stream)
  (sb-kernel::make-double-float (restore-first (get-child "HIGH-BITS" stream))
                                (restore-first (get-child "LOW-BITS" stream))))
         

;; EOF
;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.

(in-package :cl-store-tests)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :cl-store-xml))

(add-xml-mapping "RANDOM-OBJ")

(defstore-xml (obj random-obj stream)
  (princ-and-store "RANDOM-OBJ" (size obj) stream))

(defrestore-xml (random-obj stream)
  (random (restore-first stream)))

;; EOF
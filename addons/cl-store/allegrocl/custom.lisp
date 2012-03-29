;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.

(in-package :cl-store)

(defun setup-special-floats ()
  (flet ((short-float-values ()
           (list (cons #.excl::*infinity-single* +short-float-inf+)
                 (cons #.excl::*negative-infinity-single* +short-float-neg-inf+)
                 (cons #.excl::*nan-single* +short-float-nan+)))
         (single-float-values ()
           (list (cons #.excl::*infinity-single* +single-float-inf+)
                 (cons #.excl::*negative-infinity-single* +single-float-neg-inf+)
                 (cons #.excl::*nan-single* +single-float-nan+)))
         (double-float-values ()
           (list (cons #.excl::*infinity-double* +double-float-inf+)
                 (cons #.excl::*negative-infinity-double* +double-float-neg-inf+)
                 (cons #.excl::*nan-double* +double-float-nan+)))
         (long-float-values ()
           (list (cons #.excl::*infinity-double* +long-float-inf+)
                 (cons #.excl::*negative-infinity-double* +long-float-neg-inf+)
                 (cons #.excl::*nan-double* +long-float-nan+))))
    (setf *special-floats*
          (append (short-float-values)
                  (single-float-values)
                  (double-float-values)
                  (long-float-values)))))

;; EOF

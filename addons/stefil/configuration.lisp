;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :stefil)

;;; These definitions need to be available by the time we are reading the other files, therefore
;;; they are in a standalone file.

(defmacro debug-only (&body body)
  #+debug`(progn ,@body)
  #-debug(declare (ignore body)))

(defun file-header ()
  `(eval-always
    (setup-readtable)))

(defun setup-readtable ()
  (enable-sharp-boolean-syntax))



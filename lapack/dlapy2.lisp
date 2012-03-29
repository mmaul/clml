;;; Compiled by f2cl version:
;;; ("$Id: f2cl1.l,v 1.209 2008/09/11 14:59:55 rtoy Exp $"
;;;  "$Id: f2cl2.l,v 1.37 2008/02/22 22:19:33 rtoy Rel $"
;;;  "$Id: f2cl3.l,v 1.6 2008/02/22 22:19:33 rtoy Rel $"
;;;  "$Id: f2cl4.l,v 1.7 2008/02/22 22:19:34 rtoy Rel $"
;;;  "$Id: f2cl5.l,v 1.197 2008/09/11 15:03:25 rtoy Exp $"
;;;  "$Id: f2cl6.l,v 1.48 2008/08/24 00:56:27 rtoy Exp $"
;;;  "$Id: macros.l,v 1.106 2008/09/15 15:27:36 rtoy Exp $")

;;; Using Lisp International Allegro CL Enterprise Edition 8.1 [64-bit Linux (x86-64)] (Oct 7, 2008 17:13)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t)
;;;           (:relaxed-array-decls t) (:coerce-assigns :as-needed)
;;;           (:array-type ':array) (:array-slicing t)
;;;           (:declare-common nil) (:float-format double-float))

(in-package "LAPACK")


(let* ((zero 0.0) (one 1.0))
  (declare (type (double-float 0.0 0.0) zero)
   (type (double-float 1.0 1.0) one) (ignorable zero one))
  (defun dlapy2 (x y)
    (declare (type (double-float) y x))
    (prog ((w 0.0) (xabs 0.0) (yabs 0.0) (z 0.0) (dlapy2 0.0))
          (declare (type (double-float) w xabs yabs z dlapy2))
          (setf xabs (abs x))
          (setf yabs (abs y))
          (setf w (max xabs yabs))
          (setf z (min xabs yabs))
          (cond ((= z zero) (setf dlapy2 w))
                (t
                 (setf dlapy2
                       (* w
                          (f2cl-lib:fsqrt (+ one (expt (/ z w) 2)))))))
          (go end_label)
     end_label (return (values dlapy2 nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlapy2
                 fortran-to-lisp::*f2cl-function-info*)
        (fortran-to-lisp::make-f2cl-finfo :arg-types '((double-float)
                                                       (double-float))
          :return-values '(nil nil)
          :calls 'nil)))


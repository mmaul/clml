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

(in-package "BLAS")


(defun zrotg (ca cb c s)
  (declare (type (double-float) c) (type (f2cl-lib:complex16) s cb ca))
  (prog ((alpha #C(0.0 0.0)) (norm 0.0) (scale 0.0))
        (declare (type (double-float) scale norm)
         (type (f2cl-lib:complex16) alpha))
        (if (/= (f2cl-lib:cdabs ca) 0.0) (go label10))
        (setf c 0.0)
        (setf s (f2cl-lib:cmplx 1.0 0.0))
        (setf ca cb)
        (go label20)
   label10 (setf scale
                 (coerce (+ (f2cl-lib:cdabs ca) (f2cl-lib:cdabs cb))
                         'double-float))
        (setf norm
              (* scale
                 (f2cl-lib:dsqrt (+ (expt (f2cl-lib:cdabs (/ ca
                                                             (f2cl-lib:dcmplx scale
                                                                              0.0)))
                                          2)
                                    (expt (f2cl-lib:cdabs (/ cb
                                                             (f2cl-lib:dcmplx scale
                                                                              0.0)))
                                          2)))))
        (setf alpha (/ ca (f2cl-lib:cdabs ca)))
        (setf c (/ (f2cl-lib:cdabs ca) norm))
        (setf s (/ (* alpha (f2cl-lib:dconjg cb)) norm))
        (setf ca (* alpha norm))
   label20 (go end_label)
   end_label (return (values ca nil c s))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zrotg
                 fortran-to-lisp::*f2cl-function-info*)
        (fortran-to-lisp::make-f2cl-finfo :arg-types '((fortran-to-lisp::complex16)
                                                       (fortran-to-lisp::complex16)
                                                       (double-float)
                                                       (fortran-to-lisp::complex16))
          :return-values '(fortran-to-lisp::ca nil fortran-to-lisp::c
                           fortran-to-lisp::s)
          :calls 'nil)))


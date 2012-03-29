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


(let* ((zero 0.0))
  (declare (type (double-float 0.0 0.0) zero) (ignorable zero))
  (defun dlassq (n x incx scale sumsq)
    (declare (type (double-float) sumsq scale)
     (type (array double-float (*)) x)
     (type (f2cl-lib:integer4) incx n))
    (f2cl-lib:with-multi-array-data ((x double-float x-%data%
                                      x-%offset%))
      (prog ((absxi 0.0) (ix 0))
            (declare (type (double-float) absxi)
             (type (f2cl-lib:integer4) ix))
            (cond ((> n 0)
                   (f2cl-lib:fdo (ix 1 (f2cl-lib:int-add ix incx))
                                 ((> ix
                                     (f2cl-lib:int-add 1
                                                       (f2cl-lib:int-mul (f2cl-lib:int-add n
                                                                                           (f2cl-lib:int-sub 1))
                                                                         incx)))
                                  nil)
                                 (tagbody
                                     (cond ((/= (f2cl-lib:fref x (ix)
                                                               ((1 *)))
                                                zero)
                                            (setf absxi
                                                  (abs (f2cl-lib:fref x-%data%
                                                                      (ix)
                                                                      ((1
                                                                        *))
                                                                      x-%offset%)))
                                            (cond ((< scale absxi)
                                                   (setf sumsq
                                                         (+ 1
                                                            (* sumsq
                                                               (expt (/ scale
                                                                        absxi)
                                                                     2))))
                                                   (setf scale absxi))
                                                  (t
                                                   (setf sumsq
                                                         (+ sumsq
                                                            (expt (/ absxi
                                                                     scale)
                                                                  2)))))))
                                   label10))))
            (go end_label)
       end_label (return (values nil nil nil scale sumsq))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlassq
                 fortran-to-lisp::*f2cl-function-info*)
        (fortran-to-lisp::make-f2cl-finfo :arg-types '((fortran-to-lisp::integer4)
                                                       (array
                                                        double-float
                                                        (*))
                                                       (fortran-to-lisp::integer4)
                                                       (double-float)
                                                       (double-float))
          :return-values '(nil nil nil fortran-to-lisp::scale
                           fortran-to-lisp::sumsq)
          :calls 'nil)))


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
;;;           (:relaxed-array-decls nil) (:coerce-assigns :as-needed)
;;;           (:array-type ':array) (:array-slicing t)
;;;           (:declare-common nil) (:float-format double-float))

(in-package "BLAS")


(defun idamax (n dx incx)
  (declare (type (array double-float (*)) dx)
   (type (f2cl-lib:integer4) incx n))
  (f2cl-lib:with-multi-array-data ((dx double-float dx-%data%
                                    dx-%offset%))
    (prog ((i 0) (ix 0) (dmax 0.0) (idamax 0))
          (declare (type (double-float) dmax)
           (type (f2cl-lib:integer4) idamax ix i))
          (setf idamax 0)
          (if (or (< n 1) (<= incx 0)) (go end_label))
          (setf idamax 1)
          (if (= n 1) (go end_label))
          (if (= incx 1) (go label20))
          (setf ix 1)
          (setf dmax
                (f2cl-lib:dabs (f2cl-lib:fref dx-%data% (1) ((1 *))
                                              dx-%offset%)))
          (setf ix (f2cl-lib:int-add ix incx))
          (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1)) ((> i n) nil)
                        (tagbody
                            (if (<= (f2cl-lib:dabs (f2cl-lib:fref dx-%data%
                                                                  (ix)
                                                                  ((1
                                                                    *))
                                                                  dx-%offset%))
                                    dmax)
                                (go label5))
                            (setf idamax i)
                            (setf dmax
                                  (f2cl-lib:dabs (f2cl-lib:fref dx-%data%
                                                                (ix)
                                                                ((1 *))
                                                                dx-%offset%)))
                          label5
                            (setf ix (f2cl-lib:int-add ix incx))
                          label10))
          (go end_label)
     label20 (setf dmax
                   (f2cl-lib:dabs (f2cl-lib:fref dx-%data% (1) ((1 *))
                                                 dx-%offset%)))
          (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1)) ((> i n) nil)
                        (tagbody
                            (if (<= (f2cl-lib:dabs (f2cl-lib:fref dx-%data%
                                                                  (i)
                                                                  ((1
                                                                    *))
                                                                  dx-%offset%))
                                    dmax)
                                (go label30))
                            (setf idamax i)
                            (setf dmax
                                  (f2cl-lib:dabs (f2cl-lib:fref dx-%data%
                                                                (i)
                                                                ((1 *))
                                                                dx-%offset%)))
                          label30))
          (go end_label)
     end_label (return (values idamax nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::idamax
                 fortran-to-lisp::*f2cl-function-info*)
        (fortran-to-lisp::make-f2cl-finfo :arg-types '((fortran-to-lisp::integer4)
                                                       (array
                                                        double-float
                                                        (*))
                                                       (fortran-to-lisp::integer4))
          :return-values '(nil nil nil)
          :calls 'nil)))


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


(defun izamax (n zx incx)
  (declare (type (array f2cl-lib:complex16 (*)) zx)
   (type (f2cl-lib:integer4) incx n))
  (f2cl-lib:with-multi-array-data ((zx f2cl-lib:complex16 zx-%data%
                                    zx-%offset%))
    (prog ((i 0) (ix 0) (smax 0.0) (izamax 0))
          (declare (type (double-float) smax)
           (type (f2cl-lib:integer4) izamax ix i))
          (setf izamax 0)
          (if (or (< n 1) (<= incx 0)) (go end_label))
          (setf izamax 1)
          (if (= n 1) (go end_label))
          (if (= incx 1) (go label20))
          (setf ix 1)
          (setf smax
                (dcabs1
                 (f2cl-lib:fref zx-%data% (1) ((1 *)) zx-%offset%)))
          (setf ix (f2cl-lib:int-add ix incx))
          (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1)) ((> i n) nil)
                        (tagbody
                            (if (<= (dcabs1
                                     (f2cl-lib:fref zx-%data% (ix)
                                                    ((1 *))
                                                    zx-%offset%))
                                    smax)
                                (go label5))
                            (setf izamax i)
                            (setf smax
                                  (dcabs1
                                   (f2cl-lib:fref zx-%data% (ix)
                                                  ((1 *))
                                                  zx-%offset%)))
                          label5
                            (setf ix (f2cl-lib:int-add ix incx))
                          label10))
          (go end_label)
     label20 (setf smax
                   (dcabs1
                    (f2cl-lib:fref zx-%data% (1) ((1 *)) zx-%offset%)))
          (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1)) ((> i n) nil)
                        (tagbody
                            (if (<= (dcabs1
                                     (f2cl-lib:fref zx-%data% (i)
                                                    ((1 *))
                                                    zx-%offset%))
                                    smax)
                                (go label30))
                            (setf izamax i)
                            (setf smax
                                  (dcabs1
                                   (f2cl-lib:fref zx-%data% (i) ((1 *))
                                                  zx-%offset%)))
                          label30))
          (go end_label)
     end_label (return (values izamax nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::izamax
                 fortran-to-lisp::*f2cl-function-info*)
        (fortran-to-lisp::make-f2cl-finfo :arg-types '((fortran-to-lisp::integer4)
                                                       (array
                                                        fortran-to-lisp::complex16
                                                        (*))
                                                       (fortran-to-lisp::integer4))
          :return-values '(nil nil nil)
          :calls '(fortran-to-lisp::dcabs1))))


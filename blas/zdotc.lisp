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


(defun zdotc (n zx incx zy incy)
  (declare (type (array f2cl-lib:complex16 (*)) zy zx)
   (type (f2cl-lib:integer4) incy incx n))
  (f2cl-lib:with-multi-array-data ((zx f2cl-lib:complex16 zx-%data%
                                    zx-%offset%)
                                   (zy f2cl-lib:complex16 zy-%data%
                                    zy-%offset%))
    (prog ((i 0) (ix 0) (iy 0) (ztemp #C(0.0 0.0)) (zdotc #C(0.0 0.0)))
          (declare (type (f2cl-lib:complex16) zdotc ztemp)
           (type (f2cl-lib:integer4) iy ix i))
          (setf ztemp (f2cl-lib:cmplx 0.0 0.0))
          (setf zdotc (f2cl-lib:cmplx 0.0 0.0))
          (if (<= n 0) (go end_label))
          (if (and (= incx 1) (= incy 1)) (go label20))
          (setf ix 1)
          (setf iy 1)
          (if (< incx 0)
              (setf ix
                    (f2cl-lib:int-add (f2cl-lib:int-mul (f2cl-lib:int-sub 1
                                                                          n)
                                                        incx)
                                      1)))
          (if (< incy 0)
              (setf iy
                    (f2cl-lib:int-add (f2cl-lib:int-mul (f2cl-lib:int-sub 1
                                                                          n)
                                                        incy)
                                      1)))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1)) ((> i n) nil)
                        (tagbody
                            (setf ztemp
                                  (+ ztemp
                                     (* (f2cl-lib:dconjg (f2cl-lib:fref zx-%data%
                                                                        (ix)
                                                                        ((1
                                                                          *))
                                                                        zx-%offset%))
                                        (f2cl-lib:fref zy-%data% (iy)
                                                       ((1 *))
                                                       zy-%offset%))))
                            (setf ix (f2cl-lib:int-add ix incx))
                            (setf iy (f2cl-lib:int-add iy incy))
                          label10))
          (setf zdotc ztemp)
          (go end_label)
     label20 (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1)) ((> i n) nil)
                           (tagbody
                               (setf ztemp
                                     (+ ztemp
                                        (* (f2cl-lib:dconjg (f2cl-lib:fref zx-%data%
                                                                           (i)
                                                                           ((1
                                                                             *))
                                                                           zx-%offset%))
                                           (f2cl-lib:fref zy-%data% (i)
                                                          ((1 *))
                                                          zy-%offset%))))
                             label30))
          (setf zdotc ztemp)
          (go end_label)
     end_label (return (values zdotc nil nil nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zdotc
                 fortran-to-lisp::*f2cl-function-info*)
        (fortran-to-lisp::make-f2cl-finfo :arg-types '((fortran-to-lisp::integer4)
                                                       (array
                                                        fortran-to-lisp::complex16
                                                        (*))
                                                       (fortran-to-lisp::integer4)
                                                       (array
                                                        fortran-to-lisp::complex16
                                                        (*))
                                                       (fortran-to-lisp::integer4))
          :return-values '(nil nil nil nil nil)
          :calls 'nil)))


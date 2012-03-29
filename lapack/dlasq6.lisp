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
  (defun dlasq6 (i0 n0 z pp dmin dmin1$ dmin2 dn dnm1 dnm2)
    (declare (type (double-float) dnm2 dnm1 dn dmin2 dmin1$ dmin)
     (type (array double-float (*)) z)
     (type (f2cl-lib:integer4) pp n0 i0))
    (f2cl-lib:with-multi-array-data ((z double-float z-%data%
                                      z-%offset%))
      (prog ((d 0.0) (emin 0.0) (safmin 0.0) (temp 0.0) (j4 0)
             (j4p2 0))
            (declare (type (double-float) d emin safmin temp)
             (type (f2cl-lib:integer4) j4 j4p2))
            (if (<= (f2cl-lib:int-sub n0 i0 1) 0) (go end_label))
            (setf safmin (dlamch "Safe minimum"))
            (setf j4
                  (f2cl-lib:int-sub (f2cl-lib:int-add (f2cl-lib:int-mul 4
                                                                        i0)
                                                      pp)
                                    3))
            (setf emin
                  (f2cl-lib:fref z-%data% ((f2cl-lib:int-add j4 4))
                                 ((1 *)) z-%offset%))
            (setf d (f2cl-lib:fref z-%data% (j4) ((1 *)) z-%offset%))
            (setf dmin d)
            (cond ((= pp 0)
                   (f2cl-lib:fdo (j4 (f2cl-lib:int-mul 4 i0)
                                  (f2cl-lib:int-add j4 4))
                                 ((> j4
                                     (f2cl-lib:int-mul 4
                                                       (f2cl-lib:int-add n0
                                                                         (f2cl-lib:int-sub 3))))
                                  nil)
                                 (tagbody
                                     (setf (f2cl-lib:fref z-%data%
                                                          ((f2cl-lib:int-sub j4
                                                                             2))
                                                          ((1 *))
                                                          z-%offset%)
                                           (+ d
                                              (f2cl-lib:fref z-%data%
                                                             ((f2cl-lib:int-sub j4
                                                                                1))
                                                             ((1 *))
                                                             z-%offset%)))
                                     (cond ((= (f2cl-lib:fref z
                                                              ((f2cl-lib:int-add j4
                                                                                 (f2cl-lib:int-sub 2)))
                                                              ((1 *)))
                                               zero)
                                            (setf (f2cl-lib:fref z-%data%
                                                                 (j4)
                                                                 ((1
                                                                   *))
                                                                 z-%offset%)
                                                  zero)
                                            (setf d
                                                  (f2cl-lib:fref z-%data%
                                                                 ((f2cl-lib:int-add j4
                                                                                    1))
                                                                 ((1
                                                                   *))
                                                                 z-%offset%))
                                            (setf dmin d)
                                            (setf emin zero))
                                           ((and (< (* safmin
                                                       (f2cl-lib:fref z
                                                                      ((f2cl-lib:int-add j4
                                                                                         1))
                                                                      ((1
                                                                        *))))
                                                    (f2cl-lib:fref z
                                                                   ((f2cl-lib:int-add j4
                                                                                      (f2cl-lib:int-sub 2)))
                                                                   ((1
                                                                     *))))
                                                 (< (* safmin
                                                       (f2cl-lib:fref z
                                                                      ((f2cl-lib:int-add j4
                                                                                         (f2cl-lib:int-sub 2)))
                                                                      ((1
                                                                        *))))
                                                    (f2cl-lib:fref z
                                                                   ((f2cl-lib:int-add j4
                                                                                      1))
                                                                   ((1
                                                                     *)))))
                                            (setf temp
                                                  (/ (f2cl-lib:fref z-%data%
                                                                    ((f2cl-lib:int-add j4
                                                                                       1))
                                                                    ((1
                                                                      *))
                                                                    z-%offset%)
                                                     (f2cl-lib:fref z-%data%
                                                                    ((f2cl-lib:int-sub j4
                                                                                       2))
                                                                    ((1
                                                                      *))
                                                                    z-%offset%)))
                                            (setf (f2cl-lib:fref z-%data%
                                                                 (j4)
                                                                 ((1
                                                                   *))
                                                                 z-%offset%)
                                                  (* (f2cl-lib:fref z-%data%
                                                                    ((f2cl-lib:int-sub j4
                                                                                       1))
                                                                    ((1
                                                                      *))
                                                                    z-%offset%)
                                                     temp))
                                            (setf d (* d temp)))
                                           (t
                                            (setf (f2cl-lib:fref z-%data%
                                                                 (j4)
                                                                 ((1
                                                                   *))
                                                                 z-%offset%)
                                                  (* (f2cl-lib:fref z-%data%
                                                                    ((f2cl-lib:int-add j4
                                                                                       1))
                                                                    ((1
                                                                      *))
                                                                    z-%offset%)
                                                     (/ (f2cl-lib:fref z-%data%
                                                                       ((f2cl-lib:int-sub j4
                                                                                          1))
                                                                       ((1
                                                                         *))
                                                                       z-%offset%)
                                                        (f2cl-lib:fref z-%data%
                                                                       ((f2cl-lib:int-sub j4
                                                                                          2))
                                                                       ((1
                                                                         *))
                                                                       z-%offset%))))
                                            (setf d
                                                  (* (f2cl-lib:fref z-%data%
                                                                    ((f2cl-lib:int-add j4
                                                                                       1))
                                                                    ((1
                                                                      *))
                                                                    z-%offset%)
                                                     (/ d
                                                        (f2cl-lib:fref z-%data%
                                                                       ((f2cl-lib:int-sub j4
                                                                                          2))
                                                                       ((1
                                                                         *))
                                                                       z-%offset%))))))
                                     (setf dmin (min dmin d))
                                     (setf emin
                                           (min emin
                                                (f2cl-lib:fref z-%data%
                                                               (j4)
                                                               ((1 *))
                                                               z-%offset%)))
                                   label10)))
                  (t
                   (f2cl-lib:fdo (j4 (f2cl-lib:int-mul 4 i0)
                                  (f2cl-lib:int-add j4 4))
                                 ((> j4
                                     (f2cl-lib:int-mul 4
                                                       (f2cl-lib:int-add n0
                                                                         (f2cl-lib:int-sub 3))))
                                  nil)
                                 (tagbody
                                     (setf (f2cl-lib:fref z-%data%
                                                          ((f2cl-lib:int-sub j4
                                                                             3))
                                                          ((1 *))
                                                          z-%offset%)
                                           (+ d
                                              (f2cl-lib:fref z-%data%
                                                             (j4)
                                                             ((1 *))
                                                             z-%offset%)))
                                     (cond ((= (f2cl-lib:fref z
                                                              ((f2cl-lib:int-add j4
                                                                                 (f2cl-lib:int-sub 3)))
                                                              ((1 *)))
                                               zero)
                                            (setf (f2cl-lib:fref z-%data%
                                                                 ((f2cl-lib:int-sub j4
                                                                                    1))
                                                                 ((1
                                                                   *))
                                                                 z-%offset%)
                                                  zero)
                                            (setf d
                                                  (f2cl-lib:fref z-%data%
                                                                 ((f2cl-lib:int-add j4
                                                                                    2))
                                                                 ((1
                                                                   *))
                                                                 z-%offset%))
                                            (setf dmin d)
                                            (setf emin zero))
                                           ((and (< (* safmin
                                                       (f2cl-lib:fref z
                                                                      ((f2cl-lib:int-add j4
                                                                                         2))
                                                                      ((1
                                                                        *))))
                                                    (f2cl-lib:fref z
                                                                   ((f2cl-lib:int-add j4
                                                                                      (f2cl-lib:int-sub 3)))
                                                                   ((1
                                                                     *))))
                                                 (< (* safmin
                                                       (f2cl-lib:fref z
                                                                      ((f2cl-lib:int-add j4
                                                                                         (f2cl-lib:int-sub 3)))
                                                                      ((1
                                                                        *))))
                                                    (f2cl-lib:fref z
                                                                   ((f2cl-lib:int-add j4
                                                                                      2))
                                                                   ((1
                                                                     *)))))
                                            (setf temp
                                                  (/ (f2cl-lib:fref z-%data%
                                                                    ((f2cl-lib:int-add j4
                                                                                       2))
                                                                    ((1
                                                                      *))
                                                                    z-%offset%)
                                                     (f2cl-lib:fref z-%data%
                                                                    ((f2cl-lib:int-sub j4
                                                                                       3))
                                                                    ((1
                                                                      *))
                                                                    z-%offset%)))
                                            (setf (f2cl-lib:fref z-%data%
                                                                 ((f2cl-lib:int-sub j4
                                                                                    1))
                                                                 ((1
                                                                   *))
                                                                 z-%offset%)
                                                  (* (f2cl-lib:fref z-%data%
                                                                    (j4)
                                                                    ((1
                                                                      *))
                                                                    z-%offset%)
                                                     temp))
                                            (setf d (* d temp)))
                                           (t
                                            (setf (f2cl-lib:fref z-%data%
                                                                 ((f2cl-lib:int-sub j4
                                                                                    1))
                                                                 ((1
                                                                   *))
                                                                 z-%offset%)
                                                  (* (f2cl-lib:fref z-%data%
                                                                    ((f2cl-lib:int-add j4
                                                                                       2))
                                                                    ((1
                                                                      *))
                                                                    z-%offset%)
                                                     (/ (f2cl-lib:fref z-%data%
                                                                       (j4)
                                                                       ((1
                                                                         *))
                                                                       z-%offset%)
                                                        (f2cl-lib:fref z-%data%
                                                                       ((f2cl-lib:int-sub j4
                                                                                          3))
                                                                       ((1
                                                                         *))
                                                                       z-%offset%))))
                                            (setf d
                                                  (* (f2cl-lib:fref z-%data%
                                                                    ((f2cl-lib:int-add j4
                                                                                       2))
                                                                    ((1
                                                                      *))
                                                                    z-%offset%)
                                                     (/ d
                                                        (f2cl-lib:fref z-%data%
                                                                       ((f2cl-lib:int-sub j4
                                                                                          3))
                                                                       ((1
                                                                         *))
                                                                       z-%offset%))))))
                                     (setf dmin (min dmin d))
                                     (setf emin
                                           (min emin
                                                (f2cl-lib:fref z-%data%
                                                               ((f2cl-lib:int-sub j4
                                                                                  1))
                                                               ((1 *))
                                                               z-%offset%)))
                                   label20))))
            (setf dnm2 d)
            (setf dmin2 dmin)
            (setf j4
                  (f2cl-lib:int-sub (f2cl-lib:int-mul 4
                                                      (f2cl-lib:int-sub n0
                                                                        2))
                                    pp))
            (setf j4p2
                  (f2cl-lib:int-sub (f2cl-lib:int-add j4
                                                      (f2cl-lib:int-mul 2
                                                                        pp))
                                    1))
            (setf (f2cl-lib:fref z-%data% ((f2cl-lib:int-sub j4 2))
                                 ((1 *)) z-%offset%)
                  (+ dnm2
                     (f2cl-lib:fref z-%data% (j4p2) ((1 *))
                                    z-%offset%)))
            (cond ((= (f2cl-lib:fref z
                                     ((f2cl-lib:int-add j4
                                                        (f2cl-lib:int-sub 2)))
                                     ((1 *)))
                      zero)
                   (setf (f2cl-lib:fref z-%data% (j4) ((1 *))
                                        z-%offset%)
                         zero)
                   (setf dnm1
                         (f2cl-lib:fref z-%data%
                                        ((f2cl-lib:int-add j4p2 2))
                                        ((1 *)) z-%offset%))
                   (setf dmin dnm1)
                   (setf emin zero))
                  ((and (< (* safmin
                              (f2cl-lib:fref z
                                             ((f2cl-lib:int-add j4p2
                                                                2))
                                             ((1 *))))
                           (f2cl-lib:fref z
                                          ((f2cl-lib:int-add j4
                                                             (f2cl-lib:int-sub 2)))
                                          ((1 *))))
                        (< (* safmin
                              (f2cl-lib:fref z
                                             ((f2cl-lib:int-add j4
                                                                (f2cl-lib:int-sub 2)))
                                             ((1 *))))
                           (f2cl-lib:fref z ((f2cl-lib:int-add j4p2 2))
                                          ((1 *)))))
                   (setf temp
                         (/ (f2cl-lib:fref z-%data%
                                           ((f2cl-lib:int-add j4p2 2))
                                           ((1 *)) z-%offset%)
                            (f2cl-lib:fref z-%data%
                                           ((f2cl-lib:int-sub j4 2))
                                           ((1 *)) z-%offset%)))
                   (setf (f2cl-lib:fref z-%data% (j4) ((1 *))
                                        z-%offset%)
                         (* (f2cl-lib:fref z-%data% (j4p2) ((1 *))
                                           z-%offset%)
                            temp))
                   (setf dnm1 (* dnm2 temp)))
                  (t
                   (setf (f2cl-lib:fref z-%data% (j4) ((1 *))
                                        z-%offset%)
                         (* (f2cl-lib:fref z-%data%
                                           ((f2cl-lib:int-add j4p2 2))
                                           ((1 *)) z-%offset%)
                            (/ (f2cl-lib:fref z-%data% (j4p2) ((1 *))
                                              z-%offset%)
                               (f2cl-lib:fref z-%data%
                                              ((f2cl-lib:int-sub j4 2))
                                              ((1 *)) z-%offset%))))
                   (setf dnm1
                         (* (f2cl-lib:fref z-%data%
                                           ((f2cl-lib:int-add j4p2 2))
                                           ((1 *)) z-%offset%)
                            (/ dnm2
                               (f2cl-lib:fref z-%data%
                                              ((f2cl-lib:int-sub j4 2))
                                              ((1 *)) z-%offset%))))))
            (setf dmin (min dmin dnm1))
            (setf dmin1$ dmin)
            (setf j4 (f2cl-lib:int-add j4 4))
            (setf j4p2
                  (f2cl-lib:int-sub (f2cl-lib:int-add j4
                                                      (f2cl-lib:int-mul 2
                                                                        pp))
                                    1))
            (setf (f2cl-lib:fref z-%data% ((f2cl-lib:int-sub j4 2))
                                 ((1 *)) z-%offset%)
                  (+ dnm1
                     (f2cl-lib:fref z-%data% (j4p2) ((1 *))
                                    z-%offset%)))
            (cond ((= (f2cl-lib:fref z
                                     ((f2cl-lib:int-add j4
                                                        (f2cl-lib:int-sub 2)))
                                     ((1 *)))
                      zero)
                   (setf (f2cl-lib:fref z-%data% (j4) ((1 *))
                                        z-%offset%)
                         zero)
                   (setf dn
                         (f2cl-lib:fref z-%data%
                                        ((f2cl-lib:int-add j4p2 2))
                                        ((1 *)) z-%offset%))
                   (setf dmin dn)
                   (setf emin zero))
                  ((and (< (* safmin
                              (f2cl-lib:fref z
                                             ((f2cl-lib:int-add j4p2
                                                                2))
                                             ((1 *))))
                           (f2cl-lib:fref z
                                          ((f2cl-lib:int-add j4
                                                             (f2cl-lib:int-sub 2)))
                                          ((1 *))))
                        (< (* safmin
                              (f2cl-lib:fref z
                                             ((f2cl-lib:int-add j4
                                                                (f2cl-lib:int-sub 2)))
                                             ((1 *))))
                           (f2cl-lib:fref z ((f2cl-lib:int-add j4p2 2))
                                          ((1 *)))))
                   (setf temp
                         (/ (f2cl-lib:fref z-%data%
                                           ((f2cl-lib:int-add j4p2 2))
                                           ((1 *)) z-%offset%)
                            (f2cl-lib:fref z-%data%
                                           ((f2cl-lib:int-sub j4 2))
                                           ((1 *)) z-%offset%)))
                   (setf (f2cl-lib:fref z-%data% (j4) ((1 *))
                                        z-%offset%)
                         (* (f2cl-lib:fref z-%data% (j4p2) ((1 *))
                                           z-%offset%)
                            temp))
                   (setf dn (* dnm1 temp)))
                  (t
                   (setf (f2cl-lib:fref z-%data% (j4) ((1 *))
                                        z-%offset%)
                         (* (f2cl-lib:fref z-%data%
                                           ((f2cl-lib:int-add j4p2 2))
                                           ((1 *)) z-%offset%)
                            (/ (f2cl-lib:fref z-%data% (j4p2) ((1 *))
                                              z-%offset%)
                               (f2cl-lib:fref z-%data%
                                              ((f2cl-lib:int-sub j4 2))
                                              ((1 *)) z-%offset%))))
                   (setf dn
                         (* (f2cl-lib:fref z-%data%
                                           ((f2cl-lib:int-add j4p2 2))
                                           ((1 *)) z-%offset%)
                            (/ dnm1
                               (f2cl-lib:fref z-%data%
                                              ((f2cl-lib:int-sub j4 2))
                                              ((1 *)) z-%offset%))))))
            (setf dmin (min dmin dn))
            (setf (f2cl-lib:fref z-%data% ((f2cl-lib:int-add j4 2))
                                 ((1 *)) z-%offset%)
                  dn)
            (setf (f2cl-lib:fref z-%data%
                                 ((f2cl-lib:int-sub (f2cl-lib:int-mul 4
                                                                      n0)
                                                    pp))
                                 ((1 *)) z-%offset%)
                  emin)
            (go end_label)
       end_label (return (values nil nil nil nil dmin dmin1$ dmin2 dn
                                 dnm1 dnm2))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlasq6
                 fortran-to-lisp::*f2cl-function-info*)
        (fortran-to-lisp::make-f2cl-finfo :arg-types '((fortran-to-lisp::integer4)
                                                       (fortran-to-lisp::integer4)
                                                       (array
                                                        double-float
                                                        (*))
                                                       (fortran-to-lisp::integer4)
                                                       (double-float)
                                                       (double-float)
                                                       (double-float)
                                                       (double-float)
                                                       (double-float)
                                                       (double-float))
          :return-values '(nil nil nil nil fortran-to-lisp::dmin
                           fortran-to-lisp::dmin1$
                           fortran-to-lisp::dmin2 fortran-to-lisp::dn
                           fortran-to-lisp::dnm1 fortran-to-lisp::dnm2)
          :calls '(fortran-to-lisp::dlamch))))


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


(let* ((one 1.0) (zero 0.0))
  (declare (type (double-float 1.0 1.0) one)
   (type (double-float 0.0 0.0) zero) (ignorable one zero))
  (defun dlarft (direct storev n k v ldv tau t$ ldt)
    (declare (type (array double-float (*)) t$ tau v)
     (type (simple-array character (*)) storev direct)
     (type (f2cl-lib:integer4) ldt ldv k n))
    (f2cl-lib:with-multi-array-data ((direct character direct-%data%
                                      direct-%offset%)
                                     (storev character storev-%data%
                                      storev-%offset%)
                                     (v double-float v-%data%
                                      v-%offset%)
                                     (tau double-float tau-%data%
                                      tau-%offset%)
                                     (t$ double-float t$-%data%
                                      t$-%offset%))
      (prog ((vii 0.0) (i 0) (j 0))
            (declare (type (double-float) vii)
             (type (f2cl-lib:integer4) i j))
            (if (= n 0) (go end_label))
            (cond ((lsame direct "F")
                   (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                 ((> i k) nil)
                                 (tagbody
                                     (cond ((= (f2cl-lib:fref tau (i)
                                                              ((1 *)))
                                               zero)
                                            (f2cl-lib:fdo (j 1
                                                           (f2cl-lib:int-add j
                                                                             1))
                                                          ((> j i) nil)
                                                          (tagbody
                                                              (setf (f2cl-lib:fref t$-%data%
                                                                                   (j
                                                                                    i)
                                                                                   ((1
                                                                                     ldt)
                                                                                    (1
                                                                                     *))
                                                                                   t$-%offset%)
                                                                    zero)
                                                            label10)))
                                           (t
                                            (setf vii
                                                  (f2cl-lib:fref v-%data%
                                                                 (i i)
                                                                 ((1
                                                                   ldv)
                                                                  (1
                                                                   *))
                                                                 v-%offset%))
                                            (setf (f2cl-lib:fref v-%data%
                                                                 (i i)
                                                                 ((1
                                                                   ldv)
                                                                  (1
                                                                   *))
                                                                 v-%offset%)
                                                  one)
                                            (cond ((lsame storev "C")
                                                   (dgemv "Transpose"
                                                    (f2cl-lib:int-add (f2cl-lib:int-sub n
                                                                                        i)
                                                                      1)
                                                    (f2cl-lib:int-sub i
                                                                      1)
                                                    (- (f2cl-lib:fref tau-%data%
                                                                      (i)
                                                                      ((1
                                                                        *))
                                                                      tau-%offset%))
                                                    (f2cl-lib:array-slice v
                                                                          double-float
                                                                          (i
                                                                           1)
                                                                          ((1
                                                                            ldv)
                                                                           (1
                                                                            *)))
                                                    ldv
                                                    (f2cl-lib:array-slice v
                                                                          double-float
                                                                          (i
                                                                           i)
                                                                          ((1
                                                                            ldv)
                                                                           (1
                                                                            *)))
                                                    1 zero
                                                    (f2cl-lib:array-slice t$
                                                                          double-float
                                                                          (1
                                                                           i)
                                                                          ((1
                                                                            ldt)
                                                                           (1
                                                                            *)))
                                                    1))
                                                  (t
                                                   (dgemv
                                                    "No transpose"
                                                    (f2cl-lib:int-sub i
                                                                      1)
                                                    (f2cl-lib:int-add (f2cl-lib:int-sub n
                                                                                        i)
                                                                      1)
                                                    (- (f2cl-lib:fref tau-%data%
                                                                      (i)
                                                                      ((1
                                                                        *))
                                                                      tau-%offset%))
                                                    (f2cl-lib:array-slice v
                                                                          double-float
                                                                          (1
                                                                           i)
                                                                          ((1
                                                                            ldv)
                                                                           (1
                                                                            *)))
                                                    ldv
                                                    (f2cl-lib:array-slice v
                                                                          double-float
                                                                          (i
                                                                           i)
                                                                          ((1
                                                                            ldv)
                                                                           (1
                                                                            *)))
                                                    ldv zero
                                                    (f2cl-lib:array-slice t$
                                                                          double-float
                                                                          (1
                                                                           i)
                                                                          ((1
                                                                            ldt)
                                                                           (1
                                                                            *)))
                                                    1)))
                                            (setf (f2cl-lib:fref v-%data%
                                                                 (i i)
                                                                 ((1
                                                                   ldv)
                                                                  (1
                                                                   *))
                                                                 v-%offset%)
                                                  vii)
                                            (dtrmv "Upper"
                                             "No transpose" "Non-unit"
                                             (f2cl-lib:int-sub i 1) t$
                                             ldt
                                             (f2cl-lib:array-slice t$
                                                                   double-float
                                                                   (1
                                                                    i)
                                                                   ((1
                                                                     ldt)
                                                                    (1
                                                                     *)))
                                             1)
                                            (setf (f2cl-lib:fref t$-%data%
                                                                 (i i)
                                                                 ((1
                                                                   ldt)
                                                                  (1
                                                                   *))
                                                                 t$-%offset%)
                                                  (f2cl-lib:fref tau-%data%
                                                                 (i)
                                                                 ((1
                                                                   *))
                                                                 tau-%offset%))))
                                   label20)))
                  (t
                   (f2cl-lib:fdo (i k
                                  (f2cl-lib:int-add i
                                                    (f2cl-lib:int-sub 1)))
                                 ((> i 1) nil)
                                 (tagbody
                                     (cond ((= (f2cl-lib:fref tau (i)
                                                              ((1 *)))
                                               zero)
                                            (f2cl-lib:fdo (j i
                                                           (f2cl-lib:int-add j
                                                                             1))
                                                          ((> j k) nil)
                                                          (tagbody
                                                              (setf (f2cl-lib:fref t$-%data%
                                                                                   (j
                                                                                    i)
                                                                                   ((1
                                                                                     ldt)
                                                                                    (1
                                                                                     *))
                                                                                   t$-%offset%)
                                                                    zero)
                                                            label30)))
                                           (t
                                            (cond ((< i k)
                                                   (cond ((lsame storev
                                                           "C")
                                                          (setf vii
                                                                (f2cl-lib:fref v-%data%
                                                                               ((f2cl-lib:int-add (f2cl-lib:int-sub n
                                                                                                                    k)
                                                                                                  i)
                                                                                i)
                                                                               ((1
                                                                                 ldv)
                                                                                (1
                                                                                 *))
                                                                               v-%offset%))
                                                          (setf (f2cl-lib:fref v-%data%
                                                                               ((f2cl-lib:int-add (f2cl-lib:int-sub n
                                                                                                                    k)
                                                                                                  i)
                                                                                i)
                                                                               ((1
                                                                                 ldv)
                                                                                (1
                                                                                 *))
                                                                               v-%offset%)
                                                                one)
                                                          (dgemv
                                                           "Transpose"
                                                           (f2cl-lib:int-add (f2cl-lib:int-sub n
                                                                                               k)
                                                                             i)
                                                           (f2cl-lib:int-sub k
                                                                             i)
                                                           (- (f2cl-lib:fref tau-%data%
                                                                             (i)
                                                                             ((1
                                                                               *))
                                                                             tau-%offset%))
                                                           (f2cl-lib:array-slice v
                                                                                 double-float
                                                                                 (1
                                                                                  (f2cl-lib:int-add i
                                                                                                    1))
                                                                                 ((1
                                                                                   ldv)
                                                                                  (1
                                                                                   *)))
                                                           ldv
                                                           (f2cl-lib:array-slice v
                                                                                 double-float
                                                                                 (1
                                                                                  i)
                                                                                 ((1
                                                                                   ldv)
                                                                                  (1
                                                                                   *)))
                                                           1 zero
                                                           (f2cl-lib:array-slice t$
                                                                                 double-float
                                                                                 ((+ i
                                                                                     1)
                                                                                  i)
                                                                                 ((1
                                                                                   ldt)
                                                                                  (1
                                                                                   *)))
                                                           1)
                                                          (setf (f2cl-lib:fref v-%data%
                                                                               ((f2cl-lib:int-add (f2cl-lib:int-sub n
                                                                                                                    k)
                                                                                                  i)
                                                                                i)
                                                                               ((1
                                                                                 ldv)
                                                                                (1
                                                                                 *))
                                                                               v-%offset%)
                                                                vii))
                                                         (t
                                                          (setf vii
                                                                (f2cl-lib:fref v-%data%
                                                                               (i
                                                                                (f2cl-lib:int-add (f2cl-lib:int-sub n
                                                                                                                    k)
                                                                                                  i))
                                                                               ((1
                                                                                 ldv)
                                                                                (1
                                                                                 *))
                                                                               v-%offset%))
                                                          (setf (f2cl-lib:fref v-%data%
                                                                               (i
                                                                                (f2cl-lib:int-add (f2cl-lib:int-sub n
                                                                                                                    k)
                                                                                                  i))
                                                                               ((1
                                                                                 ldv)
                                                                                (1
                                                                                 *))
                                                                               v-%offset%)
                                                                one)
                                                          (dgemv
                                                           "No transpose"
                                                           (f2cl-lib:int-sub k
                                                                             i)
                                                           (f2cl-lib:int-add (f2cl-lib:int-sub n
                                                                                               k)
                                                                             i)
                                                           (- (f2cl-lib:fref tau-%data%
                                                                             (i)
                                                                             ((1
                                                                               *))
                                                                             tau-%offset%))
                                                           (f2cl-lib:array-slice v
                                                                                 double-float
                                                                                 ((+ i
                                                                                     1)
                                                                                  1)
                                                                                 ((1
                                                                                   ldv)
                                                                                  (1
                                                                                   *)))
                                                           ldv
                                                           (f2cl-lib:array-slice v
                                                                                 double-float
                                                                                 (i
                                                                                  1)
                                                                                 ((1
                                                                                   ldv)
                                                                                  (1
                                                                                   *)))
                                                           ldv zero
                                                           (f2cl-lib:array-slice t$
                                                                                 double-float
                                                                                 ((+ i
                                                                                     1)
                                                                                  i)
                                                                                 ((1
                                                                                   ldt)
                                                                                  (1
                                                                                   *)))
                                                           1)
                                                          (setf (f2cl-lib:fref v-%data%
                                                                               (i
                                                                                (f2cl-lib:int-add (f2cl-lib:int-sub n
                                                                                                                    k)
                                                                                                  i))
                                                                               ((1
                                                                                 ldv)
                                                                                (1
                                                                                 *))
                                                                               v-%offset%)
                                                                vii)))
                                                   (dtrmv "Lower"
                                                    "No transpose"
                                                    "Non-unit"
                                                    (f2cl-lib:int-sub k
                                                                      i)
                                                    (f2cl-lib:array-slice t$
                                                                          double-float
                                                                          ((+ i
                                                                              1)
                                                                           (f2cl-lib:int-add i
                                                                                             1))
                                                                          ((1
                                                                            ldt)
                                                                           (1
                                                                            *)))
                                                    ldt
                                                    (f2cl-lib:array-slice t$
                                                                          double-float
                                                                          ((+ i
                                                                              1)
                                                                           i)
                                                                          ((1
                                                                            ldt)
                                                                           (1
                                                                            *)))
                                                    1)))
                                            (setf (f2cl-lib:fref t$-%data%
                                                                 (i i)
                                                                 ((1
                                                                   ldt)
                                                                  (1
                                                                   *))
                                                                 t$-%offset%)
                                                  (f2cl-lib:fref tau-%data%
                                                                 (i)
                                                                 ((1
                                                                   *))
                                                                 tau-%offset%))))
                                   label40))))
            (go end_label)
       end_label (return (values nil nil nil nil nil nil nil nil
                                 nil))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlarft
                 fortran-to-lisp::*f2cl-function-info*)
        (fortran-to-lisp::make-f2cl-finfo :arg-types '((simple-array
                                                        character
                                                        (1))
                                                       (simple-array
                                                        character
                                                        (1))
                                                       (fortran-to-lisp::integer4)
                                                       (fortran-to-lisp::integer4)
                                                       (array
                                                        double-float
                                                        (*))
                                                       (fortran-to-lisp::integer4)
                                                       (array
                                                        double-float
                                                        (*))
                                                       (array
                                                        double-float
                                                        (*))
                                                       (fortran-to-lisp::integer4))
          :return-values '(nil nil nil nil nil nil nil nil nil)
          :calls '(fortran-to-lisp::dtrmv fortran-to-lisp::dgemv
                   fortran-to-lisp::lsame))))


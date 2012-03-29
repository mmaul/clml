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
  (defun dlahrd (n k nb a lda tau t$ ldt y ldy)
    (declare (type (array double-float (*)) y t$ tau a)
     (type (f2cl-lib:integer4) ldy ldt lda nb k n))
    (f2cl-lib:with-multi-array-data ((a double-float a-%data%
                                      a-%offset%)
                                     (tau double-float tau-%data%
                                      tau-%offset%)
                                     (t$ double-float t$-%data%
                                      t$-%offset%)
                                     (y double-float y-%data%
                                      y-%offset%))
      (prog ((ei 0.0) (i 0))
            (declare (type (double-float) ei)
             (type (f2cl-lib:integer4) i))
            (if (<= n 1) (go end_label))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1)) ((> i nb) nil)
                          (tagbody
                              (cond ((> i 1)
                                     (dgemv "No transpose" n
                                      (f2cl-lib:int-sub i 1) (- one) y
                                      ldy
                                      (f2cl-lib:array-slice a
                                                            double-float
                                                            ((+ k i
                                                                (f2cl-lib:int-sub 1))
                                                             1)
                                                            ((1 lda)
                                                             (1 *)))
                                      lda one
                                      (f2cl-lib:array-slice a
                                                            double-float
                                                            (1 i)
                                                            ((1 lda)
                                                             (1 *)))
                                      1)
                                     (dcopy (f2cl-lib:int-sub i 1)
                                      (f2cl-lib:array-slice a
                                                            double-float
                                                            ((+ k 1) i)
                                                            ((1 lda)
                                                             (1 *)))
                                      1
                                      (f2cl-lib:array-slice t$
                                                            double-float
                                                            (1 nb)
                                                            ((1 ldt)
                                                             (1 nb)))
                                      1)
                                     (dtrmv "Lower" "Transpose" "Unit"
                                      (f2cl-lib:int-sub i 1)
                                      (f2cl-lib:array-slice a
                                                            double-float
                                                            ((+ k 1) 1)
                                                            ((1 lda)
                                                             (1 *)))
                                      lda
                                      (f2cl-lib:array-slice t$
                                                            double-float
                                                            (1 nb)
                                                            ((1 ldt)
                                                             (1 nb)))
                                      1)
                                     (dgemv "Transpose"
                                      (f2cl-lib:int-add (f2cl-lib:int-sub n
                                                                          k
                                                                          i)
                                                        1)
                                      (f2cl-lib:int-sub i 1) one
                                      (f2cl-lib:array-slice a
                                                            double-float
                                                            ((+ k i) 1)
                                                            ((1 lda)
                                                             (1 *)))
                                      lda
                                      (f2cl-lib:array-slice a
                                                            double-float
                                                            ((+ k i) i)
                                                            ((1 lda)
                                                             (1 *)))
                                      1 one
                                      (f2cl-lib:array-slice t$
                                                            double-float
                                                            (1 nb)
                                                            ((1 ldt)
                                                             (1 nb)))
                                      1)
                                     (dtrmv "Upper" "Transpose"
                                      "Non-unit" (f2cl-lib:int-sub i 1)
                                      t$ ldt
                                      (f2cl-lib:array-slice t$
                                                            double-float
                                                            (1 nb)
                                                            ((1 ldt)
                                                             (1 nb)))
                                      1)
                                     (dgemv "No transpose"
                                      (f2cl-lib:int-add (f2cl-lib:int-sub n
                                                                          k
                                                                          i)
                                                        1)
                                      (f2cl-lib:int-sub i 1) (- one)
                                      (f2cl-lib:array-slice a
                                                            double-float
                                                            ((+ k i) 1)
                                                            ((1 lda)
                                                             (1 *)))
                                      lda
                                      (f2cl-lib:array-slice t$
                                                            double-float
                                                            (1 nb)
                                                            ((1 ldt)
                                                             (1 nb)))
                                      1 one
                                      (f2cl-lib:array-slice a
                                                            double-float
                                                            ((+ k i) i)
                                                            ((1 lda)
                                                             (1 *)))
                                      1)
                                     (dtrmv "Lower" "No transpose"
                                      "Unit" (f2cl-lib:int-sub i 1)
                                      (f2cl-lib:array-slice a
                                                            double-float
                                                            ((+ k 1) 1)
                                                            ((1 lda)
                                                             (1 *)))
                                      lda
                                      (f2cl-lib:array-slice t$
                                                            double-float
                                                            (1 nb)
                                                            ((1 ldt)
                                                             (1 nb)))
                                      1)
                                     (daxpy (f2cl-lib:int-sub i 1)
                                      (- one)
                                      (f2cl-lib:array-slice t$
                                                            double-float
                                                            (1 nb)
                                                            ((1 ldt)
                                                             (1 nb)))
                                      1
                                      (f2cl-lib:array-slice a
                                                            double-float
                                                            ((+ k 1) i)
                                                            ((1 lda)
                                                             (1 *)))
                                      1)
                                     (setf (f2cl-lib:fref a-%data%
                                                          ((f2cl-lib:int-sub (f2cl-lib:int-add k
                                                                                               i)
                                                                             1)
                                                           (f2cl-lib:int-sub i
                                                                             1))
                                                          ((1 lda)
                                                           (1 *))
                                                          a-%offset%)
                                           ei)))
                              (multiple-value-bind (var-0 var-1 var-2
                                                    var-3 var-4)
                                  (dlarfg
                                   (f2cl-lib:int-add (f2cl-lib:int-sub n
                                                                       k
                                                                       i)
                                                     1)
                                   (f2cl-lib:fref a-%data%
                                                  ((f2cl-lib:int-add k
                                                                     i)
                                                   i)
                                                  ((1 lda) (1 *))
                                                  a-%offset%)
                                   (f2cl-lib:array-slice a double-float
                                                         ((min (f2cl-lib:int-add k
                                                                                 i
                                                                                 1)
                                                               n)
                                                          i)
                                                         ((1 lda)
                                                          (1 *)))
                                   1
                                   (f2cl-lib:fref tau-%data% (i)
                                                  ((1 nb))
                                                  tau-%offset%))
                                (declare (ignore var-0 var-2 var-3))
                                (setf (f2cl-lib:fref a-%data%
                                                     ((f2cl-lib:int-add k
                                                                        i)
                                                      i)
                                                     ((1 lda) (1 *))
                                                     a-%offset%)
                                      var-1)
                                (setf (f2cl-lib:fref tau-%data% (i)
                                                     ((1 nb))
                                                     tau-%offset%)
                                      var-4))
                              (setf ei
                                    (f2cl-lib:fref a-%data%
                                                   ((f2cl-lib:int-add k
                                                                      i)
                                                    i)
                                                   ((1 lda) (1 *))
                                                   a-%offset%))
                              (setf (f2cl-lib:fref a-%data%
                                                   ((f2cl-lib:int-add k
                                                                      i)
                                                    i)
                                                   ((1 lda) (1 *))
                                                   a-%offset%)
                                    one)
                              (dgemv "No transpose" n
                               (f2cl-lib:int-add (f2cl-lib:int-sub n k
                                                                   i)
                                                 1)
                               one
                               (f2cl-lib:array-slice a double-float
                                                     (1
                                                      (f2cl-lib:int-add i
                                                                        1))
                                                     ((1 lda) (1 *)))
                               lda
                               (f2cl-lib:array-slice a double-float
                                                     ((+ k i) i)
                                                     ((1 lda) (1 *)))
                               1 zero
                               (f2cl-lib:array-slice y double-float
                                                     (1 i)
                                                     ((1 ldy) (1 nb)))
                               1)
                              (dgemv "Transpose"
                               (f2cl-lib:int-add (f2cl-lib:int-sub n k
                                                                   i)
                                                 1)
                               (f2cl-lib:int-sub i 1) one
                               (f2cl-lib:array-slice a double-float
                                                     ((+ k i) 1)
                                                     ((1 lda) (1 *)))
                               lda
                               (f2cl-lib:array-slice a double-float
                                                     ((+ k i) i)
                                                     ((1 lda) (1 *)))
                               1 zero
                               (f2cl-lib:array-slice t$ double-float
                                                     (1 i)
                                                     ((1 ldt) (1 nb)))
                               1)
                              (dgemv "No transpose" n
                               (f2cl-lib:int-sub i 1) (- one) y ldy
                               (f2cl-lib:array-slice t$ double-float
                                                     (1 i)
                                                     ((1 ldt) (1 nb)))
                               1 one
                               (f2cl-lib:array-slice y double-float
                                                     (1 i)
                                                     ((1 ldy) (1 nb)))
                               1)
                              (dscal n
                               (f2cl-lib:fref tau-%data% (i) ((1 nb))
                                              tau-%offset%)
                               (f2cl-lib:array-slice y double-float
                                                     (1 i)
                                                     ((1 ldy) (1 nb)))
                               1)
                              (dscal (f2cl-lib:int-sub i 1)
                               (- (f2cl-lib:fref tau-%data% (i)
                                                 ((1 nb))
                                                 tau-%offset%))
                               (f2cl-lib:array-slice t$ double-float
                                                     (1 i)
                                                     ((1 ldt) (1 nb)))
                               1)
                              (dtrmv "Upper" "No transpose" "Non-unit"
                               (f2cl-lib:int-sub i 1) t$ ldt
                               (f2cl-lib:array-slice t$ double-float
                                                     (1 i)
                                                     ((1 ldt) (1 nb)))
                               1)
                              (setf (f2cl-lib:fref t$-%data% (i i)
                                                   ((1 ldt) (1 nb))
                                                   t$-%offset%)
                                    (f2cl-lib:fref tau-%data% (i)
                                                   ((1 nb))
                                                   tau-%offset%))
                            label10))
            (setf (f2cl-lib:fref a-%data% ((f2cl-lib:int-add k nb) nb)
                                 ((1 lda) (1 *)) a-%offset%)
                  ei)
            (go end_label)
       end_label (return (values nil nil nil nil nil nil nil nil nil
                                 nil))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlahrd
                 fortran-to-lisp::*f2cl-function-info*)
        (fortran-to-lisp::make-f2cl-finfo :arg-types '((fortran-to-lisp::integer4)
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
                                                       (fortran-to-lisp::integer4)
                                                       (array
                                                        double-float
                                                        (*))
                                                       (fortran-to-lisp::integer4))
          :return-values '(nil nil nil nil nil nil nil nil nil nil)
          :calls '(fortran-to-lisp::dscal fortran-to-lisp::dlarfg
                   fortran-to-lisp::daxpy fortran-to-lisp::dtrmv
                   fortran-to-lisp::dcopy fortran-to-lisp::dgemv))))


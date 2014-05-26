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


(let* ((one 1.0))
  (declare (type (double-float 1.0 1.0) one) (ignorable one))
  (defun dgebak (job side n ilo ihi scale m v ldv info)
    (declare (type (array double-float (*)) v scale)
     (type (simple-array character (*)) side job)
     (type (f2cl-lib:integer4) info ldv m ihi ilo n))
    (f2cl-lib:with-multi-array-data ((job character job-%data%
                                      job-%offset%)
                                     (side character side-%data%
                                      side-%offset%)
                                     (scale double-float scale-%data%
                                      scale-%offset%)
                                     (v double-float v-%data%
                                      v-%offset%))
      (prog ((s 0.0) (i 0) (ii 0) (k 0) (leftv nil) (rightv nil))
            (declare (type (double-float) s)
             (type f2cl-lib:logical leftv rightv)
             (type (f2cl-lib:integer4) i ii k))
            (setf rightv (lsame side "R"))
            (setf leftv (lsame side "L"))
            (setf info 0)
            (cond ((and (not (lsame job "N")) (not (lsame job "P"))
                        (not (lsame job "S")) (not (lsame job "B")))
                   (setf info -1))
                  ((and (not rightv) (not leftv)) (setf info -2))
                  ((< n 0) (setf info -3))
                  ((or (< ilo 1)
                       (> ilo
                          (max (the f2cl-lib:integer4 1)
                               (the f2cl-lib:integer4 n))))
                   (setf info -4))
                  ((or (< ihi
                          (min (the f2cl-lib:integer4 ilo)
                               (the f2cl-lib:integer4 n)))
                       (> ihi n))
                   (setf info -5))
                  ((< m 0) (setf info -7))
                  ((< ldv
                      (max (the f2cl-lib:integer4 1)
                           (the f2cl-lib:integer4 n)))
                   (setf info -9)))
            (cond ((/= info 0)
                   (xerbla "DGEBAK" (f2cl-lib:int-sub info))
                   (go end_label)))
            (if (= n 0) (go end_label))
            (if (= m 0) (go end_label))
            (if (lsame job "N") (go end_label))
            (if (= ilo ihi) (go label30))
            (cond ((or (lsame job "S") (lsame job "B"))
                   (cond (rightv
                          (f2cl-lib:fdo (i ilo (f2cl-lib:int-add i 1))
                                        ((> i ihi) nil)
                                        (tagbody
                                            (setf s
                                                  (f2cl-lib:fref scale-%data%
                                                                 (i)
                                                                 ((1
                                                                   *))
                                                                 scale-%offset%))
                                            (dscal m s
                                             (f2cl-lib:array-slice v
                                                                   double-float
                                                                   (i
                                                                    1)
                                                                   ((1
                                                                     ldv)
                                                                    (1
                                                                     *)))
                                             ldv)
                                          label10))))
                   (cond (leftv
                          (f2cl-lib:fdo (i ilo (f2cl-lib:int-add i 1))
                                        ((> i ihi) nil)
                                        (tagbody
                                            (setf s
                                                  (/ one
                                                     (f2cl-lib:fref scale-%data%
                                                                    (i)
                                                                    ((1
                                                                      *))
                                                                    scale-%offset%)))
                                            (dscal m s
                                             (f2cl-lib:array-slice v
                                                                   double-float
                                                                   (i
                                                                    1)
                                                                   ((1
                                                                     ldv)
                                                                    (1
                                                                     *)))
                                             ldv)
                                          label20))))))
       label30 (cond ((or (lsame job "P") (lsame job "B"))
                      (cond (rightv
                             (f2cl-lib:fdo (ii 1
                                            (f2cl-lib:int-add ii 1))
                                           ((> ii n) nil)
                                           (tagbody
                                               (setf i ii)
                                               (if (and (>= i ilo)
                                                        (<= i ihi))
                                                   (go label40))
                                               (if (< i ilo)
                                                   (setf i
                                                         (f2cl-lib:int-sub ilo
                                                                           ii)))
                                               (setf k
                                                     (f2cl-lib:int (f2cl-lib:fref scale-%data%
                                                                                  (i)
                                                                                  ((1
                                                                                    *))
                                                                                  scale-%offset%)))
                                               (if (= k i)
                                                   (go label40))
                                               (dswap m
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
                                                                      (k
                                                                       1)
                                                                      ((1
                                                                        ldv)
                                                                       (1
                                                                        *)))
                                                ldv)
                                             label40))))
                      (cond (leftv
                             (f2cl-lib:fdo (ii 1
                                            (f2cl-lib:int-add ii 1))
                                           ((> ii n) nil)
                                           (tagbody
                                               (setf i ii)
                                               (if (and (>= i ilo)
                                                        (<= i ihi))
                                                   (go label50))
                                               (if (< i ilo)
                                                   (setf i
                                                         (f2cl-lib:int-sub ilo
                                                                           ii)))
                                               (setf k
                                                     (f2cl-lib:int (f2cl-lib:fref scale-%data%
                                                                                  (i)
                                                                                  ((1
                                                                                    *))
                                                                                  scale-%offset%)))
                                               (if (= k i)
                                                   (go label50))
                                               (dswap m
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
                                                                      (k
                                                                       1)
                                                                      ((1
                                                                        ldv)
                                                                       (1
                                                                        *)))
                                                ldv)
                                             label50))))))
            (go end_label)
       end_label (return (values nil nil nil nil nil nil nil nil nil
                                 info))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dgebak
                 fortran-to-lisp::*f2cl-function-info*)
        (fortran-to-lisp::make-f2cl-finfo :arg-types '((simple-array
                                                        character
                                                        (1))
                                                       (simple-array
                                                        character
                                                        (1))
                                                       (fortran-to-lisp::integer4)
                                                       (fortran-to-lisp::integer4)
                                                       (fortran-to-lisp::integer4)
                                                       (array
                                                        double-float
                                                        (*))
                                                       (fortran-to-lisp::integer4)
                                                       (array
                                                        double-float
                                                        (*))
                                                       (fortran-to-lisp::integer4)
                                                       (fortran-to-lisp::integer4))
          :return-values '(nil nil nil nil nil nil nil nil nil
                           fortran-to-lisp::info)
          :calls '(fortran-to-lisp::dswap fortran-to-lisp::dscal
                   fortran-to-lisp::xerbla fortran-to-lisp::lsame))))


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
  (defun dorg2r (m n k a lda tau work info)
    (declare (type (array double-float (*)) work tau a)
     (type (f2cl-lib:integer4) info lda k n m))
    (f2cl-lib:with-multi-array-data ((a double-float a-%data%
                                      a-%offset%)
                                     (tau double-float tau-%data%
                                      tau-%offset%)
                                     (work double-float work-%data%
                                      work-%offset%))
      (prog ((i 0) (j 0) (l 0))
            (declare (type (f2cl-lib:integer4) i j l))
            (setf info 0)
            (cond ((< m 0) (setf info -1))
                  ((or (< n 0) (> n m)) (setf info -2))
                  ((or (< k 0) (> k n)) (setf info -3))
                  ((< lda
                      (max (the f2cl-lib:integer4 1)
                           (the f2cl-lib:integer4 m)))
                   (setf info -5)))
            (cond ((/= info 0)
                   (xerbla "DORG2R" (f2cl-lib:int-sub info))
                   (go end_label)))
            (if (<= n 0) (go end_label))
            (f2cl-lib:fdo (j (f2cl-lib:int-add k 1)
                           (f2cl-lib:int-add j 1))
                          ((> j n) nil)
                          (tagbody
                              (f2cl-lib:fdo (l 1
                                             (f2cl-lib:int-add l 1))
                                            ((> l m) nil)
                                            (tagbody
                                                (setf (f2cl-lib:fref a-%data%
                                                                     (l
                                                                      j)
                                                                     ((1
                                                                       lda)
                                                                      (1
                                                                       *))
                                                                     a-%offset%)
                                                      zero)
                                              label10))
                              (setf (f2cl-lib:fref a-%data% (j j)
                                                   ((1 lda) (1 *))
                                                   a-%offset%)
                                    one)
                            label20))
            (f2cl-lib:fdo (i k
                           (f2cl-lib:int-add i (f2cl-lib:int-sub 1)))
                          ((> i 1) nil)
                          (tagbody
                              (cond ((< i n)
                                     (setf (f2cl-lib:fref a-%data%
                                                          (i i)
                                                          ((1 lda)
                                                           (1 *))
                                                          a-%offset%)
                                           one)
                                     (dlarf "Left"
                                      (f2cl-lib:int-add (f2cl-lib:int-sub m
                                                                          i)
                                                        1)
                                      (f2cl-lib:int-sub n i)
                                      (f2cl-lib:array-slice a
                                                            double-float
                                                            (i i)
                                                            ((1 lda)
                                                             (1 *)))
                                      1
                                      (f2cl-lib:fref tau-%data% (i)
                                                     ((1 *))
                                                     tau-%offset%)
                                      (f2cl-lib:array-slice a
                                                            double-float
                                                            (i
                                                             (f2cl-lib:int-add i
                                                                               1))
                                                            ((1 lda)
                                                             (1 *)))
                                      lda work)))
                              (if (< i m)
                                  (dscal (f2cl-lib:int-sub m i)
                                   (- (f2cl-lib:fref tau-%data% (i)
                                                     ((1 *))
                                                     tau-%offset%))
                                   (f2cl-lib:array-slice a double-float
                                                         ((+ i 1) i)
                                                         ((1 lda)
                                                          (1 *)))
                                   1))
                              (setf (f2cl-lib:fref a-%data% (i i)
                                                   ((1 lda) (1 *))
                                                   a-%offset%)
                                    (- one
                                       (f2cl-lib:fref tau-%data% (i)
                                                      ((1 *))
                                                      tau-%offset%)))
                              (f2cl-lib:fdo (l 1
                                             (f2cl-lib:int-add l 1))
                                            ((> l
                                                (f2cl-lib:int-add i
                                                                  (f2cl-lib:int-sub 1)))
                                             nil)
                                            (tagbody
                                                (setf (f2cl-lib:fref a-%data%
                                                                     (l
                                                                      i)
                                                                     ((1
                                                                       lda)
                                                                      (1
                                                                       *))
                                                                     a-%offset%)
                                                      zero)
                                              label30))
                            label40))
            (go end_label)
       end_label (return (values nil nil nil nil nil nil nil info))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dorg2r
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
                                                       (fortran-to-lisp::integer4))
          :return-values '(nil nil nil nil nil nil nil
                           fortran-to-lisp::info)
          :calls '(fortran-to-lisp::dscal fortran-to-lisp::dlarf
                   fortran-to-lisp::xerbla))))


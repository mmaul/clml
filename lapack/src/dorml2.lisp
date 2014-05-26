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
  (defun dorml2 (side trans m n k a lda tau c ldc work info)
    (declare (type (array double-float (*)) work c tau a)
     (type (simple-array character (*)) trans side)
     (type (f2cl-lib:integer4) info ldc lda k n m))
    (f2cl-lib:with-multi-array-data ((side character side-%data%
                                      side-%offset%)
                                     (trans character trans-%data%
                                      trans-%offset%)
                                     (a double-float a-%data%
                                      a-%offset%)
                                     (tau double-float tau-%data%
                                      tau-%offset%)
                                     (c double-float c-%data%
                                      c-%offset%)
                                     (work double-float work-%data%
                                      work-%offset%))
      (prog ((aii 0.0) (i 0) (i1 0) (i2 0) (i3 0) (ic 0) (jc 0) (mi 0)
             (ni 0) (nq 0) (left nil) (notran nil))
            (declare (type (double-float) aii)
             (type f2cl-lib:logical left notran)
             (type (f2cl-lib:integer4) i i1 i2 i3 ic jc mi ni nq))
            (setf info 0)
            (setf left (lsame side "L"))
            (setf notran (lsame trans "N"))
            (cond (left (setf nq m)) (t (setf nq n)))
            (cond ((and (not left) (not (lsame side "R")))
                   (setf info -1))
                  ((and (not notran) (not (lsame trans "T")))
                   (setf info -2))
                  ((< m 0) (setf info -3))
                  ((< n 0) (setf info -4))
                  ((or (< k 0) (> k nq)) (setf info -5))
                  ((< lda
                      (max (the f2cl-lib:integer4 1)
                           (the f2cl-lib:integer4 k)))
                   (setf info -7))
                  ((< ldc
                      (max (the f2cl-lib:integer4 1)
                           (the f2cl-lib:integer4 m)))
                   (setf info -10)))
            (cond ((/= info 0)
                   (xerbla "DORML2" (f2cl-lib:int-sub info))
                   (go end_label)))
            (if (or (= m 0) (= n 0) (= k 0)) (go end_label))
            (cond ((or (and left notran) (and (not left) (not notran)))
                   (setf i1 1)
                   (setf i2 k)
                   (setf i3 1))
                  (t (setf i1 k) (setf i2 1) (setf i3 -1)))
            (cond (left (setf ni n) (setf jc 1))
                  (t (setf mi m) (setf ic 1)))
            (f2cl-lib:fdo (i i1 (f2cl-lib:int-add i i3)) ((> i i2) nil)
                          (tagbody
                              (cond (left
                                     (setf mi
                                           (f2cl-lib:int-add (f2cl-lib:int-sub m
                                                                               i)
                                                             1))
                                     (setf ic i))
                                    (t
                                     (setf ni
                                           (f2cl-lib:int-add (f2cl-lib:int-sub n
                                                                               i)
                                                             1))
                                     (setf jc i)))
                              (setf aii
                                    (f2cl-lib:fref a-%data% (i i)
                                                   ((1 lda) (1 *))
                                                   a-%offset%))
                              (setf (f2cl-lib:fref a-%data% (i i)
                                                   ((1 lda) (1 *))
                                                   a-%offset%)
                                    one)
                              (dlarf side mi ni
                               (f2cl-lib:array-slice a double-float
                                                     (i i)
                                                     ((1 lda) (1 *)))
                               lda
                               (f2cl-lib:fref tau-%data% (i) ((1 *))
                                              tau-%offset%)
                               (f2cl-lib:array-slice c double-float
                                                     (ic jc)
                                                     ((1 ldc) (1 *)))
                               ldc work)
                              (setf (f2cl-lib:fref a-%data% (i i)
                                                   ((1 lda) (1 *))
                                                   a-%offset%)
                                    aii)
                            label10))
            (go end_label)
       end_label (return (values nil nil nil nil nil nil nil nil nil
                                 nil nil info))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dorml2
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
                                                       (array
                                                        double-float
                                                        (*))
                                                       (fortran-to-lisp::integer4)
                                                       (array
                                                        double-float
                                                        (*))
                                                       (fortran-to-lisp::integer4))
          :return-values '(nil nil nil nil nil nil nil nil nil nil nil
                           fortran-to-lisp::info)
          :calls '(fortran-to-lisp::dlarf fortran-to-lisp::xerbla
                   fortran-to-lisp::lsame))))


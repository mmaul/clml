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


(defun dgesv (n nrhs a lda ipiv b ldb$ info)
  (declare (type (array double-float (*)) b a)
   (type (array f2cl-lib:integer4 (*)) ipiv)
   (type (f2cl-lib:integer4) info ldb$ lda nrhs n))
  (f2cl-lib:with-multi-array-data ((ipiv f2cl-lib:integer4 ipiv-%data%
                                    ipiv-%offset%)
                                   (a double-float a-%data% a-%offset%)
                                   (b double-float b-%data%
                                    b-%offset%))
    (prog ()
          (declare)
          (setf info 0)
          (cond ((< n 0) (setf info -1))
                ((< nrhs 0) (setf info -2))
                ((< lda
                    (max (the f2cl-lib:integer4 1)
                         (the f2cl-lib:integer4 n)))
                 (setf info -4))
                ((< ldb$
                    (max (the f2cl-lib:integer4 1)
                         (the f2cl-lib:integer4 n)))
                 (setf info -7)))
          (cond ((/= info 0)
                 (xerbla "DGESV " (f2cl-lib:int-sub info))
                 (go end_label)))
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
              (dgetrf n n a lda ipiv info)
            (declare (ignore var-0 var-1 var-2 var-3 var-4))
            (setf info var-5))
          (cond ((= info 0)
                 (multiple-value-bind (var-0 var-1 var-2 var-3 var-4
                                       var-5 var-6 var-7 var-8)
                     (dgetrs "No transpose" n nrhs a lda ipiv b ldb$
                      info)
                   (declare
                    (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                     var-7))
                   (setf info var-8))))
          (go end_label)
     end_label (return (values nil nil nil nil nil nil nil info)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dgesv
                 fortran-to-lisp::*f2cl-function-info*)
        (fortran-to-lisp::make-f2cl-finfo :arg-types '((fortran-to-lisp::integer4)
                                                       (fortran-to-lisp::integer4)
                                                       (array
                                                        double-float
                                                        (*))
                                                       (fortran-to-lisp::integer4)
                                                       (array
                                                        fortran-to-lisp::integer4
                                                        (*))
                                                       (array
                                                        double-float
                                                        (*))
                                                       (fortran-to-lisp::integer4)
                                                       (fortran-to-lisp::integer4))
          :return-values '(nil nil nil nil nil nil nil
                           fortran-to-lisp::info)
          :calls '(fortran-to-lisp::dgetrs fortran-to-lisp::dgetrf
                   fortran-to-lisp::xerbla))))


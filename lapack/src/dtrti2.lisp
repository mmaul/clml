;;; Compiled by f2cl version:
;;; ("$Id: f2cl1.l,v 1.193 2008-02-22 22:37:02 rtoy Exp $"
;;;  "$Id: f2cl2.l,v 1.37 2008-02-22 22:19:33 rtoy Exp $"
;;;  "$Id: f2cl3.l,v 1.6 2008-02-22 22:19:33 rtoy Exp $"
;;;  "$Id: f2cl4.l,v 1.7 2008-02-22 22:19:34 rtoy Exp $"
;;;  "$Id: f2cl5.l,v 1.181 2008-02-22 22:52:33 rtoy Exp $"
;;;  "$Id: f2cl6.l,v 1.45 2008-02-22 22:19:34 rtoy Exp $"
;;;  "$Id: macros.l,v 1.96 2008-02-22 22:19:34 rtoy Exp $")

;;; Using Lisp International Allegro CL Enterprise Edition 8.1 [Windows] (Jun 26, 2009 10:35)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package "LAPACK")


(let* ((one 1.0))
  (declare (type (double-float 1.0 1.0) one))
  (defun dtrti2 (uplo diag n a lda info)
    (declare (type (f2cl-lib:integer4) info lda n)
     (type (simple-array character (*)) diag uplo)
     (type (array double-float (*)) a))
    (f2cl-lib:with-multi-array-data ((a double-float a-%data% a-%offset%)
                                     (uplo character uplo-%data%
                                      uplo-%offset%)
                                     (diag character diag-%data%
                                      diag-%offset%))
      (prog ((ajj 0.0) (j 0) (nounit nil) (upper nil))
            (declare (type (f2cl-lib:integer4) j)
             (type (double-float) ajj)
             (type f2cl-lib:logical nounit upper))
            (setf info 0)
            (setf upper
                  (multiple-value-bind (ret-val var-0 var-1)
                      (lsame uplo "U")
                    (declare (ignore var-1))
                    (when var-0 (setf uplo var-0))
                    ret-val))
            (setf nounit
                  (multiple-value-bind (ret-val var-0 var-1)
                      (lsame diag "N")
                    (declare (ignore var-1))
                    (when var-0 (setf diag var-0))
                    ret-val))
            (cond ((and (not upper)
                        (not (multiple-value-bind (ret-val var-0 var-1)
                                 (lsame uplo "L")
                               (declare (ignore var-1))
                               (when var-0 (setf uplo var-0))
                               ret-val)))
                   (setf info -1))
                  ((and (not nounit)
                        (not (multiple-value-bind (ret-val var-0 var-1)
                                 (lsame diag "U")
                               (declare (ignore var-1))
                               (when var-0 (setf diag var-0))
                               ret-val)))
                   (setf info -2))
                  ((< n 0) (setf info -3))
                  ((< lda
                      (max (the f2cl-lib:integer4 1)
                           (the f2cl-lib:integer4 n)))
                   (setf info -5)))
            (cond ((/= info 0)
                   (xerbla "DTRTI2" (f2cl-lib:int-sub info))
                   (go end_label)))
            (cond (upper
                   (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                                 ((> j n) nil)
                                 (tagbody
                                     (cond (nounit
                                            (setf (f2cl-lib:fref a-%data%
                                                                 (j j)
                                                                 ((1 lda)
                                                                  (1 *))
                                                                 a-%offset%)
                                                  (/ one
                                                     (f2cl-lib:fref a-%data%
                                                                    (j j)
                                                                    ((1
                                                                      lda)
                                                                     (1
                                                                      *))
                                                                    a-%offset%)))
                                            (setf ajj
                                                  (- (f2cl-lib:fref a-%data%
                                                                    (j j)
                                                                    ((1
                                                                      lda)
                                                                     (1
                                                                      *))
                                                                    a-%offset%))))
                                           (t (setf ajj (- one))))
                                     (multiple-value-bind (var-0 var-1
                                                           var-2 var-3
                                                           var-4 var-5
                                                           var-6 var-7)
                                         (dtrmv "Upper" "No transpose"
                                          diag (f2cl-lib:int-sub j 1) a
                                          lda
                                          (f2cl-lib:array-slice a
                                                                double-float
                                                                (1 j)
                                                                ((1 lda)
                                                                 (1 *)))
                                          1)
                                       (declare
                                        (ignore var-0 var-1 var-3 var-4
                                         var-6 var-7))
                                       (when var-2 (setf diag var-2))
                                       (when var-5 (setf lda var-5)))
                                     (multiple-value-bind (var-0 var-1
                                                           var-2 var-3)
                                         (dscal (f2cl-lib:int-sub j 1) ajj
                                          (f2cl-lib:array-slice a
                                                                double-float
                                                                (1 j)
                                                                ((1 lda)
                                                                 (1 *)))
                                          1)
                                       (declare
                                        (ignore var-0 var-2 var-3))
                                       (when var-1 (setf ajj var-1)))
                                   label10)))
                  (t
                   (f2cl-lib:fdo (j n
                                  (f2cl-lib:int-add j
                                                    (f2cl-lib:int-sub 1)))
                                 ((> j 1) nil)
                                 (tagbody
                                     (cond (nounit
                                            (setf (f2cl-lib:fref a-%data%
                                                                 (j j)
                                                                 ((1 lda)
                                                                  (1 *))
                                                                 a-%offset%)
                                                  (/ one
                                                     (f2cl-lib:fref a-%data%
                                                                    (j j)
                                                                    ((1
                                                                      lda)
                                                                     (1
                                                                      *))
                                                                    a-%offset%)))
                                            (setf ajj
                                                  (- (f2cl-lib:fref a-%data%
                                                                    (j j)
                                                                    ((1
                                                                      lda)
                                                                     (1
                                                                      *))
                                                                    a-%offset%))))
                                           (t (setf ajj (- one))))
                                     (cond ((< j n)
                                            (multiple-value-bind (var-0
                                                                  var-1
                                                                  var-2
                                                                  var-3
                                                                  var-4
                                                                  var-5
                                                                  var-6
                                                                  var-7)
                                                (dtrmv "Lower"
                                                 "No transpose" diag
                                                 (f2cl-lib:int-sub n j)
                                                 (f2cl-lib:array-slice a
                                                                       double-float
                                                                       ((+ j
                                                                           1)
                                                                        (f2cl-lib:int-add j
                                                                                          1))
                                                                       ((1
                                                                         lda)
                                                                        (1
                                                                         *)))
                                                 lda
                                                 (f2cl-lib:array-slice a
                                                                       double-float
                                                                       ((+ j
                                                                           1)
                                                                        j)
                                                                       ((1
                                                                         lda)
                                                                        (1
                                                                         *)))
                                                 1)
                                              (declare
                                               (ignore var-0 var-1 var-3
                                                var-4 var-6 var-7))
                                              (when var-2
                                                (setf diag var-2))
                                              (when var-5
                                                (setf lda var-5)))
                                            (multiple-value-bind (var-0
                                                                  var-1
                                                                  var-2
                                                                  var-3)
                                                (dscal
                                                 (f2cl-lib:int-sub n j)
                                                 ajj
                                                 (f2cl-lib:array-slice a
                                                                       double-float
                                                                       ((+ j
                                                                           1)
                                                                        j)
                                                                       ((1
                                                                         lda)
                                                                        (1
                                                                         *)))
                                                 1)
                                              (declare
                                               (ignore var-0 var-2 var-3))
                                              (when var-1
                                                (setf ajj var-1)))))
                                   label20))))
            (go end_label)
       end_label (return (values uplo diag nil nil lda info))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dtrti2
                 fortran-to-lisp::*f2cl-function-info*)
        (fortran-to-lisp::make-f2cl-finfo :arg-types '((simple-array
                                                        character
                                                        (1))
                                                       (simple-array
                                                        character
                                                        (1))
                                                       (fortran-to-lisp::integer4)
                                                       (array
                                                        double-float
                                                        (*))
                                                       (fortran-to-lisp::integer4)
                                                       (fortran-to-lisp::integer4))
          :return-values '(fortran-to-lisp::uplo fortran-to-lisp::diag nil
                           nil fortran-to-lisp::lda fortran-to-lisp::info)
          :calls 'nil)))


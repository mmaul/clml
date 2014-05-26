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

(let* ((one 1.0) (zero 0.0))
  (declare (type (double-float 1.0 1.0) one)
   (type (double-float 0.0 0.0) zero))
  (defun dtrtri (uplo diag n a lda info)
    (declare (type (f2cl-lib:integer4) info lda n)
     (type (simple-array character (*)) diag uplo)
     (type (array double-float (*)) a))
    (f2cl-lib:with-multi-array-data ((a double-float a-%data% a-%offset%)
                                     (uplo character uplo-%data%
                                      uplo-%offset%)
                                     (diag character diag-%data%
                                      diag-%offset%))
      (prog ((j 0) (jb 0) (nb 0) (nn 0) (nounit nil) (upper nil))
            (declare (type (f2cl-lib:integer4) j jb nb nn)
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
                   (xerbla "DTRTRI" (f2cl-lib:int-sub info))
                   (go end_label)))
            (if (= n 0) (go end_label))
            (cond (nounit
                   (f2cl-lib:fdo (info 1 (f2cl-lib:int-add info 1))
                                 ((> info n) nil)
                                 (tagbody
                                     (if (= (f2cl-lib:fref a-%data%
                                                           (info info)
                                                           ((1 lda)
                                                            (1 *))
                                                           a-%offset%)
                                            zero)
                                         (go end_label))
                                   label10))
                   (setf info 0)))
            (setf nb
                  (multiple-value-bind (ret-val var-0 var-1 var-2 var-3
                                        var-4 var-5 var-6)
                      (ilaenv 1 "DTRTRI" (f2cl-lib:f2cl-// uplo diag) n
                       -1 -1 -1)
                    (declare
                     (ignore var-0 var-1 var-2 var-4 var-5 var-6))
                    (when var-3 (setf n var-3))
                    ret-val))
            (cond ((or (<= nb 1) (>= nb n))
                   (multiple-value-bind (var-0 var-1 var-2 var-3 var-4
                                         var-5)
                       (dtrti2 uplo diag n a lda info)
                     (declare (ignore var-3))
                     (when var-0 (setf uplo var-0))
                     (when var-1 (setf diag var-1))
                     (when var-2 (setf n var-2))
                     (when var-4 (setf lda var-4))
                     (when var-5 (setf info var-5))))
                  (t
                   (cond (upper
                          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j nb))
                                        ((> j n) nil)
                                        (tagbody
                                            (setf jb
                                                  (min (the f2cl-lib:integer4
                                                            nb)
                                                       (the f2cl-lib:integer4
                                                            (f2cl-lib:int-add (f2cl-lib:int-sub n
                                                                                                j)
                                                                              1))))
                                            (multiple-value-bind (var-0
                                                                  var-1
                                                                  var-2
                                                                  var-3
                                                                  var-4
                                                                  var-5
                                                                  var-6
                                                                  var-7
                                                                  var-8
                                                                  var-9
                                                                  var-10)
                                                (dtrmm "Left" "Upper"
                                                 "No transpose" diag
                                                 (f2cl-lib:int-sub j 1)
                                                 jb one a lda
                                                 (f2cl-lib:array-slice a
                                                                       double-float
                                                                       (1
                                                                        j)
                                                                       ((1
                                                                         lda)
                                                                        (1
                                                                         *)))
                                                 lda)
                                              (declare
                                               (ignore var-0 var-1 var-2
                                                var-4 var-7 var-9))
                                              (when var-3
                                                (setf diag var-3))
                                              (when var-5
                                                (setf jb var-5))
                                              (when var-6
                                                (setf one var-6))
                                              (when var-8
                                                (setf lda var-8))
                                              (when var-10
                                                (setf lda var-10)))
                                            (multiple-value-bind (var-0
                                                                  var-1
                                                                  var-2
                                                                  var-3
                                                                  var-4
                                                                  var-5
                                                                  var-6
                                                                  var-7
                                                                  var-8
                                                                  var-9
                                                                  var-10)
                                                (dtrsm "Right" "Upper"
                                                 "No transpose" diag
                                                 (f2cl-lib:int-sub j 1)
                                                 jb (- one)
                                                 (f2cl-lib:array-slice a
                                                                       double-float
                                                                       (j
                                                                        j)
                                                                       ((1
                                                                         lda)
                                                                        (1
                                                                         *)))
                                                 lda
                                                 (f2cl-lib:array-slice a
                                                                       double-float
                                                                       (1
                                                                        j)
                                                                       ((1
                                                                         lda)
                                                                        (1
                                                                         *)))
                                                 lda)
                                              (declare
                                               (ignore var-0 var-1 var-2
                                                var-4 var-6 var-7 var-9))
                                              (when var-3
                                                (setf diag var-3))
                                              (when var-5
                                                (setf jb var-5))
                                              (when var-8
                                                (setf lda var-8))
                                              (when var-10
                                                (setf lda var-10)))
                                            (multiple-value-bind (var-0
                                                                  var-1
                                                                  var-2
                                                                  var-3
                                                                  var-4
                                                                  var-5)
                                                (dtrti2 "Upper" diag jb
                                                 (f2cl-lib:array-slice a
                                                                       double-float
                                                                       (j
                                                                        j)
                                                                       ((1
                                                                         lda)
                                                                        (1
                                                                         *)))
                                                 lda info)
                                              (declare
                                               (ignore var-0 var-3))
                                              (when var-1
                                                (setf diag var-1))
                                              (when var-2
                                                (setf jb var-2))
                                              (when var-4
                                                (setf lda var-4))
                                              (when var-5
                                                (setf info var-5)))
                                          label20)))
                         (t
                          (setf nn
                                (+ (* (the f2cl-lib:integer4
                                           (truncate (- n 1) nb))
                                      nb)
                                   1))
                          (f2cl-lib:fdo (j nn
                                         (f2cl-lib:int-add j
                                                           (f2cl-lib:int-sub nb)))
                                        ((> j 1) nil)
                                        (tagbody
                                            (setf jb
                                                  (min (the f2cl-lib:integer4
                                                            nb)
                                                       (the f2cl-lib:integer4
                                                            (f2cl-lib:int-add (f2cl-lib:int-sub n
                                                                                                j)
                                                                              1))))
                                            (cond ((<= (f2cl-lib:int-add j
                                                                         jb)
                                                       n)
                                                   (multiple-value-bind (var-0
                                                                         var-1
                                                                         var-2
                                                                         var-3
                                                                         var-4
                                                                         var-5
                                                                         var-6
                                                                         var-7
                                                                         var-8
                                                                         var-9
                                                                         var-10)
                                                       (dtrmm "Left"
                                                        "Lower"
                                                        "No transpose"
                                                        diag
                                                        (f2cl-lib:int-add (f2cl-lib:int-sub n
                                                                                            j
                                                                                            jb)
                                                                          1)
                                                        jb one
                                                        (f2cl-lib:array-slice a
                                                                              double-float
                                                                              ((+ j
                                                                                  jb)
                                                                               (f2cl-lib:int-add j
                                                                                                 jb))
                                                                              ((1
                                                                                lda)
                                                                               (1
                                                                                *)))
                                                        lda
                                                        (f2cl-lib:array-slice a
                                                                              double-float
                                                                              ((+ j
                                                                                  jb)
                                                                               j)
                                                                              ((1
                                                                                lda)
                                                                               (1
                                                                                *)))
                                                        lda)
                                                     (declare
                                                      (ignore var-0 var-1
                                                       var-2 var-4 var-7
                                                       var-9))
                                                     (when var-3
                                                       (setf diag var-3))
                                                     (when var-5
                                                       (setf jb var-5))
                                                     (when var-6
                                                       (setf one var-6))
                                                     (when var-8
                                                       (setf lda var-8))
                                                     (when var-10
                                                       (setf lda
                                                             var-10)))
                                                   (multiple-value-bind (var-0
                                                                         var-1
                                                                         var-2
                                                                         var-3
                                                                         var-4
                                                                         var-5
                                                                         var-6
                                                                         var-7
                                                                         var-8
                                                                         var-9
                                                                         var-10)
                                                       (dtrsm "Right"
                                                        "Lower"
                                                        "No transpose"
                                                        diag
                                                        (f2cl-lib:int-add (f2cl-lib:int-sub n
                                                                                            j
                                                                                            jb)
                                                                          1)
                                                        jb (- one)
                                                        (f2cl-lib:array-slice a
                                                                              double-float
                                                                              (j
                                                                               j)
                                                                              ((1
                                                                                lda)
                                                                               (1
                                                                                *)))
                                                        lda
                                                        (f2cl-lib:array-slice a
                                                                              double-float
                                                                              ((+ j
                                                                                  jb)
                                                                               j)
                                                                              ((1
                                                                                lda)
                                                                               (1
                                                                                *)))
                                                        lda)
                                                     (declare
                                                      (ignore var-0 var-1
                                                       var-2 var-4 var-6
                                                       var-7 var-9))
                                                     (when var-3
                                                       (setf diag var-3))
                                                     (when var-5
                                                       (setf jb var-5))
                                                     (when var-8
                                                       (setf lda var-8))
                                                     (when var-10
                                                       (setf lda
                                                             var-10)))))
                                            (multiple-value-bind (var-0
                                                                  var-1
                                                                  var-2
                                                                  var-3
                                                                  var-4
                                                                  var-5)
                                                (dtrti2 "Lower" diag jb
                                                 (f2cl-lib:array-slice a
                                                                       double-float
                                                                       (j
                                                                        j)
                                                                       ((1
                                                                         lda)
                                                                        (1
                                                                         *)))
                                                 lda info)
                                              (declare
                                               (ignore var-0 var-3))
                                              (when var-1
                                                (setf diag var-1))
                                              (when var-2
                                                (setf jb var-2))
                                              (when var-4
                                                (setf lda var-4))
                                              (when var-5
                                                (setf info var-5)))
                                          label30))))))
            (go end_label)
       end_label (return (values uplo diag n nil lda info))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dtrtri
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
          :return-values '(fortran-to-lisp::uplo fortran-to-lisp::diag
                           fortran-to-lisp::n nil fortran-to-lisp::lda
                           fortran-to-lisp::info)
          :calls 'nil)))


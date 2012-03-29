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


(let* ((zero 0.0) (one 1.0))
  (declare (type (double-float 0.0 0.0) zero)
   (type (double-float 1.0 1.0) one))
  (defun dgetri (n a lda ipiv work lwork info)
    (declare (type (f2cl-lib:integer4) info lwork lda n)
     (type (array double-float (*)) work a)
     (type (array f2cl-lib:integer4 (*)) ipiv))
    (f2cl-lib:with-multi-array-data ((ipiv f2cl-lib:integer4 ipiv-%data%
                                      ipiv-%offset%)
                                     (a double-float a-%data% a-%offset%)
                                     (work double-float work-%data%
                                      work-%offset%))
      (prog ((i 0) (iws 0) (j 0) (jb 0) (jj 0) (jp 0) (ldwork 0)
             (lwkopt 0) (nb 0) (nbmin 0) (nn 0) (lquery nil))
            (declare
             (type (f2cl-lib:integer4) i iws j jb jj jp ldwork lwkopt nb
              nbmin nn)
             (type f2cl-lib:logical lquery))
            (setf info 0)
            (setf nb
                  (multiple-value-bind (ret-val var-0 var-1 var-2 var-3
                                        var-4 var-5 var-6)
                      (ilaenv 1 "DGETRI" " " n -1 -1 -1)
                    (declare (ignore var-0 var-1 var-2 var-4 var-5 var-6))
                    (when var-3 (setf n var-3))
                    ret-val))
            (setf lwkopt (f2cl-lib:int-mul n nb))
            (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                  (coerce (the f2cl-lib:integer4 lwkopt) 'double-float))
            (setf lquery (coerce (= lwork -1) 'f2cl-lib:logical))
            (cond ((< n 0) (setf info -1))
                  ((< lda
                      (max (the f2cl-lib:integer4 1)
                           (the f2cl-lib:integer4 n)))
                   (setf info -3))
                  ((and (< lwork
                           (max (the f2cl-lib:integer4 1)
                                (the f2cl-lib:integer4 n)))
                        (not lquery))
                   (setf info -6)))
            (cond ((/= info 0)
                   (xerbla "DGETRI" (f2cl-lib:int-sub info))
                   (go end_label))
                  (lquery (go end_label)))
            (if (= n 0) (go end_label))
            (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
                (dtrtri "Upper" "Non-unit" n a lda info)
              (declare (ignore var-0 var-1 var-3))
              (when var-2 (setf n var-2))
              (when var-4 (setf lda var-4))
              (when var-5 (setf info var-5)))
            (if (> info 0) (go end_label))
            (setf nbmin 2)
            (setf ldwork n)
            (cond ((and (> nb 1) (< nb n))
                   (setf iws
                         (max (the f2cl-lib:integer4
                                   (f2cl-lib:int-mul ldwork nb))
                              (the f2cl-lib:integer4 1)))
                   (cond ((< lwork iws)
                          (setf nb
                                (the f2cl-lib:integer4
                                     (truncate lwork ldwork)))
                          (setf nbmin
                                (max (the f2cl-lib:integer4 2)
                                     (the f2cl-lib:integer4
                                          (multiple-value-bind (ret-val
                                                                var-0
                                                                var-1
                                                                var-2
                                                                var-3
                                                                var-4
                                                                var-5
                                                                var-6)
                                              (ilaenv 2 "DGETRI" " " n -1
                                               -1 -1)
                                            (declare
                                             (ignore var-0 var-1 var-2
                                              var-4 var-5 var-6))
                                            (when var-3 (setf n var-3))
                                            ret-val)))))))
                  (t (setf iws n)))
            (cond ((or (< nb nbmin) (>= nb n))
                   (f2cl-lib:fdo (j n
                                  (f2cl-lib:int-add j
                                                    (f2cl-lib:int-sub 1)))
                                 ((> j 1) nil)
                                 (tagbody
                                     (f2cl-lib:fdo (i
                                                    (f2cl-lib:int-add j 1)
                                                    (f2cl-lib:int-add i
                                                                      1))
                                                   ((> i n) nil)
                                                   (tagbody
                                                       (setf (f2cl-lib:fref work-%data%
                                                                            (i)
                                                                            ((1
                                                                              *))
                                                                            work-%offset%)
                                                             (f2cl-lib:fref a-%data%
                                                                            (i
                                                                             j)
                                                                            ((1
                                                                              lda)
                                                                             (1
                                                                              *))
                                                                            a-%offset%))
                                                       (setf (f2cl-lib:fref a-%data%
                                                                            (i
                                                                             j)
                                                                            ((1
                                                                              lda)
                                                                             (1
                                                                              *))
                                                                            a-%offset%)
                                                             zero)
                                                     label10))
                                     (if (< j n)
                                         (multiple-value-bind (var-0 var-1
                                                               var-2 var-3
                                                               var-4 var-5
                                                               var-6 var-7
                                                               var-8 var-9
                                                               var-10)
                                             (dgemv "No transpose" n
                                              (f2cl-lib:int-sub n j)
                                              (- one)
                                              (f2cl-lib:array-slice a
                                                                    double-float
                                                                    (1
                                                                     (f2cl-lib:int-add j
                                                                                       1))
                                                                    ((1
                                                                      lda)
                                                                     (1
                                                                      *)))
                                              lda
                                              (f2cl-lib:array-slice work
                                                                    double-float
                                                                    ((+ j
                                                                        1))
                                                                    ((1
                                                                      *)))
                                              1 one
                                              (f2cl-lib:array-slice a
                                                                    double-float
                                                                    (1 j)
                                                                    ((1
                                                                      lda)
                                                                     (1
                                                                      *)))
                                              1)
                                           (declare
                                            (ignore var-0 var-2 var-3
                                             var-4 var-6 var-7 var-9
                                             var-10))
                                           (when var-1 (setf n var-1))
                                           (when var-5 (setf lda var-5))
                                           (when var-8 (setf one var-8))))
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
                                           (min (the f2cl-lib:integer4 nb)
                                                (the f2cl-lib:integer4
                                                     (f2cl-lib:int-add (f2cl-lib:int-sub n
                                                                                         j)
                                                                       1))))
                                     (f2cl-lib:fdo (jj j
                                                    (f2cl-lib:int-add jj
                                                                      1))
                                                   ((> jj
                                                       (f2cl-lib:int-add j
                                                                         jb
                                                                         (f2cl-lib:int-sub 1)))
                                                    nil)
                                                   (tagbody
                                                       (f2cl-lib:fdo (i
                                                                      (f2cl-lib:int-add jj
                                                                                        1)
                                                                      (f2cl-lib:int-add i
                                                                                        1))
                                                                     ((> i
                                                                         n)
                                                                      nil)
                                                                     (tagbody
                                                                         (setf (f2cl-lib:fref work-%data%
                                                                                              ((f2cl-lib:int-add i
                                                                                                                 (f2cl-lib:int-mul (f2cl-lib:int-sub jj
                                                                                                                                                     j)
                                                                                                                                   ldwork)))
                                                                                              ((1
                                                                                                *))
                                                                                              work-%offset%)
                                                                               (f2cl-lib:fref a-%data%
                                                                                              (i
                                                                                               jj)
                                                                                              ((1
                                                                                                lda)
                                                                                               (1
                                                                                                *))
                                                                                              a-%offset%))
                                                                         (setf (f2cl-lib:fref a-%data%
                                                                                              (i
                                                                                               jj)
                                                                                              ((1
                                                                                                lda)
                                                                                               (1
                                                                                                *))
                                                                                              a-%offset%)
                                                                               zero)
                                                                       label30))
                                                     label40))
                                     (if (<= (f2cl-lib:int-add j jb) n)
                                         (multiple-value-bind (var-0 var-1
                                                               var-2 var-3
                                                               var-4 var-5
                                                               var-6 var-7
                                                               var-8 var-9
                                                               var-10
                                                               var-11
                                                               var-12)
                                             (dgemm "No transpose"
                                              "No transpose" n jb
                                              (f2cl-lib:int-add (f2cl-lib:int-sub n
                                                                                  j
                                                                                  jb)
                                                                1)
                                              (- one)
                                              (f2cl-lib:array-slice a
                                                                    double-float
                                                                    (1
                                                                     (f2cl-lib:int-add j
                                                                                       jb))
                                                                    ((1
                                                                      lda)
                                                                     (1
                                                                      *)))
                                              lda
                                              (f2cl-lib:array-slice work
                                                                    double-float
                                                                    ((+ j
                                                                        jb))
                                                                    ((1
                                                                      *)))
                                              ldwork one
                                              (f2cl-lib:array-slice a
                                                                    double-float
                                                                    (1 j)
                                                                    ((1
                                                                      lda)
                                                                     (1
                                                                      *)))
                                              lda)
                                           (declare
                                            (ignore var-0 var-1 var-4
                                             var-5 var-6 var-8 var-11))
                                           (when var-2 (setf n var-2))
                                           (when var-3 (setf jb var-3))
                                           (when var-7 (setf lda var-7))
                                           (when var-9
                                             (setf ldwork var-9))
                                           (when var-10 (setf one var-10))
                                           (when var-12
                                             (setf lda var-12))))
                                     (multiple-value-bind (var-0 var-1
                                                           var-2 var-3
                                                           var-4 var-5
                                                           var-6 var-7
                                                           var-8 var-9
                                                           var-10)
                                         (dtrsm "Right" "Lower"
                                          "No transpose" "Unit" n jb one
                                          (f2cl-lib:array-slice work
                                                                double-float
                                                                (j)
                                                                ((1 *)))
                                          ldwork
                                          (f2cl-lib:array-slice a
                                                                double-float
                                                                (1 j)
                                                                ((1 lda)
                                                                 (1 *)))
                                          lda)
                                       (declare
                                        (ignore var-0 var-1 var-2 var-3
                                         var-7 var-9))
                                       (when var-4 (setf n var-4))
                                       (when var-5 (setf jb var-5))
                                       (when var-6 (setf one var-6))
                                       (when var-8 (setf ldwork var-8))
                                       (when var-10 (setf lda var-10)))
                                   label50))))
            (f2cl-lib:fdo (j (f2cl-lib:int-add n (f2cl-lib:int-sub 1))
                           (f2cl-lib:int-add j (f2cl-lib:int-sub 1)))
                          ((> j 1) nil)
                          (tagbody
                              (setf jp
                                    (f2cl-lib:fref ipiv-%data% (j) ((1 *))
                                                   ipiv-%offset%))
                              (if (/= jp j)
                                  (multiple-value-bind (var-0 var-1 var-2
                                                        var-3 var-4)
                                      (dswap n
                                       (f2cl-lib:array-slice a
                                                             double-float
                                                             (1 j)
                                                             ((1 lda)
                                                              (1 *)))
                                       1
                                       (f2cl-lib:array-slice a
                                                             double-float
                                                             (1 jp)
                                                             ((1 lda)
                                                              (1 *)))
                                       1)
                                    (declare
                                     (ignore var-1 var-2 var-3 var-4))
                                    (when var-0 (setf n var-0))))
                            label60))
            (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                  (coerce (the f2cl-lib:integer4 iws) 'double-float))
            (go end_label)
       end_label (return (values n nil lda nil nil nil info))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dgetri
                 fortran-to-lisp::*f2cl-function-info*)
        (fortran-to-lisp::make-f2cl-finfo :arg-types '((fortran-to-lisp::integer4)
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
          :return-values '(fortran-to-lisp::n nil fortran-to-lisp::lda nil
                           nil nil fortran-to-lisp::info)
          :calls 'nil)))


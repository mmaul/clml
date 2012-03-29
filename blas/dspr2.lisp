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

(in-package "BLAS")


(let* ((zero 0.0))
  (declare (type (double-float 0.0 0.0) zero) (ignorable zero))
  (defun dspr2 (uplo n alpha x incx y incy ap)
    (declare (type (double-float) alpha)
     (type (array double-float (*)) ap y x)
     (type (simple-array character (*)) uplo)
     (type (f2cl-lib:integer4) incy incx n))
    (f2cl-lib:with-multi-array-data ((uplo character uplo-%data%
                                      uplo-%offset%)
                                     (x double-float x-%data%
                                      x-%offset%)
                                     (y double-float y-%data%
                                      y-%offset%)
                                     (ap double-float ap-%data%
                                      ap-%offset%))
      (prog ((i 0) (info 0) (ix 0) (iy 0) (j 0) (jx 0) (jy 0) (k 0)
             (kk 0) (kx 0) (ky 0) (temp1 0.0) (temp2 0.0))
            (declare (type (double-float) temp1 temp2)
             (type (f2cl-lib:integer4) i info ix iy j jx jy k kk kx
              ky))
            (setf info 0)
            (cond ((and (not (lsame uplo "U")) (not (lsame uplo "L")))
                   (setf info 1))
                  ((< n 0) (setf info 2))
                  ((= incx 0) (setf info 5))
                  ((= incy 0) (setf info 7)))
            (cond ((/= info 0) (xerbla "DSPR2 " info) (go end_label)))
            (if (or (= n 0) (= alpha zero)) (go end_label))
            (cond ((or (/= incx 1) (/= incy 1))
                   (cond ((> incx 0) (setf kx 1))
                         (t
                          (setf kx
                                (f2cl-lib:int-sub 1
                                                  (f2cl-lib:int-mul (f2cl-lib:int-sub n
                                                                                      1)
                                                                    incx)))))
                   (cond ((> incy 0) (setf ky 1))
                         (t
                          (setf ky
                                (f2cl-lib:int-sub 1
                                                  (f2cl-lib:int-mul (f2cl-lib:int-sub n
                                                                                      1)
                                                                    incy)))))
                   (setf jx kx)
                   (setf jy ky)))
            (setf kk 1)
            (cond ((lsame uplo "U")
                   (cond ((and (= incx 1) (= incy 1))
                          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                                        ((> j n) nil)
                                        (tagbody
                                            (cond ((or (/= (f2cl-lib:fref x
                                                                          (j)
                                                                          ((1
                                                                            *)))
                                                           zero)
                                                       (/= (f2cl-lib:fref y
                                                                          (j)
                                                                          ((1
                                                                            *)))
                                                           zero))
                                                   (setf temp1
                                                         (* alpha
                                                            (f2cl-lib:fref y-%data%
                                                                           (j)
                                                                           ((1
                                                                             *))
                                                                           y-%offset%)))
                                                   (setf temp2
                                                         (* alpha
                                                            (f2cl-lib:fref x-%data%
                                                                           (j)
                                                                           ((1
                                                                             *))
                                                                           x-%offset%)))
                                                   (setf k kk)
                                                   (f2cl-lib:fdo (i 1
                                                                  (f2cl-lib:int-add i
                                                                                    1))
                                                                 ((> i
                                                                     j)
                                                                  nil)
                                                                 (tagbody
                                                                     (setf (f2cl-lib:fref ap-%data%
                                                                                          (k)
                                                                                          ((1
                                                                                            *))
                                                                                          ap-%offset%)
                                                                           (+ (f2cl-lib:fref ap-%data%
                                                                                             (k)
                                                                                             ((1
                                                                                               *))
                                                                                             ap-%offset%)
                                                                              (* (f2cl-lib:fref x-%data%
                                                                                                (i)
                                                                                                ((1
                                                                                                  *))
                                                                                                x-%offset%)
                                                                                 temp1)
                                                                              (* (f2cl-lib:fref y-%data%
                                                                                                (i)
                                                                                                ((1
                                                                                                  *))
                                                                                                y-%offset%)
                                                                                 temp2)))
                                                                     (setf k
                                                                           (f2cl-lib:int-add k
                                                                                             1))
                                                                   label10))))
                                            (setf kk
                                                  (f2cl-lib:int-add kk
                                                                    j))
                                          label20)))
                         (t
                          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                                        ((> j n) nil)
                                        (tagbody
                                            (cond ((or (/= (f2cl-lib:fref x
                                                                          (jx)
                                                                          ((1
                                                                            *)))
                                                           zero)
                                                       (/= (f2cl-lib:fref y
                                                                          (jy)
                                                                          ((1
                                                                            *)))
                                                           zero))
                                                   (setf temp1
                                                         (* alpha
                                                            (f2cl-lib:fref y-%data%
                                                                           (jy)
                                                                           ((1
                                                                             *))
                                                                           y-%offset%)))
                                                   (setf temp2
                                                         (* alpha
                                                            (f2cl-lib:fref x-%data%
                                                                           (jx)
                                                                           ((1
                                                                             *))
                                                                           x-%offset%)))
                                                   (setf ix kx)
                                                   (setf iy ky)
                                                   (f2cl-lib:fdo (k kk
                                                                  (f2cl-lib:int-add k
                                                                                    1))
                                                                 ((> k
                                                                     (f2cl-lib:int-add kk
                                                                                       j
                                                                                       (f2cl-lib:int-sub 1)))
                                                                  nil)
                                                                 (tagbody
                                                                     (setf (f2cl-lib:fref ap-%data%
                                                                                          (k)
                                                                                          ((1
                                                                                            *))
                                                                                          ap-%offset%)
                                                                           (+ (f2cl-lib:fref ap-%data%
                                                                                             (k)
                                                                                             ((1
                                                                                               *))
                                                                                             ap-%offset%)
                                                                              (* (f2cl-lib:fref x-%data%
                                                                                                (ix)
                                                                                                ((1
                                                                                                  *))
                                                                                                x-%offset%)
                                                                                 temp1)
                                                                              (* (f2cl-lib:fref y-%data%
                                                                                                (iy)
                                                                                                ((1
                                                                                                  *))
                                                                                                y-%offset%)
                                                                                 temp2)))
                                                                     (setf ix
                                                                           (f2cl-lib:int-add ix
                                                                                             incx))
                                                                     (setf iy
                                                                           (f2cl-lib:int-add iy
                                                                                             incy))
                                                                   label30))))
                                            (setf jx
                                                  (f2cl-lib:int-add jx
                                                                    incx))
                                            (setf jy
                                                  (f2cl-lib:int-add jy
                                                                    incy))
                                            (setf kk
                                                  (f2cl-lib:int-add kk
                                                                    j))
                                          label40)))))
                  (t
                   (cond ((and (= incx 1) (= incy 1))
                          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                                        ((> j n) nil)
                                        (tagbody
                                            (cond ((or (/= (f2cl-lib:fref x
                                                                          (j)
                                                                          ((1
                                                                            *)))
                                                           zero)
                                                       (/= (f2cl-lib:fref y
                                                                          (j)
                                                                          ((1
                                                                            *)))
                                                           zero))
                                                   (setf temp1
                                                         (* alpha
                                                            (f2cl-lib:fref y-%data%
                                                                           (j)
                                                                           ((1
                                                                             *))
                                                                           y-%offset%)))
                                                   (setf temp2
                                                         (* alpha
                                                            (f2cl-lib:fref x-%data%
                                                                           (j)
                                                                           ((1
                                                                             *))
                                                                           x-%offset%)))
                                                   (setf k kk)
                                                   (f2cl-lib:fdo (i j
                                                                  (f2cl-lib:int-add i
                                                                                    1))
                                                                 ((> i
                                                                     n)
                                                                  nil)
                                                                 (tagbody
                                                                     (setf (f2cl-lib:fref ap-%data%
                                                                                          (k)
                                                                                          ((1
                                                                                            *))
                                                                                          ap-%offset%)
                                                                           (+ (f2cl-lib:fref ap-%data%
                                                                                             (k)
                                                                                             ((1
                                                                                               *))
                                                                                             ap-%offset%)
                                                                              (* (f2cl-lib:fref x-%data%
                                                                                                (i)
                                                                                                ((1
                                                                                                  *))
                                                                                                x-%offset%)
                                                                                 temp1)
                                                                              (* (f2cl-lib:fref y-%data%
                                                                                                (i)
                                                                                                ((1
                                                                                                  *))
                                                                                                y-%offset%)
                                                                                 temp2)))
                                                                     (setf k
                                                                           (f2cl-lib:int-add k
                                                                                             1))
                                                                   label50))))
                                            (setf kk
                                                  (f2cl-lib:int-add (f2cl-lib:int-sub (f2cl-lib:int-add kk
                                                                                                        n)
                                                                                      j)
                                                                    1))
                                          label60)))
                         (t
                          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                                        ((> j n) nil)
                                        (tagbody
                                            (cond ((or (/= (f2cl-lib:fref x
                                                                          (jx)
                                                                          ((1
                                                                            *)))
                                                           zero)
                                                       (/= (f2cl-lib:fref y
                                                                          (jy)
                                                                          ((1
                                                                            *)))
                                                           zero))
                                                   (setf temp1
                                                         (* alpha
                                                            (f2cl-lib:fref y-%data%
                                                                           (jy)
                                                                           ((1
                                                                             *))
                                                                           y-%offset%)))
                                                   (setf temp2
                                                         (* alpha
                                                            (f2cl-lib:fref x-%data%
                                                                           (jx)
                                                                           ((1
                                                                             *))
                                                                           x-%offset%)))
                                                   (setf ix jx)
                                                   (setf iy jy)
                                                   (f2cl-lib:fdo (k kk
                                                                  (f2cl-lib:int-add k
                                                                                    1))
                                                                 ((> k
                                                                     (f2cl-lib:int-add kk
                                                                                       n
                                                                                       (f2cl-lib:int-sub j)))
                                                                  nil)
                                                                 (tagbody
                                                                     (setf (f2cl-lib:fref ap-%data%
                                                                                          (k)
                                                                                          ((1
                                                                                            *))
                                                                                          ap-%offset%)
                                                                           (+ (f2cl-lib:fref ap-%data%
                                                                                             (k)
                                                                                             ((1
                                                                                               *))
                                                                                             ap-%offset%)
                                                                              (* (f2cl-lib:fref x-%data%
                                                                                                (ix)
                                                                                                ((1
                                                                                                  *))
                                                                                                x-%offset%)
                                                                                 temp1)
                                                                              (* (f2cl-lib:fref y-%data%
                                                                                                (iy)
                                                                                                ((1
                                                                                                  *))
                                                                                                y-%offset%)
                                                                                 temp2)))
                                                                     (setf ix
                                                                           (f2cl-lib:int-add ix
                                                                                             incx))
                                                                     (setf iy
                                                                           (f2cl-lib:int-add iy
                                                                                             incy))
                                                                   label70))))
                                            (setf jx
                                                  (f2cl-lib:int-add jx
                                                                    incx))
                                            (setf jy
                                                  (f2cl-lib:int-add jy
                                                                    incy))
                                            (setf kk
                                                  (f2cl-lib:int-add (f2cl-lib:int-sub (f2cl-lib:int-add kk
                                                                                                        n)
                                                                                      j)
                                                                    1))
                                          label80))))))
            (go end_label)
       end_label (return (values nil nil nil nil nil nil nil nil))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dspr2
                 fortran-to-lisp::*f2cl-function-info*)
        (fortran-to-lisp::make-f2cl-finfo :arg-types '((simple-array
                                                        character
                                                        (1))
                                                       (fortran-to-lisp::integer4)
                                                       (double-float)
                                                       (array
                                                        double-float
                                                        (*))
                                                       (fortran-to-lisp::integer4)
                                                       (array
                                                        double-float
                                                        (*))
                                                       (fortran-to-lisp::integer4)
                                                       (array
                                                        double-float
                                                        (*)))
          :return-values '(nil nil nil nil nil nil nil nil)
          :calls '(fortran-to-lisp::xerbla fortran-to-lisp::lsame))))


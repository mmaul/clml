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


(let* ((one (f2cl-lib:cmplx 1.0 0.0)) (zero (f2cl-lib:cmplx 0.0 0.0)))
  (declare (type (f2cl-lib:complex16) one)
   (type (f2cl-lib:complex16) zero) (ignorable one zero))
  (defun zhpmv (uplo n alpha ap x incx beta y incy)
    (declare (type (simple-array character (*)) uplo)
     (type (f2cl-lib:complex16) beta alpha)
     (type (array f2cl-lib:complex16 (*)) y x ap)
     (type (f2cl-lib:integer4) incy incx n))
    (f2cl-lib:with-multi-array-data ((ap f2cl-lib:complex16 ap-%data%
                                      ap-%offset%)
                                     (x f2cl-lib:complex16 x-%data%
                                      x-%offset%)
                                     (y f2cl-lib:complex16 y-%data%
                                      y-%offset%)
                                     (uplo character uplo-%data%
                                      uplo-%offset%))
      (prog ((i 0) (info 0) (ix 0) (iy 0) (j 0) (jx 0) (jy 0) (k 0)
             (kk 0) (kx 0) (ky 0) (temp1 #C(0.0 0.0))
             (temp2 #C(0.0 0.0)))
            (declare (type (f2cl-lib:complex16) temp1 temp2)
             (type (f2cl-lib:integer4) i info ix iy j jx jy k kk kx
              ky))
            (setf info 0)
            (cond ((and (not (lsame uplo "U")) (not (lsame uplo "L")))
                   (setf info 1))
                  ((< n 0) (setf info 2))
                  ((= incx 0) (setf info 6))
                  ((= incy 0) (setf info 9)))
            (cond ((/= info 0) (xerbla "ZHPMV " info) (go end_label)))
            (if (or (= n 0) (and (= alpha zero) (= beta one)))
                (go end_label))
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
            (cond ((/= beta one)
                   (cond ((= incy 1)
                          (cond ((= beta zero)
                                 (f2cl-lib:fdo (i 1
                                                (f2cl-lib:int-add i 1))
                                               ((> i n) nil)
                                               (tagbody
                                                   (setf (f2cl-lib:fref y-%data%
                                                                        (i)
                                                                        ((1
                                                                          *))
                                                                        y-%offset%)
                                                         zero)
                                                 label10)))
                                (t
                                 (f2cl-lib:fdo (i 1
                                                (f2cl-lib:int-add i 1))
                                               ((> i n) nil)
                                               (tagbody
                                                   (setf (f2cl-lib:fref y-%data%
                                                                        (i)
                                                                        ((1
                                                                          *))
                                                                        y-%offset%)
                                                         (* beta
                                                            (f2cl-lib:fref y-%data%
                                                                           (i)
                                                                           ((1
                                                                             *))
                                                                           y-%offset%)))
                                                 label20)))))
                         (t
                          (setf iy ky)
                          (cond ((= beta zero)
                                 (f2cl-lib:fdo (i 1
                                                (f2cl-lib:int-add i 1))
                                               ((> i n) nil)
                                               (tagbody
                                                   (setf (f2cl-lib:fref y-%data%
                                                                        (iy)
                                                                        ((1
                                                                          *))
                                                                        y-%offset%)
                                                         zero)
                                                   (setf iy
                                                         (f2cl-lib:int-add iy
                                                                           incy))
                                                 label30)))
                                (t
                                 (f2cl-lib:fdo (i 1
                                                (f2cl-lib:int-add i 1))
                                               ((> i n) nil)
                                               (tagbody
                                                   (setf (f2cl-lib:fref y-%data%
                                                                        (iy)
                                                                        ((1
                                                                          *))
                                                                        y-%offset%)
                                                         (* beta
                                                            (f2cl-lib:fref y-%data%
                                                                           (iy)
                                                                           ((1
                                                                             *))
                                                                           y-%offset%)))
                                                   (setf iy
                                                         (f2cl-lib:int-add iy
                                                                           incy))
                                                 label40))))))))
            (if (= alpha zero) (go end_label))
            (setf kk 1)
            (cond ((lsame uplo "U")
                   (cond ((and (= incx 1) (= incy 1))
                          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                                        ((> j n) nil)
                                        (tagbody
                                            (setf temp1
                                                  (* alpha
                                                     (f2cl-lib:fref x-%data%
                                                                    (j)
                                                                    ((1
                                                                      *))
                                                                    x-%offset%)))
                                            (setf temp2 zero)
                                            (setf k kk)
                                            (f2cl-lib:fdo (i 1
                                                           (f2cl-lib:int-add i
                                                                             1))
                                                          ((> i
                                                              (f2cl-lib:int-add j
                                                                                (f2cl-lib:int-sub 1)))
                                                           nil)
                                                          (tagbody
                                                              (setf (f2cl-lib:fref y-%data%
                                                                                   (i)
                                                                                   ((1
                                                                                     *))
                                                                                   y-%offset%)
                                                                    (+ (f2cl-lib:fref y-%data%
                                                                                      (i)
                                                                                      ((1
                                                                                        *))
                                                                                      y-%offset%)
                                                                       (* temp1
                                                                          (f2cl-lib:fref ap-%data%
                                                                                         (k)
                                                                                         ((1
                                                                                           *))
                                                                                         ap-%offset%))))
                                                              (setf temp2
                                                                    (+ temp2
                                                                       (* (f2cl-lib:dconjg (f2cl-lib:fref ap-%data%
                                                                                                          (k)
                                                                                                          ((1
                                                                                                            *))
                                                                                                          ap-%offset%))
                                                                          (f2cl-lib:fref x-%data%
                                                                                         (i)
                                                                                         ((1
                                                                                           *))
                                                                                         x-%offset%))))
                                                              (setf k
                                                                    (f2cl-lib:int-add k
                                                                                      1))
                                                            label50))
                                            (setf (f2cl-lib:fref y-%data%
                                                                 (j)
                                                                 ((1
                                                                   *))
                                                                 y-%offset%)
                                                  (+ (f2cl-lib:fref y-%data%
                                                                    (j)
                                                                    ((1
                                                                      *))
                                                                    y-%offset%)
                                                     (* temp1
                                                        (f2cl-lib:dble (f2cl-lib:fref ap-%data%
                                                                                      ((f2cl-lib:int-sub (f2cl-lib:int-add kk
                                                                                                                           j)
                                                                                                         1))
                                                                                      ((1
                                                                                        *))
                                                                                      ap-%offset%)))
                                                     (* alpha temp2)))
                                            (setf kk
                                                  (f2cl-lib:int-add kk
                                                                    j))
                                          label60)))
                         (t
                          (setf jx kx)
                          (setf jy ky)
                          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                                        ((> j n) nil)
                                        (tagbody
                                            (setf temp1
                                                  (* alpha
                                                     (f2cl-lib:fref x-%data%
                                                                    (jx)
                                                                    ((1
                                                                      *))
                                                                    x-%offset%)))
                                            (setf temp2 zero)
                                            (setf ix kx)
                                            (setf iy ky)
                                            (f2cl-lib:fdo (k kk
                                                           (f2cl-lib:int-add k
                                                                             1))
                                                          ((> k
                                                              (f2cl-lib:int-add kk
                                                                                j
                                                                                (f2cl-lib:int-sub 2)))
                                                           nil)
                                                          (tagbody
                                                              (setf (f2cl-lib:fref y-%data%
                                                                                   (iy)
                                                                                   ((1
                                                                                     *))
                                                                                   y-%offset%)
                                                                    (+ (f2cl-lib:fref y-%data%
                                                                                      (iy)
                                                                                      ((1
                                                                                        *))
                                                                                      y-%offset%)
                                                                       (* temp1
                                                                          (f2cl-lib:fref ap-%data%
                                                                                         (k)
                                                                                         ((1
                                                                                           *))
                                                                                         ap-%offset%))))
                                                              (setf temp2
                                                                    (+ temp2
                                                                       (* (f2cl-lib:dconjg (f2cl-lib:fref ap-%data%
                                                                                                          (k)
                                                                                                          ((1
                                                                                                            *))
                                                                                                          ap-%offset%))
                                                                          (f2cl-lib:fref x-%data%
                                                                                         (ix)
                                                                                         ((1
                                                                                           *))
                                                                                         x-%offset%))))
                                                              (setf ix
                                                                    (f2cl-lib:int-add ix
                                                                                      incx))
                                                              (setf iy
                                                                    (f2cl-lib:int-add iy
                                                                                      incy))
                                                            label70))
                                            (setf (f2cl-lib:fref y-%data%
                                                                 (jy)
                                                                 ((1
                                                                   *))
                                                                 y-%offset%)
                                                  (+ (f2cl-lib:fref y-%data%
                                                                    (jy)
                                                                    ((1
                                                                      *))
                                                                    y-%offset%)
                                                     (* temp1
                                                        (f2cl-lib:dble (f2cl-lib:fref ap-%data%
                                                                                      ((f2cl-lib:int-sub (f2cl-lib:int-add kk
                                                                                                                           j)
                                                                                                         1))
                                                                                      ((1
                                                                                        *))
                                                                                      ap-%offset%)))
                                                     (* alpha temp2)))
                                            (setf jx
                                                  (f2cl-lib:int-add jx
                                                                    incx))
                                            (setf jy
                                                  (f2cl-lib:int-add jy
                                                                    incy))
                                            (setf kk
                                                  (f2cl-lib:int-add kk
                                                                    j))
                                          label80)))))
                  (t
                   (cond ((and (= incx 1) (= incy 1))
                          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                                        ((> j n) nil)
                                        (tagbody
                                            (setf temp1
                                                  (* alpha
                                                     (f2cl-lib:fref x-%data%
                                                                    (j)
                                                                    ((1
                                                                      *))
                                                                    x-%offset%)))
                                            (setf temp2 zero)
                                            (setf (f2cl-lib:fref y-%data%
                                                                 (j)
                                                                 ((1
                                                                   *))
                                                                 y-%offset%)
                                                  (+ (f2cl-lib:fref y-%data%
                                                                    (j)
                                                                    ((1
                                                                      *))
                                                                    y-%offset%)
                                                     (* temp1
                                                        (f2cl-lib:dble (f2cl-lib:fref ap-%data%
                                                                                      (kk)
                                                                                      ((1
                                                                                        *))
                                                                                      ap-%offset%)))))
                                            (setf k
                                                  (f2cl-lib:int-add kk
                                                                    1))
                                            (f2cl-lib:fdo (i
                                                           (f2cl-lib:int-add j
                                                                             1)
                                                           (f2cl-lib:int-add i
                                                                             1))
                                                          ((> i n) nil)
                                                          (tagbody
                                                              (setf (f2cl-lib:fref y-%data%
                                                                                   (i)
                                                                                   ((1
                                                                                     *))
                                                                                   y-%offset%)
                                                                    (+ (f2cl-lib:fref y-%data%
                                                                                      (i)
                                                                                      ((1
                                                                                        *))
                                                                                      y-%offset%)
                                                                       (* temp1
                                                                          (f2cl-lib:fref ap-%data%
                                                                                         (k)
                                                                                         ((1
                                                                                           *))
                                                                                         ap-%offset%))))
                                                              (setf temp2
                                                                    (+ temp2
                                                                       (* (f2cl-lib:dconjg (f2cl-lib:fref ap-%data%
                                                                                                          (k)
                                                                                                          ((1
                                                                                                            *))
                                                                                                          ap-%offset%))
                                                                          (f2cl-lib:fref x-%data%
                                                                                         (i)
                                                                                         ((1
                                                                                           *))
                                                                                         x-%offset%))))
                                                              (setf k
                                                                    (f2cl-lib:int-add k
                                                                                      1))
                                                            label90))
                                            (setf (f2cl-lib:fref y-%data%
                                                                 (j)
                                                                 ((1
                                                                   *))
                                                                 y-%offset%)
                                                  (+ (f2cl-lib:fref y-%data%
                                                                    (j)
                                                                    ((1
                                                                      *))
                                                                    y-%offset%)
                                                     (* alpha temp2)))
                                            (setf kk
                                                  (f2cl-lib:int-add kk
                                                                    (f2cl-lib:int-add (f2cl-lib:int-sub n
                                                                                                        j)
                                                                                      1)))
                                          label100)))
                         (t
                          (setf jx kx)
                          (setf jy ky)
                          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                                        ((> j n) nil)
                                        (tagbody
                                            (setf temp1
                                                  (* alpha
                                                     (f2cl-lib:fref x-%data%
                                                                    (jx)
                                                                    ((1
                                                                      *))
                                                                    x-%offset%)))
                                            (setf temp2 zero)
                                            (setf (f2cl-lib:fref y-%data%
                                                                 (jy)
                                                                 ((1
                                                                   *))
                                                                 y-%offset%)
                                                  (+ (f2cl-lib:fref y-%data%
                                                                    (jy)
                                                                    ((1
                                                                      *))
                                                                    y-%offset%)
                                                     (* temp1
                                                        (f2cl-lib:dble (f2cl-lib:fref ap-%data%
                                                                                      (kk)
                                                                                      ((1
                                                                                        *))
                                                                                      ap-%offset%)))))
                                            (setf ix jx)
                                            (setf iy jy)
                                            (f2cl-lib:fdo (k
                                                           (f2cl-lib:int-add kk
                                                                             1)
                                                           (f2cl-lib:int-add k
                                                                             1))
                                                          ((> k
                                                              (f2cl-lib:int-add kk
                                                                                n
                                                                                (f2cl-lib:int-sub j)))
                                                           nil)
                                                          (tagbody
                                                              (setf ix
                                                                    (f2cl-lib:int-add ix
                                                                                      incx))
                                                              (setf iy
                                                                    (f2cl-lib:int-add iy
                                                                                      incy))
                                                              (setf (f2cl-lib:fref y-%data%
                                                                                   (iy)
                                                                                   ((1
                                                                                     *))
                                                                                   y-%offset%)
                                                                    (+ (f2cl-lib:fref y-%data%
                                                                                      (iy)
                                                                                      ((1
                                                                                        *))
                                                                                      y-%offset%)
                                                                       (* temp1
                                                                          (f2cl-lib:fref ap-%data%
                                                                                         (k)
                                                                                         ((1
                                                                                           *))
                                                                                         ap-%offset%))))
                                                              (setf temp2
                                                                    (+ temp2
                                                                       (* (f2cl-lib:dconjg (f2cl-lib:fref ap-%data%
                                                                                                          (k)
                                                                                                          ((1
                                                                                                            *))
                                                                                                          ap-%offset%))
                                                                          (f2cl-lib:fref x-%data%
                                                                                         (ix)
                                                                                         ((1
                                                                                           *))
                                                                                         x-%offset%))))
                                                            label110))
                                            (setf (f2cl-lib:fref y-%data%
                                                                 (jy)
                                                                 ((1
                                                                   *))
                                                                 y-%offset%)
                                                  (+ (f2cl-lib:fref y-%data%
                                                                    (jy)
                                                                    ((1
                                                                      *))
                                                                    y-%offset%)
                                                     (* alpha temp2)))
                                            (setf jx
                                                  (f2cl-lib:int-add jx
                                                                    incx))
                                            (setf jy
                                                  (f2cl-lib:int-add jy
                                                                    incy))
                                            (setf kk
                                                  (f2cl-lib:int-add kk
                                                                    (f2cl-lib:int-add (f2cl-lib:int-sub n
                                                                                                        j)
                                                                                      1)))
                                          label120))))))
            (go end_label)
       end_label (return (values nil nil nil nil nil nil nil nil
                                 nil))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zhpmv
                 fortran-to-lisp::*f2cl-function-info*)
        (fortran-to-lisp::make-f2cl-finfo :arg-types '((simple-array
                                                        character
                                                        (1))
                                                       (fortran-to-lisp::integer4)
                                                       (fortran-to-lisp::complex16)
                                                       (array
                                                        fortran-to-lisp::complex16
                                                        (*))
                                                       (array
                                                        fortran-to-lisp::complex16
                                                        (*))
                                                       (fortran-to-lisp::integer4)
                                                       (fortran-to-lisp::complex16)
                                                       (array
                                                        fortran-to-lisp::complex16
                                                        (*))
                                                       (fortran-to-lisp::integer4))
          :return-values '(nil nil nil nil nil nil nil nil nil)
          :calls '(fortran-to-lisp::xerbla fortran-to-lisp::lsame))))


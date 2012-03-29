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


(let* ((zero 0.0) (one 1.0) (two 2.0) (three 3.0) (four 4.0))
  (declare (type (double-float 0.0 0.0) zero)
   (type (double-float 1.0 1.0) one) (type (double-float 2.0 2.0) two)
   (type (double-float 3.0 3.0) three)
   (type (double-float 4.0 4.0) four)
   (ignorable zero one two three four))
  (defun dlasd5 (i d z delta rho dsigma work)
    (declare (type (double-float) dsigma rho)
     (type (array double-float (*)) work delta z d)
     (type (f2cl-lib:integer4) i))
    (f2cl-lib:with-multi-array-data ((d double-float d-%data%
                                      d-%offset%)
                                     (z double-float z-%data%
                                      z-%offset%)
                                     (delta double-float delta-%data%
                                      delta-%offset%)
                                     (work double-float work-%data%
                                      work-%offset%))
      (prog ((b 0.0) (c 0.0) (del 0.0) (delsq 0.0) (tau 0.0) (w 0.0))
            (declare (type (double-float) b c del delsq tau w))
            (setf del
                  (- (f2cl-lib:fref d-%data% (2) ((1 2)) d-%offset%)
                     (f2cl-lib:fref d-%data% (1) ((1 2)) d-%offset%)))
            (setf delsq
                  (* del
                     (+ (f2cl-lib:fref d-%data% (2) ((1 2)) d-%offset%)
                        (f2cl-lib:fref d-%data% (1) ((1 2))
                                       d-%offset%))))
            (cond ((= i 1)
                   (setf w
                         (+ one
                            (/ (* four rho
                                  (+ (/ (* (f2cl-lib:fref z-%data% (2)
                                                          ((1 2))
                                                          z-%offset%)
                                           (f2cl-lib:fref z-%data% (2)
                                                          ((1 2))
                                                          z-%offset%))
                                        (+ (f2cl-lib:fref d-%data% (1)
                                                          ((1 2))
                                                          d-%offset%)
                                           (* three
                                              (f2cl-lib:fref d-%data%
                                                             (2)
                                                             ((1 2))
                                                             d-%offset%))))
                                     (/ (* (- (f2cl-lib:fref z-%data%
                                                             (1)
                                                             ((1 2))
                                                             z-%offset%))
                                           (f2cl-lib:fref z-%data% (1)
                                                          ((1 2))
                                                          z-%offset%))
                                        (+ (* three
                                              (f2cl-lib:fref d-%data%
                                                             (1)
                                                             ((1 2))
                                                             d-%offset%))
                                           (f2cl-lib:fref d-%data% (2)
                                                          ((1 2))
                                                          d-%offset%)))))
                               del)))
                   (cond ((> w zero)
                          (setf b
                                (+ delsq
                                   (* rho
                                      (+ (* (f2cl-lib:fref z-%data% (1)
                                                           ((1 2))
                                                           z-%offset%)
                                            (f2cl-lib:fref z-%data% (1)
                                                           ((1 2))
                                                           z-%offset%))
                                         (* (f2cl-lib:fref z-%data% (2)
                                                           ((1 2))
                                                           z-%offset%)
                                            (f2cl-lib:fref z-%data% (2)
                                                           ((1 2))
                                                           z-%offset%))))))
                          (setf c
                                (* rho
                                   (f2cl-lib:fref z-%data% (1) ((1 2))
                                                  z-%offset%)
                                   (f2cl-lib:fref z-%data% (1) ((1 2))
                                                  z-%offset%)
                                   delsq))
                          (setf tau
                                (/ (* two c)
                                   (+ b
                                      (f2cl-lib:fsqrt (abs (- (* b b)
                                                              (* four
                                                                 c)))))))
                          (setf tau
                                (/ tau
                                   (+ (f2cl-lib:fref d-%data% (1)
                                                     ((1 2))
                                                     d-%offset%)
                                      (f2cl-lib:fsqrt (+ (* (f2cl-lib:fref d-%data%
                                                                           (1)
                                                                           ((1
                                                                             2))
                                                                           d-%offset%)
                                                            (f2cl-lib:fref d-%data%
                                                                           (1)
                                                                           ((1
                                                                             2))
                                                                           d-%offset%))
                                                         tau)))))
                          (setf dsigma
                                (+ (f2cl-lib:fref d-%data% (1) ((1 2))
                                                  d-%offset%)
                                   tau))
                          (setf (f2cl-lib:fref delta-%data% (1) ((1 2))
                                               delta-%offset%)
                                (- tau))
                          (setf (f2cl-lib:fref delta-%data% (2) ((1 2))
                                               delta-%offset%)
                                (- del tau))
                          (setf (f2cl-lib:fref work-%data% (1) ((1 2))
                                               work-%offset%)
                                (+ (* two
                                      (f2cl-lib:fref d-%data% (1)
                                                     ((1 2))
                                                     d-%offset%))
                                   tau))
                          (setf (f2cl-lib:fref work-%data% (2) ((1 2))
                                               work-%offset%)
                                (+ (f2cl-lib:fref d-%data% (1) ((1 2))
                                                  d-%offset%)
                                   tau
                                   (f2cl-lib:fref d-%data% (2) ((1 2))
                                                  d-%offset%))))
                         (t
                          (setf b
                                (- (* rho
                                      (+ (* (f2cl-lib:fref z-%data% (1)
                                                           ((1 2))
                                                           z-%offset%)
                                            (f2cl-lib:fref z-%data% (1)
                                                           ((1 2))
                                                           z-%offset%))
                                         (* (f2cl-lib:fref z-%data% (2)
                                                           ((1 2))
                                                           z-%offset%)
                                            (f2cl-lib:fref z-%data% (2)
                                                           ((1 2))
                                                           z-%offset%))))
                                   delsq))
                          (setf c
                                (* rho
                                   (f2cl-lib:fref z-%data% (2) ((1 2))
                                                  z-%offset%)
                                   (f2cl-lib:fref z-%data% (2) ((1 2))
                                                  z-%offset%)
                                   delsq))
                          (cond ((> b zero)
                                 (setf tau
                                       (/ (* (- two) c)
                                          (+ b
                                             (f2cl-lib:fsqrt (+ (* b b)
                                                                (* four
                                                                   c)))))))
                                (t
                                 (setf tau
                                       (/ (- b
                                             (f2cl-lib:fsqrt (+ (* b b)
                                                                (* four
                                                                   c))))
                                          two))))
                          (setf tau
                                (/ tau
                                   (+ (f2cl-lib:fref d-%data% (2)
                                                     ((1 2))
                                                     d-%offset%)
                                      (f2cl-lib:fsqrt (abs (+ (* (f2cl-lib:fref d-%data%
                                                                                (2)
                                                                                ((1
                                                                                  2))
                                                                                d-%offset%)
                                                                 (f2cl-lib:fref d-%data%
                                                                                (2)
                                                                                ((1
                                                                                  2))
                                                                                d-%offset%))
                                                              tau))))))
                          (setf dsigma
                                (+ (f2cl-lib:fref d-%data% (2) ((1 2))
                                                  d-%offset%)
                                   tau))
                          (setf (f2cl-lib:fref delta-%data% (1) ((1 2))
                                               delta-%offset%)
                                (- (+ del tau)))
                          (setf (f2cl-lib:fref delta-%data% (2) ((1 2))
                                               delta-%offset%)
                                (- tau))
                          (setf (f2cl-lib:fref work-%data% (1) ((1 2))
                                               work-%offset%)
                                (+ (f2cl-lib:fref d-%data% (1) ((1 2))
                                                  d-%offset%)
                                   tau
                                   (f2cl-lib:fref d-%data% (2) ((1 2))
                                                  d-%offset%)))
                          (setf (f2cl-lib:fref work-%data% (2) ((1 2))
                                               work-%offset%)
                                (+ (* two
                                      (f2cl-lib:fref d-%data% (2)
                                                     ((1 2))
                                                     d-%offset%))
                                   tau)))))
                  (t
                   (setf b
                         (- (* rho
                               (+ (* (f2cl-lib:fref z-%data% (1)
                                                    ((1 2)) z-%offset%)
                                     (f2cl-lib:fref z-%data% (1)
                                                    ((1 2))
                                                    z-%offset%))
                                  (* (f2cl-lib:fref z-%data% (2)
                                                    ((1 2)) z-%offset%)
                                     (f2cl-lib:fref z-%data% (2)
                                                    ((1 2))
                                                    z-%offset%))))
                            delsq))
                   (setf c
                         (* rho
                            (f2cl-lib:fref z-%data% (2) ((1 2))
                                           z-%offset%)
                            (f2cl-lib:fref z-%data% (2) ((1 2))
                                           z-%offset%)
                            delsq))
                   (cond ((> b zero)
                          (setf tau
                                (/ (+ b
                                      (f2cl-lib:fsqrt (+ (* b b)
                                                         (* four c))))
                                   two)))
                         (t
                          (setf tau
                                (/ (* two c)
                                   (- (f2cl-lib:fsqrt (+ (* b b)
                                                         (* four c)))
                                      b)))))
                   (setf tau
                         (/ tau
                            (+ (f2cl-lib:fref d-%data% (2) ((1 2))
                                              d-%offset%)
                               (f2cl-lib:fsqrt (+ (* (f2cl-lib:fref d-%data%
                                                                    (2)
                                                                    ((1
                                                                      2))
                                                                    d-%offset%)
                                                     (f2cl-lib:fref d-%data%
                                                                    (2)
                                                                    ((1
                                                                      2))
                                                                    d-%offset%))
                                                  tau)))))
                   (setf dsigma
                         (+ (f2cl-lib:fref d-%data% (2) ((1 2))
                                           d-%offset%)
                            tau))
                   (setf (f2cl-lib:fref delta-%data% (1) ((1 2))
                                        delta-%offset%)
                         (- (+ del tau)))
                   (setf (f2cl-lib:fref delta-%data% (2) ((1 2))
                                        delta-%offset%)
                         (- tau))
                   (setf (f2cl-lib:fref work-%data% (1) ((1 2))
                                        work-%offset%)
                         (+ (f2cl-lib:fref d-%data% (1) ((1 2))
                                           d-%offset%)
                            tau
                            (f2cl-lib:fref d-%data% (2) ((1 2))
                                           d-%offset%)))
                   (setf (f2cl-lib:fref work-%data% (2) ((1 2))
                                        work-%offset%)
                         (+ (* two
                               (f2cl-lib:fref d-%data% (2) ((1 2))
                                              d-%offset%))
                            tau))))
            (go end_label)
       end_label (return (values nil nil nil nil nil dsigma nil))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlasd5
                 fortran-to-lisp::*f2cl-function-info*)
        (fortran-to-lisp::make-f2cl-finfo :arg-types '((fortran-to-lisp::integer4)
                                                       (array
                                                        double-float
                                                        (2))
                                                       (array
                                                        double-float
                                                        (2))
                                                       (array
                                                        double-float
                                                        (2))
                                                       (double-float)
                                                       (double-float)
                                                       (array
                                                        double-float
                                                        (2)))
          :return-values '(nil nil nil nil nil fortran-to-lisp::dsigma
                           nil)
          :calls 'nil)))


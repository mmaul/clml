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


(defun dlamc3 (a b)
  (declare (type (double-float) b a))
  (prog ((dlamc3 0.0))
        (declare (type (double-float) dlamc3))
        (setf dlamc3 (+ a b))
        (go end_label)
   end_label (return (values dlamc3 nil nil))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlamc3
                 fortran-to-lisp::*f2cl-function-info*)
        (fortran-to-lisp::make-f2cl-finfo :arg-types '((double-float)
                                                       (double-float))
          :return-values '(nil nil)
          :calls 'nil)))

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


(let ((lieee1 nil) (lbeta 0) (lrnd nil) (lt$ 0) (first$ nil))
  (declare (type (f2cl-lib:integer4) lbeta)
   (type f2cl-lib:logical lieee1 lrnd first$) (type (integer) lt$))
  (setq first$ f2cl-lib:%true%)
  (defun dlamc1 (beta t$ rnd ieee1)
    (declare (type f2cl-lib:logical ieee1 rnd)
     (type (f2cl-lib:integer4) t$ beta))
    (prog ((a 0.0) (b 0.0) (c 0.0) (f 0.0) (one 0.0) (qtr 0.0)
           (savec 0.0) (t1 0.0) (t2 0.0) (lt$ 0))
          (declare (type (double-float) t2 t1 savec qtr one f c b a)
           (type (f2cl-lib:integer4) lt$))
          (cond (first$
                 (tagbody
                     (setf first$ f2cl-lib:%false%)
                     (setf one
                           (coerce (the f2cl-lib:integer4 1)
                                   'double-float))
                     (setf a
                           (coerce (the f2cl-lib:integer4 1)
                                   'double-float))
                     (setf c
                           (coerce (the f2cl-lib:integer4 1)
                                   'double-float))
                   label10
                     (cond ((= c one)
                            (setf a (* 2 a))
                            (setf c (dlamc3 a one))
                            (setf c (dlamc3 c (- a)))
                            (go label10)))
                     (setf b
                           (coerce (the f2cl-lib:integer4 1)
                                   'double-float))
                     (setf c (dlamc3 a b))
                   label20
                     (cond ((= c a)
                            (setf b (* 2 b))
                            (setf c (dlamc3 a b))
                            (go label20)))
                     (setf qtr (/ one 4))
                     (setf savec c)
                     (setf c (dlamc3 c (- a)))
                     (setf lbeta (f2cl-lib:int (+ c qtr)))
                     (setf b
                           (coerce (the f2cl-lib:integer4 lbeta)
                                   'double-float))
                     (setf f (dlamc3 (/ b 2) (/ (- b) 100)))
                     (setf c (dlamc3 f a))
                     (cond ((= c a) (setf lrnd f2cl-lib:%true%))
                           (t (setf lrnd f2cl-lib:%false%)))
                     (setf f (dlamc3 (/ b 2) (/ b 100)))
                     (setf c (dlamc3 f a))
                     (if (and lrnd (= c a))
                         (setf lrnd f2cl-lib:%false%))
                     (setf t1 (dlamc3 (/ b 2) a))
                     (setf t2 (dlamc3 (/ b 2) savec))
                     (setf lieee1 (and (= t1 a) (> t2 savec) lrnd))
                     (setf lt$ 0)
                     (setf a
                           (coerce (the f2cl-lib:integer4 1)
                                   'double-float))
                     (setf c
                           (coerce (the f2cl-lib:integer4 1)
                                   'double-float))
                   label30
                     (cond ((= c one)
                            (setf lt$ (f2cl-lib:int-add lt$ 1))
                            (setf a (* a lbeta))
                            (setf c (dlamc3 a one))
                            (setf c (dlamc3 c (- a)))
                            (go label30))))))
          (setf beta lbeta)
          (setf t$ lt$)
          (setf rnd lrnd)
          (setf ieee1 lieee1)
          (go end_label)
     end_label (return (values beta t$ rnd ieee1)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlamc1
                 fortran-to-lisp::*f2cl-function-info*)
        (fortran-to-lisp::make-f2cl-finfo :arg-types '((fortran-to-lisp::integer4)
                                                       (fortran-to-lisp::integer4)
                                                       fortran-to-lisp::logical
                                                       fortran-to-lisp::logical)
          :return-values '(fortran-to-lisp::beta fortran-to-lisp::t$
                           fortran-to-lisp::rnd fortran-to-lisp::ieee1)
          :calls '(fortran-to-lisp::dlamc3))))

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


(defun dlamc4 (emin start base)
  (declare (type (double-float) start)
   (type (f2cl-lib:integer4) base emin))
  (prog ((a 0.0) (b1 0.0) (b2 0.0) (c1 0.0) (c2 0.0) (d1 0.0) (d2 0.0)
         (one 0.0) (rbase 0.0) (zero 0.0) (i 0))
        (declare
         (type (double-float) zero rbase one d2 d1 c2 c1 b2 b1 a)
         (type (f2cl-lib:integer4) i))
        (setf a start)
        (setf one (coerce (the f2cl-lib:integer4 1) 'double-float))
        (setf rbase (/ one base))
        (setf zero (coerce (the f2cl-lib:integer4 0) 'double-float))
        (setf emin 1)
        (setf b1 (dlamc3 (* a rbase) zero))
        (setf c1 a)
        (setf c2 a)
        (setf d1 a)
        (setf d2 a)
   label10 (cond ((and (= c1 a) (= c2 a) (= d1 a) (= d2 a))
                  (setf emin (f2cl-lib:int-sub emin 1))
                  (setf a b1)
                  (setf b1 (dlamc3 (/ a base) zero))
                  (setf c1 (dlamc3 (* b1 base) zero))
                  (setf d1 zero)
                  (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                ((> i base) nil)
                                (tagbody (setf d1 (+ d1 b1)) label20))
                  (setf b2 (dlamc3 (* a rbase) zero))
                  (setf c2 (dlamc3 (/ b2 rbase) zero))
                  (setf d2 zero)
                  (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                ((> i base) nil)
                                (tagbody (setf d2 (+ d2 b2)) label30))
                  (go label10)))
        (go end_label)
   end_label (return (values emin nil nil))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlamc4
                 fortran-to-lisp::*f2cl-function-info*)
        (fortran-to-lisp::make-f2cl-finfo :arg-types '((fortran-to-lisp::integer4)
                                                       (double-float)
                                                       (fortran-to-lisp::integer4))
          :return-values '(fortran-to-lisp::emin nil nil)
          :calls '(fortran-to-lisp::dlamc3))))

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


(let* ((zero 0.0) (one 1.0))
  (declare (type (double-float 0.0 0.0) zero)
   (type (double-float 1.0 1.0) one) (ignorable zero one))
  (defun dlamc5 (beta p emin ieee emax rmax)
    (declare (type (double-float) rmax) (type f2cl-lib:logical ieee)
     (type (f2cl-lib:integer4) emax emin p beta))
    (prog ((oldy 0.0) (recbas 0.0) (y 0.0) (z 0.0) (exbits 0)
           (expsum 0) (i 0) (lexp 0) (nbits 0) (try 0) (uexp 0))
          (declare (type (double-float) oldy recbas y z)
           (type (f2cl-lib:integer4) exbits expsum i lexp nbits try
            uexp))
          (setf lexp 1)
          (setf exbits 1)
     label10 (setf try (f2cl-lib:int-mul lexp 2))
          (cond ((<= try (f2cl-lib:int-sub emin))
                 (setf lexp try)
                 (setf exbits (f2cl-lib:int-add exbits 1))
                 (go label10)))
          (cond ((= lexp (f2cl-lib:int-sub emin)) (setf uexp lexp))
                (t
                 (setf uexp try)
                 (setf exbits (f2cl-lib:int-add exbits 1))))
          (cond ((> (f2cl-lib:int-add uexp emin)
                    (f2cl-lib:int-add (f2cl-lib:int-sub lexp)
                                      (f2cl-lib:int-sub emin)))
                 (setf expsum (f2cl-lib:int-mul 2 lexp)))
                (t (setf expsum (f2cl-lib:int-mul 2 uexp))))
          (setf emax
                (f2cl-lib:int-sub (f2cl-lib:int-add expsum emin) 1))
          (setf nbits (f2cl-lib:int-add 1 exbits p))
          (cond ((and (= (mod nbits 2) 1) (= beta 2))
                 (setf emax (f2cl-lib:int-sub emax 1))))
          (cond (ieee (setf emax (f2cl-lib:int-sub emax 1))))
          (setf recbas (/ one beta))
          (setf z (- beta one))
          (setf y zero)
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1)) ((> i p) nil)
                        (tagbody
                            (setf z (* z recbas))
                            (if (< y one) (setf oldy y))
                            (setf y (dlamc3 y z))
                          label20))
          (if (>= y one) (setf y oldy))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1)) ((> i emax) nil)
                        (tagbody
                            (setf y (dlamc3 (* y beta) zero))
                          label30))
          (setf rmax y)
          (go end_label)
     end_label (return (values beta nil emin nil emax rmax)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlamc5
                 fortran-to-lisp::*f2cl-function-info*)
        (fortran-to-lisp::make-f2cl-finfo :arg-types '((fortran-to-lisp::integer4)
                                                       (fortran-to-lisp::integer4)
                                                       (fortran-to-lisp::integer4)
                                                       fortran-to-lisp::logical
                                                       (fortran-to-lisp::integer4)
                                                       (double-float))
          :return-values '(fortran-to-lisp::beta nil
                           fortran-to-lisp::emin nil
                           fortran-to-lisp::emax fortran-to-lisp::rmax)
          :calls '(fortran-to-lisp::dlamc3))))

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


(let ((lbeta 0)
      (lemax 0)
      (lemin 0)
      (leps 0.0)
      (lrmax 0.0)
      (lrmin 0.0)
      (lt$ 0)
      (first$ nil)
      (iwarn nil))
  (declare (type (f2cl-lib:integer4) lbeta lemax lemin)
   (type f2cl-lib:logical first$ iwarn)
   (type (double-float) leps lrmax lrmin) (type (integer) lt$))
  (setq first$ f2cl-lib:%true%)
  (setq iwarn f2cl-lib:%false%)
  (defun dlamc2 (beta t$ rnd eps emin rmin emax rmax)
    (declare (type (double-float) rmax rmin eps)
     (type f2cl-lib:logical rnd)
     (type (f2cl-lib:integer4) emax emin t$ beta))
    (prog ((a 0.0) (b 0.0) (c 0.0) (half 0.0) (one 0.0) (rbase 0.0)
           (sixth$ 0.0) (small 0.0) (third$ 0.0) (two 0.0) (zero 0.0)
           (gnmin 0) (gpmin 0) (i 0) (ngnmin 0) (ngpmin 0) (ieee nil)
           (lieee1 nil) (lrnd nil) (lt$ 0))
          (declare
           (type (double-float) zero two third$ small sixth$ rbase one
            half c b a)
           (type f2cl-lib:logical lrnd lieee1 ieee)
           (type (f2cl-lib:integer4) lt$ ngpmin ngnmin i gpmin gnmin))
          (cond (first$
                 (tagbody
                     (setf first$ f2cl-lib:%false%)
                     (setf zero
                           (coerce (the f2cl-lib:integer4 0)
                                   'double-float))
                     (setf one
                           (coerce (the f2cl-lib:integer4 1)
                                   'double-float))
                     (setf two
                           (coerce (the f2cl-lib:integer4 2)
                                   'double-float))
                     (multiple-value-bind (var-0 var-1 var-2 var-3)
                         (dlamc1 lbeta lt$ lrnd lieee1)
                       (declare (ignore))
                       (setf lbeta var-0)
                       (setf lt$ var-1)
                       (setf lrnd var-2)
                       (setf lieee1 var-3))
                     (setf b
                           (coerce (the f2cl-lib:integer4 lbeta)
                                   'double-float))
                     (setf a (expt b (f2cl-lib:int-sub lt$)))
                     (setf leps a)
                     (setf b (/ two 3))
                     (setf half (/ one 2))
                     (setf sixth$ (dlamc3 b (- half)))
                     (setf third$ (dlamc3 sixth$ sixth$))
                     (setf b (dlamc3 third$ (- half)))
                     (setf b (dlamc3 b sixth$))
                     (setf b (abs b))
                     (if (< b leps) (setf b leps))
                     (setf leps
                           (coerce (the f2cl-lib:integer4 1)
                                   'double-float))
                   label10
                     (cond ((and (> leps b) (> b zero))
                            (setf leps b)
                            (setf c
                                  (dlamc3 (* half leps)
                                   (* (expt two 5) (expt leps 2))))
                            (setf c (dlamc3 half (- c)))
                            (setf b (dlamc3 half c))
                            (setf c (dlamc3 half (- b)))
                            (setf b (dlamc3 half c))
                            (go label10)))
                     (if (< a leps) (setf leps a))
                     (setf rbase (/ one lbeta))
                     (setf small one)
                     (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                   ((> i 3) nil)
                                   (tagbody
                                       (setf small
                                             (dlamc3 (* small rbase)
                                              zero))
                                     label20))
                     (setf a (dlamc3 one small))
                     (multiple-value-bind (var-0 var-1 var-2)
                         (dlamc4 ngpmin one lbeta)
                       (declare (ignore var-1 var-2))
                       (setf ngpmin var-0))
                     (multiple-value-bind (var-0 var-1 var-2)
                         (dlamc4 ngnmin (- one) lbeta)
                       (declare (ignore var-1 var-2))
                       (setf ngnmin var-0))
                     (multiple-value-bind (var-0 var-1 var-2)
                         (dlamc4 gpmin a lbeta)
                       (declare (ignore var-1 var-2))
                       (setf gpmin var-0))
                     (multiple-value-bind (var-0 var-1 var-2)
                         (dlamc4 gnmin (- a) lbeta)
                       (declare (ignore var-1 var-2))
                       (setf gnmin var-0))
                     (setf ieee f2cl-lib:%false%)
                     (cond ((and (= ngpmin ngnmin) (= gpmin gnmin))
                            (cond ((= ngpmin gpmin)
                                   (setf lemin ngpmin))
                                  ((= (f2cl-lib:int-add gpmin
                                                        (f2cl-lib:int-sub ngpmin))
                                      3)
                                   (setf lemin
                                         (f2cl-lib:int-add (f2cl-lib:int-sub ngpmin
                                                                             1)
                                                           lt$))
                                   (setf ieee f2cl-lib:%true%))
                                  (t
                                   (setf lemin
                                         (min (the f2cl-lib:integer4
                                                   ngpmin)
                                              (the f2cl-lib:integer4
                                                   gpmin)))
                                   (setf iwarn f2cl-lib:%true%))))
                           ((and (= ngpmin gpmin) (= ngnmin gnmin))
                            (cond ((= (abs (f2cl-lib:int-add ngpmin
                                                             (f2cl-lib:int-sub ngnmin)))
                                      1)
                                   (setf lemin
                                         (max (the f2cl-lib:integer4
                                                   ngpmin)
                                              (the f2cl-lib:integer4
                                                   ngnmin))))
                                  (t
                                   (setf lemin
                                         (min (the f2cl-lib:integer4
                                                   ngpmin)
                                              (the f2cl-lib:integer4
                                                   ngnmin)))
                                   (setf iwarn f2cl-lib:%true%))))
                           ((and (= (abs (f2cl-lib:int-add ngpmin
                                                           (f2cl-lib:int-sub ngnmin)))
                                    1)
                                 (= gpmin gnmin))
                            (cond ((= (f2cl-lib:int-add gpmin
                                                        (f2cl-lib:int-sub (min (the f2cl-lib:integer4
                                                                                    ngpmin)
                                                                               (the f2cl-lib:integer4
                                                                                    ngnmin))))
                                      3)
                                   (setf lemin
                                         (f2cl-lib:int-add (f2cl-lib:int-sub (max (the f2cl-lib:integer4
                                                                                       ngpmin)
                                                                                  (the f2cl-lib:integer4
                                                                                       ngnmin))
                                                                             1)
                                                           lt$)))
                                  (t
                                   (setf lemin
                                         (min (the f2cl-lib:integer4
                                                   ngpmin)
                                              (the f2cl-lib:integer4
                                                   ngnmin)))
                                   (setf iwarn f2cl-lib:%true%))))
                           (t
                            (setf lemin
                                  (min (the f2cl-lib:integer4 ngpmin)
                                       (the f2cl-lib:integer4 ngnmin)
                                       (the f2cl-lib:integer4 gpmin)
                                       (the f2cl-lib:integer4 gnmin)))
                            (setf iwarn f2cl-lib:%true%)))
                     (cond (iwarn
                            (setf first$ f2cl-lib:%true%)
                            (f2cl-lib:fformat 6
                                              ("~%" "~%"
                                               " WARNING. The value EMIN may be incorrect:-"
                                               "  EMIN = " 1 (("~8D"))
                                               "~%"
                                               " If, after inspection, the value EMIN looks"
                                               " acceptable please comment out "
                                               "~%"
                                               " the IF block as marked within the code of routine"
                                               " DLAMC2," "~%"
                                               " otherwise supply EMIN explicitly."
                                               "~%" "~%")
                                              lemin)))
                     (setf ieee (or ieee lieee1))
                     (setf lrmin
                           (coerce (the f2cl-lib:integer4 1)
                                   'double-float))
                     (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                   ((> i
                                       (f2cl-lib:int-add 1
                                                         (f2cl-lib:int-sub lemin)))
                                    nil)
                                   (tagbody
                                       (setf lrmin
                                             (dlamc3 (* lrmin rbase)
                                              zero))
                                     label30))
                     (multiple-value-bind (var-0 var-1 var-2 var-3
                                           var-4 var-5)
                         (dlamc5 lbeta lt$ lemin ieee lemax lrmax)
                       (declare (ignore var-1 var-3))
                       (setf lbeta var-0)
                       (setf lemin var-2)
                       (setf lemax var-4)
                       (setf lrmax var-5)))))
          (setf beta lbeta)
          (setf t$ lt$)
          (setf rnd lrnd)
          (setf eps leps)
          (setf emin lemin)
          (setf rmin lrmin)
          (setf emax lemax)
          (setf rmax lrmax)
          (go end_label)
     end_label (return (values beta t$ rnd eps emin rmin emax rmax)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlamc2
                 fortran-to-lisp::*f2cl-function-info*)
        (fortran-to-lisp::make-f2cl-finfo :arg-types '((fortran-to-lisp::integer4)
                                                       (fortran-to-lisp::integer4)
                                                       fortran-to-lisp::logical
                                                       (double-float)
                                                       (fortran-to-lisp::integer4)
                                                       (double-float)
                                                       (fortran-to-lisp::integer4)
                                                       (double-float))
          :return-values '(fortran-to-lisp::beta fortran-to-lisp::t$
                           fortran-to-lisp::rnd fortran-to-lisp::eps
                           fortran-to-lisp::emin fortran-to-lisp::rmin
                           fortran-to-lisp::emax fortran-to-lisp::rmax)
          :calls '(fortran-to-lisp::dlamc5 fortran-to-lisp::dlamc4
                   fortran-to-lisp::dlamc3 fortran-to-lisp::dlamc1))))

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
  (let ((eps 0.0)
        (sfmin 0.0)
        (base 0.0)
        (t$ 0.0f0)
        (rnd 0.0)
        (emin 0.0)
        (rmin 0.0)
        (emax 0.0)
        (rmax 0.0)
        (prec 0.0)
        (first$ nil))
    (declare (type (single-float) t$) (type f2cl-lib:logical first$)
     (type (double-float) eps sfmin base rnd emin rmin emax rmax prec))
    (setq first$ f2cl-lib:%true%)
    (defun dlamch (cmach)
      (declare (type (simple-array character (*)) cmach))
      (f2cl-lib:with-multi-array-data ((cmach character cmach-%data%
                                        cmach-%offset%))
        (prog ((rmach 0.0) (small 0.0) (t$ 0.0) (beta 0) (imax 0)
               (imin 0) (it 0) (lrnd nil) (dlamch 0.0))
              (declare (type (double-float) rmach small t$ dlamch)
               (type f2cl-lib:logical lrnd)
               (type (f2cl-lib:integer4) beta imax imin it))
              (cond (first$
                     (setf first$ f2cl-lib:%false%)
                     (multiple-value-bind (var-0 var-1 var-2 var-3
                                           var-4 var-5 var-6 var-7)
                         (dlamc2 beta it lrnd eps imin rmin imax rmax)
                       (declare (ignore))
                       (setf beta var-0)
                       (setf it var-1)
                       (setf lrnd var-2)
                       (setf eps var-3)
                       (setf imin var-4)
                       (setf rmin var-5)
                       (setf imax var-6)
                       (setf rmax var-7))
                     (setf base
                           (coerce (the f2cl-lib:integer4 beta)
                                   'double-float))
                     (setf t$
                           (coerce (the f2cl-lib:integer4 it)
                                   'double-float))
                     (cond (lrnd
                            (setf rnd one)
                            (setf eps
                                  (/ (expt base
                                           (f2cl-lib:int-sub 1 it))
                                     2)))
                           (t
                            (setf rnd zero)
                            (setf eps
                                  (expt base
                                        (f2cl-lib:int-sub 1 it)))))
                     (setf prec (* eps base))
                     (setf emin
                           (coerce (the f2cl-lib:integer4 imin)
                                   'double-float))
                     (setf emax
                           (coerce (the f2cl-lib:integer4 imax)
                                   'double-float))
                     (setf sfmin rmin)
                     (setf small (/ one rmax))
                     (cond ((>= small sfmin)
                            (setf sfmin (* small (+ one eps)))))))
              (cond ((lsame cmach "E") (setf rmach eps))
                    ((lsame cmach "S") (setf rmach sfmin))
                    ((lsame cmach "B") (setf rmach base))
                    ((lsame cmach "P") (setf rmach prec))
                    ((lsame cmach "N") (setf rmach t$))
                    ((lsame cmach "R") (setf rmach rnd))
                    ((lsame cmach "M") (setf rmach emin))
                    ((lsame cmach "U") (setf rmach rmin))
                    ((lsame cmach "L") (setf rmach emax))
                    ((lsame cmach "O") (setf rmach rmax)))
              (setf dlamch rmach)
              (go end_label)
         end_label (return (values dlamch nil)))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlamch
                 fortran-to-lisp::*f2cl-function-info*)
        (fortran-to-lisp::make-f2cl-finfo :arg-types '((simple-array
                                                        character
                                                        (1)))
          :return-values '(nil)
          :calls '(fortran-to-lisp::lsame fortran-to-lisp::dlamc2))))


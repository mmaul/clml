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

(in-package :clml.lapack)


(defun ilaenv (ispec name opts n1 n2 n3 n4)
  (declare (type (simple-array character (*)) opts name)
   (type (f2cl-lib:integer4) n4 n3 n2 n1 ispec))
  (f2cl-lib:with-multi-array-data ((name character name-%data%
                                    name-%offset%)
                                   (opts character opts-%data%
                                    opts-%offset%))
    (prog ((i 0) (ic 0) (iz 0) (nb 0) (nbmin 0) (nx 0)
           (subnam
            (make-array '(6)
              :element-type 'character
              :initial-element #\Space))
           (c3
            (make-array '(3)
              :element-type 'character
              :initial-element #\Space))
           (c2
            (make-array '(2)
              :element-type 'character
              :initial-element #\Space))
           (c4
            (make-array '(2)
              :element-type 'character
              :initial-element #\Space))
           (c1
            (make-array '(1)
              :element-type 'character
              :initial-element #\Space))
           (cname nil) (sname nil) (ilaenv 0))
          (declare (type (simple-array character (2)) c4 c2)
           (type (simple-array character (1)) c1)
           (type (simple-array character (3)) c3)
           (type f2cl-lib:logical sname cname)
           (type (simple-array character (6)) subnam)
           (type (f2cl-lib:integer4) ilaenv nx nbmin nb iz ic i))
          (f2cl-lib:computed-goto (label100 label100 label100 label400
                                   label500 label600 label700 label800
                                   label900 label1000 label1100)
                                  ispec)
          (setf ilaenv -1)
          (go end_label)
     label100 (setf ilaenv 1)
          (f2cl-lib:f2cl-set-string subnam name (string 6))
          (setf ic
                (f2cl-lib:ichar (f2cl-lib:fref-string subnam (1 1))))
          (setf iz (f2cl-lib:ichar "Z"))
          (cond ((or (= iz 90) (= iz 122))
                 (cond ((and (>= ic 97) (<= ic 122))
                        (f2cl-lib:fset-string (f2cl-lib:fref-string subnam
                                                                    (1
                                                                     1))
                                              (f2cl-lib:fchar (f2cl-lib:int-sub ic
                                                                                32)))
                        (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                                      ((> i 6) nil)
                                      (tagbody
                                          (setf ic
                                                (f2cl-lib:ichar (f2cl-lib:fref-string subnam
                                                                                      (i
                                                                                       i))))
                                          (if (and (>= ic 97)
                                                   (<= ic 122))
                                              (f2cl-lib:fset-string (f2cl-lib:fref-string subnam
                                                                                          (i
                                                                                           i))
                                                                    (f2cl-lib:fchar (f2cl-lib:int-sub ic
                                                                                                      32))))
                                        label10)))))
                ((or (= iz 233) (= iz 169))
                 (cond ((or (and (>= ic 129) (<= ic 137))
                            (and (>= ic 145) (<= ic 153))
                            (and (>= ic 162) (<= ic 169)))
                        (f2cl-lib:fset-string (f2cl-lib:fref-string subnam
                                                                    (1
                                                                     1))
                                              (f2cl-lib:fchar (f2cl-lib:int-add ic
                                                                                64)))
                        (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                                      ((> i 6) nil)
                                      (tagbody
                                          (setf ic
                                                (f2cl-lib:ichar (f2cl-lib:fref-string subnam
                                                                                      (i
                                                                                       i))))
                                          (if (or (and (>= ic 129)
                                                       (<= ic 137))
                                                  (and (>= ic 145)
                                                       (<= ic 153))
                                                  (and (>= ic 162)
                                                       (<= ic 169)))
                                              (f2cl-lib:fset-string (f2cl-lib:fref-string subnam
                                                                                          (i
                                                                                           i))
                                                                    (f2cl-lib:fchar (f2cl-lib:int-add ic
                                                                                                      64))))
                                        label20)))))
                ((or (= iz 218) (= iz 250))
                 (cond ((and (>= ic 225) (<= ic 250))
                        (f2cl-lib:fset-string (f2cl-lib:fref-string subnam
                                                                    (1
                                                                     1))
                                              (f2cl-lib:fchar (f2cl-lib:int-sub ic
                                                                                32)))
                        (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                                      ((> i 6) nil)
                                      (tagbody
                                          (setf ic
                                                (f2cl-lib:ichar (f2cl-lib:fref-string subnam
                                                                                      (i
                                                                                       i))))
                                          (if (and (>= ic 225)
                                                   (<= ic 250))
                                              (f2cl-lib:fset-string (f2cl-lib:fref-string subnam
                                                                                          (i
                                                                                           i))
                                                                    (f2cl-lib:fchar (f2cl-lib:int-sub ic
                                                                                                      32))))
                                        label30))))))
          (f2cl-lib:f2cl-set-string c1
                                    (f2cl-lib:fref-string subnam (1 1))
                                    (string 1))
          (setf sname
                (or (f2cl-lib:fstring-= c1 "S")
                    (f2cl-lib:fstring-= c1 "D")))
          (setf cname
                (or (f2cl-lib:fstring-= c1 "C")
                    (f2cl-lib:fstring-= c1 "Z")))
          (if (not (or cname sname)) (go end_label))
          (f2cl-lib:f2cl-set-string c2
                                    (f2cl-lib:fref-string subnam (2 3))
                                    (string 2))
          (f2cl-lib:f2cl-set-string c3
                                    (f2cl-lib:fref-string subnam (4 6))
                                    (string 3))
          (f2cl-lib:f2cl-set-string c4 (f2cl-lib:fref-string c3 (2 3))
                                    (string 2))
          (f2cl-lib:computed-goto (label110 label200 label300) ispec)
     label110 (setf nb 1)
          (cond ((f2cl-lib:fstring-= c2 "GE")
                 (cond ((f2cl-lib:fstring-= c3 "TRF")
                        (cond (sname (setf nb 64)) (t (setf nb 64))))
                       ((or (f2cl-lib:fstring-= c3 "QRF")
                            (f2cl-lib:fstring-= c3 "RQF")
                            (f2cl-lib:fstring-= c3 "LQF")
                            (f2cl-lib:fstring-= c3 "QLF"))
                        (cond (sname (setf nb 32)) (t (setf nb 32))))
                       ((f2cl-lib:fstring-= c3 "HRD")
                        (cond (sname (setf nb 32)) (t (setf nb 32))))
                       ((f2cl-lib:fstring-= c3 "BRD")
                        (cond (sname (setf nb 32)) (t (setf nb 32))))
                       ((f2cl-lib:fstring-= c3 "TRI")
                        (cond (sname (setf nb 64)) (t (setf nb 64))))))
                ((f2cl-lib:fstring-= c2 "PO")
                 (cond ((f2cl-lib:fstring-= c3 "TRF")
                        (cond (sname (setf nb 64)) (t (setf nb 64))))))
                ((f2cl-lib:fstring-= c2 "SY")
                 (cond ((f2cl-lib:fstring-= c3 "TRF")
                        (cond (sname (setf nb 64)) (t (setf nb 64))))
                       ((and sname (f2cl-lib:fstring-= c3 "TRD"))
                        (setf nb 32))
                       ((and sname (f2cl-lib:fstring-= c3 "GST"))
                        (setf nb 64))))
                ((and cname (f2cl-lib:fstring-= c2 "HE"))
                 (cond ((f2cl-lib:fstring-= c3 "TRF") (setf nb 64))
                       ((f2cl-lib:fstring-= c3 "TRD") (setf nb 32))
                       ((f2cl-lib:fstring-= c3 "GST") (setf nb 64))))
                ((and sname (f2cl-lib:fstring-= c2 "OR"))
                 (cond ((f2cl-lib:fstring-= (f2cl-lib:fref-string c3
                                                                  (1
                                                                   1))
                                            "G")
                        (cond ((or (f2cl-lib:fstring-= c4 "QR")
                                   (f2cl-lib:fstring-= c4 "RQ")
                                   (f2cl-lib:fstring-= c4 "LQ")
                                   (f2cl-lib:fstring-= c4 "QL")
                                   (f2cl-lib:fstring-= c4 "HR")
                                   (f2cl-lib:fstring-= c4 "TR")
                                   (f2cl-lib:fstring-= c4 "BR"))
                               (setf nb 32))))
                       ((f2cl-lib:fstring-= (f2cl-lib:fref-string c3
                                                                  (1
                                                                   1))
                                            "M")
                        (cond ((or (f2cl-lib:fstring-= c4 "QR")
                                   (f2cl-lib:fstring-= c4 "RQ")
                                   (f2cl-lib:fstring-= c4 "LQ")
                                   (f2cl-lib:fstring-= c4 "QL")
                                   (f2cl-lib:fstring-= c4 "HR")
                                   (f2cl-lib:fstring-= c4 "TR")
                                   (f2cl-lib:fstring-= c4 "BR"))
                               (setf nb 32))))))
                ((and cname (f2cl-lib:fstring-= c2 "UN"))
                 (cond ((f2cl-lib:fstring-= (f2cl-lib:fref-string c3
                                                                  (1
                                                                   1))
                                            "G")
                        (cond ((or (f2cl-lib:fstring-= c4 "QR")
                                   (f2cl-lib:fstring-= c4 "RQ")
                                   (f2cl-lib:fstring-= c4 "LQ")
                                   (f2cl-lib:fstring-= c4 "QL")
                                   (f2cl-lib:fstring-= c4 "HR")
                                   (f2cl-lib:fstring-= c4 "TR")
                                   (f2cl-lib:fstring-= c4 "BR"))
                               (setf nb 32))))
                       ((f2cl-lib:fstring-= (f2cl-lib:fref-string c3
                                                                  (1
                                                                   1))
                                            "M")
                        (cond ((or (f2cl-lib:fstring-= c4 "QR")
                                   (f2cl-lib:fstring-= c4 "RQ")
                                   (f2cl-lib:fstring-= c4 "LQ")
                                   (f2cl-lib:fstring-= c4 "QL")
                                   (f2cl-lib:fstring-= c4 "HR")
                                   (f2cl-lib:fstring-= c4 "TR")
                                   (f2cl-lib:fstring-= c4 "BR"))
                               (setf nb 32))))))
                ((f2cl-lib:fstring-= c2 "GB")
                 (cond ((f2cl-lib:fstring-= c3 "TRF")
                        (cond (sname
                               (cond ((<= n4 64) (setf nb 1))
                                     (t (setf nb 32))))
                              (t
                               (cond ((<= n4 64) (setf nb 1))
                                     (t (setf nb 32))))))))
                ((f2cl-lib:fstring-= c2 "PB")
                 (cond ((f2cl-lib:fstring-= c3 "TRF")
                        (cond (sname
                               (cond ((<= n2 64) (setf nb 1))
                                     (t (setf nb 32))))
                              (t
                               (cond ((<= n2 64) (setf nb 1))
                                     (t (setf nb 32))))))))
                ((f2cl-lib:fstring-= c2 "TR")
                 (cond ((f2cl-lib:fstring-= c3 "TRI")
                        (cond (sname (setf nb 64)) (t (setf nb 64))))))
                ((f2cl-lib:fstring-= c2 "LA")
                 (cond ((f2cl-lib:fstring-= c3 "UUM")
                        (cond (sname (setf nb 64)) (t (setf nb 64))))))
                ((and sname (f2cl-lib:fstring-= c2 "ST"))
                 (cond ((f2cl-lib:fstring-= c3 "EBZ") (setf nb 1)))))
          (setf ilaenv nb)
          (go end_label)
     label200 (setf nbmin 2)
          (cond ((f2cl-lib:fstring-= c2 "GE")
                 (cond ((or (f2cl-lib:fstring-= c3 "QRF")
                            (f2cl-lib:fstring-= c3 "RQF")
                            (f2cl-lib:fstring-= c3 "LQF")
                            (f2cl-lib:fstring-= c3 "QLF"))
                        (cond (sname (setf nbmin 2))
                              (t (setf nbmin 2))))
                       ((f2cl-lib:fstring-= c3 "HRD")
                        (cond (sname (setf nbmin 2))
                              (t (setf nbmin 2))))
                       ((f2cl-lib:fstring-= c3 "BRD")
                        (cond (sname (setf nbmin 2))
                              (t (setf nbmin 2))))
                       ((f2cl-lib:fstring-= c3 "TRI")
                        (cond (sname (setf nbmin 2))
                              (t (setf nbmin 2))))))
                ((f2cl-lib:fstring-= c2 "SY")
                 (cond ((f2cl-lib:fstring-= c3 "TRF")
                        (cond (sname (setf nbmin 8))
                              (t (setf nbmin 8))))
                       ((and sname (f2cl-lib:fstring-= c3 "TRD"))
                        (setf nbmin 2))))
                ((and cname (f2cl-lib:fstring-= c2 "HE"))
                 (cond ((f2cl-lib:fstring-= c3 "TRD") (setf nbmin 2))))
                ((and sname (f2cl-lib:fstring-= c2 "OR"))
                 (cond ((f2cl-lib:fstring-= (f2cl-lib:fref-string c3
                                                                  (1
                                                                   1))
                                            "G")
                        (cond ((or (f2cl-lib:fstring-= c4 "QR")
                                   (f2cl-lib:fstring-= c4 "RQ")
                                   (f2cl-lib:fstring-= c4 "LQ")
                                   (f2cl-lib:fstring-= c4 "QL")
                                   (f2cl-lib:fstring-= c4 "HR")
                                   (f2cl-lib:fstring-= c4 "TR")
                                   (f2cl-lib:fstring-= c4 "BR"))
                               (setf nbmin 2))))
                       ((f2cl-lib:fstring-= (f2cl-lib:fref-string c3
                                                                  (1
                                                                   1))
                                            "M")
                        (cond ((or (f2cl-lib:fstring-= c4 "QR")
                                   (f2cl-lib:fstring-= c4 "RQ")
                                   (f2cl-lib:fstring-= c4 "LQ")
                                   (f2cl-lib:fstring-= c4 "QL")
                                   (f2cl-lib:fstring-= c4 "HR")
                                   (f2cl-lib:fstring-= c4 "TR")
                                   (f2cl-lib:fstring-= c4 "BR"))
                               (setf nbmin 2))))))
                ((and cname (f2cl-lib:fstring-= c2 "UN"))
                 (cond ((f2cl-lib:fstring-= (f2cl-lib:fref-string c3
                                                                  (1
                                                                   1))
                                            "G")
                        (cond ((or (f2cl-lib:fstring-= c4 "QR")
                                   (f2cl-lib:fstring-= c4 "RQ")
                                   (f2cl-lib:fstring-= c4 "LQ")
                                   (f2cl-lib:fstring-= c4 "QL")
                                   (f2cl-lib:fstring-= c4 "HR")
                                   (f2cl-lib:fstring-= c4 "TR")
                                   (f2cl-lib:fstring-= c4 "BR"))
                               (setf nbmin 2))))
                       ((f2cl-lib:fstring-= (f2cl-lib:fref-string c3
                                                                  (1
                                                                   1))
                                            "M")
                        (cond ((or (f2cl-lib:fstring-= c4 "QR")
                                   (f2cl-lib:fstring-= c4 "RQ")
                                   (f2cl-lib:fstring-= c4 "LQ")
                                   (f2cl-lib:fstring-= c4 "QL")
                                   (f2cl-lib:fstring-= c4 "HR")
                                   (f2cl-lib:fstring-= c4 "TR")
                                   (f2cl-lib:fstring-= c4 "BR"))
                               (setf nbmin 2)))))))
          (setf ilaenv nbmin)
          (go end_label)
     label300 (setf nx 0)
          (cond ((f2cl-lib:fstring-= c2 "GE")
                 (cond ((or (f2cl-lib:fstring-= c3 "QRF")
                            (f2cl-lib:fstring-= c3 "RQF")
                            (f2cl-lib:fstring-= c3 "LQF")
                            (f2cl-lib:fstring-= c3 "QLF"))
                        (cond (sname (setf nx 128)) (t (setf nx 128))))
                       ((f2cl-lib:fstring-= c3 "HRD")
                        (cond (sname (setf nx 128)) (t (setf nx 128))))
                       ((f2cl-lib:fstring-= c3 "BRD")
                        (cond (sname (setf nx 128))
                              (t (setf nx 128))))))
                ((f2cl-lib:fstring-= c2 "SY")
                 (cond ((and sname (f2cl-lib:fstring-= c3 "TRD"))
                        (setf nx 32))))
                ((and cname (f2cl-lib:fstring-= c2 "HE"))
                 (cond ((f2cl-lib:fstring-= c3 "TRD") (setf nx 32))))
                ((and sname (f2cl-lib:fstring-= c2 "OR"))
                 (cond ((f2cl-lib:fstring-= (f2cl-lib:fref-string c3
                                                                  (1
                                                                   1))
                                            "G")
                        (cond ((or (f2cl-lib:fstring-= c4 "QR")
                                   (f2cl-lib:fstring-= c4 "RQ")
                                   (f2cl-lib:fstring-= c4 "LQ")
                                   (f2cl-lib:fstring-= c4 "QL")
                                   (f2cl-lib:fstring-= c4 "HR")
                                   (f2cl-lib:fstring-= c4 "TR")
                                   (f2cl-lib:fstring-= c4 "BR"))
                               (setf nx 128))))))
                ((and cname (f2cl-lib:fstring-= c2 "UN"))
                 (cond ((f2cl-lib:fstring-= (f2cl-lib:fref-string c3
                                                                  (1
                                                                   1))
                                            "G")
                        (cond ((or (f2cl-lib:fstring-= c4 "QR")
                                   (f2cl-lib:fstring-= c4 "RQ")
                                   (f2cl-lib:fstring-= c4 "LQ")
                                   (f2cl-lib:fstring-= c4 "QL")
                                   (f2cl-lib:fstring-= c4 "HR")
                                   (f2cl-lib:fstring-= c4 "TR")
                                   (f2cl-lib:fstring-= c4 "BR"))
                               (setf nx 128)))))))
          (setf ilaenv nx)
          (go end_label)
     label400 (setf ilaenv 6)
          (go end_label)
     label500 (setf ilaenv 2)
          (go end_label)
     label600 (setf ilaenv
                    (f2cl-lib:int (* (f2cl-lib:freal (min (the f2cl-lib:integer4
                                                               n1)
                                                          (the f2cl-lib:integer4
                                                               n2)))
                                     1.6)))
          (go end_label)
     label700 (setf ilaenv 1)
          (go end_label)
     label800 (setf ilaenv 50)
          (go end_label)
     label900 (setf ilaenv 25)
          (go end_label)
     label1000 (setf ilaenv 1)
          (cond ((= ilaenv 1) (setf ilaenv (ieeeck 0 0.0 1.0))))
          (go end_label)
     label1100 (setf ilaenv 1)
          (cond ((= ilaenv 1) (setf ilaenv (ieeeck 1 0.0 1.0))))
          (go end_label)
     end_label (return (values ilaenv nil nil nil nil nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::ilaenv
                 fortran-to-lisp::*f2cl-function-info*)
        (fortran-to-lisp::make-f2cl-finfo :arg-types '((fortran-to-lisp::integer4)
                                                       (simple-array
                                                        character
                                                        (*))
                                                       (simple-array
                                                        character
                                                        (*))
                                                       (fortran-to-lisp::integer4)
                                                       (fortran-to-lisp::integer4)
                                                       (fortran-to-lisp::integer4)
                                                       (fortran-to-lisp::integer4))
          :return-values '(nil nil nil nil nil nil nil)
          :calls '(fortran-to-lisp::ieeeck))))

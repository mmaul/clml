;;;; clml.asd
(defpackage :clml-system (:use :common-lisp :asdf))
(in-package :clml-system)

(defun call-with-clml-environment (fun)
  (let ((*read-default-float-format* 'double-float))
    (funcall fun)))

#+sbcl (declaim (sb-ext:muffle-conditions sb-ext:compiler-note))
#+sbcl (declaim (sb-ext:muffle-conditions sb-kernel:character-decoding-error-in-comment))

#+ (and has-mkl (or mswindows linux))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :mkl *features*))

#+(and allegro (version= 8 2))
(setq excl:*fasl-default-type* "fasl82")

#+(and allegro (not smp) (version= 9 0))
(setq excl:*fasl-default-type* "fasl90")

(eval-when (:compile-toplevel :load-toplevel :execute)
#+lispworksx
(progn
  (shadow 'concatenate-system)
  (shadowing-import'(defsys:defsystem defsys:load-system defsys:compile-system)))
#+lispworksx (use-package :defsys)

#+lispworksx
(defmacro concatenate-system (name destination)
  `(let* ((files (defsys::concatenate-system ,name ,destination))
          (system (eval `(lw:defsystem temp () :members ,files))))
     (lw:concatenate-system ,destination system)))


(asdf:defsystem #:clml
  :serial t
  :description "Reorginized CLML (Common Lisp Machine Learming) library from MSI"
  :author"
     Original Authors:
       Salvi Péter,
       Naganuma Shigeta,
       Tada Masashi,
       Abe Yusuke,
       Jianshi Huang,
       Fujii Ryo,
       Abe Seika,
       Kuroda Hisao
     Author Post MSI CLML Contribution:
       Mike Maul  <maul.mike@gmail.com>"
  :maintainer "Mike Maul  <maul.mike@gmail.com>"
  :license "LLGPL"
  :around-compile call-with-clml-environment
  :depends-on (
               :clml.hjs
               :clml.blas
               :clml.lapack
               :clml.statistics
               :clml.pca
               :clml.svm
               :clml.classifiers
               :clml.clustering
               :clml.decision-tree
               :clml.graph
               :clml.association-rule
               :clml.nearest-search
               :clml.nonparametric
               :clml.numeric
               :clml.som
               :clml.text
               :clml.time-series
               :clml.utility
               :clml.data
               )
  :components ((:file "package")
               )))


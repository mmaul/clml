;;;; clml.asd
(in-package "CL-USER")
(setq *read-default-float-format* 'double-float)
#+sbcl (declaim (sb-ext:muffle-conditions sb-kernel:character-decoding-error-in-comment))
(eval-when (:compile-toplevel :load-toplevel :execute)
  #+ignore                              ; don't use asdf in this project
  (require :asdf)
  #+sbcl (progn
           ;; Modules won't load if sb-fasl:*fasl-file-type* is not "fasl"
           ;; So load them first
           (loop for module in '(:sb-posix :sb-aclrepl :sb-bsd-sockets :sb-cltl2 :sb-cover
                                 :sb-introspect :sb-md5 :sb-rotate-byte :sb-sprof)
                 do (require module))))


;#+ (or sbcl allegro) 
;(eval-when (:compile-toplevel :load-toplevel :execute)
;  (pushnew :future *features*))

;#+(and unix (not future) (not lispworks))
;(eval-when (:compile-toplevel :load-toplevel :execute)
; (pushnew :fork-future *features*))

;#+(or mswindows linux)
;(eval-when (:compile-toplevel :load-toplevel :execute)
;  (pushnew :mkl *features*))

#+(and allegro (version= 8 2))
(setq excl:*fasl-default-type* "fasl82")

#+(and allegro (not smp) (version= 9 0))
(setq excl:*fasl-default-type* "fasl90")

(eval-when (:compile-toplevel :load-toplevel :execute)
#+lispworks
  (progn
    (shadow 'concatenate-system)
    (shadowing-import'(defsys:defsystem defsys:load-system defsys:compile-system)))
  #+lispworks
  (use-package :defsys)
  
  (loop while (not (eq *read-default-float-format* 'double-float))
        do
     (restart-case
         (error "Please set *read-default-float-format* to 'double-float before loading/compiling the system.")
       (use-double-float ()
         :report "Set double-float to *read-default-float-format*."
         (setq *read-default-float-format* 'double-float)))))


#+lispworks
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
  :depends-on (
               ;:fork-future
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
               
               )
  :components ((:file "package")
               ))



#+sbcl
(declaim (sb-ext:muffle-conditions sb-kernel:character-decoding-error-in-comment))

(defpackage :clml.hjs-environment (:use :common-lisp :asdf))
(in-package :clml.hjs-environment)

(defun call-with-environment (fun)
  (let ((*read-default-float-format* 'double-float))
    (funcall fun)))
#+sbcl (eval-when (:compile-toplevel :load-toplevel :execute)
         (progn
           ;; Modules won't load if sb-fasl:*fasl-file-type* is not "fasl"
           ;; So load them first
           (loop for module in '(:sb-posix :sb-aclrepl :sb-bsd-sockets :sb-cltl2 :sb-cover
                                 :sb-introspect :sb-md5 :sb-rotate-byte :sb-sprof)
              do (require module))))

(asdf:defsystem :clml.hjs
  :pathname "src/"
  :serial t
  :around-compile call-with-environment
  :depends-on (
               :alexandria
               :iterate
               :clml.blas
               :clml.lapack
               :future
               :clml.statistics
               :clml.utility
               )
  :components ((:file "package")
               (:file "utils")
               (:file "vars")
               (:file "vector")
               (:file "missing-value")
               (:file "matrix")
               (:file "read-data")
               (:file "eigenproblem")
               (:file "k-means")
               ))

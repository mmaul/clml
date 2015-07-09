#+sbcl
(declaim (sb-ext:muffle-conditions sb-kernel:character-decoding-error-in-comment))

(defpackage :clml.nonparametric-environment (:use :common-lisp :asdf))
(in-package :clml.nonparametric-environment)

(defun call-with--environment (fun)
  (let ((*read-default-float-format* 'double-float))
    (funcall fun)))

#+sbcl (eval-when (:compile-toplevel :load-toplevel :execute)
         (progn
           ;; Modules won't load if sb-fasl:*fasl-file-type* is not "fasl"
           ;; So load them first
           (loop for module in '(:sb-posix :sb-aclrepl :sb-bsd-sockets :sb-cltl2 :sb-cover
                                 :sb-introspect :sb-md5 :sb-rotate-byte :sb-sprof)
              do (require module))))


(asdf:defsystem :clml.nonparametric
                :pathname "src/"
                :serial t
                :around-compile call-with--environment
                :depends-on (
                             :clml.hjs
                             )
                :components ((:file "package")
                             (:file "statistics")
                             (:file "gamma")
                             (:file "dpm")
                             (:file "multi-dpm")
                             (:file "hdp-lda")
                             (:file "hdp")
                             (:file "hdp-hmm-class")
                             (:file "hdp-hmm")
                             (:file "gauss-hmm")
                             (:file "sticky-hdp-hmm")
                             (:file "blocked-hdp-hmm")
                             (:file "ihmm")
                             (:file "ftm")
                             (:file "dpm-interface")
                             ))

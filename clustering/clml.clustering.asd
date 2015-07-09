#+sbcl
(declaim (sb-ext:muffle-conditions sb-kernel:character-decoding-error-in-comment))

(defpackage :clml.nearest-search-environment (:use :common-lisp :asdf))
(in-package :clml.nearest-search-environment)

(defun call-with-environment (fun)
  (let ((*read-default-float-format* 'double-float))
    (funcall fun)))

(asdf:defsystem :clml.clustering
                :pathname "src/"
                :serial t
                :around-compile call-with-environment
                :depends-on (:clml.hjs
                             :clml.blas
                             :iterate
                             :clml.nearest-search
                             )
                :components (
                             (:file "package")
                             (:file "hc")
                             (:file "nmf")
                             (:file "optics")
                             (:file "spectral-clustering")
                             (:file "cluster-validation")
                             (:file "optics-speed")
                             (:file "k-means2")
                             
                             ))

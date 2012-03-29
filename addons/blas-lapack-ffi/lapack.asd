;;;; TODO: add copyright and license

(in-package :cl-user)


(asdf:defsystem lapack
  :description "A LAPACK binding."
  :author "MSI"
  :version "0.2.20100315"
  :depends-on (:blas)
  :components
  ((:module src
	    :components ((:file "lapack"))
	    :serial t
	    :perform
            (asdf:load-op :before (op c)
                          (progn
                            #+allegro (setf comp:*CLTL1-COMPILE-FILE-TOPLEVEL-COMPATIBILITY-P* nil)))
            :perform
            (asdf:load-op :after (op c)
                          (progn
                            #+allegro (setf comp:*CLTL1-COMPILE-FILE-TOPLEVEL-COMPATIBILITY-P*
                                            old-*cltl1-compile-file-toplevel-compatibility-p*))))))


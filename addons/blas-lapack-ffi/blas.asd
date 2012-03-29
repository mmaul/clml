;;;; TODO: add copyright and license

;;; TODO: make customized blas/lapack shared libraries
;; (cl:eval-when (:compile-toplevel :load-toplevel :execute)
;;   (loop for dir in (directory "addons/*/*.asd") do 
;;     (pushnew (make-pathname :directory (pathname-directory dir)) asdf:*central-registry* :test 'equal)))

(in-package :cl-user)

#+allegro
(defvar old-*cltl1-compile-file-toplevel-compatibility-p* comp:*cltl1-compile-file-toplevel-compatibility-p*)

(asdf:defsystem blas
  :description "A BLAS binding."
  :author "MSI"
  :version "0.2.20100315"
  :depends-on (:alexandria :cffi :iterate)
  :components
  ((:module src
	    :components ((:file "packages")
			 (:file "preload-for-customized-library")
			 (:file "ffi-utils" :depends-on ("packages"))
			 (:file "blas-lapack-common" :depends-on ("ffi-utils"))
			 (:file "blas" :depends-on ("blas-lapack-common")))
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


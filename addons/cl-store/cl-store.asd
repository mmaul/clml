;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.
(in-package #:cl-user)

(defpackage #:cl-store.system
  (:use #:cl #:asdf)
  (:export #:non-required-file))


(in-package #:cl-store.system)

#-(or lispworks mcl cmu clisp sbcl allegro ecl openmcl abcl)
(error "This is an unsupported lisp implementation.
Currently only MCL, OpenMCL, Lispworks, CMUCL, SBCL,
CLISP, ECL and AllegroCL are supported.")

(defclass non-required-file (cl-source-file) ()
  (:documentation
   "File containing implementation dependent code which may or may not be there."))

(defun lisp-system-shortname ()
  #+mcl :mcl #+lispworks :lispworks #+cmu :cmucl #+clisp :clisp #+sbcl :sbcl
  #+allegro :allegrocl #+ecl :ecl #+openmcl :openmcl #+abcl :abcl)

(defmethod component-pathname ((component non-required-file))
  (let ((pathname (call-next-method))
        (name (string-downcase (lisp-system-shortname))))
    (merge-pathnames
     (make-pathname :directory (list :relative name))
     pathname)))

(defmethod perform ((op compile-op) (component non-required-file))
  (when (probe-file (component-pathname component)) ;
    (call-next-method)))

(defmethod perform ((op load-op) (component non-required-file))
  (when (probe-file (component-pathname component))
    (call-next-method)))

(defmethod operation-done-p ((o operation) (c non-required-file))
  (when (probe-file (component-pathname c))
    (call-next-method)))

(defsystem cl-store
  :name "CL-STORE"
  :author "Sean Ross <sross@common-lisp.net>"
  :maintainer "Sean Ross <sross@common-lisp.net>"
  :version "0.7.12"
  :description "Serialization package"
  :long-description "Portable CL Package to serialize data"
  :licence "MIT"
  :serial t
  :components ((:file "package")
               #+(and clisp (not mop))
               (:non-required-file "mop")
               (:file "utils")
               (:file "backends")
               (:file "plumbing")
               (:file "circularities")
               (:file "default-backend")
               (:non-required-file "custom")))

(defmethod perform :after ((o load-op) (c (eql (find-system :cl-store))))
  (funcall (find-symbol "SETUP-SPECIAL-FLOATS" :cl-store))
  (provide 'cl-store))

(defmethod perform ((op test-op) (sys (eql (find-system :cl-store))))
  (oos 'load-op :cl-store-tests)
  (oos 'test-op :cl-store-tests))

(defsystem cl-store-tests
  :depends-on (rt cl-store)
  :components ((:file "tests")))

(defmethod perform ((op test-op) (sys (eql (find-system :cl-store-tests))))
  (funcall (find-symbol "RUN-TESTS" "CL-STORE-TESTS")
           (find-symbol "CL-STORE" "CL-STORE")))

;; EOF

(in-package #:common-lisp-user)

(defpackage #:metabang-bind-test
  (:use #:common-lisp #:lift #:metabang-bind)
  (:import-from #:metabang-bind
                #:bind-fix-nils-destructured
		#:bind-get-vars-from-lambda-list))
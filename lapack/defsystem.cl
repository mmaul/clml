(in-package :cl-user)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *read-default-float-format* 'double-float)
  (setf (logical-pathname-translations "lapack")
	(list (list "**;*.*.*"
		    (merge-pathnames (make-pathname :host (pathname-host *load-pathname*)
						    :directory '(:relative "." :wild-inferiors)
						    :name :wild :type :wild)
				     *load-pathname*)))))

(defsystem :lapack-package (:default-pathname "lapack:")
	   (:serial :blas-package
		    "lapack-package"))

(defsystem :lapack-real (:default-pathname "lapack:")
	   (:serial :lapack-package
		    "dlamch"
		    "dlapy2"
		    "dlartg"
		    "dgebak"
		    "dlabad"
		    "dladiv"
		    "dlaln2"
		    "dtrevc"
		    "dlarfx"
		    "dlarfg"
		    "dlacpy"
		    "dlassq"
		    "dlanhs"
		    "dlanv2"
		    "dlahqr"
		    "ieeeck"
		    "ilaenv"
		    "dlaset"
		    "dhseqr"
		    "dlarf"
		    "dlarft"
		    "dlarfb"
		    "dorg2r"
		    "dorgqr"
		    "dorghr"
		    "dlahrd"
		    "dgehd2"
		    "dgehrd"
		    "dgebal"
		    "dlascl"
		    "dlange"
		    "dgeev"
		    "dlasy2"
		    "dlaexc"
		    "dtrexc"
		    "dlacon"
		    "dlaqtr"
		    "dtrsna"
		    "dgeevx"
		    "dgetf2"
		    "dlaswp"
		    "dgetrf"
		    "dgetrs"
		    "dgesv"
		    "dorgl2"
		    "dorglq"
		    "dgelq2"
		    "dgelqf"
		    "dorgbr"
		    "dorm2r"
		    "dormqr"
		    "dorml2"
		    "dormlq"
		    "dormbr"
		    "dlasr"
		    "dlamrg"
		    "dlasd7"
		    "dlasd5"
		    "dlaed6"
		    "dlasd4"
		    "dlasd8"
		    "dlasd6"
		    "dlas2"
		    "dlasdt"
		    "dlasrt"
		    "dlasq4"
		    "dlasq5"
		    "dlasq6"
		    "dlasq3"
		    "dlasq2"
		    "dlasq1"
		    "dlasv2"
		    "dbdsqr"
		    "dlasdq"
		    "dlasda"
		    "dlasd2"
		    "dlasd3"
		    "dlasd1"
		    "dlasd0"
		    "dlanst"
		    "dbdsdc"
		    "dlabrd"
		    "dgebd2"
		    "dgebrd"
		    "dgeqr2"
		    "dgeqrf"
		    "dgesdd"
		    "dgesvd"

		    ;; For condition numbers of the singular vectors
		    "ddisna"
		    ))

(defsystem :lapack (:default-pathname "lapack:")
	   (:serial :blas
		    :lapack-package
		    :lapack-real))

(defsystem :lapack-tests (:default-pathname "lapack:")
	   (:serial :lapack
		    "lapack-tests"))

(eval-when (load eval)
  (format t "~%To build, execute this:~%(excl:load-system :lapack :compile t)~%")
  (format t "~%To test, execute this:~%(excl:load-system :lapack-tests :compile t)~%"))

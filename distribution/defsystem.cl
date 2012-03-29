(in-package :cl-user)

(defsystem :rand (:default-pathname "rand/")
  (:serial "package"
	   "utilities"
	   "rand"))

(defsystem :distribution (:default-pathname ".")
  (:serial :rand
	   "package"
	   "utilities"
	   "math"
	   "distribution"
	   ))

(eval-when (load eval)
  (format t "~%To build, execute this:~%(excl:load-system :distribution :compile t)~%"))
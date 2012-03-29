(in-package :cl-user)

(defsystem :rand (:default-pathname ".")
  (:serial "package"
	   "utilities"
	   "rand"))

(eval-when (load eval)
  (format t "~%To build, execute this:~%(excl:load-system :rand :compile t)~%"))
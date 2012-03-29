(in-package :fork-future)

;; if we are running interactively with swank, make sure that we close
;; the swank connection in the children, or the child will screw up
;; communication between swank and the parent


(defun close-swank-connections ()
  (mapc #'(lambda (c)
            (swank::close-connection c nil nil))
        swank::*connections*)
  (setq swank::*connections* nil))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew 'close-swank-connections *after-fork-hooks*))


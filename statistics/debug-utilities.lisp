;;; -*- mode: lisp; syntax: common-lisp -*-

;;; Peter Salvi, 2008

(in-package :statistics)

(defmacro do-all-functions ((f &optional (package '*package*)) &body body)
  "Do BODY on all functions that are internal/external in PACKAGE, but
are not inherited."
  (let ((p (gensym "P")))
    `(let ((,p ,package))
       (do-all-symbols (,f ,p)
	 (when (and (fboundp ,f) (not (macro-function ,f))
		    (eq (symbol-package ,f) ,p))
	   ,@body)))))

(defun trace-all-functions ()
  "Trace all functions in the current package."
  (let (acc)
    (do-all-functions (f) (push (list 'trace f) acc))
    (eval (cons 'progn acc)))
  t)

(defun print-todo ()
  "Prints all function names / docs with TODO parts."
  (do-all-functions (f)
    (let ((doc (documentation f 'function)))
      (when (and doc (search "TODO:" doc))
	(format t "~a:~%~a~%---~%" (symbol-name f) doc))))
  t)

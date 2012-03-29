(defpackage :hjs.util.meta
  (:use :cl)
  (:nicknames :util)
  (:export #:with-unique-names
	   #:once-only
           #:defun-speedy
           #:defdoublefunc
           #:gethash-or-set
           #:dmat
	   #:dvec
	   #:cvec
	   #:make-dvec
	   #:array-index
           #:dfloat
	   #:+fl
	   #:-fl
	   #:*fl
	   #:/fl
           #:safe-/
	   #:batch-elt
           #:split-seq-odd-even
           #:d-expt
           #:d-exp
           #:get-underlying-1d-array
           #:vecs2mat
           #:vecs2flatmat
           #:mat2vecs
           #:flatmat2vecs
	   ))

(in-package :hjs.util.meta)

(setf iterate::*always-declare-variables* t)

(deftype dvec () '(simple-array double-float (*)))

(deftype cvec () '(simple-array simple-string (*)))

(deftype array-index () `(mod #.array-dimension-limit))

;;Convert a number to a double-float
(defmacro dfloat (x)
  `(the double-float (coerce ,x 'double-float)))

;;Check that a number returned from a user function is a double-float
(defun dfloat-check (x)
  (check-type x double-float)
  x)

(defmacro with-unique-names ((&rest bindings) &body body)
  `(let ,(loop for v in bindings collect `(,v (gensym)))
     ,@body))

(defmacro once-only (names &body body)
  ;; Do not touch this code unless you really know what you're doing.
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
       `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
	  ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
		,@body)))))

(defmacro defun-speedy (name lambda-list &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name ,lambda-list
       (declare (optimize speed)
		#+allegro (:faslmode :immediate))
       ,@body)
     #+allegro
     (define-compiler-macro ,name ,lambda-list
       `(let (,,@(loop 
                   for n in lambda-list 
                   collect ``(,',n ,,n)))
	  (declare (optimize speed))
	  ,@',body))

     ',name))

(defmacro defdoublefunc (name input-arguments)
  `(progn
     (declaim (ftype (function ,input-arguments double-float) ,name))
     #+allegro
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (get ',name 'sys::immed-args-call)
	     '(,(mapcar (constantly :lisp) input-arguments) double-float)))))

(defmacro gethash-or-set (key table gen-value)
  (with-unique-names (val present)
    (once-only (key table)
      `(multiple-value-bind (,val ,present)
           (gethash ,key ,table)
         (if ,present
             ,val
             (setf (gethash ,key ,table) ,gen-value))))))

;; (defmacro-driver (FOR var IN-1D-SIMPLE-ARRAY array)
;;   "All the elements of a 1d simple array. AllegroCL needs as it won't recognized in-vector as optimizaiton."
;;   (let ((keyword (if generate 'generate 'for)))
;;     (with-unique-names (idx arr)
;;       `(progn
;; 	 (with ,arr = ,array)
;; 	 (for ,idx index-of-vector ,array)
;; 	 (,keyword ,var next (aref ,arr ,idx))))))


(defmacro +fl (&rest double-floats)
  `(the double-float (+ ,@double-floats)))

(defmacro -fl (&rest double-floats)
  `(the double-float (- ,@double-floats)))

(defmacro *fl (&rest double-floats)
  `(the double-float (* ,@double-floats)))

(defmacro /fl (&rest double-floats)
  `(the double-float (/ ,@double-floats)))

(defmacro safe-/ (a b)
  "Returns 0.0d0 when dividing by zero."
  (with-unique-names (divider)
    `(let ((,divider ,b))
       (if (zerop ,divider)
           0.0d0
           (/ ,a ,divider)))))

;;@ function-type: sequence -> (integer) -> sequence
;;@ precondition:
;;@  - indexes must be non-negative integer
;;@  - result-type must be either 'list or 'vector
;;@ postcondition:
;;@  - elements in the result sequence will be arranged conforming to the same order of indexes
(defun batch-elt (seq indexes &key (result-type 'list))
  (assert (typep seq 'sequence))
  (assert (and (listp indexes)
	       (every (lambda (x) (>= x 0)) indexes)))
  (loop for i in indexes
     collect (elt seq i) into result
     finally (return (coerce result (if result-type result-type 'list)))))

(defun split-seq-odd-even (seq)
  (assert (typep seq 'sequence))
  (assert (evenp (length seq)))
  (do ((se seq (subseq se 2))
       (odds nil)
       (evens nil))
      ((= 0 (length se)) (values (nreverse odds) (nreverse evens)))
    (push (elt se 0) odds)
    (push (elt se 1) evens)))


;;; for optimization
#+allegro
(excl:without-package-locks
 (define-compiler-macro expt (&whole form base power)
   (if (eql power 2)
       (let ((b '#:base))
         `(let ((,b ,base))
            (* ,b ,b)))
     form)))

(declaim (ftype (function (double-float double-float) double-float) d-expt))
(declaim (inline d-expt))
#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ff:def-foreign-call (d-expt "pow") ((base :double) (power :double))
    :returning :double
    :arg-checking nil
    :call-direct t))
#-allegro
(defun d-expt (base power)
  (declare (type double-float base power))
  (expt base power))


(declaim (ftype (function (double-float) double-float) d-exp))
(declaim (inline d-exp))
#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ff:def-foreign-call (d-exp "exp") ((arg :double))
    :returning :double
    :arg-checking nil
    :call-direct t))
#-allegro
(defun d-exp (x)
  (declare (type double-float x))
  (exp x))

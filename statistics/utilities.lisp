;;; -*- mode: lisp; syntax: common-lisp -*-

;;; Peter Salvi, 2008

(in-package :statistics)

(defun sqr (x)
  (* x x))

(defun unit-random ()
  "A random number in the range \(0, 1]."
  (let ((rand (random 1.0d0)))
    (if (zerop rand) 1.0d0 rand)))

(defun binomial (n k)
  (cond ((= k 0) 1)
	((> k n) 0)
	(t (* (/ n k) (binomial (1- n) (1- k))))))

(defun linear-combination (a x b)
  "Gives A when X = 0 and B when X = 1."
  (+ (* (- 1 x) a) (* x b)))

(defun polynomial (coefficients x)
  "Evaluates a polynomial at X."
  (labels ((rec (lst)
	     (if (null lst)
		 0
		 (+ (* (rec (rest lst)) x) (first lst)))))
    (rec (reverse coefficients))))

(defun real-integer-p (object)
  (and (realp object) (= object (truncate object))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun concatenated-symbol (&rest lst)
    (let ((strlist (mapcar #'symbol-name lst)))
      (intern (string-upcase (apply #'concatenate 'string strlist))))))

(defmacro def-on-sorted (name (sequence &rest args) &body body)
  (let ((extended-name (concatenated-symbol name '-on-sorted)))
    `(progn
       (defun ,extended-name (,sequence ,@args)
	 ,@body)
       (defun ,name (,sequence ,@args)
	 (,extended-name (sort (copy-seq ,sequence) #'<) ,@args))
       (setf (documentation ',name 'function)
	     (documentation ',extended-name 'function))
       '(,name ,extended-name))))

(defun binary-search (x fn from to)
  "Searches for K in the integer interval [FROM, TO] where
FN\(K\) is closest to X."
  (loop with low = from and high = to
     while (> (- high low) 1)
     for mid = (+ low (floor (- high low) 2))
     do (if (< (funcall fn mid) x)
	    (setf low mid)
	    (setf high mid))
     finally (return (if (< (- x (funcall fn low)) (- (funcall fn high) x))
			 low
			 high))))

(defun real-binary-search (fn from to &optional (tolerance 1.0d-8))
  "Searches for the zero position of FN in the interval [FROM, TO]."
  (loop with low = from and high = to
     while (> (- high low) tolerance)
     for mid = (+ low (/ (- high low) 2.0d0))
     do (if (< (funcall fn mid) 0.0d0)
	    (setf low mid)
	    (setf high mid))
     finally (return mid)))

;; (defmacro conditional-swap-let (pred fn var-lists &body body)
;;   "If PRED is true, swap the values of every pair in VAR-LISTS, and execute
;; BODY in this environment. Call FN on the result, if PRED was true.
;; E.g. \(CONDITIONAL-SWAP-LET T #'1+ \(\(A B) \(C D)) ..BODY..) would swap the
;; values of A and B, and C and D, and return the result of the body + 1.
;; Uses code duplication."
;;   `(if ,pred
;;        (funcall
;; 	fn (let ,(mapcan (lambda (lst) (list lst (reverse lst))) var-lists)
;; 	     ,@body))
;;        (progn
;; 	 ,@body)))

(defmacro conditional-swap-let (pred fn var-lists &body body)
  "If PRED is true, swap the values of every pair in VAR-LISTS, and execute
BODY in this environment. Call FN on the result, if PRED was true.
E.g. \(CONDITIONAL-SWAP-LET T #'1+ \(\(A B) \(C D)) ..BODY..) would swap the
values of A and B, and C and D, and return the result of the body + 1.
Uses SETF."
  (let ((vars (reduce #'append var-lists))
	(swapp (gensym "SWAPP"))
	(result (gensym "RESULT")))
    `(let ,(cons (list swapp pred)
		 (mapcar (lambda (var) (list var var)) vars))
       (progn
	 (when ,swapp
	   ,@(mapcar (lambda (pair) `(rotatef ,@pair)) var-lists))
	 (let ((,result (progn ,@body)))
	   (if ,swapp (funcall ,fn ,result) ,result))))))

(defmacro conditional-let* (pred fn var-val-lists &body body)
  "If PRED is true, bind the first, otherwise the second value of every
value pair in VAR-VAL-LISTS, and execute BODY in this environment. Call FN
on the result, if PRED was true."
  (let ((firstp (gensym "FIRSTP"))
	(result (gensym "RESULT")))
    `(let ,(cons (list firstp pred)
		 (mapcar #'first var-val-lists))
       (progn
	 (if ,firstp
	     ,(cons 'setf
		    (mapcan (lambda (pair) (list (first pair) (second pair)))
			    var-val-lists))
	     ,(cons 'setf
		    (mapcan (lambda (pair) (list (first pair) (third pair)))
			    var-val-lists)))
	 (let ((,result (progn ,@body)))
	   (if ,firstp (funcall ,fn ,result) ,result))))))

(defun count-values (seq &key (test #'equal))
  (let (alist)
    (do ((i 0 (+ i 1))
         (n (length seq)))
        ((= i n) alist)
      (declare (type integer i n))
      (let* ((val (elt seq i))
             (sub-alist (assoc val alist :test test)))
        (if sub-alist (incf (cdr sub-alist))
          (push (cons val 1) alist))))))
(in-package :rand)

(defmacro dfloat (x)
  `(the double-float (coerce ,x 'double-float)))

(declaim (ftype (function (symbol) (double-float 0d0 1d0)) unit-random))
(defun unit-random (&optional mode)
  "A random number in the range [0, 1\), \(0, 1], [0, 1] or \(0, 1\)."
  (ecase mode
    (:[
     (random 1d0))
    (:]
     (let ((rand (random 1.0d0)))
       (declare (type double-float rand))
	(if (zerop rand) 1.0d0 rand)))
    (:[]
     (loop
       (let ((rand (random 1.0000000000000001d0)))
	 (declare (type double-float rand))
	 (unless (> rand 1.0d0)
	   (return rand)))))
    ((nil)
     (loop
       (let ((rand (random 1.0d0)))
	 (declare (type double-float rand))
	 (unless (zerop rand)
	   (return rand)))))))

;; this will make all randoms efficient -- but first patch excl:fast-random-double-float.
#+ignore
(define-compiler-macro unit-random (&optional mode)
  "A random number in the range [0, 1\), \(0, 1], [0, 1] or \(0, 1\)."
  (ecase mode
    (:[
     `(random 1d0))
    (:]
     `(let ((rand (random 1.0d0)))
	(declare (type double-float rand))
	(if (zerop rand) 1.0d0 rand)))
    (:[]
     `(loop
	(let ((rand (random 1.0000000000000001d0)))
	  (declare (type double-float rand))
	  (unless (> rand 1.0d0)
	    (return rand)))))
    ((nil)
     `(loop
	(let ((rand (random 1.0d0)))
	  (declare (type double-float rand))
	  (unless (zerop rand)
	    (return rand)))))))

;; some magic numbers

;; 7 or 8
(defconstant +zuggurat-k+ 7)

(defconstant +bit-operation-m+ (floor (log most-positive-fixnum 2)))

(defconstant +lookup-table-k+ 6)
(defconstant +lookup-table-l+
    (multiple-value-bind (fixnum remainder) (floor (/ +bit-operation-m+ +lookup-table-k+))
      (if (zerop remainder)
	  (- fixnum 1)
	fixnum)))

(defmacro defun-with-cached-values (name varlist &body body)
  (let (documentation declaration binding-list)
    (when (stringp (car body))
      (setf documentation (pop body)))
    (when (eq (car (car body)) 'declare)
      (setf declaration (pop body)))
    (setf binding-list (pop body))
    (let ((cached-name (intern (format nil "~A-~A" name 'cached)))
	  (cached-arglist
	   (let (ord opt)
	     (loop
		 with flag = nil
		 for var in varlist do
		   (when (and (not flag) (char= (aref (string var) 0) #\&))
		     (setf flag t))
		   (if flag
		       (push var opt)
		     (push var ord)))
	     (append (nreverse ord) (mapcar #'car binding-list) (nreverse opt)))))
      `(progn
	 (defun ,cached-name ,cached-arglist
	   ,@(when documentation
	       `(,documentation))
	   ,@(when declaration
	       `(,declaration))
	   ,@body)
	 (defun ,name ,varlist
	   ,@(when documentation
	       `(,documentation))
	   ,@(when declaration
	       `(,declaration))
	   (let* ,binding-list
	     ,@body))))))

(declaim (ftype (function (double-float) double-float) bernoulli))
(defun bernoulli (base-p)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float base-p))
  (assert (<= 0 base-p 1))
  (if (>= (unit-random) base-p)
      0d0
    1d0))

(declaim (ftype (function (double-float integer) double-float) int-power))
(defun int-power (double integer)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float double)
	   (type fixnum integer))
  (let ((y 1d0)
	(w double))
    (declare (type double-float y w))
    (loop repeat +bit-operation-m+ do
	  (when (= (logand integer 1) 1)
	    (setf y (* y w)))
	  (setf integer (ash integer -1))
	  (when (zerop integer)
	    (return y))
	  (setf w (* w w)))))

(declaim (ftype (function (double-float rational) double-float) half-integer-power))
(defun half-integer-power (double half-integer)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float double)
	   (type rational half-integer))
  (multiple-value-bind (quotient remainder) (floor half-integer)
    (declare (type fixnum quotient)
	     (type rational remainder))
    (if (zerop remainder)
	(int-power double quotient)
      (* (int-power double quotient) (the double-float (sqrt double))))))

(declaim (ftype (function (fixnum fixnum) fixnum) combination))
(defun combination (bag choice)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type fixnum bag choice))
  (when (> (* choice 2) bag)
    (setf choice (- bag choice)))
  (let ((ans 1))
    (declare (type fixnum ans))
    (loop for i fixnum from 1 to choice
	for j fixnum from bag downto 1 do
	  (setf ans (the fixnum (/ (the fixnum (* ans j)) i))))
    ans))

;; test utilites
(defun test-random-moment (fn &optional (times 10000000))
  (let ((sum 0d0)
	(sum-of-square 0d0))
    (time (dotimes (i times)
	    (let ((r (funcall fn)))
	      (incf sum r)
	      (incf sum-of-square (* r r)))))
    (let ((ave (/ sum times)))
      (values (dfloat ave)
	      (dfloat (- (/ sum-of-square times) (* ave ave)))))))

(defun test-random-median (fn &optional (times 1000000) (testp t))
  (let ((buffer (make-array times :fill-pointer 0 :element-type 'double-float)))
    (time (dotimes (i times)
	    (vector-push (funcall fn 0d0 1d0) buffer)))
    (when testp
      (sort buffer #'<)
      (aref buffer (floor (/ times 2))))))
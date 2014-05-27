					; Eigensystems
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;Routines translated with permission by Kevin A. Broughan from ;;;;;;;;;;;
;;Numerical Recipies in Fortran Copyright (c) Numerical Recipies 1986, 1989;;;;
;;;;;;;;;;;;;;;Modified by Ken Olum for Common Lisp, April 1996;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :hjs.util.matrix)
;; functions:
;;	jacobi: eigenvalues and vectors of a symmetric matrix
;;	eigsrt: sorts eigenvectors into order by eigenvalue
;;	tred2: Householder reduction of a real symmetric matrix
;;	tqli: eigenvalues and vectors of a symmetric tridiagonal matrix
;;	balanc: balance a non-symmetric matrix
;;	elmhes: reduce a general matrix to Hessenberg form
;;	hqr: eigenvalues of a Hessenberg matrix
;;------------------------------------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Common-lisp compatibility for Numerical Recipes

;;Fortran-offset array-reference.  Fortran arrays start at 0, lisp
;;arrays at 1.

;;#+ignore
(defmacro fref (array &rest indicies)
  `(aref ,array
	 ,@(mapcar #'(lambda (index) `(1- ,index)) indicies)))

(defmacro fref1 (array i1)
  `(aref ,array (1- ,i1)))

(defmacro fref2 (array i1 i2)
  `(aref ,array (1- ,i1) (1- ,i2)))

;;Return X with the sign of Y.  Various routines like this exist
;;in the book, but they differ about what to do when Y is 0.
;;This one gives X rather than -X.
(defmacro signp (x y)
  `(if (minusp ,y) (- ,x) ,x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Eigen computations

(defun jacobi (a)
  (declare (type dmat a)) 

  (prog* ((nrot 0)
	  (n (array-dimension a 0))
	  (d (make-array n :element-type 'double-float :initial-element 0d0))
	  (v 
	   (make-array (list n n) :element-type 'double-float :initial-element 0d0))
	  (b (make-array n :element-type 'double-float :initial-element 0d0))
	  (z (make-array n :element-type 'double-float :initial-element 0d0))
	  (sm 0d0) (tresh 0d0) (g 0d0) (h 0d0) (t0 0d0) 
	  (theta 0d0) (c 0d0) (s 0d0) (tau 0d0))

     (declare (type dvec d)) 
     (declare (type dmat v)) 
     (declare (type dvec b)) 
     (declare (type dvec z))
     (declare (type fixnum n nrot))
     (declare (type double-float sm tresh g h t0 theta c s tau))
     


     (do ((ip 1 (+ ip 1)))
	 ((> ip n) t)
       (declare (type fixnum ip))
       (do ((iq 1 (+ iq 1)))
	   ((> iq n) t)
	 (declare (type fixnum iq))
	 (setf (fref  v ip iq) 0d0))
       (setf (fref v ip ip) 1d0)) 
     (do ((ip 1 (+ ip 1)))
	 ((> ip n) t)
       (declare (type fixnum ip))
       (setf (fref b ip) (fref a ip ip))
       (setf (fref d ip) (fref b ip))
       (setf (fref z ip) 0d0)) 
     (setf nrot 0) 
     (do ((i 1 (+ i 1)))
	 ((> i 50) t)
       (declare (type fixnum i))
       (setf sm 0d0)
       (do ((ip 1 (+ ip 1)))
	   ((> ip (+ n (- 1))) t)
	 (declare (type fixnum ip))
	 (do ((iq (+ ip 1) (+ iq 1)))
	     ((> iq n) t)
	   (declare (type fixnum iq))
	   (setf sm (+ sm (abs (fref a ip iq))))))
       
       (if (= sm 0d0) (go end))
       (if (< i 4) 
	   (setf tresh (/ (* 0.2d0 sm) (* (dfloat n) (dfloat n))))
	   (setf tresh 0d0))
       (do ((ip 1 (+ ip 1)))
	   ((> ip (+ n (- 1))) t)
	 (declare (type fixnum ip))
	 (do ((iq (+ ip 1) (+ iq 1)))
	     ((> iq n) t)
	   (declare (type fixnum iq))
	   (setf g (* 100d0 (abs (fref a ip iq))))
	   (cond 
	     ((and (> i 4)
		   (= (+ (abs (fref d ip)) g) (abs (fref d ip)))
		   (= (+ (abs (fref d iq)) g) (abs (fref d iq))))
	      (setf (fref a ip iq) 0d0))
	     ((> (abs (fref a ip iq)) tresh)            
	      (setf h (+ (fref d iq) (- (fref d ip))))

	      (cond 
		((= (+ (abs h) g) (abs h))
		 (setf t0 (/ (fref a ip iq) h)))
		(t
		 (setf theta (/ (* 0.5d0 h) (fref a ip iq)))
		 (setf t0 (/ 1d0 (+ (abs theta) (sqrt (the (double-float 0d0) (1+ (* theta theta)))))))
		 (if (< theta 0d0) (setf t0 (- t0)))))

	      (setf c (/ 1d0 (sqrt (the (double-float 0d0) (+ 1d0 (* t0 t0)))))) (setf s (* t0 c))
	      (setf tau (/ s (1+ c))) (setf h (* t0 (fref a ip iq)))
	      (setf (fref z ip) (+ (fref z ip) (- h)))
	      (setf (fref z iq) (+ (fref z iq) h))
	      (setf (fref d ip) (+ (fref d ip) (- h)))
	      (setf (fref d iq) (+ (fref d iq) h)) (setf (fref a ip iq) 0d0)
	      (do ((j 1 (+ j 1)))
		  ((> j (+ ip (- 1))) t)
		(declare (type fixnum j))
		(setf g (fref a j ip))
		(setf h (fref a j iq))
		(setf (fref a j ip) (+ g (* (- s) (+ h (* g tau)))))
		(setf (fref a j iq) (+ h (* s (+ g (* (- h) tau))))))
	      (do ((j (+ ip 1) (+ j 1)))
		  ((> j (+ iq (- 1))) t)
		(declare (type fixnum j))
		(setf g (fref a ip j))
		(setf h (fref a j iq))
		(setf (fref a ip j) (+ g (* (- s) (+ h (* g tau)))))
		(setf (fref a j iq) (+ h (* s (+ g (* (- h) tau))))))
	      (do ((j (+ iq 1) (+ j 1)))
		  ((> j n) t)
		(declare (type fixnum j))
		(setf g (fref a ip j))
		(setf h (fref a iq j))
		(setf (fref a ip j) (+ g (* (- s) (+ h (* g tau)))))
		(setf (fref a iq j) (+ h (* s (+ g (* (- h) tau))))))
	      (do ((j 1 (+ j 1)))
		  ((> j n) t)
		(declare (type fixnum j))
		(setf g (fref v j ip))
		(setf h (fref v j iq))
		(setf (fref v j ip) (+ g (* (- s) (+ h (* g tau)))))
		(setf (fref v j iq) (+ h (* s (+ g (* (- h) tau))))))
	      (setf nrot (+ nrot 1))))))
       (do ((ip 1 (+ ip 1)))
	   ((> ip n) t)
	 (declare (type fixnum ip))
	 (setf (fref b ip) (+ (fref b ip) (fref z ip)))
	 (setf (fref d ip) (fref b ip))
	 (setf (fref z ip) 0d0))) 
     (error "jacobi should not reach this point") 
     end
     (return (values d v nrot))))

					;------------------------------------------------------------------------------

(defun eigsrt (d v)
  (declare (type dvec d)) 
  (declare (type dmat v))

  (prog ((k 0) (n 0) (p 0d0))
     (declare (type fixnum k n))
     (declare (type double-float p))

     (setq n (array-dimension d 0))
     
     (do ((i 1 (+ i 1)))
	 ((> i (1- n)) t)
       (declare (type fixnum i))
       (setf k i)
       (setf p (aref  d (1- i)))
       (do ((j (+ i 1) (+ j 1)))
	   ((> j n) t)
	 (declare (type fixnum j))
	 (when (>= (aref d (1- j)) p) 
	   (setf k j)
	   (setf p (aref d (1- j)))))

       (when (not (= k 1))
	 (setf (aref d (1- k)) (aref d (1- i))) 
	 (setf (aref d (1- i)) p)
	 (do ((j 1 (+ j 1)))
	     ((> j n) t)
	   (declare (type fixnum j))
	   (setf p (aref v (1- j) (1- i)))
	   (setf (aref v (1- j) (1- i)) (aref v (1- j) (1- k)))
	   (setf (aref v (1- j) (1- k)) p)))) 
     
     (return (values d v))))

					;------------------------------------------------------------------------------
(defun tred2 (a &key (eigenvectors t))
  ;; (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (optimize (speed 3) (safety 1) (debug 0))) ; Due to spr36469
  (declare (type dmat a)) 
  (declare (type symbol eigenvectors))
  ;; (declare (:explain :boxing :inlining :types))
  ;; (assert (typep a 'dmat)
  ;;     (a)
  ;;   "Type of A is ~A: not dmat" (type-of a))

  ;; (print 'tred2)
  (prog* ((n (array-dimension a 0))
          (d (make-array n :element-type 'double-float :initial-element 0d0))
          (e (make-array n :element-type 'double-float :initial-element 0d0))
          (l 0) (h 0d0) 
          (scale 0d0) (f 0d0) (g 0d0) (hh 0d0))

     (declare (type dvec d)) 
     (declare (type dvec e)) 
     (declare (type fixnum n l))
     (declare (type (double-float 0.0 *) h))
     (declare (type double-float scale f g hh))
     (declare (dynamic-extent n l h scale f g hh))

     (do ((i n (1- i)))
	 ((< i 2) t)
       (declare (type fixnum i))
       (declare (dynamic-extent i))
       (setf l (1- i))
       (setf h 0d0)
       (setf scale 0d0)
       (cond 
	 ((> l 1)
	  (do ((k 1 (+ k 1)))
	      ((> k l) t)
	    (declare (type fixnum l))
	    (declare (dynamic-extent l))
	    (setf scale (+ scale (abs (fref2 a i k)))))
	  (cond 
	    ((= scale 0d0)
	     (setf (fref1 e i) (fref2 a i l)))
	    (t
	     (do ((k 1 (+ k 1)))
		 ((> k l) t)
	       (declare (type fixnum k))
	       (declare (dynamic-extent k))
	       (setf (fref2 a i k) (/ (fref2 a i k) scale))
	       (setf h (+ h (* (fref2 a i k) (fref2 a i k)))))
	     (setf f (fref2 a i l)) (setf g (- (signp (sqrt (the (double-float 0d0) h)) f)))
	     (setf (fref1 e i) (* scale g)) (setf h (+ h (* (- f) g)))
	     (setf (fref2 a i l) (+ f (- g))) (setf f 0d0)
	     (do ((j 1 (+ j 1)))
		 ((> j l) t)
	       (declare (type fixnum j))
	       (declare (dynamic-extent j))
	       (if eigenvectors (setf (fref2 a j i) (/ (fref2 a i j) h)))
	       (setf g 0d0)
	       (do ((k 1 (+ k 1)))
		   ((> k j) t)
		 (declare (type fixnum k))
		 (declare (dynamic-extent k))
		 (setf g (+ g (* (fref2 a j k) (fref2 a i k)))))
	       (do ((k (+ j 1) (+ k 1)))
		   ((> k l) t)
		 (declare (type fixnum k))
		 (declare (dynamic-extent k))
		 (setf g (+ g (* (fref2 a k j) (fref2 a i k)))))
	       (setf (fref1 e j) (/ g h))
	       (setf f (+ f (* (fref1 e j) (fref2 a i j)))))
	     (setf hh (/ f (+ h h)))
	     (do ((j 1 (+ j 1)))
		 ((> j l) t)
	       (declare (type fixnum j))
	       (declare (dynamic-extent j))
	       (setf f (fref2 a i j))
	       (setf g (+ (fref1 e j) (* (- hh) f)))
	       (setf (fref1 e j) g)
	       (do ((k 1 (+ k 1)))
		   ((> k j) t)
		 (declare (type fixnum k))
		 (declare (dynamic-extent k))
		 (setf (fref2 a j k) (+ (+ (fref2 a j k) (* (- f) (fref1 e k)))
					(* (- g) (fref2 a i k)))))))))
	 (t
	  (setf (fref1 e i) (fref2 a i l))))

       (setf (fref1 d i) h)) 

     (if eigenvectors (setf (fref1 d 1) 0d0))
     (setf (fref1 e 1) 0d0) 
     (do ((i 1 (+ i 1)))
	 ((> i n) t)
       (declare (type fixnum i))
       (declare (dynamic-extent i))
       (when eigenvectors
	 (setf l (+ i (- 1)))
	 (when 
	     (not (= (fref1 d i) 0d0))
	   (do ((j 1 (+ j 1)))
	       ((> j l) t)
	     (declare (type fixnum j))
	     (declare (dynamic-extent j))
	     (setf g 0d0)
	     (do ((k 1 (+ k 1)))
		 ((> k l) t)
	       (declare (type fixnum k))
	       (declare (dynamic-extent k))
	       (setf g (+ g (* (fref2 a i k) (fref2 a k j)))))
	     (do ((k 1 (+ k 1)))
		 ((> k l) t)
	       (declare (type fixnum k))
	       (declare (dynamic-extent k))
	       (setf (fref2 a k j) (+ (fref2 a k j) (* (- g) (fref2 a k i))))))))
       
       (setf (fref1 d i) (fref2 a i i))
       (when eigenvectors
	 (setf (fref2 a i i) 1d0)
	 (do ((j 1 (+ j 1)))
	     ((> j l) t)
	   (declare (type fixnum j))
	   (declare (dynamic-extent j))
	   (setf (fref2 a i j) 0d0)
	   (setf (fref2 a j i) 0d0)))) 
     
     (return (values a d e))))

					;------------------------------------------------------------------------------

(defun tqli (d e z &key (eigenvectors t))
  ;; (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (optimize (speed 3) (safety 1) (debug 0))) ; Due to spr36469
  (declare (type dvec d)) 
  (declare (type symbol eigenvectors))
  (declare (type dvec e)) 
  (declare (type dmat z)) 

  ;; (print 'tqli)
  (prog ((n 0) (m 0) (iter 0) (f 0d0) (s 0d0) (c 0d0) 
	 (b 0d0) (r 0d0) (g 0d0) (dd 0d0) (p 0d0)
         (itermax 30))
     (declare (type fixnum n m iter itermax))
     (declare (type double-float f s c b r g dd p))
     (declare (dynamic-extent n m iter f s c b r g dd p))

     (setq n (array-dimension d 0))
     (do ((i 2 (+ i 1)))
	 ((> i n) t)
       (declare (type fixnum i))
       (declare (dynamic-extent i))
       (setf (fref1 e (+ i (- 1))) (fref1 e i))) 
     (setf (fref1 e n) 0d0) 
     (do ((l 1 (+ l 1)))
	 ((> l n) t)
       (declare (type fixnum l))
       (declare (dynamic-extent l))
       (setf iter 0)
      label1
       (do ((mm l (+ mm 1)))
	   ((> mm (+ n (- 1))) t)
	 (declare (type fixnum mm))
	 (declare (dynamic-extent mm))
	 (setf dd (+ (abs (fref1 d mm)) (abs (fref1 d (+ mm 1)))))
	 (setq m mm)
	 (if (= (+ (abs (fref1 e mm)) dd) dd) (go label2)))
       (setf m n)
      label2
       (when (not (= m l))
         (when (<= itermax iter)
          (restart-case
              (error
              "Too many iterations in tqli for ~dD matrix: Current itermax=~d"
                     n itermax)
            (try-more (moreiters)
              :report "Try more iterations"
              :interactive
                  (lambda ()
                    (loop
                      (format t "Enter additional iteration counts: ")
                      (let ((n (read)))
                        (when (typep n '(integer 1))
                          (return (list n))))))
              (incf itermax (the fixnum moreiters))
              (format t "Restarted with new itermax=~d~%" itermax))))
         ;;
	 (setf iter (+ iter 1))
	 (setf g (/ (+ (fref1 d (+ l 1)) (- (fref1 d l))) (* 2d0 (fref1 e l))))
	 (setf r (sqrt (the (double-float 0.0 *) (+ (* g g) 1d0))))
	 (setf g (+ (+ (fref1 d m) (- (fref1 d l)))
		    (/ (fref1 e l) (+ g (signp r g)))))
	 (setf s 1d0) (setf c 1d0) (setf p 0d0)
	 (do ((i (+ m (- 1)) (+ i (- 1))))
	     ((< i l) t)
	   (declare (type fixnum i))
	   (declare (dynamic-extent i))
	   (setf f (* s (fref1 e i)))
	   (setf b (* c (fref1 e i)))
	   (cond 
	     ((>= (abs f) (abs g))
	      (setf c (/ g f))
	      (setf r (sqrt (the (double-float 0.0 *) (+ (* c c) 1d0)))) 
	      (setf (fref1 e (+ i 1)) (* f r))
	      (setf s (/ 1 r)) (setf c (* c s)) )
	     (t 
	      (setf s (/ f g))
	      (setf r (sqrt (the (double-float 0.0 *) (+ (* s s) 1d0))))
	      (setf (fref1 e (+ i 1)) (* g r))
	      (setf c (/ 1 r)) (setf s (* s c))))

	   (setf g (+ (fref1 d (+ i 1)) (- p)))
	   (setf r (+ (* (+ (fref1 d i) (- g)) s) (* (* 2 c) b)))
	   (setf p (* s r))
	   (setf (fref1 d (+ i 1)) (+ g p))
	   (setf g (+ (* c r) (- b)))
	   (when eigenvectors
	     (do ((k 1 (+ k 1)))
		 ((> k n) t)
	       (declare (type fixnum k))
	       (declare (dynamic-extent k))
	       (setf f (fref2 z k (+ i 1)))
	       (setf (fref2 z k (+ i 1)) (+ (* s (fref2 z k i)) (* c f)))
	       (setf (fref2 z k i) (+ (* c (fref2 z k i)) (* (- s) f))))))
	 (setf (fref1 d l) (+ (fref1 d l) (- p))) (setf (fref1 e l) g)
	 (setf (fref1 e m) 0d0)
	 (go label1))) 
     
     ;; (print 'tqli-exit)
     (return (values d z))))

					;------------------------------------------------------------------------------

(defun balanc (a &key (radix 2d0))
  (declare (type dmat a))
  (declare (type double-float radix))

  (prog ((n 0) (sqrdx 0d0) (c 0d0) (r 0d0) (last 0) (f 0d0)
	 (g 0d0) (s 0d0))
     (declare (type fixnum n last))
     (declare (type double-float sqrdx c r f g s))

     (setq sqrdx (* radix radix))
     (setq n (array-dimension a 0))
     label1
     (setf last 1)
     (do ((i 1 (+ i 1)))
	 ((> i n) t)
       (declare (type fixnum i))
       (setf c 0d0)
       (setf r 0d0)
       (do ((j 1 (+ j 1)))
	   ((> j n) t)
	 (declare (type fixnum j))
	 (when (not (= j i)) 
	   (setf c (+ c (abs (fref a j i))))
	   (setf r (+ r (abs (fref a i j))))))

       (when (and (not (= c 0d0)) (not (= r 0d0)))
	 (setf g (/ r radix))
	 (setf f 1d0)
	 (setf s (+ c r))

	 (tagbody label2

	    (when (< c g)
	      (setf f (* f radix))
	      (setf c (* c sqrdx))
	      (go label2)))

	 (setf g (* r radix))
	 (tagbody    label3
	    (when (> c g)
	      (setf f (/ f radix))
	      (setf c (/ c sqrdx))
	      (go label3)))

	 (when (< (/ (+ c r) f) (* 0.95d0 s))
	   (setf last 0)
	   (setf g (/ 1d0 f))
	   (do ((j 1 (+ j 1)))
	       ((> j n) t)
	     (declare (type fixnum j))
	     (setf (fref a i j) (* (fref a i j) g)))

	   (do ((j 1 (+ j 1)))
	       ((> j n) t)
	     (declare (type fixnum j))
	     (setf (fref a j i) (* (fref a j i) f))))))
     
     (if (= last 0) (go label1))
     
     (return a)))
					;-----------------------------------------------------------------------------

(defun elmhes (a)
  (declare (type dmat a))

  (prog ((n 0) (y 0d0) (x 0d0) (i 0)) 
     (declare (type fixnum n i) (type double-float y x))
     (setq n (array-dimension a 0))

     (do ((m 2 (+ m 1)))
	 ((>  m (1- n)) t)
       (declare (type fixnum m))
       (setf x 0d0)
       (setf i m)
       (do ((j m (+ j 1)))
	   ((> j n) t)
	 (declare (type fixnum j))
	 (when (> (abs (fref a j (1- m))) (abs x))
	   (setf x (fref a j (1- m))) 
	   (setf i j)))

       (when (not (= i m))
	 (do ((j (1- m) (+ j 1)))
	     ((> j n) t)
	   (declare (type fixnum j))
	   (setf y (fref a i j))
	   (setf (fref a i j) (fref a m j))
	   (setf (fref a m j) y))

	 (do ((j 1 (+ j 1)))
	     ((> j n) t)
	   (declare (type fixnum j))
	   (setf y (fref a j i))
	   (setf (fref a j i) (fref a j m))
	   (setf (fref a j m) y)))

       (when (not (= x 0d0))
	 (do ((i (1+ m) (+ i 1)))
	     ((> i n) t)
	   (declare (type fixnum i))
	   (setf y (fref a i (1- m)))
	   (when (not (= y 0d0))
	     (setf y (/ y x))
	     (setf (fref a i (1- m)) y)
	     (do ((j m (+ j 1)))
		 ((> j n) t)
	       (declare (type fixnum j))
	       (setf (fref a i j) (+ (fref a i j) (* (- y) (fref a m j)))))
	     (do ((j 1 (+ j 1)))
		 ((> j n) t)
	       (declare (type fixnum j))
	       (setf (fref a j m) (+ (fref a j m) (* y (fref a j i))))))))) 
     
     (return a)))

					;------------------------------------------------------------------------------

(defun hqr (a)
  (declare (type dmat a)) 

  (prog* (
	  (n (array-dimension a 0))
	  (wr (make-array n :element-type 'double-float :initial-element 0d0))
	  (wi (make-array n :element-type 'double-float :initial-element 0d0))
	  (t0 0d0) (nn 0) (s 0d0) 
	  (anorm 0d0) (its 0) (l 0) (x 0d0) (y 0d0) (m 0)
	  (w 0d0) (p 0d0) (q 0d0) (z 0d0) (r 0d0) (u 0d0) (v 0d0))

     (declare (type dvec wr)) 
     (declare (type dvec wi)) 
     (declare (type fixnum n nn its l m))
     (declare (type double-float t0 s anorm x y w p q z r u v))


     (setf anorm (abs (fref a 1 1))) 
     (do ((i 2 (+ i 1)))
	 ((> i n) t)
       (declare (type fixnum i))
       (do ((j (1- i) (1+ j)))
	   ((> j n) t)
	 (declare (type fixnum j))
	 (setf anorm (+ anorm (abs (fref a i j)))))) 
     (setf nn n) 
     (setf t0 0d0) 
     label1 
     (when 
	 (>= nn 1)  
       (tagbody
	  (setf its 0) 
	label2
	  (do ((ll nn (1- ll)))
	      ((< ll 2) t)
	    (declare (type fixnum ll))
	    (setq l ll)
	    (setf s (+ (abs (fref a (1- ll) (1- ll)))
		       (abs (fref a ll ll))))
	    (if (= s 0d0) (setf s anorm))
	    (if (= (+ (abs (fref a ll  (1- ll))) s) s) (go label3)))
	  (setf l 1) 
	label3 
	  (setf x (fref a nn nn))
	  (cond 
	    ((= l nn)  
	     (setf (fref wr nn) (+ x t0))
	     (setf (fref wi nn) 0d0) 
	     (setf nn (1- nn))) 
	    (t
	     (setf y (fref a (1- nn) (1- nn)))
	     (setf w (* (fref a nn (1- nn)) (fref a (1- nn) nn)))
	     (cond 
	       ((= l (1- nn))
		(setf p (* 0.5d0 (- y x))) (setf q (+ (expt p 2) w))
		(setf z (sqrt (abs q))) (setf x (+ x t0))
		(cond 
		  ((>= q 0d0) 
		   (setf z (+ p (signp z p)))
		   (setf (fref wr nn) (+ x z)) 
		   (setf (fref wr (1- nn)) (fref wr nn))
		   (if (not (= z 0d0)) (setf (fref wr nn) (+ x (/ (- w) z))))
		   (setf (fref wi nn) 0d0) 
		   (setf (fref wi (1- nn)) 0d0))
		  (t 
		   (setf (fref wr nn) (+ x p))
		   (setf (fref wr (1- nn)) (fref wr nn)) 
		   (setf (fref wi nn) z)
		   (setf (fref wi (1- nn)) (- z))))
		(setf nn (- nn 2))) 

	       (t (if (= its 30) (error " too many iterations in hqr "))

		  (when 
		      (or (= its 10) (= its 20))
		    (setf t0 (+ t0 x))

		    (do ((i 1 (+ i 1)))
			((> i nn) t)
		      (declare (type fixnum i))
		      (setf (fref a i i) (+ (fref a i i) (- x))))

		    (setf s (+ (abs (fref a nn (1- nn)))
			       (abs (fref a (1- nn) (- nn 2)))))
		    (setf x (* 0.75d0 s)) (setf y x) (setf w (* -0.4375d0 (expt s 2))))

		  (setf its (+ its 1))
		  (tagbody
		     (do ((mm (- nn 2) (1- mm)))
			 ((< mm l) t)
		       (declare (type fixnum mm))
		       (setq m mm)
		       (setf z (fref a mm mm))
		       (setf r (- x z))
		       (setf s (- y z))
		       (setf p (+ (/ (- (* r s) w) (fref a (+ mm 1) mm))
				  (fref a mm (+ mm 1))))
		       (setf q (- (- (- (fref a (+ mm 1) (+ mm 1)) z) r) s))
		       (setf r (fref a (+ mm 2) (+ mm 1)))
		       (setf s (+ (+ (abs p) (abs q)) (abs r)))
		       (setf p (/ p s))
		       (setf q (/ q s))
		       (setf r (/ r s))
		       (if (= mm l) (go label4))
		       (setf u (* (abs (fref a mm (1- mm))) (+ (abs q) (abs r))))
		       (setf v (* (abs p)
				  (+ (+ (abs (fref a (1- mm) (1- mm))) (abs z))
				     (abs (fref a (+ mm 1) (+ mm 1))))))
		       (if (= (+ u  v) v) (go label4)))

		   label4)
		  (do ((i (+ m 2) (+ i 1)))
		      ((> i nn) t)
		    (declare (type fixnum i))
		    (setf (fref a i (+ i -2)) 0d0)
		    (if (not (= i (+ 2 m))) (setf (fref a i (+ i -3)) 0d0)))

		  (do ((k m (+ k 1)))
		      ((> k (1- nn)) t)
		    (declare (type fixnum k))
		    (when 
			(not (= k m))
		      (setf p (fref a k (1- k)))
		      (setf q (fref a (+ k 1) (1- k))) (setf r 0d0)
		      (if (not (= k (1- nn)))
			  (setf r (fref a (+ k 2) (1- k))))
		      (setf x (+ (+ (abs p) (abs q)) (abs r)))
		      (when 
			  (not (= x 0d0))
			(setf p (/ p x))
			(setf q (/ q x)) 
			(setf r (/ r x))))
		    (setf s (signp (sqrt (+ (+ (expt p 2) (expt q 2)) (expt r 2))) p))

		    (when 
			(not (= s 0d0))
		      (if 
		       (= k m) 
		       (if 
			(not (= l m))
			(setf (fref a k (1- k)) (- (fref a k (1- k)))))
		       (setf (fref a k (1- k)) (* (- s) x)))

		      (setf p (+ p s)) 
		      (setf x (/ p s)) 
		      (setf y (/ q s)) 
		      (setf z (/ r s))
		      (setf q (/ q p)) 
		      (setf r (/ r p))

		      (do ((j k (+ j 1)))
			  ((> j nn) t)
			(declare (type fixnum j))
			(setf p (+ (fref a k j) (* q (fref a (+ k 1) j))))
			(when 
			    (not (= k (1- nn)))
			  (setf p (+ p (* r (fref a (+ k 2) j))))
			  (setf (fref a (+ k 2) j) (- (fref a (+ k 2) j) (* p z))))
			(setf (fref a (+ k 1) j) (- (fref a (+ k 1) j) (* p y)))
			(setf (fref a k j) (- (fref a k j) (* p x))))

		      (do ((i l (+ i 1)))
			  ((> i (min nn (+ k 3))) t)
			(declare (type fixnum i))
			(setf p (+ (* x (fref a i k)) (* y (fref a i (+ k 1)))))
			(when 
			    (not (= k (1- nn)))
			  (setf p (+ p (* z (fref a i (+ k 2)))))
			  (setf (fref a i (+ k 2)) (- (fref a i (+ k 2)) (* p r))))
			(setf (fref a i (+ k 1)) (- (fref a i (+ k 1)) (* p q)))
			(setf (fref a i k) (- (fref a i k) p)))))
		  (go label2)))
	     ))
	  (go label1))) 
     
     (return (values wr wi))))
					;------------------------------------------------------------------------------
					; end of nr11.l

(defun lu-decomposition (a &key (tiny 1.0d-20))
  (declare (type dmat a) (type double-float tiny))

  (let* ((size (array-dimension a 0))
         (index (make-array size :element-type 'fixnum :initial-element 0))
         (vv (make-array size :element-type 'double-float :initial-element 0d0))
         (d 1d0) (imax 0) (aamax 0d0)
         (sum 0d0) (dum 0d0))
    (declare (type (simple-array fixnum (*)) index)
             (type dvec vv)
             (type fixnum imax)
             (type double-float d aamax sum dum))

    (loop for i of-type fixnum below size
        do (setf aamax 0d0)
           (loop for j of-type fixnum below size
               as elmt = (abs (aref a i j))
               when (> elmt aamax) do (setf aamax elmt))
           (when (= aamax 0d0) (error "singular matrix passed to lu-decomposition"))
           (setf (aref vv i) (/ 1d0 aamax)))

    (loop for j of-type fixnum below size
        do (loop for i of-type fixnum below j
               do (setf sum (aref a i j))
                  (loop for k of-type fixnum below i
                      do (decf sum (* (the double-float (aref a i k))
                                      (the double-float (aref a k j)))))
                  (setf (aref a i j) sum))
           (setf aamax 0d0)
           (loop for i of-type fixnum from j below size
               do (setf sum (aref a i j))
                  (loop for k of-type fixnum below j
                      do (decf sum (* (the double-float (aref a i k))
                                      (the double-float (aref a k j)))))
                  (setf (aref a i j) sum
                        dum (* (the double-float (aref vv i))
                               (the double-float (abs sum))))
                  (when (>= dum aamax) 
                    (setf imax i aamax dum)))
           (when (not (= j imax))
             (loop for k of-type fixnum below size
                 do
                   (setf dum (aref a imax k)
                         (aref a imax k) (aref a j k)
                         (aref a j k) dum))
             (setf d (* -1d0 d))
             (setf (aref vv imax) (aref vv j)))

           (setf (aref index j) imax)
           (when (= (aref a j j) 0) (setf (aref a j j) tiny))
           (when (not (= (1- size) j))
             (setf dum (/ 1 (aref a j j)))
             (loop for i of-type fixnum from (1+ j) below size
                 do (setf (aref a i j) (* (the double-float (aref a i j))
                                          (the double-float dum))))))
    (values a index d)))

(defun lu-backsbst (a index b)
  (declare (type dmat a)
           (type (simple-array fixnum (*)) index) ; refers to 0 based array
           (type dvec b))
  (let ((n (array-dimension a 0))
        (ii 0) (sum 0d0) (ll 0))
    (declare (type fixnum n ii ll)
             (type double-float sum))
    (loop for i of-type fixnum from 1 to n
        do (setf ll (1+ (aref index (1- i)))
                 sum (aref b (1- ll))
                 (aref b (1- ll)) (aref b (1- i)))
           (cond ((not (= ii 0)) 
                  (loop for j of-type fixnum from ii below i
                      do (decf sum 
                               (* (the double-float (aref a (1- i) (1- j)))
                                  (the double-float (aref b (1- j)))))))
                 ((not (= sum 0d0))
                  (setf ii i)))
           (setf (aref b (1- i)) sum))

    (loop for i of-type fixnum from (1- n) downto 0
        do (setf sum (aref b i))
           (loop for j of-type fixnum from (1+ i) below n
               do (decf sum (* (the double-float (aref a i j))
                               (the double-float (aref b j)))))
           (setf (aref b i) (/ (the double-float sum)
                               (the double-float (aref a i i)))))
    b))




;;; exported routines
(eval-when (:compile-toplevel :load-toplevel :execute)
  (mapc #'export '(eigen-by-jacobi eigen-by-householder-ql eigen-by-power)))

(defun eigen-by-jacobi (a)
  (declare (type dmat a))
  (assert (typep a 'dmat)
	  (a)
	  "Type of array A is ~A: not (simple-arry double-float (* *))"
	  (type-of a))
  (jacobi (copy-mat a)))

(defun eigen-by-householder-ql (a)
  (declare (type dmat a))
  (assert (typep a 'dmat)
	  (a)
	  "Type of array A is ~A: not (simple-arry double-float (* *))"
	  (type-of a))
  (multiple-value-bind (a d e)
      (tred2 (copy-mat a))
    (tqli d e a)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; power method for eigenproblem ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this fcn. is destructive on 'mat'
(defun eigen-by-power (mat &key eigen-thld 
                                (from :max)
                                (precision 1d-8))
  "assume that mat is a positive definite matrix"
  (declare (type dmat mat))
  (assert (typep mat 'dmat)
      (mat)
	  "Type of array A is ~A: not (simple-arry double-float (* *))"
	  (type-of mat))
  (unless eigen-thld (setq eigen-thld (array-dimension mat 0)))
  (loop 
      with thld-type = (typecase eigen-thld
                         (float :float) (integer :integer)
                         (t (error "illegal value for eigen-thld: ~A" eigen-thld)))
      with thld = (cond ((and (eq thld-type :float)
                              (<= 0d0 eigen-thld 1d0))
                         (* (the double-float eigen-thld)
                            (the double-float (tr mat))))
                        ((and (eq thld-type :integer)
                              (<= 1 eigen-thld (array-dimension mat 0)))
                         eigen-thld)
                        (t (error "illegal value for eingen-thld: ~A" eigen-thld)))
      with cum-eigen = (case thld-type (:float 0d0) (:integer 0))
      with eigen-values
      with eigen-vectors
      while (< cum-eigen thld)
      do (multiple-value-bind (ev evec)
             (power-method mat :precision precision :method from)
           (incf cum-eigen ;; without concerning about negative eigen-value
                 (case thld-type (:float ev) (:integer 1)))
           (push ev eigen-values)
           (push evec eigen-vectors)
           (do-vec (r evec :type double-float :index-var row)
             (do-vec (c evec :type double-float :index-var col)
               (decf (aref mat row col)
                     (* (case from
                          (:max (the double-float ev))
                          (:min
                           (coerce most-positive-fixnum 'double-float)
                           ;; the value which is expected higher than next 'ev'
                           ;; but not so big for calculation ...
                           ))
                        (the double-float r)
                        (the double-float c))))))
      finally (return (values (specialize-vec (coerce (reverse eigen-values) 'vector))
                              (coerce (reverse eigen-vectors) 'vector)))))

(defun power-method (a &key (method :max)
                            (precision 1d-8)
                            initial-dvec)
  (declare (type dmat a))
  (let ((init-v (or initial-dvec
                    (make-array (array-dimension a 1) :initial-element 1d0
                                :element-type 'double-float)))
        (lin-eq-solver 
         (when (eq method :min)
           (let ((flag nil) (clone-a (copy-mat a)) index)
             (declare (type dmat clone-a) (type (simple-array fixnum (*)) index))
             (lambda (vec)
               (unless flag 
                 (setf index (second (multiple-value-list (lu-decomposition clone-a)))
                       flag t))
               (lu-backsbst clone-a index vec))))))
    (declare (type dvec init-v) (type function lin-eq-solver))
    (labels ((dvmax (v)
               (declare (type dvec v))
               (let ((m most-negative-double-float))
                 (do-vec (val v :type double-float :return m)
                   (when (> val m) (setf m val)))))
             (normalize (v result)
               (declare (type dvec v))
               (let ((m (dvmax v)))
                 (declare (type double-float m))
                 (do-vecs ((val v :type double-float)
                           (r result :type double-float :setf-var sf))
                   (declare (ignore r))
                   (setf sf (/ (the double-float val) (the double-float m))))))
             (standardize (v)
               (declare (type dvec v))
               (let ((sqr-sum 0d0)) 
                 (declare (type double-float sqr-sum))
                 (do-vec (val v :type double-float) (incf sqr-sum (expt val 2)))
                 (setq sqr-sum (sqrt sqr-sum))
                 (do-vec (val v :type double-float :setf-var sf :return v)
                   (setf sf (/ val sqr-sum))))))

      (loop with premax = (if initial-dvec (dvmax initial-dvec) 1d0)
          with next-v = (case method
                          (:max (m*v a init-v))
                          (:min
                           (funcall lin-eq-solver init-v)
                           ;;(solve-linear-eq a init-v)
                           ))
          with nvmax = (dvmax next-v)
          for count from 1
          as diff = (abs (- (the double-float premax) (the double-float nvmax)))
          while (< precision diff)
          do (when (> count (/ precision))
               (error 
                "It seems endless loop, unable to finish calculation for eigen pair: ~A" 
                diff))
             (normalize next-v init-v)
             (case method
               (:max (m*v a init-v next-v))
               (:min 
                (funcall lin-eq-solver init-v)
                ;; (setf next-v (solve-linear-eq a init-v))
                ))
             (setf premax nvmax nvmax (dvmax next-v))
          finally (return (values (case method (:max nvmax) (:min (/ nvmax)))
                                  (standardize next-v)))))))



;;;;;;;;;;;;;;;;;;;;;;;;
; mkl for eigenproblem ;
;;;;;;;;;;;;;;;;;;;;;;;;

;; symat-ev
;; function: Calculate eigenvalues and eigenvectors for symmetric-matrix
;; comment: This function assumes the input matrix is symmetric.
;;          And only the value of upper triangle is used for calculation.
;;          Destructive on 'sym-dmat'
;; inputs:
;;  sym-dmat : <dmat>, symmetric matrix
;;  eigen-thld : nil | 1 <= <integer>
;;  from : :min | :max
;;  vl : <double-float>, lower boundary for eigenvalue
;;  vu : <double-float>, upper boundary for eigenvalue
;;  abstol : <non-negative-double-float>, absolute value of tolerance
;;  type : :dsyevx | :dsyevr, use which lapack routine for calculation
;;                            :dsyevx is faster for a few selected eigenvalues
;; return: eigen-values
;;         eigen-vectors
#+mkl
(defun symat-ev (sym-dmat &key eigen-thld
                               (from :max) ;; :min | :max
                               (abstol 0d0)
                               (vl 0d0)
                               (vu 0d0)
                               (jobz "V")
                               (type :dsyevx) ;; :dsyevx | :dsyevr
                               )
  (declare (type dmat sym-dmat))
  (check-type sym-dmat dmat)
  (let* ((n (array-dimension sym-dmat 0))
         (range (cond ((and (typep eigen-thld 'integer)
                            (> n eigen-thld)) "I")
                      ((and (not (minusp vl)) (plusp (- vu vl))) "V")
                      (t "A")))
         (uplo "U")
         (lda (max 1 n))
         (il (if (string= range "I") (ecase from (:min 1) (:max (1+ (- n eigen-thld)))) 1))
         (iu (if (string= range "I") (ecase from (:min eigen-thld) (:max n)) n))
         (ldz (max 1 n))
         (lwork (ecase type (:dsyevx (max 1 (* 8 n))) (:dsyevr (max 1 (* 26 n)))))
         (liwork (ecase type (:dsyevx (max 1 (* 5 n))) (:dsyevr (max 1 (* 10 n)))))
         (work (make-dvec lwork))
         (iwork (make-array liwork :initial-element 0 :element-type '(unsigned-byte 32)))
         (m (1+ (- iu il)))
         (w (make-dvec (max 1 n)))
         (z (make-dmat m ldz))
         (info 0)
         (isuppz (when (eq type :dsyevr)
                   (make-array (* 2 (max 1 n)) :initial-element 0 :element-type '(unsigned-byte 32))))
         (ifail (when (eq type :dsyevx) 
                  (make-array (max 1 n) :initial-element 0 :element-type '(unsigned-byte 32)))))
    (ecase type
      (:dsyevr (mkl.lapack:dsyevr jobz range uplo n sym-dmat lda
                                  vl vu il iu abstol m w z 
                                  ldz isuppz work lwork iwork liwork info))
      (:dsyevx (mkl.lapack:dsyevx jobz range uplo n sym-dmat lda
                                  vl vu il iu abstol m w z
                                  ldz work lwork iwork ifail info)))
    (cond ((or (string= range "A") (string= range "V")) (values w z))
          ((string= range "I") 
           (ecase from
             (:min (values (subseq w 0 m) z))
             (:max (values (reverse (subseq w 0 m))
                           (adjust-array z (array-dimensions z)
                                         :initial-contents (loop for i from (1- m) downto 0
                                                               collect (row-aref z i))
                                         :element-type 'double-float)))))
          (t (error "Unexpected value | ~A" range)))))

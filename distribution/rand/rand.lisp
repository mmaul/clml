;;; 参考文献: 計算機シミュレーションのための確率分布乱数生成法, 四辻哲章, プレアデス出版, 2010
(in-package :rand)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; continuous-distributions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             ;;
;; Normal (Gauss) distribution ;;
;;                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Box-Muller transform -- make 2 standard-normal-random variable at 1 time by 2 unit-random
(let (tmp-value)
  (declare (type (or null double-float) tmp-value))
  (defun box-muller ()
    (declare (optimize (speed 3) (safety 0) (debug 0)))
    (if tmp-value
	(prog1 tmp-value (setf tmp-value nil))
      (let* ((a (unit-random))
	     (b (unit-random))
	     (sqr (sqrt (* -2.0d0 (the double-float (log a)))))
	     (angle (* 2.0d0 pi b)))
	(declare (type double-float a b sqr angle))
	(setf tmp-value
	  (* sqr (the double-float (sin angle))))
	(* sqr (the double-float (cos angle)))))))

;; Polar transform
(let (tmp-value)
  (declare (type (or null double-float) tmp-value))
  (defun gauss-polar ()
    (declare (optimize (speed 3) (safety 0) (debug 0)))
    (if tmp-value
	(prog1 tmp-value (setf tmp-value nil))
      (loop
	(let* ((a (unit-random))
	       (b (unit-random))
	       (v1 (- (* 2d0 a) 1d0))
	       (v2 (- (* 2d0 b) 1d0))
	       (v  (+ (* v1 v1) (* v2 v2))))
	  (declare (type double-float a b v1 v2 v))
	  (when (< 0d0 v 1d0)
	    (let ((w (sqrt (/ (* -2d0 (the double-float (log v))) v))))
	      (declare (type double-float w))
	      (setf tmp-value (* v1 w))
	      (return (* v2 w)))))))))

;; Monty Python method
(let* ((a (sqrt (log 4d0)))
       (b (sqrt (* 2d0 pi)))
       (d (* b b))
       (s (/ a (- b a)))
       (p (/ (+ s 1d0) 2d0))
       (q (log s)))
  (declare (type double-float a b d s p q))
  (defun gauss-monty-python ()
    (declare (optimize (speed 3) (safety 0) (debug 0)))
    (let ((u1 (* 2d0 (unit-random :[])))
	  (sign 1d0)
	  (ux 0d0))
      (declare (type double-float u1 ux sign))
      (if (> u1 1d0)
	  (setf sign -1d0
		ux (* b (the double-float (- u1 1d0))))
	(setf ux (* b u1)))
      (when (< ux a)
	(return-from gauss-monty-python (* sign ux)))
      (let ((u2 (/ (unit-random) 2d0)))
	(declare (type double-float u2))
	(when (< (the double-float (log u2)) (the double-float (- (/ (* ux ux) 2d0))))
	  (return-from gauss-monty-python (* sign ux)))
	(let ((y (* sign s (the double-float (- b ux)))))
	  (declare (type double-float y))
	  (if (< (the double-float (log (- p u2))) (the double-float (- q (/ (* y y) 2))))
	      (return-from gauss-monty-python y)
	    (loop
	      (let* ((v1 (unit-random))
		     (v2 (unit-random :[))
		     (x (sqrt (- d (* 2d0 (the double-float (log v1)))))))
		(declare (type double-float v1 v2 x))
		(when (<= (the double-float (* x v2)) b)
		  (return-from gauss-monty-python (* sign x)))))))))))

;; monty python with bit operation
(let* ((a (sqrt (log 4d0)))
       (b (sqrt (* 2d0 pi)))
       (d (* b b))
       (k (floor (* (- (expt 2 (floor +bit-operation-m+ 2)) 1)
		    (/ a b))))
       (w (/ b (- (expt 2 (floor +bit-operation-m+ 2)) 1)))
       (s (/ a (- b a)))
       (p (/ (+ s 1d0) 2d0))
       (q (log s)))
  (declare (type double-float a b d s p q w)
	   (type fixnum k))
  (defun gauss-monty-python-bit ()
    (declare (optimize (speed 3) (safety 0) (debug 0)))
    (let ((u-mbit (random most-positive-fixnum))
	  (sign -1d0)
	  (ux 0d0))
      (declare (type fixnum u-mbit)
	       (type double-float ux sign))
      (when (zerop (logand u-mbit 1))
	(setf sign 1d0))
      (setf u-mbit (ash u-mbit -1))
      (let ((u-m/2 (logand u-mbit #.(- (expt 2 (floor +bit-operation-m+ 2)) 1))))
	(declare (type fixnum u-m/2))
	(setf ux (* (dfloat u-m/2) w))
	(when (< u-m/2 k)
	  (return-from gauss-monty-python-bit (* sign ux))))
      (setf u-mbit (ash u-mbit #.(- (floor +bit-operation-m+ 2))))
      (let ((u-dash (/ (the double-float (+ u-mbit -0.5d0)) #.(dfloat (- (expt 2 (floor +bit-operation-m+ 2)) 2)))))
	(declare (type double-float u-dash))
	(when (< (the double-float (log u-dash)) (the double-float (- (/ (* ux ux) 2d0))))
	  (return-from gauss-monty-python-bit (* sign ux)))
	(let ((y (* sign s (- b ux))))
	  (declare (type double-float y))
	  (if (< (the double-float (log (- p u-dash))) (the double-float (- q (/ (* y y) 2))))
	      (return-from gauss-monty-python-bit y)
	    (loop
	      (let* ((v1 (unit-random))
		     (v2 (unit-random :[))
		     (x (sqrt (- d (* 2d0 (log v1))))))
		(declare (type double-float v1 v2 x))
		(when (<= (the double-float (* x v2)) b)
		  (return-from gauss-monty-python-bit (* sign x)))))))))))

(let* ((k +zuggurat-k+)
       (n (expt 2 k))
       (n-minus-1 (- n 1))
       (r (ecase k
	    (7 3.442619855899d0)
	    (8 3.6541528853610088d0)))
       (v (ecase k
	    (7 9.91256303526217d-3)
	    (8 4.92867323399d-3)))
       (d (* r r))
       (xn (make-array (1+ n) :element-type 'double-float)))
  (declare (type double-float r v d)
	   (type fixnum k n n-minus-1)
	   (type (vector double-float *) xn))
  ;; build xn
  (setf (aref xn n) (* v (exp (/ (* r r) 2))))
  (setf (aref xn (1- n)) r)
  (loop
      for i from (- n 2) downto 1
      for back = (aref xn (+ i 1)) do
	(setf (aref xn i) (sqrt (* -2d0
				   (log (+ (exp (/ (* back back) -2d0))
					   (/ v back)))))))
  (setf (aref xn 0) 0d0)
  (defun gauss-ziggurat ()
    (declare (optimize (speed 3) (safety 0) (debug 0)))
    (loop named gauss do
	  (let ((i 0)
		(j 0)
		(b 1)
		(u1 (unit-random :[)))
	    (declare (type fixnum i j b)
		     (type double-float u1))
	    (loop
	      (let ((u1-dash (* 2d0 u1)))
		(declare (type double-float u1-dash))
		(if (< u1-dash 1d0)
		    (setf i (+ i b)
			  u1 u1-dash)
		  (setf u1 (- u1-dash 1d0)))
		(incf j)
		(if (< j k)
		    (setf b (* b 2))
		  (return))))
	    (let* ((ux (* 2d0 u1))
		   (sign 1d0))		      
	      (declare (type double-float ux sign))
	      (when (>= ux 1d0)
		(setf sign -1d0)
		(setf ux (- ux 1d0)))
	      (setf ux (* ux (aref xn (+ i 1))))
	      (when (< ux (aref xn i))
		(return-from gauss (* sign ux)))
	      ;; step 6
	      (if (= i n-minus-1)
		  ;; step a
		  (loop
		    (let* ((v1 (unit-random :[))
			   (v2 (unit-random :[))
			   (x (sqrt (- d (* 2d0 (the double-float (log (- 1d0 v1))))))))
		      (declare (type double-float v1 v2 x))
		      (when (<= (the double-float (* x v2)) r)
			(return-from gauss (* sign x)))))
		;; step b
		(let ((gu (exp (/ (- (the double-float (* (aref xn i) (aref xn i)))
				     (the double-float (* ux ux)))
				  -2d0)))
		      (gl (exp (/ (- (the double-float (* (aref xn (1+ i)) (aref xn (1+ i))))
				     (the double-float (* ux ux)))
				  -2d0)))
		      (u2 (unit-random :[)))
		  (declare (type double-float gu gl u2))
		  (when (<= (the double-float (* u2 (- gu gl))) (the double-float (- 1d0 gl)))
		    (return-from gauss (* sign ux))))))))))

(let* ((k +zuggurat-k+)
       (n (expt 2 k))
       (n-minus-1 (- n 1))
       (r (ecase k
	    (7 3.442619855899d0)
	    (8 3.6541528853610088d0)))
       (v (ecase k
	    (7 9.91256303526217d-3)
	    (8 4.92867323399d-3)))
       (d (* r r))
       (kn (make-array n :element-type 'fixnum))
       (wn (make-array n :element-type 'double-float))
       (fn (make-array n :element-type 'double-float))
       (base (expt 2 (- +bit-operation-m+ k 1))))
  (declare (type double-float r v d)
	   (type fixnum k n n-minus-1 base)
	   (type (vector double-float *) wn fn)
	   (type (vector fixnum *) kn))
  ;; build arrays
  (setf (aref wn (- n 1)) (/ (* v (exp (/ (* r r) 2))) base))
  (setf (aref wn (- n 2)) (/ r base))
  (setf (aref kn (- n 1)) (floor (/ r (aref wn (- n 1)))))
  (setf (aref fn (- n 1)) (exp (/ (* r r) -2d0)))
  (loop
      with xn = (sqrt (* -2d0
			 (log (+ (exp (/ (* r r) -2d0))
				 (/ v r)))))
      for i from (- n 2) downto 1 do
	(setf (aref wn (- i 1)) (/ xn base))
	(setf (aref kn i) (floor (/ xn (aref wn i))))
	(setf (aref fn i) (exp (/ (* xn xn) -2d0)))
	(setf xn (sqrt (* -2d0
			  (log (+ (exp (/ (* xn xn) -2d0))
				  (/ v xn)))))))
  (setf (aref kn 0) 0)
  (setf (aref fn 0) 1d0)
  (defun gauss-ziggurat-bit ()
    (declare (optimize (speed 3) (safety 0) (debug 0)))
    (loop named gauss do
		 (let* ((u-mbit (random most-positive-fixnum))
			(i (logand u-mbit #.(- (expt 2 +zuggurat-k+) 1)))) 
		   (declare (type fixnum u-mbit i))
		   (setf u-mbit (ash u-mbit #.(- +zuggurat-k+)))
		   (let ((sign (if (zerop (logand u-mbit 1))
				   1d0
				 -1d0)))
		     (declare (type double-float sign))
		     (setf u-mbit (ash u-mbit -1))
		     (when (< u-mbit (aref kn i))
		       (return-from gauss (* u-mbit (aref wn i) sign)))
		     ;; step 5
		     (if (= i n-minus-1)
			 ;; step a
			 (loop
			   (let* ((v1 (unit-random :[))
				  (v2 (unit-random :[))
				  (x (sqrt (- d (* 2 (log (- 1d0 v1)))))))
			     (declare (type double-float v1 v2 x))
			     (when (<= (* x v2) r)
			       (return-from gauss (* sign x)))))
		       ;; step b
		       (let* ((ux (* u-mbit (aref wn i)))
			      (f (exp (/ (* ux ux) -2d0)))
			      (u (unit-random :[))
			      (fi1 (aref fn (+ i 1))))
			 (declare (type double-float f ux u))
			 (when (<= (* u (- (aref fn i) fi1))
				   (- f fi1))
			   (return-from gauss (* sign ux))))))))))

;;; gauss-ziggurat-bit is fastest of all these, so use this wrapper
;;; if you want, remove other algorithms and wrapper, just rename gauss-ziggurat-bit to normal-random
(defun standard-normal-random ()
  (gauss-ziggurat-bit))

(define-compiler-macro standard-normal-random (&whole form)
  (declare (ignore form))
  `(gauss-ziggurat-bit))

(defun normal-random (average std)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float average std))
  (let ((gauss (standard-normal-random)))
    (declare (type double-float gauss))
    (+ average (* std gauss))))

(define-compiler-macro normal-random (&whole form average std)
  (declare (ignore form))
  (cond ((and (numberp average) (= average 0d0))
	 (assert (typep average 'double-float))
	 (cond ((and (numberp std) (= std 1d0))
		(assert (typep std 'double-float))
		`(standard-normal-random))
	       (t
		`(* ,std (the double-float (standard-normal-random))))))
	((and (numberp std) (= std 1d0))
	 (assert (typep std 'double-float))
	 `(+ ,average (the double-float (standard-normal-random))))
	(t
	 `(+ ,average (* ,std (the double-float (standard-normal-random)))))))
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          ;;
;; Half Normal distribution ;;
;;                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let* ((k +zuggurat-k+)
       (n (expt 2 k))
       (n-minus-1 (- n 1))
       (r (ecase k
	    (7 3.442619855899d0)
	    (8 3.6541528853610088d0)))
       (v (ecase k
	    (7 9.91256303526217d-3)
	    (8 4.92867323399d-3)))
       (d (* r r))
       (kn (make-array n :element-type 'fixnum))
       (wn (make-array n :element-type 'double-float))
       (fn (make-array n :element-type 'double-float))
       (base (expt 2 (- +bit-operation-m+ k))))
  (declare (type double-float r v d)
	   (type fixnum k n n-minus-1 base)
	   (type (vector double-float *) wn fn)
	   (type (vector fixnum *) kn))
  ;; build arrays
  (setf (aref wn (- n 1)) (/ (* v (exp (/ (* r r) 2))) base))
  (setf (aref wn (- n 2)) (/ r base))
  (setf (aref kn (- n 1)) (floor (/ r (aref wn (- n 1)))))
  (setf (aref fn (- n 1)) (exp (/ (* r r) -2d0)))
  (loop
      with xn = (sqrt (* -2d0
			 (log (+ (exp (/ (* r r) -2d0))
				 (/ v r)))))
      for i from (- n 2) downto 1 do
	(setf (aref wn (- i 1)) (/ xn base))
	(setf (aref kn i) (floor (/ xn (aref wn i))))
	(setf (aref fn i) (exp (/ (* xn xn) -2d0)))
	(setf xn (sqrt (* -2d0
			  (log (+ (exp (/ (* xn xn) -2d0))
				  (/ v xn)))))))
  (setf (aref kn 0) 0)
  (setf (aref fn 0) 1d0)
  (defun gauss-half-ziggurat-bit ()
    (declare (optimize (speed 3) (safety 0) (debug 0)))
    (loop named gauss do
	  (let* ((u-mbit (random most-positive-fixnum))
		 (i (logand u-mbit #.(- (expt 2 +zuggurat-k+) 1)))) 
	    (declare (type fixnum u-mbit i))
	    (setf u-mbit (ash u-mbit #.(- +zuggurat-k+)))
	    (when (< u-mbit (aref kn i))
	      (return-from gauss (* u-mbit (aref wn i))))
	    ;; step 5
	    (if (= i n-minus-1)
		;; step a
		(loop
		  (let* ((v1 (unit-random :[))
			 (v2 (unit-random :[))
			 (x (sqrt (- d (* 2 (log (- 1d0 v1)))))))
		    (declare (type double-float v1 v2 x))
		    (when (<= (* x v2) r)
		      (return-from gauss x))))
	      ;; step b
	      (let* ((ux (* u-mbit (aref wn i)))
		     (f (exp (/ (* ux ux) -2d0)))
		     (u (unit-random :[))
		     (fi1 (aref fn (+ i 1))))
		(declare (type double-float f ux u))
		(when (<= (* u (- (aref fn i) fi1))
			  (- f fi1))
		  (return-from gauss ux))))))))

(defun standard-half-normal-random ()
  (gauss-half-ziggurat-bit))

(define-compiler-macro standard-half-normal-random (&whole form)
 (declare (ignore form))
  `(gauss-half-ziggurat-bit))

(defun half-normal-random (std)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float std))
  (let ((gauss (standard-half-normal-random)))
    (declare (type double-float gauss))
    (* std gauss)))

(define-compiler-macro half-normal-random (&whole form std)
  (declare (ignore form))
  (cond ((and (numberp std) (= std 1d0))
	 (assert (typep std 'double-float))
	 `(standard-half-normal-random))
	(t
	 `(* ,std (the double-float (standard-half-normal-random))))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     ;;
;; Cauchy distribution ;;
;;                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cauchy-inverse ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (tan (* pi (- (unit-random) 0.5d0))))

(defun cauchy-polar ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (loop
    (let* ((u1 (unit-random))
	   (u2 (unit-random))
	   (v1 (- (* 2d0 u1) 1d0))
	   (v2 (- (* 2d0 u2) 1d0))
	   (w  (+ (* v1 v1) (* v2 v2))))
      (declare (type double-float u1 u2 v1 v2 w))
      (when (< w 1d0)
	(return (/ v1 v2))))))

(defun cauchy-polar-gauss ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((y1 (normal-random 0d0 1d0))
	(y2 (normal-random 0d0 1d0)))
    (declare (type double-float y1 y2))
    (loop while (zerop y2) do
	  (setf y2 (normal-random 0d0 1d0)))
    (/ y1 y2)))

(let* ((b 4.766d0)
       (d (/ 1d0 (* 2d0 b)))
       (a (sqrt (- (/ (* 2d0 b) pi) 1d0)))
       (s (/ a (- b a)))
       (p (/ s))
       (q (* d (+ p 1d0)))
       (myt (atan b))
       (v (- (/ pi 2) myt)))
  (declare (type double-float a b d s p q myt v))
  (defun cauchy-monty-python ()
    (declare (optimize (speed 3) (safety 0) (debug 0)))
    (let ((u1 (* 2d0 (unit-random :[])))
	  (sign 1d0)
	  (ux 0d0))
      (declare (type double-float u1 ux sign))
      (if (> u1 1d0)
	  (setf sign -1d0
		ux (* b (the double-float (- u1 1d0))))
	(setf ux (* b u1)))
      (when (< ux a)
	(return-from cauchy-monty-python (* sign ux)))
      (let ((uy (* (unit-random :[]) d)))
	(declare (type double-float uy))
	(when (< (* (+ (* ux ux) 1d0) uy) #.(/ pi))
	  (return-from cauchy-monty-python (* sign ux)))
	(let ((y (* sign s (the double-float (- b ux)))))
	  (declare (type double-float y))
	  (if (< (* (+ 1d0 (* y y)) (- q (* p uy))) #.(/ pi))
	      (return-from cauchy-monty-python y)
	    (let ((u3 (unit-random)))
	      (return-from cauchy-monty-python (* sign (tan (+ myt (* v u3))))))))))))

(let* ((b 4.766d0)
       (d (/ (* b (- (expt 2 (floor +bit-operation-m+ 2)) 2))))
       (a (sqrt (- (/ (* 2d0 b) pi) 1d0)))
       (s (/ a (- b a)))
       (p (/ s))
       (q (/ (+ 1d0 p) (* 2d0 b)))
       (myt (atan b))
       (v (- (/ pi 2) myt))
       (k (floor (* (- (expt 2 (floor +bit-operation-m+ 2)) 1)
		    (/ a b))))
       (w (/ b (- (expt 2 (floor +bit-operation-m+ 2)) 1))))
  (declare (type double-float a b d s p q w myt v)
	   (type fixnum k))
  (defun cauchy-monty-python-bit ()
    (declare (optimize (speed 3) (safety 0) (debug 0)))
    (let ((u-mbit (random most-positive-fixnum))
	  (sign -1d0)
	  (ux 0d0))
      (declare (type fixnum u-mbit)
	       (type double-float ux sign))
      (when (zerop (logand u-mbit 1))
	(setf sign 1d0))
      (setf u-mbit (ash u-mbit -1))
      (let ((u-m/2 (logand u-mbit #.(- (expt 2 (floor +bit-operation-m+ 2)) 1))))
	(declare (type fixnum u-m/2))
	(setf ux (* (dfloat u-m/2) w))
	(when (< u-m/2 k)
	  (return-from cauchy-monty-python-bit (* sign ux))))
      (setf u-mbit (ash u-mbit #.(- (floor +bit-operation-m+ 2))))
      (let ((uy (* d u-mbit)))
	(declare (type double-float uy))
	(when (< (* (+ (* ux ux) 1d0) uy) #.(/ pi))
	  (return-from cauchy-monty-python-bit (* sign ux)))
	(let ((y (* sign s (the double-float (- b ux)))))
	  (declare (type double-float y))
	  (if (< (* (+ 1d0 (* y y)) (- q (* p uy))) #.(/ pi))
	      (return-from cauchy-monty-python-bit y)
	    (let ((u3 (unit-random)))
	      (return-from cauchy-monty-python-bit (* sign (tan (+ myt (* v u3))))))))))))

(let* ((k +zuggurat-k+)
       (n (expt 2 k))
       (n-minus-1 (- n 1))
       (r (ecase k
	    (7 158.3765842122476d0)
	    (8 320.7855843133476d0)))
       (v (ecase k
	    (7 1.26277937821681d-2)
	    (8 6.2346537399969d-3)))
       (tr1 (atan r))
       (tr2 (- (/ pi 2) tr1))
       (kn (make-array n :element-type 'fixnum))
       (wn (make-array n :element-type 'double-float))
       (fn (make-array n :element-type 'double-float))
       (base (expt 2 (- +bit-operation-m+ k 1))))
  (declare (type double-float r v tr1 tr2)
	   (type fixnum k n n-minus-1 base)
	   (type (vector double-float *) wn fn)
	   (type (vector fixnum *) kn))
  ;; build arrays
  (setf (aref wn (- n 1)) (/ (* v (+ 1 (* r r))) base))
  (setf (aref wn (- n 2)) (/ r base))
  (setf (aref kn (- n 1)) (floor (/ r (aref wn (- n 1)))))
  (setf (aref fn (- n 1)) (/ (+ (* r r) 1d0)))
  (flet ((update-x (x)
	   (sqrt (/ (- (* x x x (/ (+ (* x x) 1d0))) v)
		    (+ (* x (/ (+ (* x x) 1d0))) v)))))
    (loop
	with xn = (update-x r)
	for i from (- n 2) downto 1 do
	  (setf (aref wn (- i 1)) (/ xn base))
	  (setf (aref kn i) (floor (/ xn (aref wn i))))
	  (setf (aref fn i) (/ (+ (* xn xn) 1d0)))
	  (setf xn (update-x xn))))
  (setf (aref kn 0) 0)
  (setf (aref fn 0) 1d0)
  (defun cauchy-ziggurat-bit ()
    (declare (optimize (speed 3) (safety 0) (debug 0)))
    (loop named cauchy do
	  (let* ((u-mbit (random most-positive-fixnum))
		 (i (logand u-mbit #.(- (expt 2 +zuggurat-k+) 1)))) 
	    (declare (type fixnum u-mbit i))
	    (setf u-mbit (ash u-mbit #.(- +zuggurat-k+)))
	    (let ((sign (if (zerop (logand u-mbit 1))
			    1d0
			  -1d0)))
	      (declare (type double-float sign))
	      (setf u-mbit (ash u-mbit -1))
	      (when (< u-mbit (aref kn i))
		(return-from cauchy (* u-mbit (aref wn i) sign)))
	      ;; step 5
	      (if (= i n-minus-1)
		  ;; step a
		  (let* ((u (unit-random :[)))
		    (declare (type double-float u))
		    (return-from cauchy (* sign (tan (+ tr1 (* tr2 u))))))
		;; step b
		(let* ((ux (* u-mbit (aref wn i)))
		       (f (/ (+ (* ux ux) 1d0)))
		       (u (unit-random :[))
		       (fi1 (aref fn (+ i 1))))
		  (declare (type double-float f ux u))
		  (when (<= (* u (- (aref fn i) fi1))
			    (- f fi1))
		    (return-from cauchy (* sign ux))))))))))

;; cauchy wrapper
(defun standard-cauchy-random ()
  (cauchy-ziggurat-bit))

(define-compiler-macro standard-cauchy-random (&whole form)
 (declare (ignore form))
  `(cauchy-ziggurat-bit))

(defun cauchy-random (location scale)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float location scale))
  (let ((cauchy (standard-cauchy-random)))
    (declare (type double-float cauchy))
    (+ location (* scale cauchy))))

(define-compiler-macro cauchy-random (&whole form location scale)
  (declare (ignore form))
  (cond ((and (numberp location) (= location 0d0))
	 (assert (typep location 'double-float))
	 (cond ((and (numberp scale) (= scale 1d0))
		(assert (typep scale 'double-float))
		`(standard-cauchy-random))
	       (t
		`(* ,scale (the double-float (standard-cauchy-random))))))
	((and (numberp scale) (= scale 1d0))
	 (assert (typep scale 'double-float))
	 `(+ ,location (the double-float (standard-cauchy-random))))
	(t
	 `(+ ,location (* ,scale (the double-float (standard-cauchy-random)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          ;;
;; Exponential distribution ;;
;;                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun exp-inverse ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((u (unit-random)))
    (declare (type double-float u))
    (* -1.0d0 (the double-float (log u)))))

(defun exp-inverse-include-zero ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((u (- 1d0 (unit-random :[))))
    (declare (type double-float u))
    (* -1.0d0 (the double-float (log u)))))

(let* ((k +zuggurat-k+)
       (n (expt 2 k))
       (n-minus-1 (- n 1))
       (r (ecase k
	    (7 6.898315116616d0)
	    (8 7.697117470131d0)))
       (v (ecase k
	    (7 7.97322953955d-3)
	    (8 3.94965982258d-3)))
       (kn (make-array n :element-type 'fixnum))
       (wn (make-array n :element-type 'double-float))
       (fn (make-array n :element-type 'double-float))
       (base (expt 2 (- +bit-operation-m+ k))))
  (declare (type double-float r v)
	   (type fixnum k n n-minus-1 base)
	   (type (vector double-float *) wn fn)
	   (type (vector fixnum *) kn))
  ;; build arrays
  (setf (aref wn (- n 1)) (/ (* v (exp r)) base))
  (setf (aref wn (- n 2)) (/ r base))
  (setf (aref kn (- n 1)) (floor (/ r (aref wn (- n 1)))))
  (setf (aref fn (- n 1)) (exp (- r)))
  (loop
      with xn = (- (log (+ (exp (- r)) (/ v r))))
      for i from (- n 2) downto 1 do
	(setf (aref wn (- i 1)) (/ xn base))
	(setf (aref kn i) (floor (/ xn (aref wn i))))
	(setf (aref fn i) (exp (- xn)))
	(setf xn (- (log (+ (exp (- xn)) (/ v xn))))))
  (setf (aref kn 0) 0)
  (setf (aref fn 0) 1d0)
  (defun exp-ziggurat-bit ()
    (declare (optimize (speed 3) (safety 0) (debug 0)))
    (loop named exp do
	  (let* ((u-mbit (random most-positive-fixnum))
		 (i (logand u-mbit #.(- (expt 2 +zuggurat-k+) 1)))) 
	    (declare (type fixnum u-mbit i))
	    (setf u-mbit (ash u-mbit #.(- +zuggurat-k+)))
	    (when (< u-mbit (aref kn i))
	      (return-from exp (* (the double-float (+ u-mbit 0.5d0)) (aref wn i))))
	    ;; step 4
	    (if (= i n-minus-1)
		;; step a
		(let* ((u (unit-random :[)))
		  (declare (type double-float u))
		  (return-from exp (- r (log (- 1d0 u)))))
	      ;; step b
	      (let* ((ux (* (the double-float (+ u-mbit 0.5d0)) (aref wn i)))
		     (f (exp (- ux)))
		     (u (unit-random :[))
		     (fi1 (aref fn (+ i 1))))
		(declare (type double-float f ux u))
		(when (<= (* u (- (aref fn i) fi1))
			  (- f fi1))
		  (return-from exp ux)))))))
  (defun exp-ziggurat-bit-include-zero ()
    (declare (optimize (speed 3) (safety 0) (debug 0)))
    (loop named exp do
	  (let* ((u-mbit (random most-positive-fixnum))
		 (i (logand u-mbit #.(- (expt 2 +zuggurat-k+) 1)))) 
	    (declare (type fixnum u-mbit i))
	    (setf u-mbit (ash u-mbit #.(- +zuggurat-k+)))
	    (when (< u-mbit (aref kn i))
	      (return-from exp (* u-mbit (aref wn i))))
	    ;; step 4
	    (if (= i n-minus-1)
		;; step a
		(let* ((u (unit-random :[)))
		  (declare (type double-float u))
		  (return-from exp (- r (log (- 1d0 u)))))
	      ;; step b
	      (let* ((ux (* u-mbit (aref wn i)))
		     (f (exp (- ux)))
		     (u (unit-random :[))
		     (fi1 (aref fn (+ i 1))))
		(declare (type double-float f ux u))
		(when (<= (* u (- (aref fn i) fi1))
			  (- f fi1))
		  (return-from exp ux))))))))

;; exponential wrapper
(defun standard-exp-random (&optional include-zero)
  (if include-zero
      (exp-ziggurat-bit-include-zero)
     (exp-ziggurat-bit)))

(define-compiler-macro standard-exp-random (&whole form &optional include-zero)
  (declare (ignore form))
  (typecase include-zero
    (list
     `(if ,include-zero
	  (the double-float (exp-ziggurat-bit-include-zero))
	(the double-float (exp-ziggurat-bit))))
    (symbol
     (cond ((eq include-zero t)
	    `(exp-ziggurat-bit-include-zero))
	   ((eq include-zero nil)
	    `(exp-ziggurat-bit))
	   (t
	    `(if ,include-zero
		 (the double-float (exp-ziggurat-bit-include-zero))
	       (the double-float (exp-ziggurat-bit))))))
    (t
     `(exp-ziggurat-bit-include-zero))))

(defun exp-random (scale &optional include-zero)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float scale))
  (let ((exp (standard-exp-random include-zero)))
    (declare (type double-float exp))
    (* scale exp)))

(define-compiler-macro exp-random (&whole form scale &optional include-zero)
  (declare (ignore form))
  (cond ((and (numberp scale) (= scale 1d0))
	 (assert (typep scale 'double-float))
	 `(standard-exp-random ,include-zero))
	(t
	 `(* ,scale (the double-float (standard-exp-random ,include-zero))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      ;;
;; Laplace distribution ;;
;;                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun laplace-inverse ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((u (unit-random)))
    (declare (type double-float u))
    (if (< u 0.5d0)
	(log (* 2d0 u))
      (- (log (* 2d0 (- 1 u)))))))

(let* ((k +zuggurat-k+)
       (n (expt 2 k))
       (n-minus-1 (- n 1))
       (r (ecase k
	    (7 6.898315116616d0)
	    (8 7.697117470131d0)))
       (v (ecase k
	    (7 7.97322953955d-3)
	    (8 3.94965982258d-3)))
       (kn (make-array n :element-type 'fixnum))
       (wn (make-array n :element-type 'double-float))
       (fn (make-array n :element-type 'double-float))
       (base (expt 2 (- +bit-operation-m+ k 1))))
  (declare (type double-float r v)
	   (type fixnum k n n-minus-1 base)
	   (type (vector double-float *) wn fn)
	   (type (vector fixnum *) kn))
  ;; build arrays
  (setf (aref wn (- n 1)) (/ (* v (exp r)) base))
  (setf (aref wn (- n 2)) (/ r base))
  (setf (aref kn (- n 1)) (floor (/ r (aref wn (- n 1)))))
  (setf (aref fn (- n 1)) (exp (- r)))
  (loop
      with xn = (- (log (+ (exp (- r)) (/ v r))))
      for i from (- n 2) downto 1 do
	(setf (aref wn (- i 1)) (/ xn base))
	(setf (aref kn i) (floor (/ xn (aref wn i))))
	(setf (aref fn i) (exp (- xn)))
	(setf xn (- (log (+ (exp (- xn)) (/ v xn))))))
  (setf (aref kn 0) 0)
  (setf (aref fn 0) 1d0)
  (defun laplace-ziggurat-bit ()
    (declare (optimize (speed 3) (safety 0) (debug 0)))
    (loop named laplace do
	  (let* ((u-mbit (random most-positive-fixnum))
		 (i (logand u-mbit #.(- (expt 2 +zuggurat-k+) 1)))) 
	    (declare (type fixnum u-mbit i))		  
	    (setf u-mbit (ash u-mbit #.(- +zuggurat-k+)))
	    (let ((sign (if (zerop (logand u-mbit 1))
			    1d0
			  -1d0)))
	      (declare (type double-float sign))
	      (setf u-mbit (ash u-mbit -1))
	      (when (< u-mbit (aref kn i))
		(return-from laplace (* sign u-mbit (aref wn i))))
	      ;; step 5
	      (if (= i n-minus-1)
		  ;; step a
		  (let* ((u (unit-random :[)))
		    (declare (type double-float u))
		    (return-from laplace (* sign (- r (log (- 1d0 u))))))
		;; step b
		(let* ((ux (* u-mbit (aref wn i)))
		       (f (exp (- ux)))
		       (u (unit-random :[))
		       (fi1 (aref fn (+ i 1))))
		  (declare (type double-float f ux u))
		  (when (<= (* u (- (aref fn i) fi1))
			    (- f fi1))
		    (return-from laplace (* sign ux))))))))))

;; laplace wrapper
(defun standard-laplace-random ()
  (laplace-ziggurat-bit))

(define-compiler-macro standard-laplace-random (&whole form)
 (declare (ignore form))
  `(laplace-ziggurat-bit))

(define-compiler-macro laplace-random (&whole form location scale)
  (declare (ignore form))
  (cond ((and (numberp location) (= location 0d0))
	 (assert (typep location 'double-float))
	 (cond ((and (numberp scale) (= scale 1d0))
		(assert (typep scale 'double-float))
		`(standard-laplace-random))
	       (t
		`(* ,scale (the double-float (standard-laplace-random))))))
	((and (numberp scale) (= scale 1d0))
	 (assert (typep scale 'double-float))
	 `(+ ,location (the double-float (standard-laplace-random))))
	(t
	 `(+ ,location (* ,scale (the double-float (standard-laplace-random)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      ;;
;; weibull distribution ;;
;;                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun-with-cached-values weibull-inverse (shape &optional include-zero)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float shape))
  ((rinv (/ shape)))
  (declare (type double-float shape)
	   (ignorable shape))
  (the double-float (expt (the double-float (standard-exp-random include-zero)) rinv)))

(defun standard-weibull-random (shape &optional include-zero)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float shape))
  (case shape
    (1d0 (standard-exp-random include-zero))
    (2d0 (the double-float (sqrt (the double-float (standard-exp-random include-zero)))))
    (0.5d0 (let ((ex (standard-exp-random include-zero)))
	     (declare (type double-float ex))
	     (* ex ex)))
    (t (weibull-inverse shape include-zero))))

(define-compiler-macro standard-weibull-random (&whole form shape &optional include-zero)
  (declare (ignore form))
  (cond ((numberp shape)
	 (assert (typep shape 'double-float))
	 (case shape
	   (1d0 `(standard-exp-random ,include-zero))
	   (2d0 `(the double-float (sqrt (the double-float (standard-exp-random ,include-zero)))))
	   (0.5d0 (let ((sym (gensym "exp")))
		    `(let ((,sym (standard-exp-random ,include-zero)))
		       (declare (type double-float ,sym))
		       (* ,sym ,sym))))
	   (t `(the double-float (expt (the double-float (standard-exp-random ,include-zero)) ,(/ shape))))))
	(t
	 `(the double-float (expt (the double-float (standard-exp-random ,include-zero)) (/ ,shape))))))

(defun weibull-random (shape scale &optional include-zero)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float shape scale))
  (let ((weibull (standard-weibull-random shape include-zero)))
    (declare (type double-float weibull))
    (* scale weibull)))

(define-compiler-macro weibull-random (&whole form shape scale &optional include-zero)
  (declare (ignore form))
  (cond ((and (numberp scale) (= scale 1d0))
	 (assert (typep scale 'double-float))
	 `(standard-weibull-random ,shape ,include-zero))
	(t
	 `(* ,scale (the double-float (standard-weibull-random ,shape ,include-zero))))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;                    ;;
;; Gamma distribution ;;
;;                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun-with-cached-values gamma-inverse-shape-big (shape)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float shape))
  ((d (- shape #.(dfloat 1/3)))
   (c (/ (sqrt (* 9d0 d)))))
  (declare (ignorable shape)
	   (type double-float d c))
  (loop
    (let ((z 0d0)
	  (v 0d0))
      (loop named inner do
	    (setf z (normal-random 0d0 1d0))
	    (setf v (+ 1d0 (* c z)))
	    (when (> v 0d0)
	      (return-from inner)))
      (let* ((w (* v v v))
	     (y (* d w)))    
	(declare (type double-float z w y))
	(let ((e (standard-exp-random)))
	  (unless (< (+ e (/ (* z z) 2d0) (* d (the double-float (log w))) (- y) d) 0d0)
	    (return y)))))))

(defun-with-cached-values gamma-inverse-shape-small (shape)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float shape))
  ((shape-inv (/ shape))
   (d (- (+ shape 1d0) #.(dfloat 1/3)))
   (c (/ (sqrt (* 9d0 d)))))
  (declare (ignorable shape)
	   (type double-float shape-inv d c))
  (loop
    (let ((z 0d0)
	  (v 0d0))
      (loop named inner do
	    (setf z (normal-random 0d0 1d0))
	    (setf v (+ 1d0 (* c z)))
	    (when (> v 0d0)
	      (return-from inner)))
      (let* ((w (* v v v))
	     (y (* d w)))    
	(declare (type double-float z w y))
	(let ((e (exp-random 1d0)))
	  (unless (< (+ e (/ (* z z) 2d0) (* d (the double-float (log w))) (- y) d) 0d0)
	    (return (* y (expt (unit-random) shape-inv)))))))))

(defun gamma-inverse (shape)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float shape))
  (cond ((> shape 1d0) (gamma-inverse-shape-big shape))
	((= shape 1d0) (standard-exp-random))
	(t (gamma-inverse-shape-small shape))))

(defun-with-cached-values gamma-compression-shape-big (shape)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float shape))
  ((d (- shape #.(dfloat 1/3)))
   (c (/ (sqrt (* 9d0 d)))))
  (declare (ignorable shape)
	   (type double-float d c))
  (loop
    (let ((z 0d0)
	  (v 0d0))
      (loop named inner do
	    (setf z (normal-random 0d0 1d0))
	    (setf v (+ 1d0 (* c z)))
	    (when (> v 0d0)
	      (return-from inner)))
      (let* ((w (* v v v))
	     (y (* d w)))
	(declare (type double-float z w y))
	(let ((u (unit-random)))
	  (if (<= u (- 1d0
		       (* 0.0331 z z z z)))
	      (return y)
	    (unless (< (+ (/ (* z z) 2d0) (* d (the double-float (log w))) (- y) d) (the double-float (log u)))
	      (return y))))))))

(defun-with-cached-values gamma-compression-shape-small (shape)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float shape))
  ((shape-inv (/ shape))
   (d (- (+ shape 1d0) #.(dfloat 1/3)))
   (c (/ (sqrt (* 9d0 d)))))
  (declare (ignorable shape)
	   (type double-float shape-inv d c))
  (loop
    (let ((z 0d0)
	  (v 0d0))
      (loop named inner do
	    (setf z (normal-random 0d0 1d0))
	    (setf v (+ 1d0 (* c z)))
	    (when (> v 0d0)
	      (return-from inner)))
      (let* ((w (* v v v))
	     (y (* d w)))    
	(declare (type double-float z w y))
	(let ((u (unit-random)))
	  (if (<= u (- 1d0
		       (* 0.0331 z z z z)))
	      (return (* y (expt (unit-random) shape-inv)))
	    (unless (< (+ (/ (* z z) 2d0) (* d (the double-float (log w))) (- y) d) (the double-float (log u)))
	      (return (* y (expt (unit-random) shape-inv))))))))))

(defun gamma-compression (shape)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float shape))
  (cond ((> shape 1d0) (gamma-compression-shape-big shape))
	((= shape 1d0) (standard-exp-random))
	(t (gamma-compression-shape-small shape))))

;; gamma wrapper
(defun standard-gamma-random (shape)
  (gamma-compression shape))

;; this compiler macro is useful even rename the main function
(define-compiler-macro standard-gamma-random (&whole form shape)
  (declare (ignore form))
  (typecase shape
    (number
     (assert (typep shape 'double-float))
     (cond ((> shape 1d0)
	    (let* ((d (- shape #.(dfloat 1/3)))
		   (c (/ (sqrt (* 9d0 d)))))
	      `(gamma-compression-shape-big-cached ,shape ,d ,c)))
	   ((= shape 1d0) `(standard-exp-random))
	   (t
	    (let* ((shape-inv (/ shape))
		   (d (- (+ shape 1d0) #.(dfloat 1/3)))
		   (c (/ (sqrt (* 9d0 d)))))
	      `(gamma-compression-shape-small-cached ,shape ,shape-inv ,d ,c)))))
    (t
     `(gamma-compression ,shape))))

(defun gamma-random (shape scale)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float shape scale))
  (let ((gamma (standard-gamma-random shape)))
    (declare (type double-float gamma))
    (* scale gamma)))

(define-compiler-macro gamma-random (&whole form shape scale)
  (declare (ignore form))
  (cond ((and (numberp scale) (= scale 1d0))
	 (assert (typep scale 'double-float))
	 `(standard-gamma-random ,shape))
	(t
	 `(* ,scale (the double-float (standard-gamma-random ,shape))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             ;;
;; L/R Triangular distribution ;;
;;                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun-with-cached-values right-triangular-inverse (a b)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float a b))
  ((s (- b a)))
  (declare (type double-float s)
	   (ignorable b))
  (+ a (* s (the double-float (sqrt (unit-random))))))

(defun-with-cached-values right-triangular-compare (a b)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float a b))
  ((s (- b a)))
  (declare (type double-float s)
	   (ignorable b))
  (+ a (* s (the double-float (max (unit-random) (unit-random))))))

(defun-with-cached-values left-triangular-inverse (a b)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float a b))
  ((s (- b a)))
  (declare (type double-float s)
	   (ignorable b))
  (+ a (* s (- 1d0 (the double-float (sqrt (unit-random)))))))

(defun-with-cached-values left-triangular-compare (a b)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float a b))
  ((s (- b a)))
  (declare (type double-float s)
	   (ignorable b))
  (+ a (* s (the double-float (min (unit-random) (unit-random))))))

(defun right-triangular-random (a b)
  (right-triangular-inverse a b))

(define-compiler-macro right-triangular-random (&whole form a b)
  (declare (ignore form))
  (cond ((and (numberp a) (numberp b))
	 (assert (and (typep a 'double-float) (typep b 'double-float)))
	 (let ((s (- b a)))
	   (declare (type double-float s))
	   `(right-triangular-inverse-cached ,a ,b ,s)))
	(t
	 `(right-triangular-inverse ,a ,b))))

(defun left-triangular-random (a b)
  (left-triangular-inverse a b))

(define-compiler-macro left-triangular-random (&whole form a b)
  (declare (ignore form))
  (cond ((and (numberp a) (numberp b))
	 (assert (and (typep a 'double-float) (typep b 'double-float)))
	 (let ((s (- b a)))
	   (declare (type double-float s))
	   `(left-triangular-inverse-cached ,a ,b ,s)))
	(t
	 `(right-triangular-inverse ,a ,b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             ;;
;; Power Function distribution ;;
;;                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun-with-cached-values power-function-inverse (shape lower-boundary upper-boundary)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float shape lower-boundary upper-boundary))
  ((shape-inv (/ shape))
   (s (- upper-boundary lower-boundary)))
  (declare (type double-float shape-inv s)
	   (ignorable upper-boundary shape))
  (+ lower-boundary (* s (expt (unit-random) shape-inv))))

(defun-with-cached-values power-function-with-gamma (shape lower-boundary upper-boundary)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float shape lower-boundary upper-boundary))
  ((s (- upper-boundary lower-boundary)))
  (declare (type double-float s)
	   (ignorable upper-boundary))
  (let ((y1 (gamma-random shape 1d0))
	(y2 (exp-random 1d0)))
    (declare (type double-float y1 y2))
    (+ lower-boundary (* s (/ y1 (+ y1 y2))))))

;;; power function wrapper
(defun power-function-random (shape lower-boundary upper-boundary)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float shape lower-boundary upper-boundary))
  (cond ((= shape 1d0)
	 ;; uniform distribution
	 (+ lower-boundary (* (- lower-boundary upper-boundary) (unit-random))))
	((= shape 2d0) (right-triangular-random lower-boundary upper-boundary))
	((> shape 1d0)
	 (power-function-with-gamma shape lower-boundary upper-boundary))
	(t
	 (power-function-inverse shape lower-boundary upper-boundary))))

;;; compiler macro for effectiveness
(define-compiler-macro power-function-random (&whole form shape a b)
  (cond ((numberp shape)
	 (assert (typep shape 'double-float))
	 (cond ((= shape 1d0)
		;; uniform distribution
		(cond ((and (numberp a) (numberp b))
		       (assert (and (typep a 'double-float) (typep b 'double-float)))
		       (let ((s (- b a)))
			 `(+ ,a (* ,s (unit-random)))))
		      (t `(+ ,a (* (- ,b ,a) (unit-random))))))
	       ((= shape 2d0)
		`(right-triangular-random ,a ,b))
	       ((> shape 1d0)
		(cond ((and (numberp a) (numberp b))
		       (assert (and (typep a 'double-float) (typep b 'double-float)))
		       (let ((s (- b a)))
			  `(power-function-with-gamma-cached ,shape ,a ,b ,s)))
		      (t `(power-function-with-gamma ,shape ,a ,b))))
	       (t
		(cond ((and (numberp a) (numberp b))
		       (assert (and (typep a 'double-float) (typep b 'double-float)))
		       (let ((s (- b a))
			     (r (/ shape)))
			 `(power-function-inverse-cached ,shape ,a ,b ,r ,s)))
		      (t
		       (let ((aa (gensym "a"))
			     (bb (gensym "b"))
			     (r (/ shape)))
			 `(let ((,aa ,a)
				(,bb ,b))
			    (power-function-inverse-cached ,shape ,aa ,bb ,r (- ,bb ,aa)))))))))
	(t
	 (cond ((and (numberp a) (numberp b))
		(assert (and (typep a 'double-float) (typep b 'double-float)))
		(let ((s (- b a))
		      (sh (gensym "shape")))
		  `(let ((,sh ,shape))
		     (cond ((= ,sh 1d0)
			    ;; uniform distribution
			    (+ ,a (* ,s (unit-random))))
			   ((= ,sh 2d0) (the double-float (right-triangular-inverse-cached ,a ,b ,s)))
			   ((> ,sh 1d0)
			    (the double-float (power-function-with-gamma-cached ,sh ,a ,b ,s)))
			   (t
			    (the double-float (power-function-inverse-cached ,sh ,a ,b (/ ,sh) ,s)))))))
	       (t form)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      ;;
;; Arcsine distribution ;;
;;                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun arcsine-inverse ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((y (sin (/ (* pi (unit-random)) 2d0))))
    (declare (type double-float y))
    (* y y)))

(defun arcsine-polar ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (loop
    (let* ((u1 (unit-random))
	   (u2 (unit-random))
	   (y (* u1 u1))
	   (v (+ y (* u2 u2))))
      (declare (type double-float u1 u2 y v))
      (when (< 0d0 v 1d0)
	(return (/ y v))))))

(defun arcsine-random ()
  (arcsine-polar))

(define-compiler-macro arcsine-random (&whole form)
  (declare (ignore form))
  `(arcsine-polar))

;;;;;;;;;;;;;;;;;;;;;;;
;;                   ;;
;; Beta distribution ;;
;;                   ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun beta-random (alpha beta)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float alpha beta))
  (cond ((= alpha beta 1d0)
	 (unit-random))
	((= alpha beta 0.5d0)
	 (arcsine-random))
	((= beta 1d0)
	 (if (= alpha 2d0)
	     (right-triangular-random 0d0 1d0)
	   (power-function-random alpha 0d0 1d0)))
	((= alpha 1d0)
	 (if (= beta 2d0)
	     (left-triangular-random 0d0 1d0)
	   (let ((y (power-function-random beta 0d0 1d0)))
	     (declare (type double-float y))
	     (- 1d0 y))))
	(t
	 (let ((y1 (gamma-random alpha 1d0))
	       (y2 (gamma-random beta  1d0)))
	   (declare (type double-float y1 y2))
	   (/ y1 (+ y1 y2))))))

(define-compiler-macro beta-random (&whole form alpha beta)
  (declare (ignore form))
  (cond ((numberp alpha)
	 (assert (typep alpha 'double-float))
	 (cond ((numberp beta)
		(assert (typep beta 'double-float))
		;; all argumaents are number -- start full dispatch
		(cond ((= alpha beta 1d0)
		       `(unit-random))
		      ((= alpha beta 0.5d0)
		       `(arcsine-random))
		      ((= beta 1d0)
		       (if (= alpha 2d0)
			   `(right-triangular-random 0d0 1d0)
			 `(power-function-random alpha 0d0 1d0)))
		      ((= alpha 1d0)
		       (if (= beta 2d0)
			   `(left-triangular-random 0d0 1d0)
			 `(let ((y (power-function-random beta 0d0 1d0)))
			    (declare (type double-float y))
			    (- 1d0 y))))
		      (t
		       `(let ((y1 (gamma-random alpha 1d0))
			      (y2 (gamma-random beta  1d0)))
			  (declare (type double-float y1 y2))
			  (/ y1 (+ y1 y2))))))
	       (t
		;; only alpha is number
		(let ((sym (gensym "beta")))
		  (cond ((= alpha 1d0)
			 `(let ((,sym ,beta))
			    (declare (type double-float ,sym))
			    (cond ((= ,sym 1d0)
				   (unit-random))
				  ((= ,sym 2d0)
				   (left-triangular-random 0d0 1d0))
				  (t
				   (let ((y (power-function-random ,sym 0d0 1d0)))
				     (declare (type double-float y))
				     (- 1d0 y))))))		      
			((= alpha 0.5d0)
			 `(let ((,sym ,beta))
			    (declare (type double-float ,sym))
			    (cond ((= ,sym 0.5d0)
				   (arcsine-random))
				  (t
				   (let ((y1 (gamma-random ,alpha 1d0))
					 (y2 (gamma-random ,sym  1d0)))
				     (declare (type double-float y1 y2))
				     (/ y1 (+ y1 y2)))))))
			(t
			 `(let ((y1 (gamma-random ,alpha 1d0))
				(y2 (gamma-random ,beta  1d0)))
			    (declare (type double-float y1 y2))
			    (/ y1 (+ y1 y2)))))))))
	((numberp beta)
	 (assert (typep beta 'double-float))
	 ;; only beta is number
	 (let ((sym (gensym "alpha")))
		  (cond ((= beta 1d0)
			 `(let ((,sym ,alpha))
			    (declare (type double-float ,sym))
			    (cond ((= ,sym 1d0)
				   (unit-random))
				  ((= ,sym 2d0)
				   (left-triangular-random 0d0 1d0))
				  (t
				   (power-function-random ,sym 0d0 1d0)))))
			((= beta 0.5d0)
			 `(let ((,sym ,alpha))
			    (declare (type double-float ,sym))
			    (cond ((= ,sym 0.5d0)
				   (arcsine-random))
				  (t
				   (let ((y1 (gamma-random ,sym  1d0))
					 (y2 (gamma-random ,beta 1d0)))
				     (declare (type double-float y1 y2))
				     (/ y1 (+ y1 y2)))))))
			(t
			 `(let ((y1 (gamma-random ,alpha 1d0))
				(y2 (gamma-random ,beta  1d0)))
			    (declare (type double-float y1 y2))
			    (/ y1 (+ y1 y2)))))))
	(t
	 `(let ((y1 (gamma-random ,alpha 1d0))
		(y2 (gamma-random ,beta  1d0)))
	    (declare (type double-float y1 y2))
	    (/ y1 (+ y1 y2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     ;;
;; Erlang distribution ;;
;;                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun erlang-convolution (shape scale)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float scale)
	   (type fixnum shape))
  (let ((ans 0d0))
    (declare (type double-float ans))
    (loop repeat shape do
      (incf ans (exp-random scale)))
    ans))

(defun erlang-convolution-include-zero (shape scale)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float scale)
	   (type fixnum shape))
  (let ((ans 0d0))
    (declare (type double-float ans))
    (loop repeat shape do
      (incf ans (exp-random scale t)))
    ans))

;;; method choice strategy:
;;; if the shape is known in compile time, we can use gamma-*-cached, this is faster than convolution
;;; else, gamma-random always compute middle values, so this is slower than convolution -- then choice convolution
;;; but, the shape is huge, convolution takes O(shape), so we have to switch the method in some threshold

;;; change this value for target implementation
(defvar erlang-switch-shape 10)

(defun erlang-random (shape scale &optional include-zero)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float scale)
	   (type fixnum shape))
  (cond ((= shape 1)
	 (exp-random scale include-zero))
	(include-zero
	 (erlang-convolution-include-zero shape scale))
	((> shape (the fixnum erlang-switch-shape))
	 (gamma-random (dfloat shape) scale))
	(t
	 (erlang-convolution shape scale))))

(define-compiler-macro erlang-random (&whole form shape scale &optional include-zero)
  (cond ((numberp shape)
	 ;; shape is known value => use gamma-random
	 (assert (typep shape 'fixnum))
	 (if (= shape 1)
	     `(exp-random ,scale ,include-zero)	 
	   (typecase include-zero
	     (list
	      `(if ,include-zero
		   (erlang-convolution-include-zero ,shape ,scale)
		 (gamma-random ,(dfloat shape) ,scale)))
	     (symbol
	      (cond ((eq include-zero t)
		     `(erlang-convolution-include-zero ,shape ,scale))
		    ((eq include-zero nil)
		     `(gamma-random ,(dfloat shape) ,scale))
		    (t
		     `(if ,include-zero
			  (erlang-convolution-include-zero ,shape ,scale)
			(gamma-random ,(dfloat shape) ,scale)))))
	     (t `(if ,include-zero
		     (erlang-convolution-include-zero ,shape ,scale)
		   (gamma-random ,(dfloat shape) ,scale))))))

	(t
	 (let ((sym (gensym "shape")))
	   (typecase include-zero
	     (list
	      `(if ,include-zero
		   (the double-float (erlang-convolution-include-zero ,shape ,scale))
		 (let ((,sym ,shape))
		   (if (> ,sym (the fixnum erlang-switch-shape))
		       (the double-float (gamma-random (dfloat ,sym) ,scale))
		     (the double-float (erlang-convolution ,shape ,scale))))))
	     (symbol
	      (cond ((eq include-zero t)
		     `(erlang-convolution-include-zero ,shape ,scale))
		    ((eq include-zero nil)
		     `(let ((,sym ,shape))
			(if (> ,sym (the fixnum erlang-switch-shape))
			     (the double-float (gamma-random (dfloat ,sym) ,scale))
			  (the double-float (erlang-convolution ,shape ,scale)))))
		    (t
		     `(if ,include-zero
			  (the double-float (erlang-convolution-include-zero ,shape ,scale))
			(let ((,sym ,shape))
			  (if (> ,sym (the fixnum erlang-switch-shape))
			      (the double-float (gamma-random (dfloat ,sym) ,scale))
			    (the double-float (erlang-convolution ,shape ,scale))))))))
	     (t form))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         ;;
;; Chi-Square distribution ;;
;;                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+ignore
(defun chi-square-with-gamma-one ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((u (unit-random)))
    (declare (type double-float u))
    (* 2d0 (gamma-random 1.5d0 2d0) u u)))

(defun chi-square-convolution (freedom)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type fixnum freedom))
  (let ((x 0d0)
	(n 0))
    (declare (type double-float x)
	     (type fixnum n))
    (multiple-value-bind (quotient remainder) (floor freedom 2)
      (declare (type fixnum quotient remainder))
      (setf n quotient)
      (unless (zerop remainder)
	(let ((z (half-normal-random 1d0)))
	  (setf x (* z z)))))
    (loop repeat n do
	  (incf x (exp-random 2d0 nil)))
    x))


(defvar chi-square-switch-freedom 30)

(defun chi-square-random (freedom)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type fixnum freedom))
  (case freedom
    (1 (let ((u (half-normal-random 1d0)))
	 (declare (type double-float u))
	 (* u u)))
    (2 (exp-random 2d0 nil))
    (t (if (< freedom (the fixnum chi-square-switch-freedom))
	   (chi-square-convolution freedom)
	 (* 2d0 (the double-float (gamma-compression-shape-big (/ (the double-float freedom) 2d0))))))))

(define-compiler-macro chi-square-random (&whole form freedom)
  (cond ((numberp freedom)
	 ;; shape is known value => use gamma-random
	 (assert (typep freedom 'fixnum))
	 (case freedom
	   (1 `(let ((u (half-normal-random 1d0)))
		 (declare (type double-float u))
		 (* u u)))
	   (2 `(exp-random 2d0 nil))
	   (t `(gamma-random ,(dfloat (/ freedom 2)) 2d0))))
	(t form)))

;;;;;;;;;;;;;;;;;;;;
;;                ;;
;; F-distribution ;;
;;                ;;
;;;;;;;;;;;;;;;;;;;;

;; variance is not stable -- low accuracy?

(defun-with-cached-values f-random (freedom1 freedom2)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type fixnum freedom1 freedom2))
  ((f (/ (dfloat freedom2) (dfloat freedom1))))
  (declare (type double-float f))
  (/ (* f (the double-float (chi-square-random freedom1)))
     (the double-float (chi-square-random freedom2))))

(define-compiler-macro f-random (&whole form freedom1 freedom2)
  (cond ((and (numberp freedom1) (numberp freedom2))
	 (assert (and (typep freedom1 'fixnum) (typep freedom2 'fixnum)))
	 (let ((f (/ (dfloat freedom2) (dfloat freedom1))))
	   `(/ (* ,f (the double-float (chi-square-random ,freedom1)))
	       (the double-float (chi-square-random ,freedom2)))))
	((numberp freedom1)
	 (let ((sym (gensym "freedom")))
	   `(let* ((,sym ,freedom2)
		   (f (/ (dfloat ,sym) ,(dfloat freedom1))))
	      (declare (type double-float f)
		       (type fixnum ,sym))
	      (/ (* f (the double-float (chi-square-random ,freedom1)))
		 (the double-float (chi-square-random ,sym))))))
	((numberp freedom2)
	 (let ((sym (gensym "freedom")))
	   `(let* ((,sym ,freedom1)
		   (f (/ ,(dfloat freedom2) (dfloat ,sym))))
	      (declare (type double-float f)
		       (type fixnum ,sym))
	      (/ (* f (the double-float (chi-square-random ,sym)))
		 (the double-float (chi-square-random ,freedom2))))))
	(t form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          ;;
;; student's t distribution ;;
;;                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun-with-cached-values t-with-gamma (freedom)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type fixnum freedom))
  ((rdiv2 (/ (dfloat freedom) 2d0))
   (d (sqrt rdiv2)))
  (declare (type double-float rdiv2 d)
	   (ignorable freedom))
  (/ (* d (the double-float (normal-random 0d0 1d0)))
     (the double-float (sqrt (the double-float (gamma-random rdiv2 1d0))))))

(defun-with-cached-values t-monty-python (freedom)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type fixnum freedom))
  ((r (dfloat freedom))
   (b (case freedom       
	(3 3.142d0)
	(4 2.968d0)
	(5 2.868d0)
	(6 2.783d0)
	(7 2.756d0)
	(8 2.724d0)
	(t
	 (assert (> freedom 8))
	 (+ 2.5074d0 (expt (* 1.876d0 r) -1.042d0)))))
   (c (multiple-value-bind (quotient remainder) (floor freedom 2)
	(declare (type fixnum quotient remainder))
	(if (= remainder 0)
	    ;;; even
	    (/ (loop with ans = 1d0
		   for i fixnum from 1 below quotient do
		     (setf ans (* ans (+ (/ (* 2d0 i)) 1d0)))
		   finally (return ans))
	       (* 2d0 (the double-float (sqrt r))))
	  ;;; odd	  
	  (/ (loop with ans = 1d0
		 for i fixnum from 1 upto quotient do
		   (setf ans (* ans (+ (/ (- (* 2d0 i) 1d0)) 1d0)))
		 finally (return ans))
	     (* pi (the double-float (sqrt r)))))))
   (a (sqrt (* r (- (expt (* 2 b c) (/ 2 (+ freedom 1))) 1d0))))
   (d (/ (* 2d0 b)))
   (s (/ a (- b a)))
   (p (/ r))
   (q (/ (+ freedom 1) 2))
   (t1 (/ s))
   (t2 (* d (+ t1 1d0)))
   (v1 (+ r (* b b)))
   (v2 (/ 2d0 (- r 1d0))))
  (declare (type double-float r b c a d s q t1 t2 v1 v2)
	   (type rational q)
	   (ignorable freedom))
  (let ((u1 (* 2d0 (unit-random :[])))
	(sign 1d0)
	(ux 0d0))
    (declare (type double-float u1 sign ux))
    (if (> u1 1d0)
	(setf sign -1d0 ux (* b (- u1 1d0)))
      (setf ux (* b u1)))
    (block outer
      (when (< ux a)
	(return-from outer))
      (let ((uy (* d (unit-random :[]))))
	(declare (type double-float uy))
	(when (< (* (half-integer-power (+ 1d0 (* p ux ux)) q) uy) c)
	  (return-from outer))
	(let ((dash (* s (- b ux))))
	  (declare (type double-float dash))
	  (when (< (* (half-integer-power (+ 1d0 (* p dash dash)) q)
		      (- t2 (* t1 uy)))
		   c)
	    (setf ux dash)
	    (return-from outer))
	  (loop
	    (let ((u3 (unit-random)))
	      (declare (type double-float u3))
	      (setf ux (sqrt (- (* v1 (the double-float (expt u3 (- v2)))) r)))
	      (unless (>= (* ux (unit-random)) b)
		(return-from outer)))))))
    (* sign ux)))

(defun-with-cached-values t-monty-python-bit (freedom)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type fixnum freedom))
  ((r (dfloat freedom))
   (b (case freedom       
	(3 3.142d0)
	(4 2.968d0)
	(5 2.868d0)
	(6 2.783d0)
	(7 2.756d0)
	(8 2.724d0)
	(t
	 (assert (> freedom 8))
	 (+ 2.5074d0 (expt (* 1.876d0 r) -1.042d0)))))
   (c (multiple-value-bind (quotient remainder) (floor freedom 2)
	(declare (type fixnum quotient remainder))
	(if (= remainder 0)
	    ;;; even
	    (/ (loop with ans = 1d0
		   for i fixnum from 1 below quotient do
		     (setf ans (* ans (+ (/ (* 2d0 i)) 1d0)))
		   finally (return ans))
	       (* 2d0 (the double-float (sqrt r))))
	  ;;; odd	  
	  (/ (loop with ans = 1d0
		 for i fixnum from 1 upto quotient do
		   (setf ans (* ans (+ (/ (- (* 2d0 i) 1d0)) 1d0)))
		 finally (return ans))
	     (* pi (the double-float (sqrt r)))))))
   (a (sqrt (* r (- (expt (* 2 b c) (/ 2 (+ freedom 1))) 1d0))))
   (d (/ (* b (- (expt 2 (floor +bit-operation-m+ 2)) 2))))
   (k (floor (/ (* a (- (expt 2 (floor +bit-operation-m+ 2)) 1)) b)))
   (w (/ b (- (expt 2 (floor +bit-operation-m+ 2)) 1)))
   (s (/ a (- b a)))
   (p (/ r))
   (q (/ (+ freedom 1) 2))
   (t1 (/ s))
   (t2 (/ (+ t1 1d0) 2 b))
   (v1 (+ r (* b b)))
   (v2 (/ 2d0 (- r 1d0))))
  (declare (type double-float r b c a d w s p t1 t2 v1 v2)
	   (type rational q)
	   (type fixnum k)
	   (ignorable freedom a))
  (let ((u-mbit (random most-positive-fixnum))
	(sign -1d0)
	(ux 0d0))
    (declare (type fixnum u-mbit)
	     (type double-float ux sign))
    (when (zerop (logand u-mbit 1))
      (setf sign 1d0))
    (setf u-mbit (ash u-mbit -1))
    (let ((u-m/2 (logand u-mbit #.(- (expt 2 (floor +bit-operation-m+ 2)) 1))))
      (declare (type fixnum u-m/2))
      (setf ux (* (dfloat u-m/2) w))
      (block outer
	(when (< u-m/2 k)
	  (return-from outer))
	(setf u-mbit (ash u-mbit #.(- (floor +bit-operation-m+ 2))))
	(let ((uy (* d (dfloat u-mbit))))
	  (declare (type double-float uy))
	  (when (< (* (half-integer-power (+ 1d0 (* p ux ux)) q) uy) c)
	    (return-from outer))
	  (let ((dash (* s (- b ux))))
	    (declare (type double-float dash))
	    (when (< (* (half-integer-power (+ 1d0 (* p dash dash)) q)
			(- t2 (* t1 uy)))
		     c)
	      (setf ux dash)
	      (return-from outer))
	    (loop
	      (let ((u3 (unit-random)))
		(declare (type double-float u3))
		(setf ux (sqrt (- (* v1 (expt u3 (- v2))) r)))
		(unless (>= (* ux (unit-random)) b)
		  (return-from outer))))))))
    (* sign ux)))

(define-compiler-macro t-monty-python-bit (&whole form freedom)
  (cond ((numberp freedom)
	 (assert (typep freedom 'fixnum))
	 (let*   ((r (dfloat freedom))
		  (b (case freedom
		       (3 3.142d0)
		       (4 2.968d0)
		       (5 2.868d0)
		       (6 2.783d0)
		       (7 2.756d0)
		       (8 2.724d0)
		       (t
			(assert (> freedom 8))
			(+ 2.5074d0 (expt (* 1.876d0 r) -1.042d0)))))
		  (c (multiple-value-bind (quotient remainder) (floor freedom 2)
		       (declare (type fixnum quotient remainder))
		       (if (= remainder 0)
			   ;;; even
			   (/ (loop with ans = 1d0
				  for i fixnum from 1 below quotient do
				    (setf ans (* ans (+ (/ (* 2d0 i)) 1d0)))
				  finally (return ans))
			      (* 2d0 (the double-float (sqrt r))))
			 ;;; odd
			 (/ (loop with ans = 1d0
				for i fixnum from 1 upto quotient do
				  (setf ans (* ans (+ (/ (- (* 2d0 i) 1d0)) 1d0)))
				finally (return ans))
			    (* pi (the double-float (sqrt r)))))))
		  (a (sqrt (* r (- (expt (* 2 b c) (/ 2 (+ freedom 1))) 1d0))))
		  (d (/ (* b (- (expt 2 (floor +bit-operation-m+ 2)) 2))))
		  (k (floor (/ (* a (- (expt 2 (floor +bit-operation-m+ 2)) 1)) b)))
		  (w (/ b (- (expt 2 (floor +bit-operation-m+ 2)) 1)))
		  (s (/ a (- b a)))
		  (p (/ r))
		  (q (/ (+ freedom 1) 2))
		  (t1 (/ s))
		  (t2 (/ (+ t1 1d0) 2 b))
		  (v1 (+ r (* b b)))
		  (v2 (/ 2d0 (- r 1d0))))
	   `(t-monty-python-bit-cached ,freedom ,r ,b ,c ,a ,d ,k ,w ,s ,p ,q ,t1 ,t2 ,v1 ,v2)))
	(t form)))

(defun-with-cached-values t-compression (freedom)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type fixnum freedom))
  ((r (dfloat freedom))
   (p (/ r))
   (q (/(+ freedom 1) 2))
   (xf (sqrt (/ r (+ r 2d0))))
   (fxf (half-integer-power (/ (+ 1d0 (* p xf xf))) q))
   (x2 (/ (sqrt (* r (+ r 2d0))) q))
   (fx2 (half-integer-power (/ (+ 1d0 (* p x2 x2))) q))
   (x1 (- x2
	  (/ (* (+ r 3) (sqrt r))
	     (* (+ r 1) (sqrt (+ r 2)) fxf))))
   (fx1 (half-integer-power (/ (+ 1d0 (* p x1 x1))) q))
   (d1 (/ fxf))
   (d2 (/ fx2))
   (r4 (- 1d0 fxf))
   (d3 (/ r4))
   (s3 (/ (- fxf 1d0) x1))
   (d4 (- xf x1))
   (s4 (- fx1 fxf))
   (d5 (- x2 xf))
   (r5 (- fxf fx2))
   (t5 (- 1d0 (/ (* xf d1 r5 (+ r 3d0))
		 (* d5 (+ r 1d0)))))
   (c6 (/ -2d0 (- r 1d0)))
   (d6 (* r (expt (* x2 (- 1d0 p)) c6)))
   (aarray (make-array 6
		       :element-type 'double-float
		       :initial-contents `(,(* xf fxf) ,(* d5 fx2) ,(* x1 r4)
					   ,(/ (* d4 r4) 2d0) ,(/ (* d5 r5) 2d0) ,(/ (* (+ r (* x2 x2)) fx2)
										     (* (- r 1d0) x2)))))
   (a (reduce #'+ aarray)))
  (declare (type double-float r p xf fxf x2 fx2 x1 fx1 d1 d2 r4 d3 s3 d4 s4 d5 r5 t5 c6 d6 aarray a)
	   (type (vector double-float 6) aarray)
	   (type rational q)
	   (ignorable x1 fx1 freedom))
  (let ((u (* 2d0 (unit-random :[)))
	(i -1)
	(sign 1d0))
    (declare (type double-float u sign)
	     (type fixnum i))
    (if (< u 1d0)
	(setf u (* a u))
      (setf sign -1d0 u (* a (- u 1d0))))
    (* (loop named outer do
	 (loop
	   (incf i)
	   (if (>= u (aref aarray i))
	       (setf u (- u (aref aarray i)))
	     (return)))
	 (ecase i
	   (0 (return-from outer (* d1 u)))
	   (1 (return-from outer (+ xf (* d2 u))))
	   (2 (let* ((z (* d3 u))
		     (ws (+ (* s3 z) 1d0))
		     (v (unit-random :[)))
		(declare (type double-float z ws v))
		(when (or (<= v ws) (<= (* v (half-integer-power (+ 1d0 (* p z z)) q)) 1d0))
		  (return-from outer z))))
	   (3 (let* ((v1 (/ u (aref aarray 3)))
		     (v2 (unit-random :[]))
		     (w (max v1 v2))
		     (wdash (if (= v1 w) v2 v1))
		     (z (- xf (* d4 w)))
		     (ws (* w (+ (* s4 w) fxf)))
		     (v (* wdash (+ (* r4 w) fxf))))
		(declare (type double-float v1 v2 w wdash ws v))
		(when (or (<= v ws) (<= (* v (half-integer-power (+ 1d0 (* p z z)) q)) w))
		  (return-from outer z))))
	   (4 (let* ((v1 (/ u (aref aarray 4)))
		     (v2 (unit-random :[]))
		     (w (max v1 v2))
		     (wdash (if (= v1 w) v2 v1))
		     (z (- x2 (* d5 w)))
		     (v (* wdash (+ (* r5 w) fx2))))
		(declare (type double-float v1 v2 w wdash v))
		(when (or (and (>= w t5) (<= v (* fxf w w))) (<= (* v (half-integer-power (+ 1d0 (* p z z)) q)) w))
		  (return-from outer z))))
	   (5 (let ((z (sqrt (- (* d6 (expt (- (aref aarray 5) u) c6)) r)))
		    (v (unit-random :[)))
		(declare (type double-float z v))
		(when (<= (* v z) x2)
		  (return-from outer z)))))
	 (setf u (* a (unit-random :[)) i -1))
       sign)))

(define-compiler-macro t-compression (&whole form freedom)
  (cond ((numberp freedom)
	 (assert (typep freedom 'fixnum))
	 (let*   ((r (dfloat freedom))
		  (p (/ r))
		  (q (/(+ freedom 1) 2))
		  (xf (sqrt (/ r (+ r 2d0))))
		  (fxf (half-integer-power (/ (+ 1d0 (* p xf xf))) q))
		  (x2 (/ (sqrt (* r (+ r 2d0))) q))
		  (fx2 (half-integer-power (/ (+ 1d0 (* p x2 x2))) q))
		  (x1 (- x2
			 (/ (* (+ r 3) (sqrt r))
			    (* (+ r 1) (sqrt (+ r 2)) fxf))))
		  (fx1 (half-integer-power (/ (+ 1d0 (* p x1 x1))) q))
		  (d1 (/ fxf))
		  (d2 (/ fx2))
		  (r4 (- 1d0 fxf))
		  (d3 (/ r4))
		  (s3 (/ (- fxf 1d0) x1))
		  (d4 (- xf x1))
		  (s4 (- fx1 fxf))
		  (d5 (- x2 xf))
		  (r5 (- fxf fx2))
		  (t5 (- 1d0 (/ (* xf d1 r5 (+ r 3d0))
				(* d5 (+ r 1d0)))))
		  (c6 (/ -2d0 (- r 1d0)))
		  (d6 (* r (expt (* x2 (- 1d0 p)) c6)))
		  (aarray (make-array 6
				      :element-type 'double-float
				      :initial-contents `(,(* xf fxf) ,(* d5 fx2) ,(* x1 r4)
								      ,(/ (* d4 r4) 2d0) ,(/ (* d5 r5) 2d0) ,(/ (* (+ r (* x2 x2)) fx2)
														(* (- r 1d0) x2)))))
		  (a (reduce #'+ aarray)))
	   `(t-compression-cached ,freedom ,r ,p ,q ,xf ,fxf ,x2 ,fx2 ,x1 ,fx1 
				 ,d1 ,d2 ,r4 ,d3 ,s3 ,d4 ,s4 ,d5 ,r5 ,t5 ,c6 ,d6 ,aarray ,a)))
	(t form)))

(defun t-random (freedom)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type fixnum freedom))
  (case freedom
    (1 (cauchy-random 0d0 1d0))
    (2 (/ (the double-float (normal-random 0d0 1d0))
	  (the double-float (sqrt (exp-random 1d0)))))
    (t (t-with-gamma freedom))))

(define-compiler-macro t-random (&whole form freedom)
  (cond ((numberp freedom)
	 (assert (typep freedom 'fixnum))
	 (case freedom
	   (1 `(cauchy-random 0d0 1d0))
	   (2 `(/ (the double-float (normal-random 0d0 1d0))
		  (the double-float (sqrt (exp-random 1d0)))))
	   (t `(t-monty-python-bit ,freedom))))
	(t form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       ;;
;; logistic distribution ;;
;;                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun logistic-inverse ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((u (unit-random)))
    (declare (type double-float u))
    (log (/ u (- 1d0 u)))))

(let* ((k +zuggurat-k+)
       (n (expt 2 k))
       (n-minus-1 (- n 1))
       (r (ecase k
	    (7 7.692874394777d0)
	    (8 8.479455643668d0)))
       (v (ecase k
	    (7 3.96111313029d-3)
	    (8 1.96803010835d-3)))
       (tr (exp r))
       (kn (make-array n :element-type 'fixnum))
       (wn (make-array n :element-type 'double-float))
       (fn (make-array n :element-type 'double-float))
       (base (expt 2 (- +bit-operation-m+ k 1))))
  (declare (type double-float r v tr)
	   (type fixnum k n n-minus-1 base)
	   (type (vector double-float *) wn fn)
	   (type (vector fixnum *) kn))
  ;; build arrays
  (setf (aref wn (- n 1)) (/ (* (expt (+ 1d0 tr) 2) v) tr base))
  (setf (aref wn (- n 2)) (/ r base))
  (setf (aref kn (- n 1)) (floor (/ r (aref wn (- n 1)))))
  (setf (aref fn (- n 1)) (/ tr (expt (+ 1d0 tr) 2)))
  (labels ((f (x)
	   (/ (exp (- x))
	      (expt (+ 1d0 (exp (- x))) 2)))
	   (update-t (x)
	     (+ (f x) (/ v x)))
	   (update-x (old-t)
	     (- (log (/ (- 1d0 (* 2 old-t) (sqrt (- 1d0 (* 4 old-t))))
			(* 2 old-t))))))
    (loop
      for tn = (update-t r) then (update-t xn)
      for xn = (update-x tn)
      for i from (- n 2) downto 1 do
	(setf (aref wn (- i 1)) (/ xn base))
	(setf (aref kn i) (floor (/ xn (aref wn i))))
	(setf (aref fn i) (f xn))
	))
  (setf (aref kn 0) 0)
  (setf (aref fn 0) 0.25d0)
  (defun logistic-ziggurat-bit ()
    (declare (optimize (speed 3) (safety 0) (debug 0)))
    (loop named logistic do
	  (let* ((u-mbit (random most-positive-fixnum))
		 (i (logand u-mbit #.(- (expt 2 +zuggurat-k+) 1)))) 
	    (declare (type fixnum u-mbit i))
	    (setf u-mbit (ash u-mbit #.(- +zuggurat-k+)))
	    (let ((sign (if (zerop (logand u-mbit 1))
			    1d0
			  -1d0)))
	      (declare (type double-float sign))
	      (setf u-mbit (ash u-mbit -1))
	      (when (< u-mbit (aref kn i))
		(return-from logistic (* u-mbit (aref wn i) sign)))
	      ;; step 5
	      (if (= i n-minus-1)
		  ;; step a
		  (let ((u (unit-random :[)))
		    (declare (type double-float u))
		    (return-from logistic (* sign
					     (the double-float (log (/ (+ tr u)
								       (- 1d0 u)))))))
		;; step b
		(let* ((ux (* u-mbit (aref wn i)))
		       (f (/ (exp (- ux))
			     (expt (+ 1d0 (exp (- ux))) 2)))
		       (u (unit-random :[))
		       (fi1 (aref fn (+ i 1))))
		  (declare (type double-float f ux u))
		  (when (<= (* u (- (aref fn i) fi1))
			    (- f fi1))
		    (return-from logistic (* sign ux))))))))))

(defun standard-logistic-random ()
  (logistic-ziggurat-bit))

(define-compiler-macro standard-logistic-random ()
  `(logistic-ziggurat-bit))

(defun logistic-random (location scale)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float location scale))
  (let ((logistic (standard-logistic-random)))
    (declare (type double-float logistic))
    (+ location (* scale logistic))))

(define-compiler-macro logistic-random (&whole form location scale)
  (declare (ignore form))
  (cond ((and (numberp location) (= location 0d0))
	 (assert (typep location 'double-float))
	 (cond ((and (numberp scale) (= scale 1d0))
		(assert (typep scale 'double-float))
		`(standard-logistic-random))
	       (t
		`(* ,scale (the double-float (standard-logistic-random))))))
	((and (numberp scale) (= scale 1d0))
	 (assert (typep scale 'double-float))
	 `(+ ,location (the double-float (standard-logistic-random))))
	(t
	 `(+ ,location (* ,scale (the double-float (standard-logistic-random)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; decrete-distributions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       ;;
;; Binomial distribution ;;
;;                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun-with-cached-values binomial-inverse (size probability)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float probability)
	   (type fixnum size))
  ((s (if (<= probability 0.5d0)
	  (/ probability (- 1d0 probability))
	(/ (- 1d0 probability) probability)))
   (a (* (+ size 1) s))
   (d (if (<= probability 0.5d0)
	  (int-power (- 1d0 probability) size)
	(int-power probability size))))
  (declare (type double-float s a d))
  (let ((x (if (<= probability 0.5d0)
	       0
	     size))
	(p d)
	(u (unit-random :[)))
    (declare (type fixnum x)
	     (type double-float p u))
    (loop
      (let ((v (- u p)))
	(declare (type double-float v))
	(when (<= v 0d0)
	  (return x))
	(cond ((<= probability 0.5d0)
	       (incf x)
	       (setf p (* p (- (/ a x) s))))
	      (t
	       (decf x)
	       (setf p (* p (- (/ a (- size x)) s)))))
	(setf u v)))))

(defun-with-cached-values binomial-inverse-mode (size probability)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float probability)
	   (type fixnum size))
  ((s (/ probability (- 1d0 probability)))
   (a (* (+ size 1) s))
   (tee (/ s))
   (b (* (+ size 1) tee))
   (m (floor (* probability (+ size 1))))
   (d (* (the double-float (int-power (- 1d0 probability) size))
	 (the double-float
	   (loop with ans double-float = 1d0	     
	       for i fixnum from 1 to m do
		 (setf ans (* ans (dfloat (/ (+ (- size i) 1) i)) s))
	       finally (return ans))))))
  (declare (type double-float s a tee b d)
	   (type fixnum m)
	   (ignorable probability))
  (let ((pu d)
	(pl d)
	(xu m)
	(xl m)
	(u (unit-random :[)))
    (declare (type double-float pu pl u)
	     (type fixnum xu xl))
    (let ((v (- u pu)))
      (declare (type double-float v))
      ;; step 4
      (if (<= v 0d0)
	  xu
	(progn
	  (setf u v)
	  (loop
	    ;; step 5
	    (cond ((> xl 0)
		   (decf xl)
		   (setf pl (* pl (- (/ b (- size xl)) tee)))
		   ;; step 6
		   (setf v (- u pl))
		   ;; step 7
		   (when (<= v 0d0)
		     (return xl))
		   (setf u v))
		  ((and (= xl 0) (= xu size))
		   (return size)))
	    ;; step 8
	    (cond ((< xu size)
		   (incf xu)
		   (setf pu (* pu (- (/ a xu) s)))
		   ;; step 3
		   (setf v (- u pu))
		   ;; step 4
		   (when (<= v 0d0)
		     (return xu))
		   (setf u v))
		  ((and (= xu size) (= xl 0))
		   (return 0)))))))))

(defun binomial-convolution (size probability)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float probability)
	   (type fixnum size))
  (let ((x 0))
    (declare (type fixnum x))
    (dotimes (i size)
      (when (<= (unit-random :[) probability)
	(incf x)))
    x))

(defun-with-cached-values binomial-convolution-recycle (size probability)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float probability)
	   (type fixnum size))
  ((p (/ probability))
   (q (/ (- 1d0 probability))))
  (declare (type double-float p q))
  (let ((x 0)
	(u (unit-random :[)))
    (declare (type fixnum x)
	     (type double-float u))
    (dotimes (i size)
      (cond ((<= u probability)
	     (incf x)
	     (setf u (* p u)))
	    (t
	     (setf u (* q (- u probability))))))
    x))

(defun binomial-convolution-coinflip (size)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type fixnum size))
  (let ((x 0)
	(i 0)
	(u-mbit (random most-positive-fixnum))
	(j 0))
    (declare (type fixnum x i u-mbit j))
    (loop
      (incf x (the fixnum (logand u-mbit 1)))
      (incf i)
      (when (= i size)
	(return x))
      (incf j)
      (if (= j +bit-operation-m+)
	  (setf u-mbit (random most-positive-fixnum))
	(setf u-mbit (ash u-mbit -1))))))

;; compression table lookup methods -- single call is *very* slow and use much memory
;; only use for compiler macro for efficient code
(defun binomial-table (size probability)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float probability)
	   (type fixnum size))
  (let* ((s (/ probability (- 1d0 probability)))
	 (a (* (+ size 1) s))
	 (d (expt (- 1d0 probability) size))
	 (tix (let ((table (make-array `(,+lookup-table-l+ ,(+ size 1)) :element-type 'fixnum))
		    (pbin d))
		(declare (type double-float pbin)
			(type (array fixnum (* *)) table))
		(loop for x fixnum from 0 to size do
		      (loop for i from 0 below +lookup-table-l+ do
			    (setf (aref table i x)
			      (- (floor (* pbin (expt 2 (* (+ i 1) +lookup-table-k+))))
				 (* (expt 2 +lookup-table-k+)
				    (floor (* pbin (expt 2 (* i +lookup-table-k+))))))))
		      (setf pbin (* pbin (- (/ a (+ x 1)) s))))
		(let ((ans (make-array +lookup-table-l+ :element-type t)))
		  (loop for i from 0 below +lookup-table-l+ do
		       (let* ((n (loop for x from 0 to size summing (aref table i x)))
			      (ti (make-array n :element-type 'fixnum)))
			 (setf (aref ans i) ti)
			 (loop with j = 0
			     for x from 0 to size
			     for tix = (aref table i x) do
			       (loop for index from j below (+ j tix) do
				     (setf (aref ti index) x))
			       (incf j tix))))
		  ans)))
	 (si (let ((ans (make-array #.(- +lookup-table-l+ 1) :element-type 'fixnum)))
	       (loop
		   with acc fixnum = 0
		  for i from 0 below #.(- +lookup-table-l+ 1) do
		     (incf acc (* (length (aref tix i)) (expt 2 (* (- #.(- +lookup-table-l+ 1) i) +lookup-table-k+))))
		     (setf (aref ans i) acc))
	       ans)))
    (declare (type double-float s a d)
	     (type vector tix)
	     (type (vector fixnum *) si))
    (values tix si)))

(defun binomial-table-lookup (tix si)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type vector tix)
	   (type (array fixnum (*)) si))
  (let ((r (logand (random most-positive-fixnum) #.(- (expt 2 (* +lookup-table-k+ +lookup-table-l+)) 1)))
	(i 0))
    (declare (type fixnum r i))
    (loop
      (if (< r (aref si i))
	  (return)
	(incf i))
      (when (= i #.(- +lookup-table-l+ 1))
	(return)))
    (unless (= i 0)
      (setf r (- r (aref si (- i 1)))))
    (aref (the (vector fixnum) (aref tix i)) (ash r (* (- i #.(- +lookup-table-l+ 1)) +lookup-table-k+)))))

(defun binomial-table-histogram (size probability)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float probability)
	   (type fixnum size))
  (let* ((nsq (+ size 1))
	 (s (/ probability (- 1d0 probability)))
	 (a (* nsq s))
	 (pbins (make-array nsq :element-type 'double-float))
	 (k 7)
	 (b (expt 2 k)))
    (declare (type double-float s a)
	     (type fixnum nsq k b)
	     (type (vector double-float *) pbins))
    ;; build pbins
    (setf (aref pbins 0) (int-power (- 1d0 probability) size))
    (loop for i from 1 to size do
	  (setf (aref pbins i) (* (- (/ a i) s) (aref pbins (1- i)))))
    (loop
      (incf k)
      (setf b (* b 2))
      (when (or (= k 16)
		(> (/ (reduce #'+ pbins :key #'(lambda (x) (floor (* b x)))) b)
		   0.9))
	(return)))
    (let ((w (dfloat (expt 2 (- k +bit-operation-m+))))
	  (table (make-array b :initial-element -1 :element-type 'fixnum))
	  (thetan 0d0))
      (declare (type double-float w thetan)
	       (type (vector fixnum *) table))
      (loop with j = 0
	  for x from 0
	  for pbin across pbins
	  for tx = (floor (* b pbin)) do
	    (loop for index from j below (+ j tx) do
		  (setf (aref table index) x))
	    (incf thetan (setf (aref pbins x) (- (* b pbin) tx)))
	    (incf j tx))
      ;; pbins -> thetas
      (map-into pbins #'(lambda (x) (/ x thetan)) pbins)
      ;; robin hood
      (let ((ki (make-array nsq :element-type 'fixnum :initial-contents (loop for i from 0 to size collect i)))
	    (vi (make-array nsq :element-type 'double-float
			    :initial-contents (loop for i from 0 to size collect (dfloat (/ (+ i 1) nsq)))))
	    (c (dfloat (/ nsq))))
	(declare (type (vector fixnum *) ki)
		 (type (vector double-float *) vi)
		 (type double-float c))
	(loop repeat size do
	      (let ((maxp 0)
		    (max (aref pbins 0))
		    (minp 0)
		    (min (aref pbins 0)))
		(loop for i from 1 to size
		    for p = (aref pbins i) do
		      (cond ((> p max)
			     (setf max p maxp i))
			    ((< p min)
			     (setf min p minp i))))
		(setf (aref ki minp) maxp)
		(setf (aref vi minp) (+ (* minp c) min))
		(decf (aref pbins maxp) (- c (aref pbins minp)))
		(setf (aref pbins minp) c)))
	(values table ki vi (- b 1) (- k) w nsq)))))

(defun binomial-table-histogram-lookup (table ki vi b k w nsq)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float w)
	   (type (vector fixnum) table ki)
	   (type (vector double-float) vi)
	   (type fixnum b k nsq))
  (let* ((i (random most-positive-fixnum))
	 (ilk (logand i b))
	 (tilk (aref table ilk)))
    (declare (type fixnum i ilk tilk))
    (if (= tilk -1)
	;; step4
	(let* ((u (* w (the fixnum (ash i k))))
	       (j (floor (* nsq u))))
	  (declare (type double-float u)
		   (type fixnum j))
	  (if (< u (aref vi j))
	      j
            (aref ki j)))
      tilk)))

;; you will fix this value for your implementation
(defvar binomial-switch-size 30)

(defun binomial-random (size probability)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float probability)
	   (type fixnum size))
  (cond ((= probability 0.5d0)
	 (binomial-convolution-coinflip size))
	((> size (the fixnum binomial-switch-size))
	 (binomial-inverse size probability))
	(t
	 (binomial-convolution-recycle size probability))))

(define-compiler-macro binomial-random (&whole form size probability)
  (cond ((and (numberp size) (numberp probability))
	 (assert (and (typep size 'fixnum) (typep probability 'double-float)))
	 (multiple-value-bind (table ki vi b k w nsq) (binomial-table-histogram size probability)
	   `(binomial-table-histogram-lookup ,table ,ki ,vi ,b ,k ,w ,nsq)))
	((numberp probability)
	 (assert (typep probability 'double-float))
	 (if (= probability 0.5d0)
	     `(binomial-convolution-coinflip ,size)
	   (let ((p (/ probability))
		 (q (/ (- 1d0 probability)))
		 (s (if (<= probability 0.5d0)
			(/ probability (- 1d0 probability))
		      (/ (- 1d0 probability) probability)))
		 (d-base (if (<= probability 0.5d0)
			     (- 1d0 probability)
			   probability))
		 (sym (gensym "size")))
	     `(let ((,sym ,size))
		(declare (type fixnum ,sym))
		(if (> ,sym (the fixnum binomial-switch-size))
		    (binomial-inverse-cached ,sym ,probability ,s (* (+ ,sym 1) ,s) (int-power ,d-base ,sym))
		  (binomial-convolution-recycle-cached ,sym ,probability ,p ,q))))))
	((numberp size)
	 (assert (typep size 'fixnum))
	 (if (> size (the fixnum binomial-switch-size))
	     `(binomial-inverse ,size ,probability)
	   `(binomial-convolution-recycle ,size ,probability)))
	(t form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        ;;
;; Geometric distribution ;;
;;                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun geometric-bernoulli (probability)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float probability))
  (let ((x 1))
    (declare (type fixnum x))
    (loop
      (if (<= (unit-random :[) probability)
	  (return x)
	(incf x)))))

(defun-with-cached-values geometric-bernoulli-recycle (probability)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float probability))
  ((q (/ (- 1d0 probability))))
  (declare (type double-float q))
  (let ((x 1)
	(u (unit-random :[)))
    (declare (type fixnum x)
	     (type double-float u))
    (loop
      (if (<= u probability)
	  (return x)
	(progn (incf x)
	       (setf u (* q (- u probability))))))))

(defun geometric-bernoulli-coinflip ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((x 1)
	(u-mbit (random most-positive-fixnum))
	(j 0))
    (declare (type fixnum x u-mbit j))
    (loop
      (if (= (the fixnum (logand u-mbit 1)) 1)
	  (return x)
	(progn (incf x)
	       (incf j)
	       (if (= j +bit-operation-m+)
		   (setf u-mbit (random most-positive-fixnum) j 0)
		 (setf u-mbit (ash u-mbit -1))))))))

(defun-with-cached-values geometric-inverse (probability)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float probability))
  ((q (- 1d0 probability)))
  (declare (type double-float q))
  (let ((x 1)
	(p probability)
	(u (unit-random :[)))
    (declare (type fixnum x)
	     (type double-float p u))
    (loop
      (let ((v (- u p)))
	(declare (type double-float v))
	(if (<= v 0d0)
	    (return x)
	  (progn (incf x)
		 (setf p (* p q) u v)))))))

(defun-with-cached-values geometric-inverse-exp (probability)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float probability))
  ((d (- (/ (the double-float (log (- 1d0 probability)))))))
  (declare (ignorable probability)
	   (type double-float d))
  ;;; floor is faster than ceiling
  (+ (the fixnum (floor (* d (exp-random 1d0))))
     1))

(defun geometric-table-histogram (probability)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float probability))
  (let* ((s (- 1d0 probability))
	 (c1 (/ (log s)))
	 (pgeos (make-array 1 :element-type 'double-float :fill-pointer t :adjustable t :initial-element probability))
	 (k 7)
	 (nsq 0)
	 (b (expt 2 k)))
    (declare (type double-float s c1)
	     (type fixnum k b nsq)
	     (type (vector double-float *) pgeos))
    (loop
      (incf k)
      (setf b (* b 2))
      (setf nsq (1+ (the fixnum
		      (floor (* (- c1) (the double-float (log (* b probability))))))))
      (let ((nt 0))
	(declare (type fixnum nt))
	(loop for i from 0 below nsq do
	      (incf nt (floor (* b (if (= (fill-pointer pgeos) i)
				       (let ((new (* s (aref pgeos (1- i)))))
					 (declare (type double-float new))
					 (vector-push-extend new pgeos)
					 new)
				     (aref pgeos i))))))
	(when (or (= k 16)
		  (> (/ nt b) 0.9d0))
	  (return))))
    (let ((w (dfloat (expt 2 (- k +bit-operation-m+))))
	  (table (make-array b :initial-element -1 :element-type 'fixnum))
	  (thetan 0d0)
	  (sum 0d0))
      (declare (type double-float w thetan sum)
	       (type (vector fixnum *) table))
      (loop with j = 0
	  for x from 0
	  for pgeo across pgeos
	  for tx = (floor (* b pgeo)) do
	    (loop for index from j below (+ j tx) do
		  (setf (aref table index) (1+ x)))
	    (incf sum pgeo)
	    (incf thetan (setf (aref pgeos x) (- (* b pgeo) tx)))
	    (incf j tx))
      ;; pgeos -> thetas
      (map-into pgeos #'(lambda (x) (/ x thetan)) pgeos)
      ;; robin hood
      (let ((ki (make-array nsq :element-type 'fixnum :initial-contents (loop for i from 0 below nsq collect i)))
	    (vi (make-array nsq :element-type 'double-float
			    :initial-contents (loop for i from 0 below nsq collect (dfloat (/ (+ i 1) nsq)))))
	    (c (dfloat (/ nsq))))
	(declare (type (vector fixnum *) ki)
		 (type (vector double-float *) vi)
		 (type double-float c))
	(loop repeat (1- nsq) do
	      (let ((maxp 0)
		    (max (aref pgeos 0))
		    (minp 0)
		    (min (aref pgeos 0)))
		(loop for i from 1 below nsq
		    for p = (aref pgeos i) do
		      (cond ((> p max)
			     (setf max p maxp i))
			    ((< p min)
			     (setf min p minp i))))
		(setf (aref ki minp) maxp)
		(setf (aref vi minp) (+ (* minp c) min))
		(decf (aref pgeos maxp) (- c (aref pgeos minp)))
		(setf (aref pgeos minp) c)))
	(map-into ki #'(lambda (x) (+ x 1)) ki)
	(let* ((ptx0 (- 1d0 sum))
	       (psq (/ thetan (+ (* b ptx0) thetan)))
	       (q (/ psq))
	       (r (/ ptx0 (- 1d0 psq))))
	  (declare (type double-float ptx0 psq q r))
	  (values table ki vi (- b 1) (- k) w nsq psq q r c1))))))

(defun geometric-table-histogram-lookup (table ki vi b k w nsq psq q r c)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float w psq q r c)
	   (type (vector fixnum) table ki)
	   (type (vector double-float) vi)
	   (type fixnum b k nsq))
  (let* ((i (random most-positive-fixnum))
	 (ilk (logand i b))
	 (tilk (aref table ilk)))
    (declare (type fixnum i ilk tilk))
    (if (= tilk -1)
	;; step4
	(let ((u (* w (the fixnum (ash i k)))))
	  (declare (type double-float u))
	  (if (< u psq)
	      (let* ((udash (* q u))
		     (j (floor (* nsq udash))))
		(declare (type fixnum j)
			 (type double-float udash))
		(if (< udash (aref vi j))
		    (+ j 1)
		  (aref ki j)))
	    (+ (the fixnum (floor (* c (the double-float (log (* r (- 1d0 u))))))) 1)))
      tilk)))

;; inverse-exp-cached is faster than inverse-cached, but cache is disable in this situation
;; inverse-exp direct call is slow because of 'log' calling
(defun geometric-random (probability)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float probability))
  (if (= probability 0.5d0)
      (geometric-bernoulli-coinflip)
    (geometric-inverse probability)))

(define-compiler-macro geometric-random (&whole form probability)
  (cond ((numberp probability)
	 (assert (typep probability 'double-float))
	 (if (= probability 0.5d0)
	     `(geometric-bernoulli-coinflip)
	   (multiple-value-bind (table ki vi b k w nsq psq q r c) (geometric-table-histogram probability)
	     `(geometric-table-histogram-lookup ,table ,ki ,vi ,b ,k ,w ,nsq ,psq ,q ,r ,c))))
	(t form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      ;;
;; Poisson distribution ;;
;;                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun-with-cached-values poisson-simulate (rate)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float rate))
  ((d (exp rate)))
  (declare (type double-float d)
	   (ignorable rate))
  (let ((x 0)
	(v d))
    (declare (type fixnum x)
	     (type double-float v))
    (loop
      (let* ((u (unit-random))
	     (w (* u v)))
	(declare (type double-float u w))
	(if (< w 1d0)
	    (return x)
	  (progn (incf x) (setf v w)))))))

(defun poisson-simulate-exp (rate)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float rate))
  (let ((x 0)
	(v 0d0))
    (declare (type fixnum x)
	     (type double-float v))
    (loop
      (let* ((e (exp-random 1d0))
	     (w (+ v e)))
	(declare (type double-float e w))
	(if (> w rate)
	    (return x)
	  (progn (incf x) (setf v w)))))))

(defun-with-cached-values poisson-inverse (rate)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float rate))
  ((d (exp (- rate))))
  (declare (type double-float d))
  (let ((x 0)
	(p d)
	(u (unit-random :[)))
    (declare (type fixnum x)
	     (type double-float p u))
    (loop
      (let ((v (- u p)))
	(declare (type double-float v))
	(if (<= v 0d0)
	    (return x)
	  (progn (incf x) (setf p (* p (/ rate x)) u v)
		 (when (zerop p)
		   ;; cut off underflow
		   (return x))))))))

(defun-with-cached-values poisson-inverse-mode (rate)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float rate))
  ((c (/ rate))
   (m (floor rate))
   (d (exp (- (* m (log rate))
	      rate
	      (the double-float (loop for i from 1 upto m summing (dfloat (log i)) of-type double-float))))))
  (declare (type double-float c d)
	   (type fixnum m))
  (let ((pu d)
	(pl d)
	(xu m)
	(xl m)
	(u (unit-random :[)))
    (declare (type double-float pu pl u)
	     (type fixnum xu xl))
    (loop
      (let ((v (- u pu)))
	(declare (type double-float v))
	;; step 4
	(if (<= v 0d0)
	    (return xu)
	  (progn
 	    (setf u v)
	    ;; step 5
	    (unless (= xl 0)
	      (setf pl (* c xl pl))
	      (decf xl)
	      (setf v (- u pl))
	      (if (<= v 0d0)
		  (return xl)
		(setf u v)))
	    (incf xu)
	    (setf pu (* pu (/ rate xu)))
	    (when (zerop pu)
	      ;; cut off underflow
	      (return xu))))))))

(defun poisson-table-histogram (rate)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float rate))
  (let* ((c (/ rate))
	 (m (floor rate))
	 (pps (make-array (1+ m) :element-type 'double-float :fill-pointer t :adjustable t :initial-element 0d0))
	 (k 7)
	 (xu m)
	 (xl m)
	 (b (expt 2 k)))
    (declare (type double-float c)
	     (type fixnum k b m xu xl)
	     (type (vector double-float *) pps))
    (setf (aref pps m) (exp (- (* m (log rate))
			       rate
			       (the double-float
				 (loop for i from 1 upto m summing (dfloat (log i)) of-type double-float)))))
    (loop
      (incf k)
      (setf b (* b 2))
      (let ((nt (floor (* b (aref pps m))))
	    (threshold (dfloat (/ b))))
	(declare (type fixnum nt xu xl)
		 (type double-float threshold))
	(setf xu m xl m)
	(loop
	  (when (>= xl 1)
	    (let ((new (aref pps (1- xl))))
	      (declare (type double-float new))
	      (when (zerop new)
		(setf new (* c xl (aref pps xl)))
		(setf (aref pps (1- xl)) new))
	      (incf nt (floor (* b new))))
	    (decf xl))
	  (incf xu)
	  (when (>= xu (fill-pointer pps))
	    (let ((new (/ (* rate (aref pps (1- xu))) xu)))
	      (declare (type double-float new))
	      (vector-push-extend new pps)))
	  (incf nt (floor (* b (aref pps xu))))
	  (when (and (or (= xl 0) (< (aref pps xl) threshold)) (< (aref pps xu) threshold))
	    (return)))
	(when (or (= k 16)
		  (> (/ nt b) 0.9d0))
	  (return))))
    (let* ((pl (if (zerop xl)
		   0d0
		 (* c xl (aref pps xl))))
	   (pu (/ (* rate (aref pps xu)) xu))
	   (nsq (+ (- xu xl) 1))
	   (d (1+ xu))
	   (w (dfloat (expt 2 (- k +bit-operation-m+))))
	   (table (make-array b :initial-element -1 :element-type 'fixnum))
	   (thetan 0d0)
	   (sum 0d0))
      (declare (type double-float w thetan sum pl pu)
	       (type fixnum nsq d)
	       (type (vector fixnum *) table))
      (unless (= xl 0)
	(setf pps (subseq pps xl)))
      (loop with j = 0
	  for x from xl
	  for pp across pps
	  for tx = (floor (* b pp)) do
	    (loop for index from j below (+ j tx) do
		  (setf (aref table index) x))
	    (incf sum pp)
	    (incf thetan (setf (aref pps x) (- (* b pp) tx)))
	    (incf j tx))
      ;; pps -> thetas
      (map-into pps #'(lambda (x) (/ x thetan)) pps)
      ;; robin hood
      (let ((ki (make-array nsq :element-type 'fixnum :initial-contents (loop for i from 0 below nsq collect i)))
	    (vi (make-array nsq :element-type 'double-float
			    :initial-contents (loop for i from 0 below nsq collect (dfloat (/ (+ i 1) nsq)))))
	    (c (dfloat (/ nsq))))
	(declare (type (vector fixnum *) ki)
		 (type (vector double-float *) vi)
		 (type double-float c))
	(loop repeat (1- nsq) do
	      (let ((maxp 0)
		    (max (aref pps 0))
		    (minp 0)
		    (min (aref pps 0)))
		(loop for i from 1 below nsq
		    for p = (aref pps i) do
		      (cond ((> p max)
			     (setf max p maxp i))
			    ((< p min)
			     (setf min p minp i))))
		(setf (aref ki minp) maxp)
		(setf (aref vi minp) (+ (* minp c) min))
		(decf (aref pps maxp) (- c (aref pps minp)))
		(setf (aref pps minp) c)))
	(map-into ki #'(lambda (x) (+ x xl)) ki)
	(let* ((ptx0 (- 1d0 sum))
	       (psq (/ thetan (+ (* b ptx0) thetan)))
	       (q (/ psq))
	       (r (/ ptx0 (- 1d0 psq))))
	  (declare (type double-float ptx0 psq q r))
	  (values table ki vi (- b 1) (- k) w nsq psq q r xl d pl pu c rate))))))

(defun poisson-table-histogram-lookup (table ki vi b k w nsq psq q r xl xu pl pu c rate)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float w psq q r pl pu rate c)
	   (type (vector fixnum) table ki)
	   (type (vector double-float) vi)
	   (type fixnum b k nsq xu xl))
  (let* ((i (random most-positive-fixnum))
	 (ilk (logand i b))
	 (tilk (aref table ilk)))
    (declare (type fixnum i ilk tilk))
    (if (= tilk -1)
	;; step4
	(let ((u (* w (the fixnum (ash i k)))))
	  (declare (type double-float u))
	  (if (< u psq)
	      (let* ((udash (* q u))
		     (j (floor (* nsq udash))))
		(declare (type fixnum j)
			 (type double-float udash))
		(if (< udash (aref vi j))
		    (+ j xl)
		  (aref ki j)))
	    ;;; inverse-method
	    (let ((udash (* r (- u psq))))
	      (declare (type double-float udash))
	      (loop
		(let ((v (- udash pu)))
		  (declare (type double-float v))
		  ;; step 4
		  (if (<= v 0d0)
		      (return xu)
		    (progn
		      (setf udash v)
		      ;; step 5
		      (unless (= xl 0)
			(setf pl (* c xl pl))
			(decf xl)
			(setf v (- udash pl))
			(if (<= v 0d0)
			    (return xl)
			  (setf udash v)))
		      (incf xu)
		      (setf pu (* pu (/ rate xu)))
		      (when (zerop pu)
			(return xu)))))))))
      tilk)))

(declaim (type double-float poisson-switch-rate1))
(defvar poisson-switch-rate1 1d0)

(defun poisson-random (rate)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float rate))
  (if (< rate poisson-switch-rate1)
      (poisson-simulate-exp rate)
    (if (> rate #.(- (log least-positive-double-float)))
	(poisson-inverse-mode rate)
      (poisson-inverse rate))))

(define-compiler-macro poisson-random (&whole form rate)
  (cond ((numberp rate)
	 (assert (typep rate 'double-float))
	 `(poisson-table-histogram-lookup ,@(multiple-value-list (poisson-table-histogram rate))))
	(t form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             ;;
;; Hypergeometric distribution ;;
;;                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hypergeometric-simulate (elements successes samples)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type fixnum elements successes samples))
  (let ((s elements)
	(g successes)
	(m samples)
	(c (- elements successes))
	(x 0))
    (declare (type fixnum s g m c x))
    (loop
      (let ((u (unit-random :[)))
	(declare (type double-float u))
	(if (<= (* s u) g)
	    (progn (incf x)
		   (decf g)
		   (when (zerop g)
		     (return x)))
	  (decf c))
	(decf s)
	(decf m)
	(when (zerop m)
	  (return x))))))

(defun-with-cached-values hypergeometric-inverse (elements successes samples)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type fixnum elements successes samples))
  ((a (if (> samples (- elements successes))
	  (+ (- samples elements) successes)
	0))
   (d (/ (dfloat (the fixnum (* (combination successes a) (combination (- elements successes) (- samples a)))))
	 (dfloat (combination elements samples))))
   (b1 (- elements successes samples))
   (b2 (+ successes 1))
   (b3 (+ samples 1)))
  (declare (type fixnum a b1 b2 b3)
	   (type double-float d)
	   (ignorable elements successes samples))
  (let ((x a)
	(p d)
	(u (unit-random :[)))
    (declare (type fixnum x)
	     (type double-float p u))
    (loop
      (let ((v (- u p)))
	(declare (type double-float v))
	(when (<= v 0d0)
	  (return x))
	(incf x)
	(setf p (* p (/ (dfloat (the fixnum (* (the fixnum (- b2 x)) (the fixnum (- b3 x)))))
			(dfloat (the fixnum (* x (the fixnum (+ b1 x)))))))
	      u v)
	(when (zerop p)
	  (return x))))))

(defun-with-cached-values hypergeometric-inverse-mode (elements successes samples)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type fixnum elements successes samples))
  ((a1 (if (> samples (- elements successes))
	  (+ (- samples elements) successes)
	 0))
   (a2 (if (> samples successes)
	   successes
	 samples))
   (b1 (- elements successes samples))
   (b2 (+ successes 1))
   (b3 (+ samples 1))
   (m (floor (the fixnum (* b2 b3)) (the fixnum (+ elements 2))))
   (d (/ (dfloat (the fixnum (* (combination successes m) (combination (- elements successes) (- samples m)))))
	 (dfloat (combination elements samples)))))
  (declare (type double-float d)
	   (fixnum a1 a2 b1 b2 b3 m)
	   (ignorable elements successes samples))
  (let ((pu d)
	(pl d)
	(xu m)
	(xl m)
	(u (unit-random :[)))
    (declare (type double-float pu pl u)
	     (type fixnum xu xl))
    (let ((v (- u pu)))
      (declare (type double-float v))
      ;; step 4
      (if (<= v 0d0)
	  xu
	(progn
	  (setf u v)
	  (loop
	    ;; step 5
	    (cond ((= xl a1)
		   (when (= xu a2)
		 (return a2)))
		  (t
		   (setf pl (* (/ (dfloat (the fixnum (* xl (+ b1 xl))))
				  (dfloat (the fixnum (* (- b2 xl) (- b3 xl)))))
			       pl))
		   (decf xl)
		   ;; step6
		   (setf v (- u pl))
		   (if (<= v 0d0)
		       (return xl)
		     (setf u v))))
	    ;; step8
	    (cond ((= xu a2)
		   (when (= xl a1)
		     (return a1)))
		  (t
		   (incf xu)
		   (setf pu (* pu (/ (dfloat (the fixnum (* (- b2 xu) (- b3 xu))))
				     (dfloat (the fixnum (* xu (+ b1 xu)))))))
		   (when (zerop pu)
		     ;; cut off underflow
		     (return xu))
		   ;; step3,4
		   (setf v (- u pu))
		   (when (<= v 0d0)
		     (return xu))
		   (setf u v)))))))))

(defun hypergeometric-table-histogram (elements successes samples)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type fixnum elements successes samples))
  (let* ((a1 (if (> samples (- elements successes))
		 (+ (- samples elements) successes)
	       0))
	 (a2 (if (> samples successes)
		 successes
	       samples))
	 (nsq (+ (- a2 a1) 1))
	 (phs (make-array nsq :element-type 'double-float))
	 (k 7)
	 (b (expt 2 k)))
    (declare (type fixnum a1 a2 nsq k b)
	     (type (vector double-float *) phs))
    ;; build phs
    (setf (aref phs 0)
      (/ (dfloat (the fixnum (* (combination successes a1) (combination (- elements successes) (- samples a1)))))
	 (dfloat (combination elements samples))))
    (loop for i from 1 below nsq do
	  (setf (aref phs i)
	    (let ((x (- (+ i a1) 1)))
	      (declare (type fixnum x))
	      (* (/ (dfloat (the fixnum (* (- samples x) (- successes x))))
		    (dfloat (the fixnum (* (+ x 1) (+ (- elements successes samples) x 1)))))
		 (aref phs (1- i)))
	      #+ignore
	      (/ (dfloat (the fixnum (* (combination successes x) (combination (- elements successes) (- samples x)))))
		 (dfloat (combination elements samples))))))
    (loop
      (incf k)
      (setf b (* b 2))
      (when (or (= k 16)
		(> (/ (reduce #'+ phs :key #'(lambda (x) (floor (* b x)))) b)
		   0.9))
	(return)))
    (let ((w (dfloat (expt 2 (- k +bit-operation-m+))))
	  (table (make-array b :initial-element -1 :element-type 'fixnum))
	  (thetan 0d0))
      (declare (type double-float w thetan)
	       (type (vector fixnum *) table))
      (loop with j = 0
	  for x from a1
	  for i from 0
	  for ph across phs
	  for tx = (floor (* b ph)) do
	    (loop for index from j below (+ j tx) do
		  (setf (aref table index) x))
	    (incf thetan (setf (aref phs i) (- (* b ph) tx)))
	    (incf j tx))
      ;; phs -> thetas
      (map-into phs #'(lambda (x) (/ x thetan)) phs)
      ;; robin hood
      (let ((ki (make-array nsq :element-type 'fixnum :initial-contents (loop for i from 0 below nsq collect i)))
	    (vi (make-array nsq :element-type 'double-float
			    :initial-contents (loop for i from 0 below nsq collect (dfloat (/ (+ i 1) nsq)))))
	    (c (dfloat (/ nsq))))
	(declare (type (vector fixnum *) ki)
		 (type (vector double-float *) vi)
		 (type double-float c))
	(loop repeat (1- nsq) do
	      (let ((maxp 0)
		    (max (aref phs 0))
		    (minp 0)
		    (min (aref phs 0)))
		(loop for i from 1 below nsq
		    for p = (aref phs i) do
		      (cond ((> p max)
			     (setf max p maxp i))
			    ((< p min)
			     (setf min p minp i))))
		(setf (aref ki minp) maxp)
		(setf (aref vi minp) (+ (* minp c) min))
		(decf (aref phs maxp) (- c (aref phs minp)))
		(setf (aref phs minp) c)))
	(map-into ki #'(lambda (x) (+ x a1)) ki)
	(values table ki vi (- b 1) (- k) w nsq a1)))))

(defun hypergeometric-table-histogram-lookup (table ki vi b k w nsq a1)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float w)
	   (type (vector fixnum) table ki)
	   (type (vector double-float) vi)
	   (type fixnum b k nsq a1))
  (let* ((i (random most-positive-fixnum))
	 (ilk (logand i b))
	 (tilk (aref table ilk)))
    (declare (type fixnum i ilk tilk))
    (if (= tilk -1)
	;; step4
	(let* ((u (* w (the fixnum (ash i k))))
	       (j (floor (* nsq u))))
	  (declare (type double-float u)
		   (type fixnum j))
	  (if (< u (aref vi j))
	      (+ j a1)
            (aref ki j)))
      tilk)))

(defun hypergeometric-random (elements successes samples)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type fixnum elements successes samples))
  (hypergeometric-inverse-mode elements successes samples))

(define-compiler-macro hypergeometric-random (&whole form elements successes samples)
  (cond ((and (numberp elements) (numberp successes) (numberp samples))
	 (assert (and (typep elements 'fixnum) (typep successes 'fixnum) (typep samples 'fixnum)))
	 `(hypergeometric-table-histogram-lookup ,@(multiple-value-list
						      (hypergeometric-table-histogram elements successes samples))))
	(t form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                ;;
;; Negative Binomial distribution ;;
;;                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun-with-cached-values negative-binomial-compose (successes probability)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float successes probability))
  ((beta (- (/ probability) 1d0)))
  (declare (type double-float beta)
	   (ignorable probability))
  (poisson-random (gamma-random successes beta)))

(defun negative-binomial-convolution-integer (successes probability)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float successes probability))
  (loop until (<= successes 0d0)
      summing (- (the fixnum (geometric-random probability)) 1) of-type fixnum
      do (decf successes)))

(defun-with-cached-values negative-binomial-inverse (successes probability)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float successes probability))
  ((d (expt probability successes))
   (q (- 1d0 probability))
   (s (- successes 1d0)))
  (declare (type double-float d q s)
	   (ignorable successes probability))
  (let ((x 0)
	(p d)
	(u (unit-random :[)))
    (declare (type fixnum x)
	     (type double-float p u))
    (loop
      (let ((v (- u p)))
	(declare (type double-float v))
	(when (<= v 0d0)
	  (return x))
	(incf x)
	(setf p (/ (* p q (+ s x)) x))
	(setf u v)))))

(defun-with-cached-values negative-binomial-inverse-mode (successes probability)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float successes probability))
  ((q (- 1d0 probability))
   (s (/ q))
   (tee (- successes 1d0))
   (m (if (> successes 1d0)
	  (floor (/ (* q tee) probability))
	0))
   (d (if (> successes 1d0)
	  (* (the double-float (expt probability successes))
	     (the double-float (expt q m))
	     (loop with ans double-float = 1d0
		 for i fixnum from 1 upto m do
		   (setf ans (* ans (/ (+ i tee) i)))
		 finally (return ans)))
	(expt probability successes))))
  (declare (type double-float q s tee d)
	   (type fixnum m)
	   (ignorable successes probability))
  (let ((pu d)
	(pl d)
	(xu m)
	(xl m)
	(u (unit-random :[)))
    (declare (type double-float pu pl u)
	     (type fixnum xu xl))
    (loop
      (let ((v (- u pu)))
	(declare (type double-float v))
	;; step 4
	(when (<= v 0d0)
	  (return xu))
	(setf u v)
	;; step 5
	(unless (= xl 0)
	  (setf pl (/ (* pl s xl) (+ tee xl)))
	  (decf xl)
	  (setf v (- u pl))
	  (when (<= v 0d0)
	    (return xl))
	  (setf u v))
	;; step8
	(incf xu)
	(setf pu (/ (* pu q (+ tee xu)) xu))))))

(defun negative-binomial-table-histogram (successes probability)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float successes probability))
  (let* ((s (- 1d0 probability))
	 (u (/ s))
	 (v (- successes 1d0))
	 (m (if (> successes 1d0)
		(floor (/ (* s v) probability))
	      0))	 
	 (pnbs (make-array (1+ m) :element-type 'double-float :fill-pointer t :adjustable t :initial-element 0d0))
	 (k 7)
	 (xu m)
	 (xl m)
	 (b (expt 2 k)))
    (declare (type double-float s u v)
	     (type fixnum k b m xu xl)
	     (type (vector double-float *) pnbs))
    (setf (aref pnbs m) (if (> successes 1d0)
			   (* (the double-float (expt probability successes))
			      (the double-float (expt s m))
			      (loop with ans double-float = 1d0
				  for i fixnum from 1 upto m do
				    (setf ans (* ans (/ (+ i v) i)))
				  finally (return ans)))
			 (expt probability successes)))
    (loop
      (incf k)
      (setf b (* b 2))
      (let ((nt (floor (* b (aref pnbs m))))
	    (threshold (dfloat (/ b))))
	(declare (type fixnum nt xu xl)
		 (type double-float threshold))
	(setf xu m xl m)
	(loop
	  (when (>= xl 1)
	    (let ((new (aref pnbs (1- xl))))
	      (declare (type double-float new))
	      (when (zerop new)
		(setf new (/ (* u xl (aref pnbs xl)) (+ v xl)))
		(setf (aref pnbs (1- xl)) new))
	      (incf nt (floor (* b new))))
	    (decf xl))
	  (incf xu)
	  (when (>= xu (fill-pointer pnbs))
	    (let ((new (/ (* s (+ v xu) (aref pnbs (1- xu))) xu)))
	      (declare (type double-float new))
	      (vector-push-extend new pnbs)))
	  (incf nt (floor (* b (aref pnbs xu))))
	  (when (and (or (= xl 0) (< (aref pnbs xl) threshold)) (< (aref pnbs xu) threshold))
	    (return)))
	(when (or (= k 16)
		  (> (/ nt b) 0.9d0))
	  (return))))
    (let* ((pl (if (zerop xl)
		   0d0
		 (/ (* u xl (aref pnbs xl)) (+ v xl))))
	   (pu (/ (* s (+ v xu) (aref pnbs xu)) xu))
	   (nsq (+ (- xu xl) 1))
	   (d (1+ xu))
	   (w (dfloat (expt 2 (- k +bit-operation-m+))))
	   (table (make-array b :initial-element -1 :element-type 'fixnum))
	   (thetan 0d0)
	   (sum 0d0))
      (declare (type double-float w thetan sum pl pu)
	       (type fixnum nsq d)
	       (type (vector fixnum *) table))
      (unless (= xl 0)
	(setf pnbs (subseq pnbs xl)))
      (loop with j = 0
	  for x from xl
	  for pnb across pnbs
	  for tx = (floor (* b pnb)) do
	    (loop for index from j below (+ j tx) do
		  (setf (aref table index) x))
	    (incf sum pnb)
	    (incf thetan (setf (aref pnbs x) (- (* b pnb) tx)))
	    (incf j tx))
      ;; pnbs -> thetas
      (map-into pnbs #'(lambda (x) (/ x thetan)) pnbs)
      ;; robin hood
      (let ((ki (make-array nsq :element-type 'fixnum :initial-contents (loop for i from 0 below nsq collect i)))
	    (vi (make-array nsq :element-type 'double-float
			    :initial-contents (loop for i from 0 below nsq collect (dfloat (/ (+ i 1) nsq)))))
	    (c (dfloat (/ nsq))))
	(declare (type (vector fixnum *) ki)
		 (type (vector double-float *) vi)
		 (type double-float c))
	(loop repeat (1- nsq) do
	      (let ((maxp 0)
		    (max (aref pnbs 0))
		    (minp 0)
		    (min (aref pnbs 0)))
		(loop for i from 1 below nsq
		    for p = (aref pnbs i) do
		      (cond ((> p max)
			     (setf max p maxp i))
			    ((< p min)
			     (setf min p minp i))))
		(setf (aref ki minp) maxp)
		(setf (aref vi minp) (+ (* minp c) min))
		(decf (aref pnbs maxp) (- c (aref pnbs minp)))
		(setf (aref pnbs minp) c)))
	(map-into ki #'(lambda (x) (+ x xl)) ki)
	(let* ((ptx0 (- 1d0 sum))
	       (psq (/ thetan (+ (* b ptx0) thetan)))
	       (q (/ psq))
	       (r (/ ptx0 (- 1d0 psq))))
	  (declare (type double-float ptx0 psq q r))
	  (values table ki vi (- b 1) (- k) w nsq psq q r xl d pl pu s u v))))))

(defun negative-binomial-table-histogram-lookup (table ki vi b k w nsq psq q r xl xu pl pu que s tee)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float w psq q r pl pu que s tee)
	   (type (vector fixnum) table ki)
	   (type (vector double-float) vi)
	   (type fixnum b k nsq xu xl))
  (let* ((i (random most-positive-fixnum))
	 (ilk (logand i b))
	 (tilk (aref table ilk)))
    (declare (type fixnum i ilk tilk))
    (if (= tilk -1)
	;; step4
	(let ((u (* w (the fixnum (ash i k)))))
	  (declare (type double-float u))
	  (if (< u psq)
	      (let* ((udash (* q u))
		     (j (floor (* nsq udash))))
		(declare (type fixnum j)
			 (type double-float udash))
		(if (< udash (aref vi j))
		    (+ j xl)
		  (aref ki j)))
	    ;;; inverse-method
	    (let ((udash (* r (- u psq))))
	      (declare (type double-float udash))
	      (loop
		(let ((v (- udash pu)))
		  (declare (type double-float v))
		  ;; step 4
		  (when (<= v 0d0)
		    (return xu))
		  (setf udash v)
		  ;; step 5
		  (unless (= xl 0)
		    (setf pl (/ (* pl s xl) (+ tee xl)))
		    (decf xl)
		    (setf v (- udash pl))
		    (when (<= v 0d0)
		      (return xl))
		    (setf udash v))
		  ;; step8
		  (incf xu)
		  (setf pu (/ (* pu que (+ tee xu)) xu)))))))
      tilk)))

(defun negative-binomial-random (successes probability)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type double-float successes probability))
  (if (= successes 1d0)
      (geometric-random probability)
    (negative-binomial-inverse-mode successes probability)))

(define-compiler-macro negative-binomial-random (&whole form successes probability)
  (cond ((and (numberp successes) (numberp probability))
	 (assert (and (typep successes 'double-float) (typep probability 'double-float)))
	 `(negative-binomial-table-histogram-lookup ,@(multiple-value-list
						       (negative-binomial-table-histogram successes probability))))
	(t form)))
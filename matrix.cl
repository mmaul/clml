(defpackage :hjs.util.matrix
  (:use :cl :hjs.util.meta :hjs.util.vector :blas :lapack)
  (:nicknames :matrix)
  (:export #:sum-mat
	   #:copy-mat
	   #:nrow
	   #:ncol
	   #:transpose
	   #:transposeV
           #:trans
	   #:dmat
	   #:specialize-mat
           #:diag
           #:vcv
           #:mcm
           #:vdotv
           #:m*v
           #:m*m
           #+mkl #:symat-ev
           #:m^-1
           #:tr
           #:det
           #+mkl #:solve-linear-eq
           #:make-dmat
           #:append-mat
           #:standard-deviations-from-covariance
           #:standard-deviations
           #:covariance-matrix
           #:correlation-matrix
           #:standardize
           #:regularize-covariance
           #:vecs2mat
           #:vecs2flatmat
           #:mat2vecs
           #:flatmat2vecs
           #:row-aref
           #:c*mat
	   ))

(in-package :hjs.util.matrix)

;; (declaim (optimize (speed 0) (safety 3) (debug 3)))
(declaim (optimize (speed 3) (safety 1) (debug 1)))

(deftype dmat () `(simple-array double-float (* *))) 

(defmacro make-dmat (a b)
  `(make-array (list ,a ,b) :element-type 'double-float))


(defmacro nrow (matrix)
  `(array-dimension ,matrix 0))

(defmacro ncol (matrix)
  `(array-dimension ,matrix 1))

(defun get-underlying-1d-array (array)
  #+allegro (excl:with-underlying-simple-vector (array v) v)
  #+sbcl (sb-kernel:with-array-data ((v array) (sv) (ev))
           (declare (ignorable sv ev))
           v)
  )

;;; do-matrix
;;; TODO: I have a better idea, currently not used, so temporarily disable it.
(defmacro do-mat ((var matrix &key
		       (type 'double-float)
		       (start ''(0 0))
		       steps
		       (delta ''(0 1))
		       (delta-when-overflow ''(0 0))
		       setf-var
		       return)
		  &body body)
  (check-type var symbol)
  (check-type setf-var symbol)
  (once-only (matrix start steps delta delta-when-overflow)
    (with-unique-names (rows cols total-size start-pos next-offset next-offset-when-overflow step i)
      `(let* ((,rows (array-dimension ,matrix 0))
	      (,cols (array-dimension ,matrix 1))
	      (,total-size (* ,rows ,cols)) 
	      (,start-pos (apply #'* ,start))
	      (,next-offset (+ (* (first ,delta) ,cols) (second ,delta)))
	      (,next-offset-when-overflow (+ (* (first ,delta-when-overflow) ,cols) (second ,delta-when-overflow))))
	 (declare (type fixnum ,rows ,cols ,total-size ,start-pos ,next-offset ,next-offset-when-overflow)
		  (type (simple-array ,type (* *)) ,matrix))
	 (assert (and (< -1 ,start-pos ,total-size)
		      (< (- ,total-size) ,next-offset ,total-size)
		      (< (- ,total-size) ,next-offset-when-overflow ,total-size)))
	 (loop for ,step of-type array-index below (or ,steps ,total-size)
	       for ,i of-type array-index = ,start-pos then (+ ,i ,next-offset)
	       with ,var of-type ,type = (row-major-aref ,matrix ,i)
	       do (progn
		    (when (>= ,i ,total-size)
		      (setf ,i (+ ,next-offset-when-overflow (- ,i ,total-size))))
		    (when (< ,i 0)
		      (setf ,i (+ ,next-offset-when-overflow (+ ,i ,total-size))))
		    (setf ,var (row-major-aref ,matrix ,i))
		    (locally ,@body))
	       finally (return ,return))))))


;;@ note: from ABE, modified by huangjs
;;@ function-type: dmat -> dmat
(defun copy-mat (a &optional (target nil target-p))
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type dmat a))
  (when target-p
    (assert (typep target 'dmat))
    (assert (and (= (array-dimension target 0)
                    (array-dimension a 0))
                 (= (array-dimension target 1)
                    (array-dimension a 1)))))
  (let ((new (or target (make-array (array-dimensions a)
                                 :element-type 'double-float))))
    (declare (type dmat new)) 
    (destructuring-bind (imax jmax)
        (array-dimensions a)
      (declare (type fixnum imax jmax))
      (loop for i fixnum from 0 below imax
	    do (loop for j fixnum from 0 below jmax
		     do (setf (aref new i j) (aref a i j)))))
    new))


;;@ function-type: dmat -> (or dvec double-float)
;;@ precondition: initial (if supplied) must be large enough to contain the result
;;@ note: initial is ignored if :by is :cell
(defun sum-mat (matrix &key (by :row) initial)
  (declare (type dmat matrix)
	   (optimize (safety 0)))
  (check-type initial (or null dvec))
  (let* ((nrow (nrow matrix))
	 (ncol (ncol matrix)))
    (declare (fixnum nrow ncol))
    (ecase by
      (:row
       (let ((result (or initial (make-dvec ncol 0.0))))
	 (declare (type dvec result))
	 (assert (>= (length result) ncol))
	 (loop
	    for i of-type array-index below nrow
	    do (loop
		  for j of-type array-index below ncol
		  do (incf (aref result j)
			   (aref matrix i j))))
	 result))
      (:column
       (let ((result (or initial (make-dvec nrow 0.0))))
	 (declare (type dvec result))
	 (assert (>= (length result) nrow))
	 (loop
	    for j of-type array-index below ncol
	    do (loop
		  for i of-type array-index below nrow
		  do (incf (aref result i)
			   (aref matrix i j))))
	 result))
      (:cell
       (let ((result 0.0))		; initial is ignored
	 (declare (double-float result))
	 (loop
	    for i of-type array-index below nrow
	    do (loop
		  for j of-type array-index below ncol
		  do (incf result
			   (aref matrix i j))))
	 result)))))


(defun transpose-self (mat)
  (declare (type dmat mat))
  (assert (= (array-dimension mat 0)
             (array-dimension mat 1)))
  (let ((dim (array-dimension mat 0)))
    (declare (fixnum dim))
    (loop for i of-type array-index below dim
          do
       (loop for j of-type array-index from i below dim
             do
          (let ((a (aref mat i j))
                (b (aref mat j i)))
            (setf (aref mat i j) b)
            (setf (aref mat j i) a))))
    mat))

;;@ function-type: dmat -> dmat
(defun transpose (matrix &optional result)
  (declare (type dmat matrix))
  (check-type matrix dmat)
  (check-type result (or null dmat))
  (cond ((eq matrix result)
         (transpose-self matrix))
        (t
         (let* ((nrow (array-dimension matrix 0))
                (ncol (array-dimension matrix 1))
                (result (or result (make-array (list ncol nrow) :element-type 'double-float))))
           (declare (fixnum nrow ncol)
                    (type dmat result))
           (assert (equal (reverse (array-dimensions result))
                          (array-dimensions matrix)))
           (loop
               for i of-type array-index below nrow
               do (loop for j of-type array-index below ncol
                      do (setf (aref result j i)
                           (aref matrix i j))))
           result))))

;;@ function-type: #(dvec) -> #(dvec)
(defun transposeV (Vmatrix)
  (declare (type (simple-array dvec (*)) Vmatrix))
  (check-type Vmatrix (simple-array dvec (*)))
  (let* ((nrow (length Vmatrix))
	 (ncol (length (aref Vmatrix 0))))
    (declare (fixnum nrow ncol))
    (coerce
     (loop
	for i of-type array-index below ncol
	for vec of-type dvec = (make-dvec nrow)
	do (do-vec (v Vmatrix :type dvec :index-var iv)
	     (setf (aref vec iv)
		   (aref v i)))
	collect vec)
     'vector)))

;;@ function-type: vector -> vector 
(defun trans (Vmatrix &key (element-type t))
  (let* ((nrow (length Vmatrix))
         (ncol (length (aref Vmatrix 0))))
    (declare (fixnum nrow ncol))
    (coerce
     (loop
         for i of-type array-index below ncol
         for vec of-type vector = (make-array nrow :element-type element-type)
         do (do-vec (v Vmatrix :type vector :index-var iv)
              (setf (svref vec iv) (svref v i)))
         collect vec)
     'vector)))


;;@ function-type: (array t (* *)) -> dmat
(defun specialize-mat (array &key check)
  (declare (type (array t (* *)) array))
  (when check
    (assert 
     (eq 'ok
	 (block check
	   (loop for i of-type array-index below (array-dimension array 0)
	      do (loop for j of-type array-index below (array-dimension array 1)
		    do (when (not (typep (aref array i j) 'double-float))
			 (return-from check nil)))
	      finally (return 'ok))))))
  (let* ((nrow (array-dimension array 0))
	 (ncol (array-dimension array 1))
	 (result (make-array (list nrow ncol) :element-type 'double-float)))
    (declare (type dmat result))
    (loop for i of-type array-index below nrow
       do (loop for j of-type array-index below ncol
	     do (setf (aref result i j)
		      (coerce (aref array i j) 'double-float))))
    result))

;; function: make diagonal-matrix (dmat)
(defun diag (dim &optional (val 1.0d0))
  (declare (type double-float val))
  (check-type val double-float)
  (make-array `(,dim ,dim)
              :initial-contents 
              (loop for i of-type fixnum below dim
                  collect (let ((l (make-list dim :initial-element 0.0d0)))
                            (setf (nth i l) val) l))
              :element-type 'double-float))
              

;;; matrix calculation with blas and lapack

;;; matrix <-> vecs  utilities
(defun vecs2mat (vecs &optional mat)
  (declare (type simple-vector vecs))
  (assert (typep vecs 'simple-vector))
  (assert (> (length vecs) 0))
  (assert (eq 'double-float (array-element-type (aref vecs 0))))
  (assert (or (null mat) (typep mat 'dmat)))
  (let* ((nrow (length vecs))
         (ncol (length (aref vecs 0))) 
         (mat (or mat (make-array (list nrow ncol) :element-type 'double-float))))
    (declare (optimize speed (safety 0))
             (type array-index nrow ncol)
             (type dmat mat))
    (do-vec (v vecs :type t :index-var i)
      (do-vec (e v :type double-float :index-var j)
        (setf (aref mat i j) e)))
    mat))

(defun vecs2flatmat (vecs &optional flatmat)
  (declare (type simple-vector vecs))
  (assert (typep vecs 'simple-vector))
  (assert (> (length vecs) 0))
  (assert (eq 'double-float (array-element-type (aref vecs 0))))
  (assert (or (null flatmat) (typep flatmat 'dvec)))
  (let* ((nrow (length vecs))
         (ncol (length (aref vecs 0))) 
         (flatmat (or flatmat (make-array (* nrow ncol) :element-type 'double-float))))
    (declare (optimize speed (safety 0))
             (type array-index nrow ncol)
             (type dvec flatmat))
    (let ((i 0))
      (declare (type array-index i))
      (do-vec (v vecs :type dvec)
        (do-vec (e v :type double-float)
          (setf (aref flatmat i) e)
          (incf i))))
    flatmat))

(defun mat2vecs (mat &optional vecs)
  (declare (type dmat mat)) 
  (assert (typep mat 'dmat))
  (assert (eq 'double-float (array-element-type mat)))
  (assert (or (null vecs) (typep vecs 'simple-vector)))
  (let* ((nrow (array-dimension mat 0))
         (ncol (array-dimension mat 1))
         (vecs (or vecs
                   (let ((result (make-array nrow)))
                     (do-vec (_ result :setf-var v :return result)
                       (declare (ignorable _))
                       (setf v (make-dvec ncol)))))))
    (declare (type array-index nrow ncol)
             (type simple-vector vecs))
    (loop for i of-type array-index below nrow
          for v of-type dvec = (aref vecs i)
          do
       (loop for j of-type array-index below ncol
             do
          (setf (aref v j) (aref mat i j))))
    vecs))

(defun flatmat2vecs (flatmat nrow &optional vecs)
  (declare (type dvec flatmat)) 
  (assert (typep flatmat 'dvec))
  (assert (eq 'double-float (array-element-type flatmat)))
  (assert (or (null vecs) (typep vecs 'simple-vector)))
  (let* ((ncol (/ (length flatmat) nrow))
         (vecs (or vecs
                   (let ((result (make-array nrow)))
                     (do-vec (_ result :setf-var v :return result)
                       (declare (ignorable _))
                       (setf v (make-dvec ncol)))))))
    (declare (type array-index nrow ncol)
             (type simple-vector vecs))
    (let ((i 0))
      (declare (type array-index i))
      (do-vec (v vecs :type dvec)
        (do-vec (_ v :type double-float :setf-var e)
          (declare (ignorable _))
          (setf e (aref flatmat i))
          (incf i)))) 
    vecs))

(defun row-aref (mat nrow &optional row-vec)
  (declare (type array-index nrow)
           (type dmat mat))
  (let ((row-vec (or row-vec (make-dvec (array-dimension mat 1)))))
    (do-vec (_ row-vec :type double-float :setf-var sv :index-var iv)
      (declare (ignorable _))
      (setf sv (aref mat nrow iv)))
    row-vec))

;;; matrix -> 1d array 
;;; for blas and lapack calculation
(defun mat2array (mat &optional array)
  (declare (type dmat mat))
  (let* ((nrow (array-dimension mat 0))
         (ncol (array-dimension mat 1))
         (flatmat (get-underlying-1d-array mat))
         (array (or array (make-dvec (* nrow ncol)))))
    (declare (type array-index nrow ncol)
             (type dvec array flatmat))
    (loop for i of-type array-index below (* nrow ncol)
          do
       (setf (aref array i) (aref flatmat i)))
    array))

;;; array -> matrix
;;; for the result of blas and lapack calculation
(defun array2mat (arr col &optional mat)
  (declare (type dvec arr))
  (declare (type array-index col))
  (if (= col 0)
      (make-array '(0 0) :element-type 'double-float)
      (let* ((row (/ (length arr) col))
             (mat (or mat (make-dmat row col)))
             (flatmat (get-underlying-1d-array mat)))
        (declare (type array-index row)
                 (type dmat mat)
                 (type dvec flatmat))
        (assert (integerp row))
        (loop for i of-type array-index below (* row col)
              do
              (setf (aref flatmat i) (aref arr i)))
        mat)))

;;; each elements of vector v and w are composed by c
;;; return dvec
(defun vcv (v w &key (c #'+))
  (declare (type dvec v w))
  (assert (equal (array-dimensions v) (array-dimensions w)))
  (make-array (array-dimensions v)
              :initial-contents
              (loop for i below (array-dimension v 0)
                  collect (funcall c (aref v i) (aref w i)))
              :element-type 'double-float))

;;; each elements of matrix A and B are composed by c
;;; return dmat
(defun mcm (A B &key (c #'+))
  (declare (type dmat A B))
  (assert (equal (array-dimensions A) (array-dimensions B)))
  (make-array (array-dimensions A)
              :initial-contents 
              (loop for row below (array-dimension A 0)
                  collect (loop for col below (array-dimension B 1)
                              collect (funcall c
                                       (aref A row col)
                                       (aref B row col))))
              :element-type 'double-float))

(defun vdotv (v1 v2)
  (declare (type dvec v1 v2))
  (ddot (array-dimension v1 0) v1 1 v2 1))

#-mkl
(defun m*v (m v &optional result)
  (declare (type dmat m) (type dvec v))
  (let ((rv (or result (make-array (array-dimension m 0) :element-type 'double-float
                                   :initial-element 0d0)))
        (s 0d0))
    (declare (type dvec rv) (type double-float s))
    (assert (eql (length rv) (array-dimension m 0)))
    (do-vec (_ rv :type double-float :setf-var sf :index-var row :return rv)
      (declare (ignore _))
      (setf s 0d0 
            sf (do-vec (val v :type double-float :index-var col :return s)
                 (incf s (* (the double-float (aref m row col)) 
                            (the double-float val))))))))
#+mkl
(defun m*v (mat vec &optional result (major :row))
  (declare (type dmat mat))
  (declare (type dvec vec))
  (let* ((trans (ecase major (:row t) (:col nil)))
         (m (array-dimension mat 1))
         (n (array-dimension mat 0))
         (y (if result result 
              (make-array (if trans n m) :initial-element 0.0d0
                          :element-type 'double-float))))
    (assert (if trans (= (length vec) m) (= (length vec) n)))
    (mkl.blas:dgemv (if trans "T" "N") m n 1d0 mat m vec 1 0d0 y 1)
    y))

#-mkl
(defun m*m (A B &optional result)
  (declare (type dmat A B))
  (assert (= (array-dimension A 1) (array-dimension B 0)))
  (let* ((n (array-dimension A 1))
         (row (array-dimension A 0))
         (col (array-dimension B 1))
         (rm (or result
                 (make-array `(,row ,col) :element-type 'double-float)))
         (transpose-self-p (and (not (eq A B)) (= n col))))
    (declare (type dmat rm) (type fixnum n row col))
    (assert (and (eql row (array-dimension rm 0))
                 (eql col (array-dimension rm 1))))
    (unwind-protect
        (progn
          (setf B (cond (transpose-self-p
                         (transpose-self B))
                        (t
                         (transpose B)))) 
          (loop for r of-type fixnum below row do
                (loop for c of-type fixnum below col do
                      (setf (aref rm r c)
                        (loop for i of-type fixnum below n
                            sum (* (aref A r i) (aref B c i))
                            into result of-type double-float
                            finally (return result))))))
      (when transpose-self-p
        (transpose-self B)))
    rm))

#+mkl
;;; mkl
;;; memo: (AB)^t = B^t A^t
;;;       mkl(fortran) : column-major
;;;       CLML         : basically row-major
(defun m*m (A B &optional result (major :row)) ;; :row | :col
  (declare (type dmat A B))
  (let* ((switch-p (ecase major (:row t) (:col nil)))
         (%A (if switch-p B A))
         (%B (if switch-p A B))
         (m (array-dimension %A 1))
         (n (array-dimension %B 0))
         (%k1 (array-dimension %A 0))
         (%k2 (array-dimension %B 1))
         (k (if (eql %k1 %k2) %k1 (error "illegal matrix size")))
         (alpha 1d0)
         (beta 0d0)
         (ldc (max 1 m))
         (c-col (max 1 n))
         (lda (max 1 m))
         (ldb (max 1 k)))
    (unless result (setf result (make-array `(,c-col ,ldc) :element-type 'double-float)))
    (mkl.blas:dgemm "N" "N" m n k alpha %A lda %B ldb beta result ldc)
    result))

#-mkl
(defun m^-1 (A)
  (declare (type dmat A))
  (let* ((Ar (mat2array A))
         (m (array-dimension A 1))
         (n (array-dimension A 0))
         (lda (max 1 m))
         (ipiv (make-array (min m n) :element-type 'fixnum))
         (lwork (* m n 2))
         (work (make-array lwork :element-type 'double-float))
         (info 0))
    (assert (= m n))
    (setq info 
      (car (last
            (multiple-value-list
             (lapack::dgetrf m n Ar lda ipiv info)))))
    (assert (= 0 info))
    (setq info
      (car (last (multiple-value-list
                  (lapack::dgetri n Ar lda ipiv work lwork info)))))
    (assert (= 0 info))
    (array2mat Ar n)))
#+mkl
(defun m^-1 (A)
  (declare (type dmat A))
  (let* ((Ar (copy-mat A))
         (m (array-dimension Ar 1))
         (n (array-dimension Ar 0))
         (lda (max 1 m))
         (ipiv (make-array (min m n) :element-type '(unsigned-byte 32)))
         (lwork (* m n 2))
         (work (make-array lwork :element-type 'double-float))
         (info 0))
    (assert (= m n))
    (setq info 
      (car (last
            (multiple-value-list
             (mkl.lapack::dgetrf m n Ar lda ipiv info)))))
    (assert (= 0 info))
    (setq info
      (car (last (multiple-value-list
                  (mkl.lapack::dgetri n Ar lda ipiv work lwork info)))))
    (assert (= 0 info))
    Ar))

;; trace of matrix
(defun tr (mat)
  (declare (type dmat mat))
  (assert (= (array-dimension mat 0) (array-dimension mat 1)))
  (loop for i of-type array-index below (array-dimension mat 0)
      sum (the double-float (aref mat i i))))

#-mkl
(defun det (mat)
  (declare (type dmat mat))
  (let* ((A (mat2array mat))
         (m (array-dimension mat 1))
         (n (array-dimension mat 0))
         (lda (max 1 m))
         (ipiv (make-array (min m n) :element-type 'fixnum))
         (info 0)
         (det 1.0d0))
    (assert (= m n))

    ;; LU factorization
    (setq info (car (last (multiple-value-list
                           (lapack::dgetrf m n A lda ipiv info)))))
    (cond ((= info 0)
           ;; multiplication of diagonal elements of matrix U
           (progn (loop for i across ipiv 
                      for j from 1
                      unless (= i j)
                      do (setq det (* det -1)))
                  (* det (apply
                          #'* 
                          (loop for i below n
                              collect (aref A (+ i (* n i))))))))
          ((> info 0) 0.0d0)
          (t (error "fail to calculate determinant | ~A" info)))))
#+mkl
(defun det (mat)
  (declare (type dmat mat))
  (let* ((A (copy-mat mat))
         (m (array-dimension mat 1))
         (n (array-dimension mat 0))
         (lda (max 1 m))
         (ipiv (make-array (min m n) :element-type '(unsigned-byte 32)))
         (info 0)
         (det 1.0d0))
    (assert (= m n))

    ;; LU factorization
    (setq info (car (last (multiple-value-list
                           (mkl.lapack::dgetrf m n A lda ipiv info)))))
    (cond ((= info 0)
           ;; multiplication of diagonal elements of matrix U
           (progn (loop for i across ipiv 
                      for j from 1
                      unless (= i j)
                      do (setq det (* det -1)))
                  (* det (apply #'* (loop for i below n collect (aref A i i))))))
          ((> info 0) 0.0d0)
          (t (error "fail to calculate determinant | ~A" info)))))

#+mkl
(defun solve-linear-eq (A B)
  "A * X = B"
  (declare (type dmat A) (type dvec B))
  (check-type A dmat)
  (check-type B dvec)
  (let* ((info 0)
         (n (array-dimension A 0))
         (nrhs 1)
         (Ar (copy-mat A))
         (lda (max 1 n))
         (ipiv (make-array lda :element-type '(unsigned-byte 32)))
         (Br (copy-seq B))
         (ldb (max 1 n)))
    (setq info (car (last
                     (multiple-value-list
                      (mkl.lapack:dgesv n nrhs Ar lda ipiv Br ldb info)))))
    (assert (= info 0))
    Br))

;; (append-mat A B :direction :diagonal)
;; -> (A 0)
;;    (0 B)
(defun append-mat (A B &key (direction :diagonal)) ; :horizontal | :vertical | :diagonal
  (declare (type dmat A B))
  (check-type A dmat) 
  (check-type B dmat)
  (let ((a-row (array-dimension A 0))
        (b-row (array-dimension B 0))
        (a-col (array-dimension A 1))
        (b-col (array-dimension B 1)))
    (declare (type fixnum a-row b-row a-col b-col))
    (case direction
      (:diagonal
       (let ((row (+ a-row b-row))
             (col (+ a-col b-col)))
         (declare (type fixnum row col))
         (make-array `(,row ,col) :element-type 'double-float
                     :initial-contents
                     (loop for i-org of-type fixnum below row
                         as i of-type fixnum = (- i-org a-row)
                         collect (loop for j-org of-type fixnum below col
                                     as j of-type fixnum = (- j-org a-col)
                                     collect (cond ((and (minusp i) (minusp j))
                                                    (the double-float (aref A i-org j-org)))
                                                   ((and (>= i 0) (>= j 0))
                                                    (the double-float (aref B i j)))
                                                   (t 0d0)))))))
      (:horizontal
       (assert (= a-row b-row))
       (let ((col (+ a-col b-col)))
         (declare (type fixnum col))
         (make-array `(,a-row ,col) :element-type 'double-float
                     :initial-contents
                     (loop for i of-type fixnum below a-row
                         collect (loop for j of-type fixnum below col
                                     as j-a of-type fixnum = (- j a-col)
                                     collect (if (minusp j-a)
                                                 (the double-float (aref A i j))
                                               (the double-float (aref B i j-a))))))))
      (:vertical
       (assert (= a-col b-col))
       (let ((row (+ a-row b-row)))
         (declare (type fixnum row))
         (make-array `(,row ,a-col) :element-type 'double-float
                     :initial-contents
                     (loop for i of-type fixnum below row
                         as i-a of-type fixnum = (- i a-row)
                         collect (loop for j of-type fixnum below b-col
                                     collect (if (minusp i-a)
                                                 (the double-float (aref A i j))
                                               (the double-float (aref B i-a j)))))))))))

#||
(defun householder-trans (x)
  (declare (type dmat x))
  (let* ((n (array-dimension x 0))
         (k (array-dimension x 1))
         (d (make-array n :initial-element 0.0d0
                        :element-type 'double-float))
         (tol 1.0d-60) f g h)
    (tagbody 
      (do ((ii 1 (1+ ii)))
          ((= ii (1+ k)))
        (setq h 0.0d0)
        (do ((i 1 (1+ i)))
            ((= i (1+ n)))
         (setf (aref d (1- i)) (aref x (1- i) (1- ii))
               h (+ h (expt (aref d (1- i)) 2))))
        (when (> h tol) (go tag20))
        (setq g 0.0d0)
        (go tag100)
       tag20
        (setq g (sqrt h)
              f (aref x (1- ii) (1- ii)))
        (when (>= f 0.0d0) (setq g (- g)))
        (setf (aref d (1- ii)) (- f g)
              h (- h (* f g)))
        (do ((i (1+ ii) (1+ i)))
            ((= i (1+ n)))
          (setf (aref x (1- i) (1- ii)) 0.0d0))
        (do ((j (1+ ii) (1+ j))
             (s 0.0d0))
            ((= j (1+ k)))
          (setq s 0.0d0)
          (do ((i ii (1+ i)))
              ((= i (1+ n)))
            (setq s (+ s (* (aref d (1- i)) (aref x (1- i) (1- j))))))
          (setq s (/ s h))
          (do ((i ii (1+ i)))
              ((= i (1+ n)))
            (setf (aref x (1- i) (1- j)) 
              (- (aref x (1- i) (1- j)) (* (aref d (1- i)) s)))))
       tag100
        (setf (aref x (1- ii) (1- ii)) g)))
    x))
||#

;;@ function-type: (simple-array dvec) -> dmat
(defun covariance-matrix (trials &optional result)
  (declare (type (simple-array dvec (*)) trials))
  (assert (or (null result)
              (and (= (array-rank result) 2)
                   (= (array-dimension result 0)
                      (array-dimension result 1)
                      (length (aref trials 0))))))
  (let* ((mean (mean-points trials))
         (dim (length (aref trials 0)))
         (1/n-1 (/ 1.0 (1- (length trials))))
         (c-points (map 'vector (lambda (p) (v- p mean (make-dvec dim))) trials))
         (tc-points (transposeV c-points))
         (result (or result (make-array (list dim dim) :element-type 'double-float))))
    (declare (type (simple-array dvec (*)) tc-points)
             (type dmat result))
    (do-vec (p1 tc-points :type dvec :index-var ix)
      (do-vec (p2 tc-points :type dvec :index-var iy)
        (setf (aref result ix iy)
          (* (inner-product p1 p2) 1/n-1))))
    (values result mean)))

;;@ function-type: (simple-array dvec) -> dmat
(defun correlation-matrix (trials &optional result)
  (declare (type (simple-array dvec (*)) trials))
  (assert (or (null result)
	      (= (length trials) 0)
	      (and (= (array-rank result) 2)
		   (= (array-dimension result 0)
		      (array-dimension result 1)
		      (length (aref trials 0))))))
  (let* ((dim (length (aref trials 0)))
	 (covariance (covariance-matrix trials))
	 (sds (standard-deviations-from-covariance covariance))
	 (result (or result (make-array (list dim dim) :element-type 'double-float))))
    (declare (type dvec sds)
	     (type dmat result covariance))
    (loop for i of-type array-index below dim
       do (loop for j of-type array-index below dim
	     do (setf (aref result i j)
		      (/ (aref covariance i j)
			 (aref sds i)
			 (aref sds j)))))
    result))

;;@ function-type: dmat -> dvec
(defun standard-deviations-from-covariance (covariance &optional result)
  (declare (type dmat covariance))
  (assert (and (= (array-rank covariance) 2)
	       (= (array-dimension covariance 0)
		  (array-dimension covariance 1))))
  (let* ((dim (array-dimension covariance 0))
	 (result (or result (make-dvec dim))))
    (declare(type dvec result))
    (do-vec (_ result :type double-float :setf-var sr :index-var ir)
      (declare (ignorable _))
      (setf sr (sqrt (the (double-float 0.0) (aref covariance ir ir)))))
    result))

;;@ function-type: (simple-array dvec) -> dvec
(defun standard-deviations (trials &optional result)
  (assert (or (null result)
	      (= (length result)
		 (length (aref trials 0)))))
  (let ((covariance (covariance-matrix trials)))
    (standard-deviations-from-covariance covariance result)))

;;@ function-type: (simple-array dvec) -> (simple-array dvec)
(defun standardize (trials)
  (declare (type (simple-array dvec (*)) trials))
  (let ((mean (mean-points trials))
	(sds (standard-deviations trials))
	(points (map 'vector #'copy-seq trials)))
    (declare (type dvec mean sds)
	     (type (simple-array dvec (*)) points))
    (do-vec (p points :type dvec)
      (do-vec (x p :type double-float :setf-var sx :index-var ix)
	(setf sx (/ (- x (aref mean ix)) (aref sds ix)))))
    (values points
	    mean
	    sds)))

;; Regularization for covariance matrix
;; ref: M.Sato et.al. "On-line EM Algorithm for the Normalized Gaussian Network"
;; destructive on input cov-mat
(defun regularize-covariance (cov-mat &key (alpha 1d-2)
                                           (delta-min 1d-8)
                                           (det-thld 1d-8))
  (assert (< 0 alpha 1))
  (assert (eql (array-dimension cov-mat 0) (array-dimension cov-mat 1)))
  (let ((regularize-p nil))
    (when (> det-thld (det cov-mat))
      (let* ((n (array-dimension cov-mat 0))
             (tr/n (/ (tr cov-mat) n))
             (adj (* alpha (max tr/n delta-min))))
        (setf regularize-p t)
        (loop for i below n do (setf (aref cov-mat i i) (+ adj (aref cov-mat i i))))))
    (values cov-mat regularize-p)))

;; Calculate inverse of Matrix by Gaussian elimination method
;; destructive on mat
(defun gaussian-elimination (mat)
  (declare (type dmat mat))
  (declare (optimize speed))
  (check-type mat dmat)
  (assert (eql (array-dimension mat 0) (array-dimension mat 1)))
  (let* ((d (array-dimension mat 0))
         (mat^-1 (diag d 1d0)))
    (declare (type dmat mat^-1))
    (labels ((%sweep-out (pivot target)
               (loop with a = (aref mat target pivot)
                   for col below d do
                     (setf (aref mat target col) 
                       (-fl (aref mat target col) (*fl a (aref mat pivot col)))
                       (aref mat^-1 target col) 
                       (-fl (aref mat^-1 target col) (*fl a (aref mat^-1 pivot col))))))
             (sweep-out (pivot)
               (loop with a = (aref mat pivot pivot)
                   for col below d do
                     (setf (aref mat pivot col) (/fl (aref mat pivot col) a)
                           (aref mat^-1 pivot col) (/fl (aref mat^-1 pivot col) a)))
               (loop for target below d unless (eql pivot target) do
                     (%sweep-out pivot target))))
      (loop for pivot below d do (sweep-out pivot))
      (values mat^-1 mat))))

;; multiply number on every element in the matrix
(defun c*mat (c mat)
  (declare (type dmat mat) (type double-float c))
  (loop with dims = (array-dimensions mat)
      for i below (first dims)
      do (loop for j below (second dims)
             do (setf (aref mat i j) (* c (aref mat i j))))
      finally (return mat)))


;;;; M-Cut spectral clustering
;;;; The follwing code are besed on
;;;;   Sinnou Hiroyuki, R de manabu cluster kaiseki, Ohmsha
;;;; And the method is described in
;;;;   A Min-max Cut Algorithm for Graph Partitioning and Data Clustering
;;;;   Chris H. Q. Ding, Xiaofeng He, Hongyuan Zha, Ming Gu, Horst D. Simon
;;;;   First IEEE International Conference on Data Mining (ICDM'01), 2001.


(defpackage :spectral-clustering
    (:use :cl :hjs.util.matrix :hjs.util.meta)
  (:export #:spectral-clustering-mcut)
  (:import-from #+allegro "EXCL" #+sbcl "SB-INT" #+lispworks "LISPWORKS"
                "FIXNUMP"))


(in-package :spectral-clustering)


(defvar *debug-spectral* nil)

(defvar *sample-w*
    (make-array '(5 5) :element-type 'double-float
                :initial-contents
                '((10.0000d0  0.7071d0  0.3333d0  0.2774d0  0.3714d0)
                  ( 0.7071d0 10.0000d0  0.4472d0  0.2774d0  0.2857d0)
                  ( 0.3333d0  0.4472d0 10.0000d0  0.5000d0  0.3124d0)
                  ( 0.2774d0  0.2774d0  0.5000d0 10.0000d0  0.4851d0)
                  ( 0.3714d0  0.2857d0  0.3124d0  0.4851d0 10.0000d0))))


(defun spectral-clustering-mcut (w ncls &key (eigen-tolerance 100d0))
  ;; Patitions non-empty undirected graph W into NCLS clusters with
  ;; M-cut spectral clustering method where W is a symmetric (N,N) similarity
  ;; matrix of double-float values and NCLS is a positive integer.
  ;; The nodes of the graph are the indices of W.
  ;; The similarity of nodes i and j should be a non-negative double-float
  ;; value W[i,j].  Each similarity value of node i and itself must be
  ;; a positive value.
  ;; The keyword argument EIGEN-TOLERANCE is a positive doulbe-float value
  ;; or nil which controls accuracy of eigen computation checker
  ;; described below.
  ;;
  ;; This function returns two values as a multiple-values.
  ;; The first is the clustering result as a list of list of nodes.
  ;; The second is the status symbol of the result as follows.
  ;;
  ;;
  ;; Status           Meaning
  ;;
  ;; :success         The result is correct.
  ;; :questionable    The result may be questionable because a set of
  ;;                  eigen values and their vectors returned by
  ;;                  the eigen computation library function seems erroneous
  ;;                  with an error value, by a measure, greater than
  ;;                  specified EIGEN-TOLERANCE.
  ;;                  This check is skipped if EIGEN-TOLERANCE is nil.
  ;;
  ;; - the following cases are fatal and nil is returned as the first value -
  ;;
  ;; :input-error     Given arguments does not hold the above conditions.
  ;; :fatal-error     This situation arise in the following cases:
  ;;                   1. An eigen computation failed, or
  ;;                   2. returned eigen values could not halve a cluster.
  ;;
  (assert (and (typep w '(array * (* *)))
               (apply #'= (array-dimensions w))
               (typep ncls '(integer 1 *))))
  (let ((n (array-dimension w 0)))
    (cond
     ((not (<= ncls n))
      (values nil :input-error))
     ((not (block check
             (dotimes (i n)
               (loop for j from 0 to i
                   do (unless (and (typep (aref w i j) '(double-float 0d0 *))
                                   (if (= i j)
                                       (< 0d0 (aref w i j))
                                     (equal (aref w i j) (aref w j i))))
                        (return-from check nil))))
             t))
      (values nil :input-error))
     ;;
     (t
      ;; <Cls>  ::= (<objfv> (<Node>..))
      ;; <Clss> ::= (<Cls>..)
      (let ((clss (list (list 0 (loop for i from 0 to (- n 1) collect i))))
            (status :success))
        (dotimes (i (- ncls 1))
          (let* ((cls
                  ;; Just (pop clss) was no good because
                  ;; cls must have more than two nodes.
                  (dolist (cls1 clss)
                    (when (<= 2 (length (cadr cls1)))
                      (return (progn
                                (setq clss (delete cls1 clss))
                                cls1)))))
                 (nodes (cadr cls)))
            ;; (assert cls)
            (multiple-value-bind (cls1 cls2 status1)
                (spectral-aux w nodes eigen-tolerance)
              (cond
               ((or (null cls1)
                    (null cls2))
                (return-from spectral-clustering-mcut
                  (values nil :fatal-error)))
               ((eq status1 :fatal-error)
                (return-from spectral-clustering-mcut
                  (values nil :fatal-error)))
               ((eq status1 :questionable)
                (setq status status1)))
              (setq clss
                  (merge 'list
                         (sort (list (list (spectral-objfv w cls1) cls1)
                                     (list (spectral-objfv w cls2) cls2))
                               #'< :key #'car)
                         clss
                         #'< :key #'car)))))
        ;;
        (values (mapcar 'cadr clss)
                status))))))


(defun spectral-aux (w nodes eigen-tolerance)
  (let ((w1 (mat-minor w nodes nodes)))
    (flet ((widxs (cls) (mapcar #'(lambda (n) (elt nodes n)) cls)))
      (multiple-value-bind (cls1 cls2 status)
          (spectral-1 w1 eigen-tolerance)
        (when (eq status :fatal-error)
          (return-from spectral-aux (values nil nil :fatal-error)))
        (values (widxs cls1)
                (widxs cls2)
                status)))))

(defun spectral-1 (w eigen-tolerance)
  (let* ((n (array-dimension w 0))
         (d (spectral-d w))
         ;; (d-1/2 (spectral-d-expt d -1/2))
         (d-1/2 (spectral-d-expt--1/2 d))
         (a1 (mat* d-1/2 w d-1/2))
         (a (mat- (make-id-mat n) a1)))
    (multiple-value-bind (z2 status)
        (spectral-fielder a eigen-tolerance)
      (when (eq status :fatal-error)
        (return-from spectral-1 (values nil nil :fatal-error)))
      (let ((q (mat* d-1/2 z2))
            (cls1 nil)
            (cls2 nil))
        (dotimes (i n)
          (if (< (aref q i 0) 0)
              (push i cls1)
            (push i cls2)))
        (values (nreverse cls1)
                (nreverse cls2)
                status)))))

(defun spectral-d (w)
  ;; Make D from W.
  (let* ((n (array-dimension w 0))
         (e (make-mat n 1 :initial-element 1d0)))
    (make-diag-mat (mat-to-vec (mat* w e)))))

;; (defun spectral-d-expt (d k)
;;   ;; D ^ K.
;;   (let ((dk (copy-matrix d)))
;;     (dotimes (i (array-dimension d 0))
;;       (setf (aref dk i i) (expt (aref d i i) k)))
;;     dk))


(defun spectral-d-expt--1/2 (d)
  ;; D ^ (-1/2).
  (let ((dk (copy-matrix d)))
    (dotimes (i (array-dimension d 0))
      (assert (not (zerop (aref d i i))))
      (setf (aref dk i i)
        (let ((x (aref d i i)))
          ;; (expt x -1/2)
          (sqrt (/ x))                  ; sbcl
          ;; (/ (sqrt x))                  ; nearly acl
          )))
    dk))

(defun spectral-fielder (a eigen-tolerance)
  ;; Returns the Fielder vector of matrix A.
  (let ((status :success))
    (flet ((eigenerror (a evals evecs)
             (mat-norm (mat- (mat* a evecs)
                             (mat* evecs (make-diag-mat evals))))))
      (multiple-value-bind (evals evecs)
          (handler-case
              (eigen a)
            (error ()
              (return-from spectral-fielder (values nil :fatal-error))))
        (let* ((i -1)
               (evalv (sort (map 'vector #'(lambda (v) (cons v (incf i)))
                                 evals)
                            #'< :key #'car))
               (fi (cdr (elt evalv 1))))

          (when *debug-spectral*
            (format t "*** spectral-fielder-vector: ~s ~s ...~%"
                    (elt evalv 0) (elt evalv 1)))

          (when eigen-tolerance
            (let ((err (eigenerror a evals evecs)))
              (when *debug-spectral*
                (format t "*** eigenerror=~s~%" err))
              (when (< eigen-tolerance err)
                (setq status :questionable))))

          (values (mat-minor evecs t fi)
                  status))))))


(defun spectral-cut (w cls1 cls2)
  (let ((sum 0))
    (dolist (n1 cls1)
      (dolist (n2 cls2)
        (incf sum (aref w n1 n2))))
    sum))

;; (defun spectral-Ncut (w cls1 cls2)
;;   (* (spectral-cut w cls1 cls2)
;;      (+ (/ 1d0 (spectral-cut w cls1 cls1))
;;         (/ 1d0 (spectral-cut w cls2 cls2)))))

(defun spectral-objfv (w cls)
  (let ((n (length cls))
        (dzero (aref w 0 0)))
    (if (= n 1)
        dzero
      (/ (- (spectral-cut w cls cls)
            (* n dzero))
         (* n (- n 1))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Math library
;;; TODO: This part should be a common library of machine-learning package.

(defun make-mat (nrows ncols
                 &rest keys
                 &key (element-type 'double-float)
                      initial-element
                      initial-contents)
  (unless (or initial-element initial-contents)
    (setq keys (list* :initial-element 0d0 keys)))
  (apply #'make-array (list nrows ncols) :element-type element-type keys))


(defun mat-p (x)
  (and (arrayp x)
       (= 2 (array-rank x))))

(defun copy-matrix (a)
  (declare (optimize (speed 3) (debug 0) (safety 1))
           ;; A can be not a simple array now
           ;; (type dmat a)
           ;; (:explain :inlining)
           )
  (let ((new (make-array (array-dimensions a)
                         :element-type 'double-float)))
    (declare (type dmat new))
    (destructuring-bind (imax jmax)
        (array-dimensions a)
      (declare (type fixnum imax jmax))
      (loop for i fixnum from 0 below imax
          do (loop for j fixnum from 0 below jmax
                 do (setf (aref new i j) (aref a i j)))))
    new))

(defun seq-to-mat (seq &optional rowvecp)
  (let ((m (make-array (if rowvecp
                           (list 1 (length seq))
                         (list (length seq) 1)))))
    (dotimes (i (length seq))
      (setf (row-major-aref m i) (elt seq i)))
    m))

(defun mat-to-vec (a)
  (flet ((mtov (m)
           (let* ((n (array-total-size m))
                  (v (make-array n :element-type (array-element-type m))))
             (dotimes (i n)
               (setf (aref v i) (row-major-aref m i)))
             v)))
    (destructuring-bind (an am) (array-dimensions a)
      (if (or (= an 1) (= am 1))
          (mtov a)
        (error "~s cannot be a vector" a)))))

(defun make-id-mat (n)
  ;; Make N*N identity matrix.
  (let ((idm (make-mat n n)))
    (dotimes (i n)
      (setf (aref idm i i) 1d0))
    idm))

(defun make-diag-mat (seq)
  ;; Make diagonal matrix.
  (let* ((n (length seq))
         (m (make-mat n n)))
    (dotimes (i n)
      (setf (aref m i i) (elt seq i)))
    m))

(defun mat+ (a &rest mats)
  ;; Return sum of matrices or scalars.
  (let ((r (copy-matrix a)))
    (destructuring-bind (an am) (array-dimensions a)
      (dolist (b mats)
        (destructuring-bind (bn bm) (array-dimensions b)
          (assert (and (= an bn) (= am bm)))
          (loop for i from 0 to (- an 1) do
                (loop for j from 0 to (- am 1) do
                      (incf (aref r i j) (aref b i j)))))))
    r))

(defun mat- (a b)
  ;; Return difference between matrix A and B.
  (mat+ a (mat* -1 b)))

(defun mat* (a &rest mats)
  ;; Return product of matrices or scalars.
  (dolist (b mats)
    (setq a (if (numberp a)
                (if (numberp b)
                    (* a b)
                  (mat-smul a b))
              (if (numberp b)
                  (mat-smul b a)
                (mat-mul a b)))))
  a)

(defun mat-smul (a b)
  ;; Return scalar product of scalar A and matrix B.
  (destructuring-bind (bn bm) (array-dimensions b)
    (let ((c (make-array (list bn bm))))
      (loop for i from 0 to (- bn 1) do
            (loop for j from 0 to (- bm 1) do
                  (setf (aref c i j) (* a (aref b i j)))))
      c)))

(defun mat-mul (a b)
  ;; Return product of two matrix A and B.
  (declare (optimize (speed 3) (debug 0) (safety 1))
           (type dmat a b)
           ;; (:explain :inlining)
           )
  (destructuring-bind (an am) (array-dimensions a)
    (destructuring-bind (bn bm) (array-dimensions b)
      (declare (type fixnum an am bn bm))
      (assert (= am bn))
      (let ((c (make-array (list an bm) :element-type 'double-float)))
        (loop for i fixnum from 0 to (- an 1) do
              (loop for v of-type double-float = 0.0d0
                  for j fixnum from 0 to (- bm 1) do
                      (loop for k fixnum from 0 to (- am 1) do
                            (incf v (* (aref a i k) (aref b k j))))
                      (setf (aref c i j) v)))
        c))))

(defun mat-norm (a)
  (let ((norm 0))
    (destructuring-bind (an am) (array-dimensions a)
      (dotimes (i an)
        (dotimes (j am)
          (let ((x (aref a i j)))
            (incf norm (* x x))))))
    (sqrt norm)))

#+ignore
(defun mat-transpose (a)
  ;; Return transpose of A.
  (destructuring-bind (nrows ncols)
      (array-dimensions a)
    (let ((b (make-mat ncols nrows)))
      (dotimes (i nrows)
        (dotimes (j ncols)
          (setf (aref b j i) (aref a i j))))
      b)))


(defun mat-minor (a rows cols)
  ;; Extract a minor matrix indexed by rows and cols.
  ;; rows and cols can be one of the following S-exp:
  ;; 1. Just <Fixnum> treated as (<Fixnum>)
  ;; 2. Dotted pair of the form (<Beg> . <End>) equivalent to (<Beg> .. <End>)
  ;; 3. Just t treated as (0 .. <PossibleValue>)
  ;; 4. List of indices
  (flet ((convarg (x k)
           (cond
            ((fixnump x)
             (list x))
            ((and (consp x) (fixnump (car x)) (fixnump (cdr x)))
             (loop for i from (car x) to (cdr x) collect i))
            ((eq x t)
             (loop for i from 0 to (- (array-dimension a k) 1) collect i))
            ((listp x)
             x)
            (t
             (error "mat-minor: Illegal form of arguments: ROWS=~s COLS=~s"
                    rows cols)))))
    (setq rows (convarg rows 0)
          cols (convarg cols 1))
    '(format t "*** rows=~s cols=~s~%" rows cols)
    (let ((b (make-array (list (length rows) (length cols))
                         :element-type 'double-float)))
      (loop
          for i in rows
          for bi from 0 do
            (loop
                for j in cols
                for bj from 0 do
                  (setf (aref b bi bj) (aref a i j))))
      b)))

(defun make-random-symmetric-matrix (n &optional (rmin 0) (rmax 100))
  (let ((a (make-mat n n :element-type 'double-float)))
    (loop for i from 0 to (- n 1) do
          (loop for j from i to (- n 1) do
                (setf (aref a i j) (coerce (+ rmin (random (- rmax rmin)))
                                           'double-float))
                (unless (= i j)
                  (setf (aref a j i) (aref a i j)))))
    a))


(defun test-eigen (ndim &key (method 'eigen-by-householder-ql)
                             (verify t))
  (let ((a (make-random-symmetric-matrix ndim 0 100))
        (time (get-internal-run-time)))
    (multiple-value-bind (vals vecs)
        (funcall method (copy-matrix a))
      (setq time (/ (float (- (get-internal-run-time) time))
                    internal-time-units-per-second))
      (cond
       (verify
        (let ((norm (mat-norm (mat- (mat* a vecs)
                                    (mat* vecs (make-diag-mat vals))))))
          (when (< 0.0001 (abs norm))
            (format t "*** WARNING: error value seems too big !~%"))
          (format t "dim=~d  runtime=~,3f  error=~,4f  method=~a~%"
                  ndim time norm
                  (string-downcase (symbol-name method)))))
       (t
        (format t "dim=~d  runtime=~,3f  :verify=nil  method=~a~%"
                ndim time
                (string-downcase (symbol-name method)))))))
  (values))

(defvar *eigen-use-mkl*
    #+mkl t
    #-mkl nil)


#|| Benchmark on KISARAZU

>(load "sample/spectral-clustering-sample.cl"
      :external-format #+allegro :932 #-allegro :sjis)

>(setq *eigen-use-mkl* nil)
>(time (spectral-clustering-mcut *spectral-w* 3))
; cpu time (non-gc) 48.459683 sec user, 0.000000 sec system
; cpu time (gc)     1.402014 sec user, 0.000000 sec system
; cpu time (total)  49.861697 sec user, 0.000000 sec system
; real time  50.092000 sec
; space allocation:
;  3,818 cons cells, 165,590,144 other bytes, 0 static bytes
((2 4 6 8 11 12 14 16 18 19 ...) (0 1 3 5 7 9 10 13 15 17 ...)
 (55 73 86 95 111 146 157 257 376))
:SUCCESS

>(setq *eigen-use-mkl* t)
>(time (spectral-clustering-mcut *spectral-w* 3))
; cpu time (non-gc) 27.900119 sec user, 0.000000 sec system
; cpu time (gc)     0.821180 sec user, 0.000000 sec system
; cpu time (total)  28.721299 sec user, 0.000000 sec system
; real time  28.801000 sec
; space allocation:
;  3,957 cons cells, 189,036,032 other bytes, 0 static bytes
((0 2 3 5 11 12 15 16 19 20 ...) (1 4 6 7 8 9 10 13 14 17 ...) (219))
:SUCCESS


||#

(defun eigen (a)
  ;; Returns the eigen values, vectors, and iteration counts of the
  ;; symmetric matrix A.
  (cond
   (*eigen-use-mkl*
    #+mkl (eigen-mkl a)
    #-mkl (error "*eigen-use-mkl* is true when :mkl is not a *features*")
    )
   (t
    (eigen-hq a))))

#+mkl
(defun eigen-mkl (a)
  (multiple-value-bind (evals evecs)
      (symat-ev (copy-matrix a))
    ;; Transpose evecs
    (let ((n (length evals)))
      (dotimes (i n)
        (loop for j from (1+ i) below n
            do (rotatef (aref evecs i j) (aref evecs j i)))))
    ;;
    (values evals evecs)))

(defun eigen-hq (a)
  (eigen-by-householder-ql (copy-matrix a)))

#|
(defun eigen-by-jacobi (a)
  (declare (type dmat a))
  (jacobi (copy-matrix a)))

(defun eigen-by-householder-ql (a)
  (declare (type dmat a))
  (assert (typep a 'dmat)
      (a)
    "Type of array A is ~A: not (simple-arry double-float (* *))"
    (type-of a))
  (multiple-value-bind (a d e)
      (tred2 (copy-matrix a))
    (tqli d e a)))
|#


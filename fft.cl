; Fourier Transform Spectral Methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;Routines translated with permission by Kevin A. Broughan from ;;;;;;;;;;;
;;Numerical Recipies in Fortran Copyright (c) Numerical Recipies 1986, 1989;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; functions:
;	four1: fourier transform (FFT) in one dimension
;	realft: fourier transform of a real functions
;------------------------------------------------------------------------------

(defpackage :fast-fourier-transform
  (:use :cl :hjs.util.meta)
  (:nicknames :fft)
  (:export
   #:make-expt-array
   #:four1
   #:realft))

(in-package :fast-fourier-transform)

(defmacro fref (arr &rest indices)
  `(aref ,arr ,@(mapcar #'(lambda (x) (list '1- x)) indices)))

(defmacro fset (a b) 
  `(setf (fref ,(second a) ,@(cddr a)) ,b))

(defun list-array (a)
 (prog (a1)
  (setq a1 (make-array (apply #'* (array-dimensions a)) 
                       :displaced-to a
                       :element-type (array-element-type a)))
 (return (list-array1 a1))))

(defun list-array1 (a)
 (let ((ret nil))
      (do ((i (1- (array-dimension a 0)) (1- i))) ((< i 0) ret)
         (setq ret (cons (aref a i) ret)))))

;; make-expt-array
;; return array : D is extended to length base^i by adding 0
(defun make-expt-array (base D)
  (assert (and (integerp base) (plusp base)))
  (let* ((n (length D))
         (len
          (if (> n 1)
              (loop 
                  for i from 1
                  until (and (> n (expt base (- i 1))) (<= n (expt base i)))
                  finally (return (expt base i)))
            base))
         (new (make-array len :initial-element 0)))
    (loop for j from 0 below n
        do (setf (aref new j) (aref D j))
        finally (return new))))


(defun four1 (data nn &key (isign 1))
  (declare (type dvec data)) 
  (declare (type fixnum nn isign))

  (prog ((wr 0d0) (wi 0d0) (wpr 0d0) (wpi 0d0) (wtemp 0d0) 
                  (theta 0d0) (tempr 0d0) (tempi 0d0) (j 0) (n 0) (m 0) 
                  (mmax 0) (istep 0))
    (declare (type double-float wr wi wpr wpi wtemp theta tempr tempi)) 
    (declare (type fixnum j n m mmax istep))


    (setf n (* 2 nn)) 
    (setf j 1) 
    (do ((i 1 (+ i 2)))
        ((> i n) t)
      (declare (type fixnum i))
      (when (> j i) 
        (setf tempr (aref data (1- j)))
        (setf tempi (aref data j)) 
        (setf (aref data (1- j)) (aref data (1- i)))
        (setf (aref data j) (aref data i)) 
        (setf (aref data (1- i)) tempr)
        (setf (aref data i) tempi))
      (setf m (floor (/ n 2)))
      (loop while (and (>= m 2) (> j m))
          do (setf j (- j m)) (setf m (floor (/ m 2))))
      (setf j (+ j m)))

    (setf mmax 2)
    (loop while (> n mmax)
        do 
          (setf istep (* 2 mmax))
          (setf theta (/ 6.28318530717959d0 (* isign mmax)))
          (setf wpr (* -2.0d0 (expt (sin (* 0.5d0 theta)) 2)))
          (setf wpi (sin theta)) (setf wr 1.0d0) (setf wi 0.0d0)
          (do ((m 1 (+ m 2)))
              ((> m mmax) t)
            (declare (type fixnum m))
            (do ((i m (+ i istep)))
                ((> i n) t)
              (declare (type fixnum i))
              (setf j (+ i mmax))
              (setf tempr (+ (* (float wr) (aref data (1- j)))
                             (* (* -1d0 (float wi)) (aref data j))))
              (setf tempi (+ (* (float wr) (aref data j))
                             (* (float wi) (aref data (1- j)))))
              (setf (aref data (1- j)) (+ (aref data (1- i)) (- tempr)))
              (setf (aref data j) (+ (aref data i) (* -1d0 tempi)))
              (setf (aref data (1- i)) (+ (aref data (1- i)) tempr))
              (setf (aref data i) (+ (aref data i) tempi)))
            (setf wtemp wr)
            (setf wr (+ (+ (* wr wpr) (* (- wi) wpi)) wr))
            (setf wi (+ (+ (* wi wpr) (* wtemp wpi)) wi)))
          (setf mmax istep))
   
    (return data)))
;------------------------------------------------------------------------------

(defun realft (data n &key (isign 1))
  (declare (type dvec data)) 
  (declare (type fixnum n isign))

  (prog ((wr 0d0) (wi 0d0) (wpr 0d0) (wpi 0d0) (wtemp 0d0) 
                  (theta 0d0) (c1 0d0) (n2p3 0) (c2 0d0) (i1 0) (i2 0) (i3 0) (i4 0)
                  (wrs 0d0) (wis 0d0) (h1r 0d0) (h1i 0d0) (h2r 0d0) (h2i 0d0))
    (declare (type double-float wr wi wpr wpi wtemp theta c1 c2
                   wrs wis h1r h1i h2r h2i))
    (declare (type fixnum  n2p3 i1 i2 i3 i4))

    (setq n (/ (array-dimension data 0) 2))
 
    (setf theta (/ 3.141592653589793d0 (dfloat n))) 
    (setf c1 0.5d0) 
    (cond 
     ((= isign 1) 
      (setf c2 -0.5d0)
      (setq data (four1 data n :isign 1)))
     (t 
      (setf c2 0.5d0)
      (setf theta (- theta)))) 
    (setf wpr (* (- 2.0d0) (expt (sin (* 0.5d0 theta)) 2)))
 
    (setf wpi (sin theta)) 
    (setf wr (1+ wpr)) 
    (setf wi wpi) 
    (setf n2p3 (+ (* 2 n) 3)) 

    (do ((i 2 (+ i 1)))
        ((> i (/ n 2)) t)
      (declare (type fixnum i))
      (setf i1 (1- (* 2 i)))
      (setf i2 (+ i1 1))
      (setf i3 (+ n2p3 (- i2)))
      (setf i4 (1+ i3))

      (setf wrs (float wr))
      (setf wis (float wi))
      (setf h1r (* c1 (+ (fref data i1) (fref data i3))))
      (setf h1i (* c1 (- (fref data i2) (fref data i4))))
      (setf h2r (* (- c2) (+ (fref data i2) (fref data i4))))
      (setf h2i (* c2 (- (fref data i1) (fref data i3))))
      (fset (fref data i1) (- (+ h1r (* wrs h2r)) (* wis h2i)))
      (fset (fref data i2) (+ (+ h1i (* wrs h2i)) (* wis h2r)))
      (fset (fref data i3) (+ (- h1r (* wrs h2r)) (* wis h2i)))
      (fset (fref data i4) (+ (+ (- h1i) (* wrs h2i)) (* wis h2r)))
      (setf wtemp wr)
      (setf wr (+ (+ (* wr wpr) (* (- wi) wpi)) wr))
      (setf wi (+ (+ (* wi wpr) (* wtemp wpi)) wi))) 

    (cond 
     ((= isign 1) 
      (setf h1r (fref data 1))
      (fset (fref data 1) (+ h1r (fref data 2)))
      (fset (fref data 2) (+ h1r (- (fref data 2)))))
     (t
      (setf h1r (fref data 1)) 
      (fset (fref data 1) (* c1 (+ h1r (fref data 2))))
      (fset (fref data 2) (* c1 (+ h1r (- (fref data 2)))))
      (setq data (four1 data n :isign -1)))) 
   
    (return data)))


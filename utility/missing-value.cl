;;; Assume that processing system is following to IEEE 754
;;; e.g. allegro, sbcl, lispworks

(declaim (optimize (speed 3) (safety 0) (space 0) (debug 1)))

(defpackage :handling-missing-value
  (:use :cl :util :vector :statistics :hjs.util.meta)
  (:nicknames :missing-val)
  (:export
   #:*missing-values* #:missing-value-p
   #:*na* #:*nan* #:*c-nan* #:*+inf* #:*-inf*
   #:fill-na #:na-p #:nan-p #:c-nan-p
   #:outlier-verification))

(in-package :missing-val)

;;;;;;;;;;;;;;;;;;;;;;;;
; decide missing value ;
;;;;;;;;;;;;;;;;;;;;;;;;
;; list of missing values.
;; Symbols without nil are tested by value of symbol-name.
(defconstant *missing-values* '(nil "" "NA"))
(defun missing-value-p (value &key (missing-values-list *missing-values*)
                                   (test #'equalp))
  (member (typecase value
            (null nil)
            (symbol (symbol-name value))
            (t value))
          missing-values-list :test test))

 ;;;;;;;;;;;;;;;;;;;;;;;
 ; missing value -> NA ;
 ;;;;;;;;;;;;;;;;;;;;;;;
(defconstant *na* :NA)
(defun fill-na (seq &optional (predicate #'missing-value-p))
  (substitute-if *na* predicate seq))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; NA -> NaN | Categorical NaN ;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-int::set-floating-point-modes
   :traps
   (remove :invalid (getf (sb-int:get-floating-point-modes) :traps))))

(defconstant *nan* 
    #+allegro excl:*nan-double*
    #+sbcl #.(- #.sb-ext:double-float-positive-infinity #.sb-ext:double-float-positive-infinity)
    #+lispworks system::*double-float-nan*)
(defconstant *+inf*
    #+allegro excl:*infinity-double*
    #+sbcl #.sb-ext:double-float-positive-infinity
    #+lispworks 1D++0)
(defconstant *-inf*
    #+allegro excl:*negative-infinity-double*
    #+sbcl #.sb-ext:double-float-negative-infinity
    #+lispworks -1D++0)
;; NaN for category, assume that categorical data is serialized by positive integer
(defconstant *c-nan* 0)

(defun nan-p (value)
  #+allegro (excl::nan-p value)
  #+sbcl (and (floatp value) (sb-ext:float-nan-p value))
  #+lispworks (sys::nan-p value))
(defun c-nan-p (value) (and (numberp value) (= value *c-nan*)))
(defun na-p (value &key na-string (type :numeric)) ; :numeric | :category
  (or (case type (:numeric (nan-p value)) (:category (c-nan-p value)))
      (and na-string (stringp value) (string= value na-string))
      (eq value *na*)))

(defun subst-na-to (value seq &key na-string (type :numeric))
  (substitute-if value #'(lambda (val) (na-p val :na-string na-string :type type)) seq))

(defun na2nan (seq &optional na-string)
  (subst-na-to *nan* seq :na-string na-string :type :numeric))
(defun na2c-nan (seq &optional na-string)
  (subst-na-to *c-nan* seq :na-string na-string :type :category))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; interpolation: *na* | *nan* | *c-nan* -> value  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun subst-nan-to (value seq)
  (substitute-if value #'(lambda (val) (nan-p val)) seq))
(defun subst-c-nan-to (value seq)
  (substitute-if value #'(lambda (val) (c-nan-p val)) seq))

(defun remove-na-nan (seq                       
                      &key (seq-type :numeric) ; :numeric | :category
                           (remove-na t)
                           na-string)
  (if remove-na 
      (remove-if #'(lambda (v) (na-p v :na-string na-string :type seq-type)) seq)
    (remove-if (case seq-type (:numeric #'nan-p) (:category #'c-nan-p)) seq)))

(defun interpolate-missing-value-by-value (seq interp-val
                                           &key (na-interp t) na-string
                                                (seq-type :numeric))
  (when (and seq interp-val)
    (if na-interp 
        (subst-na-to interp-val seq :na-string na-string :type seq-type)
      (case seq-type
        (:numeric (subst-nan-to interp-val seq))
        (:category (subst-c-nan-to interp-val seq))))))

(defun interpolate (seq &key
                        (na-interp t) na-string
                        (interp :zero)
                        (seq-type :numeric))
  (when seq
    (cond ((eq seq-type :numeric)
           (cond ((member interp '(:zero :max :min :mean :median) :test #'eq)
                  (let* ((canon-seq (remove-na-nan seq :seq-type seq-type :na-string na-string))
                         (interp-val (ecase interp
                                       (:zero 0d0)
                                       (:max (reduce #'max canon-seq)) 
                                       (:min (reduce #'min canon-seq))
                                       (:mean (mean canon-seq)) 
                                       (:median (median canon-seq)))))
                    (declare (type double-float interp-val))
                    (interpolate-missing-value-by-value seq
                                                        interp-val
                                                        :na-interp na-interp
                                                        :na-string na-string
                                                        :seq-type seq-type)))
                 ((eq interp :spline)
                  (let* ((seq (if na-interp (na2nan seq na-string) seq))
                         (seq-dvec 
                          (if (typep seq 'dvec) seq
                            (map 'dvec #'(lambda (val) (coerce val 'double-float)) seq)))
                         (interped-dvec (3dim-spline-interp seq-dvec)))
                    (declare (type dvec interped-dvec))
                    (do ((i 0 (+ i 1))
                         (n (length seq)))
                        ((= i n) seq)
                      (declare (type integer i n))
                      (when (nan-p (elt seq i))
                        (setf (elt seq i) (aref interped-dvec i))))))
                 (t (error "Invalid value for keyword 'interp' | ~A" interp))))
          ((and (eq seq-type :category)
                (eq interp :mode))
           (let* ((canon-seq (remove-na-nan seq :seq-type seq-type :na-string na-string))
                  (mode (mode canon-seq)))
             (declare (type integer mode))
             (interpolate-missing-value-by-value seq 
                                                 mode
                                                 :na-interp na-interp
                                                 :na-string na-string
                                                 :seq-type seq-type)))
          (t (error "Invalid value for keyword 'seq-type' or 'interp' | ~A, ~A" seq-type interp))
          )))


 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; outlier verification: value -> *nan* | *c-nan* ;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun outlier-verification 
    (seq &key (type :smirnov-grubbs)
              ;; :std-dev | :mean-dev | :user | :freq | :smirnov-grubbs
              (outlier-value 0.05d0)
              (user-test #'=)
              (seq-type :numeric))
  (if (and (eq seq-type :numeric) (eq type :smirnov-grubbs))
      (labels ((recursive-smirnov (seq &key (type :min))
                 (let* ((no-nan-seq (remove-na-nan seq :seq-type seq-type))
                        (target (case type
                                  (:max (reduce #'max no-nan-seq))
                                  (:min (reduce #'min no-nan-seq))))
                        (outlier-value (if (numberp outlier-value) outlier-value 0.05d0))
                        (target-pos (position target no-nan-seq :test #'=)))
                   (if (smirnov-grubbs-p no-nan-seq target-pos outlier-value)
                       seq
                     (recursive-smirnov 
                      (substitute-if *nan* #'(lambda (v) (= v target)) seq) :type type)))))
        (recursive-smirnov (recursive-smirnov seq :type :min) :type :max))
    (let ((subst-value (ecase seq-type (:numeric *nan*) (:category *c-nan*)))
          (subst-test (if (eq type :user)
                          #'(lambda (val) (funcall user-test val outlier-value))
                        (let ((seq (remove-na-nan seq :seq-type seq-type)))
                          (ecase seq-type
                            (:numeric
                             (ecase type
                               (:std-dev 
                                (let* ((outlier-value 
                                        (if (numberp outlier-value) outlier-value 3d0))
                                       (k*std (* outlier-value (standard-deviation seq)))
                                       (ave (mean seq)))
                                  #'(lambda (val) 
                                      (unless (na-p val :type seq-type)
                                        (> (abs (- val ave)) k*std)))))
                               (:mean-dev
                                (let* ((outlier-value 
                                        (if (numberp outlier-value) outlier-value 3d0))
                                       (k*std (* outlier-value (mean-deviation seq)))
                                       (ave (mean seq)))
                                  #'(lambda (val) 
                                      (unless (na-p val :type seq-type)
                                        (> (abs (- val ave)) k*std)))))))
                            (:category
                             (if (eq type :freq)
                                 (let* ((outlier-value 
                                         (if (numberp outlier-value) outlier-value 0.01d0))
                                        (alist (count-values seq :test #'=))
                                        (thld (* outlier-value (length seq))))
                                   #'(lambda (val) (declare (type fixnum val))
                                             (unless (na-p val :type seq-type)
                                               (< (cdr (assoc val alist :test #'=)) thld))))
                               (error "Invalid outlier type for categorical value | ~A" type)
                               )))))))
      (substitute-if subst-value subst-test seq))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; cubic spline interpolation ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun 2- (m) (declare (type integer m)) (- m 2))

(defun spline (x y yp1 ypn)
  (declare (type dvec x y)) 
  (declare (type double-float yp1 ypn))

  (prog* ((n (length x))
          (y2 (make-array n :element-type 'double-float :initial-element 0d0))
          (u (make-array n :element-type 'double-float :initial-element 0d0))
          (p 0d0) (qn 0d0) (un 0d0) (sig 0d0))
    (declare (type dvec y2 u)) 
    (declare (type integer n))
    (declare (type double-float p qn un sig))
    
    (cond 
     ((> yp1 9.9d29) 
      (setf (aref y2 0) 0d0)
      (setf (aref u 0) 0d0))
     (t 
      (setf (aref y2 0) (- 0.5d0))
      (setf (aref u 0) (* (/ 3 (+ (aref x 1) (- (aref x 0))))
                          (+ (/ (+ (aref y 1) (- (aref y 0)))
                                (+ (aref x 1) (- (aref x 0))))
                             (- yp1))))))

    (do ((i 1 (1+ i)))
        ((> i (2- n)) t)
      (declare (type integer i))
      (setf sig (/ (+ (aref x i) (- (aref x (1- i))))
                   (+ (aref x (+ i 1)) (- (aref x (1- i))))))
      (setf p (+ (* sig (aref y2 (1- i))) 2d0))
      (setf (aref y2 i) (/ (1- sig) p))
      (setf (aref u i) (/ (+ (/ (* 6d0
                                   (+ (/ (+ (aref y (1+ i)) (- (aref y i)))
                                         (+ (aref x (1+ i)) (- (aref x i))))
                                      (/ (-
                                          (+
                                           (aref y i)
                                           (- (aref y (1- i)))))
                                         (+ (aref x i)
                                            (- (aref x (1- i)))))))
                                (+ (aref x (+ i 1))
                                   (- (aref x (1- i)))))
                             (* (- sig) (aref u (1- i))))
                          p))) 

    (cond 
     ((> ypn 9.9d29) 
      (setf qn 0d0) 
      (setf un 0d0))
     (t 
      (setf qn 0.5d0)
      (setf un (* (/ 3d0 (+ (aref x (1- n)) (- (aref x (2- n)))))
                  (+ ypn
                     (/ (- (+ (aref y (1- n)) (- (aref y (2- n)))))
                        (+ (aref x (1- n)) (- (aref x (2- n)))))))))) 

    (setf (aref y2 (1- n)) (/ (+ un (* (- qn) (aref u (2- n))))
                              (+ (* qn (aref y2 (2- n))) 1d0)))
 
    (do ((k (2- n) (1- k)))
        ((< k 0) t)
      (declare (type integer k))
      (setf (aref y2 k) (+ (* (aref y2 k) (aref y2 (+ k 1))) (aref u k)))) 
   
    (return y2)))

(defun splint (xa ya y2a x)
  (declare (type dvec xa ya y2a)) 
  (declare (type double-float x))

  (prog ((n 0) (y 0d0) (a 0d0) (b 0d0) (klo 0) (khi 0) (h 0d0) (k 0))
    (declare (type integer n klo khi k))
    (declare (type double-float y a b h))

    (setq n (array-dimension xa 0))
    (setf klo 1) 
    (setf khi n) 
   label1 

    (when (> (- khi klo) 1)
      (setf k (floor (/ (+ khi klo) 2)))
      (if 
          (> (aref xa (1- k)) x) 
          (setf khi k)
        (setf klo k))
      (go label1)) 

    (setq khi (1- khi) klo (1- klo))

    (setf h (- (aref xa khi) (aref xa klo))) 
    (if (= h 0d0) (error " bad xa input to splint ")) 
    (setf a (/ (- (aref xa khi) x) h)) 
    (setf b (/ (- x (aref xa klo)) h)) 
    (setf y (+ (+ (* a (aref ya klo)) (* b (aref ya khi)))
               (/ (* (+ (* (- (expt a 3) a) (aref y2a klo))
                        (* (- (expt b 3) b) (aref y2a khi)))
                     (expt h 2))
                  6))) 
   
    (return (the double-float y))))

;; input:
;;   fx-dvec: dependent value dvec
;;   &key
;;    x-dvec : independent value dvec
;;    yp1    : derivative at the interpolant at x_1 (default is for natural spline)
;;    ypn    : derivative at the interpolant at x_n (default is for natural spline)
;; return:
;;   interpolated fx-dvec
(defun 3dim-spline-interp (fx-dvec 
                           &key x-dvec
                                (yp1 most-positive-double-float)
                                (ypn most-positive-double-float))
  (when (> (length (remove-na-nan 
                    fx-dvec :remove-na nil :seq-type :numeric)) 2)
    (let* ((n (length fx-dvec))
           (x-dvec (or x-dvec
                       (specialize-vec
                        (make-array n 
                                    :initial-contents (loop for i from 1 to n
                                                          collect (coerce i 'double-float))))))
           (not-na-x-dvec
            (let ((not-na (make-array 0 
                                      :adjustable t 
                                      :fill-pointer t
                                      :element-type 'double-float)))
              (do-vecs ((fx fx-dvec :type double-float)
                        (x x-dvec :type double-float))
                (unless (nan-p fx) (vector-push-extend x not-na 1)))
              (specialize-vec not-na))))
      (cond ((= (length not-na-x-dvec) n) fx-dvec)
            ((> n (length not-na-x-dvec) 0)
             (let* ((not-na-fx-dvec (remove-na-nan
                                     fx-dvec :remove-na nil :seq-type :numeric))
                    (y2 (spline not-na-x-dvec not-na-fx-dvec yp1 ypn)))
               (do-vecs ((fx fx-dvec :type double-float :setf-var sf)
                         (x x-dvec :type double-float))
                 (when (nan-p fx)
                   (setf sf (splint not-na-x-dvec not-na-fx-dvec y2 x))))
               fx-dvec))))))


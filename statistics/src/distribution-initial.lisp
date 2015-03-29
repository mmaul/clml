(in-package :clml.statistics)

(eval-when (:compile-toplevel :load-toplevel)
  (defun parse-dist-slots (distribution-slots class)
    (let (class-slots methods)
      (dolist (slot distribution-slots)
        (let* ((def (cdr slot))
               (updater (getf def :accessor)))
          (if updater
              (let ((writer (intern (format nil "~A-~A" 'set updater)))
                    (s (car slot)))
                (setf def (copy-list def))
                (remf def :accessor)
                (push `(,s :reader ,updater
                           :writer ,writer ,@def) class-slots)
                ;; use (defmethod (setf foo))
                (push `(defmethod (setf ,updater) (new-value (obj ,class))
                         (setf (slot-value obj ',s) new-value)
                         (update-distribution obj))
                      methods))
              (push slot class-slots))))
      (values (nreverse class-slots)
              (nreverse methods)))))

;; easy definition of parameter cache class
(defmacro defdistribution (name direct-supers direct-slots &rest options)
  (multiple-value-bind (new-slot-definitions methods) (parse-dist-slots direct-slots name)
    `(prog1 (defclass ,name ,direct-supers ,new-slot-definitions ,@(when options
								     `(&rest . ,options)))
       ,@methods)))


(defclass distribution ()
  ((mean)
   (variance)
   (skewness)
   (kurtosis)
   (mode)))


(defclass discrete-distribution (distribution) ())
(defclass continuous-distribution (distribution)
  ((mean :initarg :mean :accessor mean :initform 0d0)))

(DEFCLASS GAMMA-LIKE-DISTRIBUTION (CONTINUOUS-DISTRIBUTION)
                 ((SCALE :READER SCALE :WRITER SET-SCALE :INITARG :SCALE)
                  (SHAPE :READER SHAPE :WRITER SET-SHAPE :INITARG :SHAPE)))

(DEFCLASS LOG-NORMAL-DISTRIBUTION (CONTINUOUS-DISTRIBUTION)
                 ((AVERAGE :READER AVERAGE :WRITER SET-AVERAGE :INITARG
                           :AVERAGE)
                  (STD :READER STD :WRITER SET-STD :INITARG :STD)))

(DEFCLASS UNIFORM-DISTRIBUTION (CONTINUOUS-DISTRIBUTION)
  ((FROM :READER UNIFORM-FROM :WRITER SET-UNIFORM-FROM :INITARG
         :FROM)
   (TO :READER UNIFORM-TO :WRITER SET-UNIFORM-TO :INITARG :TO)
   (WIDTH :READER UNIFORM-WIDTH)
   (DENOMINATOR :READER UNIFORM-DENOM :WRITER SET-UNIFORM-DENOM)
   (SKEWNESS :INITFORM 0.0) (KURTOSIS :INITFORM 1.8)))
(DEFCLASS GAMMA-DISTRIBUTION (GAMMA-LIKE-DISTRIBUTION)
                 ((GAMMA-FACTOR :READER GAMMA-FACTOR) (SHAPE-INV) (D) (C)))
(DEFCLASS ERLANG-DISTRIBUTION (GAMMA-LIKE-DISTRIBUTION)
                 ((INCLUDE-ZERO :READER INCLUDE-ZERO :WRITER SET-INCLUDE-ZERO
                                :INITARG :INCLUDE-ZERO :INITFORM NIL)))
(DEFCLASS EXPONENTIAL-DISTRIBUTION (CONTINUOUS-DISTRIBUTION)
                 ((HAZARD :READER HAZARD :WRITER SET-HAZARD :INITARG :HAZARD)
                  (SCALE :READER SCALE)
                  (INCLUDE-ZERO :READER INCLUDE-ZERO :WRITER SET-INCLUDE-ZERO
                                :INITARG :INCLUDE-ZERO :INITFORM NIL)
                  (SKEWNESS :INITFORM 2.0) (KURTOSIS :INITFORM 9.0)
                  (MODE :INITFORM 0.0)))
(DEFCLASS NORMAL-DISTRIBUTION (CONTINUOUS-DISTRIBUTION)
                 ((AVERAGE :READER AVERAGE :WRITER SET-AVERAGE :INITARG
                           :AVERAGE :INITFORM 0.0)
                  (MODE :INITFORM 0.0)
                  (STD :READER STD :WRITER SET-STD :INITARG :STD :INITFORM 0.0)
                  (SKEWNESS :INITFORM 0.0) (KURTOSIS :INITFORM 3.0)))
(DEFCLASS CHI-SQUARE-DISTRIBUTION (CONTINUOUS-DISTRIBUTION)
                 ((FREEDOM :READER FREEDOM :WRITER SET-FREEDOM :INITARG
                           :FREEDOM)
                  (EQ-GAMMA :INITFORM
                            (MAKE-INSTANCE 'GAMMA-DISTRIBUTION :SHAPE 2.0
                                           :SCALE 2.0)
                            :READER EQ-GAMMA)))
(DEFCLASS T-DISTRIBUTION (CONTINUOUS-DISTRIBUTION)
                 ((FREEDOM :READER FREEDOM :WRITER SET-FREEDOM :INITARG
                           :FREEDOM)
                  (T-PRECALC :READER T-PRECALC :WRITER SET-T-PRECALC) (R) (B)
                  (C) (A) (D) (K) (W) (S) (P) (Q) (T1) (T2) (V1) (V2)))
(DEFCLASS BETA-DISTRIBUTION (CONTINUOUS-DISTRIBUTION)
                 ((SHAPE1 :READER SHAPE1 :WRITER SET-SHAPE1 :INITARG :SHAPE1)
                  (SHAPE2 :READER SHAPE2 :WRITER SET-SHAPE2 :INITARG :SHAPE2)
                  (ALPHA-GAMMA :INITFORM
                   (MAKE-INSTANCE 'GAMMA-DISTRIBUTION :SHAPE 1.0 :SCALE 1.0))
                  (BETA-GAMMA :INITFORM
                   (MAKE-INSTANCE 'GAMMA-DISTRIBUTION :SHAPE 1.0 :SCALE
                                  1.0))))
(DEFCLASS F-DISTRIBUTION (CONTINUOUS-DISTRIBUTION)
                 ((FREEDOM1 :READER FREEDOM1 :WRITER SET-FREEDOM1 :INITARG
                            :FREEDOM1)
                  (FREEDOM2 :READER FREEDOM2 :WRITER SET-FREEDOM2 :INITARG
                            :FREEDOM2)
                  (CHI1 :INITFORM
                   (MAKE-INSTANCE 'CHI-SQUARE-DISTRIBUTION :FREEDOM 1))
                  (CHI2 :INITFORM
                   (MAKE-INSTANCE 'CHI-SQUARE-DISTRIBUTION :FREEDOM 1))
                  (F)))
(DEFCLASS BERNOULLI-RELATED-DISTRIBUTION (DISCRETE-DISTRIBUTION)
                 ((PROBABILITY :READER PROBABILITY :WRITER SET-PROBABILITY
                               :INITARG :PROBABILITY)))
(DEFCLASS BINOMIAL-DISTRIBUTION (BERNOULLI-RELATED-DISTRIBUTION)
                 ((SIZE :READER SIZE :WRITER SET-SIZE :INITARG :SIZE) (TABLE)
                  (KI) (VI) (B) (K) (W) (NSQ)))
(DEFCLASS GEOMETRIC-DISTRIBUTION (BERNOULLI-RELATED-DISTRIBUTION)
  ((TABLE) (KI) (VI) (B) (K) (W) (NSQ) (PSQ) (Q) (R) (C)))
(DEFCLASS HYPERGEOMETRIC-DISTRIBUTION (DISCRETE-DISTRIBUTION)
                 ((ELEMENTS :READER ELEMENTS :WRITER SET-ELEMENTS :INITARG
                            :ELEMENTS)
                  (SUCCESSES :READER SUCCESSES :WRITER SET-SUCCESSES :INITARG
                             :SUCCESSES)
                  (SAMPLES :READER SAMPLES :WRITER SET-SAMPLES :INITARG
                           :SAMPLES)
                  (TABLE) (KI) (VI) (B) (K) (W) (NSQ) (A1)))
(DEFCLASS CAUCHY-DISTRIBUTION (CONTINUOUS-DISTRIBUTION)
                 ((LOCATION :READER LOCATION :WRITER SET-LOCATION :INITARG
                            :LOCATION)
                  (SCALE :READER SCALE :WRITER SET-SCALE :INITARG :SCALE)))
(DEFCLASS LOGISTIC-DISTRIBUTION (CONTINUOUS-DISTRIBUTION)
                 ((LOCATION :READER LOCATION :WRITER SET-LOCATION :INITARG
                            :LOCATION)
                  (SCALE :READER SCALE :WRITER SET-SCALE :INITARG :SCALE)
                  (SKEWNESS :INITFORM 0.0) (KURTOSIS :INITFORM 4.2)))
(DEFCLASS NEGATIVE-BINOMIAL-DISTRIBUTION
                 (BERNOULLI-RELATED-DISTRIBUTION)
                 ((SUCCESS-R :READER SUCCESS-R :WRITER SET-SUCCESS-R :INITARG
                             :SUCCESS-R)
                  (TABLE) (KI) (VI) (B) (K) (W) (NSQ) (PSQ) (Q) (R) (XL) (XU)
                  (PL) (PU) (QUE) (S) (TEE)))
(DEFCLASS POISSON-DISTRIBUTION (DISCRETE-DISTRIBUTION)
                 ((RATE :READER RATE :WRITER SET-RATE :INITARG :RATE) (TABLE)
                  (KI) (VI) (B) (K) (W) (NSQ) (PSQ) (Q) (R) (XL) (XU) (PL) (PU)
                  (C)))
;;; todo -- weibull dist is not gamma distribution; bad definition
(DEFCLASS WEIBULL-DISTRIBUTION (GAMMA-LIKE-DISTRIBUTION)
                 ((INCLUDE-ZERO :READER INCLUDE-ZERO :WRITER SET-INCLUDE-ZERO
                                :INITARG :INCLUDE-ZERO :INITFORM NIL)
                  (R-INV)))

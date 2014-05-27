#||
CL-USER> (best-double-exp-parameters 
          (let ((a (make-array 100)))
            (loop for i below (length a) do (setf (aref a i) i))
            a)
          :step 0.01d0)
#(1 1)
1/100
||#

;;; #+asdf
;;; (eval-when (:load-toplevel :compile-toplevel :execute)
;;;  (asdf:operate 'asdf:load-op 'iterate))

(defpackage :exponential-smoothing
  (:use :cl :iter :ts-util :ts-stat
        :read-data :util :vector :vars :ts-read-data)
  (:nicknames :expl-smthing)
  (:export
   #:best-single-exp-parameters
   #:best-double-exp-parameters
   #:best-triple-exp-parameters
   #:holtwinters
   #:holtwinters-prediction))
(in-package :exponential-smoothing)

(defclass holtwinters-model (timeseries-model)
  ((exp-type :initarg :exp-type
             :accessor exp-type
             :type symbol
             :initform nil)
   (3-params :initarg :3-params
             :accessor 3-params
             :type list 
             :initform '(0 0 0))
   (err-info :initarg :err-info
             :accessor err-info
             :type list
             :initform '())
   (seasonal :initarg :seasonal
             :accessor seasonal
             :type symbol
             :initform nil)))
(defmethod print-object ((model holtwinters-model) stream)
  (with-accessors ((params 3-params)
                   (err err-info)
                   (seasonal seasonal)
                   (exp-type exp-type)
                   ) model
    (print-unreadable-object (model stream :type t :identity nil))
    (case exp-type 
      (:single
       (format stream "~&alpha: ~D~%"
               (first params)))
      (:double 
       (format stream "~&alpha: ~D, beta: ~D~%"
               (first params) (second params)))
      (:triple
       (format stream "~&alpha: ~D, beta: ~D, gamma: ~D~%"
               (first params) (second params) (third params))
       (format stream "~&seasonal: ~A~%" seasonal)))
    (format stream "~&error: ~D ( ~A )~%" (second err) (first err))))

(defgeneric n (obj)
  (:method (obj) (declare (ignorable obj)))
  (:method ((seq sequence)) (length seq)))

(defgeneric v (obj time)
  (:method ((func function) time) (funcall func time))
  (:method ((n number) time) (declare (ignorable time)) n)
  (:method ((seq sequence) time) (elt seq time)))

(defun ^2 (x) (* x x))

; (symmetric) mean absolute percentage error
(defun mape (a f &key (symmetric t))
  (let ((n (n a)))
    (/
     (loop for time below n
         summing
           (let* ((a (v a time)) 
                  (f (v f time))
                  (denom (if symmetric
                             (/ (+ a f) 2)
                           a)))
             (if (zerop denom)
                 (abs f)
               (abs (/ (- a f) denom)))))
     n)))

; mean square error
(defun mse (a f)
  (let ((n (n a)))
    (/ (loop for time below n summing
             (let ((a (v a time)) (f (v f time)))
               (^2 (- a f))))
       n)))

; relative absolute error
(defun rae (a f)
  (let* ((n (n a))
         (numerator 
          (loop for time below n summing
                (let ((a (v a time)) (f (v f time)))
                  (abs (- a f)))))
         (mean-value (/ (loop for time below n summing
                              (v a time))
                        n))
         (denominator
          (loop for time below n summing
                (let ((a (v a time)))
                  (abs (- a mean-value))))))
    (if (zerop denominator)
        most-positive-double-float
      (/ numerator denominator))))

; mean absolute error
(defun mae (a f)
  (let ((n (n a)))
    (/ (loop for time below n summing
             (let ((a (v a time)) (f (v f time)))
               (abs (- a f))))
       n)))

; relative error
(defun re (a f)
  (let ((n (n a)))
    (/ (loop for time below n summing
             (let ((a (v a time)) (f (v f time)))
               (if (zerop a)
                   (abs f)
                 (/ (abs (- f a)) a))))
       n)))

; relative square error
(defun rr (a f)
  (let ((n (n a)))
    (/ (loop for time below n summing
             (let ((a (v a time)) (f (v f time)))
               (if (zerop a)
                   (^2 f)
                 (/ (^2 (- f a)) (^2 a)))))
       n)))

; logarithm
(defun log-mse (a f)
  (flet ((safe-log (num)
           (if (zerop num)
               most-negative-double-float
             (log num))))
    (let* ((n (n a))
           (value
            (/ (loop for time below n summing
                     (let ((a (v a time)) (f (v f time)))
                       (^2 (- (safe-log f) (safe-log a)))))
               n)))
      (if (complexp value)
          ;; length of polar coordinate system
          (sqrt (+ (^2 (realpart value)) (^2 (imagpart value))))
        value))))
             

(defun make-exp-forecaster (&key (alpha 1/2) s)
  (lambda (x)
    (unless s (setf s x))
    (setf s (+ (* x alpha) (* (- 1 alpha) s)))))

(defun make-double-exp-forecaster (&key (alpha 1/2) (beta 1/2) a b s)
  (let ((init2 (make-array 2 :initial-element :na)))
  (lambda (x)
    (if (find :na init2)
        (setf (aref init2 (position :na init2)) x)
      (progn
        (unless (and a b s)
          (setf a (aref init2 1)
                b (- (aref init2 1) (aref init2 0))
                s (+ a b)))
        (let ((a_-1 a))
          (setf 
              a (+ (* alpha x) (* (- 1 alpha) s))
              b (+ (* beta (- a a_-1)) (* (- 1 beta) b))
              s (+ a b))))))))

(defun forecast-sequence (forecaster sequence)
  (map 'vector forecaster sequence))

(defun make-triple-exp-forecaster (&key (alpha 1/2) (beta 1/2) (gamma 1/2) 
                                        frequency (seasonal :additive) l)
  (declare (ignorable l))
  (assert (>= frequency 2))
  (let ((s (make-array frequency :initial-element nil)) (n 0) a b)
    (lambda (Y &optional (steps-ahead 1))
      (setf n (mod (1+ n) frequency))
      (let ((s_-p (aref s n)))
        (cond 
         (s_-p
          (if (and a b)
              (flet ((scale (parameter with without)
                       (+ (* parameter with) (* (- 1 parameter) without))))
                (let* ((a_+1 (scale alpha 
                                    (case seasonal (:additive (- Y s_-p))
                                          (:multiplicative (/ Y s_-p)))
                                    (+ a b)))
                       (s_+1 (scale gamma 
                                    (case seasonal (:additive (- Y a_+1))
                                          (:multiplicative (/ Y a_+1)))
                                    s_-p))
                       (b_+1 (scale beta (- a_+1 a) b)))
                  (setf a a_+1
                        b b_+1
                        (aref s n) s_+1)))
            (error "illegal seasonal word was given"))
          (case seasonal
            (:additive
             (+ a (* steps-ahead b) (aref s (mod (+ n steps-ahead) frequency))))
            (:multiplicative
             (* (+ a (* steps-ahead b)) (aref s (mod (+ n steps-ahead) frequency))))))
         (t                             ; set value for seasonal while first one period
          (setf (aref s n) Y)
          (when (zerop n)
            (case seasonal
              (:additive
               (loop 
                   for old-y = nil then y
                   for y across s
                   for i from 0
                   when old-y
                   summing (- y old-y) into m
                   summing y into total
                   finally 
                     (setf b (/ m frequency)
                           a (/ total frequency)))
               (loop for i below frequency do
                     (decf (aref s i) a)))
              (:multiplicative
               (progn
                 (setf b (/ (- (aref s 0) (aref s (1- frequency)))
                            (case frequency
                              (1 (1- l)) (t (* (- frequency 1) l))))
                       a (- (/ (reduce #'+ s) (length s))
                            (* (/ l 2) b)))
                 (loop for i below frequency
                     as j = (mod (1+ i) frequency)
                     with x_t = (/ (reduce #'+ s) (length s))
                     do (setf (aref s j)
                          (/ x_t (- x_t (* b (- (/ (1+ l) 2) j))))))))))
          Y))))))

(defun simple-forecaster-quality (forecaster seq &key (measure 'mse))
  (funcall measure seq
	   (lambda (time) 
	     (if (zerop time)
		 (v seq time)
		 (funcall forecaster (v seq (1- time)))))))

(defun brute-optimize-parameters (function parameters &key (step 1/10))
  (let ((parameters (copy-seq parameters)) (n (length parameters)))
    (loop for i below (length parameters) do (setf (elt parameters i) 0.0d0))
    (flet ((next-parameters ()
             (loop for i from (1- n) downto 0 thereis
                   (symbol-macrolet ((p (aref parameters i)))
                     (cond ((< p 1) (incf p step) (when (> p 1) (setf p 1)) i)
                           (t (setf p 0) nil))))))
      (let (best-score best-parameters)
        (loop do
              (let ((score (funcall function parameters)))
                (when (or (not best-score) (< score best-score))
                  (setf best-score score
                        best-parameters (copy-seq parameters))))

            while (next-parameters))
        (values best-parameters best-score)))))


(defun best-single-exp-parameters (sequence &key (step 0.01d0)
                                                 (measure 'mse))
  (brute-optimize-parameters 
   (lambda (parameters)
     (simple-forecaster-quality (make-exp-forecaster 
                                 :alpha (elt parameters 0))
                                sequence
                                :measure measure
                                ))
   (make-array 1)
   :step step))

(defun best-double-exp-parameters (sequence &key (step 0.01d0)
                                                 (measure 'mse))
  (brute-optimize-parameters 
   (lambda (parameters)
     (simple-forecaster-quality (make-double-exp-forecaster 
                                 :alpha (elt parameters 0)
                                 :beta (elt parameters 1))
                                sequence
                                :measure measure
                                ))
   (make-array 2)
   :step step))


(defun best-triple-exp-parameters (sequence &key (step 0.01d0) frequency
                                                 (measure 'mse)
                                                 (seasonal :additive)
                                                 l)
  (brute-optimize-parameters 
   (lambda (parameters)
     (simple-forecaster-quality (make-triple-exp-forecaster
                                 :alpha (elt parameters 0)
                                 :beta (elt parameters 1)
                                 :gamma (elt parameters 2)
                                 :frequency frequency
                                 :seasonal seasonal :l l)
                                sequence :measure measure
                                ))
   (make-array 3)
   :step step))

(defmethod holtwinters ((d time-series-dataset) &key alpha beta gamma 
                                                     (err-measure 'mse) 
                                                     (optim-step 0.1d0)
                                                     (seasonal :additive))
  (with-accessors ((start ts-start)
                   (end ts-end)
                   (points ts-points)
                   (freq ts-freq)
                   (dims dataset-dimensions)) d
    (assert (= 1 (length dims)))
    (let* ((exp-type (cond ((and (every #'numberp (list beta gamma))
                                 (every #'zerop (list beta gamma)))
                            :single)
                           ((or (and (numberp gamma) (zerop gamma)) (= freq 1))
                                :double)
                           (t :triple)))
           (learn-seq (map 'vector #'(lambda (p) (aref (ts-p-pos p) 0))
                           (ts-points d)))
           (err -1)
           (best-score
            (if (some #'null (list alpha beta gamma))
                (case exp-type
                  (:single
                   (multiple-value-bind (score diff)
                       (best-single-exp-parameters learn-seq
                                                   :measure err-measure
                                                   :step optim-step)
                     (prog1 score (setq err diff))))
                  (:double
                   (multiple-value-bind (score diff)
                       (best-double-exp-parameters learn-seq
                                                   :measure err-measure
                                                   :step optim-step)
                     (prog1 score (setq err diff))))
                  (:triple
                   (assert (>= (length points) (* 3 freq)))
                   (multiple-value-bind (score diff)
                       (best-triple-exp-parameters learn-seq
                                                   :measure err-measure
                                                   :frequency freq
                                                   :step optim-step
                                                   :seasonal seasonal
                                                   :l (round (length learn-seq) freq))
                     (prog1 score (setq err diff)))))
              (list alpha beta gamma))))
      (make-instance 'holtwinters-model
        :exp-type exp-type :3-params (coerce best-score 'list)
        :err-info `(,err-measure ,err) :observed-ts d
        :seasonal seasonal))))

(defmethod predict ((model holtwinters-model) &key (n-ahead 0))
  (with-accessors ((ts observed-ts)
                   (best-score 3-params)
                   (exp-type exp-type)
                   (seasonal seasonal)) model
    (assert (not (minusp n-ahead)))
    (let* ((learn-seq (map 'vector
                        #'(lambda (p) (aref (ts-p-pos p) 0))
                        (ts-points ts)))
           (forecaster 
            (case exp-type
              (:single (make-exp-forecaster :alpha (elt best-score 0)))
              (:double (make-double-exp-forecaster
                        :alpha (elt best-score 0) :beta (elt best-score 1)))
              (:triple (make-triple-exp-forecaster
                        :alpha (elt best-score 0) :beta (elt best-score 1) 
                        :gamma (elt best-score 2) :frequency (ts-freq ts)
                        :seasonal seasonal 
                        :l (round (length (ts-points ts)) (ts-freq ts))))))
           (est-start (tf-incl (ts-start ts) 1 :freq (ts-freq ts)))
           (ested-seq (if (plusp n-ahead)
                          (coerce 
                           (loop with len = (length learn-seq)
                               with last-val
                               for i from 0 below (1- (+ len n-ahead))
                               collect (if (< i len)
                                           (setq last-val 
                                             (funcall forecaster (aref learn-seq i)))
                                         (setq last-val
                                           (funcall forecaster last-val))))
                           'vector)
                        (subseq (forecast-sequence forecaster learn-seq)
                                0 (1- (length learn-seq))))))
      (make-constant-time-series-data
       (map 'list #'dimension-name (dataset-dimensions ts))
       (map 'vector
         (lambda (val) (let* ((sp (make-dvec 1))) (declare (type dvec sp))
                             (setf (aref sp 0) (coerce val 'double-float))
                             sp)) ested-seq)
       :start est-start :freq (ts-freq ts)))))

(defmethod HoltWinters-prediction ((d time-series-dataset)
                                   &key alpha beta gamma
                                        (seasonal :additive)
                                        (err-measure 'mse) 
                                        (optim-step 0.1d0)
                                        n-learning
                                        (n-ahead 0) 
                                        target-col)
  (with-accessors ((start ts-start)
                   (end ts-end)
                   (points ts-points)
                   (freq ts-freq)
                   (dims dataset-dimensions)) d
    (assert (and (>= n-ahead 0) (integerp n-ahead)))
    (if n-learning
        (setq n-learning (min (length (ts-points d)) n-learning))
      (setq n-learning (length (ts-points d))))
    (let* ((d (if target-col
                  (let ((pos (position target-col (dataset-dimensions d)
                                       :test #'string-equal
                                       :key #'dimension-name)))
                    (if pos (sub-ts d :range `(,pos) 
                                    :end (tf-incl start n-learning :freq freq))
                      (error "Does not exist column ~A" target-col)))
                (progn (assert (= 1 (length (dataset-dimensions d)))) d)))
           (model (holtwinters d :alpha alpha :beta beta :gamma gamma
                               :err-measure err-measure :optim-step optim-step
                               :seasonal seasonal))
           (pred (predict model :n-ahead n-ahead)))
      (values pred model))))


  
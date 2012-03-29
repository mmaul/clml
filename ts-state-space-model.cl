;;  classes and methods for state-space-model
;;  reference: "ŽžŒn—ñ‰ðÍ“ü–å ’˜:–kìŒ¹Žl˜Y Šâ”g‘“X" 9 ÍˆÈ~
(defpackage :ts-state-space
  (:use :cl :read-data :util :vector :matrix
        :statistics :ts-util :ts-stat :ts-read-data
        :handling-missing-value)
  (:nicknames :ts-stsp)
  (:export 
   #:trend #:trend-prediction
   #:seasonal #:seasonal-adj))

(in-package :ts-stsp)

(defclass state-space-model (timeseries-model)
  ((F-matrices :initarg :F-matrices 
               :initform (error "Must specify the F matrices"))
   (G-matrices :initarg :G-matrices 
               :initform (error "Must specify the G matrices"))
   (H-matrices :initarg :H-matrices 
               :initform (error "Must specify the H matrices"))
   (Q-matrices :initarg :Q-matrices :initform nil)
   (R-matrices :initarg :R-matrices :initform nil)
   ))

(defmethod F ((stsp state-space-model) n)
  (let ((val (slot-value stsp 'F-matrices)))
    (cond ((functionp val) (funcall val n))
          ((arrayp val) val)
          ((listp val) (nth n val))
          (t (error "illegal F-matrices | ~A" val)))))
(defmethod G ((stsp state-space-model) n)
  (let ((val (slot-value stsp 'G-matrices)))
    (cond ((functionp val) (funcall val n))
          ((arrayp val) val)
          ((listp val) (nth n val))
          (t (error "illegal G-matrices | ~A" val)))))
(defmethod H ((stsp state-space-model) n)
  (let ((val (slot-value stsp 'H-matrices)))
    (cond ((functionp val) (funcall val n))
          ((arrayp val) val)
          ((listp val) (nth n val))
          (t (error "illegal H-matrices | ~A" val)))))
(defmethod Q ((stsp state-space-model) n)
  (let ((val (slot-value stsp 'Q-matrices)))
    (cond ((functionp val) (funcall val n))
          ((arrayp val) val)
          ((listp val) (nth n val))
          (t (error "illegal Q-matrices | ~A" val)))))
(defmethod R ((stsp state-space-model) n)
  (let ((val (slot-value stsp 'R-matrices)))
    (cond ((functionp val) (funcall val n))
          ((arrayp val) val)
          ((listp val) (nth n val))
          (t (error "illegal R-matrices | ~A" val)))))

(defclass gaussian-stsp-model (state-space-model)
  ((x-nn :initarg :x-nn :accessor x-nn :initform nil :type list)
   (x-nn-1 :initarg :x-nn-1 :accessor x-nn-1 :initform nil :type list)
   (v-nn :initarg :v-nn :accessor v-nn :initform nil :type list)
   (v-nn-1 :initarg :v-nn-1 :accessor v-nn-1 :initform nil :type list)))
(defgeneric x-00 (gaussian-stsp-model)
  (:documentation "Initial value of State x with gaussian-stsp-model"))
(defgeneric v-00 (gaussian-stsp-model)
  (:documentation "Initial value of covariance v with gaussian-stsp-model"))
(defgeneric aic (gaussian-stsp-model)
  (:documentation "Akaike Information Criterion for gaussian-stsp-model"))

(defmethod kalman-filter ((stsp gaussian-stsp-model) &key x-00 v-00)
  (let ((x-00 (if x-00 x-00 (x-00 stsp)))
        (v-00 (if v-00 v-00 (v-00 stsp))))
    (declare (type dvec x-00))
    (declare (type dmat v-00))
    (assert (and x-00 v-00 (observed-ts stsp)))
    (with-accessors ((x x-nn)
                     (x-1 x-nn-1)
                     (v v-nn)
                     (v-1 v-nn-1)) stsp
      (loop with len = (length (ts-points (observed-ts stsp)))
          for n below len
          initially (setf x-1 (make-list len)
                          x (make-list len)
                          v (make-list len)
                          v-1 (make-list len))
          do (multiple-value-bind (%x-nn %v-nn %x-nn-1 %v-nn-1)
                 (one-step-kalman-filter stsp x-00 v-00
                                         (ts-p-pos (aref (ts-points (observed-ts stsp)) n))
                                         n)
               (setf (nth n x-1) %x-nn-1
                     (nth n v-1) %v-nn-1
                     (nth n x) %x-nn
                     (nth n v) %v-nn
                     x-00 %x-nn
                     v-00 %v-nn))))))
(defmethod one-step-kalman-filter ((stsp gaussian-stsp-model) x v observed-pt n)
  (multiple-value-bind (frcst-x frcst-v) (one-step-forecast stsp x v n)
    (apply #'values `(,@(multiple-value-list (filtering stsp frcst-x frcst-v observed-pt n))
                        ,frcst-x ,frcst-v))))
    
(defmethod one-step-forecast ((stsp gaussian-stsp-model) x v n)
  (declare (type dvec x))
  (declare (type dmat v))
  (values
   (m*v (F stsp n) x)
   (mcm (m*m (m*m (F stsp n) v) (transpose (F stsp n)))
        (m*m (m*m (G stsp n) (Q stsp n)) (transpose (G stsp n))))))

(defmethod filtering ((stsp gaussian-stsp-model) x v observed-pt n)
  (declare (type dvec x))
  (declare (type dmat v))  
  (let ((nanp (find-if #'nan-p observed-pt)))
    (if nanp (values x v)
      (let ((kalman-gain
             (m*m (m*m v (transpose (H stsp n)))
                  (m^-1 (mcm (m*m (m*m (H stsp n) v) (transpose (H stsp n)))
                             (R stsp n))))))
        (values
         (vcv x (m*v kalman-gain (vcv observed-pt (m*v (H stsp n) x) :c #'-)) :c #'+)
         (mcm v (m*m (m*m kalman-gain (H stsp n)) v) :c #'-))))))

  
(defmethod long-step-forecast ((stsp gaussian-stsp-model) x v n n-ahead)
  (assert (>= n-ahead 1))
  (loop for i from 1 to n-ahead
      with x-nn = (make-list n-ahead)
      with v-nn = (make-list n-ahead)
      do (multiple-value-setq (x v)
           (one-step-forecast stsp x v (+ n i)))
         (setf (nth (1- i) x-nn) x
               (nth (1- i) v-nn) v)
      finally (return (values x-nn v-nn))))

(defmethod forecast ((stsp gaussian-stsp-model) n-ahead
                     &key (smoothing nil))
  "Forecast observation value"
  (assert (>= n-ahead 0))
  (flet ((map-mat (mat fcn)
           (loop for i below (array-dimension mat 0)
               do (loop for j below (array-dimension mat 1)
                      do (setf (aref mat i j) (funcall fcn (aref mat i j)))))
           mat)
         (fore-y (index state)
           (m*v (H stsp index) state))
         (fore-d (index v)
           (mcm (m*m (m*m (H stsp index) v)
                     (transpose (H stsp index)))
                (R stsp index))))
    (with-accessors ((x x-nn) (v v-nn)
                     (x-1 x-nn-1) (v-1 v-nn-1)) stsp
      (multiple-value-bind (smthed-x smthed-v)
          (if smoothing (smoothing stsp) (values x-1 v-1))
        (let* ((len (length x))
               (result-len (+ len n-ahead))
               (ret-x (make-list result-len))
               (ret-v (make-list result-len))
               x-list v-list)
          (when (> n-ahead 0)
            (multiple-value-setq (x-list v-list)
              (long-step-forecast 
               stsp (car (last x)) (car (last v)) (1- len) n-ahead)))
          (loop for i below result-len
              do (if (< i len)
                     (setf (nth i ret-x) (fore-y i (nth i smthed-x))
                           (nth i ret-v) (fore-d i (nth i smthed-v)))
                   (setf (nth i ret-x) (fore-y i (nth (- i len) x-list))
                         (nth i ret-v) (fore-d i (nth (- i len) v-list))))
              finally 
                (return 
                  (values ret-x (map 'list #'(lambda (mat) 
                                               (map-mat mat #'(lambda (v)
                                                                (dfloat (sqrt (/ v len))))))
                                     ret-v)))))))))

(defmethod smoothing ((stsp gaussian-stsp-model))
  "Do smoothing under the condition N which is a number of observed data"
  (let ((N (length (ts-points (observed-ts stsp)))))
    (unless (and (x-nn stsp) (v-nn stsp))
      (kalman-filter stsp))
    (with-accessors ((x x-nn)
                     (x-1 x-nn-1)
                     (v v-nn)
                     (v-1 v-nn-1)) stsp
      (loop for i from (- N 2) downto 0
          with smthed-x = `(,(nth (1- N) x))
          with smthed-v = `(,(nth (1- N) v))
          with An-list
          as (%smthed-x %smthed-v An) = 
            (multiple-value-list (one-step-smoothing stsp (car smthed-x) (car smthed-v)
                                                     (nth (1+ i) x-1) (nth (1+ i) v-1)
                                                     (nth i x) (nth i v) i))
          do (push %smthed-x smthed-x)
             (push %smthed-v smthed-v)
             (push An An-list)
          finally (return (values smthed-x smthed-v An-list))))))
(defmethod one-step-smoothing ((stsp gaussian-stsp-model) smthed-x smthed-v pred-x pred-v x v n)
  (let ((An (m*m (m*m v (transpose (F stsp (1+ n)))) (m^-1 pred-v))))
    (values (vcv x (m*v An (vcv smthed-x pred-x :c #'-)))
            (mcm v (m*m (m*m An (mcm smthed-v pred-v :c #'-)) (transpose An)))
            An)))

(defmethod log-likelihood ((stsp gaussian-stsp-model) 
                           &key (with-s^2 t) (smoothing nil))
  (let ((org-r (if with-s^2 (kalman-filter stsp)
                 (prog1 (slot-value stsp 'R-matrices)
                   (setf (slot-value stsp 'R-matrices) 
                     (diag (array-dimension (R stsp 0) 0) 1.0d0))
                   (kalman-filter stsp)))))
    (with-accessors ((x x-nn-1) (v v-nn-1) (ts observed-ts)) stsp
      (multiple-value-bind (x v)
          (if smoothing (smoothing stsp) (values x v))
        (let* ((n (length x))
               (y-n (loop for i below n collect
                          (m*v (H stsp i) (nth i x))))
               (d-n (loop for i below n collect
                          (mcm (m*m (m*m (H stsp i) (nth i v)) (transpose (H stsp i)))
                               (R stsp i)))))
          (if with-s^2
              (* -1/2
                 (+ (* (length (dataset-dimensions ts)) n
                       (log (* 2 (coerce pi 'double-float))))
                    (loop for d-i in d-n
                        as log-detd = (log (det d-i))
                        sum log-detd)
                    (loop for d-i in d-n
                        for y-i in y-n
                        for y across (map 'vector #'ts-p-pos (ts-points ts))
                        as dy = (vcv y y-i :c #'-)
                        sum (vdotv dy (m*v (m^-1 d-i) dy)))))
            (progn 
              (assert (= 1 (length (dataset-dimensions ts))))
              (let* ((d-n (loop for d in d-n
                              with cov = (aref (ts-covariance ts) 0 0)
                              collect (/ (aref d 0 0) cov)))
                     (s^2 
                      (* (/ n)
                         (loop for i from 1 to n
                             for y across (map 'vector #'ts-p-pos (ts-points ts))
                             for y-i in y-n
                             for d in d-n
                             as dy = (aref (vcv y y-i :c #'-) 0)
                             sum (/ (expt dy 2) d)))))
                (setf (slot-value stsp 'R-matrices) org-r)
                (values
                 (* -1/2
                    (+ (* n (1+ (log (* 2 (coerce pi 'double-float) s^2))))
                       (loop for d in d-n sum (log d))))
                 s^2)))))))))

(defun make-ts-by-forecast (pos-list se-list org-ts
                            &key (n-ahead 0))
  (let* ((start (ts-start org-ts))
         (end (tf-incl (ts-end org-ts) n-ahead :freq (ts-freq org-ts)))
         (time-labels (concatenate 'vector
                        (map 'vector #'ts-p-label (ts-points org-ts))
                        (make-array n-ahead :element-type 'string
                                    :initial-element "predicted"))))
    (values
     (make-constant-time-series-data
      '("forecast")
      (map 'vector
        #'(lambda (trend)
            (make-array (length trend)
                        :initial-contents trend
                        :element-type 'double-float))
        pos-list)
      :time-label-name (time-label-name org-ts)
      :time-labels time-labels
      :start start :end end :freq (ts-freq org-ts))
     (make-constant-time-series-data
      '("standard error")
      (map 'vector
        #'(lambda (se) (matrix::mat2array se)) se-list)
      :time-label-name (time-label-name org-ts)
      :time-labels time-labels
      :start start :end end :freq (ts-freq org-ts))
     )))

(defmethod predict ((model gaussian-stsp-model) &key (n-ahead 0))
  (kalman-filter model)
  (multiple-value-bind (pos-list se-list)
      (forecast model n-ahead :smoothing t)
    (with-accessors ((ts observed-ts)) model
      (make-ts-by-forecast pos-list se-list ts :n-ahead n-ahead))))

#||
(progn 
  (setq x
    (make-instance 'gaussian-stsp-model
      :F-matrices (make-array '(9 9)
                              :initial-contents
                              `((0.17438913366790465 -0.20966263354643136 0.459202505071864 1.0144694385486095 0.2871426375860843 -0.09273505423571009 -0.13087574744684466 -0.34467398543738703 -0.1765456124104221)
                                ,@(loop for i below (1- 9)
                                      collect (let ((list (make-list 9 :initial-element 0d0)))
                                                (setf (nth i list) 1d0) list)))
                              :element-type 'double-float)
      :G-matrices (make-array '(9 1)
                              :initial-contents
                              (loop for i below 9
                                  collect (if (= i 0) '(1.0d0) '(0.0d0)))
                              :element-type 'double-float)
      :H-matrices (make-array '(1 9)
                              :initial-contents
                              `(,(loop for i below 9
                                     collect (if (= i 0) 1.0d0 0.0d0)))
                              :element-type 'double-float)
      :Q-matrices (ts-covariance ukgas)
      :R-matrices (diag 1 0.0d0)
      :observed-ts (ts-demean ukgas)
      ))
  (kalman-filter x :x-00 (make-array 9 :initial-element 0.0d0
                                     :element-type 'double-float)
                 :v-00 (diag 9 1.0d0)))
||#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; trend model (1-dimensional) ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass trend-model (gaussian-stsp-model)
  ((diff-k :initarg :diff-k :initform nil :type integer :accessor diff-k)
   (tau^2 :initarg :tau^2 :initform nil :type number :accessor tau^2)
   (aic :initarg :aic :initform *nan* :type number)))

(defmethod print-object ((model trend-model) stream)
  (with-accessors ((k diff-k) (t^2 tau^2) (ts observed-ts)) model
    (print-unreadable-object (model stream :type t :identity nil))
    (format stream "~&K:   ~D~%" k)
    (format stream "~&T^2: ~F~%" t^2)
    (format stream "~&AIC: ~F~%" (slot-value model 'aic))))

(defmethod aic ((model trend-model))
  (+ (* -2 (log-likelihood model :smoothing t))
     (* 2 (+ (diff-k model) 2))))

(defun c_i (k i)
  (declare (type fixnum k i))
  (labels ((fact (i) (if (<= i 0) 1 (* i (fact (1- i))))))
    (* (expt (- 1) (1+ i))
       (/ (fact k) (* (fact (- k i)) (fact i))))))

(defun make-trend-F (diff-k)
  (declare (type fixnum diff-k))
  (make-array 
   `(,diff-k ,diff-k)
   :initial-contents 
   `(,(loop for i from 1 to diff-k
          collect (coerce (c_i diff-k i) 'double-float))
      ,@(loop for i below (1- diff-k)
            collect 
              (let ((list (make-list diff-k :initial-element 0d0)))
                (setf (nth i list) 1d0) list)))
   :element-type 'double-float))
(defun make-trend-G (size)
  (declare (type fixnum size))
  (make-array `(,size 1)
              :initial-contents
              (loop for i below size
                  collect `(,(if (= i 0) 1.0d0 0.0d0)))
              :element-type 'double-float))
(defun make-trend-H (size)
  (declare (type fixnum size))
  (make-array `(1 ,size)
              :initial-contents
              `(,(loop for i below size
                     collect (if (= i 0) 1.0d0 0.0d0)))
              :element-type 'double-float))
(defun make-trend-Q (t^2)
  (make-array '(1 1) :initial-element (coerce t^2 'double-float)
              :element-type 'double-float))
(defun make-trend-R (v)
  (make-array '(1 1) :initial-element (coerce v 'double-float)
              :element-type 'double-float))

(defmethod trend ((d time-series-dataset)
                  &key (k 1) (t^2 0d0) (opt-t^2 nil) (s^2 1d0)
                       (delta 0.1d0) (search-width 10))
  (assert (numberp t^2))
  (when (< 1 (length (dataset-dimensions d))) (error "Trend model is for one-dimensional dataset."))
  (if opt-t^2
      (loop for i from (- search-width) to search-width
          as t^2-i = (let ((tt (+ t^2 (* delta i)))) (if (>= tt 0d0) tt 0d0))
          as m = (trend d :k k :t^2 t^2-i :opt-t^2 nil)
          as aic = (slot-value m 'aic)
          with min-aic = most-positive-double-float
          with model
          when (> min-aic aic)
          do (setq model m
                   min-aic aic)
          finally (return model))
    (let ((model
           (make-instance 'trend-model
             :diff-k k :tau^2 t^2 :observed-ts d
             :Q-matrices (make-trend-Q t^2)
             :R-matrices (make-trend-R s^2) ;; (ts-covariance d)
             :F-matrices (make-trend-F k)
             :G-matrices (make-trend-G k)
             :H-matrices (make-trend-H k))))
      (setf (slot-value model 'aic) (aic model))
      model)))

(defmethod x-00 ((model trend-model))
  (let* ((seq-without-nan (remove-if #'nan-p (map 'dvec (lambda (pt) (aref (ts-p-pos pt) 0)) 
                                                  (ts-points (observed-ts model)))))
         (mean (mean seq-without-nan))
         ;; (aref (ts-p-pos (aref (ts-points (observed-ts model)) 0)) 0)
         )
    (when (>= 2 (length seq-without-nan)) (error "Too many missing-values"))
    (make-dvec (diff-k model) mean)))

(defmethod v-00 ((model trend-model))
  (let* ((seq-without-nan (remove-if #'nan-p (map 'dvec (lambda (pt) (aref (ts-p-pos pt) 0)) 
                                                  (ts-points (observed-ts model)))))
         (1st-mom 0d0)
         (2nd-mom 0d0)
         (n (length seq-without-nan)))
    (declare (type double-float 1st-mom 2nd-mom) (type fixnum n))
    (when (>= 2 n) (error "Too many missing-values"))
    (do-vec (val seq-without-nan :type double-float)
      (incf 1st-mom val)
      (incf 2nd-mom (d-expt val 2d0)))
    (diag (diff-k model) (- (/ 2nd-mom n) (d-expt (/ 1st-mom n) 2d0)))))
(defmethod trend-prediction ((d time-series-dataset) 
                             &key (k 1) (t^2 0.1) (n-ahead 0)
                                  (delta 0.1d0) (search-width 10))
  (predict (trend d :k k :t^2 t^2 :delta delta :search-width search-width)
           :n-ahead n-ahead))


;;;;;;;;;;;;;;;;;;
; seasonal model ;
;;;;;;;;;;;;;;;;;;
(defclass seasonal-model (gaussian-stsp-model)
  ((s-deg  :initarg :s-deg :initform nil :type fixnum :accessor s-deg)
   (s-freq  :initarg :s-freq :initform nil :type fixnum :accessor s-freq)
   (tau^2 :initarg :tau^2 :initform nil :type number :accessor tau^2)))

(defun d_i (i deg freq)
  (declare (type fixnum i deg freq))
  (flet ((poly-multiplication (coef-ar1 coef-ar2)
           (declare (type (simple-array fixnum (*)) coef-ar1 coef-ar2))
           (make-array 
            (* (length coef-ar1) (length coef-ar2))
            :element-type 'fixnum
            :initial-contents
            (loop for coef1 of-type fixnum across coef-ar1
                append (loop for coef2 of-type fixnum across coef-ar2
                           collect (the fixnum (+ coef1 coef2)))))))
    (let ((ar1 (make-array freq :element-type 'fixnum
                           :initial-contents 
                           (loop for i of-type fixnum below freq collect i))))
      (if (eql deg 1) -1
        (loop with ar = ar1
            repeat (1- deg)
            do (setq ar (poly-multiplication ar ar1))
            finally (return (- (count i ar :test #'eql))))))))

(defun seasonal-mat-size (s-deg s-freq)
  (declare (type fixnum s-deg s-freq))
  (* s-deg (1- s-freq)))
  
(defun make-seasonal-F (s-deg s-freq)
  (declare (type fixnum s-deg s-freq))
  (let ((size (seasonal-mat-size s-deg s-freq)))
    (declare (type fixnum size))
    (make-array
     `(,size ,size)
     :initial-contents 
     `(,(loop for i from 1 to size
            collect (dfloat (d_i i s-deg s-freq)))
        ,@(loop for i below (1- size)
              collect 
                (let ((list (make-list size :initial-element 0d0)))
                  (setf (nth i list) 1d0) list)))
     :element-type 'double-float)))
(defun make-seasonal-G (s-deg s-freq)
  (declare (type fixnum s-deg s-freq))
  (let ((size (seasonal-mat-size s-deg s-freq)))
    (declare (type fixnum size))
    (make-trend-G size)))
(defun make-seasonal-H (s-deg s-freq)
  (declare (type fixnum s-deg s-freq))
  (let ((size (seasonal-mat-size s-deg s-freq)))
    (declare (type fixnum size))
    (make-trend-H size)))
(defun make-seasonal-Q (t^2)
  (make-array `(1 1) :initial-element (coerce t^2 'double-float)
              :element-type 'double-float))
(defun make-seasonal-R (v)
  (make-array `(1 1) :initial-element (coerce v 'double-float)
              :element-type 'double-float))
  
(defmethod seasonal ((d time-series-dataset)
                     &key (degree 1) freq (t^2 0d0) (s^2 1d0))
  (unless freq (setq freq (ts-freq d)))
  (assert (> freq 1))
  (make-instance 'seasonal-model
    :observed-ts d
    :s-deg degree :s-freq freq :tau^2 t^2
    :Q-matrices (make-seasonal-Q t^2)
    :R-matrices (make-seasonal-R s^2)   ; (aref (ts-covariance d) 0 0)
    :F-matrices (make-seasonal-F degree freq)
    :G-matrices (make-seasonal-G degree freq)
    :H-matrices (make-seasonal-H degree freq)))

(defmethod x-00 ((model seasonal-model))
  (make-dvec (seasonal-mat-size (s-deg model) (s-freq model))
             (dfloat (aref (ts-mean (observed-ts model)) 0))))

(defmethod v-00 ((model seasonal-model))
  (diag (seasonal-mat-size (s-deg model) (s-freq model))
        (dfloat (aref (ts-covariance (observed-ts model)) 0 0))))

(defmethod predict ((model seasonal-model) &key (n-ahead 0))
  (kalman-filter model)
  (multiple-value-bind (pos-list se-list)
      (forecast model n-ahead :smoothing t)
    (with-accessors ((ts observed-ts)) model
      (make-ts-by-forecast pos-list se-list ts :n-ahead n-ahead))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; seasonal-adjustment-model ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass seasonal-adjustment-model (gaussian-stsp-model)
  ((trend :initarg :trend :initform nil :type trend-model :accessor trend-model)
   (seasonal :initarg :seasonal :initform nil :type seasonal-model :accessor seasonal-model)))

(defmethod seasonal-adj ((d time-series-dataset)
                         &key (tr-k 1) (tr-t^2 0d0)
                              (s-deg 1) s-freq (s-t^2 0d0)
                              (s^2 1d0))
  (unless s-freq (setq s-freq (ts-freq d)))
  (let ((trend (trend d :k tr-k :t^2 tr-t^2))
        (seasonal (seasonal d :degree s-deg :freq s-freq :t^2 s-t^2)))
    (make-instance 'seasonal-adjustment-model
      :trend trend :seasonal seasonal :observed-ts d
      :Q-matrices (append-mat (slot-value trend 'Q-matrices)
                              (slot-value seasonal 'Q-matrices))
      :R-matrices (make-array '(1 1) :element-type 'double-float :initial-element s^2)
      :F-matrices (append-mat (slot-value trend 'F-matrices)
                              (slot-value seasonal 'F-matrices))
      :G-matrices (append-mat (slot-value trend 'G-matrices)
                              (slot-value seasonal 'G-matrices))
      :H-matrices (append-mat (slot-value trend 'H-matrices)
                              (slot-value seasonal 'H-matrices)
                              :direction :horizontal))))

(defmethod x-00 ((model seasonal-adjustment-model))
  (with-accessors ((tr trend-model)
                   (sea seasonal-model)) model
    (concatenate 'dvec (x-00 tr) (x-00 sea))))

(defmethod v-00 ((model seasonal-adjustment-model))
  (with-accessors ((tr trend-model)
                   (sea seasonal-model)) model
    (append-mat (v-00 tr) (v-00 sea))))


;;; extra: trend double!
(defclass double-trend (gaussian-stsp-model)
  ((trend1 :initarg :trend1 :initform nil :accessor trend1-model)
   (trend2 :initarg :trend2 :initform nil :accessor trend2-model)
   (seasonal :initarg :seasonal :initform nil :type seasonal-model :accessor seasonal-model)))

(defmethod double-trend-adj ((d time-series-dataset)
                             &key (tr1-k 1) (tr1-t^2 0d0)
                                  (tr2-k 1) (tr2-t^2 0d0)
                                  (s-deg 1) s-freq (s-t^2 0d0)
                                  (s^2 1d0))
  (unless s-freq (setq s-freq (ts-freq d)))
  (let ((trend1 (trend d :k tr1-k :t^2 tr1-t^2))
        (trend2 (trend d :k tr2-k :t^2 tr2-t^2))
        (seasonal (seasonal d :degree s-deg :freq s-freq :t^2 s-t^2)))
    (make-instance 'double-trend
      :trend1 trend1 :trend2 trend2 :seasonal seasonal :observed-ts d
      :Q-matrices (append-mat (slot-value trend1 'Q-matrices)
                              (append-mat (slot-value trend2 'Q-matrices)
                                          (slot-value seasonal 'Q-matrices)))
      :R-matrices (make-array '(1 1) :element-type 'double-float :initial-element s^2)
      :F-matrices (append-mat (slot-value trend1 'F-matrices)
                              (append-mat (slot-value trend2 'F-matrices)
                                          (slot-value seasonal 'F-matrices)))
      :G-matrices (append-mat (slot-value trend1 'G-matrices)
                              (append-mat (slot-value trend2 'G-matrices)
                                          (slot-value seasonal 'G-matrices)))
      :H-matrices (append-mat (slot-value trend1 'H-matrices)
                              (append-mat (slot-value trend2 'H-matrices)
                                          (slot-value seasonal 'H-matrices)
                                          :direction :horizontal)
                              :direction :horizontal))))

(defmethod x-00 ((model double-trend))
  (with-accessors ((tr1 trend1-model)
                   (tr2 trend2-model)
                   (sea seasonal-model)) model
    (concatenate 'dvec (x-00 tr1) (x-00 tr2) (x-00 sea))))

(defmethod v-00 ((model double-trend))
  (with-accessors ((tr1 trend1-model)
                   (tr2 trend2-model)
                   (sea seasonal-model)) model
    (append-mat (v-00 tr1) (append-mat (v-00 tr2) (v-00 sea)))))


  
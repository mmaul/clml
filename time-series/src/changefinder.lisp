;-*- coding: utf-8 -*-
;; ref. 山西健司 "データマイニングによる異常検知"


(in-package :clml.time-series.changefinder)

(defclass changefinder ()
  ((n-dim :initarg :n-dim :initform nil :accessor n-dim)
   (ts-model :initarg :ts-model :initform nil :accessor ts-model)
   (last-pt-stats :initarg :last-pt-stats :initform nil :accessor last-pt-stats)
   (pre-score-list :initarg :pre-score-list :initform nil :accessor pre-score-list)
   (score-model :initarg :score-model :initform nil :accessor score-model)
   (score-type :initarg :score-type :initform :log :accessor score-type) ;; :log | :hellinger
   (score-list :initarg :score-list :initform nil :accessor score-list)
   (last-qt-stats :initarg :last-qt-stats :initform nil :accessor last-qt-stats)
   (ts-wsize :initarg :ts-wsize :initform nil :accessor ts-wsize)
   (score-wsize :initarg :score-wsize :initform nil :accessor score-wsize)
   (discount :initarg :discount :initform nil :accessor discount))
  (:documentation "- accessors:
  - score-type : calculation method for change point score
  - ts-wsize : window size for 1st smoothing
  - score-wsize : window size for 2nd smoothing
  - discount : discounting parameter
"))

(defgeneric init-changefinder (ts &key)
  (:documentation "- return: <changefinder>
- arguments:
  - ts          : <time-series-dataset>
  - score-type  : :log | :hellinger, :log for logarithmic loss, :hellinger for hellinger distance
  - ts-wsize    : <positive integer>, window size for 1st smoothing
  - score-wsize : <positive integer>, window size for 2nd smoothing
  - sdar-k      : <positive integer>, degree for AR
  - discount    : 0 < <double-float> < 1, discounting parameter"))
(defmethod init-changefinder ((ts time-series-dataset)
                              &key (score-type :log) ;; :log | :hellinger
                                   (ts-wsize 5)
                                   (score-wsize 5)
                                   (sdar-k 4)
                                   (discount 0.005d0))
  (assert (>= (length (ts-points ts)) (+ ts-wsize score-wsize (* 2 sdar-k)))
      (ts ts-wsize score-wsize)
    "Number of points have to be more than ~D = ts-wsize + score-wsize + 2*sdar-k."
    (+ ts-wsize score-wsize (* 2 sdar-k)))
  (assert (< 0d0 discount 1d0))
  (let* ((dim (length (dataset-dimensions ts)))
         (ts-sdar (clml.time-series.autoregression::init-sdar ts :ar-k sdar-k))
         (last-pt-stats
          (loop for i below sdar-k
              as dvec = (ts-p-pos (aref (ts-points ts) i))
              do (clml.time-series.autoregression::update-xt-array ts-sdar dvec)
              finally (return (multiple-value-bind (mu s) (clml.time-series.autoregression::predict-sdar ts-sdar)
                                `(:pt-1 ,(multi-gaussian mu s) :pt nil)))))
         (cf (make-instance 'changefinder
               :n-dim dim :ts-model ts-sdar :last-pt-stats last-pt-stats
               :pre-score-list (make-list ts-wsize)
               :score-type score-type
               :score-list (make-list score-wsize)
               :discount discount
               :score-wsize score-wsize
               :ts-wsize ts-wsize)))

    (flet ((smoothing (data-list wsize)
             (loop for i from wsize to (length data-list)
                 as window = (subseq data-list (- i wsize) i)
                 collect (mean window))))

      (let* ((train-for-score-model
              (smoothing (map 'list (lambda (p)
                                      (update-ts-score cf (coerce (ts-p-pos p) '(simple-array double-float (*)))) )
                              (subseq (ts-points ts) sdar-k))
                         score-wsize))
             (ts (make-constant-time-series-data '("smthed-score")
                                                 (map 'vector (lambda (v) (make-dvec 1 v))
                                                      train-for-score-model)))
             (score-sdar (clml.time-series.autoregression::init-sdar ts :ar-k sdar-k))
             (last-qt-stats
              (loop for i below sdar-k
                  as dvec = (ts-p-pos (aref (ts-points ts) i))
                  do (clml.time-series.autoregression::update-xt-array score-sdar dvec)
                  finally (return (multiple-value-bind (mu s) (clml.time-series.autoregression::predict-sdar score-sdar)
                                    `(:pt-1 ,(multi-gaussian mu s) :pt nil))))))

        (setf (score-model cf) score-sdar
              (last-qt-stats cf) last-qt-stats)
        (loop for p across (subseq (ts-points ts) sdar-k) do
             (update-score cf (ts-p-pos p)))
        cf))))

;; online update changefinder
(defgeneric update-changefinder (cf new-dvec)
  (:documentation "- return: (values score score-before-smoothing)
- arguments:
  - cf       : <changefinder>, return value of #'init-changefinder
  - new-dvec : vector representing time series data point"))
(defmethod update-changefinder ((cf changefinder) new-dvec)
  (declare (type dvec new-dvec))
  (assert (eql (n-dim cf) (length new-dvec)) () "wrong dimension number")
  (update-ts-score cf new-dvec)
  (update-score cf (make-dvec 1 (mean (pre-score-list cf)))) ;; 1st smoothing
  (values (mean (score-list cf)) ;; 2nd smoothing
          (car (last (score-list cf)))))

(defun score-calculation (pt-stats dvec score-type)
  (declare (type dvec dvec))
  (destructuring-bind (&key pt-1 pt &allow-other-keys) pt-stats

    (ecase score-type
      (:log (- (log (density pt-1 dvec))))
      (:hellinger (hellinger-distance pt-1 pt)))))

(macrolet ((update-score (model stats score-list)
             `(progn
                (multiple-value-bind (new-mu new-sigma)
                    (clml.time-series.autoregression::update-sdar (,model cf) new-dvec :discount (discount cf))
                  (when (eq (score-type cf) :hellinger)
                    (setf (getf (,stats cf) :pt) (multi-gaussian new-mu new-sigma)))

                  (let ((score (score-calculation (,stats cf) new-dvec (score-type cf))))

                    (setf (,score-list cf) (append (cdr (,score-list cf)) (list score)))

                    (multiple-value-bind (new-mu new-sigma) (clml.time-series.autoregression::predict-sdar (,model cf))
                      (setf (getf (,stats cf) :pt-1) (multi-gaussian new-mu new-sigma)))
                    score)))))

  (defgeneric update-ts-score (cf new-dvec))
  (defmethod update-ts-score ((cf changefinder) new-dvec)
    (declare (type dvec new-dvec))
    (update-score ts-model last-pt-stats pre-score-list))

  (defgeneric update-score (cf new-dvec))
  (defmethod update-score ((cf changefinder) new-dvec)
    (declare (type (simple-array double-float (1)) new-dvec))
    (check-type new-dvec (simple-array double-float (1)))
    (update-score score-model last-qt-stats score-list)))


;; multi-dimensional gaussian mixture
(defclass multi-gaussian (clml.statistics::continuous-distribution)
  ((mean :initarg :mean :reader mean)
   (sigma :initarg :sigma :reader sigma)))
(defun multi-gaussian (mean sigma)
  (make-instance 'multi-gaussian :mean mean :sigma sigma))


(defmethod density ((d multi-gaussian) x)
  (multivariate-normal-density (mean d) (sigma d) (coerce x 'dvec)))

(defparameter *stabilizer* 1d-2)

(defun multivariate-normal-density (mu sigma vec &optional m)
  (declare (type dvec mu vec) (type dmat sigma))
  (setf mu (round-vec mu) vec (round-vec vec)
        sigma (mcm (round-mat sigma) (diag (array-dimension sigma 0) *stabilizer*) :c #'+))
  ;; (noise-on-diag (round-mat sigma) :order *stabilizer*)
  (let* ((dim (array-dimension sigma 0))
         (det (det sigma))
         (inv (if (>= 0d0 det)
                  (error "sigma must be positive definite: det = ~A" det) ;; positive definite check
                  (m^-1 sigma)))

         (coef (/ (* (expt (* 2d0 pi) (/ (if (numberp m) m dim) 2)) (sqrt det))))

         (in-exp (calc-in-exp inv mu vec))
         )
    (declare (type double-float coef in-exp))
    (* coef (handler-case (exp in-exp)
              (floating-point-underflow (c) (declare (ignore c))
                (warn "MND underflow: in-exp:~A" in-exp) least-positive-double-float)
              (floating-point-overflow (c) (declare (ignore c))
                (warn "MND overflow: in-exp:~A" in-exp) most-positive-double-float)))))



(defun calc-in-exp (inv-sigma mu vec)
  (let* ((dim (length mu))
         (x-m (vcv vec mu :c #'-))
         (x-mx-m (make-array `(,dim ,dim) :element-type 'double-float))
         (res (make-array `(,dim ,dim) :element-type 'double-float)))

    (loop for col below dim
       as val1 = (aref x-m col)
       do (loop for row below dim
              as val = (* val1 (aref x-m row))
              do (setf (aref x-mx-m col row) val)))
    #+mkl
    (mkl.blas:dgemm "N" "N" dim dim dim -0.5d0 inv-sigma dim x-mx-m dim 0d0 res dim)
    #-mkl
    (let ((a-inv-sigma (mat2array inv-sigma))
          (a-x-mx-m (mat2array x-mx-m ))
          (a-res (mat2array res))
          )
      (clml.blas:dgemm "N" "N" dim dim dim -0.5d0 a-inv-sigma dim a-x-mx-m dim 0d0 a-res dim)
      `(array2mat a-res ,dim res))
    (tr res)
    ))

;; hellinger score
(defgeneric hellinger-distance (pt-1 pt))
(defmethod hellinger-distance ((pt-1 multi-gaussian) (pt multi-gaussian))
  (flet ((safe-exp (d) (declare (type double-float d))
                   (handler-case (exp d)
                     (FLOATING-POINT-UNDERFLOW (c) (declare (ignore c)) 0d0)))
         (safe-det (mat) (declare (type dmat mat))
                   (let ((val (det mat)))
                     (cond ((< -1d-12 val 1d-12) 0d0)
                           ((minusp val) (error "~A is not positive definite." mat))
                           (t val))))
         (safe-log (val) (declare (type double-float val))
                   (if (zerop val) +-inf+ (log val))))
    (with-accessors ((m mean) (%%s sigma)) pt-1
      (with-accessors ((mm mean) (%%ss sigma)) pt
        (let* ((dim (length m))
               (%s (mcm %%s (diag dim *stabilizer*)))
               (%ss (mcm %%ss (diag dim *stabilizer*)))
               (s (m^-1 %s))
               (ss (m^-1 %ss))
               (s+ss (mcm s ss :c #'+))
               (sm (m*v s m))
               (ssmm (m*v ss mm)))
          (- 2d0 (safe-exp
                  (+ (+ (log 2d0) (* -0.5d0 (safe-log (safe-det (c*mat 0.5d0 (copy-mat s+ss))))))
                     (- (* 0.25d0 (log (safe-det %s))))
                     (- (* 0.25d0 (log (safe-det %ss))))
                     (* 0.5d0 (inner-product (vcv sm ssmm :c #'+)
                                             (m*v (m^-1 s+ss) (vcv sm ssmm :c #'+))))
                     (* -0.5d0 (+ (inner-product m sm)
                                  (inner-product mm ssmm)))))))))))
;; 値を丸める
(defun round-value (value &key (precision 1e-12))
  (dfloat (* precision (round value precision))))
;; ベクトルの各値を丸める
(defun round-vec (vec)
  (do-vec (val vec :type double-float :setf-var sf :return vec)
    (setf sf (round-value val))))
;; 行列の各値を丸める
(defun round-mat (mat &optional (precision 1e-12))
  (assert (> 1 precision))
  (loop for i below (array-dimension mat 0)
      do (loop for j below (array-dimension mat 1)
             as val = (round-value (aref mat i j) :precision precision)
             do (setf (aref mat i j) val))
      finally (return mat)))


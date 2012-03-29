(defpackage :ts-autoregression
  (:use :cl :read-data :util :vector :matrix :vars
        :statistics :ts-util :ts-stat :ts-read-data :ts-stsp)
  (:nicknames :ts-ar)
  (:export
   #:ar #:ar-prediction
   #:parcor #:parcor-filtering))

(in-package :ts-autoregression)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ar-model (1-dimensional) ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass ar-model (ts-stsp::gaussian-stsp-model)
  ((ar-coefficients :initarg :ar-coefficients 
                    :accessor ar-coefficients
                    :type list 
                    :initform '())
   (sigma^2 :initarg :sigma^2
            :accessor sigma^2
            :type number
            :initform 0.0d0)
   (aic :initarg :aic
        :type list
        :initform '())
   (demean :initarg :demean
           :accessor demean
           :initform nil)
   (ar-method :initarg :ar-method
           :accessor ar-method
           :type symbol
           :initform nil)))
(defmethod print-object ((model ar-model) stream)
  (with-accessors ((coefs ar-coefficients)
                   (method ar-method)
                   (s sigma^2)) model
    (let ((selected-order (car coefs)))
      (print-unreadable-object (model stream :type t :identity nil))
      (format stream "~&method: ~A~%" method)
      (format stream "~&Coefficients:~%")
      (loop for coef in (nth (car coefs) (cdr coefs))
          for i from 1
          do (format stream "a~D~T~F~%" i coef))
      (format stream "Order selected ~D~%~Tsigma^2 estimated as ~D~%~TAIC: ~D"
              selected-order s (nth selected-order (slot-value model 'aic))))))
(defmethod ts-stsp::x-00 ((model ar-model))
  (with-accessors ((ar-coefs ar-coefficients)
                   (ts ts-stsp::observed-ts)) model
    (make-array (car ar-coefs)
                :initial-element (aref (ts-mean ts) 0)
                :element-type 'double-float)))
(defmethod ts-stsp::v-00 ((model ar-model))
  (let ((var (aref (ts-covariance (ts-stsp::observed-ts model)) 0 0)))
    (diag (car (ar-coefficients model)) var)))
(defmethod aic ((model ar-model))
  (with-slots ((aic aic)) model
    (let ((min 0))
      ;;(apply #'min aic)))
      (princ (format nil "~&Order~TAIC~%"))
      (loop for val in (mapcar #'(lambda (val) (- val min)) aic)
          for i from 0
          do (princ (format nil "~D~T~F~%" i val))))))

(defun make-ar-model (ts ar-coefs s^2 aic demean method)
  (let* ((k (car ar-coefs))
         (ar-list (nth k (cdr ar-coefs))))
    (make-instance 'ar-model
      :observed-ts ts :ar-coefficients ar-coefs
      :sigma^2 s^2 :aic aic :demean demean :ar-method method
      :F-matrices
      (if (= k 0) (make-array '(0 0) :element-type 'double-float)
        (transpose 
         (make-array `(,k ,k)
                     :initial-contents
                     `(,ar-list
                       ,@(loop for i below (1- k)
                             collect 
                               (let ((list (make-list k :initial-element 0d0)))
                                 (setf (nth i list) 1d0) list)))
                     :element-type 'double-float)))
      :G-matrices 
      (if (= k 0) (make-array '(0 1) :element-type 'double-float)
        (make-array `(,k 1)
                    :initial-contents
                    (loop for i below k
                        collect `(,(if (= i 0) 1d0 0d0)))
                    :element-type 'double-float))
      :H-matrices
      (if (= k 0) (make-array '(1 0) :element-type 'double-float)
        (make-array `(1 ,k)
                    :initial-contents `(,(loop for i below k
                                             collect (if (= i 0) 1d0 0d0)))
                    :element-type 'double-float))
      :Q-matrices (ts-covariance ts)
      :R-matrices (diag 1 0d0))))

(defun calc-aic (cov num-of-coef num-of-data)
  (if (>= 0d0 cov)
      handling-missing-value:*-inf*
    (+ (* num-of-data (1+ (log (* 2 (coerce pi 'double-float) cov))))
       (* 2 (1+ num-of-coef)))))

(defun ^2 (x) (* x x))

(defmethod ar ((d time-series-dataset) 
               &key order-max
                    (demean t)
                    (method :burg) ; :yule-walker | :burg
                    (aic t)
                    )
  (with-accessors ((dims dataset-dimensions)
                   (ps ts-points)) d
  (assert (= 1 (length dims)))
  (unless order-max
    (setq order-max (if (>= 10 (length ps))
                        (1- (length ps))
                      (round (* 10 (log (length ps) 10))))))
  (assert (> (length ps) order-max))
  (if (= 0 (aref (ts-covariance d) 0 0))
      (make-ar-model (if demean (ts-demean d) d) (cons 0 '((0.0d0)))
                     0.0d0 '(0.0d0) (when demean (ts-mean d)) method)
    (case method
      (:yule-walker
       (ar-yw d :order-max order-max :demean demean :aic aic))
      (:burg
       (unless demean
         (print "Info: Demean is inevitable for burg."))
       (ar-burg d :order-max order-max :aic aic))
      (t)))))

(defmethod ar-burg ((d time-series-dataset) &key order-max aic)
  (let ((d-demean (ts-demean d)))
    (with-accessors ((dims dataset-dimensions)
                     (ps ts-points)) d-demean
      (assert (and (= 1 (length dims)) (> (length ps) order-max)))
      (loop 
          with num-of-data = (length ps)
          with val-vec = (map 'vector #'(lambda (p) (aref (ts-p-pos p) 0)) ps)
          with sigma^2-list = (cons (/ (loop for val across val-vec sum (^2 val)) num-of-data)
                                    (make-list order-max))
          with aic-list = (cons (calc-aic (first sigma^2-list) 0.0d0 num-of-data)
                                (make-list order-max))
          with coef-list = (cons '() (loop for i from 1 to order-max collect (make-list i)))
          with v-list = (cons (coerce val-vec 'list) 
                              (loop for i from 1 to order-max collect (make-list num-of-data)))
          with w-list = (cons (coerce val-vec 'list)
                              (loop for i from 1 to order-max collect (make-list num-of-data)))
          for m from 1 to order-max
          as m-1 = (1- m)
          as parcor = (setf (nth m-1 (nth m coef-list)) 
                        (* 2 
                           (loop for n from (1+ m) to num-of-data
                               sum (* (nth (1- n) (nth m-1 v-list)) (nth (- n m 1) (nth m-1 w-list))
                                      (/ (+ (loop for n from (1+ m) to num-of-data 
                                                sum (^2 (nth (- n m 1) (nth m-1 w-list))))
                                            (loop for n from (1+ m) to num-of-data 
                                                sum (^2 (nth (1- n) (nth m-1 v-list))))))))))
          do (loop for i from 1 to m-1
                 as j = (1- i)
                 do (setf (nth j (nth m coef-list))
                      (- (nth j (nth m-1 coef-list)) 
                         (* parcor (nth (1- (- m i)) (nth m-1 coef-list))))))
             (loop for n from (1+ m) to num-of-data
                 do (setf (nth (1- n) (nth m v-list)) 
                      (- (nth (1- n) (nth m-1 v-list))
                         (* parcor (nth (1- (- n m)) (nth m-1 w-list))))))                 
             (loop for n from (1+ m) to num-of-data
                 do (setf (nth (1- (- n m)) (nth m w-list))
                      (- (nth (1- (- n m)) (nth m-1 w-list))
                         (* parcor (nth (1- n) (nth m-1 v-list))))))
             (setf (nth m sigma^2-list) (max (* (nth m-1 sigma^2-list) (- 1 (^2 parcor)))
                                             0d0)
                   (nth m aic-list) (calc-aic (nth m sigma^2-list) m num-of-data))
          finally
            (return 
              (let ((pos (if aic 
                             (position (apply #'min aic-list) aic-list :test #'=)
                           order-max)))
                (make-ar-model d-demean (cons pos coef-list) (nth pos sigma^2-list)
                               aic-list (ts-mean d) :burg)))))))


;;; return: (values coef-list sigma^2-list aic-list)
(defun levinson-algorithm (num-of-data cov-list order-max)
  (assert (and (>= order-max 1) (> (length cov-list) order-max)))
  (loop 
      with sigma^2-list = (cons (first cov-list) (make-list order-max))
      with aic-list = (cons (calc-aic (first cov-list) 0.0d0 num-of-data)
                            (make-list order-max))
      with coef-list = (cons '() (make-list order-max))
      for m from 1 to order-max
      do 
    (setf (nth m coef-list) 
      (loop for i from 1 to (- m 1)
          with amm = (/ (- (nth m cov-list)
                           (loop for j from 0 to (- m 2)
                               sum (* (nth j (nth (1- m) coef-list))
                                      (nth (- m (1+ j)) cov-list))))
                        (nth (1- m) sigma^2-list))
          with coefs = (append (make-list (1- m)) `(,amm))
          do (setf (nth (1- i) coefs)
               (- (nth (1- i) (nth (1- m) coef-list))
                  (* amm (nth (1- (- m i)) (nth (1- m) coef-list)))))
          finally (return coefs))
      (nth m sigma^2-list) (* (nth (1- m) sigma^2-list) 
                              (- 1 (expt (nth (1- m) (nth m coef-list)) 2)))
      (nth m aic-list) (calc-aic (nth m sigma^2-list) m num-of-data))
      finally
        (return (values coef-list sigma^2-list aic-list))))

(defmethod ar-yw ((d time-series-dataset) &key order-max demean aic)
  (let ((d-demean (if demean (ts-demean d) d)))
    (with-accessors ((dims dataset-dimensions)
                     (ps ts-points)) d-demean
      (assert (and (= 1 (length dims)) (> (length ps) order-max)))
      (let ((num-of-data (length ps))
            (cov-list (loop for m to order-max collect
                            (aref (ts-covariance d :k m :demean demean) 0 0))))
        (multiple-value-bind (coef-list sigma^2-list aic-list)
            (levinson-algorithm num-of-data cov-list order-max)
          (let ((pos (if aic 
                         (position (apply #'min aic-list) aic-list :test #'=)
                       order-max)))
            (make-ar-model d-demean (cons pos coef-list) 
                           (* (nth pos sigma^2-list) 
                              (/ num-of-data (- num-of-data (1+ (length (nth pos coef-list))))))
          ;;; consideration for freedom
                           aic-list (when demean (ts-mean d)) :yule-walker)))))))

(defmethod predict ((model ar-model) &key (n-ahead 0))
  (assert (not (minusp n-ahead)))
  (with-accessors ((ts ts-stsp::observed-ts) (demean demean) 
                   (ar-c ar-coefficients)) model
    (let* ((n (length (ts-points ts)))
           (pos-list (make-list (+ n n-ahead)
                      :initial-element 
                      (if demean (make-dvec (length (dataset-dimensions ts)) 0d0)
                        (ts-mean ts))))
           (se-list (make-list (+ n n-ahead)
                               :initial-element
                               (make-array `(,(length (dataset-dimensions ts)) 1)
                                           :initial-element (sqrt (sigma^2 model))
                                           :element-type 'double-float))))
      (unless (= 0 (car ar-c))
        (ts-stsp::kalman-filter model)
        (multiple-value-setq (pos-list se-list)
          (ts-state-space::forecast model n-ahead)))
      (let* ((start (tf-incl (ts-start ts) (car ar-c) :freq (ts-freq ts)))
             (end (tf-incl (ts-end ts) n-ahead :freq (ts-freq ts)))
             (time-labels
              (subseq 
               (concatenate 'vector
                 (map 'vector #'ts-p-label (ts-points ts))
                 (make-array n-ahead :initial-element "predicted" 
                             :element-type 'string))
               (car ar-c)))
             (pos-list (subseq pos-list (car ar-c)))
             (se-list (subseq se-list (car ar-c))))
        (values
         (make-constant-time-series-data
          (map 'list #'dimension-name (dataset-dimensions ts))
          (if demean 
              (map 'vector #'(lambda (v)
                               (vcv v demean)) pos-list)
            (coerce pos-list 'vector))
          :time-label-name (time-label-name ts)
          :time-labels time-labels
          :start start :end end :freq (ts-freq ts))
         (make-constant-time-series-data
          '("standard error")
          (coerce (mapcar #'(lambda (mat) (matrix::mat2array mat)) se-list)
                  'vector)
          :time-label-name (time-label-name ts)
          :time-labels time-labels
          :start start :end end :freq (ts-freq ts)))))))

(defmethod ar-prediction ((d time-series-dataset) &key 
                                                  (method :yule-walker) ; :ols :burg
                                                  (aic t) order-max
                                                  n-learning
                                                  (n-ahead 0)
                                                  (demean t)
                                                  target-col)
  (assert (integerp n-ahead))
  (if n-learning
      (setq n-learning (min (length (ts-points d)) n-learning))
    (setq n-learning (length (ts-points d))))
  (let* ((d (if target-col
                (let ((pos (position target-col (dataset-dimensions d)
                                     :test #'string-equal
                                     :key #'dimension-name)))
                  (if pos (sub-ts d :range `(,pos) 
                                  :end (tf-incl (ts-start d) n-learning :freq (ts-freq d)))
                    (error "Does not exist column ~A" target-col)))
              (progn (assert (= 1 (length (dataset-dimensions d)))) 
                     (sub-ts d :end (tf-incl (ts-start d) n-learning :freq (ts-freq d))))))
         (model (ar d :order-max order-max :demean demean :aic aic :method method)))
    (multiple-value-bind (pred se)
        (predict model :n-ahead n-ahead)
      (values pred model se))))

(defmethod parcor ((ts time-series-dataset) &key (order 1) (method :burg)
                                                 ppm-file)
  (assert (integerp order))
  (loop 
      with hash = (make-hash-table :test #'equal)
      with colnames = (map 'vector #'dimension-name (dataset-dimensions ts))
      for colname across colnames
      for i from 0
      as subts = (sub-ts ts :range `(,i))
      as model = (ar subts :order-max order :aic nil :method method)
      do 
    (with-accessors ((coefs ar-coefficients)) model
      (let ((parcor-list 
             (map 'list #'(lambda (list) (car (last list))) (cdr coefs))))
        (setf (gethash colname hash) 
          (or (nth order parcor-list) (nth 0 parcor-list)))))
      finally (return 
                (progn (when ppm-file
                         (draw-ppm `(,(loop for colname across colnames
                                          collect (gethash colname hash)))
                                   ppm-file))
                       hash))))

(defun parcor-stat (tss &key (parcor-order 1) (ar-method :burg) 
                    (print-stat nil) (ppm-file nil) (ppm-height-unit 10))
  (loop 
      with hash = (make-hash-table :test #'equal)
      with colnames = 
        (coerce (loop with colnames
         for ts across (coerce tss 'vector)
         as cols = (map 'list #'dimension-name (dataset-dimensions ts))
         do (setq colnames (if colnames cols (union colnames cols :test #'string-equal)))
         finally (return colnames)) 'vector)
      for ts across (coerce tss 'vector)
      for i from 1
      when ts
      do (loop with sub-hash = (parcor ts :order parcor-order :method ar-method)
             for colname across colnames
             as p = (gethash colname sub-hash)
             do (push p (gethash colname hash)))
      finally (return (progn
      (when print-stat
        (maphash #'(lambda (colname vals)
          (let* ((vals (remove nil vals))
                 (mn (when vals (/ (apply #'+ vals) (length vals))))
                 (std (when vals (sqrt (/ (apply #'+ 
                                                 (map 'list #'(lambda (val) (expt (- val mn) 2)) vals))
                                          (coerce (length vals) 'double-float))))))

            (princ
             (format nil "~%~A (~A): MIN ~A | MAX ~A | MEAN ~A | STD ~A"
                     colname (length vals)
                     (when vals (apply #'min vals))
                     (when vals (apply #'max vals))
                     mn std))))
                 hash))
      (let ((data (loop for colname across colnames
             collect (cons colname (reverse (gethash colname hash))))))
        (when ppm-file
          (draw-ppm (loop with val-list = (mapcar #'cdr data)
                        for i below (length (car val-list))
                        collect (mapcar #'(lambda (lis) 
                                 (let ((val (nth i lis)))
                                   (if val (abs val) val))) val-list))
                    ppm-file :height-unit ppm-height-unit))
        data)))))

(defmethod parcor-filtering ((ts time-series-dataset)
                             &key (divide-length 15) 
                                  (parcor-order 1)
                                  (n-ahead 10)
                                  ppm-fname)
  (assert (and (< parcor-order divide-length) (<= divide-length (length (ts-points ts)))))
  (let* ((sub-tss 
          (loop for i from 1
              as start = (tf-incl (ts-start ts) (* (1- i) divide-length) :freq (ts-freq ts))
              as end = (tf-incl (ts-start ts) (1- (* i divide-length)) :freq (ts-freq ts))
              while (>= (tf-gap start (ts-end ts) :freq (ts-freq ts)) 0)
              as subts = (sub-ts ts :start start :end end)
              when (= (length (ts-points subts)) divide-length)
              collect subts))
         (wide-stat
          (parcor-stat sub-tss :parcor-order parcor-order :ar-method :burg))
         (wide-parcor
          (make-constant-time-series-data 
           (mapcar #'car wide-stat)
           (transposeV (coerce
                        (loop for data in (mapcar #'cdr wide-stat)
                            collect (specialize-vec (map 'vector #'abs data)))
                        'vector))
           :time-labels
           (coerce (loop for ts in sub-tss
                       as ps = (ts-points ts)
                       collect (format nil "~A - ~A" 
                                       (ts-p-label (svref ps 0))
                                       (ts-p-label (svref ps (1- (length ps))))))
                   'vector)
           :time-label-name (time-label-name ts))))
    ;; predict parcor
    (with-accessors ((dims dataset-dimensions)) wide-parcor
      (let ((data
             (transposeV (coerce
                          (loop for colname across (map 'vector #'dimension-name dims)
                              for i across (map 'vector #'dimension-index dims)
                              as subts = (sub-ts wide-parcor :range `(,i))
                              as model = (ar subts :method :burg)
                              as pred = (predict model :n-ahead n-ahead)
                              collect
                                (specialize-vec
                                 (concatenate 'vector
                                   (map 'vector #'(lambda (s) (aref (ts-p-pos s) 0)) (ts-points subts))
                                   (when (plusp n-ahead)
                                     (map 'vector #'(lambda (s) (aref (ts-p-pos s) 0))
                                          (ts-points (sub-ts pred :start (tf-incl (ts-end subts) 1))))))))
                          'vector))))
        (when ppm-fname
          (draw-ppm
           (map 'list #'(lambda (v) (coerce v 'list)) data) ppm-fname))
        (make-constant-time-series-data 
         (mapcar #'car wide-stat) data
         :time-labels (concatenate 'vector (map 'vector #'ts-p-label (ts-points wide-parcor))
                                   (make-array n-ahead :initial-element "prediction"
                                               :element-type 'string))
         :time-label-name (time-label-name ts))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; オンライン忘却型多変量AR(SequencialDiscountingAR)
;;; d次元, k次のARモデルとする

;; μを更新
;; μ = (1-r)*μ + r*x_t
;; 引数 meu(d次元ベクトル)
;;       xt(d次元ベクトル)
;;        r(スカラー)
;; 返り値 meu(d次元ベクトル)
(defun update-meu (meu xt r)
  (declare (type dvec xt meu)
           (type double-float r)
           (optimize speed))
  (assert (< 0.0d0 r 1.0d0))
  (assert (= (length xt) (length meu)))
  (loop 
      with 1-r = (- 1.0d0 r)
      for i below (length meu)
      do (setf (aref meu i)
           (+ (* 1-r (aref meu i)) (* r (aref xt i))))
      finally (return meu)))

;; ベクトルの引き算
(defun dvec-subtract (dvec-a dvec-b)
  #+ignore
  (declare (type dvec dvec-a dvec-b)
           (optimize speed))
  (assert (= (length dvec-a) (length dvec-b)))
  (loop with result = (make-dvec (length dvec-a) 0.0d0)
      for a across dvec-a
      for b across dvec-b
      for i from 0
      do (setf (aref result i) (- a b))
      finally (return result)))

;; ベクトルの足し算
(defun dvec-sum (dvec-a dvec-b)
  (declare (type dvec dvec-a dvec-b)
           (optimize speed))
  (assert (= (length dvec-a) (length dvec-b)))
  (loop with result = (make-dvec (length dvec-a) 0.0d0)
      for a across dvec-a
      for b across dvec-b
      for i from 0
      do (setf (aref result i) (+ a b))
      finally (return result)))

;; ベクトルの定数倍
(defun c*dvec (c dvec)
  (declare (type dvec dvec)
           (type double-float c)
           (optimize speed))
  (loop with result = (make-dvec (length dvec) 0.0d0)
      for a across dvec
      for i from 0
      do (setf (aref result i) (* a c))
      finally (return result)))

;; 行列の足し算
(defun dmat-sum (dmat-a dmat-b)
  (declare (type dmat dmat-a dmat-b)
           (optimize speed))
  (assert (equal (array-dimensions dmat-a)
                 (array-dimensions dmat-b)))
  (let ((result (make-dmat (array-dimension dmat-a 0) (array-dimension dmat-a 1))))
    (loop for i from 0 to (1- (array-dimension dmat-a 0))
        do (loop for j from 0 to (1- (array-dimension dmat-a 1))
               do (setf (aref result i j)
                    (+ (aref dmat-a i j)
                       (aref dmat-b i j)))))
    result))

;; 行列の引き算
(defun dmat-subtract (dmat-a dmat-b)
  (declare (type dmat dmat-a dmat-b)
           (optimize speed))
  (assert (equal (array-dimensions dmat-a)
                 (array-dimensions dmat-b)))
  (let ((result (make-dmat (array-dimension dmat-a 0) (array-dimension dmat-a 1))))
    (loop for i from 0 to (1- (array-dimension dmat-a 0))
        do (loop for j from 0 to (1- (array-dimension dmat-a 1))
               do (setf (aref result i j)
                    (- (aref dmat-a i j)
                       (aref dmat-b i j)))))
    result))

;; vecA(vecB)^T
(defun dvec-cov (dvec-a dvec-b)
  (declare (type dvec dvec-a dvec-b)
           (optimize speed))
  (let ((dim1 (length dvec-a))
        (dim2 (length dvec-b)))
    (make-array (list dim1 dim2)
                :element-type 'double-float
                :initial-contents
                (loop for i from 0 to (1- dim1)
                    collect (loop for j from 0 to (1- dim2)
                                collect (* (aref dvec-a i) (aref dvec-b j)))))))

;; C_jを更新する
;; C_j = (1 - r)C_j + r(x_t-μ)(x_{t-j}-μ)^T
;; 現状は新しいdmatが返える(引数のcjは更新されない)
;; 引数 C_j(dxdの行列)
;;      meu(d次元ベクトル)
;;      xt(d次元ベクトル)
;;      xt-j(d次元ベクトル)
;;      r(スカラー)
(defun update-cj (cj meu xt xt-j r)
  (declare (type dmat cj)
           (type dvec meu xt xt-j)
           (type double-float r)
           (optimize speed))
  (assert (< 0.0d0 r 1.0d0))
  (assert (= (length meu) (length xt) (length xt-j)))
  (dmat-sum
   (c*mat (- 1.0d0 r) cj)
   (c*mat r
          (dvec-cov
           (dvec-subtract xt meu)
           (dvec-subtract xt-j meu)))))

;; x_tの予測値を計算する
;; xhat_t = (\sigma_{i=1}^{i=k} {w_i(x_{t-i}-μ)}) + μ
;; 引数 w-vec (k個の要素からなる配列 #(w1 w2 w3 ... wk))
;;            w1,w2,w3,...,wkはdxdの行列
;;      xt-vec (k個の要素からなる配列 #(x_{t-k} x_{t-k+1} x_{t-k+2} ... x_{t-1});;            x_{t-k},x_{t-k+1},...,x_{t-1}はd次元ベクトル
;;      meu (d次元ベクトル)
(defun calculate-xhat (w-vec xt-vec meu)
  (declare (type dvec meu)
           (optimize speed))
  (dvec-sum
   (reduce #'dvec-sum
           (loop for wi across w-vec
               for xt-i across (reverse xt-vec)
               collect (m*v wi (dvec-subtract xt-i meu))))
   meu))

;; Σを更新する
;; Σ = (1-r)Σ+r(x_t - xhat_t)(x_t - xhat_t)^T
;; 現状は新しいdmatが返える(引数のsigmaは更新されない)
;; 引数 sigma(dxdの行列)
;;      xt(d次元ベクトル)
;;      xhatt(d次元ベクトル)
;;      r(スカラー)
(defun update-sigma (sigma xt xhatt r)
  (declare (type dmat sigma)
           (type double-float r)
           (type dvec xt)
           (type dvec xhatt)
           (optimize speed))
  (dmat-sum
   (c*mat (- 1.0d0 r) sigma)
   (c*mat r
          (dvec-cov
           (dvec-subtract xt xhatt)
           (dvec-subtract xt xhatt)))))

;;;; Solve Yule-Walker Eqn using Levinson Algolism
;;;; C_k = \sigma_{j=1}^{j=m}{A_j^m C_{k-j}}
;;;; Ref: http://sbsjp.nips.ac.jp/docs/brain1004.pdf

(defclass levinson-obj ()
  ((v :accessor lev-v :initarg :v :initform nil) ; V_m
   (u :accessor lev-u :initarg :u :initform nil) ; U_m
   (w :accessor lev-w :initform nil)    ; W_m
   (a :accessor lev-a :initform (make-hash-table :test #'equal)) ; A_i^m key "i-m" value A_i^m
   (b :accessor lev-b :initform (make-hash-table :test #'equal)) ; B_i^m key "i-m" value B_i^m
   (aic :accessor lev-aic :initarg :aic :initform nil) ; AIC_m
   ))

;; cj-vec #(c0 c1 c2 c3 ... cp)
;; c0,c1,c2,c3,...,cpはdxdの行列
;; n データ数
;; 返り値 A_j^m (dxdの行列がp個ある配列), levinson-obj(参考用)
(defun multivariate-levinsion (cj-vec n &key (calc-aic t))
  (flet ((make-key (i m)
           (format nil "~a-~a" i m))
         (calc-aic (cov d m)
           (let ((d-cov (det cov)))
             (cond ((>= 0 d-cov) handling-missing-value:*nan*)
                   ((and (minusp d-cov) (> (abs d-cov) *epsilon*))
                    (error "Covariance must be positive-definite matrix ~A" cov))
                   (t (+ (* n (+ (* d (log (* 2 pi))) (log d-cov) d))
                         (* d (1+ d))
                         (* 2 (* d d m)))))))
         (%m^-1 (mat)
           ;; round
           (loop with precision = 1d-8
               for i below (array-dimension mat 0)
               do (loop for j below (array-dimension mat 1)
                      as val = (dfloat (* precision (round (aref mat i j) precision)))
                      do (setf (aref mat i j) val)))
           (m^-1 (if (> *epsilon* (abs (det mat)))
                     ;; regularize
                     (loop with %mat = (copy-mat mat)
                         with alpha = 1d-2
                         for i below (array-dimension mat 0)
                         as val = (aref %mat i i)
                         do (setf (aref %mat i i) 
                              (if (minusp val) (- val alpha) (+ val alpha)))
                         finally (return %mat))
                   mat))))
    (let* ((d (array-dimension (aref cj-vec 0) 0)) ; data dimension
           (p (1- (length cj-vec)))
           (obj (make-instance 'levinson-obj
                  :v (aref cj-vec 0)    ;c0
                  :u (aref cj-vec 0)    ;c0
                  :aic (when calc-aic (calc-aic (aref cj-vec 0) d 0)))))
      (loop for m from 1 to p do 
            (setf (lev-w obj)
              (if (= m 1)
                  (aref cj-vec m)
                                        ; W_m = C_m - \sigma_{i=1}^{m-1}{A_i^{m-1}C_{m-i}}
                (dmat-subtract
                 (aref cj-vec m)
                 (reduce #'dmat-sum
                         (loop for i from 1 to (- m 1)
                             collect (m*m (gethash (make-key i (- m 1)) (lev-a obj))
                                          (aref cj-vec (- m i))))))))

                                        ; A_m^m = W_m inv(U_{m-1})
            (setf (gethash (make-key m m) (lev-a obj))
              (m*m (lev-w obj)
                   (%m^-1 (lev-u obj))))
                                        ; B_m^m = transpose(W_m) inv(V_{m-1})

            (setf (gethash (make-key m m) (lev-b obj))
              (m*m (transpose (lev-w obj))
                   (%m^-1 (lev-v obj))))

            (loop for i from 1 to (- m 1)
                do                      ; A_i^m = A_i^{m-1} - A_m^m B_{m-i}^{m-1}
                  (setf (gethash (make-key i m) (lev-a obj))
                    (dmat-subtract 
                     (gethash (make-key i (1- m)) (lev-a obj))
                     (m*m (gethash (make-key m m) (lev-a obj))
                          (gethash (make-key (- m i) (- m 1))
                                   (lev-b obj)))))
                                        ; B_i^m = B_i^{m-1} - B_m^m A_{m-i}^{m-1}
                  (setf (gethash (make-key i m) (lev-b obj))
                    (dmat-subtract 
                     (gethash (make-key i (1- m)) (lev-b obj))
                     (m*m (gethash (make-key m m) (lev-b obj))
                          (gethash (make-key (- m i) (- m 1))
                                   (lev-a obj))))))
                                        ; V_m = C_0- \sigma_{i=1}^m A_i^m transpose(C_i)
            (setf (lev-v obj)
              (dmat-subtract 
               (aref cj-vec 0)
               (reduce #'dmat-sum
                       (loop for i from 1 to m
                           collect (m*m (gethash (make-key i m) (lev-a obj))
                                        (transpose (aref cj-vec i)))))))
                                        ; U_m = C_0- \sigma_{i=1}^m B_i^m C_i
            (setf (lev-u obj)
              (dmat-subtract 
               (aref cj-vec 0)
               (reduce #'dmat-sum
                       (loop for i from 1 to m
                           collect (m*m (gethash (make-key i m) (lev-b obj))
                                        (aref cj-vec i))))))
                                        ; AIC_m = N(dlog2pi+log|V_m|+k) + k(k+1) + 2K^2m
            (setf (lev-aic obj) (when calc-aic (calc-aic (lev-v obj) d m))))
                                        ; A_i^p(i = 1,...,p)がYule-Walker方程式の答え
      (values 
       (coerce 
        (loop for i from 1 to p
            collect (gethash (make-key i p) (lev-a obj)))
        'array)
       obj))))


;; データxがd次元でK次のARモデルを仮定する
;; meu(d次元のベクトル)
;; cj-array #(C_0 C_1 C_2 C_3 ... C_k) k+1個の要素からなるarray
;; (C_0 C_1,C_2,C-3,...,C_kは dxdの行列)
;; sigma(dxdの行列)
;; old-xt-array #(x_{t-k} x_{t-k+1} x_{t-k+2} ... x_{t-1}) k個の要素からなるarray
;; (x_{t-k},x_{t-k+1},x_{t-k+2},...,x_{t-1}はd次元のベクトル)
;; new-xt d次元のベクトル
;; r スカラー(忘却parameter)
;; n スカラー(データ数)
(defun 1step-sdar (meu cj-array sigma old-xt-array new-xt r n)
  (update-meu meu new-xt r)
  (let* ((k (length old-xt-array))
         (new-cj-array
          (coerce 
           (loop for j below (length cj-array)
               collect (update-cj (aref cj-array j)
                                  meu new-xt
                                  (if (= j 0)
                                      new-xt
                                    (aref old-xt-array (- k j)))
                                  r))
           'array))
         (w-vec (multivariate-levinsion new-cj-array n :calc-aic nil)) ; k個のdxdの行列からなる配列
         (xhatt (calculate-xhat w-vec old-xt-array meu))
         (new-sigma (update-sigma sigma new-xt xhatt r)))
    ;; old-xt-array更新
    (loop for i from 0 to (- k 2)
        do (setf (aref old-xt-array i)
             (aref old-xt-array (1+ i)))
        finally (setf (aref old-xt-array (1- k)) new-xt))
    (list xhatt
          meu
          new-cj-array
          new-sigma
          old-xt-array
          w-vec)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; class for SDAR (Sequential Discounting Auto Regression) ;
; ref. The book of Yamanishi                              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass sdar ()
  ((coef-vec :initarg :coef-vec :initform nil :accessor coef-vec)
   (mu :initarg :mu :initform nil :accessor mu)
   (sigma :initarg :sigma :initform nil :accessor sigma)
   (cj-array :initarg :cj-array :initform nil :accessor cj-array)
   (xt-array :initarg :xt-array :initform nil :accessor xt-array)
   (n :initarg :n :initform nil :accessor n)))
(defmethod print-object ((sdar sdar) stream)
  (print-unreadable-object (sdar stream :type t :identity nil)
    (format stream "(~A)" (length (coef-vec sdar)))))
(defmethod init-sdar ((ts time-series-dataset) 
                      &key (ar-k nil)) ;; AR次数、nilならAICによる自動選択
  (with-accessors ((dims dataset-dimensions)
                   (ps ts-points)) ts
    (let ((len (length ps))
          (mu (ts-mean ts)))
      (if (numberp ar-k)
          (let ((cj-vec (coerce (loop for %k to ar-k collect (ts-covariance ts :k %k)) 'vector)))
            (multiple-value-bind (coef-vec lev-obj) (multivariate-levinsion cj-vec len)
              (make-instance 'sdar
                :coef-vec coef-vec :n len
                :mu mu :sigma (lev-v lev-obj) :cj-array cj-vec
                :xt-array (map 'vector #'ts-p-pos (subseq ps (- len ar-k))))))
        (progn 
          (setq ar-k (cond ((>= 5 len) (error "Data is too short to estimate VAR."))
                           ((>= 10 len) 3)
                           (t (round (* 10 (log len 10))))))
          (loop with min-aic = most-positive-double-float
              with cj-array = nil
              with coef-vec = nil
              with cov = nil
              with all-cj-vec = 
                (coerce (loop for %k to ar-k collect (ts-covariance ts :k %k)) 'vector)
              for k from 1 to ar-k
              as cj-vec = (subseq all-cj-vec 0 (1+ k))
              as (%coef-vec lev-obj) = (multiple-value-list
                                        (multivariate-levinsion cj-vec len))
              as aic = (lev-aic lev-obj)
              when (and (not (handling-missing-value:nan-p aic))
                        (> min-aic (lev-aic lev-obj))) do
                (setf min-aic (lev-aic lev-obj)
                      coef-vec %coef-vec
                      cov (lev-v lev-obj)
                      cj-array cj-vec)
              finally (return 
                        (make-instance 'sdar
                          :coef-vec coef-vec :mu mu :sigma cov :cj-array cj-array
                          :xt-array (map 'vector #'ts-p-pos 
                                         (subseq ps (- len (length coef-vec))))
                          :n len))))))))
(defmethod update-sdar ((sdar sdar) new-xt &key (discount 0.01d0))
  (destructuring-bind (xhatt meu new-cj-array new-sigma xt-array w-vec)
      (1step-sdar (mu sdar) (cj-array sdar) (sigma sdar) (xt-array sdar) new-xt discount (n sdar))
    (setf (mu sdar) meu
          (cj-array sdar) new-cj-array
          (sigma sdar) new-sigma
          (xt-array sdar) xt-array
          (coef-vec sdar) w-vec)
    (values xhatt new-sigma)))
(defmethod update-xt-array ((sdar sdar) new-xt)
  (with-accessors ((old-xt-array xt-array)) sdar
    (let ((k (length (coef-vec sdar))))
      (loop for i from 0 to (- k 2)
          do (setf (aref old-xt-array i)
               (aref old-xt-array (1+ i)))
          finally (setf (aref old-xt-array (1- k)) new-xt)))))
(defmethod predict-sdar ((sdar sdar))
  (values (calculate-xhat (coef-vec sdar) (xt-array sdar) (mu sdar))
          (sigma sdar)))

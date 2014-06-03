(defpackage :ts-anomaly-detection
  (:use :cl :excl :vars :util :vector :matrix :statistics :read-data
        :missing-val :csv :ts-read-data :ts-stat :ts-util :ts-state-space :ts-ar)
  (:export :make-db-detector
           :make-periodic-detector
           :make-eec-detector
           :make-snn
           :e-scores))

(in-package :ts-anomaly-detection)

;; make Direction-based anomaly detector
;; ref: T.Ide and H.Kashima "Eigenspace-based Anomaly Detection in Computer Systems" sec.5
(defmethod make-db-detector ((ts time-series-dataset)
                             &key beta (typical :svd) (pc 0.005d0) (normalize t))
  (assert (< 0d0 pc 1d0) (pc))
  (let* ((dim (length (dataset-dimensions ts)))
         (vecs (map 'list
                 (lambda (p) (let ((vec (ts-p-pos p)))
                               (if normalize (normalize-vec vec (copy-seq vec)) vec)))
                 (ts-points ts)))
         (moments (get-initial-moments vecs :typical-method typical)))
    (unless beta (setf beta (dfloat (/ (length vecs)))))
    (lambda (new-dvec)
      (assert (eql (length new-dvec) dim))
      (let* ((vec (if normalize (normalize-vec new-dvec (copy-seq new-dvec)) new-dvec))
             (typ (calc-typical-pattern vecs :method typical))
             (score (dissimilarity typ vec)))
        (setf moments (next-moments score moments beta)
              vecs (append (cdr vecs) (list vec)))
        (values score (multiple-value-bind (n-1 sigma)
                          (estimate-vmf-parameters moments)
                        (vmf-threshold n-1 sigma pc))
                typ)))))
;; make periodic anomaly detector
;; ref: 山西健司「データマイニングによる異常検知」
(defmethod make-periodic-detector ((ts time-series-dataset) &key (r 0.5d0))
  (let* ((freq (ts-freq ts))
         (dim (length (dataset-dimensions ts)))
         (vecs (map 'list #'ts-p-pos (ts-points ts)))
         (pwindow (init-period-window freq dim vecs :r r)))
    (lambda (new-dvec)
      (assert (eql (length new-dvec) dim))
      (prog1 (score-by-mahalanobis new-dvec pwindow)
        (update-pwindow new-dvec pwindow r)))))

;; Stochastic-Nearest-Neighbors
;; ref:
;; T.Ide, S.Papadimitriou, M.Vlachos
;; "Computing Correlation Anomaly Scores using Stochastic Nearest Neighbors"
(defclass snn ()
  ((names :initarg :names :accessor names)
   (snn-k :initarg :snn-k :accessor snn-k)
   (sigma-i :initarg :sigma-i :accessor sigma-i)
   (graphs  :initarg :graphs  :accessor graphs)))
(defmethod make-snn ((ts time-series-dataset) k &key (sigma-i 1d0))
  (assert (and (numberp k)))
  (let* ((wmat (dissimilarity-matrix (map 'vector #'ts-p-pos (ts-points ts))))
         (names (map 'list #'dimension-name (dataset-dimensions ts)))
         (size (length names))
         (%nbrs-list (knn-list wmat k))
         (nbrs-list (coupling-probabilities %nbrs-list sigma-i))
         (graphs (loop for i below size
                     for nbrs in nbrs-list
                     collect (cons i nbrs))))
    (values (make-instance 'snn :names names :snn-k k :sigma-i sigma-i :graphs graphs)
            wmat)))
(defmethod e-scores ((target-model snn) (ref-model snn))
  (let* ((target-names (names target-model))
         (ref-names (names ref-model))
         (names (sort (copy-list (union target-names ref-names :test #'equal)) #'string<)))
    (assert (and (null (set-difference names target-names :test #'equal))
                 (null (set-difference names ref-names :test #'equal))) ()
      "names for target run and reference run must be same.")
    (loop for name in names
        as target-i = (position name target-names :test #'equal)
        as ref-i = (position name ref-names :test #'equal)
        collect (cons name (e-score target-i ref-i target-model ref-model)))))

;; EEC
;; ref: 
;; - S.Hirose, K.Yamanishi, T.Nakata, R.Fujimaki
;;   "Network Anomaly Detection based on Eigen Equation Compression"
;; - Gupta and Nagar "Matrix Variate Distributions"
(defmethod make-eec-detector ((ts time-series-dataset) window-size &key (xi 0.8d0) (global-m 3))
  (let ((pts (map 'vector #'ts-p-pos (ts-points ts)))
        (dim (length (dataset-dimensions ts)))
        window global-cov local-covs)
    (assert (>= (length pts) (1+ window-size)) (window-size)
      "For initialization, number of points must be more than window-size + 1.")
    (assert (>= dim global-m) (global-m) "global-m must be less than number of dimesion(~A)." dim)
    (assert (< 0d0 xi 1d0) (xi) "xi must be 0 < xi < 1.")
    (loop for start from 0 
        for end from window-size to (length pts)
        as %window = (subseq pts start end)
        as (global locals) = (multiple-value-list (calc-eec-features %window dim xi global-m))
        collect global into globals
        collect locals into locals-list finally 
          (setf window %window
                global-cov (init-covariance (coerce globals 'vector) (coerce globals 'vector))
                local-covs (loop for i below dim
                               as locals = (map 'vector (lambda (l) (nth i l)) locals-list)
                               collect (init-matrix-covariance locals))))
    (lambda (new-dvec)
      (setf window (concatenate 'vector (subseq window 1) (list new-dvec)))
      (multiple-value-bind (global locals cor-str-mat cls)          
          (calc-eec-features window dim xi global-m)
        (declare (ignorable cor-str-mat cls))
        (prog1 (list :score (global-anomaly-score global 
                                                  (x-mean-vec global-cov)
                                                  (get-covariance global-cov))
                     :local-scores
                     (loop for local in locals
                         for cov in (mapcar #'get-matrix-covariance local-covs)
                         for mu in (mapcar #'mean-mat local-covs)
                         as score = (local-anomaly-score local mu cov)
                         collect score))
          (update-covariance global-cov global global)
          (mapc (lambda (local cov-obj)
                  (update-matrix-covariance cov-obj local))
                locals local-covs))))))

;;;;;;;;;;;;;;;;;;;;
; routines for eec ;
;;;;;;;;;;;;;;;;;;;;
(defun calc-eec-features (window dim xi global-m)
  (let ((aij (correlation-with-constant (coerce window 'vector) :absolute t)))
    (multiple-value-bind (global-feature princ-eigen-vec)
        (calc-global-feature aij global-m)
      (multiple-value-bind (local-features clusterings)
          (calc-local-features dim princ-eigen-vec aij xi)
        (values global-feature local-features aij clusterings)))))
(defun calc-global-feature (cor-mat global-m &optional (abstol 1d-12))
  (multiple-value-bind (eigen-vals eigen-mat) 
      #+mkl (symat-ev (copy-mat cor-mat) :eigen-thld global-m :abstol abstol)
      #-mkl (eigen-by-power (copy-mat cor-mat) :eigen-thld global-m :precision abstol)
      (let ((principal (make-dvec (array-dimension cor-mat 0))))
        (do-vec (_ principal :type double-float :index-var i :setf-var sf)
          (declare (ignore _))
          (setf sf (aref eigen-mat 0 i))) ;; section 3.3
        (values eigen-vals principal))))
(defun calc-local-features (dim psi-vec cor-strength-mat xi &key clusters-list)
  (assert (equal `(,dim ,dim) (array-dimensions cor-strength-mat)))
  (loop for i below dim
      as %clusters = (when clusters-list (nth i clusters-list))
      as (mat clusters) = 
        (multiple-value-list (calc-local-feature i dim cor-strength-mat psi-vec 
                                                 :xi xi :clusters %clusters))
      collect mat into mats
      collect clusters into clusterings
      finally (return (values mats clusterings))))
(defun calc-local-feature (index dim cor-str-mat psi &key (xi 0.8d0) (clusters nil))
  (declare (type dmat cor-str-mat) (type dvec psi))
  (check-type psi dvec)
  (check-type cor-str-mat dmat)
  (let* ((clusters (if clusters clusters (local-clusters index cor-str-mat :xi xi)))
         (size (length clusters))
         (mat (make-array `(,size ,size) :element-type 'double-float :initial-element 0d0)))
    (loop for row below size
        do (loop for col to row
               as ck1 = (nth col clusters)
               as ck2 = (nth row clusters)
               as val = (if (and ck1 ck2)
                            (calc-local-feature-component dim cor-str-mat psi ck1 ck2)
                          0d0) ;; 空クラスタ処理
               do (setf (aref mat col row) val
                        (aref mat row col) val))
        finally (return (values mat clusters)))))
;; Cluster structure
;; C1 : i itself
;; C2 : strong correlations
;; C3 : weak correlations
(defun local-clusters (index cor-str-mat &key (xi 0.8d0))
  (let ((cor-str-vec (row-aref cor-str-mat index)))
    (declare (type dvec cor-str-vec))
    (loop for cor-str across cor-str-vec
        for i from 0
        with c1
        with c2
        with c3
        do (cond ((eql i index) (push i c1))
                 ((>= cor-str xi) (push i c2))
                 ((<= 0 cor-str xi) (push i c3))
                 (t (error "Unexpected situation")))
        finally (return `(,c1 ,c2 ,c3)))))
;; 局所特徴量の各要素値
(defun calc-local-feature-component (dim cor-str-mat psi ck1 ck2)
  (declare (type dmat cor-str-mat) (type dvec psi))
  (flet ((%m*v (symat vec)
           #+mkl
           (let* ((uplo "U")
                  (n dim)
                  (alpha 1d0)
                  (a symat)
                  (lda (max 1 n))
                  (x vec)
                  (incx 1)
                  (beta 0d0)
                  (y (make-dvec n))
                  (incy 1))
             (mkl.blas:dsymv uplo n alpha a lda x incx beta y incy))
           #-mkl
           (m*v symat vec)))
    (inner-product (cck psi dim ck2) (%m*v cor-str-mat (cck psi dim ck1)))))
(defun cck (psi dim ck)
  (declare (type dvec psi) (optimize speed))
  (let* ((pck*psi (proj-by-ck psi ck dim))
         (denom (sqrt (inner-product psi pck*psi))))
    (declare (type dvec pck*psi) (type double-float denom))
    (cond ((every #'zerop pck*psi) pck*psi)
          ((zerop denom) (error "Unexpected situation at #'cck : ~A" pck*psi))
          (t (do-vec (val pck*psi :type double-float :setf-var sf :return pck*psi)
               (setf sf (/ val denom)))))))
(defun proj-by-ck (vec ck dim)
  "クラスタリング ck による射影"
  (loop with projed = (make-dvec (length vec) 0d0)
      for i in ck
      do (assert (< i dim))
         (setf (aref projed i) (aref vec i))
      finally (return projed)))
(defun global-anomaly-score (target mu sigma)
  (let ((density (changefinder::multivariate-normal-density mu sigma target)))
    (- (log 
        (cond ((>= 0d0 density) least-positive-double-float)
              ((= #.*INFINITY-DOUBLE* density) most-positive-double-float)
              (t density))))))
(defun local-anomaly-score (target mu sigma)
  (let* ((dims (array-dimensions sigma))
         (target-vec (vectorization (transpose target)))
         (mu-vec (vectorization (transpose mu))))
    (assert (= (length mu-vec) (length target-vec)
               (first dims) (second dims)))
    (let ((density (changefinder::multivariate-normal-density mu-vec sigma target-vec)))
      (- (log 
          (cond ((>= 0d0 density) least-positive-double-float)
                ((= #.*INFINITY-DOUBLE* density) most-positive-double-float)
                (t density)))))))

;; ベクトル値データの分散共分散行列算出クラス
;; - 初期化: init-covariance
;; - 更新: update-covariance
;; - 共分散値: get-covariance
;; - 平均値: mean-vec
(defclass calc-covariance ()
  ((xy-mat-expec :initarg :xy-mat-expec :accessor xy-mat-expec)
   (x-mean-vec :initarg :x-mean-vec :accessor x-mean-vec)
   (y-mean-vec :initarg :y-mean-vec :accessor y-mean-vec)
   (n :initarg :n :accessor n)))
(defun init-covariance (x-trials y-trials)
  (declare (type (vector dvec) x-trials y-trials))
  (check-type x-trials (vector dvec))
  (check-type y-trials (vector dvec))
  (let* ((n (length x-trials))
         (dim (length (aref x-trials 0)))
         (x-mu (mean-points x-trials))
         (y-mu (mean-points y-trials))
         (xy-mat (make-array `(,dim ,dim) :element-type 'double-float
                             :initial-element 0d0)))
    (assert (and (>= n 1) (= n (length y-trials))))
    (assert (= dim (length (aref y-trials 0))))
    (loop for x-vec across x-trials
        for y-vec across y-trials
        do (setf xy-mat (mcm xy-mat (xy-mat x-vec y-vec) :c #'+))
        finally (setf xy-mat (c*mat (dfloat (/ n)) xy-mat)))
    (make-instance 'calc-covariance :xy-mat-expec xy-mat :x-mean-vec x-mu :y-mean-vec y-mu :n n)))
(defmethod update-covariance ((cov calc-covariance) x-vec y-vec)
  (let ((new-xy-mat (xy-mat x-vec y-vec)))
    (setf (xy-mat-expec cov) (update-mat-expec (xy-mat-expec cov) (n cov) new-xy-mat)
          (x-mean-vec cov) (update-vec-expec (x-mean-vec cov) (n cov) x-vec)
          (y-mean-vec cov) (update-vec-expec (y-mean-vec cov) (n cov) y-vec)
          (n cov) (1+ (n cov)))
    cov))
(defmethod get-covariance ((cov calc-covariance))
  (let* ((dims (array-dimensions (xy-mat-expec cov)))
         (cov-mat (make-array dims :element-type 'double-float))
         (n/n-1 (dfloat (/ (n cov) (1- (n cov))))))
    (with-accessors ((xy xy-mat-expec) 
                     (x-mu x-mean-vec)
                     (y-mu y-mean-vec)) cov
      (loop for col below (first dims)
          as y-mu-val = (aref y-mu col)
          do (loop for row below (second dims)
                 as x-mu-val = (aref x-mu row)
                 as val = (- (aref xy col row) (* x-mu-val y-mu-val))
                 do (setf (aref cov-mat col row) val))
          finally (return (c*mat n/n-1 cov-mat))))))
(defun xy-mat (x-vec y-vec)
  (declare (type dvec x-vec y-vec))
  (assert (eql (length x-vec) (length y-vec)))
  (loop with n = (length x-vec)
      with res = (make-array `(,n ,n) :element-type 'double-float
                             :initial-element 0d0)
      for col below n
      as y-val = (aref y-vec col)
      do (loop for row below n
             as val = (* (aref x-vec row) y-val)
             do (setf (aref res col row) val))
      finally (return res)))
;; 行列値データの分散共分散行列算出クラス
;; - 初期化: init-matrix-covariance
;; - 更新: update-matrix-covariance
;; - 共分散値: get-matrix-covariance
;; - 平均値: mean-mat
;; ref. Gupta, Nagar "MATRIX VARIATE DISTRIBUTIONS"
(defclass matrix-covariance ()
  ((covs :initarg :covs :type (simple-array t (* *)) :accessor covs)
   (mean-mat :initarg :mean-mat :accessor mean-mat)
   (x-col :initarg :x-col :accessor x-col)
   (x-row :initarg :x-row :accessor x-row)
   (n :initarg :n :accessor n)))
(defun init-matrix-covariance (mats)
  (declare (type (vector dmat) mats))
  (check-type mats (vector dmat))
  (let* ((sample (aref mats 0))
         (x-col (array-dimension sample 0))
         (x-row (array-dimension sample 1))
         (covs (make-array `(,x-row ,x-row) :element-type t))
         (mu (matrix-mean (coerce mats 'list)))
         (row-series-hash (make-row-series-hash mats)))
    (loop for col below x-row
        as col-x-rows = (get-row-series col row-series-hash)
        do (loop for row below x-row
               as row-x-rows = (get-row-series row row-series-hash)
               do (setf (aref covs col row) (init-covariance row-x-rows col-x-rows))))
    (make-instance 'matrix-covariance 
      :covs covs :x-col x-col :x-row x-row :mean-mat mu :n (length mats))))
(defun make-row-series-hash (mats)
  (declare (type (vector dmat) mats))
  (let ((hash (make-hash-table :test #'eql))
        (nrow (array-dimension (aref mats 0) 1)))
    (loop for mat across mats
        do (loop for row below nrow
               as row-vec = (get-row mat row)
               do (push row-vec (gethash row hash nil)))
        finally (return hash))))
(defun matrix-mean (mats)
  (let* ((len (length mats))
         (dims (array-dimensions (car mats)))
         (result (make-array dims :element-type 'double-float :initial-element 0d0)))
    (loop for mat in mats
        do (setf result (mcm mat result :c #'+))
        finally (return (c*mat (dfloat (/ len)) result)))))
(defun get-row (mat nrow)
  (declare (type dmat mat))
  (check-type mat dmat)
  (let ((row (make-dvec (array-dimension mat 0))))
    (do-vec (val row :type double-float :return row :index-var i :setf-var sf)
      (declare (ignore val))
      (setf sf (aref mat i nrow)))))
(defun get-row-series (n hash) (coerce (reverse (gethash n hash)) 'vector))
(defmethod update-matrix-covariance ((model matrix-covariance) new-mat)
  (with-accessors ((x-col x-col)
                   (x-row x-row)
                   (covs covs)) model
    (assert (and (= (array-dimension new-mat 0) (x-col model))
                 (= (array-dimension new-mat 1) (x-row model))))
    (setf (mean-mat model) (update-mat-expec (mean-mat model) (n model) new-mat)
          (n model) (1+ (n model)))
    (loop with rows = (loop for i below x-row collect (get-row new-mat i))
        for col below x-row
        as col-x-row = (nth col rows)
        do (loop for row below x-row
               as row-x-row = (nth row rows)
               do (update-covariance (aref covs col row) row-x-row col-x-row)))))
(defmethod get-matrix-covariance ((model matrix-covariance))
  (with-accessors ((x-col x-col) (x-row x-row) (covs covs)) model
    (let* ((dim (* x-col x-row))
           (cov (make-array `(,dim ,dim) :element-type 'double-float)))
      (loop for col-block below x-row
          do (loop for row-block below x-row
                 as cov-mat = (get-covariance (aref covs col-block row-block))
                 do (loop for %col below x-col
                        do (loop for %row below x-col
                               as col = (+ %col (* col-block x-col))
                               as row = (+ %row (* row-block x-row))
                               as val = (aref cov-mat %col %row)
                               do (setf (aref cov col row) val))))
          finally (return cov)))))
;;; general tool for expectation calculation
(defun update-expectation (old-e old-n new-sample)
  (let* ((numerator (+ (* old-e old-n) new-sample))
         (denom (1+ old-n)))
    (/ numerator denom)))
(defun update-vec-expec (expec-vec old-n new-sample)
  (declare (type dvec old-n new-sample))
  (check-type expec-vec dvec)
  (check-type new-sample dvec)
  (do-vecs ((mv expec-vec :type double-float :setf-var sf)
            (nv new-sample :type double-float))
    (setf sf (update-expectation mv old-n nv)))
  expec-vec)
(defun update-mat-expec (expec-mat old-n new-sample)
  (declare (type dmat expec-mat new-sample))
  (check-type expec-mat dmat)
  (check-type new-sample dmat)
  (loop for col below (array-dimension expec-mat 0)
      do (loop for row below (array-dimension expec-mat 1)
             do (setf (aref expec-mat col row)
                  (update-expectation (aref expec-mat col row) old-n 
                                      (aref new-sample col row))))
      finally (return expec-mat)))
(defun vectorization (mat &optional result)
  (let* ((dims (array-dimensions mat))
         (col (first dims))
         (row (second dims))
         (dim (* row col))
         (result (if result result (make-dvec dim))))
    (assert (= dim (length result)))
    (loop for i below col
        do (loop for j below row
               do (setf (aref result (+ (* i col) j)) (aref mat i j)))
        finally (return result))))

;;;;;;;;;;;;;;;;;;;
; routines for db ;
;;;;;;;;;;;;;;;;;;;
;; SVD による典型的パターン抽出
(defun typical-pattern-svd (act-vecs)
  (let* ((n (length act-vecs))
         (m (length (car act-vecs)))
         (cmat (make-array `(,n ,m) :element-type 'double-float
                           :initial-contents (coerce act-vecs 'vector)))
         (s (make-dvec (min m n)))
         (u (make-array `(,m ,m) :element-type 'double-float))
         (vt (make-array `(,1 ,1) :element-type 'double-float))
         (ldu m)
         (ldvt 1)
         (lda (max 1 m))
         (lwork (max (+ (* 3 (min m n)) (max m n)) (* 5 (min m n))))
         (work (make-array lwork :element-type 'double-float))
         (info 0)
         (pattern (make-dvec m))
         result)
    (setq result 
      (third (multiple-value-list
              #+mkl
              (mkl.lapack:dgesvd "S" "N" m n cmat lda s u ldu vt ldvt work lwork info)
              #-mkl           
              (lapack::dgesvd "S" "N" m n cmat lda s u ldu vt ldvt work lwork info))))
    (assert (= info 0))    
    (do-vec (_ pattern :type double-float :setf-var sf :index-var i)
      (declare (ignore _))
      (let ((val (abs (aref result 0 i)))) (setf sf (if (> *epsilon* val) 0d0 val))))
    pattern))
;; 平均
(defun typical-pattern-mean (act-vecs)
  (let* ((dim (length (car act-vecs)))
         (size (length act-vecs))
         (pattern (make-dvec dim 0d0)))
    (declare (type integer dim size))
    (loop for act-vec in act-vecs
        do (do-vecs ((_ pattern :type double-float :setf-var sf)
                     (val act-vec :type double-float))
             (declare (ignore _))
             (incf sf val)))
    (do-vec (val pattern :type double-float :setf-var sf :return pattern)
      (setf sf (/ val size)))))
(defun calc-typical-pattern (act-vecs &key (method :svd)) ;; :svd | :mean
  (ecase method
    (:svd (typical-pattern-svd act-vecs))
    (:mean (typical-pattern-mean act-vecs))))
;; 非類似度
(defun dissimilarity (typical-vec act-vec)
  (declare (type dvec typical-vec act-vec))
  (- 1 (inner-product typical-vec act-vec)))
;; 1次および2次の moment の漸化式
;; ref: 論文の 式 (18), (19)
(defun next-moments (zt last-moments beta)
  (let ((scale (- 1 beta))
        (zt (dfloat zt)))
    (cons (+ (* scale (car last-moments)) (* beta zt))
          (+ (* scale (cdr last-moments)) (* beta (expt zt 2))))))
;; moment の初期値を求める
(defun get-initial-moments (act-vecs &key (typical-method :svd))
  (let* ((window-size (length act-vecs))
         (typ-vec (calc-typical-pattern (subseq act-vecs 0 (1- window-size))
                                        :method typical-method))
         (act-vec (nth (1- window-size) act-vecs))
         (dissim (dissimilarity typ-vec act-vec)))
    (cons dissim (expt dissim 2))))
;; moment から 式 (17) の sigma, n-1 の両パラメータを推定する
(defun estimate-vmf-parameters (moments)
  (let* ((1st (car moments))
         (2nd (cdr moments))
         (1st^2 (expt 1st 2))
         (2nd-1st^2 (- 2nd 1st^2)))
    (declare (type double-float 1st 2nd 1st^2))
    (values (/ (* 2 1st^2) 2nd-1st^2)
            (/ 2nd-1st^2 (* 2 1st)))))
;; Z_th を求める
(defun vmf-threshold (n-1 sigma pc)
  (let* ((scale (* 2 sigma))
         (shape (/ n-1 2))         
         (1-pc (- 1 pc)))
    (if (and (< 0 scale *+inf*)
             (< 0 shape *+inf*))
        (let ((gamma-dist (gamma-distribution scale shape)))
          (if (typep gamma-dist 'statistics::gamma-like-distribution)
              (quantile-ili gamma-dist 1-pc)
            (quantile gamma-dist 1-pc)))
      :na)))

;;;;;;;;;;;;;;;;;;;;;;;;;
; routines for periodic ;
;;;;;;;;;;;;;;;;;;;;;;;;;
(defun init-period-window (period dim vec-list &key (var 0.01d0) (r 0.5d0))
  (assert (<= period (length vec-list)))
  (let ((pwindow (loop repeat period 
                     for init-mu in vec-list
                     collect (list :mu init-mu :cov (diag dim var)))))
    (loop for i from period below (length vec-list)
        do (update-pwindow (nth i vec-list) pwindow r))
    pwindow))
(defun update-pwindow (vec pwindow &optional (r 0.5d0))
  (declare (type dvec vec))
  (assert (<= 0d0 r 1d0))
  (destructuring-bind (&key mu cov) (first pwindow)
    (update-mu-cov mu cov vec r)
    (loop for i below (1- (length pwindow))
        as j = (1+ i)
        do (setf (nth i pwindow) (nth j pwindow))
        finally (setf (nth i pwindow) (list :mu mu :cov cov))))
  pwindow)
(defun update-mu-cov (mu cov dvec r)
  (declare (type dvec mu dvec) (type dmat cov) (optimize speed))
  (let ((dim (length dvec))
        (1-r (- 1d0 r)))
    (do-vecs ((m mu :type double-float :setf-var sf)
              (val dvec :type double-float))
      (setf sf (+ (* 1-r m) (* r val))))
    (loop for i below dim
        as bi-mi double-float = (- (aref dvec i) (aref mu i))
        do (loop for j from i below dim
               as bj-mj double-float = (- (aref dvec j) (aref mu j))
               as vij = (aref cov i j)
               as val = (+fl (*fl 1-r vij) (*fl r bi-mi bj-mj))
               do (setf (aref cov i j) val (aref cov j i) val)))))
(defun score-by-mahalanobis (dvec pwindow  &key (beta 1d-2))
  (destructuring-bind (&key mu cov &allow-other-keys) (first pwindow)
    (declare (type dvec dvec mu) (type dmat cov))
    (let* ((dim (length mu))
           (prec (m^-1 (mcm cov (diag dim beta)))))
      (flet ((conditional-gauss (i)
               (assert (and (<= 0 i) (< i dim)))
               (let ((pii^-1 (/ (aref prec i i))))
                 (when (minusp pii^-1) (setf pii^-1 (- pii^-1)))
                 (cons (- (aref mu i)
                          (* pii^-1
                             (loop for j below dim unless (eql i j)
                                 sum (* (aref prec i j) (- (aref dvec j) (aref mu j))))))
                       (sqrt pii^-1)))))
        (list :score (let* ((x-y (vcv dvec mu :c #'-))
                            (d^2 (inner-product x-y (m*v prec x-y))))
                       (if (minusp d^2) 0d0 (sqrt d^2)))
              :local-scores (loop for b across dvec
                                for i from 0
                                as (m . s) = (conditional-gauss i)
                                collect (/ (- b m) s)))))))

;;;;;;;;;;;;;;;;;;;;
; routines for snn ;
;;;;;;;;;;;;;;;;;;;;
(defun dissimilarity-matrix (pts)
  (let* ((cor (correlation-with-constant pts :absolute nil))
         (n (array-dimension cor 0))
         (a (make-array `(,n ,n) :element-type 'double-float)))
    (declare (type dmat cor a) (optimize speed))
    (flet ((almost= (a b) (<= (- b *epsilon*) a (+ b *epsilon*))))
      (loop for i below n
          do (loop for j to i
                 as cij double-float = (aref cor i j)
                 as aij double-float = (cond ((zerop cij) (kronecker-delta i j))
                                             (t cij))
                 as d double-float = 
                   (cond ((almost= aij 0d0) *+inf*)
                         ;; infinite dissimilarity
                         ((almost= aij 1d0) 0d0)
                         ((almost= aij -1d0) 0d0)
                         ((< 0d0 (abs aij) 1d0)
                          (- (log (the double-float (abs aij)))))
                         (t (error "Unexpected value for correlation ~A (~A ~A)" aij i j)))
                 do (setf (aref a i j) d (aref a j i) d))
          finally (return a)))))
(defun correlation-with-constant (pts &key (absolute nil))
  (declare (type (vector dvec) pts))
  (check-type pts (vector dvec))
  (let* ((cov (covariance-matrix pts))
         (n (array-dimension cov 0))
         (a (make-array (list n n) :element-type 'double-float)))
    (declare (type dmat cov a) (type fixnum n) (optimize speed))
    (loop for i below n
        do (loop for j to i
               as cij double-float = (aref cov i j)
               as cii double-float = (aref cov i i)
               as cjj double-float = (aref cov j j)
               as aij double-float =
                 (if (or (zerop cii) (zerop cjj)) (kronecker-delta i j)
                   (/ (the double-float cij) (the double-float (sqrt (* cii cjj)))))
               do (when absolute (setf aij (abs aij)))
                  (setf (aref a i j) aij (aref a j i) aij))
        finally (return a))))
(defun kronecker-delta (i j) (if (eql i j) 1d0 0d0))
(defun knn-list (wmat k)
  (declare (type dmat wmat))
  (assert (> k 0))
  (loop with size = (first (array-dimensions wmat))
      with vec = (make-dvec size 0d0)
      for i below size
      as i-d-alist = (loop for d across (row-aref wmat i vec)
                         for %i from 0
                         unless (eql %i i) collect (cons %i d))
      collect (subseq (sort i-d-alist #'< :key #'cdr) 0 k)))
(defun coupling-probabilities (nbrs-d-list sigma-i)
  (flet ((z_i (i)
           (let ((nbrs-d (nth i nbrs-d-list)))
             (if (> (length nbrs-d) 0)
                 (+ 1d0 (loop for j-d in nbrs-d
                            sum (handler-case (exp (- (/ (the double-float (cdr j-d)) sigma-i)))
                                  (FLOATING-POINT-UNDERFLOW (c) (declare (ignore c)) 0d0))))
               1d0))))
    (loop for i from 0
        for nbrs-d in nbrs-d-list
        collect (loop for (j . d) in nbrs-d
                    as prob = (/ (handler-case
                                     (exp (- (/ (the double-float d) sigma-i)))
                                   (FLOATING-POINT-UNDERFLOW (c) (declare (ignore c)) 0d0))
                                 (z_i i))
                    collect (cons j `(:cp ,prob :d ,d))))))
(defun coupling-tightness (nbrs cp-alist)
  (if (> (length nbrs) 0)
      (loop for j in nbrs
          as prob = (getf (cdr (assoc j cp-alist :test #'eql)) :cp)
          sum (if prob prob 0d0))
    0d0))
(defun e-score (target-i ref-i target-model ref-model)
  (let* ((target-cp-alist (cdr (assoc target-i (graphs target-model) :test #'eql)))
         (ref-cp-alist (cdr (assoc ref-i (graphs ref-model) :test #'eql)))
         (target-nbrs (mapcar #'car target-cp-alist))
         (ref-nbrs (mapcar #'car ref-cp-alist)))
    (max (abs (- (coupling-tightness target-nbrs target-cp-alist)
                 (coupling-tightness target-nbrs ref-cp-alist)))
         (abs (- (coupling-tightness ref-nbrs target-cp-alist)
                 (coupling-tightness ref-nbrs ref-cp-alist))))))
(defmethod %e-scores ((target-model snn) (ref-model snn))
  (let* ((target-names (names target-model))
         (ref-names (names ref-model))
         (names (sort (copy-list (union target-names ref-names :test #'equal)) #'string<)))
    (assert (and (null (set-difference names target-names :test #'equal))
                 (null (set-difference names ref-names :test #'equal))) ()
      "names for target run and reference run must be same.")
    (loop for name in names
        as target-i = (position name target-names :test #'equal)
        as ref-i = (position name ref-names :test #'equal)
        collect (cons name (e-score target-i ref-i target-model ref-model)))))
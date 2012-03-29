(defpackage :anomaly-detection
  (:use :cl :excl :vars :util :vector :matrix :statistics :read-data
        :missing-val :csv :ts-read-data :ts-stat :ts-util :ts-state-space :ts-ar
        :read-graph :graph-centrality :graph-shortest-path)
  (:export ))

(in-package :anomaly-detection)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; benchmark tools 
(defparameter *comment-interval* 288)
(defclass benchmark ()
  ((results :initform nil :initarg :results :accessor results)))
(defun init-eec-benchmark ()
  (make-instance 'benchmark
    :results `(:correlation-mat ,(list 0 0d0 0d0) ;; (list count real-time cpu-time)
                                :eigen ,(list 0 0d0 0d0)
                                :compression ,(list 0 0d0 0d0)
                                :scoring ,(list 0 0d0 0d0))))
(defun init-snn-benchmark ()
  (make-instance 'benchmark
    :results `(:weight-matrix ,(list 0d0 0d0) ;; (list real-time cpu-time)
                              :knn-graph ,(list 0d0 0d0)
                              :coupling-probabilities ,(list 0d0 0d0)
                              :scoring ,(list 0d0 0d0))))
(defmacro bench-eec (bench-obj
                     section ;; :cor | :eigen | :compression | :scoring
                     incf-count ;; nil | t
                     &body body)
  `(if ,bench-obj
       (let ((start-real (get-internal-real-time))
             (start-run (get-internal-run-time)))
         (multiple-value-prog1 ,@body
           (let ((real (/ (- (get-internal-real-time) start-real) internal-time-units-per-second))
                 (run (/ (- (get-internal-run-time) start-run) internal-time-units-per-second))
                 (target (getf (results ,bench-obj)
                               (ecase ,section
                                 (:cor :correlation-mat)
                                 (:eigen :eigen)
                                 (:compression :compression)
                                 (:scoring :scoring)))))
             (when ,incf-count (incf (first target)))
             (incf (second target) real)
             (incf (third target) run))))
     ,@body))
(defmacro bench-snn (bench-obj
                     section ;; :w-mat | :knn | :c-p | :scoring
                     &body body)
  `(if ,bench-obj
       (let ((start-real (get-internal-real-time))
             (start-run (get-internal-run-time)))
         (multiple-value-prog1 ,@body
           (let ((real (/ (- (get-internal-real-time) start-real) internal-time-units-per-second))
                 (run (/ (- (get-internal-run-time) start-run) internal-time-units-per-second))
                 (target (getf (results ,bench-obj)
                               (ecase ,section
                                 (:w-mat :weight-matrix)
                                 (:knn :knn-graph)
                                 (:c-p :coupling-probabilities)
                                 (:scoring :scoring)))))
             (incf (first target) real)
             (incf (second target) run))))
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ide, Kashima "Eigenspace-based Anomaly Detection in Computer Systems"
;; による異常検出
(defclass anomaly-detector-eb ()
  ((window :initform nil :initarg :window :accessor window)
   (window-size :initform nil :initarg :window-size :accessor window-size)
   (moments :initform (cons 0d0 0d0) :initarg :moments :accessor moments)
   (beta :initform nil :initarg :beta :accessor beta)
   (pc :initform nil :initarg :pc :accessor pc)
   (alpha-order :initform nil :initarg :alpha-order :accessor alpha-order)
   (activity-method :initform :svd :initarg :activity-method :accessor activity-method)
   (typical-method :initform :svd :initarg :typical-method :accessor typical-method)
   (pagerank-c :initform nil :initarg :pagerank-c :accessor pagerank-c)
   (localizep :initform nil :initarg :localizep :accessor localizep)
   (ar-conf-coef :initform nil :initarg :ar-conf-coef :accessor ar-conf-coef)
   (ar-default-var :initform nil :initarg :ar-default-var :accessor ar-default-var)
   (param-names :initform nil :initarg :param-names :accessor param-names)))

;; input: input, 入力データファイル名, Dependency matrix (対角成分に α によるノイズは入っていない) の系列データ, 
;;               データ記述形式は read-graph-series に準ずる
;;        output, 出力データファイル名
;;        window-size, ウィンドウサイズ
;;        file-format, :csv | :sexp | :edgelist 入力データファイルの形式
;;        beta, 論文 (18), nil なら 1/window-size
;;        pc, 論文 5.3
;;        alpha-order, 論文 (1) and 6.2
;;        activity-method, :eigen | :pagerank, activity vector 算出法
;;        typical-method, :svd | :mean, 典型的パターン抽出法 (:svd SVD, :mean 平均)
;;        pagerank-c, ページランクのc
;;        localizep, ARモデルによる局所異常スコア算出を行うか
;;        ar-conf-coef, 局所異常スコア算出のための信頼度係数(0<a<1)
;;        ar-default-var, 局所異常スコア算出のための最小分散値
;; output: list of plist (:time * :score * :threshold *)
;;         :score は異常スコア、:threshold は閾値
(defun anomaly-detection-eb (input output window-size
                             &key (file-format :sexp)
                                  (beta nil)
                                  (pc 0.05d0)
                                  (alpha-order 0.01d0)
                                  (activity-method :eigen) ;; :eigen | :pagerank
                                  (typical-method :svd) ;; :svd | :mean
                                  (pagerank-c 0.85d0)
                                  (localizep nil)
                                  (ar-conf-coef 0.99d0)
                                  (ar-default-var *epsilon*)
                                  (print-status t)
                                  (external-format :default))
  (let* ((window (read-graph-series input
                                    :external-format external-format
                                    :format file-format
                                    :directed nil
                                    :start 0 :end window-size))
         (detector 
          (progn (when print-status (format t "Initialize detector...~%"))
                 (init-anomaly-detector-eb window
                                           :beta beta
                                           :pc pc
                                           :alpha-order alpha-order
                                           :activity-method activity-method
                                           :typical-method typical-method
                                           :pagerank-c pagerank-c
                                           :localizep localizep
                                           :ar-conf-coef ar-conf-coef
                                           :ar-default-var ar-default-var))))
    (with-open-file (in input :direction :input :external-format external-format)
      (with-open-file (out output :direction :output :external-format external-format
                       :if-exists :supersede)
        (when print-status (format t "Detecting anomaly...~%"))
        (format out "(")
        (let ((ut (get-universal-time)) (i 1))
          (do-graph-series ((gr label) in :start window-size :format file-format
                                       :directed nil)
            (write `(:time ,label ,@(update-detector-eb detector gr)) :stream out)
            (terpri out)
            (when (and print-status (zerop (mod i *comment-interval*)))
              (format t "Now at ~A : ~,5F sec/graph~%" label (/ (- (get-universal-time) ut) i)))
            (incf i)))
        (format out ")")))
    detector))
;;;ANOMALY-DETECTION(253): (anomaly-detection-eb "~/machine-learning/sample/linkdown35-graph-series"
;;;                                              "foo.sexp" 12)
;;;#<ANOMALY-DETECTOR-EB @ #x100d8ebaf2>
           
(defmethod init-anomaly-detector-eb ((gr-series simple-graph-series)
                                     &key (beta nil)
                                          (pc 0.05d0)
                                          (alpha-order 0.01d0)
                                          (activity-method :eigen) ;; :eigen | :pagerank
                                          (typical-method :svd) ;; :svd | :mean
                                          (pagerank-c 0.85d0)
                                          (localizep nil)
                                          (ar-conf-coef 0.99d0)
                                          (ar-default-var *epsilon*))
  (let* ((graph-series-list (graphs gr-series))
         (param-names 
          (mapcar #'node-name (sort (copy-seq (nodes (car (graphs gr-series)))) #'< :key #'node-id)))
         (window-size (length graph-series-list)))
    (unless beta (setf beta (dfloat (/ window-size))))
    (assert (< 0 beta 1))
    (assert (< 0 pc 1))
    (assert (< 0 ar-conf-coef 1))
    (let* ((detector (make-instance 'anomaly-detector-eb 
                       :beta beta :pc pc :alpha-order alpha-order
                       :activity-method activity-method
                       :typical-method typical-method
                       :window-size window-size
                       :pagerank-c pagerank-c
                       :localizep localizep
                       :ar-conf-coef ar-conf-coef
                       :ar-default-var ar-default-var
                       :param-names param-names))
           (act-vecs (loop for gr in graph-series-list
                         collect (extract-activity-vector gr 
                                                          :alpha alpha-order
                                                          :method activity-method
                                                          :pagerank-c pagerank-c)))
           (moments (get-initial-moments act-vecs :typical-method typical-method)))
      (setf (window detector) act-vecs (moments detector) moments)
      detector)))

(defmethod update-detector-eb ((detector anomaly-detector-eb) (gr simple-graph))
  (assert (window detector))
  (let* ((act-vec (extract-activity-vector gr :alpha (alpha-order detector)
                                           :method (activity-method detector)
                                           :pagerank-c (pagerank-c detector)))
         (typ-vec (calc-typical-pattern (window detector) :method (typical-method detector)))
         (zt (dissimilarity typ-vec act-vec))
         (local-scores (calc-local-scores detector act-vec 
                                          :ar-default-var (ar-default-var detector)))
         (moments (next-moments zt (moments detector) (beta detector)))
         (thld
          (multiple-value-bind (n-1 sigma)
              (estimate-vmf-parameters moments)
            (vmf-threshold n-1 sigma (pc detector)))))
    (setf (window detector) `(,@(cdr (window detector)) ,act-vec)
          (moments detector) moments)
    (values `(:score ,zt :threshold ,thld
                     :activity-vector ,act-vec :typical-pattern ,typ-vec
                     :local-scores ,(if local-scores local-scores *na*)
                     :moments ,moments)
            detector)))
            
(defmethod extract-activity-vector ((gr simple-graph) &key (method :eigen)
                                                           (alpha 0.01d0)
                                                           (pagerank-c 0.85d0))
  (declare (optimize speed))
  (flet ((make-stabilizer (size)
           (let ((mat (make-array `(,size ,size) :element-type 'double-float
                                  :initial-element 0d0)))
             (loop for i below size
                 do (setf (aref mat i i) (random alpha))))))
    (let* ((size (length (nodes gr))))
      (ecase method
        (:eigen (eigen-centrality gr :stabilizer (make-stabilizer size)))
        (:pagerank (pagerank gr :c pagerank-c))))))

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

;; AR on activity-vector
;; trial to localize anomaly by using AutoRegressive time-series-model
;;
;; 局所異常スコア
(defmethod calc-local-scores ((detector anomaly-detector-eb) target 
                              &key (ar-default-var *epsilon*))
  (declare (type dvec target))
  (when (localizep detector)
    (let* ((local-scores 
            (local-anomaly-score-by-ar (window detector) target
                                       :confidence-coefficient (ar-conf-coef detector)
                                       :default-var ar-default-var))
           (names (if (param-names detector) (param-names detector)
                    (loop for i below (length local-scores) collect (format nil "~A" i)))))
      (loop for name in names
          for d in local-scores
          collect `(:name ,name :score ,d)))))

;; #'local-anomaly-score-by-ar
;; input: window, <list activity-vector>
;;        actvec, target activity-vector
;;        confidence-coefficient, <number> ( 0 < 1 )
;; description:
;;  actvec の各要素の異常度合いを、各要素のARモデルを生成し、その予測区間とのずれから算出する。
;;  予測区間は予測値のガウス分布の信頼度区間 (confidence-coefficient) とし、
;;  その区間内であれば異常ではない。区間外ならば (外れ幅) / (推定区間幅) を外れ度合いとする。
;;  (外れ幅) は値が減っている方向に外れていれば負の値
;;             値が増えている方向に外れていれば正の値とする。
;; return: (cons 局所異常スコア 区間幅) のリスト
(defun local-anomaly-score-by-ar (window target &key (confidence-coefficient 0.99d0) ; 99%
                                                     (default-var *epsilon*)
                                                     (ar-order-max 10))
  (declare (type dvec target) (type (cons dvec) window))
  (assert (>= (length window) 2))
  (loop with len = (length window)
      with buff = (coerce (loop repeat len collect (make-dvec 1 0d0)) 'vector)
      for i from 0
      for val across target
      as ts = (make-constant-time-series-data '("activity")
                                              (loop for j below len do
                                                    (setf (aref (aref buff j) 0)
                                                      (aref (nth j window) i))
                                                  finally (return buff)))
      as gaussian = (predict-gaussian-by-ar ts :default-var default-var :order-max ar-order-max)
      as d = (degree-of-outrange gaussian val 
                                 :confidence-coefficient confidence-coefficient)
      collect d))
(defmethod range-by-confidence ((dist normal-distribution) confidence-coefficient)
  (let* ((alpha (/ (- 1 confidence-coefficient) 2.0d0))
         (lower-confident-limit (quantile dist alpha))
         (upper-confident-limit 
          (- (* 2 (statistics::expected-value dist)) lower-confident-limit)
          #+ignore (quantile dist (- 1.0d0 alpha))))
    (cons lower-confident-limit upper-confident-limit)))
(defun degree-of-outrange (gaussian val &key (confidence-coefficient 0.99d0))
  (assert (< 0 confidence-coefficient 1))
  (let* ((range (range-by-confidence gaussian confidence-coefficient))
         (width (dfloat (- (cdr range) (car range)))))
    (declare (type double-float width))
    (values
     (cond ((minusp width) (error "illegal range: ~A" range))
           ((and (<= (car range) val) (<= val (cdr range))) 0d0)
           ((< val (car range))
            (let ((err (- val (car range)))) (/ err width)))
           ((< (cdr range) val)
            (let ((err (- val (cdr range)))) (/ err width)))
           (t (error "Unexpected width: ~A" width)))
     range)))
;; make instance of 'time-series-dataset by list of activity vectors
(defun make-tss-by-actvecs (actvecs)
  (check-type actvecs (cons dvec))
  (setq actvecs (transposeV (coerce actvecs 'vector)))
  (loop for vec across actvecs
      collect (make-ts-by-vec vec)))
(defun make-ts-by-vec (vec)
  (check-type vec dvec)
  (let ((data (map 'vector (lambda (val) (make-dvec 1 val)) vec)))
    (make-constant-time-series-data '("activity") data)))
;; ARモデルを生成し、その予測値のガウス分布を求める。
(defmethod predict-gaussian-by-ar ((ts time-series-dataset) &key (default-var *epsilon*)
                                                                 (order-max nil))
  (let* ((s^2 (aref (ts-covariance ts) 0 0)))
    (if (> *epsilon* s^2)
        (let* ((val (aref (ts-mean ts) 0))
               (gaussian (normal-distribution val (sqrt default-var))))
          (values gaussian nil))
      (let ((model (ar ts :demean t :method :burg :aic t :order-max order-max)))
        (multiple-value-bind (pred std-err) (time-series-util:predict model :n-ahead 1)
          (let* ((last-pos (1- (length (ts-points pred))))
                 (pred-val (aref (ts-p-pos (aref (ts-points pred) last-pos)) 0))
                 (std-err-val (aref (ts-p-pos (aref (ts-points std-err) last-pos)) 0))
                 (gaussian 
                  (normal-distribution
                   pred-val (max std-err-val default-var))))
            (values gaussian model)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ide, Kashima "Eigenspace-based Anomaly Detection in Computer Systems"
;; の Activity Vector を SDAR で予測(predict)していく異常検出
(defclass anomaly-detector-eb-predict ()
  ((sdar :initform nil :initarg :sdar :accessor sdar)
   (ar-k :initform nil :initarg :ar-k :accessor ar-k)
   (discount :initform nil :initarg :discount :accessor discount)
   (threshold :initform nil :initarg :threshold :accessor threshold)
   (activity-method :initform nil :initarg :activity-method :accessor activity-method)
   (alpha-order :initform nil :initarg :alpha-order :accessor alpha-order)
   (pagerank-c :initform nil :initarg :pagerank-c :accessor pagerank-c)
   (localizep :initform nil :initarg :localizep :accessor localizep)
   (ar-conf-coef :initform nil :initarg :ar-conf-coef :accessor ar-conf-coef)
   (ar-default-var :initform nil :initarg :ar-default-var :accessor ar-default-var)
   (param-names :initform nil :initarg :param-names :accessor param-names)))
;; input: input, 入力データファイル名, Dependency matrix (対角成分に α によるノイズは入っていない) の系列データ, 
;;               データ記述形式は read-graph-series に準ずる
;;        output, 出力データファイル名
;;        file-format, :csv | :sexp | :edgelist 入力データファイルの形式
;;        ar-k, VAR次数、 nilならAICによる次数選択
;;        discount, 忘却パラメータ
;;        threshold, マハラノビス距離の閾値
;;        activity-method, :eigen | :pagerank, activity vector算出法
;;        alpha-order, Ide&Kashima 論文 (1) and 6.2
;;        pagerank-c, ページランクのc
;;        init-n, SDARモデル初期化のために使うデータ点数
;;        localizep, ARモデルによる局所異常スコア算出を行うか
;;        ar-conf-coef, 局所異常スコア算出のための信頼度係数(0<a<1)
;;        ar-default-var, 局所異常スコア算出のための最小分散値
(defun anomaly-detection-eb-predict (input output
                                     &key (file-format :sexp)
                                          (ar-k 1)
                                          (discount 0.01d0)
                                          (threshold 1d0)
                                          (activity-method :pagerank) ;; :eigen | :pagerank
                                          (alpha-order 0.01d0)
                                          (pagerank-c 0.85d0)
                                          (init-n 288)
                                          (localizep nil)
                                          (ar-conf-coef 0.99d0)
                                          (ar-default-var *epsilon*)
                                          (print-status t)
                                          (external-format :default))  
  (let* ((grs (read-graph-series input
                                 :format file-format
                                 :directed nil
                                 :start 0
                                 :end init-n
                                 :external-format external-format))
         (detector (progn (assert (= (length (graphs grs)) init-n) (init-n)
                            "init-n has to be less than ~D." (length (graphs grs)))
                          (when (numberp ar-k)
                            (assert (>= (floor (* 10 (log init-n 10))) ar-k) (ar-k)
                              "ar-k has to be less than or equal to ~A."
                              (floor (* 10 (log init-n 10)))))
                          (when print-status (format t "Initialize detector...~%"))
                          (init-anomaly-detector-eb-predict grs
                                                            :ar-k ar-k
                                                            :discount discount
                                                            :threshold threshold
                                                            :activity-method activity-method
                                                            :alpha-order alpha-order
                                                            :pagerank-c pagerank-c
                                                            :localizep localizep
                                                            :ar-conf-coef ar-conf-coef
                                                            :ar-default-var ar-default-var))))
    (with-open-file (in input :direction :input :external-format external-format)
      (with-open-file (out output :direction :output :external-format external-format
                       :if-exists :supersede)
        (when print-status (format t "Detecting anomaly...~%"))
        (let ((ut (get-universal-time)) (i 1))
          (format out "(")
          (do-graph-series ((gr label) in :start init-n :format file-format)
            (write `(:time ,label ,@(update-detector-eb-predict detector gr)) :stream out)
            (terpri out)
            (when (and print-status (zerop (mod i *comment-interval*)))
              (format t "Now at ~A : ~,5F sec/graph~%" label (/ (- (get-universal-time) ut) i)))
            (incf i))
          (format out ")"))))
    detector))

(defmethod init-anomaly-detector-eb-predict ((gr-series simple-graph-series)
                                             &key (ar-k 1)
                                                  (discount 1d-2)
                                                  (threshold 1d0)
                                                  (activity-method :eigen) ;; :eigen | :pagerank
                                                  (alpha-order 1d-2)
                                                  (pagerank-c 0.85d0)
                                                  (localizep nil)
                                                  (ar-conf-coef 0.99d0)
                                                  (ar-default-var *epsilon*))
  (assert (or (null ar-k) (plusp ar-k)))
  (assert (< 0 discount 1))
  (assert (< 0 threshold))
  (assert (< 0 ar-conf-coef 1))
  (let* ((param-names 
          (mapcar #'node-name (sort (copy-seq (nodes (car (graphs gr-series))))
                                    #'< :key #'node-id)))         
         (ts (loop for gr in (graphs gr-series) collect
                   (extract-activity-vector gr
                                            :method activity-method 
                                            :alpha alpha-order
                                            :pagerank-c pagerank-c) into actvecs
                 finally (return (make-constant-time-series-data param-names
                                                                 (coerce actvecs 'vector)))))
         (sdar (ts-autoregression::init-sdar ts :ar-k ar-k)))
    (make-instance 'anomaly-detector-eb-predict
      :sdar sdar
      :ar-k ar-k
      :discount discount
      :threshold threshold
      :activity-method activity-method
      :alpha-order alpha-order
      :pagerank-c pagerank-c
      :localizep localizep
      :ar-conf-coef ar-conf-coef
      :ar-default-var ar-default-var
      :param-names param-names)))

(defmethod update-detector-eb-predict ((detector anomaly-detector-eb-predict)
                                       (gr simple-graph))
  (assert (sdar detector))
  (let ((act-vec (extract-activity-vector gr :alpha (alpha-order detector)
                                          :method (activity-method detector)
                                          :pagerank-c (pagerank-c detector))))
    (multiple-value-bind (pre-mu pre-v d state)        
        (update-sdar-as-detector (sdar detector) act-vec
                                 :discount (discount detector)
                                 :threshold (threshold detector))
      (declare (ignore state))
      (let ((local-scores 
             (when (localizep detector)
               (loop for name in (param-names detector)
                   for i from 0
                   as mu = (aref pre-mu i)
                   as v = (aref pre-v i i)
                   as gaussian =
                     (normal-distribution mu (sqrt (max (ar-default-var detector) v)))
                   as x = (aref act-vec i)
                   as score = (degree-of-outrange gaussian x
                                                  :confidence-coefficient
                                                  (ar-conf-coef detector))
                   collect `(:name ,name :score ,score)))))
        (values `(:score ,d 
                         :activity-vector ,act-vec 
                         :predicted-vector ,pre-mu
                         :local-scores ,(if local-scores local-scores *na*))
                detector)))))

(defmethod update-sdar-as-detector ((sdar ts-autoregression::sdar) new-xt
                                    &key (discount 0.01d0)
                                         (threshold 5d0))
  (multiple-value-bind (pre-mu pre-v) (ts-ar::predict-sdar sdar)
    (let* ((n (array-dimension pre-v 0))
           (inv-v (round-mat
                   (m^-1 (mcm (round-mat pre-v) (diag n 1d-2) :c #'+)))))
      (declare (type dmat inv-v))
      (let* ((d (%mahalanobis-distance new-xt pre-mu inv-v))
             (state (if (>= threshold d) :normal :anomaly)))
        (ecase state
          (:normal (ts-ar::update-sdar sdar new-xt :discount discount))
          (:anomaly (let* ((v (vcv new-xt pre-mu :c #'-))
                           (denom (loop for j below n sum
                                        (* (loop for i below n sum
                                                 (* (aref v i) (aref inv-v j i)))
                                           (aref v j))))
                           (scale (cond ((zerop denom) 1d0)
                                        ((minusp denom) 0d0)
                                        (t (sqrt (/ (* threshold threshold) denom)))))
                           (new-xt (make-dvec n)))
                      (v+ pre-mu (v-scale v scale new-xt) new-xt)
                      (ts-ar::update-sdar sdar new-xt :discount discount))))
        (values pre-mu pre-v d state)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SNN
;; T.Ide, S.Papadimitriou, M.Vlachos
;; "Computing Correlation Anomaly Scores using Stochastic Nearest Neighbors"
;; 論文中の E score を target run および 複数の reference run から算出する。
;; target-run            : target runである時系列データによる numeric-dataset
;; reference-runs        : reference runである時系列データ達による numeric-dataset のリスト
;; k                     : 近傍数
;; sigma-list            : 各パラメータに対応するsigma_iの値のリスト、
;;                         並びはtarget-runやreference-runsのカラムの並びに対応
;;                         nilなら全て*default-sigma-i*の値となる
;; independent-pairs : 独立なパラメータ名のペア(cons) のリスト -> 相関を強制的に 0 にする
(defparameter *default-sigma-i* 1d0)
(defmethod e-scores ((target-run numeric-dataset)
                     reference-runs
                     &key (k 3)
                          (sigma-list nil)
                          (independent-pairs nil)
                          (benchmarkp nil)
                          (smoothing nil)
                          (smoothing-args '(:trend-k 2 :trend-t^2 0.1d0))
                          (cor-alpha 0.05d0)
                          (cor-hash nil)
                          (print-status nil))
  (assert (and (typep target-run 'numeric-dataset)
               (typep reference-runs '(cons numeric-dataset))))
  (let* ((bench-obj (when benchmarkp (init-snn-benchmark)))
         (names (map 'list #'dimension-name (dataset-dimensions target-run)))
         (test-model 
          (progn 
            (when print-status (format t "Analyzing SNN structure for target run...~%"))
            (make-snn-model target-run 
                            :k k :sigma-list sigma-list
                            :independent-pairs independent-pairs
                            :smoothing smoothing
                            :smoothing-args smoothing-args
                            :cor-alpha cor-alpha
                            :cor-hash cor-hash
                            :bench bench-obj))))
    (when print-status )
    (loop with cum-e-scores = (make-list (length names) :initial-element 0d0)
        for i from 1
        for ref-d in reference-runs        
        as ref-model = (progn (assert (every #'string= names 
                                             (map 'list #'dimension-name 
                                                  (dataset-dimensions ref-d))))
                              (when print-status
                                (format t "Analyzing SNN structure for reference run ~A...~%" i))
                              (make-snn-model ref-d :k k :sigma-list sigma-list
                                              :independent-pairs independent-pairs
                                              :smoothing smoothing
                                              :smoothing-args smoothing-args
                                              :cor-alpha cor-alpha
                                              :cor-hash cor-hash
                                              :bench bench-obj))
        as e-scores = (bench-snn bench-obj :scoring (%e-scores test-model ref-model))
        do (loop for i from 0
               for e-score in e-scores
               do (incf (nth i cum-e-scores) e-score))
        collect ref-model into ref-models
        finally (return (if benchmarkp
                            bench-obj
                          (values (sort (mapcar (let ((n (length reference-runs)))
                                                  (lambda (name val) (cons name (/ val n))))
                                                names cum-e-scores)
                                        #'> :key #'cdr)
                                  test-model
                                  ref-models))))))
(defparameter *reference-fname* "ref-~D")
(defparameter *target-fname* "target")
;; input: data-dir, reference-run と target-run のCSVファイルが入ったディレクトリパス
;;                  reference-run のファイル名は ref-*.csv で * は 1 から連続する正数
;;                  target-run    のファイル名は target.csv
;;                  各CSVの全カラムは連続値データである。
;;        output, 結果出力ファイルパス
;;        format, :csv | :sexp 入力ファイル形式
;;        k, 近傍ノード数
;;        sigma-list, 各パラメータに対応するsigma_iの値のリスト、
;;                    並びはtarget-runやreference-runsのカラムの並びに対応
;;                    nilなら全て*default-sigma-i*の値となる
;;        knn-output-dir, 近傍情報出力ディレクトリ
(defun anomaly-detection-snn (data-dir output &key (format :csv) ;; :sexp | :csv
                                                   (k 3)
                                                   (sigma-list nil)
                                                   (knn-output-dir nil)
                                                   (external-format :default)
                                                   (print-status t))
  (when print-status (format t "Reading runs...~%"))
  (let* ((reference-fname (format nil "~A.~A" *reference-fname* 
                                  (ecase format (:csv "csv") (:sexp "sexp"))))
         (target-fname (format nil "~A.~A" *target-fname*
                               (ecase format (:csv "csv") (:sexp "sexp"))))
         (refs (loop for i from 1
                   as fname = (merge-pathnames (format nil reference-fname i)
                                               (pathname-as-directory data-dir))
                   while (probe-file fname) collect 
                     (read-run-file fname :format format :external-format external-format)))
         (target (read-run-file (merge-pathnames target-fname
                                                 (pathname-as-directory data-dir))
                                :format format :external-format external-format)))
    (assert (not (zerop (length refs))) () 
      "No reference run file. Reference run filename format must be ~A ." *reference-fname*)
    (assert target () "Unable to find target run file. It must be named ~A ." *target-fname*)
    (multiple-value-bind (result test-model ref-models) 
        (e-scores target refs :k k :sigma-list sigma-list :benchmarkp nil
                  :cor-alpha nil :smoothing nil :print-status print-status)
      (when print-status (format t "Writing results...~%"))
      (with-open-file (out output :direction :output :if-exists :supersede
                       :external-format external-format)
        (write (loop for (name . score) in result collect `(:name ,name :score ,score))
               :stream out))
      (when (and knn-output-dir (probe-directory knn-output-dir))
        (output-knn-info knn-output-dir test-model ref-models
                         :external-format external-format))
      (values result test-model ref-models))))
;; データを読み込む
(defun read-run-file (fname &key (format :sexp)
                                 (external-format :default))
  (assert (probe-file fname))
  (with-open-file (in fname :direction :input :external-format external-format)
    (let* ((params (ecase format
                     (:sexp (read in nil nil nil))
                     (:csv (csv::parse-csv-string (read-line in nil nil nil)))))
           (n (length params))
           (csv-type-spec (make-list n :initial-element 'double-float))
           (data 
            (ecase format
              (:sexp (loop for sexp = (read in nil nil nil) while sexp collect
                           (map 'vector (lambda (v) (dfloat v)) sexp)))
              (:csv (read-csv-stream in :header nil :type-spec csv-type-spec)))))
      (make-numeric-dataset (coerce params 'list)
                            (map 'vector (lambda (v) (coerce v 'dvec)) data)))))
;; knn構造情報をダンプする
(defun output-knn-info (output-dir test-model ref-models
                        &key (external-format :default))
  (loop for ref-model in ref-models
      for i from 1
      as fname = (merge-pathnames (concatenate 'string 
                                    (format nil *reference-fname* i) ".sexp")
                                  (pathname-as-directory output-dir))
      do (%output-knn-info fname ref-model :external-format external-format))
  (%output-knn-info (merge-pathnames (concatenate 'string *target-fname* ".sexp")
                                     (pathname-as-directory output-dir))
                    test-model :external-format external-format))
(defun %output-knn-info (fname model &key (external-format :default))
  (let ((names (map 'vector #'dimension-name (dataset-dimensions (dataset model)))))
    (with-open-file (out fname :direction :output :if-exists :supersede
                     :external-format external-format)
      (write (loop for snn in (snns model)
                 as name = (aref names (cnode snn))
                 as nbrs = (nbrs snn) collect
                   `(:name ,name :neighbors 
                           ,(mapcar (lambda (nbr)
                                      (let ((name (aref names (car nbr)))
                                            (cp (getf (cdr nbr) :cp))
                                            (d (getf (cdr nbr) :d)))
                                        `(,name :coupling-probability ,cp
                                                :dissimilarity ,d))) nbrs)))
             :stream out))))

(defclass snn () ;; Stochastic-Nearest-Neighbor
  ((cnode :initarg :cnode :accessor cnode)  ;; central node id
   (nbrs  :initarg :nbrs  :accessor nbrs))) ;; neighbors (cons id `(:cp ** :d **))

(defclass snn-model ()
  ((dataset :initarg :dataset :accessor dataset)
   (snns    :initarg :snns    :accessor snns)))

(defmethod print-neighbors ((model snn-model) name)
  (let* ((names (map 'vector #'dimension-name (dataset-dimensions (dataset model))))
         (id (position name names :test #'equal)))
    (if (integerp id)
        (let ((snn (nth id (snns model))))
          (with-accessors ((cnode cnode) (nbrs nbrs)) snn
            (assert (eql cnode id))
            (if (listp nbrs)
                (progn (princ (format nil "Neighbor : Pji~%"))
                       (loop for (nbr . prob) in nbrs
                           as name1 = (aref names nbr)
                           do (princ (format nil "~A : ~A~%" name1 prob))))
              (princ (format nil "No neighbor~%")))))
      (error "There is no ~A" name))))

(defmethod make-snn-model ((d numeric-dataset) &key (k nil) 
                                                    (independent-pairs nil)
                                                    (sigma-list nil)
                                                    (bench nil)
                                                    (smoothing nil)
                                                    (smoothing-args nil)
                                                    (cor-alpha nil)
                                                    (cor-hash nil))
  (assert (and (numberp k)))
  (let* ((wmat (bench-snn bench :w-mat 
                          (dissimilarity-matrix d :smoothing smoothing
                                                :smoothing-args smoothing-args
                                                :independent-pairs independent-pairs
                                                :cor-alpha cor-alpha
                                                :cor-hash cor-hash)))
         (size (length (dataset-dimensions d)))
         (%nbrs-list (bench-snn bench :knn (knn-list wmat k)))
         (nbrs-list (bench-snn bench :c-p (coupling-probabilities %nbrs-list sigma-list)))
         (snns (loop for i below size
                   for nbrs in nbrs-list
                   collect (make-instance 'snn :cnode i :nbrs nbrs))))
    (assert (equal `(,size ,size) (array-dimensions wmat)))
    (make-instance 'snn-model :dataset d :snns snns)))

(defmethod dissimilarity-matrix ((d numeric-dataset) &key (independent-pairs nil)
                                                          (smoothing nil)
                                                          (smoothing-args nil)
                                                          (cor-alpha nil)
                                                          (cor-hash nil))
  (let* ((names (map 'list #'dimension-name (dataset-dimensions d)))
         (independent-index-pairs (mapcar (lambda (pair)
                                            (cons (position (car pair) names :test #'string=)
                                                  (position (cdr pair) names :test #'string=)))
                                          independent-pairs))
         (pts (dataset-numeric-points d))
         (cor (correlation-with-constant pts 
                                         :absolute nil
                                         :alpha cor-alpha
                                         :smoothing smoothing
                                         :smoothing-args smoothing-args
                                         :sig-p-hash cor-hash))
         (n (first (array-dimensions cor)))
         (a (make-array `(,n ,n) :element-type 'double-float)))
    (declare (type dmat cor a) (optimize speed))
    (flet ((almost= (a b) (<= (- b *epsilon*) a (+ b *epsilon*))))
      (loop for i below n
          do (loop for j to i
                 as cij double-float = (aref cor i j)
                 as aij double-float = (cond ((zerop cij) (kronecker-delta i j))
                                             (t cij))
                 as d double-float = (cond
                                      ((and independent-pairs 
                                            (or (find (cons i j) independent-index-pairs
                                                      :test #'equal)
                                                (find (cons j i) independent-index-pairs
                                                      :test #'equal)))
                                       most-positive-double-float)
                                      ((almost= aij 0d0) *+inf*)
                                      ;; infinite dissimilarity
                                      ((almost= aij 1d0) 0d0)
                                      ((almost= aij -1d0) 0d0)
                                      ((< 0d0 (abs aij) 1d0)
                                       (- (log (the double-float (abs aij)))))
                                      (t (error "Unexpected value for correlation ~A (~A ~A)"
                                                aij i j)))
                 do (setf (aref a i j) d (aref a j i) d))
          finally (return a)))))

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

(defun coupling-probabilities (nbrs-d-list sigma-list)
  (flet ((z_i (i sigma)
           (let ((nbrs-d (nth i nbrs-d-list)))
             (if (> (length nbrs-d) 0)
                 (+ 1d0 (loop for j-d in nbrs-d
                            sum (handler-case (exp (- (/ (the double-float (cdr j-d)) sigma)))
                                  (FLOATING-POINT-UNDERFLOW (c) (declare (ignore c)) 0d0))))
               1d0))))
    (loop for i from 0
        for nbrs-d in nbrs-d-list
        as sigma = (if (typep sigma-list '(cons number)) (nth i sigma-list)
                     *default-sigma-i*)
        collect (loop for (j . d) in nbrs-d
                    as prob = (/ (handler-case
                                     (exp (- (/ (the double-float d) sigma)))
                                   (FLOATING-POINT-UNDERFLOW (c) (declare (ignore c)) 0d0))
                                 (z_i i sigma))
                    collect (cons j `(:cp ,prob :d ,d))))))

(defun coupling-tightness (nbrs cp-alist)
  (if (> (length nbrs) 0)
      (loop for j in nbrs
          as prob = (getf (cdr (assoc j cp-alist :test #'eql)) :cp)
          sum (if prob prob 0d0))
    0d0))

(defun e-score (param-index test-model ref-model)
  (let* ((test-cp-alist (nbrs (nth param-index (snns test-model))))
         (ref-cp-alist (nbrs (nth param-index (snns ref-model))))
         (test-nbrs (mapcar #'car test-cp-alist))
         (ref-nbrs (mapcar #'car ref-cp-alist)))
    (max (abs (- (coupling-tightness test-nbrs test-cp-alist)
                 (coupling-tightness test-nbrs ref-cp-alist)))
         (abs (- (coupling-tightness ref-nbrs test-cp-alist)
                 (coupling-tightness ref-nbrs ref-cp-alist))))))                                     

(defmethod %e-scores ((test-model snn-model) (ref-model snn-model))
  (let ((test-d (dataset test-model))
        (ref-d (dataset ref-model)))
    (assert (loop for n1 in (map 'list #'dimension-name (dataset-dimensions test-d))
                for n2 in (map 'list #'dimension-name (dataset-dimensions ref-d))
                always (equalp n1 n2)))      
    (loop for i below (length (dataset-dimensions test-d))
        collect (e-score i test-model ref-model))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EEC
;; S.Hirose, K.Yamanishi, T.Nakata, R.Fujimaki
;; "Network Anomaly Detection based on Eigen Equation Compression"
;; を元にした異常検出
(defclass anomaly-detector-eec ()
  ((window :initform nil :initarg :window :accessor window)
   (window-size :initform nil :initarg :window-size :accessor window-size)
   (params :initform nil :initarg :params :accessor params)
   (xi :initform nil :initarg :xi :accessor xi)
   (global-m :initform nil :initarg :global-m :accessor global-m)
   (global-cov :initform nil :initarg :global-cov :accessor global-cov)
   (local-covs :initform nil :initarg :local-covs :accessor local-covs)
   (scoring :initform :mahalanobis :initarg :scoring :accessor scoring) ;; :mahalanobis | :log-pdf
   (pc :initform nil :initarg :pc :accessor pc)
   (beta :initform nil :initarg :beta :accessor beta)
   (global-moments :initform nil :initarg :global-moments :accessor global-moments)
   (local-moments-list :initform nil :initarg :local-moments-list :accessor local-moments-list)
   (param-graph :initform nil :initarg :param-graph :accessor param-graph)
   (new-params :initform nil :initarg :new-params :accessor new-params)
   (sig-alpha :initform nil :initarg :sig-alpha :accessor sig-alpha)
   (sig-hash :initform nil :initarg :sig-hash :accessor sig-hash)
   (smoothing :initform nil :initarg :smoothing :accessor smoothing-p)
   (smoothing-args :initform nil :initarg :smoothing-args :accessor smoothing-args)))

;; input : input, formatで指定する形式のファイル
;;         output, 結果出力ファイル名
;;         format, :sexp | :csv, :sexpはたとえば sample/UKgas.sexp の全体を一つの括弧で囲わない形
;;         time-label-name, 時間ラベル列名
;;         xi, 相関強度閾値
;;         global-m, 大域的特徴量のためのm 論文3.3
;;         scoring, :mahalanobis のみ
;;         pc, 閾値のための上側累積確率
;;         beta, 閾値のための忘却パラメータ　
;;         cor-output, 相関情報出力ファイル名, nilなら出力しない
(defun anomaly-detection-eec (input output window-size
                              &key (format :sexp) ;; :sexp | :csv
                                   (time-label-name "Time")
                                   (xi 0.8d0)
                                   (global-m 3)
                                   (scoring :mahalanobis)
                                   (pc 0.05d0)
                                   (beta nil)
                                   (param-graph nil)
                                   (sig-alpha 0.01d0)
                                   (smoothing nil)
                                   (smoothing-args '(:trend-k 2 :trend-t^2 0.1d0))
                                   (initial-global-cov nil)
                                   (initial-local-covs nil)
                                   (initial-global-moments nil)
                                   (initial-local-moments-list nil)
                                   (external-format :default)
                                   (cor-output nil)
                                   (print-status t)
                                   (benchmarkp nil))
  (macrolet ((with-cor-out (cor-output &body body)
               `(if ,cor-output (with-open-file (cor-out ,cor-output 
                                                 :direction :output :external-format external-format
                                                 :if-exists :supersede) ,@body)
                  (let ((cor-out nil)) ,@body))))
    (with-cor-out cor-output
      (with-open-file (in input :direction :input :external-format external-format)
        (with-open-file (out output :direction :output :external-format external-format
                         :if-exists :supersede)
          (multiple-value-bind (params time-pos)
              (read-name-part in time-label-name :format format)
            (setq params (coerce params 'list))
            (unless time-pos (error "Unable to find time label: ~A" time-label-name))
            (let ((dim (length params))
                  detector)
              (setq detector
                (loop repeat (1+ window-size)
                    as vec = (read-value-part in time-pos dim :format format)
                    initially (when print-status (format t "Initialize detector...~%"))
                    collect vec into vecs
                    finally (return (init-anomaly-detector-eec 
                                     (coerce vecs 'vector)
                                     params
                                     window-size
                                     :xi xi
                                     :global-m global-m
                                     :scoring scoring
                                     :pc pc
                                     :beta beta
                                     :param-graph param-graph
                                     :sig-alpha sig-alpha
                                     :smoothing smoothing
                                     :smoothing-args smoothing-args
                                     :initial-global-cov initial-global-cov
                                     :initial-local-covs initial-local-covs
                                     :initial-global-moments initial-global-moments
                                     :initial-local-moments-list initial-local-moments-list
                                     :benchmarkp benchmarkp))))
              (when print-status (format t "Detecting anomaly...~%"))
              (loop with ut = (get-universal-time)
                  for i from 1
                  for (vec time-label)
                  = (multiple-value-list (read-value-part in time-pos dim :format format))
                  initially (format out "(")
                  while vec
                  as (result cor-str-mat)
                  = (multiple-value-list (update-anomaly-detector-eec detector vec))
                  do (write `(:time ,time-label ,@result) :stream out)
                     (terpri out)
                     (when cor-output
                       (write `(:time ,time-label :names ,(params detector)
                                      :cor-strength ,cor-str-mat)
                              :stream cor-out)
                       (terpri cor-out))
                  when (and print-status (zerop (mod i *comment-interval*))) do
                    (format t "Now at ~A : ~,5F sec/line~%"
                            time-label (/ (- (get-universal-time) ut) i))
                  finally (format out ")"))
              detector)))))))
(defun read-name-part (stream time-label-name &key (format :sexp))
  (let ((parsed-names))
    (ecase format
      (:sexp (let ((sexp (read stream nil nil nil)))
               (setq parsed-names sexp)))
      (:csv (let ((line (read-line stream nil nil nil)))
              (when line (let ((strs (csv::parse-csv-string line)))
                           (setq parsed-names strs))))))
    (when parsed-names
      (values (remove time-label-name parsed-names :test #'string=)
              (position time-label-name parsed-names :test #'string=)))))
(defun read-value-part (stream time-pos dim &key (format :sexp))
  (let ((%vals nil) time-label)
    (ecase format
      (:sexp (let ((sexp (read stream nil nil nil)))
               (when sexp
                 (let ((vals (loop for i from 0
                                 for val in sexp
                                 unless (= i time-pos)
                                 collect (progn (assert (numberp val)) (dfloat val)))))
                   (assert (eql dim (length vals)))
                   (setq %vals vals
                         time-label (nth time-pos sexp))))))
      (:csv (let ((line (read-line stream nil nil nil)))
              (when line
                (let* ((strs (csv::parse-csv-string line))
                       (vals (loop for i from 0
                                 for str across strs
                                 unless (= i time-pos)
                                 collect (let ((val (read-from-string str)))
                                           (assert (numberp val))
                                           (dfloat val)))))
                  (assert (eql dim (length vals)))
                  (setq %vals vals
                        time-label (aref strs time-pos)))))))
    (when %vals
      (values (make-array dim :element-type 'double-float :initial-contents %vals)
              time-label))))

(defun init-anomaly-detector-eec (points params window-size &key (xi 0.8d0)
                                                                 (global-m 3)
                                                                 (scoring :mahalanobis)
                                                                 (pc 0.05d0)
                                                                 (beta nil)
                                                                 (param-graph nil)
                                                                 (sig-alpha 0.05d0)
                                                                 (smoothing nil)
                                                                 (smoothing-args nil)
                                                                 (initial-global-cov nil)
                                                                 (initial-local-covs nil)
                                                                 (initial-global-moments nil)
                                                                 (initial-local-moments-list nil)
                                                                 (benchmarkp nil))
  (declare (type (vector dvec) points))
  (check-type points (vector dvec))
  (assert (>= (length points) (1+ window-size)))
  ;; 特徴量の分散の初期値を得るため、特徴量は最低限 2 つ必要

  (loop with bench = (when benchmarkp (init-eec-benchmark))
      with beta = (if (numberp beta) beta (dfloat (/ window-size)))
      with detector = (make-instance 'anomaly-detector-eec
                        :window-size window-size :params params
                        :xi xi :global-m global-m :scoring scoring
                        :pc pc :beta beta :param-graph param-graph
                        :smoothing smoothing
                        :smoothing-args smoothing-args
                        :sig-alpha sig-alpha
                        :sig-hash (when (numberp sig-alpha)
                                    (statistics::make-sig-p-hash window-size sig-alpha)))
      for start from 0
      for end from window-size to (length points)
      as window = (subseq points start end)
      as (global locals) = 
        (progn (setf (window detector) window)
               (multiple-value-list
                (calc-eec-features detector :bench bench)))
      collect global into globals
      collect locals into locals-list
      finally (progn
                (bench-eec bench :scoring nil
                           (let ((global-cov
                                  (if initial-global-cov initial-global-cov
                                    (init-covariance (coerce globals 'vector)
                                                     (coerce globals 'vector))))
                                 (local-covs 
                                  (if initial-local-covs initial-local-covs
                                    (loop for i below (length params)
                                        as locals = (mapcar (lambda (lis) (nth i lis)) locals-list)
                                        collect (init-matrix-covariance (coerce locals 'vector))))))
                             (setf (global-cov detector) global-cov
                                   (local-covs detector) local-covs
                                   (global-moments detector) initial-global-moments
                                   (local-moments-list detector) initial-local-moments-list)))
                (return (if benchmarkp (values detector bench) detector)))))

(defmethod update-anomaly-detector-eec ((detector anomaly-detector-eec) point
                                        &key (new-param-pos-alist nil)
                                             (removed-params nil)
                                             (new-param-graph nil)
                                             (bench nil))
  (declare (type dvec point))
  (check-type point dvec)
  (with-accessors ((global-cov global-cov)
                   (local-covs local-covs)
                   (%params params)
                   (scoring scoring)) detector
    (when (typep new-param-graph 'simple-graph) (setf (param-graph detector) new-param-graph))
    (update-params detector point new-param-pos-alist removed-params)
    (update-window detector point)
    (multiple-value-bind (global locals cor-str-mat)
        (calc-eec-features detector :bench bench)
      (bench-eec bench :scoring t
                 (let* ((g-score (global-anomaly-score global 
                                                       (x-mean-vec global-cov)
                                                       (get-covariance global-cov)))
                        (l-scores (loop for local in locals
                                      for cov in (mapcar #'get-matrix-covariance local-covs)
                                      for mu in (mapcar #'mean-mat local-covs)
                                      as score = (local-anomaly-score local mu cov :type scoring)
                                      collect score))
                        (g-thld (update-global-moments detector g-score))
                        (l-thlds (update-local-moments detector l-scores)))
                   (multiple-value-prog1
                       (values 
                           `(:global-score ,g-score :global-threshold ,g-thld
                                           :locals
                                           ,(loop for i from 0
                                                for param in %params
                                                for l-score in l-scores
                                                for l-thld in l-thlds
                                                collect `(:name ,param
                                                                :score ,l-score
                                                                :threshold ,l-thld)))
                           cor-str-mat)
                     (update-covariance global-cov global global)
                     (loop for local in locals
                         for cov-obj in local-covs
                         do (update-matrix-covariance cov-obj local))))))))

(defmethod update-window ((detector anomaly-detector-eec) point)
  (declare (type dvec point))
  (let* ((n (length (params detector)))
         (new-param-poss (mapcar (lambda (plis) (getf plis :pos)) (new-params detector)))
         (new-point
          (if (eql (length point) n) point
            (loop with new-point = (make-dvec (length (params detector)))
                with new-pos = 0
                for i from 0
                for val across point
                unless (member i new-param-poss :test #'eql)
                do (setf (aref new-point new-pos) val
                         new-pos (1+ new-pos))
                finally (return new-point)))))
    (setf (window detector) (concatenate 'vector
                              (subseq (window detector) 1) `(,new-point)))))
(defmethod update-params ((detector anomaly-detector-eec) point new-param-pos-alist removed-params)
  (assert (eql (length point) (+ (length (params detector))
                                 (length (new-params detector))
                                 (length new-param-pos-alist)
                                 (- (length removed-params)))))
  (when removed-params (remove-params detector removed-params new-param-pos-alist))  
  (new-params-to-params detector)
  (add-new-params detector point new-param-pos-alist))
(defmethod remove-params ((detector anomaly-detector-eec) target-params new-param-pos-alist)
  (setf (new-params detector) 
    (loop with new-poss = (mapcar #'cdr new-param-pos-alist)
        with removed-poss = (mapcar (lambda (param) (position param (params detector) :test #'equal))
                                    target-params)
        for plis in (new-params detector)
        as new-param = (getf plis :name)
        as pos = (getf plis :pos)
        unless (member new-param target-params :test #'equal)
        collect (let ((new-pos pos))
                  (setf new-pos (- new-pos (count-if (lambda (rpos) (< rpos new-pos)) 
                                                     removed-poss :test #'eql))
                        new-pos (- new-pos (count-if (lambda (npos) (<= npos new-pos))
                                                     new-poss :test #'eql)))
                  `(:name ,new-param :pos ,new-pos :vals ,(getf plis :vals)))))
  (loop for i from 0
      for param in (params detector)
      for vec across (transposev (window detector))
      for local-cov in (local-covs detector)
      as local-moments = (nth i (local-moments-list detector))
      unless (member param target-params :test #'equal)
      collect param into params and
      collect vec into vecs and
      collect local-cov into local-covs and
      collect local-moments into local-moments-list
      finally (setf (params detector) params
                    (window detector) (transposev (coerce vecs 'vector))
                    (local-covs detector) local-covs
                    (local-moments-list detector) local-moments-list)))
(defmethod add-new-params ((detector anomaly-detector-eec) point new-param-pos-alist)
  (declare (type dvec point))  
  (loop for plis in (new-params detector)
      as pos = (getf plis :pos)
      as val = (aref point pos)
      do (push val (getf plis :vals)))
  (loop for (new-param . pos) in new-param-pos-alist
      as val = (aref point pos)
      as plis = `(:name ,new-param :pos ,pos :vals (,val))
      do (push plis (new-params detector))))
(let ((last-window))
  (defmethod new-params-to-params ((detector anomaly-detector-eec))
    (with-accessors ((w window-size)
                     (new-params new-params)
                     (params params)
                     (l-moms-list local-moments-list)
                     (local-covs local-covs)) detector
      ;; w + 1 の処理, params への移動
      (setf new-params
        (loop with w+1 = (1+ w)
            for plis in new-params
            as vals = (getf plis :vals)
            as n = (length vals)
            if (= n w+1)
            collect (getf plis :name) into %params and
            collect (make-array w+1 :element-type 'double-float
                                :initial-contents (reverse vals)) into new-vecs and
            collect (getf plis :pos) into new-poss
            else
            collect plis
            finally
              (when %params
                (setf params (insert-values params %params new-poss)
                      l-moms-list (insert-values l-moms-list nil new-poss))
                (update-local-covs detector new-vecs new-poss last-window))))
      ;; w の処理
      (setf last-window
        (loop for plis in (new-params detector)
            as vals = (getf plis :vals)
            as n = (length vals)
            when (= n w)
            return (map 'vector #'copy-seq (window detector)))))))
(defmethod update-local-covs ((detector anomaly-detector-eec) new-vecs new-poss last-window)
  (let ((new-local-mats-list)
        (new-local-covs))
    (setf new-local-mats-list
      (loop with current-window = (map 'vector #'copy-seq (window detector))
          with w = (window-size detector)
          for start to 1
          as end = (+ start w)
          as %new-vecs = (mapcar (lambda (vec) (subseq vec start end)) new-vecs)
          as new-window = (transposev (coerce
                                       (insert-values (coerce (transposev 
                                                               (if (eql start 0) last-window 
                                                                 current-window))
                                                              'list)
                                                      %new-vecs
                                                      new-poss)
                                       'vector))
          collect (progn (setf (window detector) new-window)
                         (multiple-value-bind (_ local-mats)
                             (calc-eec-features detector)
                           (declare (ignore _))
                           (mapcar (lambda (pos) (nth pos local-mats)) new-poss))))
      new-local-covs (loop for i below (length new-poss)
                         as local-mats = (mapcar (lambda (mats) (nth i mats)) new-local-mats-list)
                         collect (init-matrix-covariance (coerce local-mats 'vector)))
      (local-covs detector)
      (insert-values (local-covs detector)
                     new-local-covs
                     new-poss))))

(defmethod update-global-moments ((detector anomaly-detector-eec) g-score)
  (with-accessors ((moms global-moments)) detector
    (multiple-value-bind (thld new-mom) (score-threshold g-score
                                                         :1st-moment (car moms)
                                                         :2nd-moment (cdr moms)
                                                         :beta (beta detector)
                                                         :pc (pc detector))
      (setf (global-moments detector) new-mom)
      thld)))
(defmethod update-local-moments ((detector anomaly-detector-eec) l-scores)
  (with-accessors ((moms-list local-moments-list)) detector
    (loop for i from 0
        for l-score in l-scores
        as moms = (nth i moms-list)
        as (thld new-moms) = (multiple-value-list
                              (score-threshold l-score
                                               :1st-moment (car moms)
                                               :2nd-moment (cdr moms)
                                               :beta (beta detector)
                                               :pc (pc detector)))
        collect thld into l-thlds
        collect new-moms into new-moms-list
        finally (return (progn (setf (local-moments-list detector) new-moms-list)
                               l-thlds)))))

;;;;;;;;;;;;;;
; 特徴量算出 ;
;;;;;;;;;;;;;;
;; output: global 特徴量(vector)
;;         local 特徴量(3x3matrix) のリスト ( 順番は入力 d のカラム名に対応 )
;;         clustering 結果
(defmethod calc-eec-features ((detector anomaly-detector-eec) &key (bench nil))
  (with-accessors ((pts window)
                   (params params)
                   (global-m global-m)
                   (alpha sig-alpha)
                   (hash sig-hash)) detector
    (let* ((dim (length params))
           (weight-mat (bench-eec bench :cor nil (get-weight-mat detector)))
           (aij (bench-eec bench :cor t 
                           (correlation-with-constant pts
                                                      :absolute t
                                                      :weight-mat weight-mat
                                                      :smoothing (smoothing-p detector)
                                                      :smoothing-args (smoothing-args detector)
                                                      :alpha alpha
                                                      :sig-p-hash hash))))
      ;; All elements of aij must be non negative
      ;; for Perron-Frobenius theorem.
      (assert (<= global-m dim))
      (assert (= (length pts) (window-size detector)))
      (multiple-value-bind (global-feature princ-eigen-vec)
          (calc-global-feature aij :global-m global-m :bench bench :principal t)
        (bench-eec bench :compression t
                   (multiple-value-bind (local-features clusterings)
                       (calc-local-features dim princ-eigen-vec aij :xi (xi detector))
                     (declare (ignore clusterings))
                     (values global-feature
                             local-features
                             aij)))))))
(defun calc-global-feature (cor-mat &key (global-m 3) (bench nil) (principal nil) (abstol 1d-12))
  (multiple-value-bind (eigen-vals eigen-mat) 
      (bench-eec bench :eigen t 
                 #+mkl 
                 (symat-ev (copy-mat cor-mat) :eigen-thld global-m :abstol abstol)
                 #-mkl
                 (eigen-by-power (copy-mat cor-mat) :eigen-thld global-m :precision abstol))
    (if principal
        (let ((principal (make-dvec (array-dimension cor-mat 0))))
          (do-vec (_ principal :type double-float :index-var i :setf-var sf)
            (declare (ignore _))
            (setf sf (aref eigen-mat 0 i))) ;; section 3.3
          (values (round-vec eigen-vals) (round-vec principal)))
      (round-vec eigen-vals))))
(defun calc-local-features (dim psi-vec cor-strength-mat &key (xi 0.8d0)
                                                              (clusters-list nil))
  (assert (equal `(,dim ,dim) (array-dimensions cor-strength-mat)))
  (loop for i below dim
      as %clusters = (when clusters-list (nth i clusters-list))
      as (mat clusters) = (multiple-value-list (calc-local-feature i dim cor-strength-mat psi-vec 
                                                                   :xi xi :clusters %clusters))
      collect mat into mats
      collect clusters into clusterings
      finally (return (values mats clusterings))))
(defun calc-local-feature (index dim cor-str-mat psi 
                           &key (xi 0.8d0) (clusters nil))
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
  (declare (type dvec psi))
  (let* ((pck*psi (proj-by-ck psi ck dim))
         (denom (sqrt (inner-product psi pck*psi))))
    (cond ((zerovecp pck*psi) (make-dvec dim 0d0))
          ((zerop denom) (error "Unexpected situation : ~A" pck*psi))
          (t (do-vec (val pck*psi :type double-float :setf-var sf :return pck*psi)
               (setf sf (/ val denom)))))))
(defun proj-by-ck (vec ck dim)
  "クラスタリング ck による射影"
  (loop with projed = (make-dvec (length vec) 0d0)
      for i in ck
      do (assert (< i dim))
         (setf (aref projed i) (aref vec i))
      finally (return projed)))
(defmethod get-weight-mat ((detector anomaly-detector-eec))
  (with-accessors ((params params)
                   (gr param-graph)) detector
    (let ((dim (length params)))
      (if (typep gr 'simple-graph)
          (with-accessors ((nodes nodes)) gr
            (let* ((dist-mat (graph-distance-matrix gr))
                   (name-pos-alist (mapcar (lambda (node) (cons (node-name node)
                                                                (1- (node-id node))))
                                           nodes))
                   (weight-mat (make-array `(,dim ,dim) :element-type 'double-float)))
              (assert (subsetp params (mapcar #'car name-pos-alist) :test #'equal))
              (loop for col below dim
                  for name1 in params
                  as pos1 = (cdr (assoc name1 name-pos-alist :test #'equal))
                  do (loop for row below dim
                         for name2 in params
                         as pos2 = (cdr (assoc name2 name-pos-alist :test #'equal))
                         as val double-float = (aref dist-mat pos1 pos2)
                         do (setf (aref weight-mat col row)
                              (if (zerop val) 1d0 (/ val))))
                  finally (return weight-mat))))
        (make-array `(,dim ,dim) :element-type 'double-float :initial-element 1d0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; anomaly score for characteristic value ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun global-anomaly-score (target mu sigma &key (type :mahalanobis)) ; :mahalanobis | :mnd
  (ecase type
    (:mahalanobis (mahalanobis-distance target mu sigma))
    (:mnd (let ((density (multivariate-normal-density mu sigma target)))
            (- (log 
                (cond ((zerop density) least-positive-double-float)
                      ((= #.*INFINITY-DOUBLE* density) most-positive-double-float)
                      (t density))))))))
(defun local-anomaly-score (target mu sigma &key (type :mahalanobis)) ; :mahalanobis | :mnd
  (let* ((dims (array-dimensions sigma))
         (target-vec (vectorization (transpose target)))
         (mu-vec (vectorization (transpose mu))))
    (assert (= (length mu-vec) (length target-vec)
               (first dims) (second dims)))
    (ecase type
      (:mahalanobis (mahalanobis-distance target-vec mu-vec sigma))
      (:mnd (let ((density (multivariate-normal-density mu-vec sigma target-vec)))
              (- (log 
                  (cond ((zerop density) least-positive-double-float)
                        ((= #.*INFINITY-DOUBLE* density) most-positive-double-float)
                        (t density)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; anomaly score の threshold ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; - 1次のモーメント, 2次のモーメントを元に type で指定した確率分布を推定し、
;;   上側累積確率 pc としての閾値を算出する。
;; - score, beta より 1次／2次のモーメントを更新する。 ref Ide & Kashima
;; output: 閾値
;;         (cons 新・1次のモーメント 新・2次のモーメント)
(defun score-threshold (score &key (1st-moment nil)
                                   (2nd-moment nil)
                                   (type :normal) ;; :normal | :log-normal | :gamma
                                   (beta nil)
                                   (pc 0.05d0))
  (assert (and (< 0 beta 1) (< 0 pc 1)))
  (flet ((var (mom1 mom2) (- mom2 (expt mom1 2))))
    (let ((pdf (ecase type
                 (:normal
                  (unless 1st-moment (setq 1st-moment 0d0))
                  (unless 2nd-moment (setq 2nd-moment 1d0))
                  (normal-distribution 1st-moment (sqrt (var 1st-moment 2nd-moment))))
                 (:log-normal
                  (unless 1st-moment (setq 1st-moment 1d0))
                  (unless 2nd-moment (setq 2nd-moment 2d0))
                  (let ((params (log-normal-params 1st-moment (var 1st-moment 2nd-moment))))
                    (log-normal-distribution (getf params :mu) (getf params :sigma))))
                 (:gamma
                  (unless 1st-moment (setq 1st-moment 1d0))
                  (unless 2nd-moment (setq 2nd-moment 2d0))
                  (let ((params (gamma-params 1st-moment (var 1st-moment 2nd-moment))))
                    (gamma-distribution (getf params :scale) (getf params :shape))))))
          (lower-pc (- 1d0 pc)))
      (values (if (typep pdf 'statistics::gamma-like-distribution)
                  (quantile-ili pdf lower-pc) (quantile pdf lower-pc))
              (next-moments score (cons 1st-moment 2nd-moment) beta)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utils for anomaly-detection

;; ベクトル値データの分散共分散行列算出クラス
;; - 初期化: init-covariance
;; - 更新: update-covariance
;; - 共分散値: get-covariance
;; - 平均値: mean-vec
(defclass covariance ()
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
    (make-instance 'covariance :xy-mat-expec xy-mat :x-mean-vec x-mu :y-mean-vec y-mu :n n)))
(defmethod update-covariance ((cov covariance) x-vec y-vec)
  (let ((new-xy-mat (xy-mat x-vec y-vec)))
    (setf (xy-mat-expec cov) (update-mat-expec (xy-mat-expec cov) (n cov) new-xy-mat)
          (x-mean-vec cov) (update-vec-expec (x-mean-vec cov) (n cov) x-vec)
          (y-mean-vec cov) (update-vec-expec (y-mean-vec cov) (n cov) y-vec)
          (n cov) (1+ (n cov)))
    cov))
(defmethod get-covariance ((cov covariance))
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
(defun get-row-series (n hash)
  (coerce (reverse (gethash n hash)) 'vector))
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

;; 片方の変数が一定だった場合、相関係数が 0 になるような
;; 相互相関行列を算出
(defun correlation-with-constant (pts &key (absolute nil)
                                           (weight-mat nil)
                                           (smoothing nil)
                                           (smoothing-args '(:trend-k 2 :trend-t^2 0.1d0))
                                           (alpha nil)
                                           (sig-p-hash nil))
  (declare (type (vector dvec) pts))
  (check-type pts (vector dvec))
  (if (or smoothing alpha)
      (correlation-with-constant-and-cleaning pts 
                                              :absolute absolute 
                                              :smoothing smoothing
                                              :smoothing-args smoothing-args
                                              :alpha alpha
                                              :sig-p-hash sig-p-hash)
    (let* ((cov (covariance-matrix pts))
           (n (array-dimension cov 0))
           (a (make-array `(,n ,n) :element-type 'double-float))
           (weight-mat (if (arrayp weight-mat) weight-mat
                         (make-array `(,n ,n) :element-type 'double-float :initial-element 1d0))))
      (declare (type dmat cov a) (type fixnum n) (optimize speed))
      (flet ((noise-cut (val) (if (> *epsilon* (abs val)) 0d0 val)))
        (loop for i below n
            do (loop for j to i
                   as cij double-float = (noise-cut (aref cov i j))
                   as cii double-float = (noise-cut (aref cov i i))
                   as cjj double-float = (noise-cut (aref cov j j))
                   as aij double-float = (if (or (zerop cii) (zerop cjj)) (kronecker-delta i j)
                                           (/ (the double-float cij)
                                              (the double-float (sqrt (* cii cjj)))))
                   do (when absolute (setf aij (abs aij)))
                      (setf (aref a i j) (round-value (* (aref weight-mat i j) aij))
                            (aref a j i) (round-value (* (aref weight-mat j i) aij))))
            finally (return a))))))
(defun correlation-with-constant-and-cleaning (pts &key (absolute nil)
                                                        (weight-mat nil)
                                                        (smoothing nil)
                                                        (smoothing-args
                                                         '(:trend-k 2 :trend-t^2 0.1d0))
                                                        (alpha nil)
                                                        (sig-p-hash nil))
  (declare (type (vector dvec) pts))
  (check-type pts (vector dvec))
  (let* ((tr-window (window-cleaning pts :smoothing smoothing :smoothing-args smoothing-args
                                     :alpha alpha :sig-p-hash sig-p-hash))
         (dim (length tr-window))
         (result (make-array (list dim dim) :element-type 'double-float))
         (weight-mat (if (arrayp weight-mat) weight-mat
                       (make-array `(,dim ,dim) :element-type 'double-float :initial-element 1d0))))
    (loop for i below dim 
        as v1 = (aref tr-window i) do
          (loop for j to i 
              as v2 = (aref tr-window j)
              as cor = (if (eql i j) 1d0 (correlation-with-nan v1 v2)) do
                (when absolute (setf cor (abs cor)))
                (setf (aref result i j) (round-value (* (aref weight-mat i j) cor))
                      (aref result j i) (round-value (* (aref weight-mat j i) cor))))
        finally (return result))))

;; 窓の掃除
;; 外れ値除去 -> 平滑化
;; どちらか片方だけやることもできる。
;; 両方やる場合で、トレンド平滑化のときは、外れ値は補間される。
;; 外れ値は *nan* になる
;; 返り値は窓の転置
(defun window-cleaning (window &key (smoothing nil) ;; nil | t
                                    (smoothing-args '(:trend-k 2 :trend-t^2 0.1d0))
                                    (alpha nil) ;; nil | <number>
                                    (sig-p-hash nil) ;; nil | hash-table (ref. #'make-sig-p-hash)
                                    )
  (declare (type (vector dvec) window))
  (check-type window (vector dvec))
  (let ((tr-window (transposev window)))
    (when (and (numberp alpha) (plusp alpha))
      (setf tr-window 
        (map 'vector (lambda (dvec) (sweep-outlier dvec :alpha alpha :sig-p-hash sig-p-hash))
             tr-window)))
    (if smoothing
        (map 'vector (lambda (dvec) (apply #'smoothing dvec smoothing-args)) tr-window)
      tr-window)))
(defun correlation-with-nan (v1 v2)
  (declare (type dvec v1 v2) (optimize speed))
  (labels ((noise-cut (val) (if (> *epsilon* (abs val)) 0d0 val))
           (calc-cov (xy x y n-1) 
             (noise-cut (- (the double-float (/ (the double-float xy) n-1))
                           (the double-float (/ (the double-float (* x y))
                                                (the fixnum (* (+ 1 n-1) n-1))))))))
    (loop with n-1 fixnum = -1
        with v1-mom double-float = 0d0
        with v2-mom double-float = 0d0
        with v11-mom double-float = 0d0
        with v22-mom double-float = 0d0
        with v1v2-mom double-float = 0d0
        for val1 double-float across v1
        for val2 double-float across v2
        unless (or (nan-p val1) (nan-p val2)) do
          (setf n-1 (+ n-1 1)
                v1-mom (+ v1-mom val1)
                v2-mom (+ v2-mom val2)
                v11-mom (+ v11-mom (d-expt val1 2d0))
                v22-mom (+ v22-mom (d-expt val2 2d0))
                v1v2-mom (+ v1v2-mom (* val1 val2)))
        finally (return (if (plusp n-1)
                            (let ((cov1 (calc-cov v11-mom v1-mom v1-mom n-1))
                                  (cov2 (calc-cov v22-mom v2-mom v2-mom n-1))
                                  (cov12 (calc-cov v1v2-mom v1-mom v2-mom n-1)))
                              (declare (type double-float cov1 cov2 cov12))
                              (if (or (zerop cov1) (zerop cov2)) 0d0
                                (/ cov12 (sqrt (* cov1 cov2)))))
                          0d0)))))
(defun sweep-outlier (vec ;; destructive on vec
                      &key (alpha 0.01d0) (sig-p-hash nil))
  (declare (type dvec vec))
  (multiple-value-bind (res poss)
      (d-smirnov-grubbs vec alpha :type :max :recursive t :sig-p-hash sig-p-hash)
    (declare (ignore res))
    (when poss (mapcar (lambda (pos) (setf (aref vec pos) *nan*)) poss))
    vec))
(defun smoothing (vec ;; destructive on vec
                  &key (trend-k 2) (trend-t^2 0.1d0))
  (declare (type dvec vec))
  (check-type vec dvec)
  (let* ((ts (make-constant-time-series-data '("foo") (transposev (coerce `(,vec) 'vector))))
         (model (trend ts :k trend-k :t^2 trend-t^2))
         (smthd (predict model :n-ahead 0)))
    (do-vec (_ vec :type double-float :index-var i :setf-var sf :return vec)
      (declare (ignore _))
      (setf sf (aref (ts-p-pos (aref (ts-points smthd) i)) 0)))))



;; クロネッカーのデルタ
(defun kronecker-delta (i j)
  (declare (type fixnum i j))
  (check-type i fixnum)
  (check-type j fixnum)
  (if (eql i j) 1d0 0d0))
;; 対称行列か否か
(defun symatp (mat size)
  (and (equal (array-dimensions mat) `(,size ,size))
       (loop for i below size
           always (loop for j from (1+ i) below size
                      always (= (aref mat i j) (aref mat j i))))))
;; 行列平均
(defun matrix-mean (mats)
  (let* ((len (length mats))
         (dims (array-dimensions (car mats)))
         (result (make-array dims :element-type 'double-float
                             :initial-element 0d0)))
    (loop for mat in mats
        do (setf result (mcm mat result :c #'+))
        finally (return (c*mat (dfloat (/ len)) result)))))
;; ベクトル化
;; dmat -> dvec
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
;; 零ベクトルか否か
(defun zerovecp (vec)
  (loop for val across vec always (zerop val)))
;; 値を丸める
(defun round-value (value &key (precision *epsilon*))
  (dfloat (* precision (round value precision))))
;; ベクトルの各値を丸める
(defun round-vec (vec)
  (do-vec (val vec :type double-float :setf-var sf :return vec)
    (setf sf (round-value val))))
;; 行列の各値を丸める
(defun round-mat (mat &optional (precision *epsilon*))
  (assert (> 1 precision))
  (loop for i below (array-dimension mat 0)
      do (loop for j below (array-dimension mat 1)
             as val = (round-value (aref mat i j))
             do (setf (aref mat i j) val))
      finally (return mat)))
;; 行をとってくる
(defun get-row (mat nrow)
  (declare (type dmat mat))
  (check-type mat dmat)
  (let ((row (make-dvec (array-dimension mat 0))))
    (do-vec (val row :type double-float :return row :index-var i :setf-var sf)
      (declare (ignore val))
      (setf sf (aref mat i nrow)))))
;; 対称行列の要素がゼロである行／列の index
(defun get-symat-zero-indices (symat)
  (loop with n = (array-dimension symat 0)
      for i below n
      when (loop for j below n always (zerop (aref symat i j)))
      collect i))
;; 行列の次元を減らす
(defun compress-dmat (mat target-indices)
  (declare (type dmat mat))
  (assert (eql (array-dimension mat 0) (array-dimension mat 1)))
  (let* ((remain-indices (loop for i below (array-dimension mat 0)
                             unless (member i target-indices :test #'eql)
                             collect i))
         (m (length remain-indices))
         (%mat (make-array `(,m ,m) :element-type 'double-float)))
    (loop for %col below m
        for col in remain-indices
        do (loop for %row below m
               for row in remain-indices
               do (setf (aref %mat %col %row) (aref mat col row)))
        finally (return %mat))))

;; 対数正規分布のパラメータ推定
(defun log-normal-params (mean var)
  (assert (plusp mean))
  (let* ((log-mean (log mean))
         (2log-mean (* 2 log-mean))
         (constant (+ (exp 2log-mean) var))
         (fn (lambda (s) (- (exp (+ 2log-mean (expt s 2))) constant)))
         (derivative (lambda (s) (* (exp (+ 2log-mean (expt s 2))) (* 2d0 s))))
         (sigma (statistics::newton-raphson fn derivative :initial-guess (sqrt var)))
         (mu (- log-mean (/ (expt sigma 2) 2))))
    `(:mu ,mu :sigma ,sigma)))
;; ガンマ分布のパラメータ推定
(defun gamma-params (mean var)
  "calculate parameters, scale and shape, for gamma distribution by mean and variance."
  `(:shape ,(dfloat (/ (expt mean 2) var)) :scale ,(dfloat (/ var mean))))

;; 多変量正規分布の確率密度推定
(defparameter *noise-order* 1d-2)
(defun multivariate-normal-density (mu sigma vec &optional m)
  (declare (type dvec mu vec) (type dmat sigma))
  (setf mu (round-vec mu) vec (round-vec vec)
        sigma (mcm (round-mat sigma) (diag (array-dimension sigma 0) *noise-order*)))
  (let* ((dim (array-dimension sigma 0))
         (det (det sigma))
         (inv (if (zerop det) (error "singular sigma: ~A" sigma) ;; 正則性
                (m^-1 sigma)))
         (coef (/ (* (expt (* 2d0 pi) (/ (if (numberp m) m dim) 2))
                     (sqrt det))))
         (in-exp (calc-in-exp inv mu vec)))
    (declare (type double-float coef in-exp))
    (* coef
       (handler-case  (exp in-exp)
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
    (blas:dgemm "N" "N" dim dim dim -0.5d0 inv-sigma dim x-mx-m dim 0d0 res dim)
    (tr res)))
;; マハラノビス距離 
(defun mahalanobis-distance (x y sigma)
  (declare (type dvec x y) (type dmat sigma))
  (check-type x dvec)
  (check-type y dvec)
  (check-type sigma dmat)
  (let ((d (diag (array-dimension sigma 0) *noise-order*))
        ;; sigma 零行列対策および逆行列の安定性のため、対角成分にノイズを加える
        ;; -> 無いと以下の内積結果が負になったりするなど、不安定
        )
    (setf sigma (mcm sigma d :c #'+))
    (let* ((inv-sigma (m^-1 sigma))) 
      (%mahalanobis-distance x y inv-sigma))))
(defun %mahalanobis-distance (x mu inv-sigma)
  (declare (type dvec x mu)
           (type dmat inv-sigma))
  (let* ((x-y (vcv x mu :c #'-))
         (d^2 (inner-product x-y (m*v inv-sigma x-y))))
    (if (minusp d^2) 0d0 (sqrt d^2))))

(defun insert-values (list values positions)
  (let* ((n (+ (length list) (length values)))
         (new-list (make-list n)))
    (loop with old-pos = 0
        for pos below n
        as %pos = (position pos positions :test #'eql)
        if (numberp %pos)
        do (setf (nth pos new-list) (nth %pos values))
        else
        do (setf (nth pos new-list) (nth old-pos list)
                 old-pos (1+ old-pos)))
    new-list))

(defun d-smirnov-grubbs (dvec alpha &key (type :max) (recursive nil) (sig-p-hash nil))
  (declare (type dvec dvec) (type double-float alpha) (optimize speed))
  "length of dvec must be more than 4"
  (flet ((dvec-max-pos (dvec) (let ((max most-negative-double-float)
                                    (pos))
                                (declare (type double-float max))
                                (do-vec (val dvec :type double-float :index-var i :return pos)
                                  (when (> val max) (setf max val pos i)))))
         (dvec-min-pos (dvec) (let ((min most-positive-double-float)
                                    (pos))
                                (declare (type double-float min))
                                (do-vec (val dvec :type double-float :index-var i :return pos)
                                  (when (< val min) (setf min val pos i))))))
    (if (>= (length dvec) 4)
        (let ((target-pos (ecase type 
                            (:max (dvec-max-pos dvec))
                            (:min (dvec-min-pos dvec)))))
          (declare (type fixnum target-pos))
          (multiple-value-bind (ok t_i sig-p n)
              (d-smirnov-grubbs-p dvec target-pos alpha :sig-p-hash sig-p-hash)
            (declare (ignore t_i sig-p))
            (cond ((and recursive ok) dvec)
                  ((and recursive (not ok))
                   (let ((next-dvec (make-array (1- n) :element-type 'double-float
                                                :initial-contents (loop for i below n
                                                                      unless (eql i target-pos)
                                                                      collect (aref dvec i)))))
                     (multiple-value-bind (%dvec removed-poss)
                         (d-smirnov-grubbs next-dvec alpha
                                           :type type :recursive recursive
                                           :sig-p-hash sig-p-hash)
                       (values %dvec
                               (if removed-poss 
                                   (cons target-pos 
                                         (mapcar (lambda (pos) (if (>= pos target-pos) (1+ pos) pos))
                                                 removed-poss))
                                 `(,target-pos))))))
                  (t nil))))
      (progn (warn "Smirnov-Grubbs:length of the vector should be more than 4.~%~A" dvec)
             dvec))))
;;; input: sequence, target position, alpha
;;; return: boolean( t -> o.k. nil -> outlier )
(defun d-smirnov-grubbs-p (dvec position alpha &key (sig-p-hash nil))
  (declare (type dvec dvec) (type fixnum position) (type double-float alpha)
           (optimize speed))
   (flet ((dvec-sumup (dvec) (declare (type dvec dvec))
                     (let ((res 0d0)) (declare (type double-float res))
                          (do-vec (val dvec :type double-float :return res)
                            (incf res val)))))
    (when (and (>= (length dvec) 4) position)
      (let* ((target (aref dvec position))
             (n (length dvec))
             (m (/ (dvec-sumup dvec) n))
             (u-dev (sqrt                     
                     (/ (let ((res 0d0)) (declare (type double-float res))
                             (do-vec (val dvec :type double-float :return res)
                               (incf res (expt (- val (the double-float m)) 2))))
                        (1- n))))
             (sig-p (when (hash-table-p sig-p-hash)
                      (cdr (assoc alpha (gethash n sig-p-hash) :test #'=)))))
        (declare (type double-float target u-dev m))
        (unless sig-p (setf sig-p (statistics::get-sig-p n alpha)))
        (if (zerop u-dev)
            (values t nil sig-p n)
          (let ((t_i (/ (abs (- target m)) u-dev)))
            (declare (type double-float t_i))
            (values (< t_i sig-p) t_i sig-p n)))))))

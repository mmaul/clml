(defpackage :time-series-statistics
  (:use :cl :hjs.learn.read-data :hjs.util.meta :hjs.util.vector
        :hjs.util.matrix :statistics :hjs.learn.vars
        :ts-util :ts-read-data :fft)
  (:nicknames :ts-stat)
  (:export
   #:lag
   #:diff
   #:ts-ratio
   #:ts-log                             ; include logit transformation
   #:ts-min
   #:ts-max
   #:ts-mean
   #:ts-median
   #:ts-demean
   #:ts-covariance
   #:ts-correlation
   #:ma
   #:acf
   #:ccf
   #:periodgram))

(in-package :time-series-statistics)

(defmethod lag ((d time-series-dataset) &key (k 1))
  (assert (integerp k))
  (let* ((data (copy-ts d))
         (freq (ts-freq data))
         (start (tf-incl (ts-start data) (- k) :freq freq))
         (end (tf-incl (ts-end data) (- k) :freq freq)))
    (setf 
        (ts-start data) start
        (ts-end data) end)
    (loop for point across (ts-points data)
        for i from 0
        as tf = (tf-incl start i :freq freq)
        do (setf (ts-p-time point) (first tf)
                 (ts-p-freq point) (second tf)))
    data))
(defmethod diff ((d time-series-dataset) &key (lag 1)
                                              (differences 1))
  (assert (every #'integerp `(,lag ,differences)))
  (assert (plusp differences))
  (if (= differences 1)
      (ts- d (lag d :k (- lag)))
    (diff (ts- d (lag d :k (- lag))) :lag lag :differences (1- differences))
    ))
(defmethod ts-ratio ((d time-series-dataset) &key (lag 1))
  (unless lag
    (setq lag (ts-freq d)))
  (compose-ts (merge-ts d (lag d :k (- lag))) :composer #'/ 
              :column-name (format nil "ratio (lag: ~A)" lag)))
      
(defmethod ts-log ((d time-series-dataset) &key (logit-transform nil)
                                                (log-base (exp 1)))
  (let ((ts (copy-ts d)))
    (loop for point across (ts-points ts)
        if logit-transform
        do (setf (ts-p-pos point)
             (map 'dvec #'(lambda (val)
                            (assert (<= 0 val 1))
                            (cond ((= val 1)
                                   most-positive-double-float)
                                  ((= val 0)
                                   most-negative-double-float)
                                  (t
                                   (log (/ val (- 1 val)) log-base))))
                  (ts-p-pos point)))
        else
        do (setf (ts-p-pos point)
             (map 'dvec #'(lambda (val) (dfloat (log val log-base)))
                  (ts-p-pos point))))
    ts))
(defmethod ts-min ((d time-series-dataset))
  (with-accessors ((dims dataset-dimensions)
                   (points ts-points)) d
    (assert (>= (length points) 1))
    (map 'vector #'(lambda (seq)
                     (apply #'min (coerce seq 'list)))
         (transposev (map 'vector #'ts-p-pos points)))))
(defmethod ts-max ((d time-series-dataset))
  (with-accessors ((dims dataset-dimensions)
                   (points ts-points)) d
    (assert (>= (length points) 1))
    (map 'vector #'(lambda (seq)
                     (apply #'max (coerce seq 'list)))
         (transposev (map 'vector #'ts-p-pos points)))))
(defmethod ts-mean ((d time-series-dataset))
  (with-accessors ((dims dataset-dimensions)
                   (points ts-points)) d
    (assert (>= (length points) 1))
    (mean-points (map 'vector #'ts-p-pos points))))
(defmethod ts-median ((d time-series-dataset))
  (with-accessors ((dims dataset-dimensions)
                   (points ts-points)) d
    (assert (>= (length points) 1))
    (map 'vector (lambda (seq) (median seq))
         (transposeV (map 'vector #'ts-p-pos points)))))
(defmethod ts-stat ((d time-series-dataset))
  (with-accessors ((points ts-points)) d
    (map 'list (lambda (seq)
                 (loop with min = most-positive-double-float
                     with max = most-negative-double-float
                     with x = 0d0
                     with x^2 = 0d0
                     for val double-float across seq
                     do (setf x (+fl x val) x^2 (+fl (d-expt val 2d0) x^2))
                     if (> val max) do (setf max val)
                     else if (< val min) do (setf min val)
                     finally (return (let* ((n (length seq))
                                            (mean (/ x n))
                                            (cov (- (/ x^2 n) (d-expt mean 2d0))))
                                       (list min max mean 
                                             (if (plusp cov) (sqrt cov) 0d0)
                                             (median seq))))))
         (transposeV (map 'vector #'ts-p-pos points)))))
(defmethod ts-demean ((d time-series-dataset))
  (with-accessors ((dims dataset-dimensions)
                   (ps ts-points)
                   (freq ts-freq)
                   (start ts-start)
                   (end ts-end)
                   (label-name time-label-name)) d
    (assert (>= (length ps) 1))
    (let ((time-label-array (map 'vector #'ts-p-label ps))
          (mean (ts-mean d)))
      (make-constant-time-series-data
       (map 'list #'dimension-name dims)
       (map 'vector 
  #'(lambda (p) 
      (let ((dvec (make-dvec (length dims))))
        (loop for i below (length dims)
            do (setf (aref dvec i) 
                 (coerce (- (aref (ts-p-pos p) i) (aref mean i)) 'double-float)))
        dvec)) ps)
       :start start :end end :freq freq 
       :time-labels time-label-array :time-label-name label-name))))
    
(defmethod ts-covariance ((d time-series-dataset) &key (k 0) (demean t))
  (assert (and (>= k 0) (> (length (ts-points d)) k)))
  (let* ((mu (ts-mean d))
         (dim (length (dataset-dimensions d)))
         (1/n (/ 1.0 (length (ts-points d))))
         (lag-d
          (sub-ts (lag d :k (- k)) :end (ts-end d)))
         (d
          (sub-ts d :start (tf-incl (ts-start d) k :freq (ts-freq d))))
         (points-1 (transposeV
                    (map 'vector
                      (lambda (p) (if demean (v- p mu (make-dvec dim))
                                    (specialize-vec p)))
                      (map 'vector #'ts-p-pos (ts-points d)))))
         (points-2 (transposeV
                    (map 'vector
                      (lambda (p) (if demean (v- p mu (make-dvec dim))
                                    (specialize-vec p)))
                      (map 'vector #'ts-p-pos (ts-points lag-d)))))
         (result (make-array (list dim dim) :element-type 'double-float)))
    (declare (type (simple-array dvec (*)) points-1 points-2))
    (do-vec (p1 points-1 :type dvec :index-var ix)
      (do-vec (p2 points-2 :type dvec :index-var iy)
        (do-vec (p1-val p1 :type double-float :setf-var p1v)
          (when (> *epsilon* (abs p1-val)) (setf p1v 0.0d0)))
        (do-vec (p2-val p2 :type double-float :setf-var p2v)
          (when (> *epsilon* (abs p2-val)) (setf p2v 0.0d0)))
        (setf (aref result ix iy)
          (* (inner-product p1 p2) 1/n))))
    result))
(defmethod ts-correlation ((d time-series-dataset) &key (k 0))
  (assert (and (>= k 0) (> (length (ts-points d)) k)))
  (let* ((C-k (ts-covariance d :k k))
         (C-0 (ts-covariance d :k 0)))
    (loop for i below (length (dataset-dimensions d))
        do (loop for j below (length (dataset-dimensions d))
               do (setf (aref C-k i j)
                    (/ (aref C-k i j)
                       (sqrt (* (aref C-0 i i) (aref C-0 j j)))))))
    C-k))
  
;;; moving-average
(defmethod ma ((d time-series-dataset) &key (k 5) weight)
  (with-accessors ((dims dataset-dimensions)
                   (ps ts-points)
                   (start ts-start)
                   (end ts-end)
                   (freq ts-freq)) d
    (assert (and (= (length dims) 1) (integerp k)))
    (assert (and (listp weight) (every #'numberp weight)))
    (let ((2k+1 (1+ (* 2 k))))
      (setq weight
        (when weight
          (let ((total (apply #'+ weight)))
            (assert (/= total 0))
            (assert (= 2k+1 (length weight)))
            (mapcar #'(lambda (val) (/ val total)) weight))))
      (flet ((calc-ma (seq pos)
               (let* ((s-p (max 0 (- pos k)))
                      (e-p (min (length seq) (+ pos k 1)))
                      (w-s-p (- (floor 2k+1 2) (- pos s-p)))
                      (w-e-p (+ (floor 2k+1 2) (- e-p pos)))
                      (target (subseq seq s-p e-p))
                      (weight (when weight (subseq weight w-s-p w-e-p)))
                      (w*t (if weight (map 'list #'* target weight)
                             (map 'list #'(lambda (val) (/ val (length target))) target))))
                 (coerce 
                  (apply #'+ w*t) 'double-float))))
        (let* ((seq (map 'vector #'(lambda (p)
                                     (aref (ts-p-pos p) 0)) ps))
               (ma-seq (coerce 
                        (loop for pos below (length seq)
                            collect (calc-ma seq pos)) 'vector))
               (data (coerce 
                      (loop for val across ma-seq
                          collect (make-dvec 1 val))
                      'vector)))
          (make-constant-time-series-data
           (map 'list #'dimension-name dims)
           data :start start :end end :freq freq))))))

;;; auto-correlation or auto-covariance function
(defmethod acf ((d time-series-dataset) &key (type :correlation)
                                             (plot nil)
                                             (print t)
                                             max-k)
  (with-accessors 
      ((freq ts-freq) (ps ts-points) (start ts-start)
       (end ts-end) (dims dataset-dimensions)) d
    (unless max-k
      (setq max-k 
        (round 
        (* 10 (log (length ps) 10)))))
    (assert (> (length ps) max-k))
    (let* ((mat-list
            (loop for k to max-k
                collect 
                  (case type
                    (:correlation
                     (ts-correlation d :k k))
                    (:covariance
                     (ts-covariance d :k k))
                    (t (error "Unknow type: ~A" type)))))
           (params (map 'list #'dimension-name dims))
           (len (length dims))
           (results
            (loop for param in params
                for i from 0
                collect 
                  (progn
                    (when print
                      (princ (format nil "~&~A~%~{~A~^~T~}~%" param params)))
                    (loop for mat in mat-list
                        for k from 0
                        with seq-lag-list = (loop repeat len collect (make-list 2))
                        as seq = (loop for j below len
                                     as val = (aref mat j i)
                                     collect (prog1 val
                                               (push val (nth 0 (nth j seq-lag-list)))))
                        as lag-seq = (loop for j below len
                                         with lag = (/ k freq)
                                         as val = (if (>= i j) lag (- lag))
                                         collect (prog1 val
                                                   (push val (nth 1 (nth j seq-lag-list)))))
                        when print
                        do (princ (format nil "~{~,3F (~,3F)~^~T~}~%"
                                          (mapcan #'list seq lag-seq)))
                        finally (return seq-lag-list))))))
      (cond (plot
             (let ((f-name (loop for i from 1
                               as f-name = (format nil "acf-~A.eps" i)
                               when (not (probe-file f-name))
                               do (return f-name))))
               (with-r
                   (format *r-stream* "postscript(\"~A\", height=9, width=14, pointsize=15)~%" f-name)
                 (format *r-stream* "par(mfcol=c(~A,~A))~%" len len)
                 (loop for param-1 in params
                     for result in results
                     do (loop for seq-lag in result
                            for param-2 in params
                            do (format 
                                *r-stream* 
                                "plot(c(~{~,3F~^,~}),c(~{~,3F~^,~}),type=\"h\",main=\"~A : ~A\",xlab=\"lag\",ylab=\"ACF\")~%"
                                (nth 1 seq-lag) (nth 0 seq-lag) param-2 param-1)))
                 (format *r-stream* "dev.off()~%"))
               (open-eps-file f-name)))
            (print nil)
            (t results)))))

(defun ccf (d1 d2 &key (type :correlation) (plot nil) (print t) max-k)
  (check-type d1 time-series-dataset)
  (check-type d2 time-series-dataset)
  (assert (= (ts-freq d1) (ts-freq d2)))
  (assert (and (= 1 (length (dataset-dimensions d1))) (= 1 (length (dataset-dimensions d2)))))
  (let* ((d (merge-ts d1 d2)))
    (with-accessors 
        ((freq ts-freq) (ps ts-points) (start ts-start) 
         (end ts-end) (dims dataset-dimensions)) d
      (assert (> (length ps) 0))
      (unless max-k
        (setq max-k 
          (round
          (* 10 (log (length ps) 10)))))
      (let* ((mat-list
              (loop for k to max-k
                  collect (case type
                            (:correlation (ts-correlation d :k k))
                            (:covariance (ts-covariance d :k k))
                            (t (error "Unknow type: ~A" type)))))
             (results
              (progn
                (when print
                  (princ (format nil "~&~{~A~^ : ~}~%" (map 'list #'dimension-name dims))))
                (loop for mat in mat-list
                    for k to max-k
                    with seq-lag-posi = (make-list 2)
                    with seq-lag-nega = (make-list 2)
                    do (if (= k 0)
                           (progn (push (aref mat 0 1) (nth 0 seq-lag-posi))
                                  (push 0 (nth 1 seq-lag-posi)))
                         (progn (push (aref mat 0 1) (nth 0 seq-lag-posi))
                                (push (aref mat 1 0) (nth 0 seq-lag-nega))
                                (push (/ k freq) (nth 1 seq-lag-posi))
                                (push (- (/ k freq)) (nth 1 seq-lag-nega))))
                    finally (let ((results (mapcan #'(lambda (seq1 seq2)
                                                       `(,(append seq1 (reverse seq2))))
                                                   seq-lag-nega seq-lag-posi)))
                              (when print
                                (princ (format nil "~&~{~,3F (~,3F)~^~%~}"
                                               (mapcan #'list (first results) (second results)))))
                              (return results))))))
        (cond (plot
               (let ((f-name (loop for i from 1
                                 as f-name = (format nil "ccf-~A.eps" i)
                                 when (not (probe-file f-name))
                                 do (return f-name))))
                 (with-r
                     (format *r-stream* "postscript(\"~A\", height=9, width=14, pointsize=15)~%" f-name)
                   (format *r-stream*
                           "plot(c(~{~,3F~^,~}),c(~{~,3F~^,~}),type=\"h\",main=\"~{~A~^ : ~}\",xlab=\"lag\",ylab=\"CCF\")~%"
                           (second results) (first results) (map 'list #'dimension-name dims))
                   (format *r-stream* "dev.off()~%"))
                 (open-eps-file f-name)))
              (print nil)
              (t results))))))

;;; calculate periodgram
(defmethod periodgram ((d time-series-dataset) &key step (print t) (plot nil) (log t)
                                                    (smoothing :raw) ; :mean :hanning :hamming
                                                    )
  (with-accessors ((dims dataset-dimensions) (pts ts-points)
                   (st ts-start) (ed ts-end) (freq ts-freq)) d
    (unless step
      (setq step (length pts)))
    (assert (> step 1))
    (assert (>= (length pts) step))
    (assert (= 1 (length dims)))
    (let* ((periodgram
            (cond ((or (eq smoothing :raw) (eq smoothing :mean))
                   (when (eq smoothing :raw)
                     (setq step (length pts)))
                   (let* ((sub-ts-ds (loop repeat (floor (length pts) step)
                                         with start = st
                                         as end = (tf-incl start (1- step) :freq freq)
                                         collect (prog1
                                                     (sub-ts d :start start :end end)
                                                   (setq start end))))
                          (sub-periodgrams (loop for ts-d in sub-ts-ds
                                               collect (periodgram-fft ts-d)))
                          (m (length (car sub-periodgrams)))
                          (n/l (length sub-ts-ds)))
                     (loop for i below m
                         as sp-freq = (/ i (* 2 (1- m)))
                         as sp-val = (/ (loop for p in sub-periodgrams
                                            sum (nth i p)) n/l)
                         collect sp-freq into freq-list
                         collect (if log (log sp-val) sp-val) into val-list
                         finally (return `(,freq-list ,val-list)))))
                  ((or (eq smoothing :hanning) (eq smoothing :hamming))
                   (unless step
                     (setq step (* 2 (round (sqrt (length pts))))))
                   (assert (>= (length pts) step))
                   (let* ((l/2 (floor (/ step 2)))
                          (pj-list
                           (loop for j from 0 to l/2
                               with c_0 = (aref (ts-covariance d :k 0) 0 0)
                               as fj = (/ j step)
                               as pj = (+ c_0 (* 2
                                                 (loop for k from 1 to (1- step)
                                                     as c_k = (aref (ts-covariance d :k k) 0 0)
                                                     sum (* c_k (cos (* 2 pi k fj))))))
                               collect pj)))
                     (loop for j from 0 to l/2
                         as sp-freq = (/ j step)
                         as sp-val = (loop for i from (1- j) to (1+ j)
                                         for w in (case smoothing
                                                    (:hanning '(0.25d0 0.5d0 0.25d0))
                                                    (:hamming '(0.23d0 0.54d0 0.23d0)))
                                         sum (cond ((minusp i)
                                                    (* w (nth (- i) pj-list)))
                                                   ((= i (1+ l/2))
                                                    (* w (nth (1- l/2) pj-list)))
                                                   (t (* w (nth i pj-list)))))
                         collect sp-freq into freq-list
                         collect (if log (log sp-val) sp-val) into val-list
                         finally (return `(,freq-list ,val-list)))))
                  (t (error "invalid :smoothing specifier | ~A" smoothing)))))
      (when print
        (princ (if log (format nil "Frequency | log P(f)~%")
                 (format nil "Frequency | P(f)~%")))
        (loop for sp-freq in (cdr (first periodgram)) ; avoid freq 0
            for sp-val in (cdr (second periodgram))
            do (princ (format nil "~,8F | ~,8F~%" sp-freq sp-val))))

      (cond (plot
             (let ((f-name (loop for i from 1
                               as f-name = (format nil "period-~A.eps" i)
                               when (not (probe-file f-name))
                               do (return f-name))))
               (with-r
                   (format *r-stream*
                           "postscript(\"~A\", height=9, width=14, pointsize=15)~%" f-name)
                 (format *r-stream*
                         "plot(c(~{~,3F~^,~}),c(~{~,3F~^,~}),type=\"h\",main=\"~A\",xlab=\"frequency\",ylab=\"~A\")~%"
                         (cdr (first periodgram)) (cdr (second periodgram))
                         (dimension-name (aref dims 0))
                         (if log "log P(f)" "P(f)"))
                 (format *r-stream* "dev.off()~%"))
               (open-eps-file f-name)))
            (print nil)
            (t periodgram)))))

;;; calculate periodgram by fft
(defmethod periodgram-fft ((d time-series-dataset))
  (with-accessors ((dims dataset-dimensions)
                   (pts ts-points)) d
    (assert (= 1 (length dims)))
    (let* ((extended-pts (make-expt-array 
                          2 (map 'vector #'(lambda (point)
                                             (aref (ts-p-pos point) 0))
                                 pts)))
           (four1-pts (make-array 
                       (* 2 (length extended-pts))
                       :element-type 'double-float
                       :initial-contents
                       (loop for val across extended-pts
                           append `(,(coerce (realpart val) 'double-float)
                                    ,(coerce (imagpart val) 'double-float)))))
           (n (length extended-pts)))

      (four1 four1-pts n)

      (let* ((four1-pts (multiple-value-list (split-seq-odd-even four1-pts))))
        (loop for r-val in (first four1-pts)
            for i-val in (second four1-pts)
            for i from 0
            as p = (/ (+ (expt r-val 2) (expt i-val 2)) n)
            while (>= (/ n 2) i)
            collect p)))))

;;; sample time-series-dataset
#||
(defparameter ukgas 
    (time-series-data
     (read-data-from-file "sample/UKgas.sexp")
     :range '(1) :time-label 0
     :start 1960 :frequency 4))
(defparameter useco
    (time-series-data
     (read-data-from-file "sample/USeconomic.sexp")))
(defparameter tokyo
    (time-series-data
     (read-data-from-file "sample/tokyo-temperature.sexp")))
(defparameter sunspot
    (time-series-data (read-data-from-file "sample/sunspot.sexp")
                      :range '(1) :time-label 0))
(defparameter whard
    (time-series-data (read-data-from-file "sample/whard.sexp")
                      :frequency 12))
(defparameter food
    (time-series-data (read-data-from-file "sample/bls-food.sexp")
                      :range '(1) :time-label 0 :frequency 12))
(defparameter msi
    (time-series-data
     (read-data-from-file
      "sample/msi-access-stat/access-log-stat-0.sexp")
     :range '(1) :start '(18 3) :frequency 24))
(export '(ukgas useco tokyo sunspot whard food msi))
||#
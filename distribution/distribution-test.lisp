;;;;;;;;;;;;;;;;;;;;;
; distribution-test ;
;;;;;;;;;;;;;;;;;;;;;

(in-package :statistics)

;;; refer R-code at http://aoki2.si.gunma-u.ac.jp/R/
      
#||
normal-dist-test (正規分布検定)

OUTPUT( 3 values of property-list )
 result (:total :mean :variance :SD)
 table (:MID 各階級の中心値
        :FREQ 各級の度数
        :Z 級限界の標準化得点
        :CDF 累積確率
        :EXPECTATION 期待値)
 result2 (:CHI-SQ カイ二乗統計量 :D.F. 自由度 :P-VALUE P-値)
||#
(defun normal-dist-test (freq-seq       ; sequence of frequency
                         inf            ; infimum of the first class
                         width          ; class width
                         precision)     ; precision of measurement
  (assert (notevery #'zerop freq-seq))
  (assert (> width 0))
  (assert (>= precision 0))
  (let* ((n (reduce #'+ freq-seq))
         (x `(0 ,@(coerce freq-seq 'list) 0))
         (k (length x))
         (mid (mapcar #'(lambda (val)
                     (- val (/ precision 2)))
                 (loop for i from 0 below k
                     with init-val = (- inf (/ width 2))
                     collect (+ init-val (* i width)))))
         (xbar (/ (apply #'+ (mapcar #'* x mid)) n))
         (variance (/ 
                    (apply #'+ 
                           (mapcar #'* x
                                   (mapcar #'(lambda (a)
                                               (sqr (- a xbar)))
                                           mid)))
                    n))
         (sd (sqrt variance))
         (result `(:total ,n :mean ,xbar :variance ,variance :sd ,sd))
         
         (z (mapcar #'(lambda (a)
                        (/ (- (+ a (/ width 2)) xbar) sd))
                    mid))
         (p (mapcar #'(lambda (a)
                        (cdf (standard-normal-distribution) a))
                    z)))
    (setf (nth (1- k) p) 1
          p (mapcar #'- p
                    `(0 ,@(subseq p 0 (1- k)))))
    (let* ((expectation (mapcar #'(lambda (a) (* n a)) p))
           (table `(:mid ,mid :freq ,x 
                         :z ,z :cdf ,p
                         :expectation ,expectation)))
      (setq x (copy-seq x)
            expectation (copy-seq expectation))
      (loop while (< (first expectation) 1)
          do (incf (nth 1 x) (nth 0 x))
             (incf (nth 1 expectation) (nth 0 expectation))
             (decf k)
             (setf x (cdr x)
                   expectation (cdr expectation)))
      (loop while (< (nth (1- k) expectation) 1)
          do (decf k)
             (incf (nth (1- k) x) (nth k x))
             (incf (nth (1- k) expectation) (nth k expectation))
             (setf x (subseq x 0 k)
                   expectation (subseq expectation 0 k)))
      (let* ((chisq (apply #'+
                           (mapcar #'(lambda (x-val exp-val)
                                       (/ (expt (- x-val exp-val) 2)
                                          exp-val))
                                   x expectation)))
             result2)
        (decf k 3)
        (setf p (- 1 (cdf (chi-square-distribution k) chisq))
              result2 `(:chi-sq ,chisq :d.f. ,k :p-value ,p))
        
        (values result table result2)))))
#||
STAT(6): (normal-dist-test '(4 19 86 177 105 33 2) 40 5 0.1)
(:TOTAL 426 :MEAN 57.931225 :VARIANCE 26.352928 :SD 5.13351)
(:MID (37.45 42.45 47.45 52.45 57.45 62.45 67.45 72.45 77.45) :FREQ
(0 4 19 86 177 105 33 2 0) :Z
(-3.5027153 -2.5287228 -1.5547304 -0.58073795 0.3932545 1.3672462
 2.3412387 3.315231 4.2892237)
:CDF
(2.3027066827641107d-4 0.005493650023016494d0 0.0542812231219722d0
 0.2207033969433026d0 0.3722256949242654d0 0.2612916822967053d0
 0.07616414571442975d0 0.009152099332533692d0 4.578369754981715d-4)
:EXPECTATION
(0.09809530468575112d0 2.4383902144907776d0 23.123801049960157d0
 94.01964709784691d0 158.56814603773705d0 111.31025665839645d0
 32.44592607434708d0 4.093832867221574d0 0.19503855156222105d0))
(:CHI-SQ 6.000187256825313d0 :D.F. 4 :P-VALUE 0.19913428945535006d0)
||#


#||
poisson-dist-test (ポアソン分布検定)

OUTPUT( 3 values of p-list )
result (:N 全度数
        :MEAN 平均)
table (:C-ID 仮の階級値
       :FREQ 度数
       :P 確率
       :E 期待値)
result2 (:CHI-SQ カイ二乗統計量 
         :D.F. 自由度
         :P-VALUE P-値)
||#
(defun poisson-dist-test (d)            ; sequence of frequency
  (assert (> (length d) 1))
  (let* ((k (length d))
         (n (apply #'+ (coerce d 'list)))
         (x (loop for i from 0 below k
                collect i))
         (mean (coerce (/ (apply #'+ (map 'list #'* d x)) n)
                       *read-default-float-format*))
         (result `(:n ,n :mean ,mean))
         (p (mapcar #'(lambda (num)
                        (/ (* (exp (- 0 mean))
                              (expt mean num))
                           (do ((i 1 (1+ i))
                                (fact 1 (* fact i)))
                               ((> i num) fact))))
                    x)))
    (setf (nth (1- k) p)
      (- 1 (apply #'+ (subseq p 0 (1- k)))))
    (let* ((e (mapcar #'(lambda (num)
                          (* n num)) p))
           (table `(:c-id ,x :freq ,d :p ,p :e ,e)))
      (setq d (copy-seq d)
            e (copy-seq e))
      (loop while (< (nth 0 e) 1)
          do (setf (nth 1 d) (+ (nth 1 d) (nth 0 d))
                   (nth 1 e) (+ (nth 1 e) (nth 0 e))
                   d (cdr d)
                   e (cdr e))
             (decf k))
      (loop while (< (nth (1- k) e) 1)
          do (setf (nth (- k 2) d) (+ (nth (- k 2) d) (nth (1- k) d))
                   (nth (- k 2) e) (+ (nth (- k 2) e) (nth (1- k) e))
                   d (subseq d 0 (1- (length d)))
                   e (subseq e 0 (1- (length e))))
             (decf k))
      (let* ((chisq (apply #'+
                           (mapcar #'(lambda (d-val e-val)
                                       (/ (expt (- d-val e-val) 2)
                                          e-val))
                                   d e)))
             result2)
        (decf k 2)
        (setf p (- 1 (cdf (chi-square-distribution k) chisq))
              result2 `(:chi-sq ,chisq :d.f. ,k :p-value ,p))
        (values result table result2)))))
#||
STAT(10): (poisson-dist-test '(27 61 77 71 54 35 20 11 6 2 1))
(:N 365 :MEAN 1092/365)
(:C-ID (0 1 2 3 4 5 6 7 8 9 ...) :FREQ (27 61 77 71 54 35 20 11 6 2 ...)
 :P
 (0.050197963 0.1501813 0.22465476 0.22403927 0.1675691 0.100266
  0.04999565 0.021368004 0.0079910485 0.002656385 ...)
 :E
 (18.322256 54.816174 81.998985 81.77434 61.162724 36.59709 18.248411
  7.7993217 2.9167328 0.96958053 ...))
(:CHI-SQ 14.143778 :D.F. 8 :P-VALUE 0.07809402061210624d0)
||#

#||
二項分布検定(binom-dist-test)

OUTPUT( 3 values of p-list )
result (:D-SIZE サンプルサイズ
        :PROBABILITY) 母比率
table (:FREQ 度数分布
       :P 確率
       :E) 期待値
result2 (:CHI-SQ カイ二乗統計量
         :D.F. 自由度
         :P-VALUE) P-値
||#
(defun binom-dist-test (d               ; sequence of frequency
                        x               ; sequence of class-value
                        size)           ; size of Bernoulli trials
  (let ((k (length d)))
    (assert (= k (length x)))
    (assert (notany #'minusp d))
    (assert (notany #'minusp x))
    (assert (every #'integerp d))
    (assert (every #'integerp x))
    (assert (every #'(lambda (a) (>= size a)) x))

    (let* ((n (apply #'+ (coerce d 'list)))
           (prob 
            (coerce (/ (/ (apply #'+ (map 'list #'* d x)) n) size)
                    *read-default-float-format*))
           (binom-dist (binomial-distribution size prob))
           (p (map 'list #'(lambda (val)
                             (coerce (mass binom-dist val)
                                     *read-default-float-format*)) x))
           (e (mapcar #'(lambda (val)
                          (* val n)) p))
           (result `(:size ,k :probability ,prob))
           (table `(:freq ,d :p ,p :e ,e)))
      (setq d (copy-seq d)
            e (copy-seq e))
      (loop while (< (nth 0 e) 1)
          do (setf (nth 1 d) (+ (nth 1 d) (nth 0 d))
                   (nth 1 e) (+ (nth 1 e) (nth 0 e))
                   d (cdr d)
                   e (cdr e))
             (decf k))
      (loop while (< (nth (1- k) e) 1)
          do (setf (nth (- k 2) d) (+ (nth (- k 2) d) (nth (1- k) d))
                   (nth (- k 2) e) (+ (nth (- k 2) e) (nth (1- k) e))
                   d (subseq d 0 (1- k))
                   e (subseq e 0 (1- k)))
             (decf k))
      (let* ((chisq (apply #'+
                           (mapcar #'(lambda (d-val e-val)
                                       (/ (expt (- d-val e-val) 2)
                                          e-val))
                                   d e)))
             result2)
        (decf k 2)
        (setf p (- 1 (cdf (chi-square-distribution k) chisq))
              result2 `(:chi-sq ,chisq :d.f. ,k :p-value ,p))
        (values result table result2)))))
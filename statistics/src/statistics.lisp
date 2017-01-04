;;; -*- mode: lisp; syntax: common-lisp -*-

;;; Peter Salvi, 2008

;;; Notes
;;; -----
;;; * Numbers are not converted to (double) floats, for better accuracy with
;;;   whole number data. This should be OK, since double data will generate
;;;   double results (the number type is preserved).
;;; * Some functions that operate on sorted sequences have two versions, FUN
;;;   and FUN-ON-SORTED, the latter of which assumes that the sequence is
;;;   already sorted.
;;; * Distributions are implemented as functions that return CLOS objects,
;;;   that have methods like CDF (cumulative distribution function),
;;;   DENSITY, QUANTILE and RAND (gives a random number according to the
;;;   given distribution).
;;; * Places marked with TODO are not optimal or not finished.

(in-package :clml.statistics)


;;;;;;;;;;;;;;;;;;;;;
;;; Data analysis ;;;
;;;;;;;;;;;;;;;;;;;;;

;;; Functions on one-valued data

(defgeneric mean (obj)
  (:documentation "Returns the mean of SEQ."))
(defmethod mean ((sequence sequence))
  (/ (reduce #'+ sequence) (length sequence)))

(def-on-sorted median (sequence)
  "Returns the median of SEQ.
(Variant: median-on-sorted (sorted-seq))"
  (let ((n (length sequence)))
    (if (evenp n)
        (let ((n/2 (/ n 2)))
          (/ (+ (elt sequence (1- n/2)) (elt sequence n/2)) 2))
        (elt sequence (/ (1- n) 2)))))

(def-on-sorted discrete-quantile (sequence cuts)
  "Returns the quantile(s) of SEQ at the given cut point(s). CUTS can be a
single value or a list.
(Variant: discrete-quantile-on-sorted (sorted-seq cuts))

The function gives the mean of the two numbers closest to the given ratio
if the ratio does not give an exact (whole) position. This is what LISP-STAT
does, but returning
  \(LINEAR-COMBINATION \(ELT SEQUENCE Q) R \(ELT SEQUENCE (1+ Q)))
may be better. More on this at http://mathworld.wolfram.com/Quantile.html.

CUTS is a single number or a list of numbers, each in the interval [0,1]."
  (let ((n (length sequence)))
    (flet ((section (cut)
             (multiple-value-bind (q r)
                 (floor (* (1- n) cut))
               (if (zerop r)
                   (elt sequence q)
                   (/ (+ (elt sequence q) (elt sequence (1+ q))) 2)))))
      (if (listp cuts)
          (mapcar #'section cuts)
          (section cuts)))))

(def-on-sorted five-number-summary (sequence)
  "Returns the \"five number summary\" of SEQ, ie. the discrete quantiles at the
cut points 0, 1/4, 1/2, 3/4 and 1.
(Variant: five-number-summary-on-sorted (sorted-seq))"
  (discrete-quantile-on-sorted sequence '(0 1/4 1/2 3/4 1)))

(defun range (sequence)
  "Returns the interquartile range of SEQ, ie. the difference of the discrete
quantiles at 3/4 and 1/4.
(Variant: interquartile-range-on-sorted (sorted-seq))"
  (- (reduce #'max sequence) (reduce #'min sequence)))

(def-on-sorted interquartile-range (sequence)
  (- (discrete-quantile-on-sorted sequence 3/4)
     (discrete-quantile-on-sorted sequence 1/4)))

(defun sum-on-deviation (function sequence)
  (let ((mean (mean sequence)))
    (reduce (lambda (sum x) (+ sum (funcall function (- x mean)))) sequence
            :initial-value 0)))

(defun mean-deviation (sequence)
  "Returns the mean deviation of SEQ."
  (/ (sum-on-deviation #'abs sequence) (length sequence)))
#|
(defgeneric variance (obj))
(defmethod variance ((sequence sequence))
  (/ (sum-on-deviation #'sqr sequence) (length sequence)))

(defun standard-deviation (sequence &key populationp)
  "Sample standard deviation; or population standard deviation if POPULATIONP."
  (sqrt (coerce (/ (sum-on-deviation #'sqr sequence)
                   (if populationp (length sequence) (1- (length sequence))))
                'double-float)))
|#

;;; Functions on two-valued data



(defun rank-list (sequence)
  "Returns the indices of the values as if sorted in ascending order.

TODO: This could be done more efficiently."
  (let ((sorted (sort (remove-duplicates sequence) #'<)))
    (map 'list (lambda (x) (1+ (position x sorted))) sequence)))

;;;this fcn is added by naganuma@msi
(defun rank-list-with-tie (sequence)
  "Returns the indices of the values as if sorted in ascending order
with considering tie of rank."
  (let ((sorted (sort (copy-seq sequence) #'<)))
    (map 'list
      (lambda (x)
        (let ((x-begin (1+ (position x sorted :from-end nil)))
              (x-end (1+ (position x sorted :from-end t))))
          (cond ((= x-begin x-end)
                 x-begin)
                ((> x-end x-begin)
                 (/ (+ x-begin x-end) 2)) ; mean of rank
                (t (error "illegal sorted seq.")))))
      sequence)))

(defun spearman-rank-correlation (seq1 seq2)
  "Gives the correlation coefficient based on just the relative size of the
given values."
  (let ((n1 (length seq1))
        (n2 (length seq1)))
    (assert (= n1 n2) (seq1 seq2)
            "The two sequences must have the same length.")
    ;;;  below is edited by naganuma@msi
    ;;;  ref: http://aoki2.si.gunma-u.ac.jp/lecture/Soukan/spearman.html
    (let* ((rank1 (rank-list-with-tie seq1))
           (rank2 (rank-list-with-tie seq2))
           (n3-n (- (expt n1 3) n1))
           (t1 (/ (- n3-n
                     (loop for i in (remove-duplicates rank1 :test #'=)
                         as n = (count i rank1 :test #'=)
                         sum (- (expt n 3) n))) 12))
           (t2 (/ (- n3-n
                     (loop for i in (remove-duplicates rank2 :test #'=)
                         as n = (count i rank2 :test #'=)
                         sum (- (expt n 3) n))) 12))
           (sum-d (apply #'+ (mapcar (lambda (x y) (sqr (- x y)))
                                     rank1 rank2))))
      (if (and (= t1 0) (= t2 0))
          (- 1 (/ (* 6 sum-d) n3-n))
        (/ (- (+ t1 t2) sum-d) (* 2 (sqrt (* t1 t2))))))))

(defun kendall-rank-correlation (seq1 seq2)
  "Returns the Kendall \"tau\" rank correlation coefficient."
  (let ((n1 (length seq1))
        (n2 (length seq1)))
    (assert (= n1 n2) (seq1 seq2)
            "The two sequences must have the same length.")
    ;;;  below is edited by naganuma@msi
    ;;;  ref: http://aoki2.si.gunma-u.ac.jp/lecture/Soukan/kendall.html
    (let* ((rank1 (rank-list-with-tie seq1))
           (rank2 (rank-list-with-tie seq2))
           (denom (* n1 (1- n1)))
           (t1 (loop for i in (remove-duplicates rank1 :test #'=)
                   as n = (count i rank1 :test #'=)
                   sum (/ (* n (1- n)) 2)))
           (t2 (loop for i in (remove-duplicates rank2 :test #'=)
                   as n = (count i rank2 :test #'=)
                   sum (/ (* n (1- n)) 2)))
           (sigma-p (loop for i from 0 below n1
                        sum
                          (loop
                              for j from 0 below n1
                              when (/= i j)
                              sum (if (or
                                       (and (> (elt seq1 i) (elt seq1 j))
                                            (> (elt seq2 i) (elt seq2 j)))
                                       (and (< (elt seq1 i) (elt seq1 j))
                                            (< (elt seq2 i) (elt seq2 j))))
                                      1
                                    0))))
           (sigma-q (loop for i from 0 below n1
                        sum
                          (loop
                              for j from 0 below n1
                              when (/= i j)
                              sum (if (or
                                       (and (> (elt seq1 i) (elt seq1 j))
                                            (< (elt seq2 i) (elt seq2 j)))
                                       (and (< (elt seq1 i) (elt seq1 j))
                                            (> (elt seq2 i) (elt seq2 j))))
                                      1
                                    0)))))
      (if (and (= t1 0) (= t2 0))
          (/ (- sigma-p sigma-q)
             denom)
        (/ (/ (- sigma-p sigma-q) 2)
           (* (sqrt (- (/ denom 2) t1))
              (sqrt (- (/ denom 2) t2))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Probability distribution ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; For reference see Wikipedia and
;;; Statistical Distributions, 2nd Edition, Merran Evans et al., Wiley 1993.

;;; get the most frequent object in sequence
;;; Usually, this is for Categorical valued data
;;; return: object, count
(defun statistics-mode (seq)
  (let* ((alist (count-values seq))
         (val-num (when alist (car (sort alist #'> :key #'cdr)))))
    (when val-num
      (values (car val-num) (cdr val-num)))))





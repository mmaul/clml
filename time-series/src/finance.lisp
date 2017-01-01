(in-package :clml.time-series.finance)

(defgeneric atr (d &key)
  (:documentation "
True range (TR) is a measure of volatility of a High-Low-Close series; average true range (ATR) is a Welles Wilder's style moving average of the TR. Developed by J. Welles Wilder in 1978.

- return: copy of <time-series-dataset> starting after nth observation
          with tr and atr dimensions appended
- arguments:
  - d : <time-series-dataset>, one dimensional
  - n : numper of previous observations used to calculate first atr
  - high : column index containg High measurement
  - low : column index containg Low measurement
  - close : column index containg Close measurement
"))


(defmethod atr ((d time-series-dataset) &key (n 14) (high 1) (low 2) (close 3) )
  (with-accessors ((dims dataset-dimensions)
                   (ps ts-points)
                   (start ts-start)
                   (end ts-end)
                   (freq ts-freq)) d

      (let* ((1st-point (clml.time-series.read-data:ts-p-pos (aref ps  0)))
             (1st-tr (- (aref 1st-point high) (aref 1st-point low)))
             )
        (flet ((avg (v) (coerce (/ (reduce #'+ v) (length v)) 'double-float))
               (calc-tr (pos lc)
                 (let ((h-l (- (aref pos high) (aref pos low)))
                       (h-clp (abs (- (aref pos high) lc)))
                       (l-clp (abs (- (aref pos low) lc))))
                   (max h-l h-clp l-clp))))
          (make-constant-time-series-data
           (append (map 'list #'dimension-name dims) '("tr" "atr"))
           (subseq
            (coerce  (loop for p across ps
                           for i from 0 upto (length ps)
                           for pos = (ts-p-pos p)
                           collect (aref pos close) into lcs
                           when (= 0 i)
                             collect 1st-tr into trs
                           when (and (> i 0) (<= i (- n 1)))
                             collect (calc-tr pos (elt lcs (- i 1)))  into trs
                           when (= i (- n 1))
                             collect  (v2dvec (coerce (append (coerce pos 'list) (list (nth i trs) (avg trs))) 'vector)) into data
                           when (>= i n)
                             collect  (let ((last-atr (elt (nth (- (length data) 1) data) (+ (length dims) 1) )))
                                        (v2dvec (coerce (append (coerce pos 'list)
                                                                (list (calc-tr pos (elt lcs (- i 1)))
                                                                      (/ (+ (* last-atr 13) (calc-tr pos (elt lcs (- i 1)))) n))
                                                                )
                                                        'vector)))
                               into data
                           finally (return data)
                           )
                     'vector) 1)
           :start (list n 1) :end end
           :freq freq
           :time-labels (map 'vector (lambda (l) (clml.time-series.read-data:ts-p-label l)) (subseq (ts-points d) n)))
))))

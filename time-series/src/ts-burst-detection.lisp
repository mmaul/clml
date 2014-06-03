;;; -*- Mode: Lisp; Syntax: Common-Lisp; Coding:iso-2022-jp -*-
;;;
;;; References 
;;; [D] http://www.w3.org/TR/NOTE-datetime  
;;; [K] J. Kleinberg, Bursty and Hierarchical Structure in Streams,
;;;     http://www.cs.cornell.edu/home/kleinber/bhs.pdf , 2002.
;;; [R] bursts: Markov model for bursty behavior in streams, Version 1.0
;;;     http://cran.r-project.org/web/packages/bursts/index.html , 2012-10-24.


(defpackage :ts-burst-detection
  (:use :cl :read-data :handling-missing-value
        :ts-util :ts-stat :ts-read-data)
  (:export
   #:continuous-kleinberg
   #:print-burst-indices
   #:enumerate-kleinberg))

(in-package :ts-burst-detection)

(defun index-of-minimum-value (value-vector &key key)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (loop for val across value-vector
      for i fixnum from 0
      with min-val = *+inf*
      with index = 0
      when key
      do (setq val (funcall key val i))
      when (< val min-val)
      do (setq index i)
         (setq min-val val)
      finally (return (values index min-val))))

(defun compute-gaps (offsets)
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type list offsets))
  (loop for (old new) on offsets
      while new
      as gap = (- (car new) (car old))
      do (assert (/= gap 0))
      count 1 into gaps-length
      sum gap into gap-total
      collect gap into gaps
      finally (return (values gaps gap-total gaps-length))))

(defun compute-burst-index (offsets s gamma)
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type list offsets))
  (multiple-value-bind (gaps gap-total gap-length)
      (compute-gaps offsets)
    (let* ((ghat (/ gap-total gap-length))
           (k (ceiling (+ 1 (log gap-total s)
                          (log (/ 1 (apply #'min gaps) s)))))
           (gammalogn (* gamma (log gap-length)))
           (alpha-list (loop for x from 0 below k
                           collect (/ (expt s x) ghat))))
      (labels ((tau (i j)
                 (if (>= i j)
                     0
                   (* (- j i) gammalogn)))
               (func-f (x alpha)
                 (handler-case
                     (* alpha (exp (* (- alpha) x)))
                   (floating-point-underflow (cond)
                     (declare (ignore cond))
                     0.0d0)))
               (calc-cost (gap alpha)
                 (handler-case
                     (let ((val (func-f gap alpha)))
                       (if (zerop val) *-inf* (log val)))
                   (arithmetic-error (cond)
                     (declare (ignore cond))
                     *-inf*)))
               (make-result-index-list (q min-c-index)
                 (append (list (list 0   ; initial-i0 value
                                     (or (cadar offsets) (caar offsets))))
                         (loop for i from 0 below (array-dimension q 1)
                             for time in (cdr offsets)
                             collect (list (aref q min-c-index i)
                                           (or (cadr time) (car time)))))))
        (loop for gap in gaps
            for time-count from 0
            with c = (make-array k :initial-element *+inf*)
            and q = (make-array (list k gap-length))
            initially (setf (aref c 0) 0)
            do ;(format t "===== ~d =====~%" (1+ time-count))
              (loop for j fixnum from 0 below k
                  with c-prime = (make-array k :initial-element *+inf*)
                  as alpha in alpha-list
                  as (ell min-cost)
                  = (multiple-value-list
                     (index-of-minimum-value c :key
                                             #'(lambda (val idx)
                                                 ;(format t "~%val=~f, tau=~d, total=~f" val (tau (1+ idx) j) (+ val (tau (1+ idx) j)))
                                                 (+ val (tau idx j)))))
                  do ;(format t "~%ell=~d, min-cost=~d" ell min-cost)
                    (setf (aref c-prime j) (- min-cost (calc-cost gap alpha)))
                    (loop for i from 0 to time-count
                        as val = (aref q ell i)
                        do (setf (aref q j i) val)
                        finally (setf (aref q j time-count) j))
                  finally (setq c c-prime))
            finally (return (make-result-index-list q (index-of-minimum-value c))))))))
      
(defmethod continuous-kleinberg ((offsets list) ; list of (utime) or (utime "label")
                                 &key (s 2)
                                      (gamma 1)
                                      (if-overlap :error))
  (assert (> s 1))
  (assert (> gamma 0))
  (assert (> (length offsets) 0))
  (if (= (length offsets) 1)
      (let ((val (nth 0 offsets)))
        `((0 ,(or (cadr val) (car val)))))
    (let ((offsets-s (copy-list offsets)))
      (when (eql if-overlap :delete)
        (setq offsets-s (remove-duplicates offsets-s :test #'= :key #'car)))
      (compute-burst-index (sort offsets-s #'< :key #'car) s gamma))))

(defmethod continuous-kleinberg ((ts time-series-dataset)
                                 &key (s 2)
                                      (gamma 1)
                                      (column-number nil)
                                      (time-reader nil)
                                      (if-overlap :error))
  (continuous-kleinberg (loop for i below (length (ts-points ts))
                            as label = (ts-p-label (aref (ts-points ts) i))
                            when (or (null column-number)
                                     (and column-number
                                          (> (aref (ts-p-pos (aref (ts-points ts) i)) column-number) 0)))
                            if time-reader
                            collect (list (funcall time-reader label) label)
                            else
                            collect (list (handler-case
                                              (read-from-string label)
                                            (error (cond)
                                              (declare (ignore cond))
                                              (error "Can't convert to integer/double from \"~a\"." label)))))
                        :s s :gamma gamma :if-overlap if-overlap))

(defun draw-burst-graph (burst-indices &optional (stream t))
  (let ((time-sequence (to-time-sequence burst-indices)))
    (flet ((label-length (time)
             (length (format nil "~a" time))))
      (loop for (time index) in time-sequence
          with time-strlen-max = (loop for val in time-sequence
                                     maximize (label-length (car val)))
          with formatter = (format nil "~~~d<~~A~~> |" time-strlen-max)
          do (format stream formatter time)
             (loop repeat index
                 do (format stream "+"))
             (format stream "~%")))))

(defun to-time-sequence (burst-indices)
  (loop for ((prev-index prev-end) (cur-index cur-end)) on burst-indices
      for i from 0
      if (or (null cur-index)           ; last data
             (> cur-index prev-index)
             (= i 0))
      collect (list prev-end (or cur-index prev-index))
      else if (< cur-index prev-index)
      collect (list prev-end cur-index)))

(defun to-burst-index-sequence (burst-indices)
  (let ((result)
        (stack))
    (flet ((add-cell (index start)
             (let ((cell (list :index index :start start)))
               (push cell result)
               (push cell stack)))
           (set-end (end count)
             (loop repeat count
                 as cell = (pop stack)
                 while cell
                 do (nconc cell (list :end end)))))
      (loop for ((prev-index prev-end) (cur-index cur-end)) on burst-indices
          initially (apply #'add-cell (car burst-indices))
          until (null cur-index)        ; last
          if (> cur-index prev-index)
          do (loop while (> cur-index prev-index)
                 do (incf prev-index)
                    (add-cell prev-index prev-end))
          else if (< cur-index prev-index)
          do (set-end prev-end (- prev-index cur-index))
          finally (set-end prev-end (length stack)))
      (reverse result))))
  
(defun print-burst-indices (burst-indices &key (stream t) (type :graph))
  (ecase type
    (:graph (draw-burst-graph burst-indices stream))
    (:time-sequence
     (let ((result (to-time-sequence burst-indices)))
       (format stream "~s~%" result)
       result))
    (:burst-index-sequence
     (let ((result (to-burst-index-sequence burst-indices)))
       (format stream "~s~%" result)
       result))))

; for enumerating burst detection

(defun assert-state-in-enumerate-states (state-index)
  (assert (or (= state-index 0) (= state-index 1))))

(defun enumerate-burst-state-p (state-index) 
  (assert-state-in-enumerate-states state-index)
  (if (= state-index 1) T nil))
    
(defun log-factorial (n)
  (assert (<= 1 n))
  (do ((i 1 (1+ i))
       (ret 0 (incf ret (log i))))
      ((> i n) ret)))
    
(defun log-choose (m n)
  (assert (<= 0 n))
  (assert (<= n m))
  (cond 
   ((= n 0) 0)
   ((= m n) 0)
   (t (- (log-factorial m) 
         (+ (log-factorial n) (log-factorial (- m n)))))))

; p_0: notation in [K] p14.
(defun func-sigma (p_0 state-index d-on-batch r-on-batch scaling-param)
  (assert (<= 0 r-on-batch))
  (assert (<= r-on-batch d-on-batch))
  ; p_i: notation in [K] p14.
  (let ((p_i (* p_0 (expt scaling-param state-index))))
    (- (+ (log-choose d-on-batch r-on-batch) 
          (* r-on-batch (log p_i))
          (* (- d-on-batch r-on-batch) (log (- 1 p_i)))))))

(defun get-ratio-of-sum-of-d-and-sum-of-r (batches)
  (let ((d-list (mapcar #'first batches))
        (r-list (mapcar #'second batches)))
    (/ (apply #'+ r-list) (apply #'+ d-list))))

(defun index-of-minimum-value (value-vector &key key)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (loop for val across value-vector
      for i fixnum from 0
      with min-val = *+inf*
      with index = 0
      when key
      do (setq val (funcall key val i))
      when (< val min-val)
      do (setq index i)
         (setq min-val val)
      finally (return (values index min-val))))

(defun compute-enumerate-burst-index (batches
                                      &key (scaling-param 2) 
                                           (gamma 1))
  (let* ((batches-size (length batches))
         (gammalogn (* gamma (log batches-size)))
         (p_0 (get-ratio-of-sum-of-d-and-sum-of-r batches)) 
         (states-num 2))
    (assert (<= 0 p_0))
    (assert (<= (* scaling-param p_0) 1))
    (flet ((tau (i j)
             (if (>= i j) 0
               (* (- j i) gammalogn)))
           (make-result-index-list (q min-c-index)
               (loop for current-batch-id from 0 below batches-size by 1
                   collect (aref q min-c-index current-batch-id))))
      (loop
          with c = (make-array states-num :initial-element *+inf*) ; used in [R] R-source under the name of "C" 
          with q = (make-array (list states-num batches-size)) ; used in [R] R-source under the name of "q"
          for current-batch-id from 0 below batches-size by 1
          for (d-on-current-batch r-on-current-batch) in batches
          initially (setf (aref c 0) 0)
          do ;(format t "~%- current-batch-id=~d, d=~d, r=~d" current-batch-id d-on-current-batch r-on-current-batch)
             (loop for state-index-on-current-batch from 0 below states-num
                 with c-prime = (make-array states-num :initial-element *+inf*) ; used in [R] R-source under the name of "Cprime"
                 as (ell min-cost)
                 = (multiple-value-list
                    (index-of-minimum-value c :key
                                            #'(lambda (val state-index-on-previous-batch)
                                                (+ val (tau state-index-on-previous-batch 
                                                            state-index-on-current-batch)))))
                 do ;(format t "~%  -- state-index=~d, ell=~d, min-cost=~f" state-index-on-current-batch ell min-cost)
                    (setf (aref c-prime state-index-on-current-batch) 
                      (+ min-cost (func-sigma p_0
                                              state-index-on-current-batch 
                                              d-on-current-batch r-on-current-batch
                                              scaling-param)))
                    (loop for batch-id from 0 below current-batch-id by 1
                        as val = (aref q ell batch-id)
                        do ;(format t "~%    --- batch-id=~d, val=~d" batch-id val)
                           (setf (aref q state-index-on-current-batch batch-id) val)
                        finally (setf (aref q state-index-on-current-batch current-batch-id) state-index-on-current-batch))
                 finally (setq c c-prime))
          finally (return (make-result-index-list q (index-of-minimum-value c)))))))

(defun get-burst-periods (state-indices)
  (mapcar #'assert-state-in-enumerate-states state-indices)
  (flet ((get-same-state-batches-size-from (target-batch-id)
           (loop 
               with state-index-on-target-batch = (nth target-batch-id state-indices)
               with same-state-batches-size = 0
               with loop-state-indices = (subseq state-indices target-batch-id)
               for state-index-on-current-batch in loop-state-indices
               while (= state-index-on-current-batch state-index-on-target-batch)
               do (incf same-state-batches-size)
               finally (return same-state-batches-size))))
    (loop
        with batches-size = (length state-indices)
        for burst-period-start-batch-id
        = (if (enumerate-burst-state-p (first state-indices)) 0
            (get-same-state-batches-size-from 0))
        then (+ base-period-start-batch-id base-period-length)
        while (< burst-period-start-batch-id batches-size)
        for burst-period-length = (get-same-state-batches-size-from burst-period-start-batch-id)
        for base-period-start-batch-id = (+ burst-period-start-batch-id burst-period-length)
        for base-period-length = (get-same-state-batches-size-from base-period-start-batch-id)
        collect (list burst-period-start-batch-id
                      (+ burst-period-start-batch-id burst-period-length)))))

(defun get-weight-list (batches burst-periods &key (scaling-param 2))
  (loop 
      for (burst-period-min burst-period-sup) in burst-periods
      with p_0 = (get-ratio-of-sum-of-d-and-sum-of-r batches) ; p_0: notation in [K] p14.
      collect (loop 
                  with weight = 0
                  and loop-batches = (subseq batches burst-period-min burst-period-sup)
                  for (d-on-current-batch r-on-current-batch) in loop-batches
                  for (sigma-0 sigma-1) = (mapcar #'(lambda (state-index) 
                                                      (func-sigma p_0 state-index
                                                                  d-on-current-batch
                                                                  r-on-current-batch
                                                                  scaling-param))
                                                  (list 0 1))
                  do (incf weight (- sigma-0 sigma-1))
                  finally (return weight))))


(defun enumerate-kleinberg (batches
                            &key (scaling-param 2)
                                 (gamma 1))
  (assert (> scaling-param 1))
  (assert (> gamma 0))
  (let* ((optimal-state-indices (compute-enumerate-burst-index batches 
                                                               :scaling-param scaling-param 
                                                               :gamma gamma))
         (burst-periods (get-burst-periods optimal-state-indices))
         (weight-list-on-burst-periods (get-weight-list batches burst-periods)))
    (loop 
        for burst-period in burst-periods
        and weight-on-burst-period in weight-list-on-burst-periods
        collect (append burst-period (list weight-on-burst-period)))))

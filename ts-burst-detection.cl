
(defpackage :ts-burst-detection
  (:use :cl :read-data :handling-missing-value
        :ts-util :ts-stat :ts-read-data)
  (:export
   #:continuous-kleinberg
   #:print-burst-indices
   #:date-time-to-ut
   #:ut-to-date-time))

(in-package :ts-burst-detection)

#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :datetime))

(defun date-time-to-ut (date-time)
#+allegro
  (util.date-time:date-time-to-ut date-time)
#-allegro
  (error "date-time-to-ut is not supported.")
  )

(defun ut-to-date-time (date-time)
#+allegro
  (format nil "~a" (util.date-time:ut-to-date-time date-time))
#-allegro
  (error "ut-to-date-time is not supported.")
  )

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

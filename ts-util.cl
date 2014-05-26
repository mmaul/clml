;;; -*- Mode: Lisp; Syntax: Common-Lisp; Coding:iso-2022-jp -*-
;;;
;;; References 
;;; [D] http://www.w3.org/TR/NOTE-datetime  

(defpackage :time-series-util
  (:use :cl 
        :excl
        :hjs.learn.read-data :hjs.util.meta :hjs.util.vector
        :hjs.util.matrix :statistics
        :ts-read-data)
  (:nicknames :ts-util)
  (:export #:ts-to-sta
           #:ts-
           #:sub-ts
           #:compose-ts
           #:merge-ts
           
           #:timeseries-model
           #:observed-ts
           #:predict
           #:statvis
           #:draw-ppm
           #:*r-stream*
           #:with-r
           #:open-eps-file
           #:date-time-to-ut
           #:ut-to-date-time))

(in-package :time-series-util)

#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :datetime))

(defclass timeseries-model ()
  ((observed-ts
    :initarg :observed-ts
    :accessor observed-ts
    :type time-series-dataset
    :initform (error "Must specify the observed timeseries data"))))
(defgeneric predict (timeseries-model &key n-ahead)
  (:documentation 
   "Calculate the value based on the timeseries-model for the observed timeseries data."))

(defmethod ts-to-sta ((d time-series-dataset) f-name 
                      &key (external-format :default) (fit t))
  (with-accessors ((dims dataset-dimensions)
                   (label time-label-name)
                   (pts ts-points)) d
  (with-open-file (out f-name :direction :output :if-exists :supersede
                   :external-format external-format)
    (princ (format nil
                   "(manifest :name \"~A\" :item-labels ~A~%~
~T:time-value #'(lambda (nth data) ~A)~%~
~T:data-values #'(lambda (nth data) ~A)~%~
~T:chart-fit ~A~%~
~T:chart-view :line~%~
~T:chart-item-tics ~A~%~
~T:chart-drawicons nil)"
                   (pathname-name f-name)
                   (cons (if label (format nil "\"~A\"" label) "\"Time\"")
                         (map 'list 
                           #'(lambda (dim) (format nil "\"~A\"" (dimension-name dim)))
                           dims))
                   (if label "(car data)" "nth")
                   (if label "(cdr data)" "data")
                   fit (expt 10 (1- (ceiling (log (length (ts-points d)) 10)))))
           out)
    (terpri out)
    (loop for p across pts
        do (terpri out)
           (if label
               (write (concatenate 'list `(,(ts-p-label p)) (ts-p-pos p)) :stream out)
             (write (coerce (ts-p-pos p) 'list) :stream out)
           )))))

(defmethod sub-ts ((d time-series-dataset) 
                   &key start end (range :all) except)
  (assert (notevery #'null (list start end range except)))
  (with-accessors ((d-start ts-start)
                   (d-end ts-end)
                   (dims dataset-dimensions)
                   (freq ts-freq)) d
    (cond ((integerp start)
           (setq start (list start 1)))
          ((null start)
           (setq start (ts-start d))))
    (cond ((integerp end)
           (setq end (list end 1)))
          ((null end)
           (setq end (ts-end d))))
    (assert (and (>= (tf-gap start d-end :freq freq) 0) (>= (tf-gap d-start end :freq freq) 0)))
    (let* ((data (copy-ts d))
           (freq (ts-freq data))
           (total-size (length (dataset-dimensions d)))
           (range1 (if (eq range :all)
                       (loop for i below total-size collect i)
                     (loop for i in range collect
                           (if (stringp i) 
                               (position i dims :test #'string-equal :key #'dimension-name) i))))
           (range (sort (set-difference range1 except) #'<)))
      (assert (every #'(lambda (pos) (> total-size pos)) range))
      (setf (ts-start data) (if (> (tf-gap start d-start :freq freq) 0)
                                d-start start)
            (ts-end data) (if (> (tf-gap end d-end :freq freq) 0)
                              end d-end)
            (dataset-dimensions data)
            (map 'vector #'(lambda (pos)
                             (aref (dataset-dimensions d) pos))
                 range)
            (ts-points data)
            (coerce
             (loop for point across (ts-points d)
                 as tf = (list (ts-p-time point) (ts-p-freq point))
                 as pos = (make-ts-point (ts-p-time point)
                                         (ts-p-freq point)
                                         (ts-p-label point)
                                         (let ((vec (make-dvec (length range))))
                                           (loop for i in range
                                               for j from 0
                                               as val = (coerce 
                                                         (aref (ts-p-pos point) i)
                                                         'double-float)
                                               do (setf (aref vec j) val))
                                           vec))
                 when (and (>= (tf-gap start tf :freq freq) 0)
                           (>= (tf-gap tf end :freq freq) 0))
                 collect pos)
             'vector))
      data)))

(defun time-intersection (d1 d2)
  (check-type d1 time-series-dataset)
  (check-type d2 time-series-dataset)
  (assert (= (ts-freq d1) (ts-freq d2)))
  (let* ((start (if (> (tf-gap (ts-start d1) (ts-start d2) :freq (ts-freq d1)) 0)
                    (ts-start d2) (ts-start d1)))
         (end (if (> (tf-gap (ts-end d1) (ts-end d2) :freq (ts-freq d1)) 0)
                  (ts-end d1) (ts-end d2))))
    (if (>= (tf-gap start end :freq (ts-freq d1)) 0)
        (values start end)
      (values nil nil))))

(defun merge-ts (d1 d2)
  (check-type d1 time-series-dataset)
  (check-type d2 time-series-dataset)
  (assert (= (ts-freq d1) (ts-freq d2)))
  (multiple-value-bind (start end)
      (time-intersection d1 d2)
    (assert (and start end))
    (let* ((dims (map 'list #'dimension-name (concatenate 'list (dataset-dimensions d1) (dataset-dimensions d2))))
           (n (length dims))
           (sub-d1 (sub-ts d1 :start start :end end))
           (sub-d2 (sub-ts d2 :start start :end end))
           (time-label-name (when (string-equal (time-label-name sub-d1)
                                                (time-label-name sub-d2))
                              (time-label-name sub-d1)))
           (time-labels (when time-label-name (map 'vector #'ts-p-label (ts-points sub-d1))))
           (pts (coerce 
                 (loop for point-1 across (map 'vector #'ts-p-pos (ts-points sub-d1))
                     for point-2 across (map 'vector #'ts-p-pos (ts-points sub-d2))
                     as point = (concatenate 'vector point-1 point-2)
                     collect (let ((vec (make-dvec n)))
                               (loop for val across point
                                   for i from 0
                                   do (setf (aref vec i) (coerce val 'double-float)))
                               vec))
                 'vector)))
      (make-constant-time-series-data 
       dims pts :time-label-name time-label-name :time-labels time-labels
       :start start :end end :freq (ts-freq d1)))))

(defmethod compose-ts ((d time-series-dataset) &key (range :all) except
                                                    (composer #'+)
                                                    (column-name "composed value"))
  (with-accessors ((dims dataset-dimensions)
                   (label time-label-name)
                   (pts ts-points)) d
  (let* ((total-size (length dims))
         (range1 (if (eq range :all)
                     (loop for i below total-size collect i)
                   range))
         (range (sort (set-difference range1 except) #'<))
         time-labels)
    (assert (every #'(lambda (pos) (> total-size pos)) range))
    (let ((data
           (map 'vector #'(lambda (p) 
                            (progn (push (ts-p-label p) time-labels)
                            (make-dvec 1 (apply composer (loop for i in range
                                                             with vec = (ts-p-pos p)
                                                             collect (aref vec i))))))
                pts)))
      (make-constant-time-series-data
       `(,column-name) data :time-label-name label 
       :time-labels (coerce (reverse time-labels) 'vector)
       :start (ts-start d) :end (ts-end d) :freq (ts-freq d))))))

(defun ts- (d1 d2)
  (check-type d1 time-series-dataset)
  (check-type d2 time-series-dataset)
  (assert (= (ts-freq d1) (ts-freq d2)))
  (assert (= (length (dataset-dimensions d1)) (length (dataset-dimensions d2))))
  (multiple-value-bind (start end)
      (time-intersection d1 d2)
    (assert (and start end))
    (let* ((sub-d1 (map 'vector #'(lambda (p) (ts-p-pos p))
                        (ts-points (sub-ts d1 :start start :end end))))
           (sub-d2 (map 'vector #'(lambda (p) (ts-p-pos p))
                        (ts-points (sub-ts d2 :start start :end end))))
           (column-names
            (map 'list #'dimension-name (dataset-dimensions d1)))
           (size (length column-names))
           (data (map 'vector
                   (lambda (p1 p2)
                     (declare (type dvec p1 p2))
                     (let* ((sp (make-dvec size)))
                       (declare (type dvec sp))
                       (loop for i from 0 below size
                           as p-val = (- (aref p1 i) (aref p2 i))
                           do (setf (aref sp i) (coerce p-val 'double-float))
                           finally (return sp))))
                   sub-d1 sub-d2)))
      (make-constant-time-series-data
       column-names data
       :start start :end end :freq (ts-freq d1)))))


(defun draw-ppm (data-list fname &key (width-unit 10) (height-unit 10))
  (with-open-file (out fname :direction :output :if-exists :supersede)
    (format out "P3~%~d ~d~%255~%" 
            (* width-unit (length (car data-list))) ;width
            (* height-unit (length data-list))) ;height
    (loop for data in data-list
        do (loop repeat height-unit
               with str-list
               = (loop for val in data
                     append (if val 
                 (make-list width-unit :initial-element
                            (let ((val (min 255 (max 0 (round (* val 255))))))
                              (unless (or (zerop val) (plusp val))
                                (error "invalid values: ~A" val))
                              (format nil "~D ~D ~D~%" val val val)))
                 (make-list width-unit :initial-element (format nil "0 255 255~%"))))
               do (format out "~{~A~}" str-list)))))
    
(defun statvis (ts &key (external-format :default)
                     (fname "temp"))
  (let* ((stafile (format nil "statvis/~A.sta" fname)))
    (ts-to-sta ts stafile :external-format external-format)
    (let ((stream
           (run-shell-command
            (format nil "\"statvis/STATVIS\" -- \"~A\""
                    stafile) :input :stream :output :stream :wait nil)))
      (close-cmd-stream stream))))


;;;;; for R input/output
(defparameter *r-path* 
  #+unix "R"
  #+mswindows
  "C:/Program Files/R/R-2.4.1/bin/R.exe") ; Pathname to "R.exe"
(defparameter *r-stream* nil)

(defun read-new-value ()
  (format t "Enter a new value: ")
  (multiple-value-list (eval (read))))

(defun start-r ()
  (unless (probe-file *r-path*)
    (restart-case
        (error "File ~A does not exist." *r-path*)
      (set-r-path (new-path)
          :report "Set *r-path*."
          :interactive read-new-value
        (setq *r-path* new-path))))
  (multiple-value-bind (shell-stream err pid)
      (run-shell-command (format nil "~A --no-save" *r-path*) 
                         :wait nil :input :stream :output :stream :show-window :hide)
    (declare (ignore err pid))
    (setf *r-stream* shell-stream)))

(defun close-cmd-stream (stream)
  (close stream)
  #+allegro (system:reap-os-subprocess)
  (setf stream nil))
(defmacro with-r (&rest body)
  `(unwind-protect
       (progn 
         (start-r)
         ,@body)
     (close-cmd-stream *r-stream*)))
  

(defun get-from-shell (stream)
  (do ((ch (read-char-no-hang stream)
           (read-char-no-hang stream)))
      ((null ch))
    (write-char ch *standard-output*)))

(defun open-eps-file (f-name)
  #+mswindows
  (let ((stream (run-shell-command "cmd" :wait nil :input :stream :output :stream
                                   :show-window :hide)))
    (format stream "~A~%" f-name)
    (close-cmd-stream stream))
  #+unix
  (run-shell-command (format nil "evince ~S" f-name) :wait nil))

(defun draw-by-R (&rest ts-datasets)
  (let* ((f-name (loop for i from 1
                     as f-name = (format nil "ts-~A.eps" i)
                     when (not (probe-file f-name))
                     do (return f-name))))
    (with-r
        (let ((id-list
               (loop for d in ts-datasets
                   for id from 1
                   collect 
                     (progn
                       (with-accessors ((start ts-start)
                                        (end ts-end)
                                        (points ts-points)
                                        (freq ts-freq)
                                        (dims dataset-dimensions)) d
                         (assert (= 1 (length dims)))
                         (let ((seq (map 'list #'(lambda (p)
                                                   (aref (ts-p-pos p) 0))
                                         points)))
                           (format *r-stream* "temp~D<-c(~{~,3F~^,~})~%" id seq)
                           (format *r-stream* "temp~D<-ts(temp~D,start=c(~D,~D),frequency=~D)~%"
                                   id id (first start) (second start) freq)))
                       id))))
          (format *r-stream* "postscript(\"~A\", height=9, width=14, pointsize=15)~%" f-name)
          (format *r-stream* "ts.plot(~{temp~D~^,~},gpars=list(col=c(~{~D~^,~})))~%"
                  id-list id-list)
          (format *r-stream* "dev.off()~%")
          (format *r-stream* "q()~%")))
    (open-eps-file f-name)))
  
(defmethod draw-exp-smoothing-by-R ((d time-series-dataset)
                                    &key 
                                    (learn-end (tf-incl (ts-end d) -1 :freq (ts-freq d))))
  (with-accessors ((start ts-start)
                   (end ts-end)
                   (points ts-points)
                   (freq ts-freq)
                   (dims dataset-dimensions)) d
    (assert (= 1 (length dims)))
    (let ((f-name (loop for i from 1
                      as f-name = (format nil "HoltWinters-R-~A.eps" i)
                      when (not (probe-file f-name))
                      do (return f-name)))
          (seq (map 'list #'(lambda (p)
                              (aref (ts-p-pos p) 0))
                    points)))
      (with-r
          (format *r-stream* "temp<-c(~{~A~^,~})~%" seq)
        (format *r-stream* "temp<-ts(temp,start=c(~A,~A),frequency=~A)~%"
                (first start) (second start) freq)
        (format *r-stream* "past<-window(temp,end=c(~A,~A))~%" (first learn-end) (second learn-end))
        (format *r-stream* "future<-window(temp,start=c(~A,~A))~%" (first learn-end) (second learn-end))
        (format *r-stream* "model<-HoltWinters(past,seasonal=\"additive\")~%")
        (format *r-stream* "pred<-predict(model,n.ahead=~A)~%" (1+ (tf-gap learn-end (ts-end d) :freq freq)))
        (format *r-stream* "postscript(\"~A\", height=9, width=14, pointsize=15)~%" f-name)
        (format *r-stream* "plot(model,predicted.value=pred)~%lines(future)~%")
        (format *r-stream* "dev.off()~%")
        )
      (open-eps-file f-name))))

(defun ts-to-sexp (d)
  (check-type d time-series-dataset)
  (concatenate 'list
    `(,(map 'list #'dimension-name (dataset-dimensions d)))
    (loop for p across (ts-points d)
        collect (coerce (ts-p-pos p) 'list))))



(defun split (delim str)
  (loop with delim-size = (length delim)
      for start = 0 then (and pos (+ pos delim-size))
      for pos = (and start (search delim str :start2 start))
      while start
      collect (subseq str start pos)))

(defun date-time-to-ut (date-time &optional daylight-saving-time-p)
  (let ((ut
         #+allegro
         (util.date-time:date-time-to-ut date-time)
         #-allegro
         (labels ((to-YYYY-MM-DD-list (str) 
                    (let* ((list-split-by-hyphen (split "-" str))
                           (list-split-by-hyphen-size (length list-split-by-hyphen)))
                      (case list-split-by-hyphen-size 
                        (1 (append (mapcar #'parse-integer list-split-by-hyphen) (list 1 1)))
                        (2 (append (mapcar #'parse-integer list-split-by-hyphen) (list 1)))
                        (3 (mapcar #'parse-integer list-split-by-hyphen))
                        (t (error "data-time-to-ut: Argument does not match the format of ISO8691.")))))
                  (hh-mm-to-hour (str)
                    (let* ((list-split-by-colon (mapcar #'parse-integer (split ":" str)))
                           (list-split-by-colon-size (length list-split-by-colon)))
                      (case list-split-by-colon-size
                        (1 (first list-split-by-colon))
                        (2 (+ (first list-split-by-colon) (/ (second list-split-by-colon) 60)))
                        (t (error "data-time-to-ut: Argument does not match the format of ISO8691.")))))
                  (split-hh-mm-ss-and-timezone (str)
                    (let ((list-split-by-plus (split "+" str))
                          (list-split-by-minus (split "-" str)))
                      (cond ((= (length list-split-by-plus) 2)
                             (setf (second list-split-by-plus)
                               (- (hh-mm-to-hour (second list-split-by-plus))))
                             list-split-by-plus)
                            ((= (length list-split-by-minus) 2)
                             (setf (second list-split-by-minus)
                               (hh-mm-to-hour (second list-split-by-minus)))
                             list-split-by-minus)
                            (t (list str nil)))))
                  (to-hh-mm-ss-list (str)
                    (let* ((list-split-by-colon (split ":" str))
                           (list-split-by-colon-size (length list-split-by-colon)))
                      (case list-split-by-colon-size
                        (1 (setf (first list-split-by-colon)
                             (substitute #\. #\, (first list-split-by-colon)))
                           (let* ((list-split-by-dot (split "." (pop list-split-by-colon)))
                                  (hh (parse-integer (pop list-split-by-dot)))
                                  (mm (let ((hh-decimal (pop list-split-by-dot)))
                                        (if (null hh-decimal) 0
                                          (/ (* 60 (parse-integer hh-decimal))
                                             (expt 10 (length hh-decimal)))))))
                             (list hh mm 0)))
                        (2 (setf (second list-split-by-colon)
                             (substitute #\. #\, (second list-split-by-colon)))
                           (let* ((hh (parse-integer (pop list-split-by-colon)))
                                  (list-split-by-dot (split "." (pop list-split-by-colon)))
                                  (mm (parse-integer (pop list-split-by-dot)))
                                  (ss (let ((mm-decimal (pop list-split-by-dot)))
                                        (if (null mm-decimal) 0
                                          (/ (* 60 (parse-integer mm-decimal))
                                             (expt 10 (length mm-decimal)))))))
                             (list hh mm ss)))
                        (3 (mapcar #'parse-integer list-split-by-colon))
                        (t (error "data-time-to-ut: Argument does not match the format of ISO8691.")))))
                  (my-encode-universal-time 
                      (&key year month day hour minute second time-zone)
                    (if (null time-zone) (+ (encode-universal-time 0 minute hour day month year) second)
                      (+ (encode-universal-time 0 0 hour day month year time-zone) 
                         (* minute 60)
                         second))))
           (let* ((list-split-by-T (split "T" date-time))
                  (YYYY-MM-DD-queue (to-YYYY-MM-DD-list (pop list-split-by-T)))
                  (list-hh-mm-ss-and-timezone (split-hh-mm-ss-and-timezone (pop list-split-by-T)))
                  (hh-mm-ss-queue (to-hh-mm-ss-list (pop list-hh-mm-ss-and-timezone)))
                  (time-zone (pop list-hh-mm-ss-and-timezone)))
             (my-encode-universal-time 
              :year (pop YYYY-MM-DD-queue) :month (pop YYYY-MM-DD-queue)
              :day (pop YYYY-MM-DD-queue) :hour (pop hh-mm-ss-queue)
              :minute (pop hh-mm-ss-queue) :second (pop hh-mm-ss-queue) :time-zone time-zone)))))
    (if daylight-saving-time-p
        (- ut 3600) ut)))

(defun ut-to-date-time (ut &optional daylight-saving-time-p)
  (when daylight-saving-time-p
    (setq ut (+ ut 3600)))
  #+allegro
  (format nil "~a" (util.date-time:ut-to-date-time ut))
  #-allegro
  (assert (>= ut 0))
  (multiple-value-bind
      (second minute hour date month year day daylight-p zone)
      (decode-universal-time ut)
    (declare (ignore day))
    (declare (ignore daylight-p))
    (let* ((zone-sign 
            (if (<= zone 0) "+" "-"))
           (zone (abs zone)))
      (format nil "~d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d~a~2,'0d:00"
              year month date
              hour minute second
              zone-sign zone))))
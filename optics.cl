;;; -*- mode: lisp; syntax: common-lisp -*-

;;; Clustering Method : OPTICS
;;; NAGANUMA Shigeta, 2009

;;; Reference:
;;; Mihael Ankerst, Markus M. Breuning, Hans-Peter Kriegel and Jorg Sander. 
;;; "OPTICS:Ordering Points To Identify the Clustering Structure."
;;; Institute for Computer Science, University of Munich

(defpackage :optics
  (:use :cl
        :hjs.learn.read-data
        :hjs.util.matrix
        :statistics)
  (:export :optics
           :optics-main
           :make-optics-input))

(in-package :optics)

(defclass optics-input ()
  ((input-data :initform nil :initarg :input-data :accessor input-data)
   (distance :initform nil :initarg :distance :accessor distance)
   (epsilon :initform nil :initarg :epsilon :accessor epsilon)
   (min-pts :initform nil :initarg :min-pts :accessor min-pts)
   (r-epsilon :initform nil :initarg :r-epsilon :accessor r-epsilon)
   (normalize :initform nil :initarg :normalize :accessor normalize)
   (target-cols :initform nil :initarg :target-cols :accessor target-cols)
   (point-objs :initform nil :initarg :point-objs :accessor point-objs)
   ))

(defclass optics-point-object ()
  ((id :initform nil :initarg :id :accessor id)
   (c-id :initform nil :initarg :c-id :accessor c-id)
   (processed :initform nil :initarg :processed :accessor processed)
   (reachability-d :initform nil :initarg :reachability-d
                   :accessor reachability-d)
   (core-d :initform nil :initarg :core-d :accessor core-d)
   (optics-input :initform nil :initarg :optics-input :accessor optics-input)))

(defclass optics-output ()
  ((ordered-data :initform nil :initarg :ordered-data
                 :accessor ordered-data)
   (cluster-info :initform nil :initarg :cluster-info
                 :accessor cluster-info)))

(defmethod print-object ((o optics-output) stream)
  (with-accessors ((info cluster-info)) o
    (print-unreadable-object (o stream :type t :identity nil))
    (format stream "~&[ClusterID] SIZE |~{ [~A] ~A~^ |~}~%"
            (loop for (c-id . size) in info
                append `(,c-id ,size)))))

(defclass order-seeds ()
  ((seeds :initform nil :initarg :seeds :accessor seeds)))

;;; optics function
;;; optics
;;; -input
;;; input-path    入力ファイルのパス, :csv or :sexp (1行目はカラム名, 以降データ)
;;; epsilon       近傍半径
;;; min-pts       近傍最小データ数
;;; r-epsilon     reachability への閾値
;;; target-cols   対象カラム(sequence)
;;; &key
;;; file-type     :csv or :sexp
;;; csv-type-spec csvファイルの各列の型 string double-float integer etc. 
;;; distance      def. of distance :manhattan, :euclid or :cosine
;;; normalize     t or nil (now only nil)
;;; -output
;;; object of class :optics-output
(defun optics (input-path epsilon min-pts r-epsilon
               target-cols &key (file-type :sexp) 
                                (csv-type-spec '(string double-float double-float))
                                (distance :manhattan)
                                (normalize nil)
                                (external-format :default))
  (assert (plusp epsilon))
  (assert (plusp min-pts))
  (assert (and (plusp r-epsilon) (<= r-epsilon epsilon)))
  (assert target-cols)
  (let* ((dataset
          (read-data-from-file input-path 
                               :type file-type
                               :csv-type-spec csv-type-spec
                               :external-format external-format))
         (range (map 'list
                  #'(lambda (col-name)
                      (dimension-index
                       (find col-name (dataset-dimensions dataset)
                             :key #'dimension-name :test #'string-equal)))
                  target-cols)))
    (setq dataset
      (pick-and-specialize-data dataset
                                :range range
                                :data-types 
                                (make-list (length range)
                                           :initial-element :numeric)))
    (%optics dataset epsilon min-pts r-epsilon
             :distance distance :normalize normalize)))

(defmethod %optics ((numeric-dataset numeric-dataset) epsilon min-pts r-epsilon
                    &key (distance :manhattan) normalize)
  (when normalize
    (setf (dataset-numeric-points numeric-dataset) 
      (standardize (dataset-numeric-points numeric-dataset))))
  (let* ((optics-input (make-optics-input
                        (dataset-numeric-points numeric-dataset)
                        epsilon min-pts r-epsilon
                        :distance distance :normalize normalize))
         (optics-output (make-instance 'optics-output)))
    (optics-main optics-input optics-output)
    optics-output))

(defun optics-main (optics-input optics-output)
  (loop for object across (point-objs optics-input)
      when (not (processed object))
      do (let* ((neighbors (get-neighbors optics-input object))
                (order-seeds
                 (make-instance 'order-seeds :seeds nil)))
           (setf (processed object) t)
           (add optics-output object)
           (when (set-core-d object neighbors)
             (update order-seeds neighbors object)
             (loop while (seeds order-seeds)
                 as current-obj = (next order-seeds)
                 as neighbors = (get-neighbors optics-input current-obj)
                 do (setf (processed current-obj) t)
                    (add optics-output current-obj)
                    (when (set-core-d current-obj neighbors)
                      (update order-seeds neighbors current-obj))))))
  (cluster-numbering optics-output optics-input))

(defun make-optics-input (input-data epsilon min-pts r-epsilon 
                          &key (distance :manhattan)
                               (normalize nil))
  (assert (<= r-epsilon epsilon))
  (setq distance
    (case distance
      (:manhattan #'hjs.util.vector:manhattan-distance)
      (:euclid #'hjs.util.vector:euclid-distance)
      (:cosine #'hjs.util.vector:cosine-distance)
      (t (error "illegal name for distance | ~A" distance))))
  (let* ((optics-input (make-instance 'optics-input
                         :input-data input-data :distance distance
                         :epsilon epsilon :min-pts min-pts
                         :r-epsilon r-epsilon :normalize normalize))
         (point-objs
          (coerce 
           (loop for point across input-data
               for id from 0
               collect (make-instance 'optics-point-object
                         :id id :optics-input optics-input))
           'vector)))
    (setf (point-objs optics-input) point-objs)
    optics-input))

(defmethod point ((obj optics-point-object))
  (svref (input-data (optics-input obj)) (id obj)))

(defmethod get-neighbors ((input optics-input) object)
  (let* ((d (distance input))
         (e (epsilon input))
         (line-id (id object))
         (target-vals (point object)))
    (coerce 
     (loop for point-obj across (point-objs input)
         for i from 0
         as obj-vals = (point point-obj)
         as distance = (funcall d target-vals obj-vals)
         when (and (/= i line-id)
                   (<= distance e))
         collect point-obj)
     'vector)))

(defmethod set-core-d ((object optics-point-object) 
                       neighbors)
  (let* ((input (optics-input object))
         (d (distance input))
         (minpts (min-pts input)))
    (if (>= (length neighbors) minpts)
        (let* ((d-list (sort (map 'list
                               #'(lambda (obj)
                                   (funcall d 
                                            (point object)
                                            (point obj)))
                               neighbors)
                             #'<)))
          (setf (core-d object)
            (car (last (subseq d-list 0 minpts)))))
      (setf (core-d object) nil))
    (core-d object)))

(defmethod update ((o-seeds order-seeds) neighbors center-obj)
  (let* ((d (distance (optics-input center-obj)))
         (c-d (core-d center-obj)))
    (assert (numberp c-d))
    (loop for obj across neighbors
        when (not (processed obj))
        do (let ((r-dist (max c-d (funcall d 
                                           (point obj)
                                           (point center-obj)))))
             (cond ((not (reachability-d obj))
                    (setf (reachability-d obj) r-dist)
                    (insert o-seeds obj))
                   ((< r-dist (reachability-d obj))
                    (setf (reachability-d obj) r-dist)
                    (decrease o-seeds obj)))))))

(defmethod insert ((o-seeds order-seeds) obj)
  (if (seeds o-seeds)
      (setf (seeds o-seeds)
        (merge 'list
               (sort (seeds o-seeds) #'< :key #'reachability-d)
               `(,obj)
               #'< :key #'reachability-d))
    (setf (seeds o-seeds) `(,obj))))

(defmethod decrease ((o-seeds order-seeds) obj)
  (declare (ignore obj))
  (setf (seeds o-seeds)
    (sort (seeds o-seeds) #'< :key #'reachability-d)))

(defmethod next ((o-seeds order-seeds))
  (let* ((seeds (seeds o-seeds))
         (obj (car seeds))
         (next-sds (cdr seeds)))
    (setf (seeds o-seeds) next-sds)
    obj))

(defmethod add ((output optics-output) obj)
  (setf (ordered-data output)
    (append (ordered-data output) `(,obj))))

(defmethod cluster-numbering ((output optics-output) 
                              input &key (a 1.01))
  (let* ((r-e (r-epsilon input))
         (a-e (* a (epsilon input))))
    (setf (ordered-data output)
      (coerce 
       (cons 
        #("ID" "reachability" "core distance" "ClusterID")
        (loop for obj in (ordered-data output)
            as r = (or (reachability-d obj) a-e)
            as c = (or (core-d obj) a-e)
            as id = (id obj)
            with c-id = 0
            with cluster-info
            collect
              (if (> r r-e)
                  (if (<= c r-e)
                      (progn
                        (incf c-id)
                        (if (assoc c-id cluster-info :test #'=)
                            (incf (cdr (assoc c-id cluster-info :test #'=)))
                          (push (cons c-id 1) cluster-info))
                        `#(,id ,a-e ,c ,c-id))
                    (progn
                      (if (assoc -1 cluster-info :test #'=)
                          (incf (cdr (assoc -1 cluster-info :test #'=)))
                        (push (cons -1 1) cluster-info))
                      `#(,id ,a-e ,a-e -1)))
                (progn
                  (if (assoc c-id cluster-info :test #'=)
                            (incf (cdr (assoc c-id cluster-info :test #'=)))
                          (push (cons c-id 1) cluster-info))
                  `#(,id ,r ,c ,c-id)))
            finally
              (setf (cluster-info output)
                (sort cluster-info #'< :key #'car))))
       'vector))))

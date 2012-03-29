;; package of interfaces for :text.hdp-lda
(defpackage :hdp-lda
  (:use :cl :hjs.learn.read-data :hjs.util.vector :text.hdp-lda)
  (:import-from :text.hdp-lda 
                #:hdp-lda-alpha
                #:hdp-lda-beta
                #:hdp-lda-gamma)
  (:export #:hdp-lda
           #:topic-count
           #:get-trend-topics
           #:hdp-lda-alpha
           #:hdp-lda-beta
           #:hdp-lda-gamma))
           
(in-package :hdp-lda)

(defmethod hdp-lda ((dataset numeric-dataset) &key (sampling 100) 
                                                   (initial-k 0)
                                                   hyper-parameters ;; (alpha gamma beta)
                                                   )
  (assert (and (integerp sampling) (plusp sampling)))
  (let* ((bow-hash (make-bow-hash dataset))
         (docs (make-docs dataset bow-hash))
         (model (if hyper-parameters
                    (progn (assert (and (every #'numberp hyper-parameters)
                                        (eql 3 (length hyper-parameters))))
                           (make-instance 'hdp-lda :data docs
                                          :k initial-k
                                          :alpha (first hyper-parameters)
                                          :beta (second hyper-parameters)
                                          :gamma (third hyper-parameters)))
                  (make-instance 'hdp-lda :data docs :k initial-k))))
    (loop repeat sampling
        initially (initialize model)
        do (sampling model)
        finally (assign-theta model))
    (values (make-document-theta-result model)
            (make-topic-beta-result model)
            model)))

(defmethod make-document-theta-result ((model hdp-lda))
  (let* ((docs (sort (copy-seq (hdp-lda-data model)) #'< :key #'document-id))
         (doc-thetas (map 'vector (lambda (doc) (specialize-vec (document-thetas doc))) docs))
         (column-names (topic-names model)))
    (make-numeric-dataset column-names doc-thetas)))
(defmethod make-topic-beta-result ((model hdp-lda))
  (let* ((column-names (cons "Topic ID" (loop for i upto (vocabulary model) collect (revert-word model i))))
         (phi (map 'vector #'specialize-vec (get-phi model)))
         (topic-ids (map 'vector (lambda (id) (make-array 1 :initial-element id :element-type t))
                         (topic-names model)))
         (category-index '(0))
         (numeric-indices (loop for i from 1 to (length (aref phi 0)) collect i)))
    (make-numeric-and-category-dataset column-names phi numeric-indices
                                       topic-ids category-index)))

(defmethod topic-names ((model hdp-lda))
  (loop repeat (topic-count model) for i from 1
      collect (format nil "Topic ~D" i)))
(defmethod make-bow-hash ((dataset numeric-dataset))
  (loop with hash = (make-hash-table :test #'eql)
      for name across (map 'vector #'dimension-name (dataset-dimensions dataset))
      for i from 0
      do (setf (gethash i hash) name)
      finally (return hash)))
(defmethod make-docs ((dataset numeric-dataset) bow-hash)
  (let ((id -1))
    (coerce 
     (loop for freq-vec across (dataset-numeric-points dataset)
         as words = (extract-words freq-vec bow-hash)
         when (> (length words) 0)
         collect (make-instance 'document :id (incf id) :words words))
     'vector)))
(defun extract-words (freq-vec bow-hash)
  (coerce (loop for freq across freq-vec for i from 0
              when (> freq 0)
              append (multiple-value-bind (q r) (floor freq)
                       (if (zerop r)
                           (make-list q :initial-element (gethash i bow-hash))
                         (error "illegal frequency value: ~A" freq))))
          'vector))

;; ref: Finding scientific topics, Thomas L.Griffiths, Mark Steyvers
;;      http://www.ncbi.nlm.nih.gov/pmc/articles/PMC387300/
(defmethod get-trend-topics ((model hdp-lda) &key (trend :hot) ;; :hot | :cold
                                                  (ntopics 10)
                                                  (nwords 10))
  (assert (and (plusp ntopics) (plusp nwords)))
  (let* ((mean-vec (mean-theta model))
         (id-mean-alist (map 'list #'cons 
                             (loop for i below (topic-count model) collect i)
                             mean-vec))
         (n-words (get-top-n-words model nwords))
         (names (topic-names model)))
    (loop repeat ntopics
        for (id . mean-theta) in (sort id-mean-alist (ecase trend (:hot #'>) (:cold #'<)) :key #'cdr)
        as words = (aref n-words id)
        as name = (nth id names)
        collect (cons name (cons words mean-theta)))))
(defmethod mean-theta ((hdp-lda hdp-lda))
  (let ((mean-vec (make-dvec (topic-count hdp-lda) 0d0)))
    (loop for doc across (hdp-lda-data hdp-lda)
        for len fixnum from 1
        as theta = (specialize-vec (document-thetas doc))
        do (do-vecs ((_ mean-vec :type double-float :setf-var sf)
                     (val theta :type double-float))
             (declare (ignore _))
             (incf sf val))
        finally (return 
                  (map 'vector 
                    (lambda (val) (/ (the double-float val) len))
                    mean-vec)))))
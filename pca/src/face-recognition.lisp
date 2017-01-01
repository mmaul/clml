
(in-package :clml.pca)

(defun alist-count (list &key (test #'eq))
  (loop with alist for item in list
      as sub-alist = (assoc item alist :test test)
      do (if sub-alist (incf (cdr sub-alist))
           (push (cons item 1) alist))
      finally (return alist)))

;;; Normalize face
(defgeneric normalize-faces (face-dataset &key sd))
(defmethod normalize-faces ((face-dataset numeric-and-category-dataset) &key sd)
  (assert (or (null sd) (and (numberp sd) (plusp sd))))
  (let ((d (copy-dataset face-dataset)))
    (loop for face-pt of-type dvec across (dataset-numeric-points d)
        as face-sd of-type double-float = (standard-deviation face-pt)
        as face-m of-type double-float = (mean face-pt)
        as sd/sd of-type double-float
        = (if sd (dfloat (/ sd face-sd)) 1d0)
        do (do-vec (val face-pt :type double-float :setf-var sf)
             (setf sf (* sd/sd (- val face-m)))))
    d))

(defun base-projection (bases pt result)
  (check-type bases (simple-array dvec (*)))
  (check-type pt dvec)
  (check-type result dvec)
  (do-vecs ((base bases :type dvec)
            (_ result :type double-float :setf-var sf))
    #-sbcl (declare (ignore _))
    (setf sf (inner-product base pt)))
  result)

(defun base-projection-pts (bases points result)
  (check-type bases (simple-array dvec (*)))
  (check-type points (simple-array dvec (*)))
  (check-type result (simple-array dvec (*)))
  (do-vecs ((p points :type dvec :index-var ip)
            (r result :type dvec))
    (base-projection bases p r))
  result)

;;; note: this fcn is destructive on 'score'
(defun make-eigenface-hash (ids score &optional (type :mean))
  (declare (type vector ids)
           (type (simple-array dvec (*)) score))
  (let ((hash (make-hash-table :test #'equalp))) ;; !!!!
    (case type
      (:mean
       (loop
           with counter = (map 'list (lambda (id) (cons id 0)) ids)
           for id across ids
           for sc across score
           do (incf (cdr (assoc id counter :test #'string-equal))) ;; !!!!
              (multiple-value-bind (val exist-p) (gethash id hash)
                (setf (gethash id hash)
                  (if exist-p (v+ sc val sc) sc)))
           finally (maphash (lambda (id vec)
                              (let ((c (cdr (assoc id counter :test #'string-equal)))) ;; !!!!
                                (setf (gethash id hash)
                                  (do-vec (v vec :type double-float :setf-var sf :return vec)
                                    (setf sf (/ v c))))))
                            hash)))
      (:set
       (loop for id across (remove-duplicates ids :test #'string-equal) ;; !!!!
           do (setf (gethash id hash)
                (coerce
                 (loop for id-1 across ids for i from 0
                     when (string-equal id-1 id) ;; !!!!
                     collect (aref score i)) 'vector)))))
    hash))

(defgeneric make-face-estimator-eigenface (face-dataset pca-result pca-model &key dimension-thld
                               id-column
                               d-fcn))
(defmethod make-face-estimator-eigenface
    ((face-dataset numeric-and-category-dataset)
     pca-result pca-model &key dimension-thld
                               (id-column "personID")
                               (d-fcn #'euclid-distance))
  (unless dimension-thld (setq dimension-thld (length (loading-factors pca-model))))
  (let* ((points (map 'vector #'copy-seq
                      (dataset-numeric-points face-dataset)))
         (e-mean (centroid pca-model))
         (c-points (do-vec (p points :type dvec :return points) (v- p e-mean p)))
         (dim
          (cond ((and (typep dimension-thld 'integer)
                      (plusp dimension-thld)) dimension-thld)
                ((and (typep dimension-thld 'float)
                      (< 0 dimension-thld 1))
                 (count-if
                  (let ((s (reduce #'+ (contributions pca-result))) (c 0))
                    (lambda (v)
                      (cond ((>= dimension-thld c) (incf c (/ v s)) t) (t nil))))
                  (contributions pca-result)))))
         (bases (subseq (loading-factors pca-model) 0 dim))
         (score (coerce (loop repeat (length points) collect (make-dvec dim)) 'vector))
         (ids (let ((pos (dimension-index
                          (find id-column (dataset-dimensions face-dataset)
                                :test #'string-equal
                                :key #'dimension-name))))
                (map 'vector
                  (lambda (vec) (aref vec pos)) (dataset-category-points face-dataset))))
         eigenface-hash)
    (declare (type (simple-array dvec (*)) c-points score))

    ;; projection
    (base-projection-pts bases c-points score)

    ;; make eigenfaces
    (setq eigenface-hash
      (make-eigenface-hash ids score :mean))

    ;; make estimator
    (let ((face-estimator
           (lambda (face-vec)
             (assert (eql (length face-vec) (length (aref bases 0))))
             (let ((s-dvec (make-dvec (length bases)))
                   (face-v (copy-seq face-vec)))
               (declare (type dvec s-dvec face-v))

               ;; demean
               (v- face-v e-mean face-v)
               ;; projection
               (base-projection bases face-v s-dvec)

               ;; calc distance
               (loop with candidate
                   with min = most-positive-double-float
                   for id being each hash-key in eigenface-hash
                   using (hash-value eigen-face)
                   as d = (funcall d-fcn s-dvec eigen-face)
                   when (> min d) do (setq candidate id min d)
                   finally (return candidate))))))
      (values face-estimator eigenface-hash (length bases)))))


(defparameter *minimum-number-for-subspace* 5)
(defun make-subspace-hash
    (ids data-vecs &key (pca-method :correlation) (dimension-thld 5))
  (declare (type vector ids) (type (simple-array dvec (*))))
  (let ((hash (make-hash-table :test #'equalp)) ;; !!!!
        (c 0) total)
    (loop for id across ids
        for vec across data-vecs
        do (push vec (gethash id hash)))
    (maphash
     (lambda (id vecs)
       (when (> *minimum-number-for-subspace* (length vecs))
         (restart-case (error "Number of data for ~A (~A) is insufficient." id (length vecs))
           (remove-the-id ()
               :report (lambda (strm) (format strm "Abandon ~A to learn." id))
             (remhash id hash))))) hash)
    (setq total (hash-table-count hash))
    (maphash (lambda (k v)
               (format t "~&generating subspace for ~A: ~A of ~A~%" k (incf c) total)
               (let* ((d (make-numeric-dataset (loop repeat (length (nth 0 v)) for i from 1
                                                   collect (format nil "col~A" i))
                                               (coerce (reverse v) 'vector)))
                      (pca-result
                       (ignore-errors
                        (sub-princomp d :method pca-method :dimension-thld dimension-thld))))
                 (if pca-result
                     (progn
                       (format t "~&Subspace Dimension: ~A~%" (length (loading-factors pca-result)))
                       (setf (gethash k hash)
                         `(:mean ,(centroid pca-result) :eigen-vecs ,(loading-factors pca-result)
                                 :standard-deviations ,(when (eq pca-method :correlation)
                                                         (orig-data-standard-deviations pca-result)))))
                   (progn (remhash k hash)
                          (format t "~&Failed to make subspace for ~A.~%" k)))))
             hash)
    hash))

(defun make-face-estimator-subspace%
    (subspace-hash pca-method
     &key
     (similarity-fcn
      (lambda (proj-vec org-vec) ;; |cosine|
        (declare (type dvec proj-vec org-vec))
        (abs (/ (inner-product proj-vec org-vec)
                (* (distance-to-origin proj-vec) (distance-to-origin org-vec)))))))
  (lambda (face-vec)
    (loop
        with candidate
        with max-s = most-negative-double-float
        for id being each hash-key in subspace-hash using (hash-value val)
        as mvec = (getf val :mean)
        as dim = (length mvec)
        as demvec = (let ((demvec (v- face-vec mvec (make-dvec dim))))
                      (when (eq pca-method :correlation)
                        (do-vecs ((v demvec :type double-float :setf-var sv)
                                  (s (getf val :standard-deviations) :type double-float))
                          (setf sv (/ v s)))) demvec)
        as eigen-vecs = (getf val :eigen-vecs)
        as proj-vec = (let ((proj-vec (make-dvec dim 0d0)))
                        (do-vecs ((evec eigen-vecs :type dvec)
                                  (val (base-projection eigen-vecs demvec (make-dvec (length eigen-vecs)))
                                       :type double-float))
                          (v+ proj-vec (v-scale evec val (make-dvec dim)) proj-vec))
                        proj-vec)
        as similarity = (funcall similarity-fcn proj-vec demvec)
        when (> similarity max-s)
        do (setq max-s similarity candidate id)
        collect (cons id similarity) into ss
        finally (return (values candidate ss)))))

(defgeneric make-face-estimator-subspace
    (face-dataset
     &key id-column
          dimension-thld
          pca-method))
(defmethod make-face-estimator-subspace
    ((face-dataset numeric-and-category-dataset)
     &key (id-column "personID")
          (dimension-thld 5)
          (pca-method :correlation))
  (unless dimension-thld (length (dataset-dimensions face-dataset)))
  (let* ((ids (let ((pos (dimension-index
                          (find id-column (dataset-dimensions face-dataset)
                                :test #'string-equal
                                :key #'dimension-name))))
                (map 'vector (lambda (vec) (aref vec pos)) (dataset-category-points face-dataset))))
         (subspace-hash
          (make-subspace-hash ids (dataset-numeric-points face-dataset)
                              :pca-method pca-method :dimension-thld dimension-thld))
         (base-num
          (loop with max = 0
              with min = most-positive-fixnum
              for v being each hash-value in subspace-hash
              as dim = (the fixnum (length (getf v :eigen-vecs)))
              when (> dim max) do (setq max dim)
              when (< dim min) do (setq min dim)
              finally (return (if (eql max min) max
                                (format nil "min: ~A, max: ~A" min max)))))
         (estimator (make-face-estimator-subspace% subspace-hash pca-method)))
    (values estimator subspace-hash base-num)))
(defgeneric make-face-estimator-random-forest-with-pca
    (face-dataset pca-result pca-model &key dimension-thld id-column
                               tree-test
                               tree-number))
(defmethod make-face-estimator-random-forest-with-pca
    ((face-dataset numeric-and-category-dataset)
     pca-result pca-model &key dimension-thld (id-column "personID")
                               (tree-test #'clml.decision-tree.decision-tree::delta-gini)
                               (tree-number 500))
  (unless dimension-thld (setq dimension-thld (length (loading-factors pca-model))))
  (let* ((points (map 'vector #'copy-seq
                      (dataset-numeric-points face-dataset)))
         (e-mean (centroid pca-model))
         (c-points (do-vec (p points :type dvec :return points) (v- p e-mean p)))
         (dim
          (cond ((and (typep dimension-thld 'integer)
                      (plusp dimension-thld)) dimension-thld)
                ((and (typep dimension-thld 'float)
                      (< 0 dimension-thld 1))
                 (count-if
                  (let ((s (reduce #'+ (contributions pca-result))) (c 0))
                    (lambda (v)
                      (cond ((>= dimension-thld c) (incf c (/ v s)) t) (t nil))))
                  (contributions pca-result)))))
         (bases (subseq (loading-factors pca-model) 0 dim))
         (score (coerce (loop repeat (length points) collect (make-dvec dim)) 'vector))
         (ids (let ((pos (dimension-index
                          (find id-column (dataset-dimensions face-dataset)
                                :test #'string-equal
                                :key #'dimension-name))))
                (map 'vector
                  (lambda (vec) (aref vec pos)) (dataset-category-points face-dataset)))))
    (declare (type (simple-array dvec (*)) c-points score))

    ;; projection
    (base-projection-pts bases c-points score)

    ;; make random-forest and estimator
    (let* ((train-dataset
            (make-unspecialized-dataset
             (cons id-column (loop for i from 1 to dim collect (format nil "princ~A" i)))
             (map 'vector (lambda (id pt) (concatenate 'simple-vector `(,id) pt)) ids score)
             :missing-value-check nil))
           (forest (clml.decision-tree.random-forest:make-random-forest
                    train-dataset id-column :test tree-test
                    :tree-number tree-number))
           (face-estimator
            (lambda (face-vec)
              (assert (eql (length face-vec) (length (aref bases 0))))
              (let ((s-dvec (make-dvec (length bases)))
                    (face-v (copy-seq face-vec)))
                (declare (type dvec s-dvec face-v))
                ;; demean
                (v- face-v e-mean face-v)
                ;; projection
                (base-projection bases face-v s-dvec)
                ;; forest prediction
                (clml.decision-tree.random-forest:predict-forest (concatenate 'simple-vector
                                                `(,clml.hjs.missing-value:+na+) s-dvec)
                                              train-dataset forest)))))
      (values face-estimator forest (length bases) train-dataset))))
(defgeneric make-face-estimator (face-dataset &key)
  (:documentation "*** make-face-estimator ((face-dataset numeric-and-category-dataset)
                         &key id-column dimension-thld method
                              pca-method d-fcn pca-result pca-model)
- return: (values estimator hash)
- arguments:
  - face-dataset : <numeric-and-category-dataset>
  - id-column : <string>, the name for the face ID column, default value is personID
  - dimension-thld : 0 < <number> < 1 | 1 <= <integer>, the threshold for determining the number of dimensions to use.
  - method : :eigenface | :subspace, method for face recognition, eigenface or subspace method.
  - pca-method : :covariance | :correlation, only valid when method is :subspace
  - d-fcn : distance function for eigenface, default value is euclid-distance
  - pca-result : <pca-result>, necessary for :eigenface
  - pca-model : <pca-model>, necessary for :eigenface
- note:
  - When 0 < /dimension-thld/ < 1, it means the threshold for accumulated
    contribution ratio. A principle component's contribution ratio means
    its proportion in all principle components' contributions.
  - When 1 <= /dimension-thld/ ( integer ), it means the number of principle components.
- reference:
  - [[ http://www.ism.ac.jp/editsec/toukei/pdf/49-1-023.pdf ][ An example principal component analysis face recognition pattern recognition in sharpness (Banno)]]
  - [[http://www.face-rec.org/][Face Recognition Homepage]]
"))
(defmethod make-face-estimator ((face-dataset numeric-and-category-dataset)
                                &key dimension-thld
                                     (id-column "personID")
                                     (method :eigenface) ;; :eigenface | :subspace
                                     (pca-method :covariance)
                                     (d-fcn #'euclid-distance)
                                     pca-result pca-model
                                  bagging)

  (assert (or (null bagging) (numberp bagging)))
  (flet ((make-estimator (d)
           (case method
             (:eigenface
              (assert (and pca-result pca-model))
              (make-face-estimator-eigenface d pca-result pca-model
                                             :dimension-thld dimension-thld
                                             :id-column id-column :d-fcn d-fcn))
             (:subspace
              (make-face-estimator-subspace d :id-column id-column
                                            :dimension-thld dimension-thld
                                            :pca-method pca-method)))))
    (multiple-value-bind (estimator info base-num)
        (if bagging
            (let ((ds (make-bootstrap-sample-datasets face-dataset :number-of-datasets bagging)))
              (apply #'values
                     (loop for d in ds
                         as (est inf b-n) = (multiple-value-list (make-estimator d))
                         collect est into ests collect inf into infs collect b-n into b-ns
                         finally
                           (return
                             `(,(lambda (face-vec)
                                  (let ((alist
                                         (sort (alist-count
                                                (mapcar (lambda (est) (funcall est face-vec)) ests)
                                                :test #'string=) #'> :key #'cdr)))
                                    (values (caar alist) alist)))
                                  ,infs ,b-ns)))))
          (make-estimator face-dataset))
      (format t "~&Dimension : ~A~%" base-num)
      (format t "~&Number of self-misjudgement : ~A~%"
              (loop for id across
                    (map 'vector
                      (let ((pos (dimension-index
                                  (find id-column
                                        (dataset-dimensions face-dataset)
                                        :key #'dimension-name :test #'string-equal))))
                        (lambda (v) (aref v pos))) (dataset-category-points face-dataset))
                  for face-vec across (dataset-numeric-points face-dataset)
                  count (not (string-equal (funcall estimator face-vec) id))))
      (values estimator info))))

(defgeneric face-estimate (d estimator))
(defmethod face-estimate ((d numeric-and-category-dataset) estimator)
  "*** face-estimate ((d numeric-dataset) estimator)
- return: <numeric-and-category-dataset>
- arguments:
  - d : <numeric-dataset>
  - estimator : <closure>, the first return value for make-face-estimator
*** Note
- when using princomp and sub-princomp, if there exists two columns
  that are of same value, the result for :correlation
  method will not be converged. Therefore pick-and-specialize-data or
  divide-dataset must be used to remove one column.
*** sample usage
#+INCLUDE: sample/face-estimate.org
"
             (pick-and-specialize-data
   (make-unspecialized-dataset
    (cons "estimated-face" (map 'list #'dimension-name (dataset-dimensions d)))
    (loop for num-vec across (dataset-numeric-points d)
        for vec across (dataset-points d)
        for ested = (funcall estimator num-vec)
        collect (concatenate 'vector `(,ested) vec)
        into result
        finally (return (coerce result 'vector))))
   :data-types (cons :category
                     (map 'list #'dimension-type (dataset-dimensions d)))))

(defun face-hitting-ratio (ested-d org-d &key (id "personID"))
  (let* ((ei (dimension-index (find "estimated-face" (dataset-dimensions ested-d)
                                    :test #'string-equal :key #'dimension-name)))
         (oi (dimension-index (find id (dataset-dimensions org-d)
                                    :test #'string-equal :key #'dimension-name)))
         (el (loop for v across (dataset-category-points ested-d)
                 collect (aref v ei)))
         (ol (loop for v across (typecase org-d
                                  (unspecialized-dataset (dataset-points org-d))
                                  (t (dataset-category-points org-d)))
                 collect (aref v oi))))
    (let ((ok 0) (not-ok 0)
          (total (length el)))
      (loop for e in el for o in ol
          do (if (string-equal e o)
                 (incf ok) (incf not-ok)))
      (prog ((hr (/ ok total)))
        (format t "~&true: ~A, false: ~A, hitting-ratio: ~,2F~%" ok not-ok hr)
        (return hr)))))

(defun get-face-center (eye-pgm &optional (external-format :default))
  (let (lx ly rx ry)
    (with-open-file (in eye-pgm :external-format external-format)
      (read-line in)
      (setq lx (read in nil nil) ly (read in nil nil)
            rx (read in nil nil) ry (read in nil nil)))
    (cons (floor (+ (* 0.5 (- lx rx)) rx
                    10)) ;; width for "eye"
          (floor (+ (* 0.5 (abs (- ly ry))) (min ly ry))))))


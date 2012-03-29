
(in-package :hjs.learn.pca)

(defun alist-count (list &key (test #'eq))
  (loop with alist for item in list
      as sub-alist = (assoc item alist :test test)
      do (if sub-alist (incf (cdr sub-alist)) 
           (push (cons item 1) alist))
      finally (return alist)))

;;; Normalize face
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
    (declare (ignore _))
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

(defmethod make-face-estimator-random-forest-with-pca
    ((face-dataset numeric-and-category-dataset)
     pca-result pca-model &key dimension-thld (id-column "personID")
                               (tree-test #'decision-tree::delta-gini)
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
           (forest (random-forest:make-random-forest
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
                (random-forest:predict-forest (concatenate 'simple-vector 
                                                `(,handling-missing-value:*na*) s-dvec)
                                              train-dataset forest)))))
      (values face-estimator forest (length bases) train-dataset))))

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

(defmethod face-estimate ((d numeric-and-category-dataset) estimator)
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

#||
(defun get-face-center (eye-pgm &optional (external-format :default))
  (let (lx ly rx ry)
    (with-open-file (in eye-pgm :external-format external-format)
      (read-line in)
      (setq lx (read in nil nil) ly (read in nil nil)
            rx (read in nil nil) ry (read in nil nil)))
    (cons (floor (+ (* 0.5 (- lx rx)) rx 
                    10)) ;; width for "eye"
          (floor (+ (* 0.5 (abs (- ly ry))) (min ly ry))))))

(defun foo (in-pgm out-pgm eye-pgm &key width height (h-upper-ratio 0.35) (w-left-ratio 0.5))
  (let* ((width-left (floor (* width w-left-ratio)))
         (width-right (- width width-left))
         (height-upper (floor (* height h-upper-ratio)))
         (height-lower (- height height-upper))
         (center (get-face-center eye-pgm))
         (x-range (cons (- (car center) width-left)
                        (1- (+ (car center) width-right))))
         (y-range (cons (- (cdr center) height-upper)
                        (1- (+ (cdr center) height-lower)))))
    (when (>= 0 (car x-range))
      (setq x-range (cons 1 (- (cdr x-range) (1- (car x-range))))))
    (when (>= (cdr x-range) 384)
      (setq x-range (cons (- (car x-range) (- (cdr x-range) 383)) 383)))
    (when (>= 0 (car y-range))
      (setq y-range (cons 1 (- (cdr y-range) (1- (car y-range))))))
    (when (>= (cdr y-range) 286)
      (setq y-range (cons (- (car y-range) (- (cdr y-range) 285)) 285)))
    
    (with-open-file (in in-pgm :element-type '(unsigned-byte 8))
      (with-open-file (out out-pgm :direction :output 
                       :if-exists :supersede :if-does-not-exist :create)
        (format out "P2~%~D ~D~%255~%" 
                (if x-range (1+ (abs (- (car x-range) (cdr x-range)))) 384)
                (if y-range (1+ (abs (- (car y-range) (cdr y-range)))) 286))
        (loop repeat 4 do (read-byte in))
        (loop for i from 0
            for num = (read-byte in nil nil)
            as (x y) = (multiple-value-bind (yy xx) (floor i 384)
                         `(,(1+ xx) ,(1+ yy)))
            while num 
            do (when (and (or (null x-range)
                              (<= (car x-range) x (cdr x-range)))
                          (or (null y-range)
                              (<= (car y-range) y (cdr y-range))))
                 (format out "~D~%" num)))))))

(loop with c = 0
    with files = (directory (excl:pathname-as-directory "sample/faces-org"))
    for file = (second files)           ; pgm file
    while (< c 200)
    do (when (string-equal "pgm" (pathname-type file))
         (incf c)
         (let ((eye-pgm (make-pathname
                         :directory (pathname-directory file)
                         :name (pathname-name file)
                         :type "eye"
                         :device (pathname-device file))))
           (setq files (remove file files :test #'excl::pathname-equalp)
                 files (remove eye-pgm files :test #'excl::pathname-equalp))
           #+ignore(foo file (format nil "sample/faces/~A.pgm" c) eye-pgm
                        :width 140 :height 140)
           (foo file (format nil "sample/faces/deye-~A.pgm" c) eye-pgm
                :width 115 :height 26 :h-upper-ratio 0.5 :w-left-ratio 0.5))))

(defun bar (pgm-file)
  (with-open-file (in pgm-file)
    (read-line in)
    (let* ((w-h (excl:delimited-string-to-list (read-line in nil nil) " "))
           (width (read-from-string (first w-h)))
           (height (read-from-string (second w-h)))
           (v (make-dvec (* width height))))
      (read-line in)
      (loop for i below (* width height)
          for num = (read-from-string (read-line in nil nil))
          while num
          do (setf (aref v i) (dfloat num))
          finally (return v)))))

(defun make-face-dataset (&key fname)
  (let* ((n 200)
         (dat (make-array n))
         cols)
    (loop for c below n
        as file = (if fname (format nil "sample/faces/~A-~A.pgm" fname (1+ c))
                    (format nil "sample/faces/~A.pgm" (1+ c)))
        do (setf (aref dat c) (bar file))
           (push (pathname-name file) cols))
    (setq cols (reverse cols))
    (with-open-file (out (if fname (format nil "sample/~As~A.sexp" fname n)
                           (format nil "sample/faces~A.sexp" n))
                     :direction :output :if-exists :supersede)
      (write
       (append `(("id" "personID" ,@(loop for i from 1 to (length (aref dat 0))
                                        collect (format nil "p~A" i))))
               (loop for vec across dat
                   for col in cols
                   for pid in '("kevin" "kevin" "kevin" "kevin" "kevin" "kevin" "kevin" "kevin"
                                "kevin" "kevin" "arthur" "kevin" "kevin" "kevin" "kevin" "kevin"
                                "igor" "igor" "kevin" "max" "max" "max" "max" "max" "max" "max" "max"
                                "max" "max" "max" "max" "max" "max" "max" "max" "max" "max" "max"
                                "max" "max" "max" "max" "max" "max" "max" "max" "kevin" "max" "kevin"
                                "kevin" "maria" "maria" "maria" "maria" "maria" "maria" "maria"
                                "maria" "maria" "maria" "maria" "maria" "maria" "maria" "maria"
                                "maria" "maria" "maria" "maria" "maria" "maria" "maria" "maria"
                                "maria" "maria" "maria" "mary" "mary" "mary" "mary" "mary" "mary"
                                "mary" "mary" "mary" "mary" "mary" "mary" "mary" "mary" "scott"
                                "scott" "scott" "scott" "scott" "scott" "scott" "scott" "arthur"
                                "arthur" "arthur" "arthur" "arthur" "arthur" "arthur" "arthur"
                                "arthur" "arthur" "arthur" "arthur" "arthur" "arthur" "arthur"
                                "arthur" "arthur" "david" "david" "david" "david" "david" "david"
                                "david" "david" "david" "david" "david" "david" "david" "david"
                                "david" "david" "david" "david" "david" "david" "david" "david"
                                "david" "david" "david" "david" "david" "david" "david" "david"
                                "david" "kevin" "scott" "scott" "scott" "scott" "scott" "scott"
                                "scott" "scott" "scott" "scott" "scott" "scott" "scott" "scott"
                                "scott" "scott" "scott" "scott" "scott" "scott" "scott" "scott"
                                "scott" "kevin" "kevin" "kevin" "kevin" "kevin" "kevin" "kevin"
                                "kevin" "kevin" "kevin" "sam" "sam" "sam" "sam" "sam" "sam" "sam"
                                "sam" "sam" "sam" "sam" "sam" "sam" "sam" "sam" "sam" "sam" "sam"
                                "sam" "sam")
                   collect `(,col ,pid ,@(coerce vec 'list))))
       :stream out))))

;; test eigen-face
(let ((eyes
       (normalize-faces
        (pick-and-specialize-data
         (read-data-from-file "sample/deyes200.sexp")
         :data-types (append (make-list 2 :initial-element :category)
                             (make-list 2990 :initial-element :numeric))))))
  (loop for dim-thld in '(5 10 15 20 25 30 35)
      collect
        (cons dim-thld 
              (loop repeat 10 
                  collect
                    (multiple-value-bind (learn-eyes estimate-eyes)
                        (progn (print "divide data")
                               (divide-dataset eyes :divide-ratio '(1 1) :random t))
                      (multiple-value-bind (pca-result pca-model)
                          (progn (print "princomp")
                                 (time (sub-princomp (divide-dataset learn-eyes :except '(0 1)) 
                                                     :method :covariance
                                                     :dimension-thld dim-thld)))
                        (let* ((estimator 
                                (progn (print "make estimator")
                                       (make-face-estimator 
                                        learn-eyes :pca-result pca-result :pca-model pca-model
                                        :method :eigenface :bagging 20)))
                               (ested 
                                (progn (print "estimation")
                                       (face-estimate (divide-dataset
                                                       estimate-eyes :except '(0 1))
                                                      estimator))))
                          (face-hitting-ratio ested estimate-eyes)))))) into hitting-ratios-list
      finally (loop for (dim-thld . hitting-ratios) in hitting-ratios-list
                  do (format t "~&ŽŸŒ³”: ~A, ³‰ð—¦(min max mean): ~,2F ~,2F ~,2F~%"
                             dim-thld (apply #'min hitting-ratios) (apply #'max hitting-ratios)
                             (/ (apply #'+ hitting-ratios) (length hitting-ratios))))))

;;test subspace
(let ((eyes (pick-and-specialize-data
             (read-data-from-file "sample/eyes200.sexp" :external-format :shiftjis)
             :data-types (append (make-list 2 :initial-element :category)
                                 (make-list 1680 :initial-element :numeric)))))
  (loop for dim-thld in '(5 10 15 20) 
      collect
        (cons dim-thld 
              (loop repeat 10 
                  collect
                    (multiple-value-bind (learn-eyes estimate-eyes)
                        (progn (print "divide data")
                               (divide-dataset eyes :divide-ratio '(1 1) :random t))
                      (let* ((estimator 
                              (progn (print "make estimator")
                                     (make-face-estimator learn-eyes :dimension-thld dim-thld
                                                          :pca-method :covariance
                                                          ; :correlation
                                                          :method :subspace)))
                             (ested 
                              (progn (print "estimation")
                                     (face-estimate (divide-dataset
                                                     estimate-eyes :except '(0 1))
                                                    estimator))))
                        (face-hitting-ratio ested estimate-eyes))))) into hitting-ratios-list
      finally (loop for (dim-thld . hitting-ratios) in hitting-ratios-list
                  do (format t "~&ŽŸŒ³”: ~A, ³‰ð—¦(min max mean): ~,3F ~,3F ~,3F~%"
                             dim-thld (apply #'min hitting-ratios) (apply #'max hitting-ratios)
                             (/ (apply #'+ hitting-ratios) (length hitting-ratios))))))
||#

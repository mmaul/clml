(defpackage :hjs.learn.pca
  (:use :cl :hjs.util.meta :hjs.util.matrix :hjs.util.vector
        :hjs.learn.read-data :statistics :hjs.learn.vars)
  (:nicknames :pca)
  (:export 
   #:princomp
   #:princomp-projection
   #:sub-princomp
   #:kernel-princomp
   #:make-face-estimator
   #:face-estimate
   #:components
   #:contributions
   #:loading-factors
   ))

(in-package :hjs.learn.pca)

(declaim (optimize (speed 3) (safety 1) (debug 1)))

#||
Here we calculate the PCA by solving the eigenvectors/eigenvalues of
the covariance matrix of the input dataset. The dataset is of size (M,
N), where M is the number of ponits and N is the dimension size. 
||#

(defclass pca-result ()
  ((components :initarg :components :accessor components)
   (contributions :initarg :contributions :accessor contributions)
   (loading-factors :initarg :loading-factors :accessor loading-factors)
   (pca-method :initarg :pca-method :accessor pca-method)
   (centroid :initarg :centroid :accessor centroid)
   (orig-data-standard-deviations :initarg :orig-data-standard-deviations :accessor orig-data-standard-deviations)))

(defun make-pca-result (components contributions loading-factors pca-method centroid orig-data-standard-deviations)
  (make-instance 'pca-result
		 :components components
		 :contributions contributions
		 :loading-factors loading-factors
		 :pca-method pca-method
		 :centroid centroid
		 :orig-data-standard-deviations orig-data-standard-deviations))

(defclass pca-model ()
  ((loading-factors :initarg :loading-factors :accessor loading-factors)
   (pca-method :initarg :pca-method :accessor pca-method)
   (centroid :initarg :centroid :accessor centroid)
   (orig-data-standard-deviations :initarg :orig-data-standard-deviations :accessor orig-data-standard-deviations)))

(defun make-pca-model (loading-factors pca-method centroid orig-data-standard-deviations)
  (make-instance 'pca-model
		 :loading-factors loading-factors
		 :pca-method pca-method
		 :centroid centroid
		 :orig-data-standard-deviations orig-data-standard-deviations))

(defmethod make-cov-or-cor ((dataset numeric-dataset) 
                            &key (method :covariance)
                            (type :matrix)) ; :matrix | :closure
  (let* ((points (map 'vector
                      #'copy-seq
                      (dataset-numeric-points dataset)))
         (dim (length (aref (the simple-vector points) 0)))
         (size (length points))
         ;; get empirial mean
         (e-mean (mean-points points))
         ;; centering the points
         ;; NOTE: points cannot be used anymore
         (c-points 
          (do-vec (p points :type dvec :return points)
            (v- p e-mean p)))
         ;; transpose
         (tc-points (transposeV c-points))
         ;; constant 1/n
         (1/n (/ 1.0 size))
         ;; covariance = 1/n * data * data'
         (covariance
          (ecase type
            (:matrix
               (let ((result (make-array (list dim dim) :element-type 'double-float)))
                 (declare (type dmat result))
                 (#+future par-do-vec #-future do-vec (p1 tc-points :type dvec :index-var ix)
                           (do-vec (p2 tc-points :type dvec :index-var iy)
                             (setf (aref result ix iy)
                                   (* (inner-product p1 p2) 1/n))))
                 result))
            (:closure
               (lambda (row col)
                 (declare (type integer row col))
                 (* (inner-product (svref tc-points row) 
                                   (svref tc-points col)) 1/n)))))
         ;; computing the standard deviations
         (standard-deviations
          (if (eq method :correlation)
              (ecase type
                (:matrix
                   (let ((result (make-dvec dim)))
                     (declare (type dvec result))
                     (do-vec (_ result :type double-float :setf-var sr :index-var ir)
                       (declare (ignore _))
                       (let ((val (aref covariance ir ir)))
                         (declare (type double-float val))
                         (when (zerop val)
                           (error "Dimension ~A is constant, please except the dimension."
                                  (dimension-name (svref (dataset-dimensions dataset) ir))))
                         (setf sr (sqrt (the (double-float 0.0) val)))))
                     result))
                (:closure
                   (let ((dims (dataset-dimensions dataset)))
                     (declare (type vector dims))
                     (lambda (ir) (declare (type integer ir))
                       (let ((val (funcall covariance ir ir)))
                         (declare (type double-float val))
                         (when (zerop val)
                           (error "Dimension ~A is constant, please except the dimension."
                                  (dimension-name (svref dims ir))))
                         (sqrt (the (double-float 0.0) val)))))))
              (ecase type
                (:matrix (fill-vec (make-dvec dim) handling-missing-value:*nan*))
                (:closure (lambda (ir) (declare (ignore ir)) handling-missing-value:*nan*)))))
         ;; computing the correlation matrix
         (correlation
          (when (eq method :correlation)
            (ecase type
              (:matrix
                 (let ((result (make-array (list dim dim) :element-type 'double-float)))
                   (declare (type (simple-array dvec (*)) tc-points)
                            (type dvec standard-deviations)
                            (type dmat result covariance))
                   (loop for i of-type array-index below dim
                         do (loop for j of-type array-index below dim
                                  do (setf (aref result i j)
                                           (/ (aref covariance i j)
                                              (aref standard-deviations i)
                                              (aref standard-deviations j)))))
                   result))
              (:closure
                 (lambda (row col)
                   (declare (type integer row col))
                   (/ (funcall covariance row col)
                      (funcall standard-deviations row)
                      (funcall standard-deviations col)))))))
         (cov-or-cor
          (case method
            (:covariance covariance)
            (:correlation correlation)))
         (z-scores c-points))
    (declare (type (simple-array dvec (*)) z-scores))
    ;; compute the z-scores
    ;; NOTE: not part of PCA but useful in many applications, mathematica does this.
    (when (eq method :correlation)
      (do-vec (p z-scores :type dvec)
        (ecase type
          (:matrix
             (do-vecs ((v p :type double-float :setf-var sv)
                       (s standard-deviations :type double-float))
               (setf sv (/ v s))))
          (:closure
             (do-vec (v p :type double-float :index-var i :setf-var sv)
               (setf sv (/ v (the (double-float 0.0) 
                               (funcall standard-deviations i)))))))))
    (values cov-or-cor e-mean z-scores standard-deviations)))

;;@ function-type: 2d-double-array -> fn -> (values pcomps eigenvalues rotated-matrix)
;;@ precondition:
;;@  - dataset must be a numeric-dataset
;;@  - model can either be :covariance or :correlation
;;@  - components is either :all or an integer
(defmethod princomp ((dataset numeric-dataset) &key (method :correlation))
  (assert (eq (type-of dataset) 'numeric-dataset))
  (assert (find method '(:covariance :correlation)))
  (multiple-value-bind (cov-or-cor e-mean z-scores standard-deviations)
      (make-cov-or-cor dataset :method method)
    (declare (type dmat cov-or-cor)
             (type dvec e-mean standard-deviations)
             (type (simple-array dvec (*)) z-scores))
    ;; find the eigenvectors and eigenvalues
    (multiple-value-bind (eigen-values eigen-vectors)
        #-mkl (eigen-by-householder-ql cov-or-cor)
      #+mkl (symat-ev cov-or-cor)
      (declare (type dvec eigen-values)
               (type dmat eigen-vectors))
      ;; NOTE: this is important
      (do-vec (v eigen-values :type double-float :setf-var sv)
        (when (and (< v 0.0)
                   (< (- v) *epsilon*))
          (setf sv 0.0)))
      (when (some (lambda (e) (declare (type double-float e)) (< e 0.0)) eigen-values)
        (error "Covariance matrix is not non-negative definite"))
      (let* ((dim (length eigen-values))
             (eigen-vectors
              (coerce
               (loop for i of-type array-index below dim
                     for vec of-type dvec = (make-dvec dim)
                     do (do-vec (_ vec :type double-float :index-var iv :setf-var sv)
                          (declare (ignore _))
                          (setf sv 
                                #-mkl (aref eigen-vectors iv i)
                                #+mkl (aref eigen-vectors i iv)))
                     collect vec)
               'vector)))
        ;; sort by eigenvalues
        (declare (type (simple-array dvec (*)) eigen-vectors))
        (let ((indices (make-array dim :element-type 'fixnum)))
          (declare (type (simple-array fixnum (*)) indices))
          (do-vec (_ indices :type fixnum :setf-var si :index-var i)
            (declare (ignore _))
            (setf si i))
          (sort indices #'> :key (lambda (i) (declare (type fixnum i)) (aref eigen-values i)))
          ;; reorder eigenvectors and eigenvalues
          (let* ((eigen-values (reorder-dvec eigen-values indices))
                 (eigen-vectors (reorder-vec eigen-vectors indices)))
            ;; (sdev (specialize-vec (map 'vector #'sqrt eigen-values)))
            (declare (type (simple-array (double-float 0.0) (*)) eigen-values)
                     (type (simple-array dvec (*)) eigen-vectors z-scores))
            ;; basis transformation
            ;; finding score
            (let* ((score (map 'vector (lambda (_) (declare (ignore _)) (make-dvec dim)) z-scores)))
              (declare (type (simple-array dvec (*)) score))
              (#+future par-do-vec #-future do-vec (p z-scores :type dvec :index-var ip)
                (let ((r (aref score ip)))
                  (declare (type dvec r))
                  (do-vec (v eigen-vectors :type dvec :index-var iv)
                    (setf (aref r iv)
                          (inner-product p v)))))
              (values (make-pca-result score eigen-values eigen-vectors 
                                       method e-mean standard-deviations)
                      (make-pca-model eigen-vectors method e-mean standard-deviations)))))))))

(defmethod princomp-projection ((dataset numeric-dataset) (pca-model pca-model))
  (assert (eq (type-of dataset) 'numeric-dataset))
  (let* ((points (map 'vector
                   #'copy-seq
                   (dataset-numeric-points dataset)))          
         ;; get empirial mean
         (e-mean (centroid pca-model))
         ;; centering the points
         ;; NOTE: points cannot be used anymore
         (c-points 
          (do-vec (p points :type dvec :return points)
            (v- p e-mean p)))
         ;; computing the standard deviations
         (standard-deviations (orig-data-standard-deviations pca-model))
         ;; bases
         (bases (loading-factors pca-model))
         (dim (length (the vector bases)))
         ;; z-scores
         (z-scores c-points)            ; NOTE: c-points cannot be used anymore
         ;; score
         (score (map 'vector (lambda (_) 
                               (declare (ignore _))
                               (make-dvec dim)) z-scores))
         )
    (declare (type (simple-array dvec (*)) bases z-scores score)
             (type dvec standard-deviations))
    ;; calculate z-scores
    (when (eq (pca-method pca-model) :correlation)
      (do-vec (p z-scores :type dvec)
        (do-vecs ((v p :type double-float :setf-var sv)
                  (s standard-deviations :type double-float))
          (setf sv (/ v s)))))
    ;; 
    (do-vecs ((p z-scores :type dvec :index-var ip)
              (r score :type dvec))
      (do-vec (v bases :type dvec :index-var iv)
        (setf (aref r iv)
          (inner-product p v))))
    score))

(defmethod sub-princomp ((dataset numeric-dataset) &key (method :correlation)
                                                        (dimension-thld 0.8d0))
  (assert (find method '(:covariance :correlation)))
  (multiple-value-bind (cov-or-cor e-mean z-scores standard-deviations)
      (make-cov-or-cor dataset :method method)
    (declare (type dmat cov-or-cor)
             (type dvec e-mean standard-deviations)
             (type (simple-array dvec (*)) z-scores))
    ;; find the eigenvectors and eigenvalues
    (let ((eigen-method #-mkl :power
                        #+mkl (if (integerp dimension-thld) :mkl :power)))
      (multiple-value-bind (eigen-values eigen-vectors)
          (ecase eigen-method
            (:power (eigen-by-power cov-or-cor :eigen-thld dimension-thld))
            (:mkl #+mkl (symat-ev cov-or-cor :eigen-thld dimension-thld)))
        (declare (type dvec eigen-values))
        ;; NOTE: this is important
        (do-vec (v eigen-values :type double-float :setf-var sv)
          (when (and (< v 0.0)
                     (< (- v) *epsilon*))
            (setf sv 0.0)))
        (when (some (lambda (e) (< e 0.0)) eigen-values)
          (error "Covariance matrix is not non-negative definite"))
        ;; basis transformation
        ;; finding score
        (let* ((dim (length eigen-values))
               (eigen-vectors 
                (ecase eigen-method
                  (:power eigen-vectors)
                  (:mkl
                   (coerce
                    (loop for i of-type array-index below dim
                        for vec of-type dvec = (make-dvec (array-dimension eigen-vectors 1))
                        do (do-vec (_ vec :type double-float :index-var iv :setf-var sv)
                             (declare (ignore _))
                             (setf sv (aref eigen-vectors i iv)))
                        collect vec)
                    'vector)))))
          (let* ((score (map 'vector (lambda (_) (declare (ignore _)) (make-dvec dim)) z-scores)))
            (declare (type (simple-array dvec (*)) score))
            (do-vecs ((p z-scores :type dvec :index-var ip)
                      (r score :type dvec))
              (do-vec (v eigen-vectors :type dvec :index-var iv)
                (setf (aref r iv) (inner-product p v))))
            (values 
             (make-pca-result score eigen-values eigen-vectors 
                              method e-mean standard-deviations)
             (make-pca-model eigen-vectors method e-mean standard-deviations))))))))


;;;;;;;;;;;;;;;;;
; kernel P.C.A. ;
;;;;;;;;;;;;;;;;;
;; reference: 
;; - パターン認識と機械学習下: ベイズ理論による統計的予測 著者: C.M.ビショップ
;; - B.Schlkoph and A.J.Smola, Learning With Kernel:Section 5, MIT Press, 2002. 

;;;;;;;;;;;;;;;;;;;;
; kernel functions ;
;;;;;;;;;;;;;;;;;;;;
;; ref: kernlab-An S4 Package for Kernel method in R, http://www.jstatsoft.org/v11/i09
(defmacro make-kernel-fcn (&body body)
  `(lambda (v w) (declare (type dvec v w))
           (assert (eql (length v) (length w)))
           (dfloat ,@body)))
(defun polynomial (&key (scale 1d0) (offset 0d0) (degree 1))
  (declare (type fixnum degree))
  (check-type degree fixnum)
  (make-kernel-fcn
   (expt (+ (* (inner-product v w) scale) offset) degree)))
(defun gaussian (&key (sigma 0.1))
  (assert (> sigma 0d0))
  (make-kernel-fcn
   (let ((d (make-dvec (length v)))) (declare (type dvec d))
        (handler-case
            (progn
              (v- v w d)
              (exp (* (- sigma) (inner-product d d))))
          (FLOATING-POINT-UNDERFLOW (c)
            (declare (ignore c)) 0.0d0)))))
(defun laplace (&key (sigma 0.1))
  (make-kernel-fcn
   (let ((d (make-dvec (length v)))) (declare (type dvec d))
        (handler-case
            (progn
              (v- v w d)
              (exp (* (- sigma) (distance-to-origin d))))
          (FLOATING-POINT-UNDERFLOW (c)
            (declare (ignore c)) 0.0d0)))))
(defun anova (&key (sigma 0.1) (degree 1))
  (declare (type fixnum degree))
  (check-type degree fixnum)
  (make-kernel-fcn
   (let ((s 0d0)) (declare (type double-float s))
        (do-vecs ((x v :type double-float)
                  (y w :type double-float))
          (incf s (exp (* (- sigma) (expt (- x y) 2)))))
        (expt s degree))))
(defun sigmoid (&key (scale 1) (offset 0))
  (make-kernel-fcn
   (handler-case
       (tanh (+ (* scale (inner-product v w)) offset))
     (FLOATING-POINT-UNDERFLOW (c) (declare (ignore c)) 0.0d0))))
(defparameter +linear+ (polynomial :scale 1d0 :offset 0d0 :degree 1))

(defclass kernel-pca-result ()
  ((components :initarg :components :accessor components)
   (contributions :initarg :contributions :accessor contributions)
   (loading-factors :initarg :loading-factors :accessor loading-factors)
   (kernel-fcn :initarg :kernel-fcn :accessor kernel-fcn)
   (centroid :initarg :centroid :accessor centroid)
   (n-points :initarg :n-points :accessor n-points)))
(defclass kernel-pca-model ()
  ((loading-factors :initarg :loading-factors :accessor loading-factors)
   (kernel-fcn :initarg :kernel-fcn :accessor kernel-fcn)
   (centroid :initarg :centroid :accessor centroid)
   (demean-org-pts :initarg :demean-org-pts :accessor demean-org-pts)))

(defun make-kernel-pca-result 
    (components contributions loading-factors kernel-fcn centroid n-points)
  (make-instance 'kernel-pca-result
    :components components :contributions contributions
    :loading-factors loading-factors :kernel-fcn kernel-fcn
    :centroid centroid :n-points n-points))

(defun make-kernel-pca-model (eigen-vecs kernel-fcn e-mean demean-pts)
  (make-instance 'kernel-pca-model
    :loading-factors eigen-vecs :kernel-fcn kernel-fcn
    :centroid e-mean :demean-org-pts demean-pts))

(defmethod make-kernel-mat ((d numeric-dataset) kernel-fcn)
  (let* ((points (map 'vector
                   #'copy-seq
                   (dataset-numeric-points d)))
         (size (length points))
         (-1/n (/ -1d0 size))
         (e-mean (mean-points points))
         (c-points 
          (do-vec (p points :type dvec :return points)
            (v- p e-mean p)))
         (mem-kernel 
          (let ((mem (make-hash-table :test #'eql)))
            (lambda (r c) (declare (type fixnum r c))
                    (let ((key (if (> r c) ;; kernel-fcn is commutative
                                   (+ (* c size) r)
                                 (+ (* r size) c))))
                      (multiple-value-bind (value pr-p)
                          (gethash key mem)
                        (if pr-p value 
                          (setf (gethash key mem)
                            (funcall kernel-fcn
                                     (svref c-points r)
                                     (svref c-points c)))))))))
         (k-mat (make-array `(,size ,size) :element-type 'double-float)))
    (assert (> size 0))
    (loop for row of-type fixnum below size
        do (loop for col of-type fixnum below size
               do 
                 (setf (aref k-mat row col)
                   (+ (funcall mem-kernel row col)
                      (* -1/n (loop for p of-type fixnum below size
                                  sum (funcall mem-kernel p col)))
                      (* -1/n (loop for p of-type fixnum below size
                                  sum (funcall mem-kernel row p)))
                      (* (expt -1/n 2)
                         (loop for p1 of-type fixnum below size
                             sum (loop for p2 of-type fixnum below size
                                     sum (funcall mem-kernel p1 p2))))))))
    (values k-mat size c-points e-mean)))

(defun kernel-score (eigen-vecs kernel-fcn demean-pts target-pts)
  (let* ((dim (length eigen-vecs))
         (score (map 'vector (lambda (_) (declare (ignore _)) (make-dvec dim))
                     demean-pts)))
    (do-vecs ((r score :type dvec :index-var ri)
              (pts target-pts :type dvec))
      (do-vecs ((vec eigen-vecs :type dvec)
                (_ r :type double-float :setf-var sr))
        (declare (ignore _))
        (let ((s 0d0)) (declare (type double-float s))
             (do-vecs ((alpha vec :type double-float)
                       (pts-n demean-pts :type dvec))
               (incf s (* alpha (funcall kernel-fcn pts pts-n))))
             (setf sr s))))
    score))

(defmethod kernel-princomp ((dataset numeric-dataset) 
                            &key dimension-thld
                                 (kernel-fcn +linear+))
  (declare (optimize debug))
  (unless dimension-thld 
    (setf dimension-thld (length (dataset-numeric-points dataset))))
  (let ((eigen-method #-mkl :power
                      #+mkl (if (integerp dimension-thld) :mkl :power)))
    (multiple-value-bind (kernel-mat n demean-pts e-mean)
        (progn (princ (format nil "Calcualting kernel matrix ...~%"))
               (make-kernel-mat dataset kernel-fcn))
      (declare (type dmat kernel-mat) (type fixnum n)
               (type (simple-array dvec (*)) demean-pts))
      (princ (format nil "done~%"))
      (multiple-value-bind (eigen-vals eigen-vecs)
          (progn (princ (format nil "Calculating eigen values and vectors ...~%"))
                 (ecase eigen-method
                   (:power (eigen-by-power kernel-mat :eigen-thld dimension-thld))
                   (:mkl #+mkl (symat-ev kernel-mat :eigen-thld dimension-thld))))
        (declare (type dvec eigen-vals) (type vector eigen-vecs))
        (princ (format nil "done~%"))
        (do-vec (v eigen-vals :type double-float :setf-var sv)
          (when (and (< v 0.0) (< (- v) *epsilon*)) (setf sv 0.0)))

        ;; normalize eigen-vecs and find score
        (let* ((dim (length eigen-vals))
               (eigen-vecs 
                (ecase eigen-method
                  (:power eigen-vecs)
                  (:mkl (coerce
                         (loop for i of-type array-index below dim
                             for vec of-type dvec = (make-dvec (array-dimension eigen-vecs 1))
                             do (do-vec (_ vec :type double-float :index-var iv :setf-var sv)
                                  (declare (ignore _))
                                  (setf sv (aref eigen-vecs i iv)))
                             collect vec)
                         'vector))))
               (score))
          (declare (type (simple-array dvec (*)) score))
          ;; normalize
          (do-vecs ((vec eigen-vecs :type dvec)
                    (val eigen-vals :type double-float :setf-var sval))
            (let ((val/n (/ val n))) (declare (type double-float val/n))
                 (do-vec (v vec :type double-float :setf-var svec)
                   (setf svec (/ v (sqrt val)))) ;; normalize eigenvec
                 (setf sval val/n))) ;; normalize eigenval
          ;; score
          (setf score (kernel-score eigen-vecs kernel-fcn demean-pts demean-pts))

          (values (make-kernel-pca-result
                   score eigen-vals eigen-vecs kernel-fcn e-mean n)
                  (make-kernel-pca-model eigen-vecs kernel-fcn e-mean demean-pts))
          )))))
          
(defmethod princomp-projection ((dataset numeric-dataset) (kpca-model kernel-pca-model))
  (with-accessors ((e-mean centroid)
                   (egn-vecs loading-factors)
                   (kfcn kernel-fcn)
                   (org-pts demean-org-pts)) kpca-model
    (declare (type dvec e-mean) 
             (type (simple-array dvec (*)) egn-vecs org-pts))
    (let* ((points (map 'vector #'copy-seq (dataset-numeric-points dataset)))
           (c-points
            (do-vec (p points :type dvec :return points)
              (v- p e-mean p))))
      (declare (type (simple-array dvec (*)) c-points))
      (kernel-score egn-vecs kfcn org-pts c-points))))

#+ignore
(defun plot-pca-result (2-dim-pts fname &key (scale 100) (blank 5) (radius 0.01d0))
  (with-open-file (s fname :direction :output :if-exists :supersede)
    (let* ((minx (reduce #'min 2-dim-pts :key (lambda (p) (elt p 0))))
           (miny (reduce #'min 2-dim-pts :key (lambda (p) (elt p 1))))
           (maxx (reduce #'max 2-dim-pts :key (lambda (p) (elt p 0))))
           (maxy (reduce #'max 2-dim-pts :key (lambda (p) (elt p 1))))
           (width (print (+ blank (* (ceiling (- maxx minx)) scale))))
           (height (print (+ blank (* (ceiling (- maxy miny)) scale)))))
      (with-open-file (s fname :direction :output :if-exists :supersede)
        (format s "P2~%~d ~d~%255~%" width height)
        (flet ((closep (p q)
                 (let ((d (make-dvec (length p))))
                   (v- p q d)
                   (< (distance-to-origin d) radius))))
          (loop for y below height
              do (loop for x below width
                     do
                       (let ((pos (specialize-vec 
                                   (make-array 2 :initial-contents
                                               (list (dfloat (+ minx (/ (- x (/ blank 2)) scale)))
                                                     (dfloat (+ miny (/ (- y (/ blank 2)) scale))))))))
                         (if (member pos (coerce 2-dim-pts 'list) :test #'closep)
                             (format s "0~%")
                           (format s "255~%"))))))))))

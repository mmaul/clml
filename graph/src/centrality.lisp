;-*- coding: utf-8 -*-


(in-package :clml.graph.graph-centrality)

(defgeneric eccentricity-centrality (g)
  (:documentation "- return: (SIMPLE-ARRAY DOUBLE-FLOAT (* )), vector of centraliry
- argument:
  - graph: return value of #'make-simple-graph"))
(defmethod eccentricity-centrality ((gr simple-graph))
  (let* ((dist-mat (graph-distance-matrix gr))
         (n (length (nodes gr)))
         (vec (make-dvec n)))
    (do-vec (_ vec :type double-float :setf-var sf :index-var row :return vec)
      #-sbcl (declare (ignore _))
      (let ((distances (loop for col below n collect (aref dist-mat col row))))
        (setf sf (dfloat (/ (apply #'max distances))))))))

(defgeneric closeness-centrality (g &key)
    (:documentation "- return: (SIMPLE-ARRAY DOUBLE-FLOAT (* )), vector of centraliry
- argument:
  - graph: return value of #'make-simple-graph
  - standardize: t | nil, standardize centrality or not"))
(defmethod closeness-centrality ((gr simple-graph) &key (standardize nil))
  (let* ((dist-mat (graph-distance-matrix gr))
         (n (length (nodes gr)))
         (vec (make-dvec n)))
    (do-vec (_ vec :type double-float :setf-var sf :index-var row :return vec)
      #-sbcl (declare (ignore _))
      (let* ((distances (loop for col below n collect (aref dist-mat col row)))
             (denom (if standardize (mean distances) (apply #'+ distances))))
        (setf sf (dfloat (/ denom)))))))

(defgeneric degree-centrality (g &key)
  (:documentation "- return: (SIMPLE-ARRAY DOUBLE-FLOAT (* )), vector of centraliry
- argument:
  - graph: return value of #'make-simple-graph
  - mode: :in | :out
  - standardize: t | nil, standardize centrality or not"))
(defmethod degree-centrality ((gr simple-graph)
                              &key (mode :in) ;; :in | :out
                                   (standardize nil))
  (let* ((n (length (nodes gr)))
         (n-1 (1- n))
         (vec (make-dvec n 0d0)))
    (if (directed-p gr)
        (ecase mode
          (:in (loop for link in (links gr)
                   do (incf (aref vec (1- (link-node2 link))))))
          (:out (loop for link in (links gr)
                    do (incf (aref vec (1- (link-node1 link)))))))
      (loop for link in (links gr)
          do (incf (aref vec (1- (link-node1 link))))
             (incf (aref vec (1- (link-node2 link))))))
    (if standardize
        (do-vec (val vec :type double-float :setf-var sf :return vec)
          (setf sf (/ val n-1)))
      vec)))

(defgeneric eigen-centrality (g &key)
  (:documentation "- return: (SIMPLE-ARRAY DOUBLE-FLOAT (* )), vector of centraliry
          DOUBLE-FLOAT, eigenvalue
- argument:
  - graph: return value of #'make-simple-graph"))
(defmethod eigen-centrality ((gr simple-graph) &key (stabilizer nil))
  (assert (null (directed-p gr)))
  (let* ((adj-mat (adjacency-matrix gr))
         (size (array-dimension adj-mat 0))
         (vec (make-dvec size)))
    (when (and (typep stabilizer 'dmat)
               (equal `(,size ,size) (array-dimensions stabilizer)))
      (setf adj-mat (mcm adj-mat stabilizer :c #'+)))
    (multiple-value-bind (eigen-vals eigen-vecs)
        #+mkl (symat-ev adj-mat :eigen-thld 1 :from :max)
        #-mkl (eigen-by-power adj-mat :eigen-thld 1 :from :max)
        (do-vec (_ vec :type double-float :setf-var sf :index-var i)
          #-sbcl (declare (ignore _))
          (let ((val #+mkl (aref eigen-vecs 0 i)
                     #-mkl (aref (aref eigen-vecs 0) i)))
            (setf sf (if (> *epsilon* (abs val)) 0d0 val))))
        (values (cond ((every (lambda (val) (not (minusp val))) vec) vec)
                      ((every (lambda (val) (not (plusp val))) vec)
                       (vcv vec (make-dvec size -1d0) :c #'*))
                      (t (error "Unexpected eigen vector: ~A" vec)))
                (aref eigen-vals 0)))))

;; pagerank
;; 分離/有向グラフでは、しばしば強連結ではないので principal eigen vector が定まらない。
;; pagerank ではそれを強連結とするため、推移確率行列の全てのノード間で小さな重みのリンクを仮定する。
;; c はその重みを調整するパラメータ、大きいほど重みは小さくなり、実際のリンクの推移確率が優先される。
;; (0 < c < 1)
(defgeneric pagerank (g &key)
  (:documentation "- return: (SIMPLE-ARRAY DOUBLE-FLOAT (* )), vector of centraliry
- argument:
  - graph: return value of #'make-simple-graph
  - c : ratio for transition probability matrix

The separation / directed graph, principal eigen vector is not determined because it is not often a strong connection.
Order to strongly connected to it, it is assumed that the link of small weight among all the nodes of the transition probability matrix in pagerank.
The parameter for adjusting the weight, the weight decreases as large, the transition probability of the actual link is priority c.

- reference:
  L. Page, S. Brin, R. Motwani, T. Winograd The PageRank citation ranking: Bringing order to the web. 1999")
)
(defmethod pagerank ((gr simple-graph) &key (c 0.85d0))
  (declare (type double-float c))
  (assert (< 0 c 1))
  (check-type c double-float)
  (let* ((n (length (nodes gr)))
         (tp-mat (transition-probability-matrix (adjacency-matrix gr)))
         (strongly-connected-mat
          (loop for col below n do
                (loop for row below n
                    as val = (aref tp-mat col row) do
                      (setf (aref tp-mat col row)
                        (+fl (*fl c val) (/fl (-fl 1d0 c) n))))
              finally (return tp-mat))))
    (declare (type dmat tp-mat))
    (multiple-value-bind (eigen-vals eigen-vecs)
        (eigen-by-power strongly-connected-mat :eigen-thld 1 :from :max)
      (declare (ignore eigen-vals))
      (aref eigen-vecs 0))))
(defun transition-probability-matrix (adj-mat)
  (declare (type dmat adj-mat))
  (check-type adj-mat dmat)
  (let* ((n (array-dimension adj-mat 0))
         (row-sum-list (loop for row below n collect
                             (loop for col below n sum (aref adj-mat col row)))))
    (loop for row below n
        for row-sum of-type double-float in row-sum-list
        unless (zerop row-sum)
        do (loop for col below n do
                 (setf (aref adj-mat col row) (/ (aref adj-mat col row) row-sum)))
        finally (return adj-mat))))
#+ignore
(defmethod power-centrality ((gr simple-graph)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 以下は多くの経路を媒介するノードは中心的であるとする指標
;; 媒介中心性
;; 頂点が、他の頂点間の最短経路上に位置する程度を中心性とする。
#+ignore
(defmethod betweenness-centrality ((gr simple-graph) &key (standardize nil))
  (let* ((dim (length (nodes gr)))
         (b (make-array `(,dim ,dim) :element-type 'double-float :initial-element 0d0))
         (res (make-dvec dim))
         (dp (directed-p gr))
         (denom (cond ((and dp standardize) (* (1- dim) (- dim 2)))
                      (standardize (* (1- dim) (- dim 2)))
                      (dp 1d0)
                      (t 2d0))))
    (multiple-value-bind (d-mat path-mat) (graph-distance-matrix gr t)
      (loop for start-i below dim
          do (loop for dest-i below dim
                 unless (eql start-id dest-i)
                 as paths = (%find-all-shortest-paths d-mat path-mat start-i dest-i)
                 do (setf (aref b dest-i start-i)))))
    (do-vec (_ res :type double-float :index-var i :setf-var sf :return res)
      (declare (ignore _))
      (setf sf (loop for row below dim sum (aref b i row) into cent
                   finally (return (/ cent denom)))))))

#+ignore
(defmethod information-centrality ((gr simple-graph)))




(defpackage :graph-shortest-path
  (:use :cl :excl :util :vector :matrix :read-graph :graph-utils :priority-que :missing-val)
  (:export #:find-shortest-path-dijkstra
           #:graph-distance-matrix))

(in-package :graph-shortest-path)

;; ダイクストラのアルゴリズムによって各ノードへの最短経路および最短距離を求める。
;; 終点ノードIDを指定すれば、始点から終点までの最短経路および最短距離が求まったらただちに計算
;; を終了し、それらを返す。
;; 無い場合は nil が返る
;; input: gr, <simple-graph>
;;        start-id-or-name, 始点ノードのIDまたは名前
;;        end-id-or-name, 終点ノードのIDまたは名前
;;        data-structure, :list | :binary | :binomial | :fibonacci, どのデータ構造を用いるか
;; output: 最短経路
;;         最短距離
(defmethod find-shortest-path-dijkstra ((gr simple-graph) start-id-or-name
                                        &key (end-id-or-name nil)
                                             (data-structure :binary)
                                             ;; :list | :binary | :binomial | :fibonacci
                                             )
  (let* ((start (retrieve-node gr start-id-or-name))
         (dest (when end-id-or-name (retrieve-node gr end-id-or-name)))
         (nodes (nodes gr))
         (nnodes (length nodes))
         (Dvec (make-array nnodes :initial-element *+inf*))
         (Pvec (make-array nnodes))
         (Bvec (make-array nnodes)))
    (when start
      (macrolet ((D (node) `(aref Dvec (node-buff ,node)))
                 (P (node) `(aref Pvec (node-buff ,node)))
                 (B (node) `(aref Bvec (node-buff ,node))))
        (let ((prique (make-prique data-structure
                                   :maxcount nnodes
                                   :lessp #'<
                                   :key #'(lambda (x) (D x))))
              (v nil))
          (flet ((update (v w Lvw)
                   (when (< (+ (D v) Lvw) (D w))
                     (setf (D w) (+ (D v) Lvw)
                           (P w) v)
                     (after-decrease-key-prique prique (B w)))))
            (loop for n in nodes
                for i from 0
                do (setf (node-buff n) i))
            (setf (D start) 0)
            (dolist (n nodes)
              (setf (B n) (insert-prique prique n)))
            (while (not (prique-empty-p prique))
              (setq v (delete-min-prique prique))
              (when (= (D v) *+inf*)
                (setq v nil)
                (return))
              (when (and dest (eq v dest)) (return))
              (loop for (w . Lvw) in (adjacency v gr)
                  do (assert (plusp Lvw))
                     (update v w Lvw)))
            ;;
            (flet ((get-path-distance (node)
                     (let ((path nil)
                           (w node)
                           (distance (D node)))
                       (if (= distance *+inf*)
                           (setq path `(,start nil ,node))
                         (progn (push w path)
                                (while (setq w (P w)) (push w path))))
                       `(:path ,path :distance ,(dfloat distance)))))
              (cond (dest (get-path-distance dest))
                    ((every #'null Pvec) nil)
                    (t (loop for node in (remove start nodes :test #'eq)
                           collect (get-path-distance node)))))))))))
;; (最短)距離行列を求める
(defmethod graph-distance-matrix ((gr simple-graph) &optional (path-mat-p nil))
  (let* ((nodes (nodes gr))
         (n (length nodes))
         (mat (make-array `(,n ,n) :element-type 'double-float :initial-element *+inf*))
         (path-mat (when path-mat-p (make-array `(,n ,n) :element-type t :initial-element nil))))
    (loop for i below n do (setf (aref mat i i) 0d0))
    (loop for row below n
        as path-d-list = (find-shortest-path-dijkstra gr (1+ row))
        when path-d-list
        do (loop for path-d in path-d-list
               as path = (getf path-d :path)
               as d = (getf path-d :distance)
               as col = (1- (node-id (car (last path))))
               unless (some #'null path)
               do (setf (aref mat col row) (dfloat d))
                  (when path-mat-p (setf (aref path-mat col row) path)))
        finally (return (values mat path-mat)))))

(defun %find-all-shortest-paths (d-mat path-mat start-i dest-i &key (d-thld nil))
  (flet ((row-aref (mat i) (declare (type dmat mat) (type fixnum i))
                   (let ((vec (make-dvec (array-dimension mat 0))))
                     (do-vec (_ vec :type double-float :index-var j :setf-var sf :return vec)
                       (declare (ignore _))
                       (setf sf (aref mat j i))))))
    (let* ((start-vec (row-aref d-mat start-i))
           (min-d (aref d-mat dest-i start-i)))
      (remove-duplicates
       (loop with %d-thld = (if d-thld d-thld min-d)
           for val double-float across start-vec
           for i from 0
           as path = (aref path-mat i start-i)
           if (and path (not (zerop val)) (not (eql i dest-i)) (< val %d-thld))
           append (mapcar (lambda (%path)
                            (when %path
                              `(:path ,(append path (cdr (getf %path :path)))
                                      :distance ,(+ val (getf %path :distance)))))
                          (%find-all-shortest-paths d-mat path-mat i dest-i 
                                                    :d-thld (- %d-thld val)))
           else if (and path (eql i dest-i) (<= val %d-thld))
           collect `(:path ,path :distance ,val))
       :test (lambda (p1 p2) (let ((p1 (getf p1 :path)) (p2 (getf p2 :path)))
                               (and (eql (length p1) (length p2))
                                    (every #'eq p1 p2))))))))
#||
(defmethod find-all-shortest-paths ((gr simple-graph))
  (multiple-value-bind (d-mat path-mat) (graph-distance-matrix gr t)))
||#
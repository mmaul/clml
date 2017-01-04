;-*- coding: utf-8 -*-
(in-package :clml.graph.graph-utils)

(defgeneric retrieve-node (gr id-or-name))
(defmethod retrieve-node ((gr simple-graph) id-or-name)
  (etypecase id-or-name
    (fixnum (gethash id-or-name (clml.graph.read-graph::node-hashtab gr)))
    (string (loop for node being each hash-value in (clml.graph.read-graph::node-hashtab gr)
                as name = (node-name node)
                when (equal name id-or-name)
                return node))))

(defgeneric retrieve-link (gr nid1-or-name nid2-or-name))
(defmethod retrieve-link ((gr simple-graph) nid1-or-name nid2-or-name)
  (let ((node1 (retrieve-node gr nid1-or-name))
        (node2 (retrieve-node gr nid2-or-name))
        (htab (clml.graph.read-graph::link-hashtab gr))
        (dirp (directed-p gr)))
    (when (and node1 node2)
      (let ((nid1 (node-id node1))
            (nid2 (node-id node2)))
        (if dirp
            (gethash (cons nid1 nid2) htab)
          (or (gethash (cons nid1 nid2) htab)
              (gethash (cons nid2 nid1) htab)))))))

;; 隣接ノードを取り出す。
;; output: alist: key: <node> val: weight
;; 無い場合は nil
(defgeneric adjacency (nd gr)
  (:documentation "
 I take out the adjacent node .
Output:
- alist: key: <node> val: weight
  If not nil
"))
(defmethod adjacency ((nd node) (gr simple-graph))
  (loop with nid = (node-id nd)
      for link in (if (directed-p gr)
                      (remove nid (node-links nd) :key #'link-node1 :test-not #'eql)
                    (node-links nd))
      as w = (link-weight link)
      as adj = (clml.graph.read-graph::get-node gr
                                     (cond ((eql (link-node1 link) nid) (link-node2 link))
                                           ((eql (link-node2 link) nid) (link-node1 link))
                                           (t (error "illegal graph structure: ~A is a link of ~A" link nd))))
      collect (cons adj w)))

;; <simple-graph> から隣接行列を取り出す。
(defgeneric adjacency-matrix (gr)
  (:documentation "Take adjacency matrix from simple graph"))

(defmethod adjacency-matrix ((gr simple-graph))
  (let* ((size (length (nodes gr)))
         (mat (make-array `(,size ,size) :element-type 'double-float
                          :initial-element 0d0)))
    (loop for link in (links gr)
        as row = (1- (link-node1 link))
        as col = (1- (link-node2 link))
        as w = (link-weight link)
        do (if (directed-p gr)
               (setf (aref mat col row) w)
             (setf (aref mat col row) w (aref mat row col) w))
        finally (return mat))))

;; 連結なノードの集合を集める
(defgeneric get-connected-components (gr)
  (:documentation "
 Collect a set of connected nodes
"))
(defmethod get-connected-components ((gr simple-graph))
  (let ((node-buffs (mapcar (lambda (node) (prog1 (node-buff node) (setf (node-buff node) nil)))
                            (nodes gr))))
    (labels ((visited-p (node) (node-buff node))
             (visit (node)
               (setf (node-buff node) t)
               (cons node (loop for adj in (mapcar #'car (adjacency node gr))
                              unless (visited-p adj)
                              append (visit adj)))))
      (prog1 (loop for node in (nodes gr)
                 unless (visited-p node)
                 collect (visit node))
        (mapc (lambda (node buff) (setf (node-buff node) buff))
              (nodes gr) node-buffs)))))

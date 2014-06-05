;;;This package was written by Abe_s originally named 'netviz-lib.cl'.

;;;(eval-when (:load-toplevel :compile-toplevel :execute)
;;;  (setq *locale* (find-locale "japan.932")))

(defpackage :priority-que
  (:use :cl :excl)
  (:export #:make-prique
           #:prique-empty-p
           #:prique-box-item
           #:insert-prique
           #:find-min-prique
           #:delete-min-prique
           #:union-prique
           #:after-decrease-key-prique
           ))

(in-package :priority-que)

;; (declaim (optimize (speed 3) (safety 0) (debug 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some constants

(defconstant portable-most-positive-fixnum (1- (expt 2 29)))
(defconstant portable-most-negative-fixnum (- (expt 2 29)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; QUE - FIFO queue

(defstruct (que (:constructor _make-que))
  list
  last)

(defmethod print-object ((que que) stream)
  (format stream "#<que ~s>" (cdr (que-list que))))

(defun make-que ()
  (let ((que (_make-que))
	(root (list nil)))
    (setf (que-list que) root
	  (que-last que) root)
    que))
    
(defun append-que (que x)
  (setf (que-last que)
	(setf (cdr (que-last que))
	      (list x)))
  x)

(defun append-que-list (que x)
  (dolist (e x)
    (append-que que e))
  x)

(defun insert-que (que x)
  (setf (car (que-list que)) x)
  (setf (que-list que) (cons nil (que-list que)))
  x)

(defun pop-que (que)
  (if (cdr (que-list que))
      (prog1
	  (cadr (que-list que))
	(setf (cdr (que-list que))
          (cddr (que-list que)))
        ;; !!!!!
        (unless (cdr (que-list que))
          (setf (que-last que) (que-list que))))
    (error "Empty que")))

(defun insert-que-before-last (que x)
  (assert (not (eq (que-list que) (que-last que))))
  (let ((last (que-last que))
	(last1 (que-list que)))
    (while (not (eq (cdr last1) last))
      (setq last1 (cdr last1)))
    (setf (cdr last1) (cons x last))
    x))

(defun insert-que-list-before-last (que x)
  (dolist (e x)
    (insert-que-before-last que e))
  x)

(defun que-to-list (que)
  (cdr (que-list que)))

(defun list-to-que (list)
  (let ((que (make-que)))
    (append-que-list que list)
    que))

(defun que-empty-p (que)
  (null (que-to-list que)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DQUE - Doubly linked list

(defstruct dque
  next
  prev
  ;; 
  item)

(defconstant dque-item-header-tag '#'dque-item-header-tag)

(defmethod print-object ((dque dque) stream)
  (cond
   ((dque-header-p dque)
    (format stream "#<dque-header>")
    #+ignore
    (format stream "#<dque-header ~s>" (dque-to-list dque)))
   (t
    (format stream "#<dque-item>")
    #+ignore
    (format stream "#<dque-item ~s>" (dque-item dque)))))

(defun make-dque-header (&optional (d (make-dque)))
  (link2-dque d d)
  (setf (dque-item d) dque-item-header-tag)
  d)

(defun dque-header-p (x)
  (and (dque-p x)
       (eq (dque-item x) dque-item-header-tag)))

(defun dque-empty-p (dqh)
  (assert (dque-header-p dqh))
  (eq (dque-next dqh) dqh))

(defun first-dque (dqh)
  (assert (dque-header-p dqh))
  (dque-next dqh))

(defun last-dque (dqh)
  (assert (dque-header-p dqh))
  (dque-prev dqh))

(defun link2-dque (d1 d2)
  (setf (dque-next d1) d2
	(dque-prev d2) d1)
  nil)

(defun link3-dque (d1 d2 d3)
  (setf (dque-next d1) d2
	(dque-next d2) d3
	(dque-prev d3) d2
	(dque-prev d2) d1)
  nil)

(defun append-dque (d1 d2)
  #+ignore (assert (and (null (dque-prev d2)) (null (dque-next d2))))
  (link3-dque d1 d2 (dque-next d1))
  d2)

(defun insert-dque (d1 d2)
  #+ignore (assert (and (null (dque-prev d2)) (null (dque-next d2))))
  (link3-dque (dque-prev d1) d2 d1)
  d2)

(defun delete-dque (d)
  #+ignore (assert (and (dque-prev d) (dque-next d)))
  (link2-dque (dque-prev d) (dque-next d))
  #+ignore (setf (dque-prev d) nil (dque-next d) nil)
  d)

(defun merge-dque (dqh1 dqh2)
  "Example:
 -> (setq dqh1 (list-to-dque '(a b c)))
 -> (setq dqh2 (list-to-dque '(d e f)))
 -> (merge-dque dqh1 dqh2)
 -> (dque-to-list dqh1)
    (a b c d e f)
 -> (dque-empty-p dqh2)
    t"
  (assert (dque-header-p dqh1))
  (assert (dque-header-p dqh2))
  (link2-dque (last-dque dqh1) (first-dque dqh2))
  (link2-dque (last-dque dqh2) dqh1)
  (make-dque-header dqh2)
  dqh1)

(defmacro do-dque ((var dqh) &rest body)
  `(let ((,var ,dqh))
     (assert (dque-header-p ,dqh))
     (while (not (eq (setq ,var (dque-next ,var))
                     ,dqh))
       ,@body)))

(defmacro do-dque-reverse ((var dqh) &rest body)
  `(let ((,var ,dqh))
     (assert (dque-header-p ,dqh))
     (while (not (eq (setq ,var (dque-prev ,var))
                     ,dqh))
       ,@body)))

(defun list-to-dque (list)
  (let ((dqh (make-dque-header)))
    (dolist (x list)
      (insert-dque dqh (make-dque :item x)))
    dqh))

(defun dque-to-list (dqh)
  (assert (dque-header-p dqh))
  (let ((ret nil))
    (do-dque (d dqh)
      (push (dque-item d) ret))
    ;;
    (nreverse ret)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LHEAP - List implementation of priority queue

(defstruct (lheap
            (:constructor _make-lheap))
  (head nil)
  (tail nil)
  (lessp #'<)
  (key #'identity))

(defstruct lheap-box
  item
  keyval)


(defmethod print-object ((lheap lheap) sm)
  (format sm "#<lheap ~s>" (mapcar #'lheap-box-keyval (lheap-head lheap)))
  (assert (or (and (lheap-head lheap)
                   (eq (lheap-tail lheap)
                       (last (lheap-head lheap))))
              (and (null (lheap-head lheap))
                   (null (lheap-tail lheap))))))

(defun make-lheap (&key (lessp #'<) (key #'identity))
  (_make-lheap :lessp lessp :key key))

(defun lheap-empty-p (lh)
  (null (lheap-head lh)))

(defun make-lheap-empty (lh)
  (setf (lheap-head lh) nil
        (lheap-tail lh) nil))

(defun insert-lheap (lh item)
  ;; Insert ITEM into linear heap LH where
  ;; the ITEM is boxed with a box object of type lheap-box
  ;; and returns the box.
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (unless (lheap-p lh)
    (error "insert-lheap: ~s is not lheap-p" lh))
  (let* ((key    (lheap-key   lh))
         (keyval (funcall key item))
         (ib (make-lheap-box :item item :keyval keyval))
         (place (cons ib nil)))
    (cond
     ((lheap-tail lh)
      (setf (cdr (lheap-tail lh)) place
            (lheap-tail lh) place))
     (t
      (setf (lheap-head lh) place
            (lheap-tail lh) place)))
    ;;
    ib))

(defun find-min-lheap (lh)
  ;; Returns an item of minimum key in linear heap LH.
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (unless (lheap-p lh)
    (error "find-min-lheap: ~s is not lheap-p" lh))
  (when (lheap-empty-p lh)
    (error "find-min-lheap: ~s is empty" lh))
  (let ((lessp (lheap-lessp lh))
        (minib (car (lheap-head lh))))
    (dolist (ib (cdr (lheap-head lh)))
      (when (funcall lessp (lheap-box-keyval ib) (lheap-box-keyval minib))
        (setq minib ib)))
    ;;
    (lheap-box-item minib)))

(defun delete-min-lheap (lh)
  ;; Removes an item of minimum key in linear heap LH
  ;; and returns the item.
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (unless (lheap-p lh)
    (error "find-min-lheap: ~s is not lheap-p" lh))
  (when (lheap-empty-p lh)
    (error "find-min-lheap: ~s is empty" lh))
  (let* ((lessp (lheap-lessp lh))
         (head (lheap-head lh))
         (minplace head)
         (minib (car head))
         (minprev nil)
         (prev head))
    (loop
        for place on (cdr head)
        for ib = (car place)
        do
          (when (funcall lessp (lheap-box-keyval ib) (lheap-box-keyval minib))
            (setq minplace place
                  minib ib
                  minprev prev))
          (setq prev place))
    ;;
    (when (eq (lheap-tail lh) minplace)
      (setf (lheap-tail lh) minprev))
    ;;
    (cond
     (minprev
      (setf (cdr minprev) (cddr minprev)))
     (t
      (pop (lheap-head lh))))
    ;;
    (lheap-box-item minib)))

(defun union-lheap (lh1 lh2)
  ;; Make the union of two linear heaps LH1 and LH2.
  ;; The resulting heap is stored destructively into LH1 and it is returned.
  ;; LH2 is also destroyed and is made empty.
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (unless (lheap-p lh1)
    (error "union-lheap: ~s is not lheap-p" lh1))
  (unless (lheap-p lh2)
    (error "union-lheap: ~s is not lheap-p" lh2))
  (unless (eq (lheap-lessp lh1) (lheap-lessp lh2))
    (error "union-lheap: given heaps have different lessp: ~s, ~s"
           (lheap-lessp lh1) (lheap-lessp lh2)))
  (unless (eq (lheap-key lh1) (lheap-key lh2))
    (error "union-lheap: given heaps have different key: ~s, ~s"
           (lheap-key lh1) (lheap-key lh2)))
  ;;
  (cond
   ((lheap-empty-p lh1)
    (setf (lheap-head lh1) (lheap-head lh2)
          (lheap-tail lh1) (lheap-tail lh2)))
   ((lheap-empty-p lh2))
   (t
    (setf (cdr (lheap-tail lh1)) (lheap-head lh2)
          (lheap-tail lh1) (lheap-tail lh2))))
  ;;
  (make-lheap-empty lh2)
  ;;
  lh1)

(defun after-decrease-key-lheap (lh ib)
  ;; After decreasing key of an item in binary heap LH,
  ;; this function rebalances LH.
  ;; The second argument must be the box IB of the item instead of item
  ;; itself.  The box is an object of type lheap-box returned
  ;; from the function insert-lheap when the item was inserted into LH.
  ;; Returns IB.
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (unless (lheap-p lh)
    (error "after-decrease-key-lheap: ~s is not lheap-p" lh))
  (unless (lheap-box-p ib)
    (error "after-decrease-key-lheap: ~s is not lheap-box-p" ib))
  ;;
  (let* ((lessp (lheap-lessp lh))
         (key (lheap-key lh))
         (newkey (funcall key (lheap-box-item ib))))
    (when (funcall lessp (lheap-box-keyval ib) newkey)
      (error "after-decrease-key-lheap: key of ~s has been increased"
             (lheap-box-item ib)))
    (setf (lheap-box-keyval ib) newkey)
    ;;
    ib))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BIHEAP - Binary Heap implementation of priority queue

(defstruct (biheap
            (:constructor _make-biheap))
  ;;    0
  ;;  1   2
  ;; 3 4 5 6
  (array nil :type (simple-array t (*))) ; of biheap-box
  (count 0 :type fixnum)
  (lessp #'<)
  (key #'identity))

(defstruct biheap-box
  item
  (node 0 :type fixnum)
  keyval)


(defmethod print-object ((biheap biheap) sm)
  (format-biheap sm biheap))

(defmacro biheap-parent (id) `(the fixnum (floor (the fixnum (1- ,id)) 2)))
(defmacro biheap-left   (id) `(the fixnum (1+ (the fixnum (* ,id 2)))))
(defmacro biheap-right  (id) `(the fixnum (* (the fixnum (1+ ,id)) 2)))

(defun format-biheap (sm bh)
  (unless (biheap-p bh)
    (error "format-biheap: ~s is not biheap-p" bh))
  (let ((array (biheap-array bh))
        (count (biheap-count bh))
        (lessp (biheap-lessp bh))
        (key   (biheap-key   bh)))
    (flet ((childfun (id)
             (append (and (< (biheap-left id) count)
                          (list (biheap-left id)))
                     (and (< (biheap-right id) count)
                          (list (biheap-right id)))))
           (labelfun (id)
             (list
              (format nil "~s"
                      (funcall key (biheap-box-item (aref array id))))
              #+ignore
              (format nil "~s ~s ~d"
                      (funcall key (biheap-box-item
                                    (aref array id)))
                      (biheap-box-item (aref array id))
                      (biheap-box-node (aref array id)))
              "")))
      (format sm "BinaryHeap lessp=~s count=~d~%" lessp count)
      (unless (biheap-empty-p bh)
        (terpri sm)
        (format-tree sm 0 #'childfun #'labelfun)))))


(defun make-biheap (maxcount &key (lessp #'<) (key #'identity))
  (unless (typep maxcount '(integer 0 *))
    (error "make-biheap: maxcount=~s must be a non-negative integer" maxcount))
  (_make-biheap :array (make-array maxcount)
                :count 0
                :lessp lessp
                :key key))

(defun biheap-empty-p (bh)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (zerop (biheap-count bh)))

(defun make-biheap-empty (bh)
  (setf (biheap-count bh) 0)
  bh)

(defun union-biheap (bh1 bh2)
  ;; Make the union of two binary heaps BH1 and BH2.
  ;; The resulting heap is stored destructively into BH1 and it is returned.
  ;; BH2 is also destroyed and is made empty.
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (unless (biheap-p bh1)
    (error "union-biheap: ~s is not biheap-p" bh1))
  (unless (biheap-p bh2)
    (error "union-biheap: ~s is not biheap-p" bh2))
  (unless (eq (biheap-lessp bh1) (biheap-lessp bh2))
    (error "union-biheap: given heaps have different lessp: ~s, ~s"
           (biheap-lessp bh1) (biheap-lessp bh2)))
  (unless (eq (biheap-key bh1) (biheap-key bh2))
    (error "union-biheap: given heaps have different key: ~s, ~s"
           (biheap-key bh1) (biheap-key bh2)))
  ;;
  (let ((array (biheap-array bh2)))
    (dotimes (i (biheap-count bh2))
      (insert-biheap bh1 (biheap-box-item (aref array i))))
    (make-biheap-empty bh2)
    ;;
    bh1))

(defun insert-biheap (bh item)
  ;; Insert ITEM into binary heap BH where
  ;; the ITEM is boxed with a box object of type biheap-box
  ;; and returns the box.
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (unless (biheap-p bh)
    (error "insert-biheap: ~s is not biheap-p" bh))
  (let* ((array (biheap-array bh))
         (id    (biheap-count bh))
         (lessp (biheap-lessp bh))
         (key   (biheap-key   bh))
         (keyval (funcall key item))
         (ib (make-biheap-box :item item :node id :keyval keyval)))
    (declare (type fixnum id))
    (unless (< (biheap-count bh) (length array))
      (error "insert-biheap: cannot insert more than maxcount=~d items"
             (length array)))
    (incf (biheap-count bh))
    (while (and (<= 1 id)
                (funcall lessp
                         keyval
                         (biheap-box-keyval
                          (aref array (biheap-parent id)))))
      (setf (aref array id) (aref array (biheap-parent id))
            (biheap-box-node (aref array id)) id
            id (biheap-parent id)))
    (setf (aref array id) ib
          (biheap-box-node (aref array id)) id)
    ;;
    ib))

(defun find-min-biheap (bh)
  ;; Returns an item of minimum key in binary heap BH.
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (unless (biheap-p bh)
    (error "find-min-biheap: ~s is not biheap-p" bh))
  (when (biheap-empty-p bh)
    (error "find-min-biheap: ~s is empty" bh))
  ;;
  (biheap-box-item (aref (biheap-array bh) 0)))

(defun delete-min-biheap (bh)
  ;; Removes an item of minimum key in binary heap BH
  ;; and returns the item.
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (unless (biheap-p bh)
    (error "delete-min-biheap: ~s is not biheap-p" bh))
  (when (biheap-empty-p bh)
    (error "delete-min-biheap: ~s is empty" bh))
  ;;
  (labels ((heapify-biheap (bh id)
             (declare (type fixnum id))
             (let* ((array (biheap-array bh))
                    (count (biheap-count bh))
                    (lessp (biheap-lessp bh))
                    (lid   (biheap-left  id))
                    (rid   (biheap-right id))
                    (sid (if (and (< lid count)
                                  (funcall
                                   lessp
                                   (biheap-box-keyval (aref array lid))
                                   (biheap-box-keyval (aref array id))))
                             lid
                           id)))
               (when (and (< rid count)
                          (funcall lessp
                                   (biheap-box-keyval (aref array rid))
                                   (biheap-box-keyval (aref array sid))))
                 (setq sid rid))
               (unless (= id sid)
                 (let ((idbox (aref array id))
                       (sidbox (aref array sid)))
                   (setf (aref array id) sidbox
                         (biheap-box-node sidbox) id)
                   (setf (aref array sid) idbox
                         (biheap-box-node idbox) sid))
                 (heapify-biheap bh sid)))))
    ;;
    (let* ((array (biheap-array bh))
           (minitem (biheap-box-item (aref array 0))))
      (decf (biheap-count bh))
      (setf (aref array 0) (aref array (biheap-count bh)))
      (setf (biheap-box-node (aref array 0)) 0)
      (heapify-biheap bh 0)
      ;;
      minitem)))

(defun after-decrease-key-biheap (bh ib)
  ;; After decreasing key of an item in binary heap BH,
  ;; this function rebalances BH.
  ;; The second argument must be the box IB of the item instead of item
  ;; itself.  The box is an object of type biheap-box returned
  ;; from the function insert-biheap when the item was inserted into BH.
  ;; Returns IB.
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (unless (biheap-p bh)
    (error "after-decrease-key-biheap: ~s is not biheap-p" bh))
  (unless (biheap-box-p ib)
    (error "after-decrease-key-biheap: ~s is not biheap-box-p" ib))
  ;;
  (let* ((array (biheap-array bh))
         (lessp (biheap-lessp bh))
         (key   (biheap-key   bh))
         (id1 (biheap-box-node ib))
         (id2 (biheap-parent id1))
         (newkey (funcall key (biheap-box-item ib))))
    (declare (type fixnum id1 id2))
    (when (funcall lessp
                   (biheap-box-keyval (aref array id1))
                   newkey)
      (error "after-decrease-key-biheap: key ~s of item ~s has been increased"
             newkey (biheap-box-item (aref array id1))))
    (setf (biheap-box-keyval (aref array id1)) newkey)
    (while (and (<= 0 id2)
                (funcall
                 lessp
                 (funcall key (biheap-box-item (aref array id1)))
                 (funcall key (biheap-box-item (aref array id2)))))
      ;; Swap boxes of elements at id1 and id2 in array.
      (let ((ib1 (aref array id1))
            (ib2 (aref array id2)))
        (setf (aref array id1) ib2
              (biheap-box-node ib2) id1)
        (setf (aref array id2) ib1
              (biheap-box-node ib1) id2))
      ;; Up
      (setq id1 id2
            id2 (biheap-parent id2)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BNHEAP - Binomial Heap implementation of priority queue

(defstruct (bnheap
            (:constructor _make-bnheap))
  (head nil)
  (lessp #'<)
  (key #'identity))

(defstruct bnheap-node
  (box nil)
  (parent nil)
  (degree 0 :type fixnum)
  (child nil)
  (sibling nil))

(defstruct bnheap-box
  item
  node
  keyval)


(defmacro bnheap-node-keyval (bn)
  `(bnheap-box-keyval (bnheap-node-box ,bn)))

(defmethod print-object ((bnheap bnheap) sm)
  (format-bnheap sm bnheap))

(defmethod print-object ((bnheap-node bnheap-node) sm)
  (format sm "#<bnheap-node degree=~d>"
          (bnheap-node-degree bnheap-node)))

(defun bnheap-degrees (bh)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (unless (bnheap-p bh)
    (error "bnheap-degrees: ~s is not bnheap-p" bh))
  (do ((bn (bnheap-head bh) (bnheap-node-sibling bn))
       (degs nil))
      ((null bn) (nreverse degs))
    (push (bnheap-node-degree bn) degs)))

(defun bnheap-count (bh)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (unless (bnheap-p bh)
    (error "bnheap-count: ~s is not bnheap-p" bh))
  (apply #'+ (mapcar #'(lambda (k) (expt 2 k))
                     (bnheap-degrees bh))))

(defun format-bnheap (sm bh)
  (unless (bnheap-p bh)
    (error "format-bnheap: ~s is not bnheap-p" bh))
  (let ((key (bnheap-key bh)))
    (format sm "BinomialHeap lessp=~s count=~d(=~{2^~d~^+~})~%"
            (bnheap-lessp bh) (bnheap-count bh) (bnheap-degrees bh))
    (unless (bnheap-empty-p bh)
      (terpri sm)
      (do ((bn (bnheap-head bh) (bnheap-node-sibling bn)))
          ((null bn) (values))
        (format-tree
         sm
         bn
         #'(lambda (bn)
             (do ((s (bnheap-node-child bn) (bnheap-node-sibling s))
                  (child nil))
                 ((null s) (nreverse child))
               (push s child)))
         #'(lambda (bn)
             (list (format nil "~s"
                           (funcall key
                                    (bnheap-box-item
                                     (bnheap-node-box bn))))
                   "")))))))


(defun make-bnheap (&key (lessp #'<) (key #'identity))
  (_make-bnheap :lessp lessp :key key))

(defun bnheap-empty-p (bh)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (unless (bnheap-p bh)
    (error "bnheap-empty-p: ~s is not bnheap-p" bh))
  (null (bnheap-head bh)))

(defun make-bnheap-empty (bh)
  (setf (bnheap-head bh) nil)
  bh)

(defun union-bnheap (bh1 bh2)
  ;; Make the union of two binomial heaps BH1 and BH2.
  ;; The resulting heap is stored destructively into BH1 and it is returned.
  ;; BH2 is also destroyed and is made empty.
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (unless (bnheap-p bh1)
    (error "union-bnheap: ~s is not bnheap-p" bh1))
  (unless (bnheap-p bh2)
    (error "union-bnheap: ~s is not bnheap-p" bh2))
  (unless (eq (bnheap-lessp bh1) (bnheap-lessp bh2))
    (error "union-bnheap: given heaps have different lessp: ~s, ~s"
           (bnheap-lessp bh1) (bnheap-lessp bh2)))
  (unless (eq (bnheap-key bh1) (bnheap-key bh2))
    (error "union-bnheap: given heaps have different key: ~s, ~s"
           (bnheap-key bh1) (bnheap-key bh2)))
  ;;
  (let* ((lessp (bnheap-lessp bh1))
         (key (bnheap-key bh1))
         (bh (make-bnheap :lessp lessp :key key)))
    (macrolet ((link-bnheap-node (y z)
                 `(setf (bnheap-node-parent ,y) ,z
                        (bnheap-node-sibling ,y) (bnheap-node-child ,z)
                        (bnheap-node-child ,z) ,y
                        (bnheap-node-degree ,z) (1+ (bnheap-node-degree ,z)))))
      (flet ((merge-bnheap (bh1 bh2)
               (let* ((bn1 (bnheap-head bh1))
                      (bn2 (bnheap-head bh2))
                      (head (make-bnheap-node))
                      (tail head))
                 (while (or bn1 bn2)
                   (cond
                    ((and bn1 bn2)
                     (let ((bn (cond
                                ((<= (bnheap-node-degree bn1)
                                     (bnheap-node-degree bn2))
                                 (prog1
                                     bn1
                                   (setq bn1 (bnheap-node-sibling bn1))))
                                (t
                                 (prog1
                                     bn2
                                   (setq bn2 (bnheap-node-sibling bn2)))))))
                       (setf (bnheap-node-sibling bn) nil
                             (bnheap-node-sibling tail) bn
                             tail bn)))
                    (bn1
                     (setf (bnheap-node-sibling tail) bn1
                           bn1 nil))
                    (bn2
                     (setf (bnheap-node-sibling tail) bn2
                           bn2 nil))))
                 ;;
                 (bnheap-node-sibling head))))
        ;;
        (setf (bnheap-head bh) (merge-bnheap bh1 bh2))
        (when (null (bnheap-head bh))
          (return-from union-bnheap bh))
        ;;
        (let* ((prev-x nil)
               (x (bnheap-head bh))
               (next-x (bnheap-node-sibling x)))
          (while next-x
            (cond
             ((or (/= (bnheap-node-degree x)
                      (bnheap-node-degree next-x))
                  (and (bnheap-node-sibling next-x)
                       (eq (bnheap-node-degree (bnheap-node-sibling next-x))
                           (bnheap-node-degree x))))
              (setq prev-x x
                    x next-x))
             ((not (funcall lessp
                            (bnheap-node-keyval next-x)
                            (bnheap-node-keyval x)))
              (setf (bnheap-node-sibling x) (bnheap-node-sibling next-x))
              (link-bnheap-node next-x x))
             (t
              (cond
               ((null prev-x)
                (setf (bnheap-head bh) next-x))
               (t
                (setf (bnheap-node-sibling prev-x) next-x)))
              (link-bnheap-node x next-x)
              (setq x next-x)))
            (setq next-x (bnheap-node-sibling x))))
        ;;
        (setf (bnheap-head bh1) (bnheap-head bh)
              (bnheap-lessp bh1) (bnheap-lessp bh)
              (bnheap-key bh1) (bnheap-key bh))
        ;;
        (make-bnheap-empty bh2)
        ;;
        bh1))))

(defun insert-bnheap (bh item)
  ;; Insert ITEM into binomial heap BH where
  ;; the ITEM is boxed with a box object of type bnheap-box
  ;; and returns the box.
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (unless (bnheap-p bh)
    (error "insert-bnheap: ~s is not bnheap-p" bh))
  (let* ((key (bnheap-key bh))
         (ib (make-bnheap-box :item item :keyval (funcall key item)))
         (bn (make-bnheap-node :box ib)))
    (setf (bnheap-box-node ib) bn)
    (union-bnheap bh (_make-bnheap :head bn
                                   :lessp (bnheap-lessp bh)
                                   :key (bnheap-key bh)))
    ;;
    ib))

(defun find-min-bnheap (bh)
  ;; Returns an item of minimum key in binomial heap BH.
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (unless (bnheap-p bh)
    (error "find-min-bnheap: ~s is not bnheap-p" bh))
  (when (bnheap-empty-p bh)
    (error "find-min-bnheap: ~s is empty" bh))
  (let ((lessp (bnheap-lessp bh))
        (minnode (bnheap-head bh)))
    (do ((node (bnheap-node-sibling minnode) (bnheap-node-sibling node)))
        ((null node))
      (when (funcall lessp
                     (bnheap-node-keyval node)
                     (bnheap-node-keyval minnode))
        (setq minnode node)))
    ;;
    (bnheap-box-item (bnheap-node-box minnode))))

(defun delete-min-bnheap (bh)
  ;; Removes an item of minimum key in binomial heap BH
  ;; and returns the item.
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (unless (bnheap-p bh)
    (error "find-min-bnheap: ~s is not bnheap-p" bh))
  (when (bnheap-empty-p bh)
    (error "find-min-bnheap: ~s is empty" bh))
  (let* ((lessp (bnheap-lessp bh))
         (minnode (bnheap-head bh))
         (node-prev minnode)
         (minnode-prev nil)
         (bh1 (make-bnheap :lessp (bnheap-lessp bh) :key (bnheap-key bh))))
    ;; Set minnode-prev to the previous node of the bnheap-node.
    (do ((node (bnheap-node-sibling minnode) (bnheap-node-sibling node)))
        ((null node))
      (when (funcall lessp
                     (bnheap-node-keyval node)
                     (bnheap-node-keyval minnode))
        (setq minnode node
              minnode-prev node-prev))
      (setq node-prev node))
    ;; Modify bh.
    (cond
     (minnode-prev
      (setf (bnheap-node-sibling minnode-prev) (bnheap-node-sibling minnode)))
     (t
      (setf (bnheap-head bh) (bnheap-node-sibling minnode))))
    (setf (bnheap-node-sibling minnode) nil)
    ;; Set up bh1.
    (let ((revsibling (bnheap-node-child minnode)))
      (setf (bnheap-node-child minnode) nil
            (bnheap-node-degree minnode) 0)
      (when revsibling
        (let ((prev nil)
              (next (bnheap-node-sibling revsibling)))
          (loop
            (setf (bnheap-node-parent revsibling) nil
                  (bnheap-node-sibling revsibling) prev
                  prev revsibling)
            (when (null next)
              (return))
            (setf revsibling next)
            (setf next (bnheap-node-sibling next)))
          (setf (bnheap-head bh1) revsibling))))
    ;; Union
    (union-bnheap bh bh1)
    ;;
    (bnheap-box-item (bnheap-node-box minnode))))

(defun after-decrease-key-bnheap (bh ib)
  ;; After decreasing key of an item in binomial heap BH,
  ;; this function rebalances BH.
  ;; The second argument must be the box IB of the item instead of item
  ;; itself.  The box is an object of type bnheap-box returned
  ;; from the function insert-bnheap when the item was inserted into BH.
  ;; Returns IB.
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (unless (bnheap-p bh)
    (error "after-decrease-key-bnheap: ~s is not bnheap-p" bh))
  (unless (bnheap-box-p ib)
    (error "after-decrease-key-bnheap: ~s is not bnheap-box-p" ib))
  ;;
  (let* ((lessp (bnheap-lessp bh))
         (key (bnheap-key bh))
         (bn1 (bnheap-box-node ib))
         (bn2 (bnheap-node-parent bn1))
         (newkey (funcall key (bnheap-box-item ib))))
    (when (funcall lessp
                   (bnheap-node-keyval bn1)
                   newkey)
      (error "after-decrease-key-bnheap: key of ~s has been increased"
             (bnheap-box-item ib)))
    (setf (bnheap-node-keyval bn1) newkey)
    (while (and bn2 (funcall lessp
                             (bnheap-node-keyval bn1)
                             (bnheap-node-keyval bn2)))
      ;; Swap boxes of bn1 and bn2.
      (let ((ib1 (bnheap-node-box bn1))
            (ib2 (bnheap-node-box bn2)))
        (setf (bnheap-node-box bn1) ib2
              (bnheap-box-node ib2) bn1)
        (setf (bnheap-node-box bn2) ib1
              (bnheap-box-node ib1) bn2))
      ;; Up
      (setq bn1 bn2
            bn2 (bnheap-node-parent bn2))))
  ;;
  ib)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FHEAP - Fibonacci heap implementation of priority queue

(defstruct fdque
  next
  prev)

(defmacro link2-fdque (d1 d2)
  (let ((vd1 (gensym))
        (vd2 (gensym)))
    `(let ((,vd1 ,d1)
           (,vd2 ,d2))
       (setf (fdque-next ,vd1) ,vd2
             (fdque-prev ,vd2) ,vd1))))

(defmacro link3-fdque (d1 d2 d3)
  (let ((vd1 (gensym))
        (vd2 (gensym))
        (vd3 (gensym)))
    `(let ((,vd1 ,d1)
           (,vd2 ,d2)
           (,vd3 ,d3))
       (setf (fdque-next ,vd1) ,vd2
             (fdque-next ,vd2) ,vd3
             (fdque-prev ,vd3) ,vd2
             (fdque-prev ,vd2) ,vd1))))

(defun make-fdque-header (&optional (d (make-fdque)))
  (link2-fdque d d)
  d)

(defmacro fdque-empty-p (dqh)
  `(eq (fdque-next ,dqh) ,dqh))

(defmacro append-fdque (d1 d2)
  (let ((vd1 (gensym))
        (vd2 (gensym)))
    `(let ((,vd1 ,d1)
           (,vd2 ,d2))
       (link3-fdque ,vd1 ,vd2 (fdque-next ,vd1)))))

(defmacro delete-fdque (d)
  (let ((vd (gensym)))
    `(let ((,vd ,d))
       (link2-fdque (fdque-prev ,vd) (fdque-next ,vd)))))

(defmacro merge-fdque (dqh1 dqh2)
  (let ((vdqh1 (gensym))
        (vdqh2 (gensym)))
    `(let ((,vdqh1 ,dqh1)
           (,vdqh2 ,dqh2))
       (link2-fdque (fdque-prev ,vdqh1) (fdque-next ,vdqh2))
       (link2-fdque (fdque-prev ,vdqh2) ,vdqh1)
       (make-fdque-header ,vdqh2))))

(defmacro do-fdque ((var dqh) &rest body)
  `(let ((,var ,dqh))
     (while (not (eq (setq ,var (fdque-next ,var))
                     ,dqh))
       ,@body)))


(defstruct (fheap
            (:constructor _make-fheap))
  (lessp #'<)
  (key #'identity)
  (count 0 :type fixnum)
  (minnode nil)
  (trees (make-fdque-header))
  (Dn 0 :type fixnum)
  (A (make-array 100) :type (simple-array t (*))))

(defstruct (fheap-node
            (:include fdque))
  (box nil)
  (parent nil)
  (degree 0 :type fixnum)
  (mark nil)
  (child (make-fdque-header)))
  
(defstruct fheap-box
  item
  node
  keyval)


(defmacro fheap-node-keyval (bn)
  `(fheap-box-keyval (fheap-node-box ,bn)))

(defmethod print-object ((fheap fheap) sm)
  (format-fheap sm fheap))

(defmethod print-object ((fheap-node fheap-node) sm)
  (let ((key #'identity))
    (format-tree
     sm
     fheap-node
     #'(lambda (fn)
         (let ((child nil))
           (do-fdque (c (fheap-node-child fn))
             (push c child))
           (nreverse child)))
     #'(lambda (fn)
         (list
          (format nil "~s"
                  (funcall key (fheap-box-item (fheap-node-box fn))))
          #+ignore
          (format nil "~s ~s"
                  (funcall key (fheap-box-item (fheap-node-box fn)))
                  (fheap-box-item (fheap-node-box fn)))
          
          "")))))

(defun format-fheap (sm fh)
  (unless (fheap-p fh)
    (error "format-fheap: ~s is not fheap-p" fh))
  (let ((key (fheap-key fh)))
    (format sm "FibonacciHeap lessp=~s count=~d minkey=~s~%"
            (fheap-lessp fh)
            (fheap-count fh)
            (and (fheap-minnode fh)
                 (fheap-node-keyval (fheap-minnode fh))))
    (unless (fheap-empty-p fh)
      (terpri sm)
      (do-fdque (fn (fheap-trees fh))
        (format-tree
         sm
         fn
         #'(lambda (fn)
             (let ((child nil))
               (do-fdque (c (fheap-node-child fn))
                 (push c child))
               (nreverse child)))
         #'(lambda (fn)
             (list
              (format nil "~s"
                      (funcall key (fheap-box-item (fheap-node-box fn))))
              #+ignore
              (format nil "~s ~s"
                      (funcall key (fheap-box-item (fheap-node-box fn)))
                      (fheap-box-item (fheap-node-box fn)))
              
              "")))))))

(defun make-fheap (&key (lessp #'<) (key #'identity))
  (_make-fheap :lessp lessp :key key))

(defun fheap-empty-p (fh)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (zerop (fheap-count fh)))

(defun make-fheap-empty (fh)
  (setf (fheap-count fh) 0
        (fheap-minnode fh) nil
        (fheap-trees fh) (make-fdque-header)
        (fheap-Dn fh) 0)
  fh)

(defun insert-fheap (fh item)
  ;; Insert ITEM into Fibonacci heap FH where
  ;; the ITEM is boxed with a box object of type fheap-box
  ;; and returns the box.
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (unless (fheap-p fh)
    (error "insert-fheap: ~s is not fheap-p" fh))
  (let* ((lessp (fheap-lessp fh))
         (key   (fheap-key   fh))
         (ib (make-fheap-box :item item :keyval (funcall key item)))
         (fn (make-fheap-node :box ib)))
    (setf (fheap-box-node ib) fn)
    (incf (fheap-count fh))
    (append-fdque (fheap-trees fh) fn)
    (when (or (null (fheap-minnode fh))
              (funcall lessp
                       (funcall key item)
                       (fheap-box-keyval (fheap-node-box (fheap-minnode fh)))))
      (setf (fheap-minnode fh) fn))
    ;;
    ib))

(defun union-fheap (fh1 fh2)
  ;; Make the union of two Fibonacci heaps FH1 and FH2.
  ;; The resulting heap is stored destructively into FH1 and it is returned.
  ;; FH2 is also destroyed and is made empty.
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (unless (fheap-p fh1)
    (error "union-fheap: ~s is not fheap-p" fh1))
  (unless (fheap-p fh2)
    (error "union-fheap: ~s is not fheap-p" fh2))
  (unless (eq (fheap-lessp fh1) (fheap-lessp fh2))
    (error "union-fheap: given heaps have different lessp: ~s, ~s"
           (fheap-lessp fh1) (fheap-lessp fh2)))
  (unless (eq (fheap-key fh1) (fheap-key fh2))
    (error "union-fheap: given heaps have different key: ~s, ~s"
           (fheap-key fh1) (fheap-key fh2)))
  (let ((lessp (fheap-lessp fh1)))
    (merge-fdque (fheap-trees fh1) (fheap-trees fh2))
    (when (or (null (fheap-minnode fh1))
              (and (fheap-minnode fh2)
                   (funcall lessp
                            (fheap-node-keyval (fheap-minnode fh2))
                            (fheap-node-keyval (fheap-minnode fh1)))))
      (setf (fheap-minnode fh1) (fheap-minnode fh2)))
    (incf (fheap-count fh1) (fheap-count fh2))
    ;;
    (make-fheap-empty fh2)
    ;;
    fh1))

(defun find-min-fheap (fh)
  ;; Returns an item of minimum key in Fibonacci heap FH.
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (unless (fheap-p fh)
    (error "find-min-fheap: ~s is not fheap-p" fh))
  (when (fheap-empty-p fh)
    (error "find-min-fheap: ~s is empty" fh))
  (fheap-box-item (fheap-node-box (fheap-minnode fh))))

(defun delete-min-fheap (fh)
  ;; Removes an item of minimum key in Fibonacci heap FH
  ;; and returns the item.
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (unless (fheap-p fh)
    (error "delete-min-fheap: ~s is not fheap-p" fh))
  (when (fheap-empty-p fh)
    (error "delete-min-fheap: ~s is empty" fh))
  (let* ((minnode (fheap-minnode fh))
         (child (fheap-node-child minnode)))
    (while (not (fdque-empty-p child))
      (let ((cn (fdque-next child)))
        (delete-fdque cn)
        (setf (fheap-node-parent cn) nil)
        (append-fdque minnode cn)))
    ;;
    (delete-fdque minnode)
    (decf (fheap-count fh))
    (cond
     ((fheap-empty-p fh)
      (setf (fheap-minnode fh) nil))
     (t
      (setf (fheap-minnode fh) (fdque-next (fheap-trees fh)))
      (consolidate-fheap fh)))
    ;;
    (fheap-box-item (fheap-node-box minnode))))

(defun consolidate-fheap (fh)
  ;; Consolidates Fibonacci heap FH and returns it.
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (unless (fheap-p fh)
    (error "consolidate-fheap: ~s is not fheap-p" fh))
  (let* ((lessp (fheap-lessp fh))
         (A (fheap-A fh))
         (trees (fheap-trees fh))
         (w (fdque-next trees)))
    (flet ((adjustA (d)
             (declare (type fixnum d))
             (when (< (fheap-Dn fh) d)
               (loop for i fixnum from 0 to d
                   do (setf (aref A i) nil))
               (setf (fheap-Dn fh) d)))
           (link (y x)
             (progn
               (delete-fdque y)
               (setf (fheap-node-parent y) x)
               (append-fdque (fheap-node-child x) y)
               (incf (fheap-node-degree x))
               (setf (fheap-node-mark y) nil))))
      (loop for d fixnum from 0 to (fheap-Dn fh)
          do (setf (aref A d) nil))
      (while (not (eq w trees))
        (let* ((w2 (fdque-next w))
               (x w)
               (d (fheap-node-degree x)))
          (declare (type fixnum d))
          (while (progn
                   (adjustA d)
                   (aref A d))
            (let ((y (aref A d)))
              (when (funcall lessp
                             (fheap-node-keyval y)
                             (fheap-node-keyval x))
                (rotatef x y))
              (link y x)
              (setf (aref A d) nil)
              (incf d)))
          (setf (aref A d) x)
          (setf (fheap-node-parent x) nil)
          (setq w w2)))
      ;;
      (setf (fheap-minnode fh) nil)
      (loop for d fixnum from 0 to (fheap-Dn fh)
          do (when (and (aref A d)
                        (or (null (fheap-minnode fh))
                            (funcall lessp
                                     (fheap-node-keyval (aref A d))
                                     (fheap-node-keyval (fheap-minnode fh)))))
               (setf (fheap-minnode fh) (aref A d))))
      ;;
      fh)))

(defun after-decrease-key-fheap (fh ib)
  ;; After decreasing key of an item in Fibonacci heap FH,
  ;; this function rebalances FH.
  ;; The second argument must be the box IB of the item instead of item
  ;; itself.  The box is an object of type fheap-box returned
  ;; from the function insert-fheap when the item was inserted into FH.
  ;; Returns IB.
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (unless (fheap-p fh)
    (error "after-decrease-key-fheap: ~s is not fheap-p" fh))
  (unless (fheap-box-p ib)
    (error "after-decrease-key-fheap: ~s is not fheap-box-p" ib))
  ;;
  (let* ((lessp (fheap-lessp fh))
         (key (fheap-key fh))
         (trees (fheap-trees fh))
         (newkey (funcall key (fheap-box-item ib))))
    (labels ((cut (x y)
               (delete-fdque x)
               (setf (fheap-node-parent x) nil)
               (decf (fheap-node-degree y))
               (append-fdque trees x)
               (setf (fheap-node-mark x) nil))
             (cascading-cut (y)
               (let ((z (fheap-node-parent y)))
                 (when z
                   (cond
                    ((not (fheap-node-mark y))
                     (setf (fheap-node-mark y) t))
                    (t
                     (cut y z)
                     (cascading-cut z)))))))
      (let* ((x (fheap-box-node ib))
             (y (fheap-node-parent x)))
        (when (funcall lessp
                       (fheap-node-keyval x)
                       newkey)
          (error "after-decrease-key-fheap: key of ~s has been increased"
                 (fheap-box-item ib)))        
        (setf (fheap-node-keyval x) newkey)
        (when (and y (funcall lessp
                              (fheap-node-keyval x)
                              (fheap-node-keyval y)))
          (cut x y)
          (cascading-cut y))
        (when (funcall lessp
                       (fheap-node-keyval x)
                       (fheap-node-keyval (fheap-minnode fh)))
          (setf (fheap-minnode fh) x))))
    ;;
    ib))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generic functions for the above priority queue implementations

(defun make-prique (;; :list | :binary | :binomial | :fibonacci
                    implementation
                    &key
                    (maxcount nil)      ; for binary heap
                    (lessp #'<)
                    (key #'identity))
  (ecase implementation
    (:list
     (make-lheap :lessp lessp :key key))
    (:binary
     (unless (typep maxcount '(integer 0 *))
       (error "make-prique(binary): maxcount=~s must be a non-negative integer"
              maxcount))
     (make-biheap maxcount :lessp lessp :key key))
    (:binomial
     (make-bnheap :lessp lessp :key key))
    (:fibonacci
     (make-fheap :lessp lessp :key key))))

(defmethod prique-empty-p ((q lheap))
  (lheap-empty-p q))
(defmethod prique-empty-p ((q biheap))
  (biheap-empty-p q))
(defmethod prique-empty-p ((q bnheap))
  (bnheap-empty-p q))
(defmethod prique-empty-p ((q fheap))
  (fheap-empty-p q))

(defmethod prique-box-item ((q lheap-box))
  (lheap-box-item q))
(defmethod prique-box-item ((q biheap-box))
  (biheap-box-item q))
(defmethod prique-box-item ((q bnheap-box))
  (bnheap-box-item q))
(defmethod prique-box-item ((q fheap-box))
  (fheap-box-item q))

(defsetf prique-box-item (ib) (k)
  `(setf (slot-value ,ib 'item) ,k))

(defmethod insert-prique ((q lheap) item)
  (insert-lheap q item))
(defmethod insert-prique ((q biheap) item)
  (insert-biheap q item))
(defmethod insert-prique ((q bnheap) item)
  (insert-bnheap q item))
(defmethod insert-prique ((q fheap) item)
  (insert-fheap q item))

(defmethod find-min-prique ((q lheap))
  (find-min-lheap q))
(defmethod find-min-prique ((q biheap))
  (find-min-biheap q))
(defmethod find-min-prique ((q bnheap))
  (find-min-bnheap q))
(defmethod find-min-prique ((q fheap))
  (find-min-fheap q))

(defmethod delete-min-prique ((q lheap))
  (delete-min-lheap q))
(defmethod delete-min-prique ((q biheap))
  (delete-min-biheap q))
(defmethod delete-min-prique ((q bnheap))
  (delete-min-bnheap q))
(defmethod delete-min-prique ((q fheap))
  (delete-min-fheap q))

(defmethod union-prique ((q1 lheap) (q2 lheap))
  (union-lheap q1 q2))
(defmethod union-prique ((q1 biheap) (q2 biheap))
  (union-biheap q1 q2))
(defmethod union-prique ((q1 bnheap) (q2 biheap))
  (union-bnheap q1 q2))
(defmethod union-prique ((q1 fheap) (q2 biheap))
  (union-fheap q1 q2))

(defmethod after-decrease-key-prique ((q lheap) (ib lheap-box))
  (after-decrease-key-lheap q ib))
(defmethod after-decrease-key-prique ((q biheap) (ib biheap-box))
  (after-decrease-key-biheap q ib))
(defmethod after-decrease-key-prique ((q bnheap) (ib bnheap-box))
  (after-decrease-key-bnheap q ib))
(defmethod after-decrease-key-prique ((q fheap) (ib fheap-box))
  (after-decrease-key-fheap q ib))

    
(defun test-prique (implementation &key (size 10000))
  ;; insert
  ;; deletemin
  ;; decrease
  ;; union
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (setq *random-state* (make-random-state t #xABE))
  (format t "size = ~d ... " size)
  (gc t)
  (time
   (let ((rnums (loop repeat size collect (random most-positive-fixnum)))
         (q (make-prique implementation :maxcount size)))
     (loop
         for np on rnums
         for n = (car np)
         do (let ((ib (insert-prique q n)))
              ;; Sometimes decrease
              (when (zerop (random 10))
                (let ((d (random 1000)))
                  (decf (car np) d)
                  (decf (prique-box-item ib) d)
                  (after-decrease-key-prique q ib)))))
     ;;
     (let ((nums (loop while (not (prique-empty-p q))
                     collect (delete-min-prique q))))
       (cond
        ((equal nums (sort rnums #'<))
         (format t "OK !~%"))
        (t
         (format t "NG...~%")))
       ;;
       (values)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tree formatter

(defun format-tree (sm tree childfun labelfun)
  ;; Format TREE of the abstract data type <Tree> defined as follows:
  ;;   CHILDFUN: <Tree> -> (<Tree>*)
  ;;   LABELFUN: <Tree> -> (<String:Node-Label> <String:Node-Info>*)
  (labels ((formataux (tree vls top)
             (let ((child (funcall childfun tree))
                   (label (funcall labelfun tree)))
               (cond
                (top
                 (format sm "~a" vls))
                (t
                 (format sm "~a+--" (subseq vls 0 (- (length vls) 3)))))
               (format sm "~a~%" (or (car label) ""))
               (dolist (s (cdr label))
                 (format sm "~a~a~a~%" vls (if child "| " "  ") s))
               (loop for as on child do
                     (formataux (car as)
                                (concatenate 'string
                                  vls (if (cdr as) "|  " "   "))
                                nil)))))
    (formataux tree "" t)
    ;;
    (values)))

(defun format-tree-demo ()
  ;; Prints the following tree using format-tree.
  ;; 
  ;; Node A
  ;; | a1
  ;; | a2
  ;; +--Node B
  ;; |  +--Node C
  ;; |       c1
  ;; |       c2
  ;; +--Node D
  ;;    | d1
  ;;    | d2
  ;;    +--Node E
  ;;         e1
  ;;
  (format-tree t
               '((a "a1" "a2")
                 ((b)
                  ((c "c1" "c2")))
                 ((d "d1" "d2")
                  ((e "e1"))))
               #'cdr
               #'(lambda (node)
                   (cons (format nil "Node ~a" (caar node))
                         (cdar node)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PQ

(defstruct (pq (:constructor make-pq-raw))
  list
  lessp)

(defmethod print-object ((pq pq) sm)
  (format sm "#<pq ~:s>" (pq-list pq)))

(defun make-pq (lessp)
  (make-pq-raw :lessp lessp))

(defun insert-pq (pq x)
  (push x (pq-list pq)))

(defun pq-empty-p (pq)
  (null (pq-list pq)))

(defun delete-min-pq (pq)
  (when (pq-empty-p pq)
    (error "delete-min-pq: Empty priority que"))
  (let* ((list (pq-list pq))
         (lessp (pq-lessp pq))
         (minx (car list)))
    (loop
        for x in (cdr list)
        do (when (funcall lessp x minx)
             (setq minx x)))
    (setf (pq-list pq)
      (delete minx (pq-list pq)
              :test #'eq
              :count 1))
    minx))
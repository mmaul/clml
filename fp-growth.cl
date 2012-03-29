;; FP-growth algorith
(in-package :association-rule)

(defclass fp-node ()
  ((key :initarg :key :accessor fp-key)
   (count :initarg :count :accessor fp-count)
   (branch :initform nil :accessor fp-branch)
   (parent :initarg :parent :accessor fp-parent)
   (tmp-mark :initform nil :accessor fp-tmp-mark)
   (tmp-count :initform nil :accessor fp-tmp-count)))

(defun make-header-table (items)
  (map 'vector #'(lambda (item)
		  (cons item (make-array 0 :fill-pointer t :adjustable t)))
       items))

(defun get-header-table-array (item header-table &key (test #'equal))
  (loop for (key . array) across header-table
      when (funcall test key item)
	return array))

(defun make-fp-node (parent key header-table-array &optional (count 0))
  (let ((new (make-instance 'fp-node
	       :key key :parent parent :count count)))
    (push new (fp-branch parent))
    (vector-push-extend new header-table-array)
    new))

(defun fp-growth-make (node items header-table &key (test #'equal))
  (loop for item across items
      for next-node = (find-if #'(lambda (x) (funcall test (fp-key x) item))
                               (fp-branch node)) do
	(unless next-node
	  (let ((new-node (make-fp-node node item
					(get-header-table-array item header-table))))
	    (setf next-node new-node)))
	(setf node next-node)
	(incf (fp-count next-node))))

;; fp-countup
;; count step of FP-growth
;; signal count from leaf to root
(defun fp-countup (from-pos header-table tmp-mark)
  ;; first call signals count
  (loop for node across (cdr (aref header-table from-pos))
      for count = (fp-count node)
      when count do
	(let ((parent (fp-parent node)))
	  (if (eq tmp-mark (fp-tmp-mark parent))
	      (incf (fp-tmp-count parent) (fp-count node))
	    (setf (fp-tmp-count parent) (fp-count node)
		  (fp-tmp-mark parent) tmp-mark))))
  ;; from second call, signals tmp-count
  (loop for pos from (1- from-pos) downto 0
      for header-table-array = (cdr (aref header-table pos)) do
	(loop for node across header-table-array
	    when (eq tmp-mark (fp-tmp-mark node)) do
	      (let ((parent (fp-parent node)))
		(if (eq tmp-mark (fp-tmp-mark parent))
		    (incf (fp-tmp-count parent) (fp-tmp-count node))
		  (setf (fp-tmp-count parent) (fp-tmp-count node)
			(fp-tmp-mark parent) tmp-mark))))))

;; fp-gennext
;; generate next fp-part-tree
(defun fp-gennext (old-node new-node new-header-table tmp-mark)
  (loop for child in (fp-branch old-node)
      for count = (fp-tmp-count child)
      when (eq (fp-tmp-mark child) tmp-mark) do
	(let* ((key (fp-key child))
	       (header-table-array (get-header-table-array key new-header-table)))
	  (when header-table-array
	    (fp-gennext child (make-fp-node new-node key header-table-array count)
			new-header-table tmp-mark)))))

(defun gen-next-header-table (old-header-table new-header-table-size)
  (let ((new-header-table (make-array new-header-table-size)))
    (loop for i from 0 to (1- new-header-table-size) do
	  (setf (aref new-header-table i) (cons (car (aref old-header-table i))
						(make-array 0 :fill-pointer t :adjustable t))))
    new-header-table))

(defun fp-growth (node header-table length trie minimum-count &optional making-itemset)
  (loop for i from (1- (length header-table)) downto 0
      for key = (car (aref header-table i))
      for tmp-mark = (gensym) do
	(fp-countup i header-table tmp-mark)
	(when (eq tmp-mark (fp-tmp-mark node))
	  (let ((itemset-count (fp-tmp-count node)))
	    (when (and itemset-count (>= itemset-count minimum-count))
	      ;; update trie
	      (let ((new-itemset (cons key making-itemset)))
		(update-list-trie trie new-itemset itemset-count)
		;; growth next fp-part-tree
		(unless (or (zerop i) (= length 1))
		  (let ((new-root (make-instance 'fp-node :key nil :count itemset-count))
			(new-header-table (gen-next-header-table header-table i)))
		    (fp-gennext node new-root new-header-table tmp-mark)
		    (fp-growth new-root new-header-table (1- length) trie minimum-count
			       new-itemset)))))))))

;; trie branch as -- (key value . branches)
(defun update-list-trie (trie itemset count)
  (loop for key in itemset do
	(let* ((insert (cdr trie))
	       (branches (cdr insert))
	       (found (find key branches :key #'car :test #'equal)))
	  (if found
	      (setf trie found)
	    (let ((new-node (list key count)))
	      (setf (cdr insert) (cons new-node branches))
	      (setf trie new-node)))))
  (setf (cadr trie) count)
  count)

(defun lookup-count-from-trie (itemset trie)
  (loop for key in itemset do
	(setf trie (find key (cddr trie) :key #'car :test #'equal)))
  (cadr trie))

(defun scan-input-data-fp-growth (labeled-dataset target-variables key-variable rule-length support)
  (let ((rule-counter (make-hash-table :test #'equal))
	(target-length (length target-variables))
	(new-labels (cons key-variable target-variables)))
    (flet ((update-dic (data)
	     (loop for i from 1 to target-length
		 for label in target-variables
		 for x = (aref data i)
		 unless (consp x) do
		   (let* ((rule (cons label x))
			  (found (gethash rule rule-counter (cons rule 0))))
		     (incf (cdr found))
		     (setf (gethash rule rule-counter) found)
		     (setf (aref data i) (car found))))))
      (let ((vecs (sort (choice-dimensions new-labels labeled-dataset)
			#'string< :key #'(lambda (x) (update-dic x) (aref x 0)))) ;; key compare predicate?
	    (rule-order (make-hash-table :test #'eq))
	    (order 0)
	    (keys nil))
	;; build up rule-order
	(maphash #'(lambda (k v)
		     (declare (ignore v))
		     (push k keys)) rule-counter)
	(setf keys (sort keys #'> :key #'(lambda (key) (cdr (gethash key rule-counter)))))
	(loop for key in keys do
	      (setf (gethash key rule-order) (incf order)))
	(let* ((tmp (make-array 0 :fill-pointer t :adjustable t))
	       (tmp-key nil)
	       (count 0)
	       (header-table (make-header-table keys))
	       (root-node (make-instance 'fp-node :key nil :count 0)))
	  (do-vec (v vecs)
	    (let ((key (aref v 0)))
	      (unless (equal tmp-key key) ;; key equation predicate?
		(fp-growth-make root-node
				(sort tmp #'< :key #'(lambda (y) ;; some normal-order
						       (gethash y rule-order)))
				header-table)
		(setf tmp-key key)
		(setf (fill-pointer tmp) 0)
		(incf count))
	      (loop for i from 1 to target-length
		  for rule = (aref v i) do
		    (unless (find rule tmp :test #'equal)
		      (vector-push-extend rule tmp)))))
	    ;; final call
	    (fp-growth-make root-node
			    (sort tmp #'< :key #'(lambda (y) ;; some normal-order
						   (gethash y rule-order)))
			    header-table)
	    (let ((root-trie (list nil 0)))
	      ;; fp-tree into trie
	      (fp-growth root-node header-table rule-length root-trie (* count (/ support 100.0)))
	      (values root-trie count)))))))

(defun map-trie (trie fn length &optional (passed (make-stack length)))
  (labels ((iter (root fn length passed)
	     (unless (zerop length)
	       (vector-push (car root) passed)
	       (loop for branch in (cddr root) do
		     (iter branch fn (1- length) passed))
	       (funcall fn passed (cadr root))
	       (decf (fill-pointer passed))
	       )))
    (loop for branch in (cddr trie) do
	  (iter branch fn length passed))))

(defun %association-analyze-fp-growth (labeled-dataset target-variables key-variable rule-length
				       &key (support 0) (confident 0) (lift 0) (conviction 0))
  (assert (and (<= 0 support 100) (<= 0 confident 100) (<= 0 lift) (<= 0 conviction)))
  (assert (and (integerp rule-length) (<= 2 rule-length)))
  (multiple-value-bind (trie total-count)
      (scan-input-data-fp-growth labeled-dataset target-variables key-variable
				 rule-length support)
    (let ((ans nil))
      (map-trie trie
       #'(lambda (rule rule-count)
	   (let ((rule-length (length rule)))
	     (when (> rule-length 1)
	       (ap-maprule-da
		#'(lambda (conc pre conc-count pre-count)
		    (unless (or (zerop conc-count) (zerop pre-count))
		      (multiple-value-bind (sup conf lif conv)
			  (rule-indexes-da conc-count pre-count total-count rule-count)
			(when (and (>= sup support) (>= conf confident) (>= lif lift) (>= conv conviction))
			  (push (make-rule conc pre sup conf lif conv) ans)))))
		rule rule-length
		#'(lambda (itemset)
		    (lookup-count-from-trie itemset trie))
		(confident->max-precount rule-count confident) #'equal))))
       rule-length)
      (make-assoc-result ans support confident lift conviction rule-length))))
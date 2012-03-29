;; Occurence Deliver
(in-package :association-rule)

(defun occurence-deliver (sub-transactions rest-keys)
  (let ((next-transactions (mapcar #'(lambda (key)
				       (cons key (cons 0 nil))) rest-keys)))
    (loop for tran in sub-transactions do
	  (loop for (key . alist) in next-transactions
	      while tran
	      when (eql (car tran) key) do
		(push (cdr tran) (cdr alist))
		(incf (car alist))
		(setf tran (cdr tran))))
    next-transactions))


(defun scan-lcm (trie transactions length rest-keys minimum-count)
  (let ((next-transactions (occurence-deliver transactions rest-keys)))
    (loop for keys on rest-keys
	for (key occur . sub-trans) in next-transactions
	when (>= occur minimum-count) do
	  (let ((new-node (update-and-walk-trie trie key occur)))
	    (unless (= length 1)
	      (scan-lcm new-node sub-trans (1- length) (cdr keys) minimum-count))))))  

(defun scan-input-data-lcm (labeled-dataset target-variables key-variable rule-length support)
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
	(let ((transactions nil)
	      (tmp nil)
	      (tmp-key nil)
	      (count 0))
	  (do-vec (v vecs)
	    (let ((key (aref v 0)))
	      (unless (equal tmp-key key) ;; key equation predicate?
		(push (sort tmp #'< :key #'(lambda (y) ;; some normal-order
					     (gethash y rule-order)))
		      transactions)
		(setf tmp-key key)
		(setf tmp nil)
		(incf count))
	      (loop for i from 1 to target-length
		  for rule = (aref v i) do
		    (pushnew rule tmp :test #'equal))))
	  ;; final call
	  (push (sort tmp #'< :key #'(lambda (y) ;; some normal-order
				       (gethash y rule-order)))
		transactions)
	  (let ((root-trie (list nil 0)))
	    ;; lcm scan into trie
	    (scan-lcm root-trie transactions rule-length keys (max 1 (* count (/ support 100.0))))
	    (values root-trie count)))))))
  
(defun %association-analyze-lcm (labeled-dataset target-variables key-variable rule-length
				       &key (support 0) (confident 0) (lift 0) (conviction 0))
  (assert (and (<= 0 support 100) (<= 0 confident 100) (<= 0 lift) (<= 0 conviction)))
  (assert (and (integerp rule-length) (<= 2 rule-length)))
  (multiple-value-bind (trie total-count)
      (scan-input-data-lcm labeled-dataset target-variables key-variable
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
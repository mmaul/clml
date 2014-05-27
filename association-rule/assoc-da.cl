;; use dobule array logic for association-rule
;; but not required check space -- so there is only base array
;; so that it called "single array"
(in-package :association-rule)

(defun dim-of (size length)
  (1+ (* 2
	 (loop with ans = 0
             with tmp = 1
             for l from 1 to length
             for s from size downto 1 do
	       (setf tmp (/ (* tmp s) l))
	       (incf ans tmp)
             finally (return ans)))))

;; build single array
;; base slot of leaf == leaf count
(defun make-assoc-sa (size length)
  (let* ((dim (dim-of size length))
         (base (make-array dim :element-type 'fixnum :initial-element 0))
         (target (1+ size)))
    ;; root has no leaf
    (labels ((build-sa-iter (base from-ref target-start code-from code-max l)
               (if (> l 1)
                   (setf (aref base from-ref) (- target-start code-from))
                 (setf (aref base from-ref) (- target-start code-max)))
               (if (> l 1)
                   (loop with start = (+ target-start code-max (- code-from) 1)
		       for code from code-from upto (1- code-max) do
                         (setf start
			   (build-sa-iter base 
					  (+ target-start code (- code-from))
					  start (1+ code) code-max (1- l)))
		       finally (return start))
                 (+ target-start 1))))
      (loop for i from 1 to size do
	    (setf target (build-sa-iter base i target (1+ i) (1+ size) length))))
    base))

(defun search-next (base ref code)
  (+ (aref base ref) code))

(defun inc-rule-pattern1 (rule length base da-pos code-table rule-pos rule-max leaf-code)
  (incf (aref base (search-next base da-pos leaf-code)))
  (unless (or (zerop length) (> rule-pos rule-max))
    (loop for ref from rule-pos to rule-max
          for key = (aref rule ref) do
          (inc-rule-pattern1 rule (1- length) base (search-next base da-pos (gethash key code-table))
                             code-table (1+ ref) rule-max leaf-code))))

(defun inc-rule-pattern-da (rule length base code-table leaf-code)
  (let ((l (1- (length rule))))
    (loop for i from 0 to l
          for key = (aref rule i) do
          (inc-rule-pattern1 rule (1- length) base (search-next base 0 (gethash key code-table))
                             code-table (1+ i) l leaf-code))))

(defun scan-input-data-da (labeled-dataset target-variables key-variable rule-length)
  (let ((rule-order (make-hash-table :test #'equal)) ;; rule compare predicate available?
	(order 0)
	(target-length (length target-variables))
	(new-labels (cons key-variable target-variables)))
    (flet ((update-dic (data)
	     (loop for i from 1 to target-length
		 for label in target-variables
		 for x = (aref data i)
		 unless (consp x) do
		   (let* ((rule (cons label x))
			  (found (gethash rule rule-order)))
		     (unless found
		       (setf (gethash rule rule-order) (incf order)))
		     (setf (aref data i) rule)))))
      (let* ((vecs (sort (choice-dimensions new-labels labeled-dataset)
			 #'string< :key #'(lambda (x) (update-dic x) (aref x 0)))) ;; key compare predicate?
	     (tmp (make-array 0 :fill-pointer t :adjustable t))
	     (tmp-key nil)
	     (count 0)
	     (size (hash-table-count rule-order))
	     (leaf-code (1+ size)))
	(let ((base
	       (make-assoc-sa size rule-length)))
	  (do-vec (v vecs)
	    (let ((key (aref v 0)))
	      (unless (equal tmp-key key) ;; key equation predicate?
		(inc-rule-pattern-da (sort tmp #'< :key #'(lambda (y) ;; some normal-order
							    (gethash y rule-order)))
				     rule-length base rule-order leaf-code)
		(setf tmp-key key)
		(setf (fill-pointer tmp) 0)
		(incf count))
	      (loop for i from 1 to target-length
		  for rule = (aref v i) do
		    (unless (find rule tmp :test #'equal)
		      (vector-push-extend rule tmp)))))
	  ;; final call
	  (inc-rule-pattern-da (sort tmp #'< :key #'(lambda (y) ;; some normal-order
						      (gethash y rule-order)))
			       rule-length base rule-order leaf-code)
	  (values base count (let ((array (make-array leaf-code)))
			       (maphash #'(lambda (k v)
					    (setf (aref array v) k)) rule-order)
			       array)))))))

(defun get-count (base ref leaf-code)
  (aref base (+ (aref base ref) leaf-code)))

(defun map-trie-da (base fn length da-pos decode-pos vocablary minimum-count &optional (passed (make-stack length)))
  (unless (zerop length)
    (loop for index from decode-pos to vocablary
	for next = (search-next base da-pos index)
	for count = (get-count base next (1+ vocablary)) do
	  (when (and next (>= count minimum-count)) ;; add cut by support -- referenced from apriori algorithm
	    (vector-push index passed)
	    (map-trie-da base fn (1- length) next (1+ index) vocablary minimum-count passed)
	    (funcall fn passed count)
	    (decf (fill-pointer passed))))))

(defun map-separated-two-groups-with-search-da
    (base bag fn passed-1 passed-2 da-pos1 da-pos2 leaf-code &optional (pos 0))
  (if (= pos (length bag))
      (unless (or (zerop (length passed-1)) (zerop (length passed-2)))
	(funcall fn passed-1 passed-2
		 (get-count base da-pos1 leaf-code)
		 (get-count base da-pos2 leaf-code)))
    (let ((element (aref bag pos)))
      (vector-push element passed-1)
      (map-separated-two-groups-with-search-da
       base bag fn passed-1 passed-2
       (search-next base da-pos1 element) da-pos2 leaf-code (1+ pos))
      (decf (fill-pointer passed-1)) ;; cleanup tail!
      (vector-push element passed-2)
      (map-separated-two-groups-with-search-da
       base bag fn passed-1 passed-2
       da-pos1 (search-next base da-pos2 element) leaf-code (1+ pos))
      (decf (fill-pointer passed-2)) ;; cleanup tail!
      )))


(defun finalize-rule-da (index decode-array)
  (symbol-macrolet ((atom-rule (aref decode-array index))) ;; optional -- for visualize
    (format nil "~A=~A" (car atom-rule) (cdr atom-rule))))
(defun finalize-rules-da (rule decode-array)
  (map 'list #'(lambda (x) (finalize-rule-da x decode-array)) rule))
(defun make-rule-da (conc pre sup conf lif conv decode-array)
  (make-array 6 :initial-contents (list (finalize-rules-da pre decode-array)
                                        (finalize-rules-da conc decode-array)
                                        sup conf lif conv)))

(defun %association-analyze-da (labeled-dataset target-variables key-variable rule-length
				&key (support 0) (confident 0) (lift 0) (conviction 0))
  (assert (and (<= 0 support 100) (<= 0 confident 100) (<= 0 lift) (<= 0 conviction)))
  (assert (and (integerp rule-length) (<= 2 rule-length)))
  (multiple-value-bind (base total-count decode-array)
      (scan-input-data-da labeled-dataset target-variables key-variable rule-length)
    (let* ((ans nil)
	   (s1 (make-stack rule-length))
	   (s2 (make-stack rule-length))
	   (leaf-code (length decode-array))
	   (vocablary (1- leaf-code)))
      (map-trie-da
       base
       #'(lambda (rule rule-count)
	   (map-separated-two-groups-with-search-da
	    base rule
	    #'(lambda (conc pre conc-count pre-count)
		(unless (or (zerop conc-count) (zerop pre-count))
		  (multiple-value-bind (sup conf lif conv)
		      (rule-indexes-da conc-count pre-count total-count rule-count)
		    (when (and (>= sup support) (>= conf confident) (>= lif lift) (>= conv conviction))
		      (push (make-rule-da conc pre sup conf lif conv decode-array) ans)))))
            s1 s2 0 0 leaf-code))
       rule-length 0 1 vocablary (max 1 (* total-count (/ support 100.0))))
      (make-assoc-result ans support confident lift conviction rule-length))))

(defun make-stack (length)
  (make-array length :fill-pointer 0))

(defun rule-indexes-da (conc-count pre-count total rule-count)
  (values (* (/ rule-count total) 100.0) ;; support
	  (* (/ rule-count pre-count) 100.0) ;; confident
	  (float (/ (/ rule-count pre-count) (/ conc-count total))) ;; lift
	  (let ((negative (- pre-count rule-count)))
	    (if (zerop negative)
		#.most-positive-single-float
	      (float (/ (* pre-count (- 1 (/ conc-count total))) (- pre-count rule-count))))) ;; conviction
	  ))

;; parent-itemset: array of double array references
(defun ap-maprule-da (fn parent-itemset itemset-length count-lookup-fn max-precount
		      &optional (test #'eql)
				(set-of-itemsets (map 'list #'(lambda (x) (list x)) parent-itemset))
				(set-of-itemsets-length 1))
  (dolist (conc set-of-itemsets)
    (let* ((pre (ordered-set-difference-da parent-itemset conc :test test))
	   (pre-count (funcall count-lookup-fn pre)))
      (if (<= pre-count max-precount)
	  (funcall fn conc pre (funcall count-lookup-fn conc) pre-count)
	(setf set-of-itemsets (delete conc set-of-itemsets)))))
  (when (and (> itemset-length (1+ set-of-itemsets-length)) set-of-itemsets)
    (ap-maprule-da fn parent-itemset itemset-length count-lookup-fn max-precount test
		   (gen-next-itemsets set-of-itemsets) (1+ set-of-itemsets-length))))

(defun ordered-set-difference-da (sorted-item-array sorted-item-list &key (test #'eql))
  (loop for item across sorted-item-array
      as search-result = (member item sorted-item-list :test test)
      unless search-result
      collect item))

(defun lookup-count-by-ref-list (reference-list base leaf-code)
  (let ((ref 0))
    (loop for code in reference-list do
	  (setf ref (search-next base ref code)))
    (get-count base ref leaf-code)))

(defun %association-analyze-da-ap-genrule (labeled-dataset target-variables key-variable rule-length
					   &key (support 0) (confident 0) (lift 0) (conviction 0))
  (assert (and (<= 0 support 100) (<= 0 confident 100) (<= 0 lift) (<= 0 conviction)))
  (assert (and (integerp rule-length) (<= 2 rule-length)))
  (multiple-value-bind (base total-count decode-array)
      (scan-input-data-da labeled-dataset target-variables key-variable rule-length)
    (let* ((ans nil)
	   (leaf-code (length decode-array))
	   (vocablary (1- leaf-code)))
      (map-trie-da
       base
       #'(lambda (rule rule-count)
	   (let ((rule-length (length rule)))
	     (when (> rule-length 1)
	       (ap-maprule-da
		#'(lambda (conc pre conc-count pre-count)
		    (unless (or (zerop conc-count) (zerop pre-count))
		      (multiple-value-bind (sup conf lif conv)
			  (rule-indexes-da conc-count pre-count total-count rule-count)
			(when (and (>= sup support) (>= conf confident) (>= lif lift) (>= conv conviction))
			  (push (make-rule-da conc pre sup conf lif conv decode-array) ans)))))
		rule rule-length
		#'(lambda (itemset)
		    (lookup-count-by-ref-list itemset base leaf-code))
		(confident->max-precount rule-count confident)))))
       rule-length 0 1 vocablary (max 1 (* total-count (/ support 100.0))))
      (make-assoc-result ans support confident lift conviction rule-length))))

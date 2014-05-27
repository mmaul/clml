(defpackage :random-forest
  (:use :cl
	:hjs.learn.read-data
	:decision-tree)
  (:import-from :decision-tree 
		#:make-variable-index-hash
		#:sum-up
		#:column-name->column-number
		#:total
		#:make-split-predicate
		#:sum-up-results
		#:gini-index
		#:entropy
		#:mean
		#:variance
		#:whole-row-numbers-list
		#:split
		#:delta-gini
		#:delta-entropy
		#:delta-variance)
  (:export  
   #:make-random-forest
   #:make-regression-forest
   #:predict-forest
   #:importance
   #:predict-regression-forest
   #:forest-validation
   #:regression-forest-validation))

(in-package :random-forest)

(defun make-bootstrap-sample (unspecialized-dataset)
  (let* ((data-vector (dataset-points unspecialized-dataset))
	 (n (array-dimension data-vector 0))
	 (new-data-vector (make-array n)))
    
    (loop
	for i below n 
	do (setf (svref new-data-vector i) (svref data-vector (random n))))
    new-data-vector))

(defun make-explanatory-variable-index-list (variable-index-hash objective-column-index)
  (let* ((n (hash-table-count variable-index-hash))
	 (m (floor (sqrt n)))
	 (all-var-index-list (loop for i below n collect i))
	 (ex-var-index-list (remove objective-column-index all-var-index-list))
	 (sample-number-list (algorithm-s m (1- n))))
    (loop
	for i in sample-number-list
	collect (nth i ex-var-index-list))))

(defun algorithm-s (n max)
  "Knuth's random sampling algorithm."
  (loop 
      for seen from 0
      when (< (* (- max seen) (random 1.0)) n)
      collect seen and do (decf n)
      until (zerop n)))
  
(defun make-split-criterion-list-for-rf (data-vector variable-index-hash objective-column-index)
  (let ((explanatory-variable-index-list (make-explanatory-variable-index-list variable-index-hash objective-column-index)))
    (loop with split-criterion-list = '()
	for var-name being the hash-keys in variable-index-hash
	using (hash-value j)
	when (member j explanatory-variable-index-list) do
	  (let* ((v (loop for line across (the simple-array data-vector)
			collect (svref line j)))
		 (w (remove-duplicates v))) ;remark
	    ;;(assert (<= 2 (length w)))
	    (if (= (length w) 2)
		(push (cons var-name (car w)) split-criterion-list)
	      (dolist (attribute w)
		(push (cons var-name attribute) split-criterion-list))))
	finally (return split-criterion-list))))

(defun select-best-splitting-attribute-for-rf (data-vector variable-index-hash
					       list-of-row-numbers split-criterion-list
					       objective-column-index &key (test #'delta-gini) (epsilon 0))
  
  (let* ((v (mapcar #'(lambda (x) (list x (funcall test data-vector variable-index-hash list-of-row-numbers (car x) (cdr x) objective-column-index)))
		    split-criterion-list))
	 
	 (w (reduce #'(lambda (x y) (if (<= (second x) (second y))
	 				y
	 			      x)) v)))
    (if (<= (second w) epsilon)
	(values nil '())
      (values (car w) (* (length list-of-row-numbers) (second w))))))

(defun make-root-node-for-rf (data-vector variable-index-hash objective-column-index column-list &key (test #'delta-gini) (epsilon 0))
  
  (let ((initial-row-numbers-list (whole-row-numbers-list data-vector)))

    (multiple-value-bind (best-split-criterion split-criterion-list)
	(select-best-splitting-attribute-for-rf
	 data-vector variable-index-hash initial-row-numbers-list
	 (make-split-criterion-list-for-rf data-vector variable-index-hash objective-column-index) objective-column-index :test test :epsilon epsilon)
    
      (let ((result-ratio (sum-up-results data-vector initial-row-numbers-list objective-column-index)))
					    					      
	(multiple-value-bind (right left) (split data-vector variable-index-hash initial-row-numbers-list
						 (car best-split-criterion) (cdr best-split-criterion))
	     
	  (list (list best-split-criterion split-criterion-list)
		result-ratio
		(list right left)
		variable-index-hash
		objective-column-index
		column-list
		))))))

(defun make-new-right-node-for-rf (data-vector variable-index-hash objective-column-index tree-node 
				   &key (test #'delta-gini) (epsilon 0))
  (if (null (caar tree-node))
      '()
    (let ((right-low-numbers-list (first (third tree-node))))
      
      (multiple-value-bind (best-split-criterion split-criterion-list)
	  (select-best-splitting-attribute-for-rf
	   data-vector variable-index-hash right-low-numbers-list 
	   (make-split-criterion-list-for-rf data-vector variable-index-hash objective-column-index)
	   objective-column-index :test test :epsilon epsilon)
	
	(let ((result-ratio (sum-up-results data-vector right-low-numbers-list
					    objective-column-index)))
	  (multiple-value-bind (right left) (split data-vector variable-index-hash right-low-numbers-list
						   (car best-split-criterion) (cdr best-split-criterion))
	   
	    (list (list best-split-criterion split-criterion-list)
		  result-ratio
		  (list right left))))))))
	 	
(defun make-new-left-node-for-rf (data-vector variable-index-hash objective-column-index tree-node 
			    &key (test #'delta-gini) (epsilon 0))
  (if (null (caar tree-node))
      '()
    (let ((left-low-numbers-list (second (third tree-node))))
      
      (multiple-value-bind (best-split-criterion split-criterion-list)
	  (select-best-splitting-attribute-for-rf
	   data-vector variable-index-hash left-low-numbers-list 
	   (make-split-criterion-list-for-rf data-vector variable-index-hash objective-column-index)
	   objective-column-index :test test :epsilon epsilon)

	(let ((result-ratio (sum-up-results data-vector left-low-numbers-list
					       objective-column-index)))
	  (multiple-value-bind (right left) (split data-vector variable-index-hash left-low-numbers-list
						 (car best-split-criterion) (cdr best-split-criterion))
	    
	    (list (list best-split-criterion split-criterion-list)
		result-ratio
		(list right left))))))))

(defun make-decision-tree-for-rf (data-vector variable-index-hash objective-column-index tree-node
			   &key (test #'delta-gini) (epsilon 0))
  (if (null (caar tree-node))
      (list (second tree-node) (car (third tree-node)))
    (list tree-node
	  (make-decision-tree-for-rf data-vector variable-index-hash objective-column-index
			      (make-new-right-node-for-rf data-vector variable-index-hash objective-column-index
						   tree-node :test test :epsilon epsilon)
			      :test test :epsilon epsilon)
	  (make-decision-tree-for-rf data-vector variable-index-hash objective-column-index
			      (make-new-left-node-for-rf data-vector variable-index-hash objective-column-index
						  tree-node :test test :epsilon epsilon)
			      :test test :epsilon epsilon))))

(defun print-random-decision-tree (unspecialized-dataset objective-column-name &key (test #'delta-gini) (stream t))
  "for test"
  (let ((tree (make-random-decision-tree unspecialized-dataset objective-column-name :test test)))
    (print-decision-tree tree stream)))

(defun make-random-decision-tree (unspecialized-dataset objective-column-name &key (test #'delta-gini))
  (let* ((data-vector (make-bootstrap-sample unspecialized-dataset))
	 (variable-index-hash (make-variable-index-hash unspecialized-dataset))
	 (objective-column-index (column-name->column-number variable-index-hash objective-column-name))
	 (column-list (loop
			  with dim-vector = (dataset-dimensions unspecialized-dataset)
			  for i below (length dim-vector)
			  if (/= i objective-column-index)
			  collect (dimension-name (aref dim-vector i))))
	 (root (make-root-node-for-rf data-vector variable-index-hash objective-column-index column-list
			       :test test)))
    (make-decision-tree-for-rf data-vector variable-index-hash objective-column-index root :test test)))

#-fork-future
(defun make-random-forest (unspecialized-dataset objective-column-name &key (test #'delta-gini) (tree-number 500))
  (let ((forest (make-array tree-number)))
    (dotimes (i tree-number forest)
	     (setf (svref forest i) (make-random-decision-tree unspecialized-dataset objective-column-name :test test)))))

#+fork-future
(defun make-random-forest (unspecialized-dataset objective-column-name &key (test #'delta-gini) (tree-number 500))
  (let ((forest (make-array tree-number)))
    (let ((futures
           (loop for nworker below hjs.learn.vars:*workers*
                 collect
              (fork-future:future 
                (loop for i from nworker below tree-number by hjs.learn.vars:*workers*
                      do
                   (setf (svref forest i)
                         (make-random-decision-tree unspecialized-dataset objective-column-name :test test)))
                forest))))
      (mapc 'fork-future:touch futures)
      (loop for nworker below hjs.learn.vars:*workers*
            do  
         (loop for i from nworker below tree-number by hjs.learn.vars:*workers*
               do
            (setf (svref forest i)
                  (aref (fork-future:touch (elt futures nworker)) i)))))
    forest))

(defun predict-forest (query-vector unspecialized-dataset forest)
  (car (reduce #'(lambda (x y) (if (<= (cdr x) (cdr y))
				   y
				 x))
	       (sum-up (loop
			   for i below (length forest)
			   collect (predict-decision-tree query-vector unspecialized-dataset (svref forest i)))))))

(defun forest-validation (validation-dataset objective-column-name forest)
  (let* ((variable-index-hash (make-variable-index-hash validation-dataset))
	 (k (column-name->column-number variable-index-hash objective-column-name))
	(validation-data-vector (dataset-points validation-dataset)))
    
    (sum-up (loop
		 for i below (length validation-data-vector)
		 collect (cons (predict-forest (svref validation-data-vector i) validation-dataset forest)
			       (svref (svref validation-data-vector i) k))))))



(defun make-random-regression-tree (unspecialized-dataset objective-column-name)
  (let* ((data-vector (make-bootstrap-sample unspecialized-dataset))
	 (variable-index-hash (make-variable-index-hash unspecialized-dataset))
	 (objective-column-index (column-name->column-number variable-index-hash objective-column-name))
	 (column-list (loop
			  with dim-vector = (dataset-dimensions unspecialized-dataset)
			  for i below (length dim-vector)
			  if (/= i objective-column-index)
			  collect (dimension-name (aref dim-vector i))))
	 (root (make-root-node-for-rf data-vector variable-index-hash objective-column-index column-list
				      :test #'delta-variance)))
    (make-regression-tree-for-rf data-vector variable-index-hash objective-column-index root :test #'delta-variance)))

(defun make-regression-tree-for-rf (data-vector variable-index-hash objective-column-index tree-node
				    &key (test #'delta-variance) (epsilon 0))
  (if (null (caar tree-node))
      (list (second tree-node) (car (third tree-node)))
    (list tree-node
	  (make-regression-tree-for-rf data-vector variable-index-hash objective-column-index
				       (make-new-right-node-for-rf data-vector variable-index-hash objective-column-index tree-node)
				       :test test :epsilon epsilon)
	  (make-regression-tree-for-rf data-vector variable-index-hash objective-column-index
				       (make-new-left-node-for-rf data-vector variable-index-hash objective-column-index tree-node)
				       :test test :epsilon epsilon))))

(defun print-random-regression-tree (unspecialized-dataset objective-column-name &key (stream t))
  "for test"
  (let ((tree (make-random-regression-tree unspecialized-dataset objective-column-name)))
    (print-regression-tree tree stream)))

#-fork-future
(defun make-regression-forest (unspecialized-dataset objective-column-name &key (tree-number 500))
  (let ((forest (make-array tree-number)))
    (dotimes (i tree-number forest)
      (setf (svref forest i) (make-random-regression-tree unspecialized-dataset objective-column-name)))))

#+fork-future
(defun make-regression-forest (unspecialized-dataset objective-column-name &key (tree-number 500))
  (let ((forest (make-array tree-number)))
    (let ((futures
           (loop for nworker below hjs.learn.vars:*workers*
               collect
                 (fork-future:future 
                  (loop for i from nworker below tree-number by hjs.learn.vars:*workers*
                      do
                        (setf (svref forest i)
                          (make-random-regression-tree unspecialized-dataset objective-column-name)))
                  forest))))
      (mapc 'fork-future:touch futures)
      (loop for nworker below hjs.learn.vars:*workers*
          do  
            (loop for i from nworker below tree-number by hjs.learn.vars:*workers*
                do
                  (setf (svref forest i)
                    (aref (fork-future:touch (elt futures nworker)) i)))))
    forest))

(defun predict-regression-forest (query-vector unspecialized-dataset forest)
 
  (/ (loop
	 for i below (length forest)
	 sum (predict-regression-tree query-vector unspecialized-dataset (svref forest i)))
     (length forest)))

(defun regression-forest-validation (validation-dataset objective-column-name regression-forest)
  (let* ((variable-index-hash (make-variable-index-hash validation-dataset))
	 (k (column-name->column-number variable-index-hash objective-column-name))
	 (validation-data-vector (dataset-points validation-dataset))
	 (n (length validation-data-vector)))
    
    (loop
	for i below n
	sum (expt (- (predict-regression-forest (svref validation-data-vector i) validation-dataset regression-forest) 
		     (svref (svref validation-data-vector i) k))
		  2) into s
	finally (return (/ s n)))))


(defun sum-up-decrease-gini (rf-tree column)
  (if (< 2 (length rf-tree))
      (let ((node-var (caaaar rf-tree))
	    (value (cadaar rf-tree)))
	(+ (if (string= column node-var)
	       value
	     0.0d0)
	   (sum-up-decrease-gini (second rf-tree) column)
	   (sum-up-decrease-gini (third rf-tree) column)))
    0.0d0))

(defun sum-up-var (rf-tree column)
  (if (< 2 (length rf-tree))
      (let ((node-var (caaaar rf-tree)))
	(+ (if (string= column node-var)
	       1
	     0)
	   (sum-up-var (second rf-tree) column)
	   (sum-up-var (third rf-tree) column)))
    0))

(defun importance (forest)
  (format t "~%")
  (loop
      with column-list = (nth 5 (first (aref forest 0)))
      for column in column-list
      as sum-gini = (loop
			for tree across forest
			sum (sum-up-decrease-gini tree column))
      do (format t "~a	~a~%" column (/ sum-gini (length forest)))))

(defun var-used (forest)
  (format t "~%")
  (loop
      with column-list = (nth 5 (first (aref forest 0)))
      for column in column-list
      as n = (loop
		 for tree across forest
		 sum (sum-up-var tree column))
      do (format t "~a	~a~%" column n)))

(defun count-tree-node (rf-tree)
  (if (>= 2 (length rf-tree))
      1
    (+ (count-tree-node (second rf-tree))
       (count-tree-node (third rf-tree)))))

(defun treesize (forest)
  (loop
      for tree across forest
      collect (count-tree-node tree)))


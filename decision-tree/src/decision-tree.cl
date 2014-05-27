;;;

(defpackage :decision-tree
  (:use :cl
	:hjs.learn.read-data
	:hjs.util.matrix)
  (:export  
   #:make-decision-tree
   #:make-regression-tree
   #:print-decision-tree
   #:print-regression-tree
   #:predict-decision-tree
   #:predict-regression-tree
   #:decision-tree-validation
   #:regression-tree-validation))
	    
(in-package :decision-tree)

(defun make-variable-index-hash (unspecialized-dataset)
  (let ((dim-vector (dataset-dimensions unspecialized-dataset)))
    (loop with variable-index-hash = (make-hash-table :test #'equal)
	for i below (length dim-vector)
	do (setf (gethash (dimension-name (svref dim-vector i)) variable-index-hash) i)
	finally (return variable-index-hash))))

(defun sum-up (lst)
  (loop with alist
      for obj in lst
      as sub-alist = (assoc obj alist :test #'equal)
      do (if sub-alist
	     (incf (cdr sub-alist))
	   (push (cons obj 1) alist))
      finally (return alist)))

(defun column-name->column-number (variable-index-hash column-name)
  (multiple-value-bind (column-number flag)
      (gethash column-name variable-index-hash)
    (if (null flag)
	(error "such column-name does not exist.")
      column-number)))

(defun total (sum-up-results-list)
  (loop 
      for obj in sum-up-results-list
      sum (cdr obj)))
  
(defun make-split-predicate (attribute &optional optimize)
  ;; make-split-predicate returns optimized function
  ;; if the optional data type is double-float
  (if (and optimize (floatp attribute))
      #'(lambda (x)
          (declare (optimize speed (safety 0) (debug 0))
                   (type double-float attribute x))
          (<= attribute x))
    (if (realp attribute)
        #'(lambda (x) (<= attribute x))
      #'(lambda (x) (equal attribute x)))))

(defun sum-up-results (data-vector list-of-row-numbers objective-column-index)
  (declare (optimize speed (debug 0) (safety 0))
           (type fixnum objective-column-index))
  (loop with alist = '()
      for i fixnum in list-of-row-numbers
      for data = (svref (svref data-vector i) objective-column-index)
      as sub-alist = (assoc data alist :test #'equal)
      do (if sub-alist
             (incf (cdr sub-alist))
           (push (cons data 1) alist))
      finally (return alist)))

(defun gini-index (sum-up-results-list)
  (let ((p (total sum-up-results-list)))
    (- 1.0d0 
       (loop
	   for obj in sum-up-results-list
	   sum (expt (/ (cdr obj) p) 2)))))

(defun entropy (sum-up-results-list)
  (let ((p (total sum-up-results-list)))
    (- (loop
	   for obj in sum-up-results-list
	   sum (* (/ (cdr obj) p) (log (/ (cdr obj) p) 2))))))

(defun mean (sum-up-results-list)
  "for regression tree, objective variable is numeric data."
  (let ((p (total sum-up-results-list)))
    (/ (loop
	   for obj in sum-up-results-list
	   sum (* (car obj) (cdr obj)))
       p)))

(defun variance (sum-up-results-list)
  "for regression tree, objective variable is numeric data."
  (let ((p (total sum-up-results-list))
	(m (mean sum-up-results-list)))
    
    (/ (loop
	   for obj in sum-up-results-list
	   sum (* (expt (- (car obj) m) 2) (cdr obj)))
       p)))

(defun whole-row-numbers-list (data-vector)
  (loop
      for i below (length data-vector) collect i))

(defun aux-split (data-vector variable-index-hash list-of-row-numbers attribute-column-name attribute)
  (let ((split-predicate (make-split-predicate attribute t))
                                        ; make-split-predicate returns optimized function
                                        ; if the second (optional) argument is T
	(attribute-column-index
         (column-name->column-number variable-index-hash attribute-column-name))
	(true-list '())
	(false-list '()))
   
    (dolist (i list-of-row-numbers (values true-list false-list))
      (if (funcall split-predicate (svref (svref data-vector i) attribute-column-index))
	  (push i true-list)
	(push i false-list)))))

(defun split (data-vector variable-index-hash list-of-row-numbers attribute-column-name attribute)
  (if (and (null attribute-column-name)
  	   (null attribute))
      list-of-row-numbers
    (aux-split data-vector variable-index-hash list-of-row-numbers attribute-column-name attribute)))

(defun delta-gini (data-vector variable-index-hash list-of-row-numbers attribute-column-name
		   attribute objective-column-index)

  (multiple-value-bind (true-list false-list)
      (aux-split data-vector variable-index-hash list-of-row-numbers attribute-column-name attribute)
    (if (or (null true-list) (null false-list))
	0.0d0
      (- (gini-index (sum-up-results data-vector list-of-row-numbers objective-column-index))
         (* (/ (length true-list) (length list-of-row-numbers))
            (gini-index (sum-up-results data-vector true-list objective-column-index)))
         (* (/ (length false-list) (length list-of-row-numbers))
            (gini-index (sum-up-results data-vector false-list objective-column-index)))))))

(defun delta-entropy (data-vector variable-index-hash list-of-row-numbers attribute-column-name
		      attribute objective-column-index) 
  (multiple-value-bind (true-list false-list)
      (aux-split data-vector variable-index-hash list-of-row-numbers attribute-column-name attribute)
    (if (or (null true-list) (null false-list))
	0.0d0
      (- (entropy (sum-up-results data-vector list-of-row-numbers objective-column-index))
         (* (/ (length true-list) (length list-of-row-numbers))
            (entropy (sum-up-results data-vector true-list objective-column-index)))
         (* (/ (length false-list) (length list-of-row-numbers))
            (entropy (sum-up-results data-vector false-list objective-column-index)))))))

(defun delta-variance (data-vector variable-index-hash list-of-row-numbers attribute-column-name
		       attribute objective-column-index)
  (multiple-value-bind (true-list false-list)
      (aux-split data-vector variable-index-hash list-of-row-numbers attribute-column-name attribute)
    (if (or (null true-list) (null false-list))
	0.0d0
      (- (variance (sum-up-results data-vector list-of-row-numbers objective-column-index))
         (* (/ (length true-list) (length list-of-row-numbers))
            (variance (sum-up-results data-vector true-list objective-column-index)))
         (* (/ (length false-list) (length list-of-row-numbers))
            (variance (sum-up-results data-vector false-list objective-column-index)))))))

(defun make-split-criterion-list (data-vector variable-index-hash objective-column-index)
  (loop with split-criterion-list = '()
      for var-name being the hash-keys in variable-index-hash
      using (hash-value j)
      unless (eql j objective-column-index) do
        (let* ((v (loop for line across (the simple-array data-vector)
                      collect (svref line j)))
               (w (remove-duplicates v)));;remark
          (assert (<= 2 (length w)))
          (if (= (length w) 2)
              (push (cons var-name (car w)) split-criterion-list)
            (dolist (attribute w)
              (push (cons var-name attribute) split-criterion-list))))
      finally (return split-criterion-list)))
  
(defun select-best-splitting-attribute (data-vector variable-index-hash
					list-of-row-numbers split-criterion-list
					objective-column-index &key (test #'delta-gini) (epsilon 0))
  
  (let* ((v (mapcar #'(lambda (x) (list x (funcall test data-vector variable-index-hash list-of-row-numbers (car x) (cdr x) objective-column-index)))
		     split-criterion-list))
	 
	 (w (reduce #'(lambda (x y) (if (<= (second x) (second y))
					y
				      x)) v)))
				
    (if (<= (second w) epsilon)
	(values nil '())
      (values (car w) (remove (car w) split-criterion-list))))) ;:test #'equal)))))

(defun make-root-node (data-vector variable-index-hash objective-column-index &key (test #'delta-gini) (epsilon 0))
  
  (let ((initial-row-numbers-list (whole-row-numbers-list data-vector)))
  
    (multiple-value-bind (best-split-criterion split-criterion-list)
	(select-best-splitting-attribute
	 data-vector variable-index-hash initial-row-numbers-list
	 (make-split-criterion-list data-vector variable-index-hash objective-column-index) objective-column-index :test test :epsilon epsilon)
      (let ((result-ratio (sum-up-results data-vector initial-row-numbers-list objective-column-index)))
	(multiple-value-bind (right left)
            (split data-vector variable-index-hash initial-row-numbers-list
                   (car best-split-criterion) (cdr best-split-criterion))
	  (list (list best-split-criterion split-criterion-list)
		result-ratio
		(list right left)))))))

(defun make-new-right-node (data-vector variable-index-hash objective-column-index tree-node 
			    &key (test #'delta-gini) (epsilon 0))
  (if (null (caar tree-node))
      '()
    (let ((right-low-numbers-list (first (third tree-node))))
      
      (multiple-value-bind (best-split-criterion split-criterion-list)
	  (select-best-splitting-attribute
	   data-vector variable-index-hash right-low-numbers-list 
	   (make-split-criterion-list data-vector variable-index-hash objective-column-index)
	   objective-column-index :test test :epsilon epsilon)

	(let ((result-ratio (sum-up-results data-vector right-low-numbers-list objective-column-index)))
	  (multiple-value-bind (right left)
              (split data-vector variable-index-hash right-low-numbers-list
                     (car best-split-criterion) (cdr best-split-criterion))
	    (list (list best-split-criterion split-criterion-list)
                  result-ratio
                  (list right left))))))))
	 	
(defun make-new-left-node (data-vector variable-index-hash objective-column-index tree-node 
                           &key (test #'delta-gini) (epsilon 0))
  (if (null (caar tree-node))
      '()
    (let ((left-low-numbers-list (second (third tree-node))))
      
      (multiple-value-bind (best-split-criterion split-criterion-list)
	  (select-best-splitting-attribute
	   data-vector variable-index-hash left-low-numbers-list 
	   (make-split-criterion-list data-vector variable-index-hash objective-column-index)
	   objective-column-index :test test :epsilon epsilon)
	(let ((result-ratio (sum-up-results data-vector left-low-numbers-list objective-column-index)))
	  (multiple-value-bind (right left)
              (split data-vector variable-index-hash left-low-numbers-list
                     (car best-split-criterion) (cdr best-split-criterion))
	    
	    (list (list best-split-criterion split-criterion-list)
                  result-ratio
                  (list right left))))))))

(defun make-tree (data-vector variable-index-hash objective-column-index tree-node
                  &key (test #'delta-gini) (epsilon 0))
  (if (null (caar tree-node))
      (list (second tree-node) (car (third tree-node)))
    (list tree-node
          (make-tree data-vector variable-index-hash objective-column-index
                     (make-new-right-node data-vector variable-index-hash objective-column-index
                                          tree-node :test test :epsilon epsilon)
                     :test test :epsilon epsilon)
          (make-tree data-vector variable-index-hash objective-column-index
                     (make-new-left-node data-vector variable-index-hash objective-column-index
                                         tree-node :test test :epsilon epsilon)
                     :test test :epsilon epsilon))))

(defun make-decision-tree (unspecialized-dataset objective-column-name
			   &key (test #'delta-gini) (epsilon 0))
  (let* ((data-vector (dataset-points unspecialized-dataset))
	 (variable-index-hash (make-variable-index-hash unspecialized-dataset)))

    (let* ((objective-column-index
            (column-name->column-number variable-index-hash objective-column-name))
           (root (make-root-node data-vector variable-index-hash objective-column-index
                                 :test test :epsilon epsilon)))
      (make-tree data-vector variable-index-hash objective-column-index root
		 :test test :epsilon epsilon))))

(defun print-decision-tree-node (tree-node &optional stream)
  (if (numberp (cdaar tree-node))
      (format stream "[~A <= ~A?]~A~%" (cdaar tree-node) (caaar tree-node) (second tree-node))
    (format stream "[~A:~A?]~A~%" (caaar tree-node) (cdaar tree-node) (second tree-node))))

(defun print-decision-tree (decision-tree &optional (stream t) (indent 0))
  (let ((indent (+ 3 indent)))
    (if (= (length decision-tree) 2)	;leaf or not leaf
	(format stream "~A~%" (car decision-tree))
      (progn
	(print-decision-tree-node (first decision-tree) stream)
	(dotimes (i indent) (princ " "))
	(format stream "Yes->")
	(print-decision-tree (second decision-tree) stream indent)
	(dotimes (i indent) (princ " "))
	(format stream "No->")
	(print-decision-tree (third decision-tree) stream indent)))))

(defun make-regression-tree (unspecialized-dataset objective-column-name
                             &key (test #'delta-variance) (epsilon 0))
  (let* ((data-vector (dataset-points unspecialized-dataset))
	 (variable-index-hash (make-variable-index-hash unspecialized-dataset)))

    (let* ((objective-column-index
            (column-name->column-number variable-index-hash objective-column-name))
           (root (make-root-node data-vector variable-index-hash objective-column-index
                                 :test test :epsilon epsilon)))
      (make-tree data-vector variable-index-hash objective-column-index root
                 :test test :epsilon epsilon))))
    	 
(defun print-regression-tree-node (tree-node &optional stream)
  (if (numberp (cdaar tree-node))
      (format stream "[~A <= ~A?] (mean = ~,2F, n = ~A)~%" 
	      (cdaar tree-node) (caaar tree-node) (mean (second tree-node)) (total (second tree-node)))
    (format stream "[~A:~A?] (mean = ~,2F, n = ~A)~%" 
	    (caaar tree-node) (cdaar tree-node) (mean (second tree-node)) (total (second tree-node)))))

(defun print-regression-tree (regression-tree &optional (stream t) (indent 0))
  (let ((indent (+ 3 indent)))
    (if (= (length regression-tree) 2)	;leaf or not leaf
	(format stream "(mean = ~,2F, n = ~A)~%" (mean (first regression-tree)) (total (first regression-tree)))
      (progn
	(print-regression-tree-node (first regression-tree) stream)
	(dotimes (i indent) (princ " "))
	(format stream "Yes->")
	(print-regression-tree (second regression-tree) stream indent)
	(dotimes (i indent) (princ " "))
	(format stream "No->")
	(print-regression-tree (third regression-tree) stream indent)))))

(defun predict-decision-tree (query-vector unspecialized-dataset tree)
  (let ((variable-index-hash (make-variable-index-hash unspecialized-dataset)))
    (if (= (length tree) 2)		;leaf or not leaf
	(car (reduce #'(lambda (x y) (if (<= (cdr x) (cdr y))
					 y
				       x)) (first tree)))
  
      (progn
	(let ((i (column-name->column-number variable-index-hash (caaaar tree))))
	  (cond ((realp (cdaaar tree))
		 (if (<= (cdaaar tree) (svref query-vector i))
		     (predict-decision-tree query-vector unspecialized-dataset (second tree))
		   (predict-decision-tree query-vector unspecialized-dataset (third tree))))
	       
		((or (stringp (cdaaar tree)) (symbolp (cdaaar tree)))
		 (if (equal (cdaaar tree) (svref query-vector i))
		     (predict-decision-tree query-vector unspecialized-dataset (second tree))
		   (predict-decision-tree query-vector unspecialized-dataset (third tree))))
	      
		(t (error "invalid dataset."))))))))
 
(defun decision-tree-validation (validation-dataset objective-column-name decision-tree)
  (let* ((variable-index-hash (make-variable-index-hash validation-dataset))
	 (k (column-name->column-number variable-index-hash objective-column-name))
	 (validation-data-vector (dataset-points validation-dataset)))
    
    (sum-up (loop
		for i below (length validation-data-vector)
		collect (cons (predict-decision-tree (svref validation-data-vector i) validation-dataset decision-tree)
			      (svref (svref validation-data-vector i) k))))))

(defun predict-regression-tree (query-vector unspecialized-dataset tree)
  (let ((variable-index-hash (make-variable-index-hash unspecialized-dataset)))
    (if (= (length tree) 2)		;leaf or not leaf
	(mean (first tree))
      (progn
	(let ((i (column-name->column-number variable-index-hash (caaaar tree))))
	  (cond ((realp (cdaaar tree))
		 (if (<= (cdaaar tree) (svref query-vector i))
		     (predict-regression-tree query-vector unspecialized-dataset (second tree))
		   (predict-regression-tree query-vector unspecialized-dataset (third tree))))
	       
		;;((stringp (cdaaar tree))
		((or (stringp (cdaaar tree)) (symbolp (cdaaar tree)))
		 (if (equal (cdaaar tree) (svref query-vector i))
		     (predict-regression-tree query-vector unspecialized-dataset (second tree))
		   (predict-regression-tree query-vector unspecialized-dataset (third tree))))
		
		(t (error "invalid dataset."))))))))

(defun regression-tree-validation (validation-dataset objective-column-name regression-tree)
  (let* ((variable-name-index (make-variable-index-hash validation-dataset))
	 (k (column-name->column-number variable-name-index objective-column-name))
	 (validation-data-vector (dataset-points validation-dataset))
	 (n (length validation-data-vector)))
    
    (loop
	for i below n
	sum (expt (- (predict-regression-tree (svref validation-data-vector i) validation-dataset regression-tree) 
		     (svref (svref validation-data-vector i) k))
		  2) into s
	finally (return (/ s n)))))


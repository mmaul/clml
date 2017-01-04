
(in-package :clml.decision-tree.random-forest)

(defmacro bag-sizer (bag-size-key set-size-number)
  `(case ,bag-size-key
     (:set-size ,set-size-number)
     (:half-set-size (round (/ ,set-size-number 2)))
     (:quarter-set-size (round (/ ,set-size-number 4)))
     (:sqrt-set-size (round (sqrt ,set-size-number)))
     (t (if (and (integerp ,bag-size-key) (< 0 ,bag-size-key))
            ,bag-size-key
            (error "~A is not a valid choice for set-size." ,bag-size-key)))))

(defun make-bootstrap-sample (unspecialized-dataset &key (data-bag-size :set-size))
  (let* ((data-vector (dataset-points unspecialized-dataset))
         (n (array-dimension data-vector 0))
         (m (bag-sizer data-bag-size n))
         (new-data-vector (make-array m)))

    (loop
        for i below m
        do (setf (svref new-data-vector i) (svref data-vector (random n))))
    new-data-vector))

(defun make-balanced-bootstrap-sample (unspecialized-dataset objective-column-index &key (data-bag-size :set-size) (balance t))
  (let* ((data-vector (dataset-points unspecialized-dataset))
         (n (array-dimension data-vector 0))
         (m (bag-sizer data-bag-size n))
         (new-data-vector (make-array m))
         (new-balance (ceiling (/ m #1=(if (and (integerp balance) (< 0 balance)) balance 2))))
         (collect-table (make-hash-table :test #'equal :size #1#)))
    (flet ((collect? (objective)
             (if #2=(gethash objective collect-table)
                 (if (<= #2# new-balance)
                     (incf #2#)
                     nil)
                 (setf #2# 1))))
      (loop
         for i below m
         do (setf (svref new-data-vector i) (do ((data-point #3=(svref data-vector (random n)) #3#)) ((collect? (svref data-point objective-column-index)) data-point))))
      new-data-vector)))

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

(defun random-subset (list new-size &optional (len (length list)))
  "Return a new list containing new-size number of randomly chosen elements of list without replacement. len is assumed to be the length of list."
  (declare (list list) (type (integer 0) new-size len))
  (if (= new-size len)
      list
      (let ((picked-table (make-hash-table :test #'eql)))
        (assert (< new-size len))
        (loop
           for i below new-size
           do (do ((index #1=(random len) #1#)) ((null (gethash index picked-table)) (setf (gethash index picked-table) t))))
        (loop
           for l in list
           for j below len
           if (gethash j picked-table)
           collect l))))

(defun make-random-decision-tree (unspecialized-dataset objective-column-name &key (test #'delta-gini) (data-bag-size :set-size) (feature-bag-size :set-size) (balance nil))
  (let* ((variable-index-hash (make-variable-index-hash unspecialized-dataset))
         (objective-column-index (column-name->column-number variable-index-hash objective-column-name))
         (data-vector (if balance
                          (make-balanced-bootstrap-sample unspecialized-dataset objective-column-index :data-bag-size data-bag-size :balance balance)
                          (make-bootstrap-sample unspecialized-dataset :data-bag-size data-bag-size)))
         (dim-vector (dataset-dimensions unspecialized-dataset))
         (dim-vector-length (length dim-vector))
         (old-column-list-size (if (< objective-column-index dim-vector-length) (1- dim-vector-length) dim-vector-length))
         (column-list (random-subset
                       (loop
                          for i below dim-vector-length
                          if (/= i objective-column-index)
                          collect (dimension-name (aref dim-vector i)))
                       (bag-sizer feature-bag-size old-column-list-size)
                       old-column-list-size))
         (root (make-root-node-for-rf data-vector variable-index-hash objective-column-index column-list
                               :test test)))
    (make-decision-tree-for-rf data-vector variable-index-hash objective-column-index root :test test)))

(defun make-random-forest (unspecialized-dataset objective-column-name &key (test #'delta-gini) (tree-number 500) (data-bag-size :set-size) (feature-bag-size :set-size) (balance nil))
  "This implementation requires lparallel:*kernel* be set with a kernel object. This can be done
by:
#+BEGIN_SRC lisp
(setf lparallel:*kernel* (lparallel:make-kernel N))
#+END_SRC
Where N is the number of worker threads which should generally be the number of CPU cores.

- return: (SIMPLE-ARRAY T (* )), random forest consisting of unpruned decision trees
- arguments:
 - unspecialized-dataset
 - objective-variable-name
 - test : delta-gini | delta-entropy , splitting criterion function, default is delta-gini
 - tree-number : the number of decision trees, default is 500
 - data-bag-size : the number of data points from unspecialized-dataset to use to train each tree in the forest. Chosen randomly with replacement for each tree. The default :set-size gives a bag the same size as unspecialized-dataset
 - feature-bag-size : the number of variables from all available to use to train each tree in the forest. Chosen randomly with replacement for each tree. The default :set-size uses all variables.
 - Other available options for data-bag-size and feature-bag-size are :half-set-size, :quarter-set-size, and :sqrt-set-size. If an integer is specified it will be used instead.
 - If balance in non-nil, the dataset will be sampled so that each of the possible values of objective-column-name are approximately equally represented. The value passed to :balance should be the number of possible values of objective-column-name; if t, assumed to be 2.
- reference : [[http://www-stat.stanford.edu/~tibs/ElemStatLearn/][Trevor Hastie, Robert Tibshirani and Jerome Friedman. The Elements of Statistical Learning:Data Mining, Inference, and Prediction]]
"
  (lparallel:pmap 'vector
                  (lambda (l)  (declare (ignore l)) (make-random-decision-tree unspecialized-dataset objective-column-name :test test :data-bag-size data-bag-size :feature-bag-size feature-bag-size :balance balance)) (make-array tree-number))
  )

(defun predict-forest (query-vector unspecialized-dataset forest)
  "- return: string, prediction
- arguments:
 - query-vector
 - unspecialized-dataset : dataset used to make a random forest
 - forest
- comments : make predictions by a majority vote of decision trees in random forest."
  (car (reduce #'(lambda (x y) (if (<= (cdr x) (cdr y))
                                   y
                                 x))
               (sum-up (loop
                           for i below (length forest)
                           collect (predict-decision-tree query-vector unspecialized-dataset (svref forest i)))))))

(defun forest-validation (validation-dataset objective-column-name forest)
  "- return: CONS, validation result
- arguments:
 - unspecialized-dataset : dataset for validation
 - objective-variable-name
 - forest
- comments : each element of returning association list represents that ((prediction . answer) . number).
"
  (let* ((variable-index-hash (make-variable-index-hash validation-dataset))
         (k (column-name->column-number variable-index-hash objective-column-name))
        (validation-data-vector (dataset-points validation-dataset)))

    (sum-up (loop
                 for i below (length validation-data-vector)
                 collect (cons (predict-forest (svref validation-data-vector i) validation-dataset forest)
                               (svref (svref validation-data-vector i) k))))))



(defun make-random-regression-tree (unspecialized-dataset objective-column-name &key (data-bag-size :set-size) (feature-bag-size :set-size))
  (let* ((data-vector (make-bootstrap-sample unspecialized-dataset :data-bag-size data-bag-size))
         (variable-index-hash (make-variable-index-hash unspecialized-dataset))
         (objective-column-index (column-name->column-number variable-index-hash objective-column-name))
         (dim-vector (dataset-dimensions unspecialized-dataset))
         (dim-vector-length (length dim-vector))
         (old-column-list-size (if (< objective-column-index dim-vector-length) (1- dim-vector-length) dim-vector-length))
         (column-list (random-subset
                       (loop
                          for i below dim-vector-length
                          if (/= i objective-column-index)
                          collect (dimension-name (aref dim-vector i)))
                       (bag-sizer feature-bag-size old-column-list-size)
                       old-column-list-size))
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

(defun make-regression-forest (unspecialized-dataset objective-column-name &key (tree-number 500) (data-bag-size :set-size) (feature-bag-size :set-size))
  "This implementation requires lparallel:*kernel* be set with a kernel boject. This can be done
by:
#+BEGIN_SRC lisp
(setf lparallel:*kernel* (lparallel:make-kernel N))
#+END_SRC
Where N is the number of worker threads which should generally be the number of CPU cores.

- return: (SIMPLE-ARRAY T (* )), regression forest consisting of unpruned decision trees
- arguments:
 - unspecialized-dataset
 - objective-variable-name
 - tree-number : the number of decision trees, default is 500
 - data-bag-size : the number of data points from unspecialized-dataset to use to train each tree in the forest. Chosen randomly with replacement for each tree. The default :set-size gives a bag the same size as unspecialized-dataset
 - feature-bag-size : the number of variables from all available to use to train each tree in the forest. Chosen randomly with replacement for each tree. The default :set-size uses all variables.
 - Other available options for data-bag-size and feature-bag-size are :half-set-size, :quarter-set-size, and :sqrt-set-size. If an integer is specified it will be used instead.
"
(let ((lparallel:*kernel* (if (null lparallel:*kernel*)
                               (lparallel:make-kernel 4) lparallel:*kernel*))
      )(lparallel:pmap 'vector
        (lambda (l) (declare (ignore l)) (make-random-regression-tree unspecialized-dataset objective-column-name :data-bag-size data-bag-size :feature-bag-size feature-bag-size)) (make-array tree-number))))

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
  "- importance of explanatory variables
- return: NIL
- arguments:
 - forest
"
  (format t "~%")
  (loop
      with column-list = (nth 5 (first (aref forest 0)))
      for column in column-list
      as sum-gini = (loop
                        for tree across forest
                        sum (sum-up-decrease-gini tree column))
      do (format t "~a  ~a~%" column (/ sum-gini (length forest)))))

(defun var-used (forest)
  (format t "~%")
  (loop
      with column-list = (nth 5 (first (aref forest 0)))
      for column in column-list
      as n = (loop
                 for tree across forest
                 sum (sum-up-var tree column))
      do (format t "~a  ~a~%" column n)))

(defun count-tree-node (rf-tree)
  (if (>= 2 (length rf-tree))
      1
    (+ (count-tree-node (second rf-tree))
       (count-tree-node (third rf-tree)))))

(defun treesize (forest)
  (loop
      for tree across forest
      collect (count-tree-node tree)))


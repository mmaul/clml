(defpackage :text.hdp-lda
  ;; (:nicknames :hdp-lda)
  (:use :cl :nonparameteric.statistics :hjs.util.meta)
  (:export :hdp-lda
	   :word
	   :document
	   :table
	   
	   :document-id
	   :document-words
	   :document-thetas
	   
	   :word-id
	   
	   :topic-count
	   :hdp-lda-data
	   :vocabulary
	   
	   :add-customer
	   :remove-customer
	   :sample-new-topic
	   :hypers-sampling
	   
	   :initialize
	   :sampling
	   :assign-theta
	   :get-phi
	   
	   :get-top-n-words
	   :revert-word
	   
	   :*alpha-base-a*
	   :*alpha-base-b*
	   :*gamma-base-a*
	   :*gamma-base-b*
	   
	   :*default-beta*))

(in-package :text.hdp-lda)

(defparameter *alpha-base-a* 1d0)
(defparameter *alpha-base-b* 1d-1)
(defparameter *gamma-base-a* 1d0)
(defparameter *gamma-base-b* 1d-1)

(defparameter *default-beta* 1d-1)

(defstruct word id assign)

(defstruct table dish (customer 0) (customers (make-adarray 0)))

(defclass document ()
  ((id :initarg :id :accessor document-id)
   (words :initarg :words :accessor document-words)
   (thetas :accessor document-thetas)
   (restaurant :accessor document-restaurant)
   (layers :initform (make-adarray 1 :initial-element 0 :element-type 'fixnum) :accessor document-layer-points)
   (p :accessor document-p)))

(defmethod initialize-instance ((instance document) &rest initargs)
  (declare (ignore initargs))
  (call-next-method)
  (let ((l (length (document-words instance))))
    (with-slots (restaurant table-topic p) instance
      (setf restaurant (make-array l))
      (setf p (make-array l :initial-element 0d0 :element-type 'double-float))))
  instance)

(defmethod print-object ((object document) stream)
  (print-unreadable-object (object stream :identity t)
    (format stream "~S ~S" (type-of object) (document-id object))))

(defclass hdp-lda ()
  ((k :initarg :k :initform 0 :accessor topic-count)
   (topics :accessor hdp-lda-topics)
   (topic-tables :accessor hdp-lda-topic-tables)
   (topic-occurs :accessor hdp-lda-topic-occurs)
   (ntables :initform 0 :accessor hdp-lda-ntables)
   (alpha  :initarg :alpha :accessor hdp-lda-alpha)
   (beta   :initarg :beta  :accessor hdp-lda-beta)
   (gamma  :initarg :gamma :accessor hdp-lda-gamma)
   (p      :accessor hdp-lda-p)
   (data   :initarg :data :accessor hdp-lda-data)
   (f-k    :accessor hdp-lda-f-k)
   (word-table :initform (make-hash-table :test #'equal) :accessor word-table)
   (revert-table :accessor revert-table)
   (id :initform -1 :accessor vocabulary)))

(defmethod ensure-word ((hdp-lda hdp-lda) string)
  (let ((memo (word-table hdp-lda)))
    (multiple-value-bind (oldid found) (gethash string memo)
      (unless found
	(setf oldid (incf (vocabulary hdp-lda)))
	(setf (gethash string memo) oldid))
      (make-word :id oldid))))

(defmethod add-customer ((hdp-lda hdp-lda) word doc &optional (old 1))
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type fixnum old))
  "add new customer word belong to doc randomly"
  (let ((tables (document-restaurant doc))
	(p      (document-p doc))
	(topic-p (hdp-lda-p hdp-lda))
	(alpha  (hdp-lda-alpha hdp-lda))
	(beta   (hdp-lda-beta  hdp-lda))
	(gamma  (hdp-lda-gamma hdp-lda))
	(occurs  (hdp-lda-topic-occurs hdp-lda))
	(v      (1+ (vocabulary hdp-lda)))
	(ttables (hdp-lda-topic-tables hdp-lda))
	(sims   (aref (hdp-lda-topics hdp-lda) (word-id word)))
	(layers (document-layer-points doc)))
    (declare (type double-float alpha beta gamma)
	     (type (array fixnum (*)) occurs ttables sims layers)
	     (type (simple-array t (*)) tables)
	     (type (array double-float (*)) topic-p)
	     (type (simple-array double-float (*)) p)
	     (type fixnum v))
    ;;; slice sampling -- l(x): customers + alpha / pi(x): topic relationship
    ;;; assume tables are sorted by customer number
    (let* ((slice (random old))
	   (sum 0d0)
	   (base (* v beta))
	   (limit (if (zerop slice)
		      (1- (aref layers 0))
		    (1- (aref layers (1- slice))))))
      (declare (type fixnum slice limit)
	       (type double-float sum base))
      ;; calculated flag to minus
      (fill topic-p -1d0)
      (when (>= alpha slice) ;; consider backoff
	;; calculate f_k(x_ij) into topic-p
	(loop
	    for s fixnum across sims
	    for occur fixnum across occurs
	    for i fixnum from 0
	    unless (zerop occur) do
	      (setf (aref topic-p i)
		(the double-float
		  (/ (the double-float (+ s beta))
		     (the double-float (+ occur base))))))
	(incf sum
	      (the double-float
		(loop for tt fixnum across ttables
		    for f-k double-float across topic-p
		    unless (minusp f-k)
		    summing (the double-float (* f-k tt)) into ans double-float
		    finally (return
			      (/ (the double-float (+ ans (the double-float (* gamma (/ v)))))
				 (the double-float (+ (the fixnum (hdp-lda-ntables hdp-lda)) gamma))))))))
      ;; calculate sliced pi(x) into p
      (loop for i fixnum from 0 upto limit
	  for table across tables
	  while table do
	    (let* ((assign (table-dish table))
		   (subp (aref topic-p assign)))
	      (declare (type fixnum assign)
		       (type double-float subp))
	      (when (minusp subp)
		(let ((new (/ (the double-float (+ (aref sims assign) beta))
			      (the double-float (+ (aref occurs assign) base)))))
		  (declare (type double-float new))
		  (setf subp new)
		  (setf (aref topic-p assign) new)))
	      (incf sum subp)
	      (setf (aref p i) subp)))
      (let ((ref (randomize-slice p sum limit))) ;; now sample new seating by slice-sampling
	(declare (type fixnum ref))
	;; add new customer
	(when(= ref -1) ;; new table
	  (incf (hdp-lda-ntables hdp-lda))
	  (setf ref (aref layers 0))
	  (unless (aref tables ref)
	    (setf (aref tables ref) (make-table)))
	  (let ((topic (sample-new-topic hdp-lda topic-p (dfloat (/ v)))))
	    (declare (type fixnum topic))
	    (setf (table-dish (aref tables ref)) topic) 
	    (incf (aref ttables topic))))
	(let* ((table (aref tables ref))
	       (topic (table-dish table))
	       (old (table-customer table))
	       (new-position (aref layers old)))
	  (declare (type fixnum topic))
	  (incf (table-customer table))
	  (vector-push-extend word (table-customers table))
	  (setf (word-assign word) table)
	  (incf (aref sims topic))
	  (incf (aref (hdp-lda-topic-occurs hdp-lda) topic))
	  ;; resort tables
	  (rotatef (aref tables ref)
		   (aref tables new-position))
	  (incf (aref layers old))
	  (when (= (length layers) (1+ old))
	    (vector-push-extend 0 layers)))
	ref))))

(defmethod sample-new-topic ((hdp-lda hdp-lda) topic-p k-new)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type (array double-float (*)) topic-p)
	   (type double-float k-new))
  ;; trick! topic-p must calculated at above and never used again -- so we can use side effect
  (let* ((ttables (hdp-lda-topic-tables hdp-lda))
	 (gamma  (hdp-lda-gamma hdp-lda))
	 (sum (* gamma k-new))
	 (zero-position (length topic-p)))
    (declare (type (array fixnum (*)) ttables)
	     (type double-float gamma sum)
	     (type fixnum zero-position))
    (loop for count fixnum across ttables
	for sim double-float across topic-p
	for i fixnum from 0
	for subp double-float = (* count sim) do
	  (when (zerop count)
	    (setf zero-position i))
	  (incf sum subp)
	  (setf (aref topic-p i) subp))
    (let ((ref (randomize-choice topic-p sum)))
      (declare (type fixnum ref))
      (when (= ref -1)
	(incf (topic-count hdp-lda))
	(setf ref zero-position)
	(when (= ref (the fixnum (length topic-p)))
	  ;; extend required
	  (vector-push-extend 0d0 topic-p)
	  (vector-push-extend 0 (hdp-lda-topic-occurs hdp-lda))
	  (loop for s across (hdp-lda-topics hdp-lda) do
		(vector-push-extend 0 s))
	  (loop for s across (hdp-lda-f-k hdp-lda) do
		(vector-push-extend 0d0 s))
	  (vector-push-extend 0 ttables)))
      ref)))

(defmethod remove-customer ((hdp-lda hdp-lda) word doc)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  "remove customer"
  (let* ((tables (document-restaurant doc))
	 (table (word-assign word))
	 (topic (table-dish table))
	 (layers (document-layer-points doc))
	 (old (table-customer table))
	 (ref (position table tables :start (aref layers old)))
	 (new (decf (the fixnum (table-customer table))))
	 (new-position (1- (the fixnum (aref layers new))))) 
    (declare (type fixnum old new topic new-position))
    ;; table resort
    (rotatef (aref tables ref)
	     (aref tables new-position))
    (decf (aref layers new))
    (when (zerop new)
      ;; delete table
      (decf (the fixnum (hdp-lda-ntables hdp-lda)))
      (when (zerop (decf (the fixnum (aref (the (array fixnum (*)) (hdp-lda-topic-tables hdp-lda)) topic))))
	;; delete topic
	(decf (the fixnum (topic-count hdp-lda)))))
    (decf (the fixnum (aref
		       (the (array fixnum (*))(aref
					       (the (array fixnum (*)) (hdp-lda-topics hdp-lda))
					       (the fixnum (word-id word))))
		       topic)))
    (decf (the fixnum (aref (the (array fixnum (*)) (hdp-lda-topic-occurs hdp-lda)) topic)))
    old))

(defmethod add-table ((hdp-lda hdp-lda) table &optional (old 1))
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type fixnum old))
  (let ((customers (table-customers table))
	(topic-p (hdp-lda-p hdp-lda))
	(beta (hdp-lda-beta hdp-lda))
	(gamma  (hdp-lda-gamma hdp-lda))
	(occurs  (hdp-lda-topic-occurs hdp-lda))
	(v      (1+ (vocabulary hdp-lda)))
	(f-k    (hdp-lda-f-k hdp-lda))
	(ttables (hdp-lda-topic-tables hdp-lda))
	(sims   (hdp-lda-topics hdp-lda))
	(max 0d0))
    (declare (type double-float gamma max)
	     (type (array fixnum (*)) occurs ttables)
	     (type (array t (*)) customers sims)
	     (type (array double-float (*)) topic-p)
	     (type fixnum v))
    ;;; slice sampling
    (let ((memo (make-hash-table))
	  (slice (random old))
	  (sum 0d0)
	  (base (* v beta))
	  (zero-position (length topic-p)))
      (declare (type fixnum slice zero-position)
	       (type double-float sum base))
      (fill topic-p 0d0)
      ;; calculate f_k(x_ij) into memoized arrays
      (loop for c across customers
	  for id fixnum = (word-id c)
	  for f-k-row = (aref f-k id)
	  for encounts = (gethash id memo 0) do
	    (when (zerop encounts)
	      ;;; calculate f_k(x_ij) using flag!!
	      (loop for occur fixnum across occurs
		  for j fixnum from 0
		  for s fixnum = (aref (aref sims (word-id c)) j) do
		    (if (zerop occur)
			(setf (aref f-k-row j) 0d0)
		      (setf (aref f-k-row j)
			(the double-float
			  (/ (the double-float (+ s beta))
			     (the double-float (+ occur base))))))))
	    ;; incf occurence in this table
	    (incf (gethash id memo 0)))
      ;; push f_k(x_ij) values into topic-p with slice!!
      (loop
	  for count across ttables
	  for occur fixnum across occurs
	  for i fixnum from 0 do
	    (cond ((zerop count)
		   (setf zero-position i))
		  ((>= count slice)
		   ;;; push
		   (let ((ans 0d0))
		     (declare (type double-float ans))
		     (maphash #'(lambda (k v)
				  (declare (type fixnum k v))
				  (let ((mul (* (log (the double-float (aref (aref f-k k) i))) v)))
				    (declare (type double-float mul))
				    (incf ans mul)))
			      memo)
		     (setf (aref topic-p i) ans)
		     (setf max (max max ans))))
		  (t (setf (aref topic-p i) 0d0))))
      ;; max -> jack
      (setf max (- #.(/ *most-positive-exp-able-float* 2) max))
      ;; k_new with slice too
      (when (>= gamma slice)
	;; jack-up!
	(setf sum
	  (safe-exp (+ (* (log (dfloat (/ v))) (length customers)) max))))
      (loop for i fixnum from 0 below (length topic-p)
	  for x = (aref topic-p i)
	  unless (zerop x) do
	    (let ((new (safe-exp (+ x max))))
	      (declare (type double-float new))
	      (setf (aref topic-p i) new)
	      (incf sum new)))
      #+ignore	    
      (map-into topic-p #'(lambda (x) (if (not (zerop x))
					  (safe-exp (+ x max))
					0d0))
		topic-p)
      #+ignore
      (incf sum (reduce #'+ topic-p))
      (let ((ref (randomize-choice topic-p sum))) ;; now sample new dish
	;; topic extention check
	(declare (type fixnum ref))
	(when (= ref -1)
	  (incf (topic-count hdp-lda))
	  (setf ref zero-position)
	  (when (= ref (the fixnum (length topic-p)))
	    ;; extend required
	    (vector-push-extend 0d0 topic-p)
	    (vector-push-extend 0 (hdp-lda-topic-occurs hdp-lda))
	    (loop for s across (hdp-lda-topics hdp-lda) do
		  (vector-push-extend 0 s))
	    (loop for s across (hdp-lda-f-k hdp-lda) do
		  (vector-push-extend 0d0 s))
	    (vector-push-extend 0 ttables)))
	;; add table itself
	(setf (table-dish table) ref)
	(incf (aref ttables ref))
	(incf (hdp-lda-ntables hdp-lda))
	;; add customers in this table
	(maphash #'(lambda (k v)
		     (incf (aref occurs ref) v)
		     (incf (aref (aref sims k) ref) v))
		 memo)
	;; no longer required customer list
	(setf (fill-pointer customers) 0)
	ref))))
  
(defmethod remove-table ((hdp-lda hdp-lda) table)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  "remove table include occupied customers"
  (let* ((customers (table-customers table))
	 (topic (table-dish table))
	 (ttables (hdp-lda-topic-tables hdp-lda))
	 (old (aref ttables topic))
	 (new (decf (aref ttables topic))))
    (declare (type fixnum old new topic))
    (decf (hdp-lda-ntables hdp-lda))
    (when (zerop new)
      ;; delete topic
      (decf (the fixnum (topic-count hdp-lda))))
    ;; delete all customers
    (loop for word across customers
	for id = (word-id word) do
	  (decf (aref (aref (hdp-lda-topics hdp-lda) id) topic))
	  (decf (aref (hdp-lda-topic-occurs hdp-lda) topic)))
    old))

(defmethod hypers-sampling ((hdp-lda hdp-lda))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  "hyperparameter sampling"
  (let ((ntables (dfloat (hdp-lda-ntables hdp-lda)))
	(old-alpha (hdp-lda-alpha hdp-lda))
	(old-gamma (hdp-lda-gamma hdp-lda)))
    (declare (type double-float ntables old-alpha old-gamma))
    (loop
	for doc across (hdp-lda-data hdp-lda)
	for nj__ fixnum = (length (document-words doc))
	summing (the double-float (log (beta-random (1+ old-alpha) (dfloat nj__)))) into wj double-float
	summing (the double-float (bernoulli (/ nj__ (+ old-alpha nj__)))) into sj double-float 
	finally (setf (hdp-lda-alpha hdp-lda)
			   (gamma-random (- (+ (the double-float *alpha-base-a*) ntables) sj)
					 (- (the double-float *alpha-base-b*) wj))))
    (setf (hdp-lda-gamma hdp-lda)
      (gamma-random (- (the double-float (dfloat (+ *gamma-base-a* (topic-count hdp-lda))))
		       (the double-float (bernoulli (/ ntables (+ old-gamma ntables)))))
		    (- (the double-float *gamma-base-b*)
		       (the double-float (log (beta-random (1+ old-gamma) ntables))))))
    (values (hdp-lda-alpha hdp-lda)
	    (hdp-lda-gamma hdp-lda))))

(defmethod initialize ((hdp-lda hdp-lda))
  (let ((predict-k (topic-count hdp-lda)))
    (setf (hdp-lda-topic-tables hdp-lda) (make-adarray predict-k :element-type 'fixnum :initial-element 0))
    (setf (hdp-lda-topic-occurs hdp-lda) (make-adarray predict-k :element-type 'fixnum :initial-element 0))
    (setf (hdp-lda-p hdp-lda) (make-adarray predict-k :element-type 'double-float :initial-element 0d0))
    (setf (hdp-lda-f-k hdp-lda) (make-adarray 0))
    (unless (slot-boundp hdp-lda 'alpha)
    (setf (hdp-lda-alpha hdp-lda) (gamma-random *alpha-base-a* *alpha-base-b*)))
    (unless (slot-boundp hdp-lda 'gamma)
      (setf (hdp-lda-gamma hdp-lda) (gamma-random *gamma-base-a* *gamma-base-b*)))
    (unless (slot-boundp hdp-lda 'beta)
      (setf (hdp-lda-beta hdp-lda) *default-beta*))
    (loop for doc across (hdp-lda-data hdp-lda) do
	  (loop with words = (document-words doc)
	      for i from 0 below (length words)
	      for w = (ensure-word hdp-lda (aref words i)) do
		(setf (aref words i) w)))
    (loop with v = (1+ (vocabulary hdp-lda))
	with a = (make-array v)
	initially (setf (hdp-lda-topics hdp-lda) a)
	for i from 0 below v do
	  (setf (aref a i) (make-adarray predict-k :initial-element 0 :element-type 'fixnum)))
    (loop with v = (1+ (vocabulary hdp-lda))
	with a = (make-array v)
	initially (setf (hdp-lda-f-k hdp-lda) a)
	for i from 0 below v do
	  (setf (aref a i) (make-adarray 0 :initial-element 0d0 :element-type 'double-float))))
  ;; reset predict-k
  (setf (topic-count hdp-lda) 0)
  ;; initial sampling
  (loop for doc across (hdp-lda-data hdp-lda) do
	(loop for w across (document-words doc) do
	      (add-customer hdp-lda w doc)))
  (loop for doc across (hdp-lda-data hdp-lda) do
	(loop for table across (document-restaurant doc)
	    for i from 0 below (aref (document-layer-points doc) 0) do
	      (add-table hdp-lda table (remove-table hdp-lda table))))
  (hypers-sampling hdp-lda))

(defmethod sampling ((hdp-lda hdp-lda))
  (loop for doc across (shuffle-vector (hdp-lda-data hdp-lda)) do
	(loop for w across (document-words doc) do
	      (add-customer hdp-lda w doc (remove-customer hdp-lda w doc))))
  (loop for doc across (hdp-lda-data hdp-lda) do
	(loop for table across (document-restaurant doc)
	    for i from 0 below (aref (document-layer-points doc) 0) do
	      (add-table hdp-lda table (remove-table hdp-lda table))))
  (hypers-sampling hdp-lda)
  )

(defmethod assign-theta ((model hdp-lda))
  (let* ((k (topic-count model))
	 (flags (hdp-lda-topic-occurs model))
	 (resolv (make-array (length flags))))
    (loop 
	with k-dash = 0
	for flag across flags
	for i from 0
	unless (zerop flag) do
	  (setf (aref resolv i) k-dash)
	  (incf k-dash)
	finally (assert (= k-dash k)))
    (loop for doc across (hdp-lda-data model)
	for tables = (document-restaurant doc)
	for limit = (1- (aref (document-layer-points doc) 0)) do
	  (let ((new (make-array k :initial-element (hdp-lda-gamma model)))) ;; add gamma as prior
	    (loop
	      for i from 0 to limit
		for table = (aref tables i) 
		for s = (table-customer table)
		for a = (table-dish table) do
		  (incf (aref new (aref resolv a)) s))
	    (setf (document-thetas doc) (normalize! new))))))

(defmethod get-phi ((model hdp-lda))
  (let ((flags (hdp-lda-topic-occurs model))
	(phi (make-array (topic-count model)))
	(v (1+ (vocabulary model)))
	(beta (hdp-lda-beta model))
	(sim  (hdp-lda-topics model)))
    (loop
	with k = 0
	for flag across flags
	for i from 0
	unless (zerop flag) do
	  (let ((new (make-array v :element-type 'double-float)))
	    (loop for j from 0 below v do
		  (setf (aref new j) (+ beta (aref (aref sim j) i))))
	    (setf (aref phi k) (normalize! new)))
	  (incf k))
    phi))

(defun revert-word (hdp-lda word-id)
  (unless (slot-boundp hdp-lda 'revert-table)
    (let* ((wt (word-table hdp-lda))
	   (rt (make-array (hash-table-count wt))))
      (maphash #'(lambda (k v)
		   (setf (aref rt v) k)) wt)
      (setf (revert-table hdp-lda) rt)))
  (aref (revert-table hdp-lda) word-id))

(defmethod get-top-n-words ((model hdp-lda) n)
  (let ((phi (get-phi model)))
    (loop for topic across phi
	for i from 0
	for n-best = (get-n-best topic n) do
	  (loop for x across n-best
	      for j from 0 do
		(setf (aref n-best j) (revert-word model x)))
	  (setf (aref phi i) n-best))
    phi))
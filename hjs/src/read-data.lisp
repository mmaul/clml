
(in-package :clml.hjs.read-data)

#+lispworks
(eval-when (:load-toplevel :execute)
  (lw:set-default-character-element-type 'lw:simple-char))
#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-int::set-floating-point-modes
   :traps
   (remove :invalid (getf (sb-int:get-floating-point-modes) :traps))))
#+ccl
(eval-when (:compile-toplevel :load-toplevel :execute)
 (ccl::set-fpu-mode :invalid nil))

;;;; helper function

;;@ function-type: t -> symbol
(defun guess-data-type (value)
  (typecase value
    (double-float :numeric)
    (string :category)
    (otherwise :unknown)))



;;;; data type definition

(define-constant +known-data-type+ '(:numeric :category :unknown) :test #'equal)

(defclass dimension ()
  ((name
    :initarg :name
    :accessor dimension-name
    :type string
    :initform "")
   (type
    :initarg :type
    :accessor dimension-type
    :type symbol
    :initform (error "Must specify the data type of the dimension.")
    :documentation "Can be numeric or category.")
   (index
    :initarg :index
    :accessor dimension-index
    :type fixnum
    :initform (error "Must specify the index of dimension either in the numerically typed data vector or the categorially typed data vector.")
    :documentation "The nth column of points array of corresponding type. (type * index) should locate the place where dimension is stored.")
   (metadata
    :initarg :metadata
    :accessor dimension-metadata
    :type list
    :initform '()
    :documentation "An alist that stores some useful information, e.g. the hashtable (for equality) for a category dimension.")))


(define-condition dimension-unknown-type-error (simple-error)
  ((dimension :initarg :dimension)))


(defmethod initialize-instance :after ((d dimension) &key &allow-other-keys)
  (when (not (find (dimension-type d) +known-data-type+))
    (error 'dimension-unknown-type-error
           :format-control "Type of dimension(~s) is unknown, it must be one of ~s"
           :format-arguments (list (dimension-type d) +known-data-type+)
           :dimension d)))

(defun make-dimension (name type index &key metadata)
  (check-type name string)
  (check-type type symbol)
  (check-type index fixnum)
  (check-type metadata list)
  (make-instance 'dimension
                 :name name
                 :type type
                 :index index
                 :metadata metadata))

(defgeneric copy-dimension (dimension))
(defmethod copy-dimension ((dimension dimension))
  (make-dimension (dimension-name dimension)
                  (dimension-type dimension)
                  (dimension-index dimension)
                  :metadata (dimension-metadata dimension)))

(defmethod print-object ((d dimension) stream)
  (with-accessors ((name dimension-name)
                   (type dimension-type)
                   (index dimension-index)) d
    (print-unreadable-object (d stream :type t :identity nil)
      (format stream "NAME: ~A, TYPE: ~A, INDEX: ~A."
              name type index))))

(defclass dataset ()
  ((dimensions
    :initarg :dimensions
    :accessor dataset-dimensions
    :type (simple-array dimension (*))
    :initform (error "Must specify the dimension information for the dataset.")))
  (:documentation
   "The base class."))

(defmethod print-object ((d dataset) stream)
  (with-accessors ((dim dataset-dimensions)) d
    (let ((*print-length* (or *print-length* 10))
          (names (map 'list #'dimension-name dim))
          (types (map 'list #'dimension-type dim)))
      (print-unreadable-object (d stream :type t :identity nil))
      (if (> (length names) *print-length*)
          (format stream "~&DIMENSIONS: ~{~A~^~T| ~} ...~%" (subseq names 0 *print-length*))
        (format stream "~&DIMENSIONS: ~{~A~^~T| ~}~%" names))
      (if (> (length names) *print-length*)
          (format stream "~&TYPES:      ~{~A~^~T| ~} ...~%" (subseq types 0 *print-length*))
        (format stream "~&TYPES:      ~{~A~^~T| ~}~%" types))
      (format stream "~&NUMBER OF DIMENSIONS: ~a~%" (length names)))))

(defclass unspecialized-dataset (dataset)
  ((points
    :initarg :points
    :accessor dataset-points
    :type (simple-array dvec (*))
    :initform (error "Must specify points of the dataset.")))
  (:documentation
   "Unspecialized data, numeric value and category value are stored in one array."))

(defmethod print-object ((d unspecialized-dataset) stream)
  (call-next-method)
  (format stream "~&DATA POINTS: ~a POINTS~%" (length (dataset-points d))))

;;@ function-type: (string) -> #(dvec) -> unspecialized-dataset
;;@ precondition:
;;@  - length of data > 0
;;@  - length of all-column-names > 0, zero dimension is meaningless
;;@  - dimensions of all-column-names = dimensions of a point
(defun make-unspecialized-dataset (all-column-names data
                                   &key (missing-value-check t)
                                        missing-values-list
                                        (missing-value-test #'equalp))
  (assert (> (length data) 0))
  (assert (> (length all-column-names) 0))
  (assert (every (let ((n (length all-column-names)))
                   (declare (type fixnum n))
                   (lambda (p) (= n (the fixnum (length p))))) data))
  ;; create dimensions

  (let ((dimensions
         (loop
             for n in all-column-names
             for i from 0
             collect (make-dimension n :unknown i) into result
             finally (return (coerce result 'vector)))))
    ;; make dataset

    (make-instance 'unspecialized-dataset
      :dimensions dimensions
      :points (if missing-value-check
                  (let (
                        (test-fcn
                         (if missing-values-list
                             #'(lambda (val) (missing-value-p
                                              val
                                              :missing-values-list missing-values-list
                                              :test missing-value-test))
                             #'(lambda (val)

                               (missing-value-p val :test missing-value-test)))))
                    (do-vec (vec data :setf-var sf :return data)
                      (setf sf (fill-na vec test-fcn))))
                data))))


(defgeneric choice-dimensions (names data)
  (:documentation "Pick up several dimensions as (vector vector)"))
(defgeneric choice-a-dimension (name data)
  (:documentation "Pick up a dimension as vector"))

(defun choice-poses (data poses l)
  (let ((ans (make-array l :element-type t))
        (i 0))
    (dolist (p poses ans)
      (setf (svref ans i) (svref data p))
      (incf i))))


(defmethod choice-dimensions (names (unsp-data unspecialized-dataset))
  (let* ((dims (dataset-dimensions unsp-data))
         (poses (mapcar
                 #'(lambda (x)
                     (dimension-index
                      (find x dims
                            :test #'string=
                            :key #'dimension-name)))
                 names))
         (l (length names)))
    (loop for vec across (dataset-points unsp-data)
        collect (choice-poses vec poses l) into result
        finally (return (coerce result 'vector)))))


(defmethod choice-a-dimension (name (unsp-data unspecialized-dataset))
  (let ((pos (dimension-index
              (find name
                    (dataset-dimensions unsp-data)
                    :test #'string=
                    :key #'dimension-name))))
    (loop for vec across (dataset-points unsp-data)
        collect (svref vec pos) into result
        finally (return (coerce result 'vector)))))

;;;; specialized dataset
(defclass specialized-dataset (dataset)
  ()
  (:documentation
   "Abstract datatype for specialized datasets."))

(defclass numeric-dataset (specialized-dataset)
  ((numeric-points
    :initarg :numeric-points
    :accessor dataset-numeric-points
    :type (simple-array dvec (*))
    :initform (error "Must specify points of the dataset.")))
  (:documentation
   "Dataset specialized in numeric values."))

(defmethod print-object ((d numeric-dataset) stream)
  (call-next-method)
  (format stream "~&NUMERIC DATA POINTS: ~A POINTS~%" (length (dataset-numeric-points d))))

;;@ function-type: (string) -> #(dvec) -> numeric-dataset
;;@ precondition:
;;@  - length of data > 0
;;@  - length of all-column-names > 0, zero dimension is meaningless
;;@  - dimensions of all-column-names = dimensions of a point
;;@  - data is of type (simple-array dvec (*)) (as what 'specialized' means)
(defun make-numeric-dataset (all-column-names specialized-data)
  (assert (> (length specialized-data) 0))
  (assert (> (length all-column-names) 0))
  (assert (= (length all-column-names)
             (length (aref specialized-data 0))))
  (check-type specialized-data simple-vector)
  (check-type (aref specialized-data 0) dvec)
  (let ((dimensions (make-array (length all-column-names))))
    (loop
        for n in all-column-names
        for i from 0
        for d = (make-dimension n :numeric i)
        do (setf (aref dimensions i) d))
    (make-instance 'numeric-dataset
      :dimensions dimensions
      :numeric-points specialized-data)))

(defclass category-dataset (specialized-dataset)
  ((category-points
    :initarg :category-points
    :accessor dataset-category-points
    :type (simple-array cvec (*))
    :initform (error "Must specify points of the dataset.")))
  (:documentation
   "Dataset specialized in category values."))

(defmethod print-object ((d category-dataset) stream)
  (call-next-method)
  (format stream "~&CATEGORY DATA POINTS: ~A POINTS~%" (length (dataset-category-points d))))

;;@ function-type: (string) -> #(dvec) -> numeric-dataset
;;@ precondition:
;;@  - length of data > 0
;;@  - length of all-column-names > 0, zero dimension is meaningless
;;@  - dimensions of all-column-names = dimensions of a point
;;@  - data is of tyep (simple-array dvec (*)) (as what 'specialized' means)
(defun make-category-dataset (all-column-names specialized-data)
  (assert (> (length specialized-data) 0))
  (assert (> (length all-column-names) 0))
  (assert (= (length all-column-names)
             (length (aref specialized-data 0))))
  (check-type specialized-data simple-vector)
  (check-type (aref specialized-data 0) simple-array)
  (let ((dimensions (make-array (length all-column-names))))
    (loop
       for n in all-column-names
       for i from 0
       for table = (make-hash-table :test 'equal #+allegro :values #+allegro nil)
       for d = (make-dimension n :category i
                               :metadata `((:table . ,table)))
       do (setf (aref dimensions i) d))
    ;; compact category values
    (loop
       for d across dimensions
       do (loop
             with i = (dimension-index d)
             with table = (cdr (assoc :table (dimension-metadata d)))
             for p across specialized-data
             for c = (aref p i)
             do (multiple-value-bind (val exist-p)
                    (gethash c table)
                  (if exist-p
                      (setf (aref p i) val)
                      (setf (gethash c table) #+allegro t #-allegro c)))))
    (make-instance 'category-dataset
                   :dimensions dimensions
                   :category-points specialized-data)))

(defclass numeric-and-category-dataset (numeric-dataset category-dataset)
  ()
  (:documentation
   "Dataset specialized in both numeric and category values."))

;;@ function-type: (string) -> #(dvec) -> #(simple-vector) -> hash-table -> numeric-and-category-dataset
;;@ precondition:
;;@  - length of data > 0
;;@  - length of all-column-names > 0
;;@  - length of element of numeric-data = length of numeric-indices
;;@  - length of element of category-data = length of category-indices
;;@  - dimensions of all-column-names = dimensions of numeric-data + dimensions of category-data
;;@  - numeric-data is of type (simple-array dvec (*)), and category-data is of type (simple-array cvec (*))
(defun make-numeric-and-category-dataset (all-column-names numeric-data numeric-indices
                                          category-data category-indices)
  (assert (and (> (length all-column-names) 0)
               (> (length numeric-data) 0)
               (> (length category-data) 0)))
  (assert (= (length all-column-names)
             (+ (length numeric-indices)
                (length category-indices))))
  (check-type numeric-data simple-vector)
  (check-type (aref numeric-data 0) dvec)
  (check-type category-data simple-vector)
  (check-type (aref category-data 0) simple-vector)
  (let ((dimensions
         (make-array (+ (length numeric-indices) (length category-indices)))))
    (loop
        for i from 0
        for index in numeric-indices
        for d = (make-dimension (nth index all-column-names) :numeric i)
        do (setf (aref dimensions index) d))
    (loop
        for i from 0
        for index in category-indices
        for table = (make-hash-table :test 'equal #+allegro :values #+allegro nil)
        for d = (make-dimension (nth index all-column-names) :category i
                                :metadata `((:table . ,table)))
        do (setf (aref dimensions index) d))
    ;; compact category values
    (loop
        for d across dimensions
        when (eq (dimension-type d) :category)
        do (loop
               with i = (dimension-index d)
               with table = (cdr (assoc :table (dimension-metadata d)))
               for p across category-data
               for c = (aref p i)
               do (multiple-value-bind (val exist-p)
                      (gethash c table)
                    (if exist-p
                        (setf (aref p i) val)
                      (setf (gethash c table) #+allegro t #-allegro c)))))
    ;; finally
    (make-instance 'numeric-and-category-dataset
      :dimensions dimensions
      :numeric-points numeric-data
      :category-points category-data)))

;;; dataset represented as multi-dimensional array
(defclass numeric-matrix-dataset (specialized-dataset)
  ((numeric-points
    :initarg :numeric-points
    :accessor dataset-numeric-points
    :type dmat
    :initform (error "Must specify points of the dataset.")))
  (:documentation
   "Dataset represented as matrix (2-dim CL array)"))

(defmethod print-object ((d numeric-matrix-dataset) stream)
  (call-next-method)
  (format stream "~&NUMERIC-MATRIX DATA POINTS: ~A POINTS~%" (array-dimension (dataset-numeric-points d) 0)))

;;@ function-type: string -> dmat -> numeric-matrix-dataset
;;@ precondition:
;;@  - dim_1 of data > 0
;;@  - length of all-column-names > 0, zero dimension is meaningless
;;@  - dimensions of all-column-names = dimensions of a point
;;@  - data is of type dmat (as what 'specialized' means)
(defun make-numeric-matrix-dataset (all-column-names specialized-matrix-data)
  (assert (> (array-dimension specialized-matrix-data 0) 0))
  (assert (> (length all-column-names) 0))
  (assert (= (length all-column-names)
             (array-dimension specialized-matrix-data 1)))
  (check-type specialized-matrix-data dmat)
  (let ((dimensions (make-array (length all-column-names))))
    (loop
      for n in all-column-names
      for i from 0
      for d = (make-dimension n :numeric i)
      do (setf (aref dimensions i) d))
    (make-instance 'numeric-matrix-dataset
                   :dimensions dimensions
                   :numeric-points specialized-matrix-data)))


(defclass numeric-matrix-and-category-dataset (numeric-matrix-dataset category-dataset)
  ()
  (:documentation
   "Dataset specialized in both numeric (as matrix) and category values."))

;;@ function-type: (string) -> #(dvec) -> #(simple-vector) -> hash-table -> numeric-matrix-and-category-dataset
;;@ precondition:
;;@  - nrow of data > 0
;;@  - length of all-column-names > 0
;;@  - length of element of numeric-data = length of numeric-indices
;;@  - length of element of category-data = length of category-indices
;;@  - dimensions of all-column-names = dimensions of numeric-data + dimensions of category-data
;;@  - numeric-data is of type dmat, and category-data is of type (simple-array cvec (*))
(defun make-numeric-matrix-and-category-dataset (all-column-names numeric-data numeric-indices
                                                 category-data category-indices)
  (assert (and (> (length all-column-names) 0)
               (> (array-dimension numeric-data 0) 0)
               (> (length category-data) 0)))
  (assert (= (length all-column-names)
             (+ (length numeric-indices)
                (length category-indices))))
  (check-type numeric-data dmat)
  (check-type category-data simple-vector)
  (check-type (aref category-data 0) simple-vector)
  (let ((dimensions
         (make-array (+ (length numeric-indices) (length category-indices)))))
    (loop
      for i from 0
      for index in numeric-indices
      for d = (make-dimension (nth index all-column-names) :numeric i)
      do (setf (aref dimensions index) d))
    (loop
      for i from 0
      for index in category-indices
      for table = (make-hash-table :test 'equal #+allegro :values #+allegro nil)
      for d = (make-dimension (nth index all-column-names) :category i
                   :metadata `((:table . ,table)))
      do (setf (aref dimensions index) d))
    ;; compact category values
    (loop
      for d across dimensions
      when (eq (dimension-type d) :category)
        do (loop
             with i = (dimension-index d)
             with table = (cdr (assoc :table (dimension-metadata d)))
             for p across category-data
             for c = (aref p i)
             do (multiple-value-bind (val exist-p)
                    (gethash c table)
                  (if exist-p
                      (setf (aref p i) val)
                      (setf (gethash c table) #+allegro t #-allegro c)))))
    ;; finally
    (make-instance 'numeric-matrix-and-category-dataset
                   :dimensions dimensions
                   :numeric-points numeric-data
                   :category-points category-data)))


;(defgeneric dataset-points (dataset))
(defmethod dataset-points ((dataset specialized-dataset))
  (ecase (type-of dataset)
    (numeric-and-category-dataset
       (with-accessors ((c-ps dataset-category-points)
                        (n-ps dataset-numeric-points)
                        (dims dataset-dimensions)) dataset
         (coerce
          (loop for row below (length c-ps)
                as vec = (make-array (length dims) :element-type t)
                collect
             (loop for dim across dims
                   for col from 0
                   as type = (dimension-type dim)
                   as index = (dimension-index dim)
                   do (setf (svref vec col)
                            (aref (svref (ecase type (:numeric n-ps) (:category c-ps)) row) index))
                   finally (return vec)))
          'vector)))
    (numeric-dataset (dataset-numeric-points dataset))
    (category-dataset (dataset-category-points dataset))
    ((numeric-matrix-dataset numeric-matrix-and-category-dataset)
       (error "It's fairly inefficient to do that, better keep the original unspecialized dataset."))))




;;;; read and process data
;;@ function-type: string -> unspecialized-dataset
(defun read-data-from-file (filename &key
                                     (type :sexp)
                                     (external-format :default external-format-p)
                                     csv-type-spec
                                     (csv-header-p t)
                                     (csv-delimiter #\,)
                                     (missing-value-check t)
                                     missing-values-list)
  "Reads an unspecialized dataset from file.
   - If TYPE is :SEXP or NIL (the default), a list is read from a s-expression
file.  The first element is a list of column names and the rest of the elements
are the  data.  EXTERNAL-FORMAT defaults to :DEFAULT for opening the file.
   - If TYPE is :CSV, the data is read as csv. The normal convention is first
line is column name.  However if CSV-HEADER-P is a list of strings then
CSV-HEADER-P specifies the column names.  EXTERNAL-FORMAT defaults to shift-jis."
  (when (and (not external-format-p)
             (eql type :csv))
    (setf external-format clml.utility.csv::*csv-default-external-format*))
  (with-open-file (stream filename :external-format external-format)
    #+allegro (setf (excl:eol-convention f) :anynl-dos)
    (read-data-from-stream stream
                            :type type
                            :csv-type-spec csv-type-spec
                            :csv-header-p csv-header-p
                            :csv-delimiter csv-delimiter
                            :missing-value-check missing-value-check
                            :missing-values-list missing-values-list)))


;;;; read and process data
;;@ function-type: string -> unspecialized-dataset
(defun read-data-from-stream (stream &key
                                     (type :csv)
                                     csv-type-spec
                                     (csv-header-p t)
                                     (csv-delimiter #\,)
                                     (missing-value-check t)
                                     (missing-values-list nil missing-values-list-p))
  "Reads an unspecialized dataset from file.
   - If TYPE is :SEXP or NIL, a list is read from a s-expression file.
The first element is a list of column names and the rest of the elements are the
data.
   - If TYPE is :CSV (the default), the data is read as csv. The normal
convention is first line is column name.  However if CSV-HEADER-P is a list
of strings then CSV-HEADER-P specifies the column names.
   - If TYPE is :ARFF, the data is read as arff.  By default, NIL and \"?\"
are used as missing values with arff format."
  (assert (member type '(:sexp :csv :arff)))
  (ecase type
    ((:sexp nil)
     (let ((tmp (with-standard-io-syntax
                  (let ((*read-eval* nil)
                        (*read-default-float-format* 'double-float))
                    (read stream)))))
       (make-unspecialized-dataset
         (first tmp)
         (map 'vector
           (lambda (p)
             (coerce p 'vector))
           (rest tmp))
         :missing-value-check missing-value-check
         :missing-values-list missing-values-list)))
    (:csv
      (multiple-value-bind (data header)
        (clml.utility.csv:read-csv-stream stream :header csv-header-p :type-spec csv-type-spec :delimiter csv-delimiter)
        (make-unspecialized-dataset (coerce header 'list) data
                                    :missing-value-check missing-value-check
                                    :missing-values-list missing-values-list)))
    (:arff
      ;arff uses ? as a missing value symbol, so use that unless told otherwise
      (unless missing-values-list-p
        (setf missing-values-list '(nil "?")))
      (multiple-value-bind (data header)
                           (clml.utility.arff:read-arff-stream stream)
                           (make-unspecialized-dataset (coerce header 'list) data
                                                       :missing-value-check missing-value-check
                                                       :missing-values-list missing-values-list)))))

;;; function-type: unspecialized-dataset -> specialized-dataset

(defgeneric pick-and-specialize-data (d  &key
                                     range
                                     except
                                     data-types ; :numeric | :category
                                     store-numeric-data-as-matrix
                                     )
 )
(defmethod pick-and-specialize-data ((d unspecialized-dataset) &key
                                     (range :all)
                                     except
                                     data-types ; :numeric | :category
                                     store-numeric-data-as-matrix
                                     )
  (assert (not (null data-types)))
  (assert (every (lambda (type) (member type '(:numeric :category))) data-types))
  (let* ((dimensions (dataset-dimensions d))
         (total-size (length dimensions))
         (range1 (if (eq range :all)
                     (loop for i below total-size collect i)
                     range))
         (range (sort (set-difference range1 except) #'<)) ; destructive on range
         (numeric-indices)              ;indices in original dataset
         (category-indices)
         (numeric-indices-new)          ;indices in new specialized dataset
         (category-indices-new))
    (assert (= (length range) (length data-types)))
    (loop
      for index in range
      for i from 0
      for dt in data-types
      do (ecase dt
           (:numeric
              (push i numeric-indices-new)
              (push index numeric-indices))
           (:category
              (push i category-indices-new)
              (push index category-indices)))
      finally (progn
                (setf numeric-indices (nreverse numeric-indices))
                (setf category-indices (nreverse category-indices))
                (setf numeric-indices-new (nreverse numeric-indices-new))
                (setf category-indices-new (nreverse category-indices-new))))
    ;;
    (let ((all-column-names
           (loop
             with dimensions = (dataset-dimensions d)
             for index in range
             for name = (dimension-name (aref dimensions index))
             collect name)))
      (cond
        ;; numeric-dataset
        ((null category-indices-new)
         (let* ((size (length numeric-indices-new))
                (data
                 (map 'vector
                      (lambda (p) (declare (simple-vector p))
                        (let* ((sp (make-dvec size)))
                          (declare (type dvec sp))
                          (loop
                            for index of-type array-index in numeric-indices
                            for i of-type array-index from 0
                            as val = (aref p index)
                            do (setf (aref sp i)
                                     (if (na-p val) +nan+ (coerce val 'double-float)))
                            finally (return sp))))
                      (dataset-points d)))
                (make-func (if store-numeric-data-as-matrix
                               #'make-numeric-matrix-dataset
                               #'make-numeric-dataset)))
           (when store-numeric-data-as-matrix
             (setf data (vecs2mat data)))
           (funcall make-func all-column-names data)))
        ;; category-dataset
        ((null numeric-indices-new)
         (let* ((size (length category-indices-new))
                (data
                 (map 'vector
                      (lambda (p)
                        (declare (simple-vector p))
                        (let* ((sp (make-array size)))
                          (declare (type simple-vector sp))
                          (loop
                            for index of-type array-index in category-indices
                            for i of-type array-index from 0
                            as val = (aref p index)
                            do (setf (aref sp i) (if (na-p val) +c-nan+ (aref p index)))
                            finally (return sp))))
                      (dataset-points d))))
           (make-category-dataset all-column-names data)))
        ;; numeric and category dataset
        (t
         (let* ((numeric-data-size (length numeric-indices-new))
                (category-data-size (length category-indices-new))
                (numeric-data
                 (map 'vector
                      (lambda (p)
                        (declare (simple-vector p))
                        (let* ((sp (make-dvec numeric-data-size)))
                          (declare (type dvec sp))
                          (loop
                            for index of-type array-index in numeric-indices
                            for i of-type array-index from 0
                            as val = (aref p index)
                            do (setf (aref sp i)
                                     (if (na-p val) +nan+ (coerce (aref p index) 'double-float)))
                            finally (return sp))))
                      (dataset-points d)))
                (category-data
                 (map 'vector
                      (lambda (p)
                        (declare (simple-vector p))
                        (let* ((sp (make-array category-data-size)))
                          (declare (type simple-vector sp))
                          (loop
                            for index of-type array-index in category-indices
                            for i of-type array-index from 0
                            as val = (aref p index)
                            do (setf (aref sp i) (if (na-p val) +c-nan+ (aref p index)))
                            finally (return sp))))
                      (dataset-points d)))
                (make-func (if store-numeric-data-as-matrix
                               #'make-numeric-matrix-and-category-dataset
                               #'make-numeric-and-category-dataset)))
           (when store-numeric-data-as-matrix
             (setf numeric-data (vecs2mat numeric-data)))
           (funcall make-func
                    all-column-names
                    numeric-data
                    numeric-indices-new
                    category-data
                    category-indices-new)))))))

(defgeneric divide-dataset (dataset &key divide-ratio random range except)
  (:documentation "Divide dataset and restrict column"))

(defmethod divide-dataset ((unsp-d unspecialized-dataset)
                           &key divide-ratio
                                random
                                (range :all)
                                except)
  (unless divide-ratio (setq divide-ratio '(1 0)))
  (assert (every (lambda (r) (and (numberp r) (integerp r) (not (minusp r)))) divide-ratio))
  (let* ((dimensions (dataset-dimensions unsp-d))
         (dim (length dimensions))
         (row-indexes
          (loop with total-size = (length (dataset-points unsp-d))
              with row-list = (loop for i below total-size collect i)
              with total-r = (apply #'+ divide-ratio)
              for r in divide-ratio
              as c = (floor (* total-size (/ r total-r)))
              collect (loop repeat c
                          as pos = (if random (random (length row-list)) 0)
                          as val = (nth pos row-list)
                          collect (progn (setf row-list
                                           (remove val row-list :test #'eql
                                                   :start pos :end (1+ pos)))
                                         val)) into row-indexes
              finally (return
                        (progn (when row-list
                                 (mapcar (lambda (i) (push i (car (last row-indexes))))
                                         row-list))
                               row-indexes))))
         (range1 (if (eq range :all)
                     (loop for i below dim collect i)
                   (copy-seq range)))
         (range (sort (set-difference range1 except) #'<)))
    (assert (<= 1 (length range) (length dimensions)))
    (let ((all-column-names
           (loop for index in range
               for name = (dimension-name (aref dimensions index))
               collect name)))
      (apply #'values
             (loop for rows in row-indexes
                 as points = (map 'vector
                               (let ((pts (dataset-points unsp-d)))
                                 (lambda (row)
                                   (map 'vector
                                     (lambda (i)
                                       (let ((pt (svref pts row)))
                                         (declare (type (simple-array t (*)) pt))
                                         (svref pt i))) range)))
                               (sort rows #'<))
                 when (plusp (length points))
                 collect (make-unspecialized-dataset
                          all-column-names points :missing-value-check nil))))))

(defmethod divide-dataset ((specialized-d specialized-dataset)
                           &key divide-ratio
                           random
                           (range :all)
                           except)
  (unless divide-ratio (setq divide-ratio '(1 0)))

  (assert (every (lambda (r)
                   (and (numberp r) (integerp r) (not (minusp r)))) divide-ratio))
  (let* ((dimensions (dataset-dimensions specialized-d))
         (dim (length dimensions))

         (row-indexes
          (loop with total-size =
                                (length (etypecase specialized-d
                                          (category-dataset (dataset-category-points specialized-d))
                                          (numeric-dataset (dataset-numeric-points specialized-d))
                                          (numeric-and-category-dataset (dataset-category-points specialized-d))))
                with row-list = (loop for i below total-size collect i)
                with total-r = (apply #'+ divide-ratio)
                for r in divide-ratio
                as c = (floor (* total-size (/ r total-r)))
                collect (loop repeat c
                              as pos = (if random (random (length row-list)) 0)
                              as val = (nth pos row-list)
                              collect (progn (setf row-list
                                                   (remove val row-list :test #'eql
                                                           :start pos :end (1+ pos)))
                                             val)) into row-indexes
                finally (return
                          (progn (when row-list
                                   (mapcar (lambda (i) (push i (car (last row-indexes))))
                                           row-list))
                                 row-indexes))))
         (range1 (if (eq range :all)
                     (loop for i below dim collect i)
                     (copy-seq range)))
         (range (sort (set-difference range1 except) #'<))
         (numeric-indices)              ;indices in original dataset
         (category-indices)
         (numeric-indices-new)          ;indices in new specialized dataset
         (category-indices-new))
    (assert (<= 1 (length range) (length dimensions)))
    (loop for index in range
          for i from 0
          for dt in (mapcar (lambda (n) (svref dimensions n)) range)
          do (ecase (dimension-type dt)
               (:numeric
                  (push i numeric-indices-new)
                  (push index numeric-indices))
               (:category
                  (push i category-indices-new)
                  (push index category-indices)))
          finally (progn
                    (setf numeric-indices (nreverse numeric-indices))
                    (setf category-indices (nreverse category-indices))
                    (setf numeric-indices-new (nreverse numeric-indices-new))
                    (setf category-indices-new (nreverse category-indices-new))))
    (let ((all-column-names
           (loop for index in range
                 for name = (dimension-name (aref dimensions index))
                 collect name)))
      (apply #'values
             (loop for rows in row-indexes
                   as points = (mapcar (let ((ps (dataset-points specialized-d)))
                                         (lambda (r) (svref ps r)))
                                       (sort rows #'<))
                   when points
                     collect
                  (cond
                    ;; numeric-dataset
                    ((null category-indices-new)
                     (let* ((size (length numeric-indices-new))
                            (data
                             (map 'vector
                                  (lambda (p)
                                    (let* ((sp (make-dvec size)))
                                      (declare (type dvec sp))
                                      (loop
                                        for index of-type array-index in numeric-indices
                                        for i of-type array-index from 0
                                        as val = (aref p index)
                                        do (setf (aref sp i)
                                                 (if (na-p val) +nan+ (coerce val 'double-float)))
                                        finally (return sp))))
                                  points)))
                       (make-numeric-dataset all-column-names data)))
                    ;; category-dataset
                    ((null numeric-indices-new)
                     (let* ((size (length category-indices-new))
                            (data
                             (map 'vector
                                  (lambda (p)
                                    (let* ((sp (make-array size)))
                                      (declare (type simple-vector sp))
                                      (loop
                                        for index of-type array-index in category-indices
                                        for i of-type array-index from 0
                                        as val = (aref p index)
                                        do (setf (aref sp i) (if (na-p val) +c-nan+ (aref p index)))
                                        finally (return sp))))
                                  points)))
                       (make-category-dataset all-column-names data)))
                    ;; numeric and category dataset
                    (t
                     (let* ((numeric-data-size (length numeric-indices-new))
                            (category-data-size (length category-indices-new))
                            (numeric-data
                             (map 'vector
                                  (lambda (p)
                                    (let* ((sp (make-dvec numeric-data-size)))
                                      (declare (type dvec sp))
                                      (loop
                                        for index of-type array-index in numeric-indices
                                        for i of-type array-index from 0
                                        as val = (aref p index)
                                        do (setf (aref sp i)
                                                 (if (na-p val) +nan+ (coerce (aref p index) 'double-float)))
                                        finally (return sp))))
                                  points))
                            (category-data
                             (map 'vector
                                  (lambda (p)
                                    (declare (simple-vector p))
                                    (let* ((sp (make-array category-data-size)))
                                      (declare (type simple-vector sp))
                                      (loop
                                        for index of-type array-index in category-indices
                                        for i of-type array-index from 0
                                        as val = (aref p index)
                                        do (setf (aref sp i) (if (na-p val) +c-nan+ (aref p index)))
                                        finally (return sp))))
                                  points)))
                       (make-numeric-and-category-dataset
                        all-column-names
                        numeric-data numeric-indices-new
                        category-data category-indices-new)))))))))

(defgeneric copy-dataset (data))
(defmethod copy-dataset ((dataset dataset))
  (flet ((copy-dims (d) (map 'vector #'copy-dimension (dataset-dimensions d)))
         (copy-pts (pts) (map 'vector #'copy-seq pts)))
    (etypecase dataset
      (numeric-and-category-dataset
         (make-instance 'numeric-and-category-dataset
                        :dimensions (copy-dims dataset)
                        :numeric-points (copy-pts (dataset-numeric-points dataset))
                        :category-points (copy-pts (dataset-category-points dataset))))
      (numeric-matrix-and-category-dataset
         (make-instance 'numeric-matrix-and-category-dataset
                        :dimensions (copy-dims dataset)
                        :numeric-points (copy-mat (dataset-numeric-points dataset))
                        :category-points (copy-pts (dataset-category-points dataset))))
      (numeric-dataset
         (make-instance 'numeric-dataset
                        :dimensions (copy-dims dataset)
                        :numeric-points (copy-pts (dataset-numeric-points dataset))))
      (numeric-matrix-dataset
         (make-instance 'numeric-matrix-dataset
                        :dimensions (copy-dims dataset)
                        :numeric-points (copy-mat (dataset-numeric-points dataset))))
      (category-dataset
         (make-instance 'category-dataset
                        :dimensions (copy-dims dataset)
                        :category-points (copy-pts (dataset-category-points dataset))))
      (unspecialized-dataset
         (make-instance 'unspecialized-dataset
                        :dimensions (copy-dims dataset)
                        :points (copy-pts (dataset-points dataset)))))))

(defmethod choice-dimensions (names (data specialized-dataset))
  (with-accessors ((dims dataset-dimensions)
                   (pts dataset-points)) data
    (let* ((poses (loop for name in names
                      collect (position name dims :key #'dimension-name :test #'string=)))
           (types (mapcar (lambda (pos) (dimension-type (aref dims pos))) poses))
           (type (cond ((every (lambda (ty) (eq ty :numeric)) types) :numeric)
                       ((every (lambda (ty) (eq ty :category)) types) :category)
                       (t t))))
      (when poses
        (loop for pt across pts
            as new-pt = (mapcar (lambda (pos) (aref pt pos)) poses)
            collect (case type
                      (:numeric (coerce new-pt 'dvec))
                      (:category (coerce new-pt 'vector)) ;;
                      (t (coerce new-pt 'vector))) into result
            finally (return (coerce result 'vector)))))))

(defmethod choice-a-dimension (name (data specialized-dataset))
  (multiple-value-bind (pos type)
      (loop for pos from 0
          for dim across (dataset-dimensions data)
          when (string= (dimension-name dim) name)
          return (values pos (dimension-type dim)))
    (when pos
      (loop for vec across (dataset-points data)
          collect (aref vec pos) into result
          finally (return (ecase type
                            (:numeric (coerce result 'dvec))
                            (:category (coerce result 'vector)) ;;
                            ))))))
(defmacro !! (dataset name)
  "Alias for choice a dimension
@Params dataset
        name - list of names or a single name
"
  `(etypecase ,name
    (string (choice-a-dimension ,name ,dataset))
    (list (choice-dimensions ,name ,dataset)))
  )

(defgeneric make-bootstrap-sample (dataset))
(defmethod make-bootstrap-sample (dataset)
  (let* ((data-pts (dataset-points dataset))
         (n (length data-pts))
         (new-data-pts (make-array n :element-type t)))
    (declare (type vector data-pts new-data-pts) (type fixnum n))
    (loop for i of-type fixnum below n
        do (setf (svref new-data-pts i)
             (copy-seq (svref data-pts (the fixnum (random n))))))
    new-data-pts))

(defgeneric make-bootstrap-sample-datasets (dataset &key number-of-datasets))
(defmethod make-bootstrap-sample-datasets ((dataset dataset) &key (number-of-datasets 10))
  (assert (and (integerp number-of-datasets) (> number-of-datasets 0)))
  (flet ((copy-dims (d) (map 'vector #'copy-dimension (dataset-dimensions d)))
         (pick-up-poses (arry poses) (mapcar (lambda (pos) (aref arry pos)) poses)))
    (loop repeat number-of-datasets
        as pts = (make-bootstrap-sample dataset)
        collect
          (etypecase dataset
            (numeric-and-category-dataset
             (make-instance 'numeric-and-category-dataset
               :dimensions (copy-dims dataset)
               :numeric-points
               (map 'vector
                 (let ((poses (loop for pos from 0
                                  for dim across (dataset-dimensions dataset)
                                  when (eq :numeric (dimension-type dim)) collect pos)))
                   (lambda (pt) (coerce (pick-up-poses pt poses) 'dvec))) pts)
               :category-points
               (map 'vector
                 (let ((poses (loop for pos from 0
                                  for dim across (dataset-dimensions dataset)
                                  when (eq :category (dimension-type dim)) collect pos)))
                   (lambda (pt) (coerce (pick-up-poses pt poses) 'vector))) ;;
                 pts)))
            (numeric-dataset (make-instance 'numeric-dataset
                               :dimensions (copy-dims dataset)
                               :numeric-points pts))
            (category-dataset (make-instance 'category-dataset
                                :dimensions (copy-dims dataset)
                                :category-points pts))
            (unspecialized-dataset (make-instance 'unspecialized-dataset
                                     :dimensions (copy-dims dataset)
                                     :points pts))))))


 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; dataset cleaning (outlier + interpolate) ;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-constant +known-outlier-types+
    '(:numeric (:std-dev :mean-dev :user :smirnov-grubbs)
      :category (:user :freq)) :test #'equal)
(define-constant +known-interp-types+
    '(:numeric (:zero :min :max :mean :median :spline)
      :category (:mode)) :test #'equal)

(defmacro outlier-points (points outlier-types outlier-values &key (type :numeric))
  (let (tr-fcn vec-type)
    (case type
      (:numeric (setq tr-fcn 'transposeV vec-type 'dvec))
      (:category (setq tr-fcn 'trans vec-type 'vector))
      (t (error "invalid type | ~A" type)))
    (let ((outlier-types-t (gensym))
          (outlier-values-t (gensym))
          (tr-data-t (gensym)))
      `(let ((,tr-data-t (funcall ',tr-fcn ,points)))
         (let ((,outlier-types-t (if ,outlier-types ,outlier-types
                                     (make-list (length ,tr-data-t) :initial-element nil)))
               (,outlier-values-t (if ,outlier-values ,outlier-values
                                      (make-list (length ,tr-data-t) :initial-element nil))))
           (assert (every (lambda (val)
                            (or (null val) (member val (getf +known-outlier-types+ ,type))))
                          ,outlier-types-t))
           (assert (= (length ,tr-data-t) (length ,outlier-types-t) (length ,outlier-values-t)))
           (funcall ',tr-fcn
                    (do-vec (vec ,tr-data-t :type ,vec-type :index-var i :setf-var sf :return ,tr-data-t)
                      (let ((outlier-type (elt ,outlier-types-t i))
                            (value (elt ,outlier-values-t i)))
                        (when outlier-type
                          (setf sf (outlier-verification
                                    vec :type outlier-type :outlier-value value :seq-type ,type)))))))))))

(defmacro interp-points (points interp-types &key (type :numeric))
  (let ((interp-types-t (gensym))
        tr-fcn vec-type)
    (case type
      (:numeric (setq tr-fcn 'transposeV vec-type 'dvec))
      (:category (setq tr-fcn 'trans vec-type 'vector))
      (t (error "invalid type | ~A" type)))
    `(let ((tr-data (funcall ',tr-fcn ,points)))
       (let ((,interp-types-t (if ,interp-types ,interp-types (make-list (length tr-data) :initial-element nil))))
         (assert (every (lambda (val)
                          (or (null val) (member val (getf +known-interp-types+ ,type))))
                        ,interp-types-t))
         (assert (= (length tr-data) (length ,interp-types-t)))
         (funcall ',tr-fcn
                  (do-vec (vec tr-data :type ,vec-type :index-var i :setf-var sf :return tr-data)
                    (let ((interp-type (elt ,interp-types-t i)))
                      (declare (type symbol interp-type))
                      (when interp-type
                        (setf sf (interpolate vec :interp interp-type :seq-type ,type))))))))))

(defmacro clean-points (points interp-types outlier-types outlier-values
                        &key (type :numeric))
  `(interp-points
    (outlier-points ,points ,outlier-types ,outlier-values :type ,type)
    ,interp-types :type ,type))

(defun convert-cleaning-alist-to-list (names interp-types-alist outlier-types-alist outlier-values-alist)
  (values (loop for name in names
              collect (cdr (assoc name interp-types-alist :test #'string=)))
          (loop for name in names
              collect (cdr (assoc name outlier-types-alist :test #'string=)))
          (loop for name in names
              as outlier-type = (cdr (assoc name outlier-types-alist :test #'string=))
              collect (when outlier-type
                        (cdr (assoc outlier-type outlier-values-alist :test #'eq))))))

(defgeneric dataset-cleaning (d &key interp-types-alist outlier-types-alist outlier-values-alist)
  (:documentation "Cleaning: Outlier verification and Interpolation
- arguments:
  - d : <dataset>
  - interp-types-alist   : a-list (key: column name, datum: interpolation(:zero :min :max :mean :median :mode :spline)) | nil\\
  - outlier-types-alist  : a-list (key: column name, datum: outlier-verification(:std-dev :mean-dev :user :smirnov-grubbs :freq)) | nil\\
  - outlier-values-alist : a-list (key: outlier-verification datum: the value according to outlier-verification) | nil
- example:
#+BEGIN_SRC
(clml.hjs.read-data:dataset-cleaning (dataset-points dataset)
                                     :outlier-types-alist '((\"hits\" . :std-dev))
                                     :outlier-values-alist '((:std-dev . 1))
                                     :interp-types-alist '((\"hits\" . :max)))
#+END_SRC
"))

(defmethod dataset-cleaning ((d numeric-dataset)
                             &key interp-types-alist
                                  outlier-types-alist
                                  outlier-values-alist)
  (let ((names (map 'list #'dimension-name (dataset-dimensions d))))
    (multiple-value-bind (interp-types outlier-types outlier-values)
        (convert-cleaning-alist-to-list names
                                        interp-types-alist
                                        outlier-types-alist
                                        outlier-values-alist)
      (make-numeric-dataset names (clean-points (dataset-numeric-points d)
                                                interp-types
                                                outlier-types
                                                outlier-values
                                                :type :numeric)))))

(defmethod dataset-cleaning ((d category-dataset)
                             &key interp-types-alist
                                  outlier-types-alist
                                  outlier-values-alist)
  (let ((names (map 'list #'dimension-name (dataset-dimensions d))))
    (multiple-value-bind (interp-types outlier-types outlier-values)
        (convert-cleaning-alist-to-list names
                                        interp-types-alist
                                        outlier-types-alist
                                        outlier-values-alist)
      (make-category-dataset names (clean-points (dataset-category-points d)
                                                 interp-types
                                                 outlier-types
                                                 outlier-values
                                                 :type :category)))))

(defmethod dataset-cleaning ((d numeric-and-category-dataset)
                             &key interp-types-alist
                                  outlier-types-alist
                                  outlier-values-alist)
  (let* ((dims (dataset-dimensions d))
         (names (map 'list #'dimension-name dims)))
    (multiple-value-bind (interp-types outlier-types outlier-values)
        (convert-cleaning-alist-to-list names
                                        interp-types-alist
                                        outlier-types-alist
                                        outlier-values-alist)
      (loop for dim across dims
          for i from 0
          as type = (dimension-type dim)
          if (eq type :numeric)
          collect (elt interp-types i) into num-interp and
          collect (elt outlier-types i) into num-out-types and
          collect (elt outlier-values i) into num-out-vals and
          collect i into num-indices
          else if (eq type :category)
          collect (elt interp-types i) into cate-interp and
          collect (elt outlier-types i) into cate-out-types and
          collect (elt outlier-values i) into cate-out-vals and
          collect i into cate-indices
          finally
            (return
              (make-numeric-and-category-dataset names
                                                 (clean-points (dataset-numeric-points d)
                                                               num-interp
                                                               num-out-types
                                                               num-out-vals
                                                               :type :numeric)
                                                 num-indices
                                                 (clean-points (dataset-category-points d)
                                                               cate-interp
                                                               cate-out-types
                                                               cate-out-vals
                                                               :type :category)
                                                 cate-indices))))))

(defgeneric dedup-dataset! (dataset))
(defmethod dedup-dataset! ((dataset dataset))
  (etypecase dataset
      (numeric-and-category-dataset (error "Unimplemented"))
      (numeric-matrix-and-category-dataset (error "Unimplemented"))
      (numeric-dataset (delete-duplicates (dataset-numeric-points dataset)))
      (numeric-matrix-dataset (error "Unimplemented"))
      (category-dataset (delete-duplicates (dataset-category-points dataset)))
      (unspecialized-dataset (delete-duplicates (dataset-points dataset)))
    )
  )

(defgeneric shuffle-dataset! (dataset))
(defmethod shuffle-dataset! ((dataset dataset))
  "Return copy of vector with elements shuffled like a deck of cards."
  (flet ((shuffle-vector (v)
           "Shuffle-vector courtsey http://osdir.com/ml/lisp.sources.code/2004-06/msg00009.html"
            (loop
               with result = (copy-seq v)
               finally (return result)
               for i from (length v) downto 1
               as j = (random i)
               do (rotatef (svref result j) (svref result (1- i))))))
    (etypecase dataset
      (numeric-and-category-dataset (error "Unimplemented"))
      (numeric-matrix-and-category-dataset (error "Unimplemented"))
      (numeric-dataset (setf (dataset-numeric-points dataset) (shuffle-vector (dataset-numeric-points dataset))))
      (numeric-matrix-dataset (error "Unimplemented"))
      (category-dataset (setf (dataset-category-points dataset) (shuffle-vector (dataset-category-points dataset))))
      (unspecialized-dataset (setf (dataset-points dataset) (shuffle-vector (dataset-points dataset)))))
    ))

(defgeneric add-dim (d name type &key points initial-value metadata))
(defmethod add-dim ((dataset category-dataset) name type &key points initial-value metadata)
  (flet ((copy-dims (d) (map 'vector #'copy-dimension (dataset-dimensions d)))
         (copy-pts (pts) (map 'vector #'copy-seq pts)))
    (let ((new-points (if points points (make-array (list (length (dataset-points dataset))) :initial-element initial-value))))

      (ecase type
        (:category
         (let ((new-dim (make-dimension name type (length (dataset-dimensions dataset)) :metadata metadata)))
           (make-instance 'category-dataset
                          :category-points (loop for vec across (dataset-points dataset)
                                              for val across new-points
                                              collect (concatenate 'vector vec (vector val) ) into result
                                              finally (return (coerce result 'vector))
                                                )
                          :dimensions (concatenate 'vector (copy-dims dataset) (vector new-dim))
                          )))
        (:numeric
         (let ((new-dim (make-dimension name type 0
                                        :metadata metadata)))
           (make-instance 'numeric-and-category-dataset
                          :category-points (copy-pts (dataset-points dataset))
                          :numeric-points (map 'vector #'vector new-points)

                          :dimensions (concatenate 'vector (copy-dims dataset) (vector new-dim))

                                                                                                                             ))))
      )))

(defmethod add-dim ((dataset numeric-dataset) name type &key points initial-value metadata)
  (flet ((copy-dims (d) (map 'vector #'copy-dimension (dataset-dimensions d)))
         (copy-pts (pts) (map 'vector #'copy-seq pts)))
    (let ((new-points (if points points (make-array (list (length (dataset-points dataset))) :initial-element initial-value))))

      (ecase type
        (:numeric
         (let ((new-dim (make-dimension name type (length (dataset-dimensions dataset)) :metadata metadata)))
           (make-instance 'numeric-dataset
                          :numeric-points (loop for vec across (dataset-points dataset)
                                              for val across new-points
                                              collect (concatenate 'vector vec (vector val) ) into result
                                              finally (return (coerce result 'vector))
                                                )
                          :dimensions (concatenate 'vector (copy-dims dataset) (vector new-dim))
                          )))
        (:category
         (let ((new-dim (make-dimension name type 0
                                        :metadata metadata)))
           (make-instance 'numeric-and-category-dataset
                          :numeric-points (copy-pts (dataset-points dataset))
                          :category-points (map 'vector #'vector new-points)

                          :dimensions (concatenate 'vector (copy-dims dataset) (vector new-dim))

                                                                                                                             ))))
      )))

(defmethod add-dim ((dataset numeric-and-category-dataset) name type &key points initial-value metadata)
  (flet ((copy-dims (d) (map 'vector #'copy-dimension (dataset-dimensions d)))
         (copy-pts (pts) (map 'vector #'copy-seq pts)))
    (let ((new-points (if points points (make-array (list (length (dataset-points dataset))) :initial-element initial-value))))

      (ecase type
        (:numeric
         (let ((new-dim (make-dimension name type (length (elt (dataset-numeric-points dataset) 0)) :metadata metadata)))
           (make-instance 'numeric-and-category-dataset
                          :category-points (copy-pts (dataset-category-points dataset))
                          :numeric-points (loop for vec across (dataset-numeric-points dataset)
                                              for val across new-points
                                              collect (concatenate 'vector vec (vector val) ) into result
                                              finally (return (coerce result 'vector))
                                                )
                          :dimensions (concatenate 'vector (copy-dims dataset) (vector new-dim))
                          )))
        (:category
         (let ((new-dim (make-dimension name type (length (elt (dataset-category-points dataset) 0))
                                        :metadata metadata)))
           (make-instance 'numeric-and-category-dataset
                          :numeric-points (copy-pts (dataset-numeric-points dataset))
                          :category-points (loop for vec across (dataset-category-points dataset)
                                              for val across new-points
                                              collect (concatenate 'vector vec (vector val) ) into result
                                              finally (return (coerce result 'vector))
                                                )

                          :dimensions (concatenate 'vector (copy-dims dataset) (vector new-dim))

                                                                                                                             ))))
      )))


(defgeneric concatenate-datasets (left right))
(defmethod concatenate-datasets ((left dataset) (right dataset))
  (labels ((copy-dims (d) (map 'vector #'copy-dimension (dataset-dimensions d)))
           (copy-pts (pts) (map 'vector #'copy-seq pts))
           (merge-dims (l r) (merge 'vector (copy-dims l) (copy-dims r) #'eql))
           (merge-pts (l r) (merge 'vector (copy-pts l) (copy-pts r) #'eql)))

    ;; ensure datasets are same type dimension tpes match
    (assert (eql (type-of left) (type-of right)))
    (assert (every #'identity (map 'list (lambda (x y) (eql (dimension-type  x) (dimension-type y))) (dataset-dimensions left) (dataset-dimensions right))))

    (etypecase left
      (numeric-and-category-dataset
       (progn ;(break "numeric-and-category-dataset")
              (make-instance 'numeric-and-category-dataset
                             :dimensions (copy-dims left)
                             :numeric-points (merge-pts (dataset-numeric-points left)
                                                        (dataset-numeric-points right))
                             :category-points (merge-pts (dataset-category-points left)
                                                         (dataset-category-points right)))))
      (numeric-matrix-and-category-dataset
           ;       (make-instance 'numeric-matrix-and-category-dataset
           ;          :dimensions (merge-dims left right)
           ;          :numeric-points (copy-mat (dataset-numeric-points dataset))
           ;          :category-points
           ;       (copy-pts
           ;                (dataset-category-points dataset)))
       (error "numeric-matrix-and-category-dataset not supported yet")
       )
      (numeric-dataset
       (progn
              (make-instance 'numeric-dataset
                             :dimensions (copy-dims left)
                             :numeric-points (merge-pts (dataset-numeric-points left)
                                                        (dataset-numeric-points right)))))
                                        ;(numeric-matrix-dataset
                                        ;(make-instance 'numeric-matrix-dataset
                                        ;               :dimensions (copy-dims left)
                                        ;               :numeric-points (copy-mat
                                        ;               (dataset-numeric-points dataset))))
      (unspecialized-dataset
       (progn
              (make-instance 'unspecialized-dataset
                                         :dimensions (copy-dims left)
                                         :points (merge-pts (dataset-points left)
                                                            (dataset-points right))
                                         ))


       )
      (category-dataset
       (progn
         (make-instance 'category-dataset
                        :dimensions (copy-dims left)
                        :category-points (merge-pts (dataset-category-points left)
                                                    (dataset-category-points right)))))
      (error "Unsupported Type")
      )))


(defgeneric head-points (dataset &optional n))
(defmethod head-points ((dataset dataset) &optional (n 5))
  "Returns first <n> data points in dataset"
  (let* ((points  (dataset-points dataset))
        (num-points (length points)))
    (subseq points
            0 (if (> n num-points) num-points n))))

(defgeneric tail-points (dataset &optional n))
(defmethod tail-points ((dataset dataset) &optional (n 5))
  "Returns last <n> data points in dataset"
  (let* ((points (dataset-points dataset))
         (len (length points)))
    (subseq points
            (- len n) len)))

(defgeneric map-over-dimension! (dataset dim-name fn))
(defmethod map-over-dimension! ((dataset dataset) dim-name fn)
  "Destructivly update data points of <dimension-name> with output of fn applied to <dimension>"
  (let* ((dim (find dim-name (dataset-dimensions dataset)
                    :test #'string=
                    :key #'dimension-name))
         (idx (clml.hjs.read-data:dimension-index dim)))
    (loop for vec across (ecase (dimension-type dim)
                           (:unknown (dataset-points dataset))
                           (:category (dataset-category-points dataset))
                           (:numeric (dataset-numeric-points dataset)))
       do (setf (elt vec idx) (funcall fn (elt vec idx))))))

(defgeneric map-over-dimensions! (dataset dim-names-fns-alist))
(defmethod map-over-dimensions! ((dataset dataset) dim-names-fn-alist)
  "Destructivly update data points of <dimension-name> with output of fn applied to <dimension>"
  (loop for (dim-name fn) in dim-names-fn-alist
       do
       (let* ((dim (find dim-name (dataset-dimensions dataset)
                         :test #'string=
                         :key #'dimension-name))
              (idx (clml.hjs.read-data:dimension-index dim)))
         (loop for vec across (ecase (dimension-type dim)
                                (:unknown (dataset-points dataset))
                                (:category (dataset-category-points dataset))
                                (:numeric (dataset-numeric-points dataset)))
            do (setf (elt vec idx) (funcall fn (elt vec idx)))))))

(defgeneric filter (dataset-in dim-name test))
(defmethod filter ((dataset-in dataset) dim-name test)
  "Destructivly update data points of <dimension-name> with output of fn applied to <dimension>"
  (let* ((dataset (copy-dataset dataset-in))
         (dim (find dim-name (dataset-dimensions dataset)
                    :test #'string=
                    :key #'dimension-name))
         (idx (clml.hjs.read-data:dimension-index dim))
         (results '()))
    (loop for vec across (ecase (dimension-type dim)
                           (:unknown (dataset-points dataset))
                           (:category (dataset-category-points dataset))
                           (:numeric (dataset-numeric-points dataset)))
       when (funcall test (elt vec idx))
       do
         (setf results (cons vec results))
       )
    (if (and results (> (length results) 0))
        (make-unspecialized-dataset (coerce
                                     (map 'vector #'dimension-name
                                          (dataset-dimensions dataset-in))'list  )
                                    (coerce results 'vector))
        nil)
    ))

(defgeneric filter! (dataset-in dim-name test))
(defmethod filter! ((dataset-in dataset) dim-name test)
  "Destructivly update data points of <dimension-name> with output of fn applied to <dimension>"
  (let* ((dataset (copy-dataset dataset-in))
         (dim (find dim-name (dataset-dimensions dataset)
                    :test #'string=
                    :key #'dimension-name))
         (idx (clml.hjs.read-data:dimension-index dim)))
    (loop for vec across (ecase (dimension-type dim)
                           (:unknown (dataset-points dataset))
                           (:category (dataset-category-points dataset))
                           (:numeric (dataset-numeric-points dataset)))
       do (when (funcall test (elt vec idx))
            (ecase (dimension-type dim)
              (:unknown (setf (dataset-points dataset)
                              (delete  (elt vec idx) (dataset-points dataset)
                                       :count 1 :test #'equalp)))
              (:category (setf (dataset-category-points dataset)
                               (delete  (elt vec idx) (dataset-category-points dataset)
                                        :count 1 :test #'equalp)))
              (:numeric (setf (dataset-numeric-points dataset)
                              (delete  (elt vec idx) (dataset-numeric-points dataset)
                                        :count 1 :test #'equalp))))))
    dataset))


(defmethod filter! ((dataset-in numeric-and-category-dataset) dim-name test)
  (let* ((dataset (copy-dataset dataset-in)))
    (multiple-value-bind (cpoints npoints)
        (loop
           with dim = (find dim-name (dataset-dimensions dataset)
                            :test #'string=
                            :key #'dimension-name)

           with dim-type = (dimension-type dim)
           with dim-idx = (dimension-index dim)

           for np across (dataset-numeric-points dataset)
           for cp across (dataset-category-points dataset)
           for do-collect = (ecase dim-type
                              (:category (funcall test (elt cp dim-idx)))
                              (:numeric (funcall test (elt np dim-idx))))
           when do-collect
             collect cp into cpoints
           when do-collect
             collect np into npoints
           finally (return (values cpoints npoints))

             )
      (setf (dataset-category-points dataset) (make-array (list (length cpoints)) :element-type '(simple-array cvec (*)) :initial-contents cpoints ))
      (setf (dataset-numeric-points dataset) (make-array (list (length npoints)) :element-type '(simple-array cvec (*)) :initial-contents npoints))
      )
    dataset)
  )

(defgeneric dataset-name-index-alist (dataset))
(defmethod dataset-name-index-alist ((dataset dataset))
  (map 'list (lambda (d) (list (dimension-name d) (dimension-index d)) ) (dataset-dimensions dataset  )))

(defun get-dimension-index (name alist)
  (cadr (assoc name alist :test #'string=)))

(defgeneric select-dimension (name data &key selector))
(defmethod select-dimension (name (data dataset) &key (selector (lambda (x y) (or x y t))))
  (let ((pos
         (loop for pos from 0
            for dim across (dataset-dimensions data)
            when (string= (dimension-name dim) name)
            return (values pos (dimension-type dim)))))
    (when pos
      (loop for vec across (dataset-points data)
           when (funcall selector data vec)
          collect (aref vec pos) into result
          finally (return (coerce result 'vector)
                          )))))

(defmethod write-dataset ((dataset clml.hjs.read-data::dataset) filename &key (csv-delimiter #\,) (external-format clml.utility.csv::*csv-default-external-format*))
  " Write dataset to csv formated file"
  (with-open-file (f filename :direction :output
                     :if-does-not-exist :create
                     :if-exists :supersede
                     :external-format external-format)
    (clml.utility.csv:write-csv-stream f (vector (map '(vector string) #'dimension-name (dataset-dimensions dataset))) :delimiter csv-delimiter)
    (clml.utility.csv:write-csv-stream f (dataset-points dataset) :delimiter csv-delimiter)))

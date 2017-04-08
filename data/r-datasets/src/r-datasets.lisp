(in-package :clml.data.r-datasets)

(define-condition no-access (error)
  ((text :initarg :text :reader text)))


(defclass dataset-directory ()
  ((url :initarg :url)
   (data :initarg :dataset))
  )

(defun get-r-dataset-directory (&optional (url))
  "-returns: <dataset-directory> object containg directory of available R datasets
- arguments:
  -url: <string> Optional URL containing the location of the R dataset directory. Only needed if a custom directory is needed."
  (make-instance 'dataset-directory :url (if (null url) "http://vincentarelbundock.github.com/Rdatasets/datasets.csv" url)))


(defmethod initialize-instance :after ((dataset-directory dataset-directory) &key) 
  (setf (slot-value dataset-directory 'data)
        (multiple-value-bind (dataset-file)
            (clml.utility.data:fetch (slot-value dataset-directory 'url))
          (if dataset-file
            (clml.hjs.read-data:read-data-from-file 
             dataset-file
             :type :csv :csv-type-spec '(string string string string string))
            (error 'no-access
                   :text (format nil "Provided url was inaccessable: ~a : ~a"
                                 (slot-value dataset-directory 'url) message))
            ))
        ) 
    )

(defgeneric get-dataset (dataset-directory package name &key)
  (:documentation "Returns the dataset specified by the ~package~ and ~name~
-return: <unspecialized-dataset> 
-arguments:
  -package: <string>  package
  -name: <string> dataset name
  -csv-type-spec: <list> column type specifier list
  -csv-header-p: <bool default t> if true first line of CSV is header
  -missing-values-check <boolean default t> Check for presence of missing values

Returns unspecialized dataset containing contents of CSV specified by the package and name.
Because type information is not included in the CSV nor in the dataset directory it will
probably in most cases be necessary to specify ~csv-type-spec~. If ~csv-type-spec~ is not
set the columns wil be read as strings. You could of course manipulate the dataset later.
It is in most cases better to specify the ~csv-type-spec~. The ~csv-type~spec~ should be a
list containing one of the follwong symbols:
  keyword symbol pathname integer double-float single-float float 
  number t nil string
The values t or nil in the spec will cause the column to be interpeted as a string.

Missing values are defined by a column value of NA or the empty string. Missing values will
cause the value to be represented by the keyword :na in the dataset. For R datasets it is
not necessary to set ~csv-header-p~ ~missing-values-check~.

Example:

#+BEGIN_SRC lisp
; grab the data and see what the types should be
CL-USER> (head-points (get-dataset dd \"datasets\" \"BOD\"))
#(#(\"1\" \"1\" \"8.3\") #(\"2\" \"2\" \"10.3\") #(\"3\" \"3\" \"19\") #(\"4\" \"4\" \"16\")
  #(\"5\" \"5\" \"15.6\") #(\"6\" \"7\" \"19.8\"))
; Looks like '(integer integer double-float) will do
CL-USER> (head-points (get-dataset dd \"datasets\" \"BOD\" :csv-type-spec '(integer integer double-float)))
#(#(1 1 8.3) #(2 2 10.3) #(3 3 19.0) #(4 4 16.0) #(5 5 15.6))
#+END_SRC
"))
(defmethod get-dataset ((dataset-directory dataset-directory) package name
                        &key
                          csv-type-spec
                          (csv-header-p t)
                          (missing-value-check t))
  (let ((package-matches (filter (slot-value dataset-directory 'data) "Package"
                                 (lambda (s) (string= s package)))))
    (if package-matches
        (let ((dataset (filter
                        package-matches
                        "Item" (lambda (s) (string= s name)))))
          (if (and dataset (> (length (dataset-points dataset)) 0))
              (clml.hjs.read-data:read-data-from-file 
               (clml.utility.data:fetch (elt (!! dataset "csv") 0))
               :type :csv :csv-type-spec csv-type-spec :csv-header-p csv-header-p
               :missing-value-check missing-value-check
               )
              nil))
        nil))
  )

(defgeneric dataset-documentation (dataset-directory package name &key stream)
  (:documentation
   "Outputs documention for the R dataset to the specified stream if no stream supplied defaults to console
-return: <unspecialized-dataset> 
-arguments:
  -package: <string>  package
  -name: <string> dataset name
  -stream: <string> <key> <optional> specify output stream for documentation 
"))

(defmethod dataset-documentation ((dataset-directory dataset-directory) package name &key (stream t))
  (let ((package-matches (filter (slot-value dataset-directory 'data) "Package"
                                 (lambda (s) (string= s package)))))
    (if package-matches
        (let ((dataset (filter
                        package-matches
                        "Item" (lambda (s) (string= s name)))))
          (if (and dataset (> (length (dataset-points dataset)) 0))
              (format stream "\"~a\"" (cl-ppcre:regex-replace-all "<[^<]*>" (drakma:http-request (elt (!! dataset "doc") 0)) ""))
              nil))
        nil))
  )

(defgeneric inventory (dataset-directory &key stream)
  (:documentation "Outputs R packages, datasets and description available datasets in inventory
-return: nil
-arguments:
  -package: <dataset-directory>  datasets
  -stream: <string> <key> <optional> specify output stream for documentation 
"
                  ))
(defmethod inventory ((dataset-directory dataset-directory) &key (stream t))
  (format stream "~{~25A ~}~%~{~25A ~}~%~{~A~^~%~}"
          (coerce
           (subseq (map 'vector #'dimension-name
                        (dataset-dimensions (slot-value dataset-directory 'data))) 0 3) 'list)
          (list "-------------------------" "-------------------------" "-------------------------")
          (loop
            for row across (dataset-points (slot-value dataset-directory 'data))
            collect (format nil "~{~25A ~}~%" (coerce (subseq row 0 3) 'list)
                            ))))

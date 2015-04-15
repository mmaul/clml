(in-package :clml.utility.r-datasets)

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
        (multiple-value-bind (dataset-file status message)
            (clml.utility.data:fetch (slot-value dataset-directory 'url))
          (if (= status 200)
            (clml.hjs.read-data:read-data-from-file 
             dataset-file
             :type :csv :csv-type-spec '(string string string string string))
            (error 'no-access
                   :text (format nil "Provided url was inaccessable: ~a : ~a"
                                 (slot-value dataset-directory 'url) message))
            ))
        ) 
    )

(defgeneric get-dataset (dataset-directory package name)
  (:documentation "Returns the dataset specified by the ~package~ and ~name~
-return: <unspecialized-dataset> 
-arguments:
  -package: <string>  package
  -name: <string> dataset name
"))
(defmethod get-dataset ((dataset-directory dataset-directory) package name)
  (let ((package-matches (filter (slot-value dataset-directory 'data) "Package"
                                 (lambda (s) (string= s package)))))
    (if package-matches
        (let ((dataset (filter
                        package-matches
                        "Item" (lambda (s) (string= s name)))))
          (if (and dataset (> (length (dataset-points dataset)) 0))
              (clml.hjs.read-data:read-data-from-file 
               (clml.utility.data:fetch (elt (!! dataset "csv") 0))
                     :type :csv )
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

(defmethod dataset-documentations ((dataset-directory dataset-directory) package name &key (stream t))
  (let ((package-matches (filter (slot-value dataset-directory 'dataset) "Package"
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
  (format stream "~{~25A ~}~%"
          (coerce
           (subseq (map 'vector #'dimension-name
                        (dataset-dimensions (slot-value ds 'data))) 0 3) 'list))
  ( format stream "~{~25A ~}~%" (list "-------------------------"))
  (loop
     for row across (dataset-points (slot-value dataset-directory 'data))
     do(format stream "~{~25A ~}~%" (coerce (subseq row 0 3) 'list)
                   )))

;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.

;; THIS BACKEND IS DEPRECATED AND WILL NOT WORK
;; ITS PRESENCE IS FOR POSTERITY ONLY
(in-package :cl-store-xml)


(defbackend xml :stream-type 'character :extends (resolving-backend))

;; The xml backend does not use any type codes
;; we figure it out when we read the tag of each object
(defvar *xml-mapping* (make-hash-table :test #'equal))
(defun add-xml-mapping (name)
  (setf (gethash name *xml-mapping*)
        (intern name :cl-store-xml)))

(add-xml-mapping "REFERRER")
(add-xml-mapping "INTEGER")
(add-xml-mapping "FLOAT")
(add-xml-mapping "SIMPLE-STRING")
(add-xml-mapping "SYMBOL")
(add-xml-mapping "CONS")
(add-xml-mapping "RATIO")
(add-xml-mapping "CHARACTER")
(add-xml-mapping "COMPLEX")
(add-xml-mapping "PATHNAME")
(add-xml-mapping "FUNCTION")
(add-xml-mapping "GENERIC-FUNCTION")

(defmethod get-next-reader ((backend xml) (place list))
  (or (gethash (car place) *xml-mapping*)
      (error "Unknown tag ~A" (car place))))

(defun princ-xml (tag value stream)
  (format stream "<~A>" tag)
  (xmls:write-xml value stream)
  (format stream "</~A>" tag))

(defun princ-and-store (tag obj stream)
  (format stream "<~A>" tag)
  (store-object obj stream)
  (format stream "</~A>" tag))

(defmacro with-tag ((tag stream) &body body)
  `(progn 
    (format ,stream "<~A>" ,tag)
    ,@body
    (format ,stream "</~A>" ,tag)))

(defun first-child (elmt)
  (first (xmls:node-children elmt)))

(defun second-child (elmt)
  (second (xmls:node-children elmt)))

(defun get-child (name elmt &optional (errorp t))
  (or (assoc name (xmls:node-children elmt) :test #'equal)
      (and errorp
           (restore-error "No child called ~A in xml ~a"
                          (list name elmt)))))

(defun get-attr (name elmt)
  (cadr (assoc name (xmls:node-attrs elmt) :test #'equal)))

(declaim (inline restore-first))
(defun restore-first (place)
  (restore-object (first-child place)))

(defmethod store-referrer ((backend xml) (ref t) (stream t))
  (princ-xml "REFERRER" ref stream))

(defrestore-xml (referrer place)
  (make-referrer :val (parse-integer (third place))))

(defmethod referrerp ((backend xml) (reader t))
  (eql reader 'referrer))

;; override backend restore to parse the incoming stream
(defmethod backend-restore ((backend xml) (place stream))
  (let ((*restore-counter* 0)
        (*need-to-fix* nil)
        (*print-circle* nil)
        (*restored-values* (and *check-for-circs*
                                (make-hash-table :test #'eq :size *restore-hash-size*))))
    (multiple-value-prog1
        (backend-restore-object backend
                                (or (xmls:parse place)
                                    (restore-error "Invalid xml")))
      (dolist (fn *need-to-fix*)
        (force fn)))))

;; integer
(defstore-xml (obj integer stream)
  (princ-xml "INTEGER" obj stream))

(defrestore-xml (integer from)
  (values (parse-integer (first-child from))))

;; floats
(defvar *special-floats* nil) ;; setup in custom-xml files

;; FIXME: add support for *special-floats*
(defstore-xml (obj float stream)
  (with-tag ("FLOAT" stream) (print obj stream)))

(defrestore-xml (float from)
  (cl-l10n:parse-number (first-child from)))

#|
(defstore-xml (obj single-float stream)
  (store-float "SINGLE-FLOAT" obj stream))

(defstore-xml (obj double-float stream)
  (store-float "DOUBLE-FLOAT" obj stream))

(defun store-float (type obj stream)
  (block body
    (let (significand exponent sign)
      (handler-bind ((simple-error
                      #'(lambda (err)
                          (declare (ignore err))
                          (when-let (type (cdr (assoc obj *special-floats*)))
                            (output-float-type type stream)
                            (return-from body)))))
        (multiple-value-setq (significand exponent sign)
            (integer-decode-float obj))
        (with-tag (type stream)
          (princ-and-store "SIGNIFICAND" significand stream)
          (princ-and-store "RADIX"(float-radix obj) stream)
          (princ-and-store "EXPONENT" exponent stream)
          (princ-and-store "SIGN" sign stream))))))
|#

; FIXME: restore flaot

;; ratio
(defstore-xml (obj ratio stream)
  (with-tag ("RATIO" stream)
    (princ-and-store "NUMERATOR" (numerator obj) stream)
    (princ-and-store "DENOMINATOR" (denominator obj) stream)))

(defrestore-xml (ratio from)
  (/ (restore-first (get-child "NUMERATOR" from))
     (restore-first (get-child "DENOMINATOR" from))))

;; char
(defstore-xml (obj character stream)
  (princ-and-store "CHARACTER" (char-code obj) stream))

(defrestore-xml (character from)
  (code-char (restore-first from)))


;; complex
(defstore-xml (obj complex stream)
  (with-tag ("COMPLEX" stream)
    (princ-and-store "REALPART" (realpart obj) stream)
    (princ-and-store "IMAGPART" (imagpart obj) stream)))


(defrestore-xml (complex from)
  (complex (restore-first (get-child "REALPART" from))
           (restore-first (get-child "IMAGPART" from))))


;; symbols 
(defstore-xml (obj symbol stream)
  (with-tag ("SYMBOL" stream)
    (princ-and-store "NAME" (symbol-name obj) stream)
    (cl-store::when-let (package (symbol-package obj))
      (princ-and-store "PACKAGE" (package-name package) stream))))

(defrestore-xml (symbol from)
  (let ((name (restore-first (get-child "NAME" from)))
        (package (when (get-child "PACKAGE" from nil)
                   (restore-first (get-child "PACKAGE" from)))))
    (if package
        (values (intern name package))
        (make-symbol name))))

;; lists
(defstore-xml (obj cons stream)
  (with-tag ("CONS" stream)
    (princ-and-store "CAR" (car obj) stream)
    (princ-and-store "CDR" (cdr obj) stream)))

(defrestore-xml (cons from)
  (resolving-object (x (cons nil nil))
    (setting (car x) (restore-first (get-child "CAR" from)))
    (setting (cdr x) (restore-first (get-child "CDR" from)))))

;; simple string
(defstore-xml (obj simple-string stream)
  (princ-xml "SIMPLE-STRING" obj stream))

(defrestore-xml (simple-string from)
  (first-child from))


;; pathnames
(defstore-xml (obj pathname stream)
  (with-tag ("PATHNAME" stream)
    (princ-and-store "DEVICE" (pathname-device obj) stream)
    (princ-and-store "DIRECTORY" (pathname-directory obj) stream)
    (princ-and-store "NAME" (pathname-name obj) stream)
    (princ-and-store "TYPE" (pathname-type obj) stream)
    (princ-and-store "VERSION" (pathname-version obj) stream)))

(defrestore-xml (pathname place)
  (make-pathname 
   :device (restore-first (get-child "DEVICE" place))
   :directory (restore-first (get-child "DIRECTORY" place))
   :name (restore-first (get-child "NAME" place))
   :type (restore-first (get-child "TYPE" place))
   :version (restore-first (get-child "VERSION" place))))


; hash table
(defstore-xml (obj hash-table stream)
  (with-tag ("HASH-TABLE" stream)
    (princ-and-store "REHASH-SIZE" (hash-table-rehash-size obj) stream)
    (princ-and-store "REHASH-THRESHOLD" (hash-table-rehash-threshold obj) stream)
    (princ-and-store "SIZE" (hash-table-size obj) stream)
    (princ-and-store "TEST" (hash-table-test obj) stream)
    (with-tag ("ENTRIES" stream)
      (loop for key being the hash-keys of obj
            using (hash-value value) do
            (with-tag ("ENTRY" stream)
              (princ-and-store "KEY" key stream)
              (princ-and-store "VALUE" value stream))))))

;; FIXME: restore hash tables

;; objects and conditions

(defun xml-dump-type-object (obj stream)
  (let* ((all-slots (serializable-slots obj)))
    (with-tag ("SLOTS" stream)
      (dolist (slot all-slots)
        (when (slot-boundp obj (slot-definition-name slot))
          (when (or *store-class-slots* 
                    (eql (slot-definition-allocation slot) :instance))
            (with-tag ("SLOT" stream)
              (let ((slot-name (slot-definition-name slot)))
                (princ-and-store "NAME" slot-name stream)
                (princ-and-store "VALUE" (slot-value obj slot-name) stream)))))))))

(defstore-xml (obj standard-object stream)
  (with-tag ("STANDARD-OBJECT" stream)
    (princ-and-store "CLASS" (type-of obj) stream)
    (xml-dump-type-object obj stream)))

(defstore-xml (obj condition stream)
  (with-tag ("CONDITION" stream)
    (princ-and-store "CLASS" (type-of obj) stream)
    (xml-dump-type-object obj stream)))


;; FIXME: restore objects



;; classes

;; FIXME : Write me 

;; built in classes
(defstore-xml (obj built-in-class stream)
  (princ-and-store "BUILT-IN-CLASS" (class-name obj) stream))

#-ecl ;; for some reason this doesn't work with ecl
(defmethod internal-store-object ((backend xml) (obj (eql (find-class 'hash-table))) stream)
  (princ-and-store "BUILT-IN-CLASS" 'cl:hash-table stream))

;; FIXME: restore built in classes

;; arrays and vectors
;; FIXME : Write me 

;; packages
;; FIXME : Write me 

;; functions
(defstore-xml (obj function stream)
  (princ-and-store "FUNCTION" (get-function-name obj) stream))

(defrestore-xml (function from)
  (fdefinition (restore-first from)))

;; generic functions
(defstore-xml (obj generic-function stream)
  (if (generic-function-name obj)
      (princ-and-store "GENERIC-FUNCTION" 
                       (generic-function-name obj) stream)
      (store-error "No generic function name for ~A." obj)))

(defrestore-xml (generic-function from) 
  (fdefinition (restore-first from)))

(setf *default-backend* (find-backend 'xml))

#|

;; required methods and miscellaneous util functions


(defrestore-xml (hash-table place)
  (let ((hash1 (make-hash-table 
                :rehash-size (restore-first (get-child "REHASH-SIZE" place))
                :rehash-threshold (restore-first 
                                   (get-child "REHASH-THRESHOLD" place))
                :size (restore-first (get-child "SIZE" place))
                :test (symbol-function (restore-first (get-child "TEST" place))))))
    (resolving-object (hash1 hash1)
      (dolist (entry (xmls:node-children (get-child "ENTRIES" place)))
        (let* ((key-place (first-child (first-child entry)))
               (val-place (first-child (second-child entry))))
          (setting-hash (restore-object key-place) 
                        (restore-object val-place)))))
    hash1))


(defun restore-xml-type-object (place)
  (let* ((class (find-class (restore-first (get-child "CLASS" place))))
         (new-instance (allocate-instance class)))
    (resolving-object new-instance
      (dolist (slot (xmls:node-children (get-child "SLOTS" place)))
        (let ((slot-name (restore-first (get-child "NAME" slot))))
          (setting (slot-value slot-name) 
                   (restore-first (get-child "VALUE" slot))))))
    new-instance))

(defrestore-xml (standard-object place)
  (restore-xml-type-object place))

(defrestore-xml (condition place)
  (restore-xml-type-object place))

;; classes
(defun store-slot (slot stream)
  (with-tag ("SLOT" stream)
    (princ-and-store "NAME" (slot-definition-name slot) stream)
    (princ-and-store "ALLOCATION" (slot-definition-allocation slot) stream)
    (princ-and-store "TYPE" (slot-definition-type slot) stream)
    (with-tag ("INITARGS" stream)
      (dolist (x (slot-definition-initargs slot))
        (princ-and-store "INITARG" x stream)))
    (with-tag ("READERS" stream)
      (dolist (x (slot-definition-readers slot))
        (princ-and-store "READER" x stream)))
    (with-tag ("WRITERS" stream)
      (dolist (x (slot-definition-writers slot))
        (princ-and-store "WRITER" x stream)))))

(defstore-xml (obj standard-class stream)
  (with-tag ("STANDARD-CLASS" stream)
    (princ-and-store "NAME" (class-name obj) stream)
    (with-tag ("SUPERCLASSES" stream)
      (loop for x in (class-direct-superclasses obj) do
            (unless (eql x (find-class 'standard-object))
              (princ-and-store "SUPERCLASS" 
                               (if *store-class-superclasses*
                                   x
                                   (class-name x))
                               stream))))
    (with-tag ("SLOTS" stream)
      (dolist (x (class-direct-slots obj))
        (store-slot x stream)))
    (princ-and-store "METACLASS" (type-of obj) stream)))



(defun xml-add-class (name slots superclasses metaclass)
  (ensure-class name :direct-slots slots
                :direct-superclasses superclasses
                :metaclass metaclass)
  #+clisp(add-methods-for-class name slots))

(defun get-values (values)
  (loop for value in (xmls:node-children values)
        collect (restore-first value)))

(defun get-slots (slots)
  (loop for slot in (xmls:node-children slots) 
        collect (list :name (restore-first (get-child "NAME" slot))
                      :allocation (restore-first (get-child "ALLOCATION" slot))
                      :type (restore-first (get-child "TYPE" slot))
                      :initargs (get-values (get-child "INITARGS" slot))
                      :readers (get-values (get-child "READERS" slot))
                      :writers (get-values (get-child "WRITERS" slot)))))

(defun get-superclasses (superclasses)
  (loop for superclass in (xmls:node-children superclasses)
        collect (restore-first superclass)))
        
(defrestore-xml (standard-class  place)
  (let* ((name (restore-first (get-child "NAME" place)))
         (superclasses (get-superclasses (get-child "SUPERCLASSES" place)))
         (slots (get-slots (get-child "SLOTS" place)))
         (metaclass (restore-first (get-child "METACLASS" place))))
    (cond (*nuke-existing-classes*
           (xml-add-class name slots superclasses metaclass))
          (t (aif (find-class name nil)
                  it
                  (xml-add-class name slots superclasses metaclass))))))

;; built-in-classes 
(defstore-xml (obj built-in-class stream)
  (princ-and-store "BUILT-IN-CLASS" (class-name obj) stream))

(defrestore-xml (built-in-class place)
  (find-class (restore-first place)))

;; I don't know if this really qualifies as a built-in-class but it 
;; does make things a bit easier
(defmethod internal-store-object ((obj (eql (find-class 'hash-table))) stream 
                                  (backend xml-backend))
  (princ-and-store "BUILT-IN-CLASS" 'cl:hash-table stream))


;; Arrays and vectors
(defstore-xml (obj array stream)
  (xml-dump-array obj stream))

(defun xml-dump-array (obj stream)
  (with-tag ("ARRAY" stream)
    (princ-and-store "DIMENSIONS" (array-dimensions obj) stream)
    (if (and (= (array-rank obj) 1)
             (array-has-fill-pointer-p obj))
        (princ-and-store "FILL-POINTER" (fill-pointer obj) stream)
        (princ-and-store "FILL-POINTER" nil stream))
    (princ-and-store "ELEMENT-TYPE" (array-element-type obj) stream)
    (multiple-value-bind (to offset) (array-displacement obj)
      (princ-and-store "DISPLACED-TO" to stream)
      (princ-and-store "DISPLACED-OFFSET" offset stream))
    (princ-and-store "ADJUSTABLE" (adjustable-array-p obj) stream)
    (with-tag ("VALUES" stream)
      (loop for x from 0 to (1- (array-total-size obj)) do
            (princ-and-store "VALUE" (row-major-aref obj x) stream)))))

(defrestore-xml (array place)
  (let* ((dimensions (restore-first (get-child "DIMENSIONS" place)))
         (fill-pointer (restore-first (get-child "FILL-POINTER" place)))
         (element-type (restore-first (get-child "ELEMENT-TYPE" place)))
         (displaced-to (restore-first (get-child "DISPLACED-TO" place)))
         (displaced-offset (restore-first (get-child "DISPLACED-OFFSET"
                                                     place)))
         (adjustable (restore-first (get-child "ADJUSTABLE" place)))
         (res (make-array dimensions  
                          :element-type element-type
                          :adjustable adjustable
                          :fill-pointer fill-pointer)))
    (when displaced-to
      (adjust-array res dimensions :displaced-to displaced-to
                    :displaced-index-offset displaced-offset))
    (resolving-object res
      (loop for value in (xmls:node-children (get-child "VALUES" place))
            for count from 0 do
            (let ((pos count))
              (setting (row-major-aref pos)
                       (restore-first value)))))))


#-(or allegro clisp)
(defstore-xml (obj simple-vector stream)
  (with-tag ("SIMPLE-VECTOR" stream)
    (princ-and-store "LENGTH" (length obj) stream)
    (with-tag ("ELEMENTS" stream)
      (loop for x across obj do
            (princ-and-store "ELEMENT" x stream)))))

#-(or allegro clisp)
(defrestore-xml (simple-vector place)
  (let* ((size (restore-first (get-child "LENGTH" place)))
         (res (make-array size)))
    (resolving-object res
      (loop for element in (xmls:node-children (get-child "ELEMENTS" place))
            for index from 1 do
            (let ((copy (1- index)))
              (setting (aref copy)
                       (restore-first element)))))))
                                      

|#
;; EOF

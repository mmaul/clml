;;; mm*
(defun mm* (a b result)
  (check-type a array)
  (check-type b array)
  (check-type result array)
  (assert (and (= (array-dimension a 1) (array-dimension b 0))
	       (= (array-dimension a 0) (array-dimension result 0))
	       (= (array-dimension b 1) (array-dimension result 1))))
  (dgemm "N" "N"
	 (array-dimension a 0) (array-dimension a 1) (array-dimension b 1)
	 1d0
	 a (array-dimension a 0)
	 b (array-dimension b 0)
	 0d0
	 result (array-dimension result 0))
  result)


#| 
;;; fiddling with cffi type translators
(declaim (inline foreign-complex-float-alloc foreign-complex-double-alloc
		 foreign-complex-float-free foreign-complex-double-free
		 foreign-complex-float-to-lisp foreign-complex-double-to-lisp))
(defun foreign-complex-float-alloc (value)
  (etypecase value
    ((complex single-float)
       (let ((ptr (cffi:foreign-alloc :float :count 2)))
	 (setf (cffi:mem-aref ptr :float 0) (realpart value))
	 (setf (cffi:mem-aref ptr :float 1) (imagpart value))
	 ptr))))

(defun foreign-complex-double-alloc (value)
  (etypecase value
    ((complex double-float)
       (let ((ptr (cffi:foreign-alloc :double :count 2)))
	 (setf (cffi:mem-aref ptr :double 0) (realpart value))
	 (setf (cffi:mem-aref ptr :double 1) (imagpart value))
	 ptr))))

(defun foreign-complex-float-free (pointer)
  (cffi:foreign-free pointer))

(defun foreign-complex-double-free (pointer)
  (cffi:foreign-free pointer))

(defun foreign-complex-float-to-lisp (pointer)
  (let ((realpart (cffi:mem-aref pointer :float 0))
	(imagpart (cffi:mem-aref pointer :float 1)))
    (declare (type single-float realpart imagpart))
    (complex realpart imagpart)))

(defun foreign-complex-double-to-lisp (pointer)
  (let ((realpart (cffi:mem-aref pointer :double 0))
	(imagpart (cffi:mem-aref pointer :double 1)))
    (declare (type double-float realpart imagpart))
    (complex realpart imagpart)))

;; definition, allocation and translation
(cffi:define-foreign-type complex-float ()
  ()
  (:actual-type :pointer)
  (:simple-parser complex-float))

(cffi:define-foreign-type complex-double ()
  ()
  (:actual-type :pointer)
  (:simple-parser complex-double))

;; NOTE: either using :complex or complex will cause CFFI to give a
;; warning so I decided not to define parser method for parsing
;; (:complex precision) like type signature here.

(defmethod cffi:translate-to-foreign (value (type complex-float))
  (foreign-complex-float-alloc value))

(defmethod cffi:translate-to-foreign (value (type complex-double))
  (foreign-complex-double-alloc value))

(defmethod cffi:translate-from-foreign (pointer (type complex-float))
  (foreign-complex-float-to-lisp pointer))

(defmethod cffi:translate-from-foreign (pointer (type complex-double))
  (foreign-complex-double-to-lisp pointer))

(defmethod cffi:free-translated-object (pointer (type complex-float) param)
  (declare (ignore param))
  (foreign-complex-float-free pointer))

(defmethod cffi:free-translated-object (pointer (type complex-double) param)
  (declare (ignore param))
  (foreign-complex-double-free pointer))

|#

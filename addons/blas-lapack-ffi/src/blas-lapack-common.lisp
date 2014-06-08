(in-package :mkl.blas-lapack-common)

;;;; type definitions

(cffi:defctype blas-int
    #+blas-use-64bit-int :int64
  #-blas-use-64bit-int :int32)


;;;; Spec of function name conventions

(define-constant +precision-definitions+
    '((:single . (:abbrev "S" :cffi-type :float))
      (:double . (:abbrev "D" :cffi-type :double))
      (:complex-single . (:abbrev "C" :cffi-type complex-float))
      (:complex-double . (:abbrev "Z" :cffi-type complex-double)))
  :test 'equalp)

(define-constant +matrix-type-definitions+
    '((:general-matrix . (:abbrev "GE"))
      (:general-band-matrix . (:abbrev "GB"))
      (:symmetric-matrix . (:abbrev "SY"))
      (:symmetric-matrix-packed-storage . (:abbrev "SP"))
      (:symmetric-band-matrix . (:abbrev "SB"))
      (:Hermitian-matrix . (:abbrev "HE"))
      (:Hermitian-matrix-packed-storage . (:abbrev "HP"))
      (:Hermitian-band-matrix . (:abbrev "HB"))
      (:triangular-matrix . (:abbrev "TR"))
      (:triangular-matrix-packed-storage . (:abbrev "TP"))
      (:abbrev (:triangular-band-matrix . "TB")))
  :test 'equalp)


;;; defblas
(define-constant +evaluation-form+
    '(if when cond case)
  :test 'equalp)

(defmacro %defblas (name return-type &rest args)
  (labels ((evaluate-forms (form)
	     (cond ((not (listp form))
		    form)
		   ((member (first form) +evaluation-form+)
		    (eval form))
		   (t
		    (mapcar #'evaluate-forms form)))))
    (let ((result
	   `(defffun ,name
		,(evaluate-forms return-type)
	      ,@(evaluate-forms args))))
      ;; clean up
      (loop for p in (mapcar #'car +precision-definitions+)
	    do (setf result (subst (getf (cdr (assoc p +precision-definitions+)) :cffi-type) p result)))
      result)))

(defmacro defblas (name precisions return-type &rest args)
  (assert (or (member precisions +precision-definitions+ :key #'car)
	      (and (listp precisions)
		   (every (lambda (p) (member p +precision-definitions+ :key #'car)) precisions)))
	  nil
	  "Precisions = ~a, is not a recognized value."
	  precisions)
  (let ((precisions (ensure-list precisions))) 
    `(progn
       ,@(loop for p in precisions
	       for function-name = (symbolicate (getf (cdr (assoc p +precision-definitions+)) :abbrev) name)
               for foreign-symbol-name = (string-upcase (nth-value 1 (cffi::parse-name-and-options function-name)))
	       do (assert (cffi:foreign-symbol-pointer foreign-symbol-name)
	       		  nil
	       		  "Cannot resolve foreign function symbol (~a)"
	       		  foreign-symbol-name)
	       collect `(%defblas ,function-name
				  ,(if (eq return-type :precision)
				       p
				       return-type)
				  ,@(subst p :precision args))
	       collect `(export ',function-name)))))


;;; deflapack
(defmacro deflapack (name precisions return-type &rest args)
  `(defblas ,name ,precisions ,return-type ,@args))


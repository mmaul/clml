(in-package #:metabang.bind.developer)

(defmethod bind-generate-bindings
    ((kind (eql :re)) variable-form value-form
     body declarations remaining-bindings)
  ;; (:re "re" vars)
  (bind (((regex &rest vars) variable-form)
	 (gok (gensym "ok"))
	 (gblock (gensym "block"))
	 ((:values vars ignores) (bind-fix-nils vars)))
    `((let ((,gok nil))
	(block ,gblock
	  (flet ((doit (,@vars)
		   ,@(when ignores `((declare (ignore ,@ignores))))
		   (return-from ,gblock
		     (progn ,@(bind-macro-helper
                       remaining-bindings declarations body)))))
	    (excl:re-let ,regex ,value-form
		,(loop for var in vars for i from 1 collect
		      `(,var ,i))
	      ,(bind-filter-declarations
		declarations variable-form)
	      (setf ,gok t)
	      (doit ,@vars))
	    (unless ,gok
	      (doit ,@(make-list (length vars) :initial-element nil)))))))))

#+(or)
(bind (((:re "(\\w+)\\s+(\\w+)\\s+(\\d{1,2})\\.(\\d{1,2})\\.(\\d{4})"
	     fname lname date month year) "Frank Zappa 21.12.1940"))
  (list fname lname date month year))

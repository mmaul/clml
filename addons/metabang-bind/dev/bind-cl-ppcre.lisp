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
	    (cl-ppcre:register-groups-bind 
		,vars (,regex ,value-form :sharedp t)
	      ,(bind-filter-declarations
		declarations variable-form)
	      (setf ,gok t)
	      (doit ,@vars))
	    (unless ,gok
	      (doit ,@(make-list (length vars) :initial-element nil)))))))))

#+(or)
;; simple but doesn't execute inner code if no bindings found
;; which isn't very bind-like
(defmethod bind-generate-bindings
    ((kind (eql :re)) variable-form value-form
     body declarations remaining-bindings)
  ;; (:re "re" vars)
  (bind (((regex &rest vars) variable-form))
    `((cl-ppcre:register-groups-bind ,vars (,regex ,value-form :sharedp t)
       ,(bind-filter-declarations
	 declarations variable-form)
       ,@(bind-macro-helper
	  remaining-bindings declarations body)))))

#+(or)
;; doesn't handle ignores
(defmethod bind-generate-bindings
    ((kind (eql :re)) variable-form value-form
     body declarations remaining-bindings)
  ;; (:re "re" vars)
  (bind (((regex &rest vars) variable-form)
	 (gok (gensym "ok"))
	 (gblock (gensym "block")))
    `((let ((,gok nil))
	(block ,gblock
	  (flet ((doit (,@vars)
		   (return-from ,gblock
		     ,@(bind-macro-helper
			remaining-bindings declarations body))))
	    (cl-ppcre:register-groups-bind 
		,vars (,regex ,value-form :sharedp t)
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

#+(or)
(macroexpand-1 
 '(bind (((:re "(\\w+)\\s+(\\w+)\\s+(\\d{1,2})\\.(\\d{1,2})\\.(\\d{4})"
	     fname lname date month year) "Frank Zappa 21.12.1940"))
  (list fname lname date month year)))

#+(or)
(bind (((:re "(\\w+)\\s+(\\w+)\\s+(\\d{1,2})\\.(\\d{1,2})\\.(\\d{4})"
	     fname lname nil month year) "Frank Zappa 21.12.1940"))
  (list fname lname month year))

#+(or)
(bind (((:re "(a|b)+" first) "cccc"))
  (format t "This will still be printed: ~A" first))

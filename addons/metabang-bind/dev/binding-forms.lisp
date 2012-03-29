(in-package #:metabang.bind)

(defgeneric bind-generate-bindings (kind variable-form value-form
					 body declarations remaining-bindings)
  )

(defbinding-form (array
		  :use-values-p t)
  (let ((array-size (array-total-size variables)))
    `(let* (,@(loop for i below array-size
		 for var = (row-major-aref variables i)
		 unless (eq var nil) collect
		 `(,var (row-major-aref ,values ,i)))))))

(defbinding-form (symbol
		  :use-values-p nil)
  `(let (,@(if values
	       `((,variables ,values))
	       `(,variables)))))

(defbinding-form (cons
		  :use-values-p nil)
  (multiple-value-bind (vars ignores)
      (bind-fix-nils-destructured variables)
    `(destructuring-bind ,vars ,values
	,@(when ignores `((declare (ignore ,@ignores)))))))

(defbinding-form (:values
		  :docstring "" 
		  :use-values-p nil)
  (multiple-value-bind (vars ignores)
      (bind-fix-nils variables)
    `(multiple-value-bind ,vars ,values
	,@(when ignores `((declare (ignore ,@ignores)))))))

(defbinding-form ((:struct :structure)
		  :docstring
		  "Structure fields are accessed using a concatenation
of the structure's `conc-name` and the name of the field. Bind
therefore needs to know two things: the conc-name and the
field-names. The binding-form looks like

    (:structure <conc-name> structure-spec*)

where each `structure-spec` is an atom or list with two elements:

* an atom specifies both the name of the variable to which the
  structure field is bound and the field-name in the structure.

* a list has the variable name as its first item and the structure
  field name as its second.
")
  (let ((conc-name (first variables))
	(vars (rest variables)))
    (assert conc-name)
    (assert vars)
    `(let* ,(loop for var in vars collect
		 (let ((var-var (or (and (consp var) (first var))
				    var))
		       (var-conc (or (and (consp var) (second var))
				     var)))
		   `(,var-var (,(intern 
				 (format nil "~a~a" 
					 conc-name var-conc)) 
				,values)))))))

(defbinding-form ((:structure/rw)
		  :docstring
		  "Structure fields are accessed using a concatenation
of the structure's `conc-name` and the name of the field. Bind
therefore needs to know two things: the conc-name and the
field-names. The binding-form looks like

    (:structure <conc-name> structure-spec*)

where each `structure-spec` is an atom or list with two elements:

* an atom specifies both the name of the variable to which the
  structure field is bound and the field-name in the structure.

* a list has the variable name as its first item and the structure
  field name as its second.
")
  (let ((conc-name (first variables))
	(vars (rest variables)))
    (assert conc-name)
    (assert vars)
    `(symbol-macrolet ,(loop for var in vars collect
			    (let ((var-var (or (and (consp var) (first var))
					       var))
				  (var-conc (or (and (consp var) (second var))
						var)))
			      `(,var-var (,(intern 
					    (format nil "~a~a" conc-name var-conc)) 
					   ,values)))))))

#|
(defbinding-form (:function
		  :docstring ""
		  :use-values-p nil)
  (destructuring-bind (name args) variables
      `(labels ((,name ,args (progn ,values))))))

(bind (((:function foo (x a)) (list a x))
       ((:function bar (a)) (foo a a)))
  (bar 3))

(bind (((:function fib (x))
	(cond ((< x 2) 1)
	      (t (+ (fib (- x 1)) (fib (- x 2)))))))
  (fib 5))

1 1 2 3 5 

;;; fails, need to combine like forms...
(bind (((:function ep (x))
	;;; failure, need to use rest instead of second in bind-macro-helper
	(progn
	  (print (list :e x))
	  (if (= x 0) t (not (op (1- x))))))
       ((:function op (x))
	(progn
	  (print (list :o x))
	  (if (= x 1) t (not (ep (1- x)))))))
  (ep 5))

	(cond ((< x 2) 1)
	      (t (+ (fib (- x 1)) (fib (- x 2)))))))
  (fib 5))

|#

(defbinding-form ((:alist :assoc)
		  :docstring
"The binding form for association-list is as follows:

    (:alist assoc-spec*)
    
where each assoc-spec is an atom or a list of up to three elements:

* atoms bind a variable with that name to an item with the same name.

* lists with a single element are treated like atoms.

* lists with two elements specify the variable in the first and the
name of the accessor in the second.

* Lists with three elements use the third element to specify a default
value (if the second element is #\_, then the accessor name is taken
to be the same as the variable name).

Note that the variables are bound to the `cdr` of the item in the list
rather than the `(item . value)` pair.")
  `(let* ,(loop for spec in variables collect
	       (let* ((spec (if (consp spec) spec (list spec)))
		      (var-name (first spec))
		      var-key var-default)
		 (case (length spec)
		   (1 (setf var-key (first spec)))
		   (2 (setf var-key (second spec)))
		   (3 (setf var-key (second spec)
			    var-default (third spec)))
		   (t
		    (error "bad properly list variable specification: ~s" 
			   spec)))
		 (when (string= (symbol-name var-key) "_")
		   (setf var-key var-name))
		 `(,var-name (or (cdr (assoc ',var-key ,values)) 
				 ,@(when var-default `(,var-default))))))))

;;;;

(defbinding-form ((:read-only-slots :slots-read-only)
		  :docstring 
		  "The `:read-only-slots` binding form is short hand for the `with-slots` macro except that it provides only read access to the class.

The syntax is (:read-only-slots slot-spec*)

Where `slot-spec` can be an atom or a list with two elements.

* an atom tells bind to use it as the name of the new variable _and_
  to treat this name as the name of the slot.

* If the specification is a list, then bind will use the first item for
  the variable's name and the second item for the slot-name.

See [slots][slots-binding-spec] for a 
variant that provides only read-write access to the class."
)		  
  `(let* (,@(loop for var in variables collect
		 (let ((var-var (or (and (consp var) (first var))
				    var))
		       (var-slot (or (and (consp var) (second var))
				     var)))
		   `(,var-var (slot-value ,values ',var-slot)))))))

(defbinding-form (:slots
		  :docstring 
		  "The `:slots` binding form is short hand for the `with-slots` macro.

The syntax is (:slots slot-spec*)

Where `slot-spec` can be an atom or a list with two elements.

* an atom tells bind to use it as the name of the new variable _and_
  to treat this name as the name of the slot.

* If the specification is a list, then bind will use the first item for
  the variable's name and the second item for the slot-name.

See [read-only-slots][read-only-slots-binding-spec] for a 
variant that provides only read-write access to the class."
)		  
  `(with-slots 
	 (,@(loop for var in variables collect
		 (let ((var-var (or (and (consp var) (first var))
				    var))
		       (var-accessor (or (and (consp var) (second var))
					 var)))
		   `(,var-var ,var-accessor))))
       ,values))

;;;;

(defbinding-form ((:read-only-accessors 
		   :accessors-read-only
		   :accessors-r/o)
		  :docstring "The `:read-only-accessors` binding form is short hand for `with-accessors` macro that provides only read access to the class.

The syntax is (:read-only-accessors accessor-spec*)

Where `accessor-spec` can be an atom or a list with two elements.

* an atom tells bind to use it as the name of the new variable _and_
  to treat this name as the name of the accessor.

* If the specification is a list, then bind will use the first item for
  the variable's name and the second item for the accessor name.

See [accessors][accessors-binding-spec] for a 
variant that provides only read-write access to the class."
)
  `(let* ,(loop for var in variables collect
	       (let ((var-var (or (and (consp var) (first var))
				  var))
		     (var-accessor (or (and (consp var) (second var))
				       var)))
		 `(,var-var (,var-accessor ,values))))))

(defbinding-form ((:accessors :writable-accessors)
		  :docstring "The `:accessors` binding form is short hand for the `with-accessors` macro.

The syntax is (:accessors accessor-spec*)

Where `accessor-spec` can be an atom or a list with two elements.

* an atom tells bind to use it as the name of the new variable _and_
  to treat this name as the name of the accessor.

* If the specification is a list, then bind will use the first item for
  the variable's name and the second item for the accessor name.

See [read-only-accessors][read-only-accessors-binding-spec] for a 
variant that provides only read-only access to the class."
)
  `(with-accessors 
	 (,@(loop for var in variables collect
		 (let ((var-var (or (and (consp var) (first var))
				    var))
		       (var-accessor (or (and (consp var) (second var))
					 var)))
		   `(,var-var ,var-accessor))))
       ,values))

(defbinding-form ((:plist :property-list :properties)
		  :docstring 
		  "The binding form for property-lists is as follows:

    (:plist property-spec*)
    
where each property-spec is an atom or a list of up to three elements:

* atoms bind a variable with that name to
a property with the same name (converting the name to a keyword in order to do the lookup). 

* lists with a single element are treated like atoms.

* lists with two elements
specify the variable in the first and the name of the
property in the second. 

* Lists with three elements use
the third element to specify a default value (if the 
second element is #\_, then the property name is taken
to be the same as the variable name).

Putting this altogether we can code the above let statement as:

    (setf plist
      '(:start 368421722 :end 368494926 :flavor :lemon
        :content :ragged))

    (bind (((:plist (start _ 0) end (fuzz fuzziness 'no)) plist))
      (list start end fuzz))
    ==> (368421722 368494926 no)

(which takes some getting used to but has the advantage of brevity).
")
  (handle-plist variables values t))

(defbinding-form (:plist-
		  :docstring "The `:plist-` binding-form is exactly like that of [plist][binding-form-plist] except that the name is not converted to a keyword.

This allows for the case when your property list uses symbols other than
keywords as keys. For example:

    \(bind \(\(\(:plist- a b \(c _ 34\)\) '\(a 5 b 2\)\)\)
      \(list a b c\)\)
    ==> \(5 2 34\)

"
)
  (handle-plist variables values nil))

(defun handle-plist (variables values form-keywords?)
  `(let* ,(loop for spec in variables collect
	       (let* ((spec (if (consp spec) spec (list spec)))
		      (var-name (first spec))
		      var-key var-default)
		 (case (length spec)
		   (1 (setf var-key (first spec)))
		   (2 (setf var-key (second spec)))
		   (3 (setf var-key (second spec)
			    var-default (third spec)))
		   (t
		    (error "bad properly list variable specification: ~s" 
			   spec)))
		 (when (string= (symbol-name var-key) "_")
		   (setf var-key var-name))
		 (when form-keywords?
		   (setf var-key (intern (symbol-name var-key) :keyword)))
		 `(,var-name (getf ,values
				   ,(if form-keywords? 
					var-key `',var-key)  
				   ,@(when var-default
					   `(,var-default))))))))

#+(or)
(bind (((:plist a (b _) (c _ 2) (dd d)) '(:b "B" :a "A" :d "D")))
  (list a b c dd))

#+(or)
(bind (((:plist- a (b _) (c _ 2) (dd d)) '(b "B" a "A" d "D")))
  (list a b c dd))

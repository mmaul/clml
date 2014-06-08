(in-package :ffi-utils)


;;; TYPE: foreign complex type declaration
(cffi:defcstruct complex-float
  (realpart :float)
  (imagpart :float))

(cffi:defcstruct complex-double
  (realpart :double)
  (imagpart :double))


;;; MACRO: make-static-array
(defmacro make-static-array (dimensions &key element-type initial-element initial-contents (warning t))
  (declare (ignorable warning))
  #+allegro
  `(make-array
    ,@`(,dimensions
	:element-type ,element-type
	,@(when initial-element `(:initial-element ,initial-element))
	,@(when initial-contents `(:initial-contents ,initial-contents))
	:allocation :lispstatic-reclaimable))
  #+sbcl
  (progn
    (eval-when (:compile-toplevel)
      (when warning
        (warn "SBCL does not support static array currently. Use them with macro with-arrays-as-foreign-arrays and turn of GC during foreign funcalls (e.g. sb-sys:without-gcing)."))) 
    `(make-array
      ,@`(,dimensions
	  :element-type ,element-type
	  ,@(when initial-element `(:initial-element ,initial-element))
	  ,@(when initial-contents `(:initial-contents ,initial-contents)))))
  #+lispworks
  `(make-array
    ,@`(,dimensions
	:element-type ,element-type
	,@(when initial-element `(:initial-element ,initial-element))
	,@(when initial-contents `(:initial-contents ,initial-contents))
	:allocation :static))
  )


;;; MACRO: with-safe-foreign-function-call-settings
;;; TODO: try to remove without-interrupts
(defmacro with-safe-foreign-function-call-settings (&body body)
  #+(and allegro smp-macros)
  `(excl:with-delayed-interrupts
     ,@body)
  #+(and allegro (not smp-macros))
  `(excl:without-interrupts ,@body)
  #+sbcl
  ;; sb-sys:without-gcing also works, but interrupting intel mkl calls
  ;; might crash SBCL.
  `(sb-sys:without-interrupts ,@body)
  #+lispworks
  `(lispworks:without-interrupts ,@body)
  )


;;; MACRO: with-arrays-as-foreign-arrays
;; array-bindings ::= (pointer-var lisp-var)*
(defmacro with-arrays-as-foreign-arrays ((&rest array-bindings) &body body)
  (declare (ignorable array-bindings))
  (cond ((null array-bindings)
         `(locally ,@body))
        (t 
         #+(and allegro smp-macros)
         ;; FIXME: currently there's no way to ensure the object won't
         ;; be moved in SMP allegro.
         ;; 
         ;; (warn "Currently SMP-enabled AllegroCL cannot forbid GC so~
         ;; it's not safe. You must assure that the arrays are allocated~
         ;; in the static space.")
         `(let ,(mapcar
                 (lambda (array-binding)
                   `(,(first array-binding) ,(second array-binding)))
                 array-bindings)
            ,@body) 
         #+(and allegro (not smp-macros))
         ;; NOTE: with-safe-foreign-function-call-settings already has
         ;; without-interrupts, so no special treatment is needed here
         `(let ,(mapcar
                 (lambda (array-binding)
                   `(,(first array-binding) ,(second array-binding)))
                 array-bindings)
            ,@body) 
         #+sbcl
         `(sb-sys:with-pinned-objects ,(mapcar #'second array-bindings)
            (let ,(mapcar (lambda (array-binding)
                            `(,(first array-binding)
                               (sb-sys:vector-sap (SB-KERNEL:%WITH-ARRAY-DATA ,(second array-binding) 0 0))))
                   array-bindings)
              ,@body))
         #+lispworks
         ;; NOTE: with-safe-foreign-function-call-settings already has
         ;; without-interrupts, so no special treatment is needed here
         (labels ((build-exp (array-bindings &rest body)
                    (cond (array-bindings
                           `(fli:with-dynamic-lisp-array-pointer ,(first array-bindings)
                              ,(apply #'build-exp (rest array-bindings) body)))
                          (t`(locally ,@body)))))
           (apply #'build-exp array-bindings body)))))



;;; Type specifier for fortran types

;; Array type specifier: (array <element-type> *...)
;; String type specifier: :string
;; Complex type specifier: complex-float complex-double

;; NOTE: The function defined with defffun will return all the
;; non-array out-params and the return value in multiple values, the
;; first is the return value. If it's a fortran subroutine which means
;; the return type is :void, it may not return any value.

(defun make-arg (var-name var-type &optional (var-mode :in))
  (list var-name var-type (or var-mode :in)))

;;; helper functions
(defun string-type-p (var-type)
  ;; a string var has type :string
  (assert (or (symbolp var-type) (listp var-type)))
  (member var-type '(:string)))

(defun complex-type-p (var-type)
  (assert (or (symbolp var-type) (listp var-type)))
  (member var-type '(complex-float complex-double)))

(defun array-type-p (var-type)
  ;; array type: (:array <type> <dimensions>...)
  (assert (or (symbolp var-type) (listp var-type)))
  (eq (first (ensure-list var-type)) :array))


;;; The following function is for distinguish types with different
;;; ways of handling. There are basically 3 types:

;; 1) value-type args (char, int, float, double, complex, ...)
;; 2) pointer-type args (array, ...)
;; 3) string args (string will be converted automatically)

;; In fortran ABI, everything is a reference, so we need to pass
;; value-type args by reference. Pointer and string args are
;; unchanged.

(defun pointer-type-p (var-type)
  (assert (or (symbolp var-type) (listp var-type)))
  (and (not (string-type-p var-type))
       (not (member var-type '(complex-float complex-double)))
       (eq (cffi::canonicalize-foreign-type var-type) :pointer)))


;;; mode
(defun in-parameter-p (var-mode)
  (member var-mode '(:in :in-out)))

(defun out-parameter-p (var-mode)
  (member var-mode '(:out :in-out)))

;;; Accessor for argument spec
(defun var-name (arg)
  (first arg))

(defun var-type (arg)
  (second arg))

(defun var-mode (arg)
  (or (third arg) :in))

;;; 
(defun separate-items (predicate item-list &key key)
  ;; Return: (values filtered-items other-items)
  (assert (listp item-list)) 
  (loop for item in item-list
        for value = (if key (funcall key item) item)
	if (funcall predicate value)
	  collect item into filtered-items
	else
	  collect item into other-items
	finally (return (values filtered-items other-items))))

(defun separate-args (predicate args &key (key #'var-type))
  (assert (listp args))
  (separate-items
   (lambda (arg)
     (funcall predicate arg))
   args
   :key key))

(defun extract-string-args (args)
  (assert (listp args))
  (separate-args #'string-type-p args))

(defun extract-array-bindings (args)
  (assert (listp args))
  (separate-args #'array-type-p args))

(defun extract-pointer-type-args (args)
  (assert (listp args))
  (separate-args #'pointer-type-p args))

(defun extract-out-args (args)
  (assert (listp args))
  (separate-args #'out-parameter-p args :key #'var-mode))



;;; MACRO: defffun for fortran calling convention

;; Every non-array arguments will be passed by reference

;; TODO: reduce overhead
;; TODO: test passing structs
;; TODO: add type checking
;; TODO: add type declarations

;; utilities
(defun make-low-level-lisp-name (lisp-name)
  (intern (concatenate 'string "%" (symbol-name lisp-name))))

(defun make-fortran-name (foreign-name)
  #-windows (concatenate 'string foreign-name "_")
  #+windows (string-upcase foreign-name))

(defun canonicalize-args (args)
  (assert (every (lambda (a) (>= (length a) 2)) args))
  (loop for arg in args
        for var-name = (var-name arg)
        for var-type = (var-type arg)
        for var-mode = (or (var-mode arg) :in)
        collect (list var-name var-type var-mode)))

(defun process-function-name-and-options (name-and-options args)
  (multiple-value-bind (lisp-name foreign-name options)
      (cffi::parse-name-and-options name-and-options)
    (declare (ignorable lisp-name foreign-name options))
    ;; generate function names
    (let* ((internal-lisp-name (make-low-level-lisp-name lisp-name))
           (foreign-name (make-fortran-name foreign-name))
           (docstring (when (stringp (car args)) (pop args)))
           (args (canonicalize-args args)))
      (values lisp-name internal-lisp-name foreign-name docstring args))))

(defun process-args (args)
  ;; divide args to 3 categories
  (multiple-value-bind (string-args other-args)
      (extract-string-args args)
    (multiple-value-bind (pointer-type-args value-type-args)
        (extract-pointer-type-args other-args)
      (values value-type-args pointer-type-args string-args))))

(defun lookup (key map)
  (typecase map
    (list (let ((rec (assoc key map)))
            (if rec
                (values (cdr rec) t)
                (values nil nil))))
    (hash-table (gethash key map))))

;; build string-arg-handling, value-type-arg-handling, array-arg-handling forms
(defun build-body-with-value-type-args-handling (value-type-args
                                                 temp-value-type-arg-names
                                                 temp-value-type-bindings
                                                 &rest body)
  `(cffi:with-foreign-objects ,temp-value-type-bindings
     (setf
      ,@(loop for binding in value-type-args
              for temp-var in temp-value-type-arg-names
              if (not (member (second binding) '(complex-float complex-double)))
                collect `(cffi:mem-aref ,(cdr temp-var) ',(second binding))
                and 
                  collect `,(var-name binding)
              else    
                collect `(cffi:foreign-slot-value ,(cdr temp-var) ',(second binding) 'realpart)
                and
                  collect `(realpart ,(var-name binding))
                  and 
                    collect `(cffi:foreign-slot-value ,(cdr temp-var) ',(second binding) 'imagpart)
                    and 
                      collect `(imagpart ,(var-name binding))))
     (locally ,@body)))

(defun build-body-with-string-args-handling (string-bindings &rest body)
  `(cffi:with-foreign-strings ,string-bindings
     ,@body))

(defun build-body-with-array-args-handling (array-args &rest body)
  `(with-arrays-as-foreign-arrays
       ,(mapcar (lambda (array-arg)
                  (list (var-name array-arg) (var-name array-arg)))
                array-args)
     ,@body))

(defun substitute-args-with-temp-names (temp-value-type-arg-names
                                        temp-string-arg-names
                                        args)
  (loop for arg in args
        for var-name = (let ((v-name (lookup (var-name arg) temp-value-type-arg-names))
                             (s-name (lookup (var-name arg) temp-string-arg-names)))
                         (assert (not (and v-name s-name)))
                         (or v-name s-name))
        if var-name
          collect var-name
        else
          collect (var-name arg)))

(defun build-foreign-values-to-lisp-form (var-name var-type)
  (cond ((complex-type-p var-type)
         `(complex (cffi:foreign-slot-value ,var-name ',var-type 'realpart)
                   (cffi:foreign-slot-value ,var-name ',var-type 'imagpart)))
        ((string-type-p var-type)
         `(cffi:foreign-string-to-lisp ,var-name))
        ((array-type-p var-type)
         var-name)
        (t
         `(cffi:mem-aref ,var-name ',var-type))))

(defmacro defffun (name-and-options return-type &rest args)
  ;; deal with out parameters
  (multiple-value-bind (lisp-name internal-lisp-name foreign-name docstring args)
      (process-function-name-and-options name-and-options args)
    (declare (ignorable foreign-name))
    (multiple-value-bind (out-parameters in-parameters)
        (extract-out-args args)
      (declare (ignorable in-parameters))
      (multiple-value-bind (value-type-args pointer-type-args string-args)
          (process-args args)
        (assert (and (null (intersection value-type-args pointer-type-args :key #'var-name))
                     (null (intersection value-type-args string-args :key #'var-name))
                     (null (intersection pointer-type-args string-args :key #'var-name))))
        ;; generate new var names and binding forms
        (let* ((temp-value-type-arg-names (mapcar
                                           (lambda (binding)
                                             (let ((name (var-name binding)))
                                               (cons name (gensym (symbol-name name)))))
                                           value-type-args))
               (temp-value-type-bindings (mapcar
                                          (lambda (name binding)
                                            `(,(cdr name) ',(var-type binding)))
                                          temp-value-type-arg-names
                                          value-type-args))
               (temp-string-arg-names (mapcar
                                       (lambda (binding)
                                         (let ((name (var-name binding)))
                                           (cons name (gensym (symbol-name name)))))
                                       string-args))
               (temp-string-bindings (mapcar
                                      (lambda (name binding)
                                        (list (cdr name) (var-name binding)))
                                      temp-string-arg-names
                                      string-args))
               (array-args (extract-array-bindings pointer-type-args)))
          ;; build the body for lisp side
          (let ((invoke `(,internal-lisp-name
                          ,@(substitute-args-with-temp-names
                             temp-value-type-arg-names
                             temp-string-arg-names
                             args))))
            (when array-args
              (setf invoke (build-body-with-array-args-handling array-args invoke)))
            (let* ((body (with-unique-names (result)
                           ;; deal with the return value if it's a complex-number
                           ;; also return out params as multiple values
                           ;; don't return nil if the return type of the function is :void
                           `(let ((,result ,invoke))
                              (declare (ignorable ,result))
                              (values
                                ,@(unless (eq return-type :void)
                                    (if (or (string-type-p return-type)
                                            (not (pointer-type-p return-type)))
                                        `(,result)
                                        `(,(build-foreign-values-to-lisp-form result return-type))))
                                ,@(loop for arg in out-parameters
                                        collect (build-foreign-values-to-lisp-form
                                                 (or (or (cdr (assoc (var-name arg) temp-value-type-arg-names))
                                                         (cdr (assoc (var-name arg) temp-string-arg-names)))
                                                     (var-name arg))
                                                 (var-type arg)))))))) 
              (when string-args
                (setf body (build-body-with-string-args-handling temp-string-bindings body)))
              (when value-type-args
                (setf body (build-body-with-value-type-args-handling
                            value-type-args
                            temp-value-type-arg-names
                            temp-value-type-bindings
                            body)))
              ;; the final form 
              `(progn
                 (declaim (inline ,internal-lisp-name))
                 (cffi:defcfun (,internal-lisp-name ,foreign-name) ,return-type ; (,internal-lisp-name ,foreign-name)
                   ,@(mapcar (lambda (a) (list (first a) :pointer)) args))
                 (defun ,lisp-name ,(mapcar #'car args)
                   ,@(ensure-list docstring)
                   (with-safe-foreign-function-call-settings
                     ,body))))))))))

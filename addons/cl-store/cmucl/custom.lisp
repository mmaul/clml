;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.

(in-package :cl-store)

; special floats
(defun create-float-values (value &rest codes)
  "Returns a alist of special float to float code mappings."
  (ext:with-float-traps-masked (:overflow :invalid)
    (let ((neg-inf (expt value 3)))
      (mapcar 'cons
              (list (expt (abs value) 2)
                    neg-inf
                    (/ neg-inf neg-inf))
              codes))))

;; Custom Structures
(defstore-cl-store (obj structure-object stream)
  (output-type-code +structure-object-code+ stream)
  (store-type-object obj stream))

(defrestore-cl-store (structure-object stream)
  (restore-type-object stream))

;; Structure definitions
(defun get-layout (obj)
  (slot-value obj 'pcl::wrapper))

(defun get-info (obj)
  (declare (type kernel:layout obj))
  (slot-value obj 'ext:info))

(defun dd-name (dd)
  (slot-value dd 'kernel::name))

(defvar *cmucl-struct-inherits*
  (list (get-layout (find-class t))
        (get-layout (find-class 'kernel:instance))
        (get-layout (find-class 'cl:structure-object))))

(defstruct (struct-def (:conc-name sdef-))
  (supers (required-arg :supers) :type list)
  (info (required-arg :info) :type kernel:defstruct-description))

(defun info-or-die (obj)
  (let ((wrapper (get-layout obj)))
    (if wrapper
        (or (get-info wrapper) 
            (store-error "No defstruct-definition for ~A." obj))
        (store-error "No wrapper for ~A." obj))))

(defun save-able-supers (obj)
  (set-difference (coerce (slot-value (get-layout obj) 'kernel::inherits)
                          'list)
                  *cmucl-struct-inherits*))

(defun get-supers (obj)
  (loop for x in (save-able-supers obj) 
     collect (let ((name (dd-name (get-info x))))
               (if *store-class-superclasses* 
                   (find-class name)
                   name))))

(defstore-cl-store (obj structure-class stream)
  (output-type-code +structure-class-code+ stream)
  (store-object (make-struct-def :info (info-or-die obj)
                                 :supers (get-supers obj))
                stream))

(defstore-cl-store (obj struct-def stream)
  (output-type-code +struct-def-code+ stream)
  (store-object (sdef-supers obj) stream)
  (store-object (sdef-info obj) stream))

;; Restoring 
(defun cmu-struct-defs (dd)
  (append (kernel::define-constructors dd)
          (kernel::define-raw-accessors dd)
          (kernel::define-class-methods dd)))

(defun create-make-foo (dd)
  (let ((*compile-print* nil))
    (funcall (compile nil `(lambda () ,@(cmu-struct-defs dd))))
    (find-class (dd-name dd))))

(defun cmu-define-structure (dd supers)
  (cond ((or *nuke-existing-classes*  
             (not (find-class (dd-name dd) nil)))
         ;; create-struct
         (kernel::%defstruct dd supers)
         ;; compiler stuff
         ;;(kernel::%compiler-defstruct dd) 
         ;; create make-?
         (create-make-foo dd))
        (t (find-class (dd-name dd)))))
  
(defun super-layout (super)
  (etypecase super
    (symbol (get-layout (find-class super)))
    (structure-class 
     (super-layout (dd-name (info-or-die super))))))

(defun super-layouts (supers)
  (loop for super in supers 
     collect (super-layout super)))

(defrestore-cl-store (structure-class stream)
  (restore-object stream))
    
(defrestore-cl-store (struct-def stream)
  (let* ((supers (super-layouts (restore-object stream)))
         (dd (restore-object stream)))
    (cmu-define-structure dd (if supers 
                                 (coerce (append  *cmucl-struct-inherits*
                                                  supers)
                                         'vector)
                                 (coerce *cmucl-struct-inherits* 'vector)))))

;; EOF
;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.

(in-package :cl-store)

; special floats
(defun create-float-values (value &rest codes)
  "Returns a alist of special float to float code mappings."
  (sb-int:with-float-traps-masked (:overflow :invalid)
    (let ((neg-inf (expt value 3)))
      (mapcar 'cons
              (list (expt (abs value) 2)
                    neg-inf
                    (/ neg-inf neg-inf))
              codes))))

;; Custom structure storing

(defstore-cl-store (obj structure-object stream)
  (output-type-code +structure-object-code+ stream)
  (store-type-object obj stream))

(defrestore-cl-store (structure-object stream)
  (restore-type-object stream))


;; Structure definition storing
(defun get-layout (obj)
  (slot-value obj 'sb-pcl::wrapper))

(defun get-info (obj)
  (declare (type sb-kernel:layout obj))
  (slot-value obj 'sb-int:info))

(defun dd-name (dd)
  (slot-value dd 'sb-kernel::name))

(defvar *sbcl-struct-inherits*
  `(,(get-layout (find-class t))
    ,@(when-let (class (find-class 'sb-kernel:instance nil))
        (list (get-layout class)))
    ,(get-layout (find-class 'cl:structure-object))))

(defstruct (struct-def (:conc-name sdef-))
  (supers (required-arg :supers) :type list)
  (info (required-arg :info) :type sb-kernel:defstruct-description))

(defun info-or-die (obj)
  (let ((wrapper (get-layout obj)))
    (if wrapper
        (or (get-info wrapper) 
            (store-error "No defstruct-definition for ~A." obj))
        (store-error "No wrapper for ~A." obj))))

(defun save-able-supers (obj)
  (set-difference (coerce (slot-value (get-layout obj) 'sb-kernel::inherits)
                          'list)
                  *sbcl-struct-inherits*))

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
(defun sbcl-struct-defs (info)
  (append (sb-kernel::constructor-definitions info)
          (sb-kernel::class-method-definitions info)))

(defun create-make-foo (dd)
  (declare (optimize speed))
  (funcall (compile nil `(lambda () ,@(sbcl-struct-defs dd))))
  (find-class (dd-name dd)))

;;; with apologies to christophe rhodes ...
;; takes a source location as a third argument.
(eval-when (:compile-toplevel)
  (defun split (string &optional max (ws '(#\Space #\Tab)))
    (flet ((is-ws (char) (find char ws)))
      (nreverse
       (let ((list nil) (start 0) (words 0) end)
         (loop
          (when (and max (>= words (1- max)))
            (return (cons (subseq string start) list)))
          (setf end (position-if #'is-ws string :start start))
          (push (subseq string start end) list)
          (incf words)
          (unless end (return list))
          (setf start (1+ end))))))))

;; From 0.9.6.25 sb-kernel::%defstruct
;; takes a source location as a third argument.
(eval-when (:compile-toplevel)
  (labels ((make-version (string)
             (map-into (make-list 4 :initial-element 0)
                       #'(lambda (part)
                           (parse-integer part :junk-allowed t))
                       (split string nil '(#\.))))
           (version>= (v1 v2)
             (loop for x in (make-version v1)
                   for y in (make-version v2)
                   when (> x y) :do (return t)
                   when (> y x) :do (return nil)
                   finally (return t))))
    (when (version>= (lisp-implementation-version)
                     "0.9.6.25")
      (pushnew :defstruct-has-source-location *features*))))

(defun sb-kernel-defstruct (dd supers source)
  (declare (ignorable source))
  #+defstruct-has-source-location 
  (sb-kernel::%defstruct dd supers source)
  #-defstruct-has-source-location
  (sb-kernel::%defstruct dd supers))

(defun sbcl-define-structure (dd supers)
  (cond ((or *nuke-existing-classes*  
             (not (find-class (dd-name dd) nil)))
         ;; create-struct
         (sb-kernel-defstruct dd supers nil)
         ;; compiler stuff
         (sb-kernel::%compiler-defstruct dd supers) 
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
    (sbcl-define-structure dd (if supers 
                                  (coerce (append  *sbcl-struct-inherits*
                                                   supers)
                                          'vector)
                                  (coerce *sbcl-struct-inherits* 'vector)))))

;; EOF
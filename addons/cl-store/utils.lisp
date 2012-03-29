;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.

;; Miscellaneous utilities used throughout the package.
(in-package :cl-store)

(defmacro aif (test then &optional else)
  `(let ((it ,test))
    (if it ,then ,else)))

(defmacro with-gensyms (names &body body)
  `(let ,(mapcar #'(lambda (x) `(,x (gensym))) names)
    ,@body))

(defgeneric serializable-slots (object)
  (declare (optimize speed))
  (:documentation 
   "Return a list of slot-definitions to serialize. The default
    is to call serializable-slots-using-class with the object 
    and the objects class")
  (:method ((object standard-object))
   (serializable-slots-using-class object (class-of object)))
#+(or sbcl cmu openmcl)
  (:method ((object structure-object))
   (serializable-slots-using-class object (class-of object)))
  (:method ((object condition))
   (serializable-slots-using-class object (class-of object))))

; unfortunately the metaclass of conditions in sbcl and cmu 
; are not standard-class

(defgeneric serializable-slots-using-class (object class)
  (declare (optimize speed))
  (:documentation "Return a list of slot-definitions to serialize.
   The default calls compute slots with class")
  (:method ((object t) (class standard-class))
   (class-slots class))
#+(or sbcl cmu openmcl) 
  (:method ((object t) (class structure-class))
   (class-slots class))
#+sbcl
  (:method ((object t) (class sb-pcl::condition-class))
   (class-slots class))
#+cmu
  (:method ((object t) (class pcl::condition-class))
   (class-slots class)))


; Generify get-slot-details for customization (from Thomas Stenhaug)
#-abcl
(defgeneric get-slot-details (slot-definition)
  (declare (optimize speed))
  (:documentation 
   "Return a list of slot details which can be used 
    as an argument to ensure-class")
  (:method ((slot-definition #+(or ecl (and clisp (not mop))) t 
                             #-(or ecl (and clisp (not mop))) slot-definition))
   (list :name (slot-definition-name slot-definition)
         :allocation (slot-definition-allocation slot-definition)
         :initargs (slot-definition-initargs slot-definition)
         ;; :initform. dont use initform until we can
         ;; serialize functions
         :readers (slot-definition-readers slot-definition)
         :type (slot-definition-type slot-definition)
         :writers (slot-definition-writers slot-definition)))
  #+openmcl
  (:method ((slot-definition ccl::structure-slot-definition))
   (list :name (slot-definition-name slot-definition)
         :allocation (slot-definition-allocation slot-definition)
         :initargs (slot-definition-initargs slot-definition)
         ;; :initform. dont use initform until we can
         ;; serialize functions
         ;; :readers (slot-definition-readers slot-definition)
         :type (slot-definition-type slot-definition)
         ;; :writers (slot-definition-writers slot-definition)
         )))

(defmacro when-let ((var test) &body body)
  `(let ((,var ,test))
     (when ,var
       ,@body)))


;; because clisp doesn't have the class single-float or double-float.
(defun float-type (float)
  (etypecase float
    (single-float 0)
    (double-float 1)
    (short-float 2)
    (long-float 3)))

(defun get-float-type (num)
  (ecase num
    (0 1.0)
    (1 1.0d0)
    (2 1.0s0)
    (3 1.0l0)))

(deftype ub32 ()
  `(unsigned-byte 32))

(deftype sb32 ()
  `(signed-byte 32))

(deftype array-size ()
  "The maximum size of a vector"
  `(integer 0 , array-dimension-limit))

(deftype array-tot-size ()
  "The maximum total size of an array"
  `(integer 0 , array-total-size-limit))

(defun store-32-bit (obj stream)
  "Write OBJ down STREAM as a 32 bit integer."
  (declare (optimize speed (debug 0) (safety 0))
           (type ub32 obj))
    (write-byte (ldb (byte 8 0) obj) stream)
    (write-byte (ldb (byte 8 8) obj) stream)
    (write-byte (ldb (byte 8 16) obj) stream)
    (write-byte (+ 0 (ldb (byte 8 24) obj)) stream))

(defmacro make-ub32 (a b c d)
  `(the ub32 (logior (ash ,a 24) (ash ,b 16) (ash ,c 8) ,d)))

(defun read-32-bit (buf &optional (signed t))
  "Read a signed or unsigned byte off STREAM."
  (declare (optimize speed (debug 0) (safety 0)))
  (let ((byte1 (read-byte buf))
        (byte2 (read-byte buf))
        (byte3 (read-byte buf))
        (byte4 (read-byte buf)))
    (declare (type (mod 256) byte1 byte2 byte3 byte4))
    (let ((ret (make-ub32 byte4 byte3 byte2 byte1)))
      (if (and signed (> byte1 127))
          (logior (ash -1 32) ret)
          ret))))

(defun kwd (name)
  (values (intern (string-upcase name) :keyword)))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (x args)
      (format s "~@:(~A~)" x))))

(defun symbolicate (&rest syms)
  "Concatenate all symbol names into one big symbol"
  (values (intern (apply #'mkstr syms))))

;; Taken straight from swank.lisp --- public domain
;; and then slightly modified
(defun safe-length (list)
  "Similar to `list-length', but avoid errors on improper lists.
Return two values: the length of the list and the last cdr.
Modified to work on non proper lists."
  (do ((n 0 (+ n 2))                    ;Counter.
       (fast list (cddr fast))          ;Fast pointer: leaps by 2.
       (slow list (cdr slow)))          ;Slow pointer: leaps by 1.
      (nil)
    (cond ((null fast) (return (values n nil)))
          ((not (consp fast)) (return (values n fast)))
          ((null (cdr fast)) (return (values (1+ n) (cdr fast))))
          ((and (eq fast slow) (> n 0)) (return (values (/ n 2) list)))
          ((not (consp (cdr fast))) (return (values (1+ n) (cdr fast)))))))

;; EOF

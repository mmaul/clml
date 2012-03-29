7;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.

;; The cl-store backend. 
(in-package :cl-store)

(defbackend cl-store :magic-number 1395477571
            :stream-type '(unsigned-byte 8)
            :old-magic-numbers (1912923 1886611788 1347635532 1886611820 1414745155
                                1349740876 1884506444 1347643724 1349732684 1953713219
                                1416850499)
            :extends (resolving-backend)
            :fields ((restorers :accessor restorers 
                                :initform (make-hash-table :size 100))))

(defun register-code (code name &optional (errorp nil))
  (aif (and (gethash code (restorers (find-backend 'cl-store))) errorp)
       (error "Code ~A is already defined for ~A." code name)
       (setf (gethash code (restorers (find-backend 'cl-store)))
             name))
  code)


;;  Type code constants
(defparameter +referrer-code+ (register-code 1 'referrer))
(defparameter +special-float-code+ (register-code 2 'special-float))
(defparameter +unicode-string-code+ (register-code 3 'unicode-string))
(defparameter +integer-code+ (register-code 4 'integer))
(defparameter +simple-string-code+ (register-code 5 'simple-string))
(defparameter +float-code+ (register-code 6 'float))
(defparameter +ratio-code+ (register-code 7 'ratio))
(defparameter +character-code+ (register-code 8 'character))
(defparameter +complex-code+ (register-code 9 'complex))
(defparameter +symbol-code+ (register-code 10 'symbol))
(defparameter +cons-code+ (register-code 11 'cons))
(defparameter +pathname-code+ (register-code 12 'pathname))
(defparameter +hash-table-code+ (register-code 13 'hash-table))
(defparameter +standard-object-code+ (register-code 14 'standard-object))
(defparameter +condition-code+ (register-code 15 'condition))
(defparameter +structure-object-code+ (register-code 16 'structure-object))
(defparameter +standard-class-code+ (register-code 17 'standard-class))
(defparameter +built-in-class-code+ (register-code 18 'built-in-class))
(defparameter +array-code+ (register-code 19 'array))
(defparameter +simple-vector-code+ (register-code 20 'simple-vector))
(defparameter +package-code+ (register-code 21 'package))
(defparameter +simple-byte-vector-code+ (register-code 22 'simple-byte-vector))

;; fast storing for 32 bit ints
(defparameter +32-bit-integer-code+ (register-code 24 '32-bit-integer))

(defparameter +function-code+ (register-code 26 'function nil))
(defparameter +gf-code+ (register-code 27 'generic-function nil))

;; Used by SBCL and CMUCL.
(defparameter +structure-class-code+ (register-code 28 'structure-class nil))
(defparameter +struct-def-code+ (register-code 29 'struct-def nil))

(defparameter +gensym-code+ (register-code 30 'gensym nil))

(defparameter +unicode-base-string-code+ (register-code 34 'unicode-base-string nil))
(defparameter +simple-base-string-code+ (register-code 35 'simple-base-string nil))

;; setups for type code mapping
(defun output-type-code (code stream)
  (declare (type ub32 code))
  (write-byte (ldb (byte 8 0) code) stream))

(declaim (inline read-type-code))
(defun read-type-code (stream)
  (read-byte stream))

(defmethod referrerp ((backend cl-store) (reader t))
  (declare (optimize speed (safety 0) (space 0) (debug 0)))
  (eql reader 'referrer))

(defparameter *restorers* (restorers (find-backend 'cl-store)))

;; get-next-reader needs to return a symbol which will be used by the
;; backend to lookup the function that was defined by
;; defrestore-cl-store to restore it, or nil if not found. 
(defun lookup-code (code)
  (declare (optimize speed (safety 0) (space 0) (debug 0)))
  (gethash code *restorers*))

(defmethod get-next-reader ((backend cl-store) (stream stream))
  (declare (optimize speed (safety 0) (space 0) (debug 0)))
  (let ((type-code (read-type-code stream)))
    (or (lookup-code type-code)
        (error "Type code ~A is not registered." type-code))))


;; referrer, Required for a resolving backend
(defmethod store-referrer ((backend cl-store) (ref t) (stream t))
  (output-type-code +referrer-code+ stream)
  (dump-int ref stream))

(defrestore-cl-store (referrer stream)
  (make-referrer :val (undump-int stream)))



;; integers
;; The theory is that most numbers will fit in 32 bits 
;; so we we have a little optimization for it

;; We need this for circularity stuff.
(defmethod int-or-char-p ((backend cl-store) (type symbol))
  (declare (optimize speed (safety 0) (space 0) (debug 0)))
  (or (eql type '32-bit-integer)
      (eql type 'integer)
      (eql type 'character)))

(defstore-cl-store (obj integer stream)
  (declare (optimize speed (safety 1) (debug 0)))
  (if (typep obj 'sb32)
      (store-32-bit-integer obj stream)
      (store-arbitrary-integer obj stream)))

(defun dump-int (obj stream)
  (declare (optimize speed (safety 0) (debug 0)))
  (etypecase obj 
    ((unsigned-byte 8) (write-byte 1 stream) (write-byte obj stream))
    ((unsigned-byte 32) (write-byte 2 stream) (store-32-bit obj stream))))

(defun undump-int (stream)
  (declare (optimize speed (safety 0) (debug 0)))
  (ecase (read-byte stream)
    (1 (read-byte stream))
    (2 (read-32-bit stream nil))))

(defun store-32-bit-integer (obj stream)
  (declare (optimize speed (safety 1) (debug 0)) (type sb32 obj))
  (output-type-code +32-bit-integer-code+ stream)
  (write-byte (if (minusp obj) 1 0) stream)
  (dump-int (abs obj) stream))

(defrestore-cl-store (32-bit-integer stream)
  (declare (optimize speed (safety 1) (debug 0)))
  (funcall (if (zerop (the fixnum (read-byte stream))) #'+ #'-)
           (undump-int stream)))


(defun num->bits (num )
  (loop for val = (abs num) then (ash val -8 )
        for count from 0
        until (zerop val)
        collect (logand val #XFF) into bits
        finally (return (values bits count))))

(defun store-arbitrary-integer (obj stream)
  (declare (type integer obj) (stream stream)
           (optimize speed))
  (output-type-code +integer-code+ stream)
  (multiple-value-bind (bits count) (num->bits obj)
    (store-object (if (minusp obj) (- count) count)
                  stream)
    (dolist (x bits) (store-32-bit x stream))))


(defrestore-cl-store (integer buff)
  (declare (optimize speed))
  (let ((count (restore-object buff)))
    (loop repeat (abs count)
          with sum = 0
          for pos from 0 by 8
          for bit = (read-32-bit buff nil)
          finally (return (if (minusp count) (- sum) sum))
          :do
          (incf sum (* bit (expt 2 pos))))))

        

(defun bits->num (bits)
  (loop with sum = 0
        for pos from 0 by 8
        for bit in bits
        finally (return sum)
        :do (incf sum (* bit (expt 2 pos)))))



;; Floats (*special-floats* are setup in the custom.lisp files)

(defconstant +short-float-inf+ 0)
(defconstant +short-float-neg-inf+ 1)
(defconstant +short-float-nan+ 2)

(defconstant +single-float-inf+ 3)
(defconstant +single-float-neg-inf+ 4)
(defconstant +single-float-nan+ 5)

(defconstant +double-float-inf+ 6)
(defconstant +double-float-neg-inf+ 7)
(defconstant +double-float-nan+ 8)

(defconstant +long-float-inf+ 9)
(defconstant +long-float-neg-inf+ 10)
(defconstant +long-float-nan+ 11)

(defvar *special-floats* nil)

;; Implementations are to provide an implementation for the create-float-value
;; function
(defun create-float-values (value &rest codes)
  "Returns a alist of special float to float code mappings."
  (declare (ignore value codes))
  nil)

(defun setup-special-floats ()
  (setf *special-floats*
        (nconc (create-float-values most-negative-short-float +short-float-inf+
                                    +short-float-neg-inf+ +short-float-nan+)
               (create-float-values most-negative-single-float +single-float-inf+
                                    +single-float-neg-inf+ +single-float-nan+)
               (create-float-values most-negative-double-float +double-float-inf+
                                    +double-float-neg-inf+ +double-float-nan+)
               (create-float-values most-negative-long-float +long-float-inf+
                                    +long-float-neg-inf+ +long-float-nan+))))

(defstore-cl-store (obj float stream)
  (declare (optimize speed))
  (block body
    (let (significand exponent sign)
      (handler-bind (((or simple-error arithmetic-error type-error)
                      #'(lambda (err)
                          (declare (ignore err))
                          (when-let (type (cdr (assoc obj *special-floats*)))
                            (output-type-code +special-float-code+ stream)
                            (write-byte type stream)
                            (return-from body)))))
        (multiple-value-setq (significand exponent sign)
            (integer-decode-float obj))
        (output-type-code +float-code+ stream)
        (write-byte (float-type obj) stream)
        (store-object significand stream)
        (store-object (float-radix obj) stream)
        (store-object exponent stream)
        (store-object sign stream)))))

(defrestore-cl-store (float stream)
  (float (* (the float (get-float-type (read-byte stream)))
            (* (the integer (restore-object stream))
               (expt (the integer (restore-object stream))
                     (the integer (restore-object stream))))
            (the integer (restore-object stream)))))

(defrestore-cl-store (special-float stream)
  (or (car (rassoc (read-byte stream) *special-floats*))
      (restore-error "Float ~S is not a valid special float.")))


;; ratio
(defstore-cl-store (obj ratio stream)
  (output-type-code +ratio-code+ stream)
  (store-object (numerator obj) stream)
  (store-object (denominator obj) stream))

(defrestore-cl-store (ratio stream)
  (/ (the integer (restore-object stream))
     (the integer (restore-object stream))))

;; chars
(defstore-cl-store (obj character stream)
  (output-type-code +character-code+ stream)    
  (store-object (char-code obj) stream))

(defrestore-cl-store (character stream)
  (code-char (restore-object stream)))

;; complex
(defstore-cl-store (obj complex stream)
  (output-type-code +complex-code+ stream)    
  (store-object (realpart obj) stream)
  (store-object (imagpart obj) stream))

(defrestore-cl-store (complex stream)
  (complex (restore-object stream)
           (restore-object stream)))

;; symbols
(defstore-cl-store (obj symbol stream)
  (declare (optimize speed))
  (cond ((symbol-package obj)
         (output-type-code +symbol-code+ stream)
         (store-object (symbol-name obj) stream)
         (store-object (package-name (symbol-package obj))
                       stream))
        ;; Symbols with no home package 
        (t (output-type-code +gensym-code+ stream)
           (store-object (symbol-name obj) stream))))

(defrestore-cl-store (symbol stream)
  (values (intern (restore-object stream)
                  (restore-object stream))))

(defrestore-cl-store (gensym stream)
  (make-symbol (restore-object stream)))


;; Lists
(defun dump-list (list length last stream)
  (declare (optimize speed (safety 1) (debug 0))
           (type cons list))
  (output-type-code +cons-code+ stream)
  (store-object length stream)
  (loop repeat length 
        for x on list do
        (store-object (car x) stream))
  (store-object last stream))

(defun restore-list (stream)
  (declare (optimize speed (safety 1) (debug 0)))
  (let* ((conses (restore-object stream))
         (ret ())
         (tail ret))
    (dotimes (x conses)
      (let ((obj (restore-object stream)))
        ;; we can't use setting here since we wan't to
        ;; be fairly efficient when adding objects to the
        ;; end of the list.
        (when (and *check-for-circs* (referrer-p obj))
          (let ((x x))
            (push (delay (setf (nth x ret)
                               (referred-value obj *restored-values*)))
                  *need-to-fix*)))
        (if ret
            (setf (cdr tail) (list obj) 
                  tail (cdr tail))
            (setf ret (list obj)
                  tail (last ret)))))
    (let ((last1 (restore-object stream)))
      ;; and check for the last possible circularity
      (if (and *check-for-circs* (referrer-p last1))
          (push (delay (setf (cdr tail)
                             (referred-value last1 *restored-values*)))
                *need-to-fix*)
          (setf (cdr tail) last1)))
    ret))

(defstore-cl-store (list cons stream)
  (multiple-value-bind (length last) (safe-length list)
    (dump-list list length last stream)))

(defrestore-cl-store (cons stream)
  (restore-list stream))


;; pathnames
(defstore-cl-store (obj pathname stream)
  (output-type-code +pathname-code+ stream)
  (store-object (pathname-device obj) stream)
  (store-object (pathname-directory obj) stream)
  (store-object (pathname-name obj) stream)
  (store-object (pathname-type obj) stream)
  (store-object (pathname-version obj) stream))

(defrestore-cl-store (pathname stream)
  (make-pathname    
   :device (restore-object stream)
   :directory (restore-object stream)
   :name (restore-object stream)
   :type (restore-object stream)
   :version (restore-object stream)))


;; hash tables
(defstore-cl-store (obj hash-table stream)
  (declare (optimize speed))
  (output-type-code +hash-table-code+ stream)    
  (store-object (hash-table-rehash-size obj) stream)
  (store-object (hash-table-rehash-threshold obj) stream)
  (store-object (hash-table-size obj) stream)
  (store-object (hash-table-test obj) stream)
  (store-object (hash-table-count obj) stream)
  (loop for key being the hash-keys of obj
        using (hash-value value) do
        (store-object key stream)
        (store-object value stream)))

(defrestore-cl-store (hash-table stream)
  (let ((rehash-size (restore-object stream))
        (rehash-threshold (restore-object stream))
        (size (restore-object stream))
        (test (restore-object stream))
        (count (restore-object stream)))
    (declare (type integer count size))
    (let ((hash (make-hash-table :test test
                                 :rehash-size rehash-size
                                 :rehash-threshold rehash-threshold
                                 :size size)))
      (resolving-object (x hash)
        (loop repeat count do
              ;; Unfortunately we can't use the normal setting here
              ;; since there could be a circularity in the key
              ;; and we need to make sure that both objects are 
              ;; removed from the stream at this point.
              (setting-hash (restore-object stream) 
                            (restore-object stream))))
      hash)))

;; The dumping of objects works by serializing  the type of the object which
;; is followed by applicable slot-name and value (depending on whether the
;; slot is bound, it's allocation and *store-class-slots*). Once each slot
;; is serialized a counter is incremented which is stored  at the end.
;; When restoring the object a new instance is allocated and then
;; restore-type-object starts reading objects from the stream.
;; If the restored object is a symbol the it names a slot and it's value
;; is pulled out and set on the newly allocated object.
;; If the restored object is an integer then this is the end marker
;; for the object and the number of slots restored is checked against
;; this counter.

;; Object and Conditions
(defun store-type-object (obj stream)
  (declare (optimize speed))
  (let ((all-slots (serializable-slots obj))
        (length 0))
    (store-object (type-of obj) stream)
    (dolist (slot all-slots)
      (let ((slot-name (slot-definition-name slot)))
        (when (and (slot-boundp obj slot-name)
                   (or *store-class-slots*
                       (not (eql (slot-definition-allocation slot)
                                 :class))))
          (store-object (slot-definition-name slot) stream)
          (store-object (slot-value obj slot-name) stream)
          (incf length))))
    (store-object length stream)))

(defstore-cl-store (obj standard-object stream)
  (output-type-code +standard-object-code+ stream)    
  (store-type-object obj stream))

(defstore-cl-store (obj condition stream)
  (output-type-code +condition-code+ stream)    
  (store-type-object obj stream))

(defun restore-type-object (stream)
  (declare (optimize speed))
  (let* ((class (find-class (restore-object stream)))
         (new-instance (allocate-instance class)))
    (resolving-object (obj new-instance)
      (loop for count from 0 do
            (let ((slot-name (restore-object stream)))
              (etypecase slot-name
                (integer (assert (= count slot-name) (count slot-name)
                           "Number of slots restored does not match slots stored.")
                         (return))
                (symbol 
                 ;; slot-names are always symbols so we don't
                 ;; have to worry about circularities
                 (setting (slot-value obj slot-name) (restore-object stream)))))))
    new-instance))

(defrestore-cl-store (standard-object stream)
  (restore-type-object stream))

(defrestore-cl-store (condition stream)
  (restore-type-object stream))


;; classes
(defstore-cl-store (obj standard-class stream)
  (output-type-code +standard-class-code+ stream)
  (store-object (class-name obj) stream)
  (store-object (mapcar #'get-slot-details (class-direct-slots obj))
                stream)
  (store-object (mapcar (if *store-class-superclasses*
                            #'identity 
                            #'class-name)
                        (class-direct-superclasses obj))
                stream)
  (store-object (type-of obj) stream))

(defrestore-cl-store (standard-class stream)
  (let* ((class (restore-object stream))
         (slots (restore-object stream))
         (supers (restore-object stream))
         (meta (restore-object stream))
         (keywords '(:direct-slots :direct-superclasses
                     :metaclass))
         (final (loop for keyword in keywords
                      for slot in (list slots 
                                        (or supers (list 'standard-object))
                                        meta)
                      nconc (list keyword slot))))
    (cond ((find-class class nil)
           (cond (*nuke-existing-classes*
                  (apply #'ensure-class class final)
                  #+(and clisp (not mop)) (add-methods-for-class class slots))
                 (t (find-class class))))
          (t (apply #'ensure-class class final)
             #+(and clisp (not mop)) (add-methods-for-class class slots)))))

;; built in classes

(defstore-cl-store (obj built-in-class stream)
  (output-type-code +built-in-class-code+ stream)
  (store-object (class-name obj) stream))

#-ecl ;; for some reason this doesn't work with ecl
(defmethod internal-store-object ((backend cl-store) (obj (eql (find-class 'hash-table))) stream)
  (output-type-code +built-in-class-code+ stream)
  (store-object 'cl:hash-table stream))

(defrestore-cl-store (built-in-class stream)
  (find-class (restore-object stream)))


;; Arrays, vectors and strings.
(defstore-cl-store (obj array stream)
  (declare (optimize speed (safety 1) (debug 0)))
  (typecase obj
    (simple-base-string (store-simple-base-string obj stream))
    (simple-string (store-simple-string obj stream))
    (simple-vector (store-simple-vector obj stream))
    ((simple-array (unsigned-byte 8) (*)) (store-simple-byte-vector obj stream))
    (t (store-array obj stream))))


(defun store-array (obj stream)
  (declare (optimize speed (safety 0) (debug 0))
           (type array obj))
  (output-type-code +array-code+ stream)
  (if (and (= (array-rank obj) 1)
           (array-has-fill-pointer-p obj))
      (store-object (fill-pointer obj) stream)
      (store-object nil stream))
  (store-object (array-element-type obj) stream)
  (store-object (adjustable-array-p obj) stream)
  (store-object (array-dimensions obj) stream)
  (dolist (x (multiple-value-list (array-displacement obj)))
    (store-object x stream))
  (store-object (array-total-size obj) stream)
  (loop for x from 0 below (array-total-size obj) do
        (store-object (row-major-aref obj x) stream)))

 


(defrestore-cl-store (array stream)
  (declare (optimize speed (safety 1) (debug 0)))
  (let* ((fill-pointer (restore-object stream))
         (element-type (restore-object stream))
         (adjustable (restore-object stream))
         (dimensions (restore-object stream))
         (displaced-to (restore-object stream))
         (displaced-offset (restore-object stream))
         (size (restore-object stream))
         (res (make-array dimensions  
                          :element-type element-type
                          :adjustable adjustable
                          :fill-pointer fill-pointer)))
    (declare (type cons dimensions) (type array-tot-size size))
    (when displaced-to 
      (adjust-array res dimensions :displaced-to displaced-to
                    :displaced-index-offset displaced-offset))
    (resolving-object (obj res)
      (loop for x from 0 below size do
            (let ((pos x))
              (setting (row-major-aref obj pos) (restore-object stream)))))))

(defun store-simple-vector (obj stream)
  (declare (optimize speed (safety 0) (debug 0))
           (type simple-vector obj))
  (output-type-code +simple-vector-code+ stream)
  (store-object (length obj) stream)
  (loop for x across obj do
    (store-object x stream)))

(defrestore-cl-store (simple-vector stream)
  (declare (optimize speed (safety 1) (debug 0)))
  (let* ((size (restore-object stream))
         (res (make-array size)))
    (declare (type array-size size))
    (resolving-object (obj res)
      (dotimes (i size)
        ;; we need to copy the index so that
        ;; it's value at this time is preserved.
        (let ((x i)) 
          (setting (aref obj x) (restore-object stream)))))
    res))

(defun store-simple-byte-vector (obj stream)
  (declare (optimize speed (safety 0) (debug 0))
           (type (simple-array (unsigned-byte 8) (*)) obj))
  (output-type-code +simple-byte-vector-code+ stream)
  (store-object (length obj) stream)
  (loop for x across obj do
        (write-byte x stream)))
 
(defrestore-cl-store (simple-byte-vector stream)
  (declare (optimize speed (safety 1) (debug 0)))
  (let* ((size (restore-object stream))
         (res (make-array size :element-type '(unsigned-byte 8))))
    (declare (type array-size size))
    (resolving-object (obj res)
      (dotimes (i size)
        ;; we need to copy the index so that
        ;; it's value at this time is preserved.
        (let ((x i)) 
          (setting (aref obj x) (read-byte stream)))))
    res))

;; Dumping (unsigned-byte 32) for each character seems
;; like a bit much when most of them will be 
;; base-chars. So we try to cater for them.
(defvar *char-marker* (code-char 255)
  "Largest character that can be represented in 8 bits")

(defun unicode-string-p (string)
  "An implementation specific test for a unicode string."
  (declare (optimize speed (safety 0) (debug 0))
           (type simple-string string))
  #+cmu nil ;; cmucl doesn't support unicode yet.
  #+lispworks (not (typep string 'lw:8-bit-string))
  #-(or cmu lispworks) (some #'(lambda (x) (char> x *char-marker*)) string))

(defun store-simple-string (obj stream)
  (declare (type simple-string obj)
           (optimize speed (safety 1) (debug 0)))
  (cond ((unicode-string-p obj)
         (output-type-code +unicode-string-code+ stream)
         (dump-string #'dump-int obj stream))
        (t (output-type-code +simple-string-code+ stream)
           (dump-string #'write-byte obj stream))))

(defun store-simple-base-string (obj stream)
  (declare (type simple-string obj)
           (optimize speed (safety 1) (debug 0)))
  (cond ((unicode-string-p obj)
         (output-type-code +unicode-base-string-code+ stream)
         (dump-string #'dump-int obj stream))
        (t (output-type-code +simple-base-string-code+ stream)
           (dump-string #'write-byte obj stream))))

(defun dump-string (dumper obj stream)
  (declare (simple-string obj) (function dumper) (stream stream)
           (optimize speed (safety 1) (debug 0)))
  (dump-int (the array-size (length obj)) stream)
  (loop for x across obj do (funcall dumper (char-code x) stream)))

(defrestore-cl-store (simple-string stream)
  (declare (optimize speed))
  (undump-string #'read-byte 'character stream))

(defrestore-cl-store (unicode-string stream)
  (declare (optimize speed))
  (undump-string #'undump-int 'character stream))

(defrestore-cl-store (simple-base-string stream)
  (declare (optimize speed))
  (undump-string #'read-byte 'base-char stream))

(defrestore-cl-store (unicode-base-string stream)
  (declare (optimize speed))
  (undump-string #'undump-int 'base-char stream))

(defun undump-string (reader type stream)
  (declare (type function reader) (type stream stream)
           (optimize speed (safety 1) (debug 0)))
  (let* ((length (the array-size (undump-int stream)) )
         (res (make-string length :element-type type)))
    (declare (type simple-string res))
    (dotimes (x length)
      (setf (schar res x) (code-char (funcall reader stream))))
    res))

;; packages (from Thomas Stenhaug)
(defstore-cl-store (obj package stream)
  (output-type-code +package-code+ stream)  
  (store-object (package-name obj) stream)
  (store-object (package-nicknames obj) stream)
  (store-object (mapcar (if *store-used-packages* #'identity #'package-name)
                        (package-use-list obj))
                stream)
  (store-object (internal-symbols obj) stream)
  (store-object (package-shadowing-symbols obj) stream)
  (store-object (external-symbols obj) stream))

(defun remove-remaining (times stream)
  (declare (optimize speed) (type fixnum times))
  (dotimes (x times)
    (restore-object stream)))

(defrestore-cl-store (package stream)
  (let* ((package-name (restore-object stream))
         (existing-package (find-package package-name)))
    (cond ((or (not existing-package)
               (and existing-package *nuke-existing-packages*))
           (restore-package package-name stream :force *nuke-existing-packages*))
          (t (remove-remaining 5 stream)
             existing-package))))

(defun internal-symbols (package)
  (let ((acc (make-array 100 :adjustable t :fill-pointer 0))
        (used (package-use-list package)))
    (do-symbols (symbol package)
      (unless (find (symbol-package symbol) used)
        (vector-push-extend symbol acc)))
    acc))

(defun external-symbols (package)
  (let ((acc (make-array 100 :adjustable t :fill-pointer 0)))
    (do-external-symbols (symbol package)
      (vector-push-extend symbol acc))
    acc))

(defun restore-package (package-name stream &key force)
  (when (and force (find-package package-name))
    (delete-package package-name))
  (let ((package (make-package package-name
			       :nicknames (restore-object stream)
			       :use (restore-object stream))))
    (loop for symbol across (restore-object stream) do
      (import symbol package))
    (shadow (restore-object stream) package)
    (loop for symbol across (restore-object stream) do
      (export symbol package))
    package))

;; Function storing hack.
;; This just stores the function name if we can find it
;; or signal a store-error.
(defun parse-name (name)
  (let ((name (subseq name 21)))
    (declare (type simple-string name))
    (if (search name "SB!" :end1 3)
        (replace name "SB-" :end1 3)
        name)))

#+sbcl
(defvar *sbcl-readtable* (copy-readtable nil))
#+sbcl
(set-macro-character #\# #'(lambda (c s) 
                             (declare (ignore c s))
                             (store-error "Invalid character in function name."))
                     nil
                     *sbcl-readtable*)

(defun get-function-name (obj)
  (multiple-value-bind (l cp name) (function-lambda-expression obj) 
    (declare (ignore l cp))
    (cond ((and name (or (symbolp name) (consp name))) name)
          ;;  Try to deal with sbcl's naming convention
          ;; of built in functions (pre 0.9)
          #+sbcl
          ((and name (stringp name)
                (search "top level local call " (the simple-string name)))
           (let ((new-name (parse-name name))
                 (*readtable* *sbcl-readtable*))
             (unless (string= new-name "")
               (handler-case (read-from-string new-name)
                 (error (c) 
                   (declare (ignore c))
                   (store-error "Unable to determine function name for ~A."
                                obj))))))
          (t (store-error "Unable to determine function name for ~A."
                          obj)))))
  

(defstore-cl-store (obj function stream)
  (output-type-code +function-code+ stream)
  (store-object (get-function-name obj) stream))



(defrestore-cl-store (function stream)
  (fdefinition (restore-object stream)))

;; Generic function, just dumps the gf-name
(defstore-cl-store (obj generic-function stream)
  (output-type-code +gf-code+ stream)
  (aif (generic-function-name obj)
       (store-object it stream)
       (store-error "No generic function name for ~A." obj)))

(defrestore-cl-store (generic-function stream)
  (fdefinition (restore-object stream)))


(setf *default-backend* (find-backend 'cl-store))

;; EOF

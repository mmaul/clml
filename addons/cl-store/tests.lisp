;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.
(defpackage :cl-store-tests
  (:use :cl :regression-test :cl-store))

(in-package :cl-store-tests)

(rem-all-tests)
(defvar *test-file* "filetest.cls")

(defun restores (val)
  (store val *test-file*)
  (let ((restored (restore *test-file*)))
    (or (and (numberp val) (= val restored))
        (and (stringp val) (string= val restored))
        (and (characterp val) (char= val restored))
        (eql val restored)
        (equal val restored)
        (equalp val restored))))

(defmacro deftestit (name val)
  `(deftest ,name (restores ,val) t))

;; integers
(deftestit integer.1 1)
(deftestit integer.2 0)
(deftestit integer.3 23423333333333333333333333423102334)
(deftestit integer.4 -2322993)
(deftestit integer.5 most-positive-fixnum)
(deftestit integer.6 most-negative-fixnum)
(deftestit integer.7 #x100000000) 

;; ratios
(deftestit ratio.1 1/2)
(deftestit ratio.2 234232/23434)
(deftestit ratio.3 -12/2)
(deftestit ratio.4 -6/11)
(deftestit ratio.5 23222/13)

;; complex numbers
(deftestit complex.1 #C(0 1))
(deftestit complex.2 #C(0.0 1.0))
(deftestit complex.3 #C(32 -23455))
(deftestit complex.4 #C(-222.32 2322.21))
(deftestit complex.5 #C(-111 -1123))
(deftestit complex.6 #C(-11.2 -34.5))


;; short floats

;; single-float
(deftestit single-float.1 3244.32)
(deftestit single-float.2 0.12)
(deftestit single-float.3 -233.001)
(deftestit single-float.4 most-positive-single-float)
(deftestit single-float.5 most-negative-single-float)

;; double-float
(deftestit double-float.1 2343.3d0)
(deftestit double-float.2 -1211111.3343d0)
(deftestit double-float.3 99999999999123456789012345678222222222222290.0987654321d0)
(deftestit double-float.4 -99999999999123456789012345678222222222222290.0987654321d0)
(deftestit double-float.5 most-positive-double-float)
(deftestit double-float.6 most-negative-double-float)

;; long floats

;; infinite floats
#+(or sbcl cmu lispworks allegro)
(progn 
  ;; #+sbcl (sb-int:set-floating-point-modes :traps nil)
  ;; #+cmu (ext:set-floating-point-modes :traps nil)
  (deftestit infinite-float.1 (expt most-positive-single-float 3))
  (deftestit infinite-float.2 (expt most-positive-double-float 3))
  (deftestit infinite-float.3 (expt most-negative-single-float 3))
  (deftestit infinite-float.4 (expt most-negative-double-float 3))
  (deftestit infinite-float.5 (/ (expt most-positive-single-float 3)
                                 (expt most-positive-single-float 3)))
  (deftestit infinite-float.6 (/ (expt most-positive-double-float 3)
                                 (expt most-positive-double-float 3))))


;; characters
(deftestit char.1 #\Space)
(deftestit char.2 #\f )
(deftestit char.3 #\Rubout)
(deftestit char.4 (code-char 255)) 


;; various strings
(deftestit string.1 "foobar")
(deftestit string.2 "how are you")
(deftestit string.3 "foo
bar")

(deftestit string.4
  (make-array 10 :initial-element #\f :element-type 'character
              :fill-pointer 3))

#+(or (and sbcl sb-unicode) lispworks clisp acl)
(progn
  (deftestit unicode.1 (map #-lispworks 'string
                            #+lispworks 'lw:text-string
                            #'code-char (list #X20AC #X3BB)))
  (deftestit unicode.2 (intern (map #-lispworks 'string
                                    #+lispworks 'lw:text-string
                                    #'code-char (list #X20AC #X3BB))
                               :cl-store-tests)))

;; vectors
(deftestit vector.1 #(1 2 3 4))


(deftestit vector.2 (make-array 5 :element-type 'fixnum 
                                :initial-contents (list 1 2 3 4 5)))

(deftestit vector.3
  (make-array 5
              :element-type 'fixnum
              :fill-pointer 2
              :initial-contents (list 1 2 3 4 5)))


(deftestit vector.4 #*101101101110)
(deftestit vector.5 #*)
(deftestit vector.6 #())


;; (array octect (*))

(deftestit vector.octet.1 (make-array 10 :element-type '(unsigned-byte 8)))
           

;; arrays
(deftestit array.1
  (make-array '(2 2) :initial-contents '((1 2) (3 4))))

(deftestit array.2
  (make-array '(2 2) :initial-contents '((1 1) (1 1))))

(deftestit array.3
  (make-array '(2 2) :element-type '(mod 10) :initial-element 3))

(deftestit array.4
  (make-array  '(2 3 5) 
              :initial-contents
              '(((1 2 #\f 5 12.0) (#\Space "fpp" 4 1 0) ('d "foo" #() 3 -1))
                ((0 #\a #\b 4 #\q) (12.0d0 0 '(d) 4 1) 
                 (#\Newline 1 7 #\4 #\0)))))

(deftestit array.5
  (let* ((a1 (make-array 5))
         (a2 (make-array 4 :displaced-to a1
                         :displaced-index-offset 1))
         (a3 (make-array 2 :displaced-to a2
                         :displaced-index-offset 2)))
    a3))
    
         


;; symbols

(deftestit symbol.1  t)
(deftestit symbol.2  nil)
(deftestit symbol.3  :foo)
(deftestit symbol.4  'cl-store-tests::foo)
(deftestit symbol.5  'make-hash-table)
(deftestit symbol.6 '|foo bar|)
(deftestit symbol.7 'foo\ bar\ baz)

(deftest gensym.1 (progn
                    (store (gensym "Foobar") *test-file*)
                    (let ((new (restore *test-file*)))
                      (list (symbol-package new)
                            (mismatch "Foobar" (symbol-name new)))))
         (nil 6))

; This failed in cl-store < 0.5.5
(deftest gensym.2 (let ((x (gensym)))
                    (store (list x x) *test-file*)
                    (let ((new (restore *test-file*)))
                      (eql (car new) (cadr new))))
         t)


;; cons

(deftestit cons.1 '(1 2 3))
(deftestit cons.2 '((1 2 3)))
(deftestit cons.3 '(#\Space 1 1/2 1.3 #(1 2 3)))
  
(deftestit cons.4  '(1 . 2))
(deftestit cons.5  '(t . nil))
(deftestit cons.6 '(1 2 3 . 5))
(deftest cons.7 (let ((list (cons nil nil)))
                  (setf (car list) list)
                  (store list *test-file*)
                  (let ((ret (restore *test-file*)))
                    (eq ret (car ret))))
         t)


;; hash tables
; for some reason (make-hash-table) is not equalp 
; to (make-hash-table) with ecl.

#-ecl
(deftestit hash.1 (make-hash-table))

#-ecl
(defvar *hash* (let ((in (make-hash-table :test #'equal 
                                          :rehash-threshold 0.4 :size 20
                                          :rehash-size 40)))
                 (dotimes (x 1000) (setf (gethash (format nil "~R" x) in) x))
                 in))
#-ecl
(deftestit hash.2 *hash*)


;; packages
(deftestit package.1 (find-package :cl-store))

(defpackage foo 
  (:nicknames foobar)
  (:use :cl)
  (:shadow cl:format)
  (:export bar))

(defun package-restores ()
  (let (( *nuke-existing-packages* t))
    (store (find-package :foo) *test-file*)
    (delete-package :foo)
    (restore *test-file*)
    (list (package-name (find-package :foo))
          (mapcar #'package-name (package-use-list :foo))
          (package-nicknames :foo)
          (equalp (remove-duplicates (package-shadowing-symbols :foo))
                  (list (find-symbol "FORMAT" "FOO")))
          (equalp (cl-store::external-symbols (find-package :foo))
                  (make-array 1 :initial-element (find-symbol "BAR" "FOO"))))))


; unfortunately it's difficult to portably test the internal symbols
; in a package so we just assume that it's OK.
(deftest package.2 
         (package-restores)
         ("FOO" ("COMMON-LISP") ("FOOBAR") t t))

;; objects 
(defclass foo ()
  ((x :accessor get-x :initarg :x)))

(defclass bar (foo)
  ((y :accessor get-y :initform nil :initarg :y)))

(defclass quux ()
  (a))

(defclass baz (quux)
  ((z :accessor get-z :initarg :z :allocation :class)))



(deftest standard-object.1
  (let ((val (store (make-instance 'foo :x 3) *test-file*)))
    (= (get-x val) (get-x (restore *test-file*))))
  t)

(deftest standard-object.2
  (let ((val (store (make-instance 'bar
                                   :x (list 1 "foo" 1.0)
                                   :y (vector 1 2 3 4))
                    *test-file*)))
    (let ((ret (restore *test-file*)))
      (and (equalp (get-x val) (get-x ret))
           (equalp (get-y val) (get-y ret)))))
  t)

(deftest standard-object.3 
  (let ((*store-class-slots* nil)
        (val (make-instance 'baz :z 9)))
    (store val *test-file*)
    (make-instance 'baz :z 2)
    (= (get-z (restore *test-file*))
       2))
  t)

(deftest standard-object.4
  (let ((*store-class-slots* t)
        (val (make-instance 'baz :z 9)))
    (store val *test-file*)
    (make-instance 'baz :z 2)
    (let ((ret (restore *test-file*)))
      (= (get-z ret )
         9)))
  t)

;; classes
(deftest standard-class.1 (progn (store (find-class 'foo) *test-file*) 
                                 (restore *test-file*)
                                 t)
  t)

(deftest standard-class.2 (progn (store (find-class 'bar) *test-file*)
                                 (restore *test-file*)
                                 t)
  t)

(deftest standard-class.3 (progn (store (find-class 'baz) *test-file*)
                                 (restore *test-file*)
                                 t)
  t)



;; conditions
(deftest condition.1
  (handler-case (/ 1 0)
    (division-by-zero (c) 
      (store c *test-file*)
      (typep (restore *test-file*) 'division-by-zero)))
  t)

(deftest condition.2
  (handler-case (car (read-from-string "3"))
    ;; allegro pre 7.0 signalled a simple-error here
    ((or type-error simple-error) (c)
      (store c *test-file*)
      (typep (restore *test-file*) 
             '(or type-error simple-error))))
  t)

;; structure-object

(defstruct a
  a b c)

(defstruct (b (:include a))
  d e f)

#+(or sbcl cmu lispworks openmcl) 
(deftestit structure-object.1 (make-a :a 1 :b 2 :c 3))
#+(or sbcl cmu lispworks openmcl)
(deftestit structure-object.2 (make-b :a 1 :b 2 :c 3 :d 4 :e 5 :f 6))
#+(or sbcl cmu lispworks openmcl)
(deftestit structure-object.3 (make-b :a 1 :b (make-a :a 1 :b 3 :c 2)
                                      :c #\Space :d #(1 2 3) :e (list 1 2 3)
                                      :f (make-hash-table)))

;; setf test
(deftestit setf.1 (setf (restore *test-file*) 0))
(deftestit setf.2 (incf (restore *test-file*)))
(deftestit setf.3 (decf (restore *test-file*) 2))

(deftestit pathname.1 #P"/home/foo")
(deftestit pathname.2 (make-pathname :name "foo"))
(deftestit pathname.3 (make-pathname :name "foo" :type "bar"))
                                      

; built-in classes
(deftestit built-in.1 (find-class 'hash-table))
(deftestit built-in.2 (find-class 'integer))
                                  

;; find-backend tests
(deftest find-backend.1 
    (and (find-backend 'cl-store) t)
  t)

(deftest find-backend.2
    (find-backend (gensym))
  nil)

(deftest find-backend.3
    (handler-case (find-backend (gensym) t)
      (error (c) (and c t))
      (:no-error (val) (and val nil)))
  t)



;; circular objects
(defvar circ1 (let ((x (list 1 2 3 4)))
                (setf (cdr (last x)) x)))
(deftest circ.1 (progn (store circ1 *test-file*)
                       (let ((x (restore *test-file*)))
                         (eql (cddddr x) x)))
  t)

(defvar circ2 (let ((x (list 2 3 4 4 5)))
                (setf (second x) x)))
(deftest circ.2 (progn (store circ2 *test-file*)
                       (let ((x (restore *test-file*)))
                         (eql (second x) x)))
  t)



(defvar circ3 (let ((x (list (list 1 2 3 4 )
                             (list 5 6 7 8)
                             9)))
                (setf (second x) (car x))
                (setf (cdr (last x)) x)
                x))

(deftest circ.3 (progn (store circ3 *test-file*)
                       (let ((x (restore *test-file*)))
                         (and (eql (second x) (car x))
                              (eql (cdddr x) x))))
  t)


(defvar circ4 (let ((x (make-hash-table)))
                (setf (gethash 'first x) (make-hash-table))
                (setf (gethash 'second x) (gethash 'first x))
                (setf (gethash 'inner (gethash 'first x)) x)
                x))

(deftest circ.4 (progn (store circ4 *test-file*)
                       (let ((x (restore *test-file*)))
                         (and (eql (gethash 'first x)
                                  (gethash 'second x))
                              (eql x
                                  (gethash 'inner
                                           (gethash 'first x))))))
  t)

(deftest circ.5  (let ((circ5 (make-instance 'bar)))
                   (setf (get-y circ5) circ5)
                   (store circ5 *test-file*)
                   (let ((x (restore *test-file*)))
                     (eql x (get-y x))))
  t)


(defvar circ6 (let ((y (make-array '(2 2 2)
                                   :initial-contents '((("foo" "bar") 
                                                        ("me" "you"))
                                                       ((5 6) (7 8))))))
                (setf (aref y 1 1 1) y)
                (setf (aref y 0 0 0) (aref y 1 1 1))
                y))


(deftest circ.6 (progn (store circ6 *test-file*)
                       (let ((x (restore *test-file*)))
                         (and (eql (aref x 1 1 1) x)
                              (eql (aref x 0 0 0) (aref x 1 1 1)))))
  t)



(defvar circ7 (let ((x (make-a)))
                (setf (a-a x) x)))

#+(or sbcl cmu lispworks)
(deftest circ.7 (progn (store circ7 *test-file*)
                       (let ((x (restore *test-file*)))
                         (eql (a-a x) x)))
  t)

(defvar circ.8 (let ((x "foo"))
                 (make-pathname :name x :type x)))


;; clisp apparently creates a copy of the strings in a pathname 
;; so a test for eqness is pointless.
#-clisp
(deftest circ.8 (progn (store circ.8 *test-file*)
                       (let ((x (restore *test-file*)))
                         (eql (pathname-name x)
                              (pathname-type x))))
  t)


(deftest circ.9 (let ((val (vector "foo" "bar" "baz" 1 2)))
                  (setf (aref val 3) val)
                  (setf (aref val 4) (aref val 0))
                  (store val *test-file*)
                  (let ((rest (restore *test-file*)))
                    (and (eql rest (aref rest 3))
                         (eql (aref rest 4) (aref rest 0)))))
  t)
                        
(deftest circ.10 (let* ((a1 (make-array 5))
                        (a2 (make-array 4 :displaced-to a1
                                        :displaced-index-offset 1))
                        (a3 (make-array 2 :displaced-to a2
                                        :displaced-index-offset 2)))
                   (setf (aref a3 1) a3)
                   (store a3 *test-file*)
                   (let ((ret (restore *test-file*)))
                     (eql a3 (aref a3 1))))
  t)

(defvar circ.11 (let ((x (make-hash-table)))
                  (setf (gethash x x) x)
                  x))

(deftest circ.11 (progn (store circ.11 *test-file*)
                        (let ((val (restore *test-file*)))
                          (eql val (gethash val val))))
  t)

(deftest circ.12 (let ((x (vector 1 2 "foo" 4 5)))
                   (setf (aref x 0) x)
                   (setf (aref x 1) (aref x 2))
                   (store x *test-file*)
                   (let ((ret (restore *test-file*)))
                     (and (eql (aref ret 0) ret)
                          (eql (aref ret 1) (aref ret 2)))))
  t)


(defclass foo.1 ()
  ((a :accessor foo1-a)))

;; a test from Robert Sedgwick which crashed in earlier
;; versions (pre 0.2)
(deftest circ.13 (let ((foo (make-instance 'foo.1))
                       (bar (make-instance 'foo.1)))
                   (setf (foo1-a foo) bar)
                   (setf (foo1-a bar) foo)
                   (store (list foo) *test-file*)
                   (let ((ret (car (restore *test-file*))))
                     (and (eql ret (foo1-a (foo1-a ret)))
                          (eql (foo1-a ret)
                              (foo1-a (foo1-a (foo1-a ret)))))))
  t)

#-abcl
(deftest circ.14 (let ((list '#1=(1 2 3 #1# . #1#)))
                   (store list *test-file*)
                   (let ((ret (restore *test-file*)))
                     (and (eq ret (cddddr ret))
                          (eq (fourth ret) ret))))
         t)
                   



#-abcl
(deftest circ.15 (let ((list '#1=(1 2 3 #2=(#2#) . #1#)))
                   (store list *test-file*)
                   (let ((ret (restore *test-file*)))
                     (and (eq ret (cddddr ret))
                          (eq (fourth ret)
                              (car (fourth ret))))))
         t)



;; this had me confused for a while since what was
;; restored #1=(1 (#1#) #1#) looks nothing like this list,
;; but it turns out that it is correct
#-abcl
(deftest circ.16  (let ((list '#1=(1 #2=(#1#) . #2#)))
                    (store list *test-file*)
                    (let ((ret (restore *test-file*)))
                      (and (eq ret (caadr ret))
                           (eq ret (third ret)))))
         t)

;; large circular lists
(deftest large.1 (let ((list (make-list 100000)))
                   (setf (cdr (last list)) list)
                   (store list *test-file*)
                   (let ((ret (restore *test-file*)))
                     (eq (nthcdr 100000 ret) ret)))
         t)

;; large dotted lists
(deftestit large.2 (let ((list (make-list 100000)))
                     (setf (cdr (last list)) 'foo)
                     list))



;; custom storing
(defclass random-obj () ((size :accessor size :initarg :size)))

(defparameter *random-obj-code* (register-code 100 'random-obj))

(defstore-cl-store (obj random-obj buff)
  (output-type-code *random-obj-code* buff)
  (store-object (size obj) buff))

(defrestore-cl-store (random-obj buff)
  (random (restore-object buff)))

  
(deftest custom.1
  (progn (store (make-instance 'random-obj :size 5) *test-file* )
         (typep (restore *test-file*) '(integer 0 4)))
  t)



(deftestit function.1 #'restores)
(deftestit function.2 #'car)

(deftestit gfunction.1 #'cl-store:restore)
(deftestit gfunction.2 #'cl-store:store)
#-clisp
(deftestit gfunction.3 #'(setf get-y))


(deftest nocirc.1 
    (let* ((string "FOO")
           (list `(,string . ,string))
           (*check-for-circs* nil))
      (store list *test-file*)
      (let ((res (restore *test-file*)))
        (and (not (eql (car res) (cdr res)))
             (string= (car res) (cdr res)))))
  t)


(defstruct st.bar x)
(defstruct (st.foo (:conc-name f-)
                   (:constructor fooo (z y x))
                   (:copier cp-foo)
                   (:include st.bar)
                   (:predicate is-foo)
                   (:print-function (lambda (obj st dep)
                                      (declare (ignore dep))
                                      (print-unreadable-object (obj st :type t) 
                                        (format st "~A" (f-x obj))))))
  (y 0 :type integer) (z nil :type simple-string))


#+(or sbcl cmu)
(deftest struct-class.1
    (let* ((obj (fooo "Z" 2 3))
           (string (format nil "~A" obj)))
      (let ((*nuke-existing-classes* t))
        (store (find-class 'st.foo) *test-file*)
        (fmakunbound 'cp-foo)
        (fmakunbound 'is-foo)
        (fmakunbound 'fooo)
        (fmakunbound 'f-x)
        (fmakunbound 'f-y)
        (fmakunbound 'f-z)
        (restore *test-file*)
        (let* ((new-obj (cp-foo (fooo "Z" 2 3)))
               (new-string (format nil "~A" new-obj)))
          (list (is-foo new-obj) (equalp obj new-obj)
                (string= new-string string)
                (f-x new-obj) (f-y new-obj) (f-z new-obj)))))
  (t t t 3 2 "Z"))

(deftest serialization-unit.1
         (with-serialization-unit ()
           (with-open-file (outs *test-file* :element-type '(unsigned-byte 8)
                                 :if-exists :supersede :direction :output)
             (dotimes (x 100)
               (cl-store:store x outs)))
           (with-open-file (outs *test-file* :element-type '(unsigned-byte 8)
                                 :if-exists :supersede)
             (loop :repeat 100 :collect (cl-store:restore outs))))
         #.(loop :for x :below  100 :collect  x))

(defun run-tests (backend)
  (with-backend backend
    (regression-test:do-tests))
  (when (probe-file *test-file*)
    (ignore-errors (delete-file *test-file*))))

(run-tests 'cl-store:cl-store)

;; EOF


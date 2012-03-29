(in-package #:metabang-bind-test)

(defclass metabang-bind-class-1 ()
  ((a :initarg :a :accessor a)
   (b :initarg :b :accessor b) 
   (c :initarg :c :accessor c)))

(defclass metabang-bind-class-2 (metabang-bind-class-1)
  ((d :initarg :d :accessor the-d)
   (e :initarg :e :accessor e)))

(deftestsuite test-classes (metabang-bind-test)
  ())

(addtest (test-classes)
  basic-slots
  (ensure-same
   (bind (((:slots-read-only a c)
	   (make-instance 'metabang-bind-class-1 :a 1 :b 2 :c 3)))
     (list a c))
   '(1 3) :test 'equal))

(addtest (test-classes)
  slots-new-variable-names
  (ensure-same
   (bind (((:slots-read-only a (my-c c) (the-b b))
	   (make-instance 'metabang-bind-class-1 :a 1 :b 2 :c 3)))
     (list a the-b my-c))
   '(1 2 3) :test 'equal))

(addtest (test-classes)
  writable-slots
  (ensure-same
   (bind ((instance (make-instance 'metabang-bind-class-1 :a 1 :b 2 :c 3))
	  ((:slots a (my-c c) (the-b b)) instance))
     (setf a :changed)
     (list (slot-value instance 'a) the-b my-c))
   '(:changed 2 3) :test 'equal))

(addtest (test-classes)
  basic-accessors-r/o-1
  (ensure-same
   (bind (((:accessors-read-only a c e)
	   (make-instance 'metabang-bind-class-2 :a 1 :b 2 :c 3 :d 4 :e 5)))
     (list e c a))
   '(5 3 1) :test 'equal))

(addtest (test-classes)
  basic-accessors-r/o-2
  (bind ((obj (make-instance 'metabang-bind-class-2 :a 1 :b 2 :c 3 :d 4 :e 5))
	 ((:accessors-read-only a c e) obj))
    (setf a :a c :c)
    (ensure-same (list a c e) '(:a :c 5) :test 'equal)
    (ensure-same
     (list (e obj) (c obj) (a obj))
    '(5 3 1) :test 'equal)))

(addtest (test-classes)
  accessors-new-variable-names-r/o
  (ensure-same
   (bind (((:accessors-r/o (my-a a) (my-c c) (d the-d))
	   (make-instance 'metabang-bind-class-2 :a 1 :b 2 :c 3 :d 4 :e 5)))
     (list d my-c my-a))
   '(4 3 1) :test 'equal))

(addtest (test-classes)
  basic-accessors-1
  (ensure-same
   (bind ((obj (make-instance 'metabang-bind-class-2 :a 1 :b 2 :c 3 :d 4 :e 5))
	  ((:accessors a c e) obj))
     (setf a :a c :c)
     (list (e obj) (c obj) (a obj)))
   '(5 :c :a) :test 'equal))

(addtest (test-classes)
  accessors-new-variable-names
  (ensure-same
   (bind ((obj (make-instance 'metabang-bind-class-2 :a 1 :b 2 :c 3 :d 4 :e 5))
	  ((:writable-accessors (my-a a) (my-c c) (d the-d))
	   obj))
     (setf my-a 42)
     (list d my-c my-a (a obj)))
   '(4 3 42 42) :test 'equal))

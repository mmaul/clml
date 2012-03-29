(in-package #:metabang-bind-test)

(defstruct (metabang-bind-test-1)
  a
  b 
  c)

(defstruct (metabang-bind-test-2 (:conc-name bind-test-))
  d
  e)

(deftestsuite test-structures (metabang-bind-test)
  ())

(addtest (test-structures)
  basic-access
  (ensure-same
   (bind (((:struct metabang-bind-test-1- a c)
	   (make-metabang-bind-test-1 :a 1 :b 2 :c 3)))
     (list a c))
   '(1 3) :test 'equal))

(addtest (test-structures)
  no-capture
  (let ((values 4))   
    (bind (((:struct metabang-bind-test-1- a c)
	    (make-metabang-bind-test-1 :a 1 :b 2 :c 3)))
      (ensure-same '(4 1 3) (list values a c) :test 'equal))))

(addtest (test-structures)
  changed-variable-name
  (ensure-same
   (bind (((:struct metabang-bind-test-1- (my-a a) c)
	   (make-metabang-bind-test-1 :a 1 :b 2 :c 3)))
     (list c my-a))
   '(3 1) :test 'equal))

(addtest (test-structures)
  changed-variable-name-2
  (ensure-same
   (bind (((:structure metabang-bind-test-1- (my-a a) c)
	   (make-metabang-bind-test-1 :a 1 :b 2 :c 3)))
     (list c my-a))
   '(3 1) :test 'equal))

(addtest (test-structures)
  nested-read-only
  (let ((c1 (make-metabang-bind-test-1 :a 1 :b 2 :c 3))
	(c2 (make-metabang-bind-test-1 :a 4 :b 5 :c 6)))
    (ensure-same
     (bind (((:structure metabang-bind-test-1- (my-a a) c) c1)
	    ((:structure metabang-bind-test-1- a b (second-c c)) c2))
       (list my-a c a b second-c))
     '(1 3 4 5 6) :test 'equal)))

(addtest (test-structures)
  read-write-nested
  (let ((c1 (make-metabang-bind-test-1 :a 1 :b 2 :c 3))
	(c2 (make-metabang-bind-test-1 :a 4 :b 5 :c 6)))
    (bind (((:structure/rw metabang-bind-test-1- (my-a a) c) c1)
	   ((:structure/rw metabang-bind-test-1- a b (second-c c)) c2))
      (setf my-a :a second-c :c b :b))
    (ensure-same (metabang-bind-test-1-a c1) :a)
    (ensure-same (metabang-bind-test-1-b c2) :b)
    (ensure-same (metabang-bind-test-1-c c2) :c)))

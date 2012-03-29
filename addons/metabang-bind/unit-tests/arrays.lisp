(in-package #:metabang-bind-test)

(deftestsuite test-arrays (metabang-bind-test)
  ())

(addtest (test-arrays)
  basic-access
  (ensure-same
   (bind ((#(a b c) #(1 2 3)))
     (list a b c))
   '(1 2 3) :test 'equal))

(addtest (test-arrays)
  two-dimensional
  (ensure-same
   (bind ((#2a((a b c) (d e f)) #2a((1 2 3) (4 5 6))))
     (list a b c d e f))
   '(1 2 3 4 5 6) :test 'equal))

(addtest (test-arrays)
  basic-access-nils
  (ensure-same
   (bind ((#(a nil c) #(1 2 3)))
     (list a c))
   '(1 3) :test 'equal))

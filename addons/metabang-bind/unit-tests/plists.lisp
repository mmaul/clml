(in-package #:metabang-bind-test)

(deftestsuite test-plists (metabang-bind-test)
  ())

(addtest (test-plists)
  basic-access
  (ensure-same
   (bind (((:plist a (b _) (c _ 2) (dd d)) '(:b #\b :a #\a :d #\d)))
     (list a b c dd))
   '(#\a #\b 2 #\d) :test 'equalp))

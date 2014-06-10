;-*- coding: utf-8 -*-
(in-package "TEST")

(define-test test-sample-optics
    (assert-true 
     (optics (asdf:system-relative-pathname 'clml "sample/syobu.csv") 10 2 10 '("がく長" "がく幅" "花びら長" "花びら幅")
             :file-type :csv :csv-type-spec '(string integer integer integer integer) 
             :distance :manhattan :external-format #+allegro :932 #+sbcl :sjis #+ccl :Windows-31j)))

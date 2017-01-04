;-*- coding: utf-8 -*-
(in-package :clml.test)

(define-test test-sample-optics
    (assert-true
     (optics (clml.utility.data:fetch "https://mmaul.github.io/clml.data/sample/syobu.csv") 10 2 10 '("がく長" "がく幅" "花びら長" "花びら幅")
             :file-type :csv :csv-type-spec '(string integer integer integer integer)
             :distance :manhattan :external-format #+allegro :932 #+sbcl :sjis #+ccl :Windows-31j)))

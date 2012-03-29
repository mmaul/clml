(defsystem :alexandria
  :version "0.0.0"
  :licence "Public Domain / 0-clause MIT"
  :components
  ((:static-file "LICENCE")
   (:static-file "tests.lisp")
   (:file "package")
   (:file "definitions" :depends-on ("package"))
   (:file "binding" :depends-on ("package"))
   (:file "strings" :depends-on ("package"))
   (:file "conditions" :depends-on ("package"))
   (:file "hash-tables" :depends-on ("package"))
   (:file "io" :depends-on ("package" "macros" "lists"))
   (:file "macros" :depends-on ("package" "strings" "symbols"))
   (:file "control-flow" :depends-on ("package" "definitions" "macros"))
   (:file "symbols" :depends-on ("package"))
   (:file "functions" :depends-on ("package" "symbols" "macros"))
   (:file "lists" :depends-on ("package" "functions"))
   (:file "types" :depends-on ("package" "symbols" "lists"))
   (:file "arrays" :depends-on ("package" "types"))
   (:file "sequences" :depends-on ("package" "lists" "types"))
   (:file "numbers" :depends-on ("package" "sequences"))
   (:file "features" :depends-on ("package" "control-flow"))))

(defmethod operation-done-p ((o test-op) (c (eql (find-system :alexandria))))
  nil)

(defmethod perform ((o test-op) (c (eql (find-system :alexandria))))
  (operate 'load-op :alexandria-tests)
  (operate 'test-op :alexandria-tests))
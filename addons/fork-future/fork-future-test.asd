(asdf:defsystem fork-future-test
  :depends-on (fork-future stefil)
  :components 
  ((:module test
            :components
            ((:file "package")
             (:file "assertions" :depends-on ("package"))
             (:file "unit-test" :depends-on ("assertions" ))
             (:file "stress-test" :depends-on ("assertions"))))))


(defsystem :clml.statistics.rand
  :author "Yotsutsuji Tetsuaki"
  :licence "?"
  :description "Random Number Generator Library"
  :serial t
  :components 
  ((:module
    
    :clml.statistics.rand
            :pathname #p "src/rand"
            :components 
            ((:file "rand/package" )
             (:file "rand/utilities" :depends-on ("rand/package"))
             (:file "rand/rand" :depends-on ("rand/utilities"))
             )
            )
   ;(:static-file "src/rand/README.md")
   
   ))

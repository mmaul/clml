(asdf:defsystem lapack-test
  :description "Testing programs for LAPACK binding."
  :author "MSI"
  :depends-on (:alexandria :iterate :stefil :lapack)
  :components
  ((:module test
	    :components ((:file "packages")
                         (:file "common") 
                         (:file "lapack-test"))
	    :serial t
	    )))

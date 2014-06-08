(asdf:defsystem lapack-ffi-test
  :description "Testing programs for LAPACK binding."
  :author "MSI"
  :depends-on (:alexandria :iterate :stefil :lapack-ffi)
  :components
  ((:module test
	    :components ((:file "packages")
                         (:file "common") 
                         (:file "lapack-test"))
	    :serial t
	    )))

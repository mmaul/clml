;;;; TODO: add copyright and license

(asdf:defsystem blas-ffi-test
  :description "Testing programs for BLAS binding."
  :author "MSI"
  :depends-on (:alexandria :iterate :stefil :blas-ffi)
  :components
  ((:module test
	    :components ((:file "packages")
                         (:file "common") 
                         (:file "blas-test"))
	    :serial t
	    )))


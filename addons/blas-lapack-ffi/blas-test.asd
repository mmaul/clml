;;;; TODO: add copyright and license

(asdf:defsystem blas-test
  :description "Testing programs for BLAS binding."
  :author "MSI"
  :depends-on (:alexandria :iterate :stefil :blas)
  :components
  ((:module test
	    :components ((:file "packages")
                         (:file "common") 
                         (:file "blas-test"))
	    :serial t
	    )))


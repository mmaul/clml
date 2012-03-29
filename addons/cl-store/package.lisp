;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.

;(in-package :cl-store.system)

(defpackage #:cl-store
  (:use #:cl) 
  (:export #:backend #:magic-number #:stream-type
           #:restorers #:resolving-backend #:find-backend #:defbackend
           #:*restore-counter* #:*need-to-fix* #:*restored-values*
           #:with-backend #:*default-backend*
           #:*current-backend* #:*store-class-slots*
           #:*nuke-existing-classes* #:*store-class-superclasses*
           #:cl-store-error #:store-error #:restore-error #:store
           #:restore #:backend-store #:store-backend-code #:store-object
           #:backend-store-object 
           #:restore #:backend-restore #:cl-store #:referrerp
           #:check-magic-number #:get-next-reader #:int-or-char-p
           #:restore-object #:backend-restore-object #:serializable-slots
           #:defstore-cl-store #:defrestore-cl-store #:register-code
           #:output-type-code #:store-referrer #:resolving-object
           #:internal-store-object #:setting #:simple-standard-string
           #:float-type #:get-float-type #:make-referrer #:setting-hash
           #:multiple-value-store #:caused-by
           #:store-32-bit #:read-32-bit #:*check-for-circs*
           #:*store-hash-size* #:*restore-hash-size* #:get-slot-details
           #:*store-used-packages* #:*nuke-existing-packages*
           #:serializable-slots-using-class

           ;; Hooks into lower level circularity tracking
           ;; to reduce consing.
           #:with-serialization-unit #:create-serialize-hash

           #:alias-backend)
  
  #+sbcl (:import-from #:sb-mop
                       #:generic-function-name
                       #:slot-definition-name
                       #:slot-definition-allocation
                       #:slot-definition
                       #:compute-slots
                       #:slot-definition-initform
                       #:slot-definition-initargs
                       #:slot-definition-name
                       #:slot-definition-readers
                       #:slot-definition-type
                       #:slot-definition-writers
                       #:class-direct-default-initargs
                       #:class-direct-slots
                       #:class-direct-superclasses
                       #:class-slots
                       #:ensure-class)

  #+ecl (:import-from #:clos
                      #:generic-function-name
                      #:compute-slots
                      #:class-direct-default-initargs
                      #:class-direct-slots
                      #:class-direct-superclasses
                      #:class-slots
                      #:ensure-class)
  
  #+cmu  (:import-from #:pcl
                       #:generic-function-name
                       #:slot-definition-name
                       #:slot-definition-allocation
                       #:compute-slots
                       #:slot-definition
                       #:slot-definition-initform
                       #:slot-definition-initargs
                       #:slot-definition-name
                       #:slot-definition-readers
                       #:slot-definition-type
                       #:slot-definition-writers
                       #:class-direct-default-initargs
                       #:class-direct-slots
                       #:class-direct-superclasses
                       #:class-slots
                       #:ensure-class)
  
  #+cmu (:shadowing-import-from #:pcl
                                #:class-name
                                #:find-class
                                #:standard-class
                                #:class-of)
  
  #+openmcl (:import-from #:openmcl-mop
                          #:generic-function-name
                          #:slot-definition-name
                          #:slot-definition-allocation
                          #:compute-slots
                          #:slot-definition
                          #:slot-definition-initform
                          #:slot-definition-initargs
                          #:slot-definition-name
                          #:slot-definition-readers
                          #:slot-definition-type
                          #:slot-definition-writers
                          #:class-direct-default-initargs
                          #:class-direct-slots
                          #:class-direct-superclasses
                          #:class-slots
                          #:ensure-class)

  #+digitool (:import-from #:ccl
                           #:generic-function-name
                           #:slot-definition-name
                           #:slot-definition-allocation
                           #:compute-slots
                           #:slot-definition
                           #:slot-definition-initform
                           #:slot-definition-initargs
                           #:slot-definition-name
                           #:slot-definition-readers
                           #:slot-definition-type
                           #:slot-definition-writers
                           #:class-direct-default-initargs
                           #:class-direct-slots
                           #:class-direct-superclasses
                           #:class-slots
                           #:ensure-class)
  
  #+(and clisp (not mop)) (:import-from #:clos
                        #:slot-value
                        #:std-compute-slots
                        #:slot-boundp
                        #:class-name
                        #:class-direct-default-initargs
                        #:class-direct-slots
                        #:class-slots
                        #:ensure-class)
  
  #+lispworks (:import-from #:clos
               #:slot-definition-name
               #:generic-function-name
               #:slot-definition-allocation
               #:compute-slots
               #:slot-definition
               #:slot-definition-initform
               #:slot-definition-initargs
               #:slot-definition-name
               #:slot-definition-readers
               #:slot-definition-type
               #:slot-definition-writers
               #:class-direct-default-initargs
               #:class-direct-slots
               #:class-slots
               #:class-direct-superclasses
               #:ensure-class)

  #+(and clisp mop) (:import-from #:clos
                     #:slot-definition-name
                     #:generic-function-name
                     #:slot-definition-allocation
                     #:compute-slots
                     #:slot-definition
                     #:slot-definition-initform
                     #:slot-definition-initargs
                     #:slot-definition-name
                     #:slot-definition-readers
                     #:slot-definition-type
                     #:slot-definition-writers
                     #:class-direct-default-initargs
                     #:class-direct-slots
                     #:class-slots
                     #:class-direct-superclasses
                     #:ensure-class)
  
  #+allegro (:import-from #:mop
                          #:slot-definition-name
                          #:generic-function-name
                          #:slot-definition-allocation
                          #:slot-definition
                          #:compute-slots
                          #:slot-definition-initform
                          #:slot-definition-initargs
                          #:slot-definition-name
                          #:slot-definition-readers
                          #:slot-definition-type
                          #:slot-definition-writers
                          #:class-direct-default-initargs
                          #:class-direct-slots
                          #:class-direct-superclasses
                          #:class-slots
                          #:ensure-class)
  )
;; EOF

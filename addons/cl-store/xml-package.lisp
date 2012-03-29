;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.

(defpackage #:cl-store-xml
  (:use #:cl #:cl-store)
  (:export #:*xml-backend*
           #:add-xml-mapping #:defstore-xml #:defrestore-xml #:princ-and-store
           #:princ-xml #:restore-first #:with-tag #:first-child 
           #:second-child #:get-child)
  (:import-from #:cl-store #:when-let #:generic-function-name #:get-function-name
                #:force #:setting #:resolving-object)
  
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
  
  #+clisp (:import-from #:clos
                        #:slot-value
                        #:std-compute-slots
                        #:slot-boundp
                        #:class-name
                        #:class-direct-default-initargs
                        #:class-direct-slots
                        #:class-slots
                        #:ensure-class)
  
  #+lispworks  (:import-from #:clos
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
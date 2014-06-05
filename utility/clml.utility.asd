(asdf:defsystem :clml.utility-package
                :pathname "src/"
                :serial t
                :components (
                             (:file "package"))
                )


(asdf:defsystem :clml.utility
                :pathname "src/"
                :serial t
                :depends-on ( :parse-number
                              :cl-ppcre
                              :iterate
                              :clml.utility-package
                             )
                :components (
                             (:file "csv")
                             (:file "priority-que")
                             )
                )

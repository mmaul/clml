(asdf:defsystem :clml.text.package
                :pathname "src/"
                :serial t
                :components (
                             (:file "package")))
(asdf:defsystem :clml.text
                :pathname "src/"
                :serial t
                :depends-on (:hjs
                             :split-sequence
                             :clml.text.package
                             )
                :components (
                             (:file "text-utils")
                             ))

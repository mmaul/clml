(asdf:defsystem :clml.numeric-package
                :pathname "src/"
                :serial t
                :components (
                             (:file "package")))


(asdf:defsystem :clml.numeric
                :pathname "src/"
                :serial t
                :depends-on (:clml.hjs
                             :clml.numeric-package
                             )
                :components (
                             (:file "fft")
                             ))

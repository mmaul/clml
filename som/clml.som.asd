(asdf:defsystem :clml.som.package
                :pathname "src/"
                :serial t
                :components (
                             (:file "package")))
(asdf:defsystem :clml.som
                :pathname "src/"
                :serial t
                :depends-on (:clml.hjs
                             :split-sequence
                             :clml.som.package
                             :clml.statistics
                             )
                :components (
                             (:file "param")
                             (:file "som_utils")
                             (:file "lvq_pak")
                             (:file "labels")
                             (:file "fileio")
                             (:file "datafile")
                             (:file "randinit")
                             (:file "som_rout")
                             (:file "som_pak")
                             (:file "vsom")
                             (:file "vcal")
                             (:file "sammon")
                             (:file "visual")
                             ))

(asdf:defsystem :clml.som.example
                :pathname "examples/"
                :serial t
                :depends-on (:clml.hjs
                             :split-sequence
                             :clml.som
                             
                             )
                
                :components (
                             (:file "package")
                             (:file "test")
                             ))


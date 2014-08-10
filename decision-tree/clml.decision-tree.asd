(asdf:defsystem :clml.decision-tree-package
                :pathname "src/"
                :serial t
                :components (
                             (:file "package")))


(asdf:defsystem :clml.decision-tree
                :pathname "src/"
                :serial t
                :depends-on ( :clml.hjs
                              #+(and unix  (not lispworks))
                              :fork-future
                             :clml.decision-tree-package
                             )
                :components (
                             
                             (:file "decision-tree")
                             (:file "random-forest")
                             ))

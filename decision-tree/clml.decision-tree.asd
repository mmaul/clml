(asdf:defsystem :decision-tree-package
                :pathname "src/"
                :serial t
                :components (
                             (:file "package")))


(asdf:defsystem :decision-tree
                :default-component-class cl-source-file.cl
                :pathname "src/"
                :serial t
                :depends-on ( :hjs
                             :decision-tree-package
                             )
                :components (
                             
                             (:file "decision-tree")
                             (:file "random-forest")
                             ))

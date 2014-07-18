(asdf:defsystem #:clml.docs
  :serial t
  :description "Documentation generation system for CLML"
  :author "Mike Maul"
  ;:email "maul.mike@gmail.com"
  :license "LLGPL"
  :depends-on (
               :clod
               :iterate
               :cl-ppcre
               :clml
               )
  :pathname "src/"
  :components ((:file "package")
               (:file "docs")
               ))

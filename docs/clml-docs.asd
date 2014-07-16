(asdf:defsystem #:clml-docs
  :serial t
  :description "Reorginized CLML library from MSI"
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

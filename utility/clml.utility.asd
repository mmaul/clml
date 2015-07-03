(asdf:defsystem :clml.utility-package
  :pathname "src/"
  :serial t
  :depends-on (:iterate :parse-number)
  :components ((:file "package")))


(asdf:defsystem :clml.utility
  :pathname "src/"
  :serial t
  :depends-on (:alexandria
               :parse-number
               :cl-ppcre
               :iterate
               :cl-fad
               :drakma
               :trivial-garbage
               :clml.utility-package)
  :components ((:file "csv")
               (:file "priority-que")
               (:file "fetch")))

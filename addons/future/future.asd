(cl:in-package :cl-user)

(asdf:defsystem future
    :description "Fork-future is a thread based future parallel library"
    :author "Jianshi Huang @ Mathematical Systems Inc. (huang@msi.co.jp)"
    :version "0.1.20100317"
    :depends-on (alexandria)
    :components 
    ((:module src
              :components
              ((:file "package")
               (:file "thread-api")
               (:file "thread-safe-simple-queue")
               (:file "thread-pool")
               (:file "future"))
              :serial t
              :perform
              (asdf:load-op :after (op c)
                            (pushnew :future *features*)))))


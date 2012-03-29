(cl:defpackage :future
    (:use :cl)
  (:export #:future
           #:future-funcall
           #:touch
           #:wait-for-future
           #:wait-for-any-future
           #:wait-for-all-futures
           #:kill-future
           #:kill-all-futures
           #:*before-start-hooks*
           #:*after-finish-hooks* 
           #:future-max-threads
           #:initialize-environment
           #:with-new-environment
           ))


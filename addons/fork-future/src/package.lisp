(cl:defpackage :fork-future
    (:use :cl)
  (:export #:future
           #:touch
           #:wait-for-future
           #:wait-for-any-future
           #:wait-for-all-futures
           #:kill-future
           #:kill-all-futures
           #:*before-fork-hooks*
           #:*after-fork-hooks*
           #:*future-result-file-template*
           #:*fork-future-max-processes*
           #:initialize-environment
           #:with-new-environment))


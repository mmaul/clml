;;;
;;; -*- Lisp -*-
;;;


(in-package :cl-user)
;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *read-default-float-format* 'double-float)
  ;;(setf *locale* (find-locale :japan.utf8))
  (pushnew :msi-workaround *features*)
  (load "defsystem")
  ;;(excl:load-system :machine-learning :compile t)
  (excl:load-system :twitter :compile t)

  (load "defsystem-agraph-clml")
  (excl:load-system :agraph-clml :compile t)
  )

(progn
  (setf drakma:*drakma-default-external-format* :utf-8)
  (pushnew '("application" . "json") drakma:*text-content-types* :test #'equal)
  (setf twitter.crawler::*oauth-keys*
        '("mA0MqzkVv3QK9f2KuDQTFg"
          "lGw5XnQrX06lMlky12V0V7PhI6smWqS5ThT6Z6qQhs"
          "214308862-IHqVBFyzgfZszNk37JxxeJCLfJH4P3ORSQLQ760A"
          "ffCOM9FkGSyHdaIjOEQ9VKdhvGdY4BWcv4W4hdAY7Gc"))
  (setf twitter.crawler::*oauth-tokens*
        (twitter.utils:make-oauth-tokens-from-keys twitter.crawler::*oauth-keys*)))


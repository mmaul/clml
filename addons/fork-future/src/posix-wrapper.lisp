(in-package :fork-future)

(cffi:defctype pid :int)

(cffi:defcfun ("wait" %wait) pid
  (stat-loc :pointer))

(defun wait ()
  "Wait for any child process to terminate."
  (cffi:with-foreign-object (stat-loc :int)
    (let ((result (%wait stat-loc)))
      (values result (cffi:mem-ref stat-loc :int)))))

(cffi:defcfun ("waitpid" %waitpid) pid
  (pid pid)
  (stat-loc :pointer)
  (options :int))

(defun waitpid (pid &key (no-hang nil) (untraced nil)) 
  (cffi:with-foreign-object (stat-loc :int)
    (let ((result (%waitpid pid
                            stat-loc
                            (logior (if no-hang 1 0) ; 1 == wnohang
                                    (if untraced 2 0))))) ; 2 == wuntraced
      (values result (cffi:mem-ref stat-loc :int)))))

(cffi:defcfun ("kill" kill :convention :cdecl :library :default) :int 
  (pid pid)
  (sig :int))

(cffi:defcfun ("fork" fork :convention :cdecl :library :default) pid)

(cffi:defcfun ("getpid" getpid :convention :cdecl :library :default) pid)

(cffi:defcfun ("exit" exit :convention :cdecl :library :default) :int 
  (code :int))


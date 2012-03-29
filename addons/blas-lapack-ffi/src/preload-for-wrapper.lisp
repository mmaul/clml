(in-package :mkl.blas-lapack-common)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *source-path*
    #.(make-pathname :directory (pathname-directory (or *compile-file-truename* *load-truename*)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (progn
    (cffi:load-foreign-library
     (merge-pathnames "lapack_wrapper.so" *source-path*))
    (pushnew :intel-mkl *features*))
  ;; "liblapack.so"
  )


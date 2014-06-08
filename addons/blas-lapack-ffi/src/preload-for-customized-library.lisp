(in-package :mkl.blas-lapack-common)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *source-path*
    #.(make-pathname :directory (pathname-directory (or *compile-file-truename* *load-truename*)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (labels ((load-mkl ()
             (cffi:load-foreign-library
              (merge-pathnames 
               #+(and linux x86-64)
               "lib/mkl_linux_em64t/mkl_linux_em64t.so"
               #+(and linux x86)
               "lib/mkl_linux_ia32/mkl_linux_ia32.so"
               #+(and windows x86)
               "lib/mkl_win32/mkl_win32.dll"
               #+(and windows x86-64)
               "lib/mkl_win64/mkl_win64.dll"
               *default-pathname-defaults*))
             (pushnew :intel-mkl *features*))
           (push-lib-path ()
             #+nil                          ; osi not supported on windows
             (progn
               (excl.osi:setenv
                "PATH"
                (concatenate
                 'string
                 (princ-to-string (merge-pathnames "lib/mkl_win32" *default-pathname-defaults*))
                 ":"
                 (excl.osi:getenv "PATH"))
                t))))
    (declare (ignorable push-lib-path))
    (handler-case 
        (load-mkl)
      (cffi:load-foreign-library-error (c)
        (progn
          #+linux
          (cffi:load-foreign-library
           (merge-pathnames
            #+x86-64
            "lib/mkl_linux_em64t/libgomp.so"
            #+x86
            "lib/mkl_linux_ia32/libgomp.so"
            *default-pathname-defaults*))
          (load-mkl))))))


(in-package :clml-docs)

(defun find-packages (package-match-strings)
  (mapcar (lambda (v) (first v))
          (remove-if #'null
                      (iterate (for package in (list-all-packages))
                               (collect
                                   (iterate (for package-match-string in package-match-strings)
                                        ;(print (list package package-match-string))
                                            (for scanner =
                                                 (cl-ppcre:create-scanner package-match-string
                                                                          :case-insensitive-mode t))
                                            (when (cl-ppcre:scan scanner
                                                                 (package-name package))
                                              (collect package))))
                               ))))

(defun make-docs (packages)
  ""
  (with-open-file (s (asdf:system-relative-pathname 'clml "docs/api/index.org")
                     :direction :output :if-exists :overwrite :if-does-not-exist :create)
    (iterate  (for package in packages)
             (for package-name = (namestring (asdf:system-relative-pathname
                                                           'clml (concatenate 'string "docs/api/"
                                                                              (package-name package) ".org"))))
             
             
             (handler-case  (progn
                              (document-package package
                                                     package-name
                                                     :internal-symbols? nil
                                                     :title ""
                                                     :author ""
                                                     :email ""
                                                     :style-sheet "styles.css"
                                                     :include-colophon nil
                                                     :include-index nil
                                                     )
                              (format s "#+INCLUDE: \"~A\" :minlevel 1~%" package-name))
               (error (e) (format t "Error exporting ~a:~a~%" e package-name))
               )))
  )

;#+INCLUDE: "~/.emacs" src emacs-lisp

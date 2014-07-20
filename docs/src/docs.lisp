(in-package :clml.docs)

(defun find-packages (package-match-strings)
  "
-return list of packages sorted by package name
-arguments:
  -package-match-strings: list of cl-ppcre regex strings"
  (sort  
   (mapcar (lambda (v) (first v))
           (remove-if #'null
                      (iterate (for package in (list-all-packages))
                               (collect
                                   (iterate (for package-match-string in package-match-strings)
                                            (for scanner =
                                                 (cl-ppcre:create-scanner package-match-string
                                                                          :case-insensitive-mode t))
                                            (when (cl-ppcre:scan scanner
                                                                 (package-name package))
                                              (collect package))))
                               )))
   (lambda (x y) (string-lessp (package-name x) (package-name y)))))

(defun make-docs (packages)
  "
-arguments:
  -packages: list of packages"
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

(defun generate-clml-api-docs ()
  "Generates Org API documentation in the *clml/docs/api* directory from loaded packages for
CLML for packages matching the following prefix patterns:
  +^clml[.]
  +^lapack
  +^hjs
  +blas
  +^future
  +^fork-future

  Documentation is in the form of Org files where one Org file per package is placed in
  *clml/docs/api*.  A package index file containing Org INCLUDE directives that include Org
  placed generated in *clml/docs/api*. 
"
  (make-docs (find-packages '("^clml[.]" "^lapack" "^hjs[.]" "^mkl[.]"
                                                  "^blas" "^future" "^fork-future" )))
  )

;; Overlay custom modifications on top of CLOD
(in-package :clod)

(defparameter *include-colophon* t)
(defparameter *include-index* t)

(defun function-lambda-list (func)
  "* Arguments
- FUNC :: A function object, macro object, generic function object,
  or a symbol bound to a function or macro.
* Returns
Two values:
- The lambda list of the function name or function object, FUNC; or nil if
  the function takes no arguments or the lambda list cannot be retrieved.
- A boolean value -- T if a lambda list (even an empty one) was found,
  NIL otherwise.
* Description
Returns the lambda list associated with the definition of the function or
macro =FUNC=. For example, the lambda list for the common lisp function
=FIND= is the list:
: (ITEM SEQUENCE &KEY :FROM-END :TEST :TEST-NOT :START :END :KEY)
"
  
  (cond
    ((and (not (functionp func))
          (not (fboundp func)))
     (error "Not a function: ~S" func))
    ((and (functionp func)
          (typep func (find-class 'generic-function)))
     (progn 
            (values
             (generic-function-lambda-list func)
             t)))
    ((and
      (or (symbolp func)                ;fboundp takes either symbol
          (and (consp func)             ;or (setf symbol)
               (cdr func)
               (null (cddr func))
               (eq (first func) 'setf) (symbolp (second func))))
      (fboundp func)
      (typep (symbol-function func) (find-class 'generic-function)))
     (progn 
            (values
             #+sbcl
             (sb-introspect:function-arglist func)
             #-sbcl
             (generic-function-lambda-list (symbol-function func))
             t)))
    (t
     (progn )
     #+sbcl
     (let ((llist (sb-introspect:function-lambda-list func)))
       (if llist
           (values llist t)
           (values nil nil)))
     #+allegro
     (handler-case (values (excl:arglist func) t)
       (simple-error () (values nil nil)))
     #+lispworks
     (let ((arglist (lw:function-lambda-list func)))
       (etypecase arglist
         ((member :dont-know)
          (values nil nil))
         (list
          (values (replace-strings-with-symbols arglist) t))))
     #+clozure
     (multiple-value-bind (arglist binding) (let ((*break-on-signals* nil))
                                              (ccl:arglist func))
       (if binding
           (values arglist t)
           (values nil nil)))
     #+armedbear
     (cond ((symbolp func)
            (multiple-value-bind (arglist present)
                (sys::arglist func)
              (when (and (not present)
                         (fboundp func)
                         (typep (symbol-function func)
                                'standard-generic-function))
                (setq arglist
                      (mop::generic-function-lambda-list
                       (symbol-function func))
                      present
                      t))
              (if present
                  (values arglist t)
                  (values nil nil))))
           (t (values nil nil)))
     #+ecl
     (when (or (functionp func) (fboundp func))
       (multiple-value-bind (name fndef)
           (if (functionp func)
               (values (function-name func) func)
               (values func (fdefinition func)))
         (typecase fndef
           (function
            (let ((fle (function-lambda-expression fndef)))
              (case (car fle)
                (si:lambda-block
                 (values (caddr fle) t))
                (t
                 (values nil nil))))))))
     #+cmu
     (let ((llist
             (etypecase func
               (function (cmucl-function-arglist fun))
               (symbol (cmucl-function-arglist (or (macro-function func)
                                                   (symbol-function func)))))))
       (if (eql llist :not-available)
           (values nil nil)
           (values llist t)))
     #+clisp
     (block nil
       (or (ignore-errors
            (return (values (ext:arglist func) t)))
           (ignore-errors
            (let ((exp (function-lambda-expression func)))
              (and exp (return (values (second exp) t)))))
           (values nil nil))))))

(defun write-preamble ()
  "* Arguments
None.
* Return Value
Ignored.
* Description
Writes some org instructions, intended to be placed at the start of the
document. These specify the document's author, title, and set some
export options."
  (write-out "~&#+TITLE: ~A" *document-title*)
  (write-out "~&#+AUTHOR: ~A" *document-author*)
  (write-out "~&#+EMAIL: ~A" *document-email*)
  (write-out "~&#+LINK: hs ~A/%s" (if (string-starts-with?
                                       *hyperspec-root* "http:")
                                      *hyperspec-root*
                                      (format nil "file:~A" *hyperspec-root*)))
  (if *document-style-sheet*
      (write-out
       "~&#+STYLE: <link rel=\"stylesheet\" type=\"text/css\" href=~S />"
       *document-style-sheet*))
  (write-out "~&#+STARTUP: showall")
  ;; H:NNN = below this many levels deep, headings become bulleted lists
  ;; toc:NNN = go this many levels deep in table of contents
  (write-out "~&#+OPTIONS: toc:1 H:10 @:t tags:nil~%~%"))

(defun write-colophon ()
  (when *include-colophon*
    (writing-section ("Colophon")
      (write-out (str+ "This documentation was generated from "
                       "Common Lisp source code using CLOD, version ~A.")
                 *clod-version-string*)
      (write-out (str+ "The latest version of CLOD is available "
                       "[[http://bitbucket.org/eeeickythump/clod/][here]].")))))

(defun write-docstring (str &key (levels *heading-level*)
                        (default ""))
  "* Arguments
- STR :: A docstring.
- LEVELS :: How many levels deep in the outline is
  this docstring?
- DEFAULT :: A string. Default text, used if =STR= is =NIL=.
* Return Value
Ignored.
* Description
Writes the docstring STR to the document. Before doing this, processes
the docstring to:
- Demote headings
- Make all hyperlinks safe for org mode and HTML export
- Convert lines beginning with ';;;' to syntax-highlighting markup."
  (cond
    (str
     (with-input-from-string (in str)
       (iterate
         (with src = nil)
         (while (listen in))
         (for line = (read-line in))
         (for str =
              (regex-replace-all
               ;; Regex matches 'heading' lines
               (format nil "^([~A]+)([ \t]+)(.*)$"
                       (quote-meta-chars
                        (format nil "~C" *heading-char*)))
               line
               (lambda (match match1 match2 match3 &rest matches)
                 (declare (ignore match match2 matches))
                 (format nil "~A ~A~%"
                         (make-string (+ (length match1)
                                         (1- levels))
                                      :initial-element *heading-char*)
                         match3))
               :simple-calls t))
         ;; Regex matches first part of "[[...][...]]" long-form
         ;; hyperlinks. Ensures first part of hyperlink is
         ;; html-safe.
         (setf str (regex-replace-all "\\[\\[([^]]*)\\]\\["
                                      str
                                      (lambda (match match1 &rest matches)
                                        (declare (ignore match matches))
                                        (format nil "[[~A]["
                                                (html-safe-string match1)))
                                      :simple-calls t))
         ;; Regex matches all [[...]] short-form hyperlinks.
         ;; Ensures html safety.
         (setf str (regex-replace-all "\\[\\[([^]]*)\\]\\]"
                                      str
                                      (lambda (match match1 &rest matches)
                                        (declare (ignore matches))
                                        (let ((safe (html-safe-string match1)))
                                          (cond
                                            ((string= safe match1)
                                             match)
                                            (t
                                             (format nil "[[~A][~A]]"
                                                     safe match1)))))
                                      :simple-calls t))
         (cond
           ((string-starts-with? str ";;;")
            (unless src
              (setf src t)
              (format *out* "#+BEGIN_SRC lisp~%"))
            (setf str (subseq str 3)))
           (t
            (when src
              (setf src nil)
              (format *out* "#+END_SRC~%"))))
         (format *out* "~A~%" str)
         (finally
          (if src
              (format *out* "#+END_SRC~%"))))))
    (default
     (format *out* "~A~%" default))))


(defun document-package-contents (pkg &optional
                                  (accessibilities *accessibilities*))
  "* Arguments
- PKG :: A package name or package object.
- ACCESSIBILITIES :: A list containing zero or more of the symbols
  =:external= or =:internal=.
* Return Value
Ignored.
* Description
Writes documentation for all symbols contained within the package =PKG=.
Does not write documentation for the actual package object."
  (let ((access nil) (uses nil)
        (functions nil)
        (generics nil)
        (macros nil)
        (vars nil)
        (constants nil)
        (structures nil)
        (types nil)
        (classes nil))  ;; structures are actually classes too
    (unless (packagep pkg)
      (setf pkg (find-pkg pkg)))
    (setf *ambiguities* (make-hash-table :test #'eql))
    (do-own-symbols (sym pkg)
      (setf access (symbol-accessibility sym pkg))
      (when (or (null accessibilities)
                (find access accessibilities))
        (setf uses (uses-for-symbol sym))
        (if (find :class uses) (push sym classes))
        (if (find :structure uses) (push sym structures))
        (if (find :type uses) (push sym types))
        (if (find :macro uses) (push sym macros))
        (if (find :generic-function uses) (push sym generics))
        (if (find :function uses) (push sym functions))
        (if (find :constant uses) (push sym constants))
        (if (find :variable uses) (push sym vars))
        (if (> (length uses) 1)
            (setf (gethash sym *ambiguities*) (copy-list uses)))))
    ;; Remove any generic functions that represent slot accessors
    (setf generics
          (set-difference generics
                          (list-all-slot-accessors
                           (mapcar #'find-class classes))))
    ;; Ensure all classes are finalised
    (dolist (c classes)
      (ensure-finalized (find-class c) nil))
    ;; === main body of definitions ===
    (when (and classes *class-diagram*)
      (writing-section ("Class Hierarchy")
        (write-class-hierarchy (mapcar #'find-class classes))
        ))
    (dolist (access accessibilities)
      (let ((accstr (format nil "~:(~A~) " access)))
        (writing-section ((str+ accstr "Symbols"))
          (write-out "~%~%")
          (write-chapter (accessible-symbols classes access pkg)
                         :class (str+ accstr "Classes"))
          (write-chapter (accessible-symbols structures access pkg)
                         :structure (str+ accstr "Structures"))
          (write-chapter (accessible-symbols types access pkg)
                         :type (str+ accstr "Types"))
          (write-chapter (accessible-symbols constants access pkg)
                         :constant (str+ accstr "Constants"))
          (write-chapter (accessible-symbols vars access pkg)
                         :variable (str+ accstr "Global Variables"))
          (write-chapter (accessible-symbols macros access pkg)
                         :macro (str+ accstr "Macros"))
          (write-chapter (accessible-symbols functions access pkg)
                         :function (str+ accstr "Functions"))
          (write-chapter (accessible-symbols generics access pkg)
                         :generic (str+ accstr "Generic Functions")))))
    (when (plusp (hash-table-count *ambiguities*))
      (writing-section ("Ambiguous Symbols")
        (iterate
          (for (sym uses) in-hashtable *ambiguities*)
          (write-disambiguation sym uses))))
    ;; === index ===
    (when *include-index*  (write-index pkg accessibilities))))

(defun document-packages (packages file/stream
                          &key (auto-links nil)
                          (lines-between-sections t)
                          (brief-methods t)
                          (internal-symbols? t)
                          (class-diagram nil)
                          (style-sheet nil)
                          (title *document-title*)
                          (author *document-author*)
                            (email *document-email*)
                            (include-colophon t)
                            (include-index t))
  "* Arguments
- PACKAGES :: A list of package objects, or symbols naming packages.
- FILE/STREAM :: A string (filename), stream object, or =NIL=.
Other arguments are the same as for [[document-package]].
* Returns
A string, or nil.
* Description
Produces documentation for all the packages in =PACKAGES=, within a
single file.

See [[document-package]] for more details."
  (let ((*auto-links* auto-links)
        (*lines-between-sections* lines-between-sections)
        (*brief-methods* brief-methods)
        (*accessibilities* (if internal-symbols?
                               (list :external :internal)
                               (list :external)))
        (*class-diagram* class-diagram)
        (*document-style-sheet* style-sheet)
        (*document-title* title)
        (*document-author* author)
        (*document-email* email))
    (setf *include-colophon* include-colophon)
    (setf *include-index* include-index)
    (cond
      ((streamp file/stream)
       (let ((*out* file/stream))
         (apply #'docpkg packages)))
      ((stringp file/stream)
       (with-open-file (*out* file/stream :direction :output
                              :if-exists :supersede)
         (apply #'docpkg packages)))
      ((null file/stream)
       (with-output-to-string (*out*)
         (apply #'docpkg packages ))))))

(defun document-package (pkg file/stream &key
                                           (auto-links nil)
                                           (lines-between-sections t)
                                           (brief-methods t)
                                           (internal-symbols? t)
                                           (class-diagram nil)
                                           (style-sheet nil)
                                           (title nil)
                                           (author *document-author*)
                                           (email *document-email*)
                                           (include-colophon t)
                                           (include-index t))
  "* Arguments
- PKG :: A package name or package object.
- FILE/STREAM :: A string (filename), stream object, or =NIL=.
- AUTO-LINKS :: Boolean.
- LINES-BETWEEN-SECTIONS :: Boolean.
- BRIEF-METHODS :: Boolean.
- INTERNAL-SYMBOLS :: Boolean.
- CLASS-DIAGRAM :: Boolean.
- STYLE-SHEET :: A string.
- TITLE :: A string.
- AUTHOR :: A string.
- EMAIL :: A string.
* Returns
A string, or nil.
* Description
Produce documentation for the package =PKG=.

The documentation's destination depends on the value of =FILE/STREAM=:
- =STRING=: documentation is written to the file named by the string.
- =STREAM=: documentation is written to the already existing stream.
- =NIL=: documentation is written to a string, which is then returned by
  this function.

Explanation of optional arguments:
- =TITLE=, =AUTHOR= and =EMAIL= specify the document title, the name of
  the author, and the email address of the author.
- =STYLE-SHEET= specifies the name of a Cascading Style Sheet (.CSS) file
  which will be used as the style for the document if you export it
  to HTML from org mode.
- If =AUTO-LINKS= is non-nil, *all* occurrences of symbol names within the
  text of docstrings will be interpreted as hyperlinks, regardless of
  whether they are marked up as hyperlinks.
- If =LINES-BETWEEN-SECTIONS= is nil, do not output a horizontal line before
  each new section of documentation.
- If =BRIEF-METHODS= is nil, document individual methods with their own
  sections, just like functions and generic functions. Most people put
  'method' documentation in the docstrings of their generic functions, but
  if you set docstrings for individual methods then set this to nil.
- If =INTERNAL-SYMBOLS?= is non-nil, document both internal and external
  (exported) symbols. If nil, only document external symbols.
- If =CLASS-DIAGRAM= is non-nil, create a section after the toplevel package
  description, containing a description of the package hierarchy
  in the form of a GraphViz 'dot' diagram (see http://www.graphviz.org/).

* Example
;;; (clod:document-package :mypkg \"mypkg-doc.org\"
;;;      :style-sheet \"swiss.css\" :title \"My Package\"
;;;      :author \"F. B. Quux\" :email \"quux@gmail.com\")

* See also
- [[document-packages]]"
  (unless (packagep pkg)
    (setf pkg (find-pkg pkg)))
  (unless title
    (setf title (format nil "The ~A package"
                        (package-name pkg))))
  (document-packages (list pkg) file/stream
                     :auto-links auto-links
                     :lines-between-sections lines-between-sections
                     :brief-methods brief-methods
                     :internal-symbols? internal-symbols?
                     :class-diagram class-diagram
                     :style-sheet style-sheet
                     :title title :author author :email email
                     :include-colophon include-colophon
                     :include-index include-index))



 

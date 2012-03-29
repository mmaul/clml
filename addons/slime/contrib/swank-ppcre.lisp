;;; swank-ppcre.lisp --- ppcre based find / replace in asdf components
;;
;; Author: Kilian Sprotte <kilian.sprotte@gmail.com>
;; License: Public Domain
;;


;;; this file definitely needs cleaning up...

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :cl-ppcre)
  (asdf:oos 'asdf:load-op :iterate)
  (defpackage :swank-ppcre
    (:use :cl :iterate :asdf)
    (:export #:all-systems
             #:find-regex-systems-to-file
             #:replace-regex-systems)))

(in-package :swank-ppcre)

(defun ensure-list (list)
  "If LIST is a list, it is returned. Otherwise returns the list designated by LIST."
  (if (listp list)
      list
      (list list)))

(defun eat-stream (stream)
  "Eats the entire STREAM and returns it as a string,
which is in fact an array of (stream-element-type STREAM)."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((file-length (file-length stream)))
    (assert file-length nil "Cannot determine file-length of stream ~S." stream)
    (let* ((buffer (make-array file-length :element-type (stream-element-type stream)))
           (chars-read (read-sequence buffer stream)))
      (cond ((< chars-read file-length)
             (ppcre::nsubseq buffer 0 chars-read))
            (t buffer)))))

;;; FIXME encoding
(defmacro with-file-content ((pathname content &rest args) &body body)
  "Binds CONTENT to the file content of PATHNAME."
  `(with-open-file (in ,pathname :direction :input ,@args)
     (let ((,content (eat-stream in)))
       ,@body)))

(defmacro with-file-content-replace ((pathname content content-changed-p &rest args) &body body)
  "Binds CONTENT to the file content of PATHNAME and CONTENT-CHANGED-P to NIL.
If CONTENT-CHANGED-P is set to T in BODY, CONTENT will be rewritten to the file."
  (check-type content symbol)
  (check-type content-changed-p symbol)
  (assert (every #'symbolp args) nil "Sorry, maybe this assertion is too strict.
Wanted to avoid multiple evaluation here.")
  (ppcre::with-rebinding (pathname)
    `(let ((,content
            (with-open-file (in ,pathname :direction :input ,@args)
              (eat-stream in)))
           (,content-changed-p nil))
       ,@body
       (when ,content-changed-p
         (with-open-file (out ,pathname :direction :output :if-exists :supersede ,@args)
           (write-sequence ,content out)))
       ,content-changed-p)))

(defun file-replace-all (pathname regex replacement &key (preserve-case t))
  "Replaces all matches of REGEX with REPLACEMENT in the file of PATHNAME."
  (with-file-content-replace (pathname content content-changed-p)
    (let ((scanner (ppcre:create-scanner regex)))
      (when (setq content-changed-p (ppcre:scan scanner content))
        (setq content (ppcre:regex-replace-all scanner content replacement :preserve-case preserve-case))))))

(defun find-line-break (position string direction)
  "Returns a position as needed for subseq in POSITION2LINE."
  (iter
   (with limit = (ecase direction (:up (length string)) (:down -1)))
   (with dp = (case direction (:up 1) (:down -1)))
   (for p from position by dp)
   (until (or (= p limit)
              (eql (char string p) #\newline)))
   (finally (return
              (case direction
                (:down (1+ p))
                (:up p))))))

(defun position2line (position string)
  (let ((start (find-line-break position string :down))
        (end (find-line-break position string :up)))
    (ppcre::nsubseq string start end)))

(defun position2line-number (position string)
  (iter
   (for line-count upfrom 0)
   ;; "2" seems to be correct here - I have forgotten why...
   (for p initially (+ position 2) then (find-line-break (- p 2) string :down))
   (until (zerop p))
   (finally (return line-count))))

(defun find-all-grep-style (pathname regex &optional (out-stream *standard-output*) rel-pathname)
  (with-file-content (pathname content #+sbcl :external-format #+sbcl :latin-1)
    (ppcre:do-scans (start end reg-starts reg-ends regex content)
      (write-string (enough-namestring pathname rel-pathname) out-stream)
      (write-char #\: out-stream)
      (princ (position2line-number start content) out-stream)
      (write-char #\: out-stream)
      (write-string (position2line start content) out-stream)
      (terpri out-stream))))

;;
(defgeneric module-collect-components (obj type)
  (:documentation "All children components of type TYPE or a subtype of TYPE."))

(defmethod module-collect-components ((obj module) type)
  (module-collect-components (module-components obj) type))

(defmethod module-collect-components ((obj list) type)
  (when obj
    (nconc (module-collect-components (car obj) type)
           (module-collect-components (cdr obj) type))))

(defmethod module-collect-components ((obj component) type)
  (when(typep obj type)
    (list obj)))

;;
(defun find-regex-grep-output (regex systems out-stream &optional rel-pathname)
  (iter
   (with scanner = (ppcre:create-scanner regex
                                         :case-insensitive-mode t
                                         :multi-line-mode t))
   (for sys in (ensure-list systems))
   (iter
    (for c in (module-collect-components sys 'component))
    (for pathname = (component-pathname c))
    (when (probe-file pathname)
      (find-all-grep-style pathname scanner out-stream rel-pathname)))))

(defun find-regex-systems-to-file (regex systems output-pathname rel-pathname)
  "Write results to file."
  ;; poll for fifo
  (iter
   (repeat 20)
   (sleep 0.05)
   (when (probe-file output-pathname) (leave))
   (finally (error "~S does not exist." output-pathname)))
  (with-open-file (out output-pathname
                       :direction :output
                       :if-exists :overwrite)
    (find-regex-grep-output regex systems out rel-pathname)))

(defun replace-regex-systems (regex replacement systems)
  (iter
   top
   (with scanner = (ppcre:create-scanner regex
                                         :case-insensitive-mode t
                                         :multi-line-mode t))
   (for sys in (ensure-list systems))
   (iter
    (for c in (module-collect-components sys 'component))
    (for pathname = (component-pathname c))
    (when (probe-file pathname)
      (in top (counting (file-replace-all pathname scanner replacement)))))))

(defun all-systems ()
  (let (systems)
    (maphash #'(lambda (key value)
                 (declare (ignore key))
                 (push (cdr value) systems))
             asdf::*defined-systems*)
    systems))

;;; let's use the proper DEFSLIMEFUN
(in-package :swank)

(defslimefun all-systems ()
  (swank-ppcre:all-systems))

(defslimefun find-regex-systems-to-file (regex systems output-pathname rel-pathname)
  (swank-ppcre:find-regex-systems-to-file regex systems output-pathname rel-pathname))

(defslimefun replace-regex-systems (regex replacement systems)
  (swank-ppcre:replace-regex-systems regex replacement systems))

(provide :swank-ppcre)
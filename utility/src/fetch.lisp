(in-package :clml.utility.data)
(eval-when (:load-toplevel :compile-toplevel :execute)

(define-condition invalid-path-error (error)
  ((text :initarg :text :reader text)))

(defun download (url output)
  ( multiple-value-bind (content-or-stream status header tk stream must-close status-string)
      (drakma:http-request url :want-stream t :external-format-out :utf-8)
    (values
     (let ((val
            (if (= status 200)
                (with-open-file (file output
                                      :direction :output
                                      :if-does-not-exist :create
                                      :if-exists :supersede
                                      :element-type '(unsigned-byte 8))
                  (do ((b (read-byte stream nil nil) (read-byte stream nil nil)))
                      ((not b))
                    (write-byte b file))
                  output
                  )
                nil)))
       (when must-close (close stream))
       (or content-or-stream tk header)
       val
       )
     status
     status-string)))

(defun split-file-path (path)
  (let ((pos-last-slash (1+ (position #\/ path :from-end t))))
    (list (subseq path 0 pos-last-slash)
          (subseq path pos-last-slash))))

(defun split-uri-string (uri-string)
  (let ((pu (puri:parse-uri uri-string)))
    (cons (puri:uri-host pu) (split-file-path (puri:uri-path pu)))
    ))

(defun condition-path (path)
  "Abuse puri:parse-uri to strip possible get args from path"
  (let ((p (puri:parse-uri path))) (puri:uri-path p)))

(defun is-file (path)
  (handler-case (probe-file path)
    (type-error (e) #+sbcl (declare (ignore e)) (error 'invalid-path-error
                 :text (format nil "Invalid path: ~A" path)))))



(defun %fetch (url-or-path &key (cache t)
                            (dir (namestring (asdf:system-relative-pathname 'clml "sample/")))
                            (flush nil)
                                 )
  "-return: path to file or nil if unable to fetch
-arguments:
  -url-or-path: <string> pathname or url string identifying file to be fetched.
  -cache: <T|NIL> if T looks for file in -dir and uses that as source if NIL then the a fresh copy of the file is fetched
  -dir: location to store fetched file, default location is in the sample directory in the top level of the clml source tree.
  -flush: if T fetch does not download the file it deletes the existing file.

Fetch file from ~url-or-location~ if not cached in ~dir~
stores the file in the location specified by dir if url or file is url the file
is stored in ~dir~/~uri-host~/~uri-path~.

Note that it is important to ensure that dir and subdir if used end in a /"
  (cond
    ((is-file (condition-path url-or-path)) (condition-path url-or-path))
    ((is-file (condition-path (concatenate 'string  dir url-or-path)))
     (condition-path (concatenate 'string  dir url-or-path)))
    ((puri:parse-uri url-or-path)
     (let* ((tmp-pathname (split-uri-string url-or-path))
            (file-pathstring (format nil "~{~A~^~}" (if dir (cons dir tmp-pathname) tmp-pathname)))
            (file-pathname (ensure-directories-exist
                            file-pathstring)))
       (if flush
           (when (is-file file-pathname) (delete-file file-pathname))
           (if (and cache (probe-file file-pathname))
               (values file-pathname 200 "OK")
               (handler-case  (download url-or-path file-pathname)
                 (drakma:parameter-error ()
                   (values nil 404 "Parameter Error")
                   ))))

       ))
    (t (values nil 404 "Not file of url"))
    )))

(defun fetch (url-or-path
              &key
                (dir (namestring (asdf:system-relative-pathname 'clml "sample/")))
                (external-format :utf-8)
                (cache t)
                (stream nil)
                (flush nil))
  "Fetch file from ~url-or-location~ if not cached in ~dir~
stores the file in the location specified by dir if url or file is url the file
is stored in ~dir~/~uri-host~/~uri-path~.

Note that it is important to ensure that dir and subdir if used end in a /

-return: path to file or stream if :stream parameter is passed
-arguments:
  - url-or-path: <string> pathname or url string identifying file to be fetched.
  - stream: resuests that fetch returns a stream
  - cache: <T|NIL> if T looks for file in -dir and uses that as source if NIL then the a fresh copy of the file is fetched
  - dir: location to store fetched file, default location is in the sample directory in the top level of the clml source tree.
  - flush: if T fetch does not download the file it deletes the existing file.
"
  (let ((fetched-path (%fetch url-or-path :dir dir :cache cache :flush flush)))
    (if (not fetched-path)
        nil
        (if stream
            (open fetched-path :direction :input :external-format external-format)
            fetched-path))))




(defun process-finance-header (stream &key (seperator "=") (column-key "COLUMNS") (len 6))
  "Reads `len` lines of stream extracting header column names and metadata. The stream is
expected to be of format:

    <metadata-key><seperator><metadata-value>
    ...
    <column-key><seperator><comma seperated list of column names>
    ...

Metadata values are parsed in to numbers where possible and comma seperated values are
stored as lists. Google Finance, and Yahoo finance follow these conventions. The defaults
are provisioned for Google Finance. Yahoo finance would use the following:
  seperator \":\"
  column-key \"values\"
  len 16
  -return: list of column names and alist of metadata or nil if unable to read stream
  -arguments:
    -stream:
#+BEGIN_SRC lisp

#+END_SRC
"
  (if stream
      (loop
        with header = '()
        with meta = '()
        for n upfrom 0
        as l = (read-line stream nil)
        if (and l (<= n len))
          do (multiple-value-bind (a cols) (cl-ppcre:scan-to-strings (concatenate 'string "(.*)" seperator "(.*)") l)
               (if (and a (string= (elt cols 0) column-key))
                   (setf header (clml.utility.csv::parse-csv-string (elt  cols 1)))
                   (when a (setf meta
                                 (acons (elt cols 0)
                                        (let ((v (elt cols 1)))
                                          (let ((pv (handler-case
                                                        (map 'list #'parse-number:parse-number
                                                             (clml.utility.csv::parse-csv-string v))
                                                      (error () v))))
                                            (if (> (length pv) 1)
                                                pv
                                                (car pv))))  meta)))))
        when (or (not l) (> n len))
          do (return (values  (coerce header 'list)  meta))
        )
      nil))





(in-package :clml.utility.data)
(eval-when (:load-toplevel :compile-toplevel :execute)


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

(defun fetch (url-or-path &key (cache t)
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
    ((probe-file url-or-path) url-or-path)
    ((probe-file (concatenate 'string  dir url-or-path))
     (concatenate 'string  dir url-or-path))
    ((puri:parse-uri url-or-path)
     (let* ((tmp-pathname (split-uri-string url-or-path))
            (file-pathstring (format nil "~{~A~^~}" (if dir (cons dir tmp-pathname) tmp-pathname)))
            (file-pathname (ensure-directories-exist
                            file-pathstring)))
       (if flush
           (when (probe-file file-pathname) (delete-file file-pathname))
           (if (and cache (probe-file file-pathname))
               file-pathname
               (handler-case  (download url-or-path file-pathname)
                 (drakma:parameter-error (c)
                   nil
                   ))))
       
       ))
    (t nil)
    )))






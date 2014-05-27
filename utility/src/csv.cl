(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'regexp2))

(defpackage :csv
  (:use :common-lisp :iterate :parse-number)
  (:export #:read-csv-file
	   #:read-csv-stream
	   #:write-csv-file
	   #:write-csv-stream
	   #:read-csv-file-and-sort))

(in-package :csv)

;;; helper function
(defun parse-number-no-error (string &optional default)
  (let ((result
         (ignore-errors
          (parse-number:parse-number string))))
    (if result
        result
      default)))

;;; cvs utilities
;;; TODO: extend parsing for quotes and linefeeds
(defparameter *csv-separator* #\,)
(defparameter *csv-quote* #\")
(defparameter *csv-print-quote-p* nil "print \" when the element is a string?")
(defparameter *csv-default-external-format* #+allegro :932 #-allegro :sjis)

(defun write-csv-line (record &key stream)
  "Accept a record and print it in one line as a csv record.

A record is a sequence of element. A element can be of any type.
If record is nil, nothing will be printed.
If stream is nil (default case), it will return a string, otherwise it will return nil.
For efficiency reason, no intermediate string will be constructed. "
  (let ((result
	 (with-output-to-string (s)
	   (let ((*standard-output* s)
		 (record-size (length record)))
	     (iter (for e in-sequence record)
		   (for i from 0)
		   (typecase e
		     (string (progn
			       (if *csv-print-quote-p*
				   (progn
				     (write-char *csv-quote*)
				     (write-string e)
				     (write-char *csv-quote*))
				   (write-string e)))) 
		     (t (princ e)))
		   (when (< i (1- record-size))
		     (write-char *csv-separator*)))))))
    (format stream "~&~a" result)))

(defun write-csv-stream (stream table)
  "Accept a stream and a table and output the table as csv form to the stream.

A table is a sequence of lines. A line is a sequence of elements.
Elements can be any types"
  (iter (for l in-sequence table)
	(write-csv-line l :stream stream))
  (write-char #\newline stream)
  '(ok))

(defun write-csv-file (filename table &key (external-format *csv-default-external-format*))
  "Accept a filename and a table and output the table as csv form to the file.

A table is a sequence of lines. A line is a sequence of elements.
Elements can be any types"
  (with-open-file (f filename :direction :output
		     :if-does-not-exist :create
		     :if-exists :supersede
		     :external-format external-format)
    (write-csv-stream f table)))

(defun parse-csv-string (str) ;; refer RFC4180
  (coerce
   ;; (regexp:split-re "," str)
   (let ((q-count (count *csv-quote* str :test #'char-equal)))
     (cond ((zerop q-count) (regexp:split-re *csv-separator* str))
           ((evenp q-count)
            (macrolet ((push-f (fld flds) `(push (coerce (reverse ,fld) 'string) ,flds)))
              (loop with state = :at-first ;; :at-first | :data-nq | :data-q | :q-in-nq | q-in-q
                  with field with fields
                  for chr of-type character across str
                  do (cond ((eq state :at-first)
                            (setf field nil)
                            (cond ((char-equal chr *csv-quote*) (setf state :data-q))
                                  ((char-equal chr *csv-separator*) (push "" fields))
                                  (t (setf state :data-nq) (push chr field))))
                           ((eq state :data-nq)
                            (cond ((char-equal chr *csv-quote*) (setf state :q-in-nq))
                                  ((char-equal chr *csv-separator*)
                                   (push-f field fields)
                                   (setf state :at-first))
                                  (t (push chr field))))
                           ((eq state :q-in-nq)
                            (cond ((char-equal chr *csv-quote*) (error "#\" inside the non quoted field"))
                                  ((char-equal chr *csv-separator*)
                                   (push-f field fields)
                                   (setf state :at-first))
                                  (t (setf state :data-nq) (push chr field))))
                           ((eq state :data-q)
                            (if (char-equal chr *csv-quote*) (setf state :q-in-q)
                              (push chr field)))
                           ((eq state :q-in-q)
                            (cond ((char-equal chr *csv-quote*) (push chr field) (setf state :data-q))
                                  ((char-equal chr *csv-separator*)
                                   (push-f field fields)
                                   (setf state :at-first))
                                  (t (error "illegal value ( ~A ) after quotation" chr)))))
                  finally (return 
                            (progn (push-f field fields) (reverse fields))))))
           (t (error "odd number of \" ( ~A ) in a line." q-count))))
   'vector))


(defun read-csv-line (stream &key type-conv-fns map-fns (start 0) end)
  "Read one line from stream and return a csv record.

A CSV record is a vector of elements.

type-conv-fns should be a list of functions.
If type-conv-fns is nil (the default case), then all will be treated
as string.

map-fns is a list of functions of one argument and output one result.
each function in it will be applied to the parsed element.
If map-fns is nil, then nothing will be applied.

start and end specifies how many elements per record will be included.
If start or end is negative, it counts from the end. -1 is the last element.
"
  (declare (type (or (simple-array function *) null) type-conv-fns map-fns))
  (let* ((line (read-line stream nil nil)))
    (when line
      (let* ((strs (parse-csv-string line))
             (strs-size (length strs)))
        (when (< start 0)
          (setf start (+ start strs-size)))
        (when (and end (< end 0))
          (setf end (+ end strs-size)))
        (setf strs (subseq strs start end))
        (when type-conv-fns
          (unless (= (length strs) (length type-conv-fns))
            (error "Number of type specifier (~a) does not match the number of elements (~a)."
                   (length strs) (length type-conv-fns))))
        (when map-fns
          (unless (= (length strs) (length map-fns))
            (error "Number of mapping functions (~a) does not match the number of elements (~a)."
                   (length strs) (length map-fns))))
        (let ((result strs))
          ;; strs is not needed so we simply overwrite it
          (when type-conv-fns
            (setf result
              (map 'vector #'funcall type-conv-fns result)))
          (when map-fns
            (setf result
              (map 'vector #'funcall map-fns result)))
          result)))))

(defun read-csv-stream (stream &key (header t) type-spec map-fns (start 0) end)
  "Read from stream until eof and return a csv table.

A csv table is a vector of csv records.
A csv record is a vector of elements.

Type spec should be a list of type specifier (symbols).
If the type specifier is nil or t, it will be treated as string.
If type-spec is nil (the default case), then all will be treated
as string.

map-fns is a list of functions of one argument and output one result.
each function in it will be applied to the parsed element.
If any function in the list is nil or t, it equals to #'identity.
If map-fns is nil, then nothing will be applied.

start and end specifies how many elements per record will be included.
If start or end is negative, it counts from the end. -1 is the last element.
"
  (let ((type-conv-fns
         (when type-spec
           (macrolet ((make-num-specifier (specifier)
                        `(lambda (s) (let ((s (parse-number-no-error s s)))
                                       (if (numberp s) (funcall ,specifier s) s)))))
           (map 'vector
             (lambda (type)
               (ecase type
                 ((t nil string) #'identity)
                 (number #'(lambda (s) (parse-number-no-error s s)))
                 (float (make-num-specifier #'float))
                 (single-float (make-num-specifier #'(lambda (s) (coerce s 'single-float))))
                 (double-float (make-num-specifier #'(lambda (s) (coerce s 'double-float))))
                 (integer (make-num-specifier #'round))
                 (pathname #'pathname)
                 (symbol #'intern)
                 (keyword (lambda (s) (intern s :keyword)))))
             type-spec))))
        (map-fns
         (when map-fns
           (map 'vector
             (lambda (fn)
               (cond ((or (eq fn t)
                          (eq fn nil))
                      #'identity)
                     ((functionp fn)
                      fn)
                     ((and (symbolp fn)
                           (not (keywordp fn)))
                      (symbol-function fn))
                     (t (error "~a is not a valid function specifier." fn))))
             map-fns)))
        (header
         (when header
           (read-csv-line stream)))) 
    (loop for rec = (read-csv-line stream :type-conv-fns type-conv-fns :map-fns map-fns
                                   :start start :end end)
        while rec
        collect rec into result
        finally (return
                  (values
                   (coerce result 'vector)
                   header)))))

(defun read-csv-file (filename &key (header t) type-spec map-fns (external-format *csv-default-external-format*)
		      (os :anynl-dos) (start 0) end)
  "Read from stream until eof and return a csv table.

A csv table is a vector of csv records.
A csv record is a vector of elements.

Type spec should be a list of type specifier (symbols).
If the type specifier is nil or t, it will be treated as string.
If type-spec is nil (the default case), then all will be treated
as string.

map-fns is a list of functions of one argument and output one result.
each function in it will be applied to the parsed element.
If any function in the list is nil or t, it equals to #'identity.
If map-fns is nil, then nothing will be applied.

external-format (default is shift-jis) is a valid AllegroCL external-format type.

OS is a set to eol-convention of the file stream.

start and end specifies how many elements per record will be included.
If start or end is negative, it counts from the end. -1 is the last element.
"
  (with-open-file (f filename :external-format external-format)
    #+allegro (setf (excl:eol-convention f) os)
    (read-csv-stream f :type-spec type-spec :map-fns map-fns
		     :start start :end end
		     :header header)))


(defun read-csv-file-and-sort (filename sort-order &key (header t) (order :ascend) type-spec map-fns (external-format *csv-default-external-format*))
  (let ((table (read-csv-file filename
			      :header header
			      :type-spec type-spec
			      :map-fns map-fns
			      :external-format external-format)))
    (loop for i in (reverse sort-order)
        do (setf table
             (stable-sort table (ecase order (:ascend #'string<=) (:descend #'string>=))
                          :key (lambda (rec) (aref rec i))))
        finally (return table))))

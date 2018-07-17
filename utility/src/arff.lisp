
(in-package :clml.utility.arff)

(defun spacep (char)
  (member char '(#\space #\tab) :test 'char-equal))

(defun next-non-space (string &optional (start 0))
  "Gets the next index of a non space character"
  (position-if-not 'spacep string :start start))

(defun next-space (string &optional (start 0))
  "Gets the next index of a space character"
  (position-if 'spacep string :start start))


(defun read-string (string &optional (start 0))
  "Reads a string from the string then returns the string
   and the next index"
  (let ((start (next-non-space string start)))
    (unless start
      (error "Cannot read a string from ~S" string))
    (let* ((start-sentinal-p (or (eql #\" (char string start)) (eql #\' (char string start))))
           (end-test (if start-sentinal-p
                           (let ((start-char (char string start)))
                             (incf start)
                             (lambda (char) (char= char start-char)))
                           (lambda (char) (member char '(#\space #\tab #\{ #\} #\,) :test #'char=))))
           (end (position-if end-test string :start start)))
      (unless end
        (setf end (length string)))
      (values (subseq string start end) (if start-sentinal-p (1+ end) end)))))

(defun read-datatype (string &optional (start 0))
  (let* ((start (next-non-space string start))
         (first-word (subseq string start (next-space string start))))
    (cond
      ((null start)
        (error "Cannot read a datatype from ~S" string))
      ((or (string-equal "numeric" first-word)
           (string-equal "real" first-word))
        'number)
      ((string-equal "integer" first-word)
        'integer)
      ((string-equal "string" first-word)
        'string)
      ((eql #\{ (char string start))
       (iter (initially (setf loop-start start))
             (for (type loop-start) = (multiple-value-list (read-string string (1+ loop-start))))
             (collect type)
             (setf loop-start (next-non-space string loop-start))
             (unless loop-start (error "No closing } for nominal specifier"))
             (while (char= (char string loop-start) #\,))))
      (t (error "Cannot read a datatype from ~S" string)))))

(defun parse-header-line (line)
  "Parses a line from the header into a cons pair containing a symbol
   describing the line and a list containing any payload for the line.
   Possible line descriptions are:
   -- :relation - The name for the data. Has a single string as the payload
   -- :attribute - The next attribute.  Has a string name and a datatype as
                   the payload
   -- :data - indicates the start of the data section
   -- :empty-line - indicates an empty or comment line"

  ;Find first non space element to use as starting point
  (let* ((start (next-non-space line))
         (line-type (when start (subseq line (1+ start) (next-space line (1+ start))))))
    (cond
      ((or (null start)
           (eql #\% (char line start)))
       '(:empty-line))
      ((not (eql #\@ (char line start)))
       (error "Unable to process line ~S" line))
      ((string-equal "relation" line-type)
       '(:relation))
      ((string-equal "attribute" line-type)
       (multiple-value-bind (name next-start)
         (read-string line (next-space line (1+ start)))
         (list :attribute name (read-datatype line next-start))))
      ((string-equal "data" line-type)
       '(:data))
      (t (error "Unable to process line ~S" line)))))

(defun parse-arff-string (str)
  (coerce
   (let ((quote-or-escape-p (or (zerop (count #\" str :test #'char-equal))
                                (zerop (count #\' str :test #'char-equal))
                                (zerop (count #\\ str :test #'char-equal))))
         (sep-regex (cl-ppcre:parse-string (string #\,))))
     (cond ((not quote-or-escape-p)
            (substitute nil "?" (cl-ppcre:split sep-regex str) :test 'string=))
           (t
            (macrolet ((push-f (fld flds) `(push (coerce (reverse ,fld) 'string) ,flds)))
              (loop with state = :at-first ;; :at-first | :data-nq | :data-sq | :data-dq | :at-end | :line-end
                    with field with fields
                    for chr of-type character across str
                    until (eq state :line-end)
                do (cond ((eq state :data-escape) (push chr field))
                         ((eq state :at-first)
                          (setf field nil)
                          (cond ((char-equal chr #\\) (setf state :data-escape))
                                ((char-equal chr #\") (setf state :data-dq))
                                ((char-equal chr #\') (setf state :data-sq))
                                ((char-equal chr #\,) (push "" fields))
                                ((char-equal chr #\%)
                                 (when fields ;else its a blank line
                                   (push "" fields))
                                 (setf state :line-end))
                                (t (setf state :data-nq) (push chr field))))
                         ((eq state :data-nq)
                          (cond ((char-equal chr #\\) (setf state :data-escape))
                                ((char-equal chr #\,)
                                 (if (equal '(#\?) field)
                                   (push nil fields)
                                   (push-f field fields))
                                 (setf state :at-first))
                                ((char-equal chr #\%)
                                 (if (equal '(#\?) field)
                                   (push nil fields)
                                   (push-f field fields))
                                 (setf state :line-end))
                                (t (push chr field))))
                         ((eq state :data-dq)
                          (cond ((char-equal chr #\\) (setf state :data-escape))
                                ((char-equal chr #\") (setf state :at-end))
                                (t (push chr field))))
                         ((eq state :data-sq)
                          (cond ((char-equal chr #\\) (setf state :data-escape))
                                ((char-equal chr #\') (setf state :at-end))
                                (t (push chr field))))
                         ((eq state :at-end)
                          (cond ((char-equal chr #\%) (setf state :line-end))
                                ((char-equal chr #\,)
                                 (push-f field fields)
                                 (setf state :at-first))
                                (t (error "illegal value ( ~A ) after quotation" chr)))))
                finally (return
                          (progn
                            (unless (eq state :line-end) (push-f field fields))
                            (reverse fields))))))))
   'vector))

(defun read-arff-line (stream &key type-conv-fns map-fns (start 0) end)
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
  (let* ((rline (read-line stream nil nil)))
    (when rline
      (let* ((line (string-trim '(#\Space #\Tab #\Newline #\Return) rline))
             (strs (parse-arff-string line))
             (strs-size (length strs)))
        (when (= (length strs) 0)
          (return-from read-arff-line nil))
        (when (< start 0)
          (setf start (+ start strs-size)))
        (when (and end (< end 0))
          (setf end (+ end strs-size)))
        (setf strs (subseq strs start end))
        (when type-conv-fns
          (unless (= (length strs) (length type-conv-fns))
            (error "Number of type specifier (~a) does not match the number of elements (~a)."
                   (length type-conv-fns) (length strs))))
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

(defun read-arff-stream (stream &key map-fns (start 0) end)
  "Read from stream in arff format until eof and return a csv table.

A csv table is a vector of csv records.
A csv record is a vector of elements.

map-fns is a list of functions of one argument and output one result.
each function in it will be applied to the parsed element.
If any function in the list is nil or t, it equals to #'identity.
If map-fns is nil, then nothing will be applied.

start and end specifies how many elements per record will be included.
If start or end is negative, it counts from the end. -1 is the last element.
"
  (let* ((attributes
          (loop for (line-type . line-data) = (parse-header-line (read-line stream))
            until (eql line-type :data)
            if (eql line-type :attribute)
            collect line-data))
         (type-conv-fns
             (macrolet ((make-num-specifier (specifier)
                          `(lambda (s) (let ((s (clml.utility.csv::parse-number-no-error s s)))
                                         (if (numberp s) (funcall ,specifier s) s)))))
               (map 'vector
                 (lambda (attribute)
                   (let ((type (second attribute)))
                     (cond
                       ((eql 'string type) #'identity)
                       ((eql 'number type) (make-num-specifier #'identity))
                       ((eql 'integer type) (make-num-specifier #'round))
                       ((listp type)
                        (lambda (value)
                          (if (member value type :test #'string-equal)
                            value
                            (error "~S is not one of ~S" value type))))
                       (t (error "Unknown attribute type ~S" type)))))
                 attributes)))
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
              map-fns))))
      (loop for rec = (read-arff-line stream :type-conv-fns type-conv-fns :map-fns map-fns
                                     :start start :end end)
            while rec
        collect rec into result
        finally (return
                  (values
                    (coerce result 'vector)
                    (map 'vector 'first attributes))))))

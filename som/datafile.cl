;;; -*- lisp -*-
;;; $Id: datafile.cl,v 1.1.2.7 2007/01/12 11:25:30 tada Exp $

(in-package :som)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (proclaim '(optimize (speed 3))))

(defun label-not-needed (level)
  (if (>= level 0)
      level
    nil))


(defun topol-type (s)
  (assert (stringp s))
  (handler-case (get-type-by-str *topol-list* s)
    (error () *topol-unknown*)))

(defun unknown-topol-p (topol)
  (= topol *topol-unknown*))

(defun neigh-type (s)
  (assert (stringp s))
  (handler-case (get-type-by-str *neigh-list* s)
    (error () *neigh-unknown*)))

(defun unknown-neigh-p (neigh)
  (= neigh *neigh-unknown*))

(defun get-elements-list-elim-space (str)
  (let ((list (delimited-string-to-list str #\space)))
    (remove "" list :test #'string=)))
    
(defun get-topol (str)
  (let ((s (second (get-elements-list-elim-space str))))
    (if (stringp s)
	(topol-type s)
      *topol-unknown*)))

(defun get-neigh (str)
  (let ((s (fifth (get-elements-list-elim-space str))))
    (if (stringp s)
	(neigh-type s)
      *neigh-unknown*)))

(defun get-xdim (str)
  (let ((s (third (get-elements-list-elim-space str))))
    (if (stringp s)
	(read-from-string s)
      0)))


(defun get-ydim (str)
  (let ((s (fourth (get-elements-list-elim-space str))))
    (if (stringp s)
	(read-from-string s)
      0)))

(defun get-weight (str)
  (read-from-string (subseq str (length "weight="))))

(defun get-fixed (str)
  (let* ((tmp (make-instance 'fixpoint))
	 (substr (subseq str (length "fixed=")))
	 (lst (delimited-string-to-list substr ",")))
    (when (/= (length lst) 2)
      (error "Fixed point incorrect"))
    (destructuring-bind (x y)
	lst
      (when (or (string= x "")
		(string= x "")
		(< (read-from-string x) 0)
		(< (read-from-string y) 0))
	(error "Fixed point incorrect value"))
      (setf (fixpoint-xfix tmp) (read-from-string x))
      (setf (fixpoint-yfix tmp) (read-from-string y)))
    tmp))
    



;; read-headers - reads the header information from file and sets the 
;; entries variables accordingly. 

(defmethod read-headers ((entries entries))
  (let ((file-info (entries-file-info entries))
	line)
    ;; find the first not-comment line
    (loop
	while (char= (elt (setq line (som-getline file-info)) 0) #\#))
    
    (let ((dim (read-from-string (first (get-elements-list-elim-space line)))))
      (when (<= dim 0)
	(error "Can't read dimension parameter in file ~a"
	       (file-info-name file-info)))
      (setf (entries-dimension entries) dim)
      (setf (entries-topol entries) (get-topol line))
      (setf (entries-neigh entries) (get-neigh line))
      (setf (entries-xdim entries) (get-xdim line))
      (setf (entries-ydim entries) (get-ydim line))
      
      )))

;; close-entries - closes the file associated with entries if there is one.
(defmethod close-entries ((entries entries))
  (close-file (entries-file-info entries)))

(defun open-entries (gdata name)
  (let ((entries (open-data-file gdata name)))
    (handler-case 
	(read-headers entries)
      (error (c)
	(ignore-errors (close-entries entries))
	(error c)))
    entries))


;; alloc-entries - make and initialize an entries-structure. 
(defun alloc-entries (gdata)
  (make-instance 'entries
    :parent-gdata gdata)
  )



;; open-data-file - opens a data file for reading. 
;; Returns entries instance.
;; If name is nil, just create the entries instance  but doesn't open any file. 
(defun open-data-file (gdata filename)
  (let ((entries (alloc-entries gdata)))
    (setf (entries-file-info entries)
      (open-file filename "r"))
    entries))




;; initialize and possibly allocate room for data-entry. If entry is NULL, 
;; a new entry is allocated and initialized. If entry is a pointer to an 
;; old entry, the old entry is re-initialized.
(defun init-entry (entr entry)
  (unless entry
    (setq entry (make-instance 'data-entry))
    (setf (data-entry-points entry)
      (make-array (entries-dimension entr) 
		  :element-type 'double-float
		  :initial-element 0.0d0)))
  
  ;; discard mask
  (setf (data-entry-mask entry) nil)
  
  ;; discard fixed point
  (setf (data-entry-fixed entry) nil)
  
  (clear-entry-labels entry)
  (setf (data-entry-weight entry)  0.0d0)
  
  entry)


(defmacro alloc-entry (entr) 
  `(init-entry ,entr nil))


;; set-mask - sets a value in mask 
(defun set-mask (mask dim n)
  (unless mask
    (setq mask (make-array dim :element-type '(signed-byte 32) :initial-element 0)))
  (when (>= n 0)
    (setf (aref mask n) 1))
  mask)
    

;; load-entry - loads one data-entry from file associated with entries. If 
;; data-entry is non-nil, an old data-entry is reused, otherwise a new 
;; data-entry is allocated.
;; when no more line returns nil
(defun load-entry (entries data-entry)
  (flet ((read-next-line (file-info)
	   (unless (file-info-eof file-info)
	     (let ((line))
	       (loop
		   do (setq line (som-getline file-info)) ;; get line from file 
		      (when (and (not line)
				 (file-info-eof file-info))
			(return nil))
		      (when (and line 
				 (not (char= (elt line 0) #\#)) ;; skip comments
				 )
			(return line))
		      )))))
    (let ((file-info (entries-file-info entries))
	  (dim (entries-dimension entries)))
      (loop
	  do (block set-one-line
	       (let ((line (read-next-line file-info)))
		 (unless line ; no more data -> loop end
		   (setq data-entry nil)
		   (return nil))
		 
		 ;; If data-entry is given, a new entry is loaded on over the old one. If 
		 ;; data-entry is nil, room for the new entry is allocated 
		 (setq data-entry (init-entry entries data-entry))
	       
		 (let ((row (file-info-lineno file-info)))
		   (when (stringp line)
		     (let ((mask)
			   (maskcnt 0)	; now many components are masked 
			   (toke-list (get-elements-list-elim-space line))
			   (ent))
		       ;; Read the vector values 
		       (loop for item in toke-list
			   for i from 0 to (1- dim)
			   do (if (string= item *masked-value*)
				  (progn
				    (setq mask (set-mask mask dim i))
				    (incf maskcnt)
				    (setq ent 0.0d0))
				(setq ent (coerce (read-from-string item) 'double-float)))
			      (setf (aref (data-entry-points data-entry) i) ent))
		       ;; Entries with all components masked off are normally discarded but 
		       ;; they are loaded if the skip_empty-flag is set in the flags of the 
		       ;; file.
		       (when (= maskcnt dim)
			 (format t "load-entry: skipping line ~d of file ~a, all components are masked off~%" row (file-info-name file-info))
			 (when (slot-value (entries-entries_flags entries) 'skip-empty)
			   (setq mask nil)
			   (return-from set-one-line nil)))
		       
		       (when mask
			 (setf (data-entry-mask data-entry) mask)
			 (setq mask nil))
		       
		       ;; Now the following tokens (if any) are label,
		       ;; weight term and fixed point description.
		       ;; Sometimes label is not needed. Other terms are never
		       ;; needed 
		     
		       (let ((label-found))
			 (loop for item in (subseq toke-list dim)
			     do (cond ((search "weight=" item)
				       (setf (data-entry-weight data-entry)
					 (get-weight item)))
				      ((search "fixed=" item)
				       (setf (data-entry-fixed data-entry)
					 (get-fixed item)))
				      ((stringp item)
				       (add-entry-label data-entry 
							(find-conv-to-ind item (entries-parent-gdata entries)))
				       (setf label-found t))
				      (t
				       (error "invalid lebel ~a~%" item))))
			 (when (and (slot-value (entries-entries_flags entries) 'labels-needed)
				    (not label-found))
			   (error "Required label missing on line ~d of file ~a~%"
				  row
				  (file-info-name file-info)))))
		     (return nil)))))))
    data-entry))
    
			  

;; read-entries - reads data from file to memory. If LOADMODE_ALL is
;; used the whole file is loaded into memory at once and the file is
;; closed. If buffered loading (LOADMODE_BUFFER) is used at most N
;; data vectors are read into memory at one time. The buffer size N is
;; given in the entries->buffer field. In both cases if there are any
;; previous entries in the entries structure, they are overwritten and
;; the memory space allocated for them is reused. 
(defun read-entries (entries)
  (let ((noc 0)
	(len (length (entries-entries entries)))
	(data-entry)
	(lst))
    (setf (entries-entries entries)
      (loop 
	  for i from 0
	  do  (if (or (= len 0)
		      (> i len))
		  (setq data-entry (load-entry entries nil))
		(setq data-entry (load-entry entries (elt (entries-entries entries) i))))
	      ;; when no more line, load-entry returns nil
	      (if data-entry
		  (progn (push data-entry lst)
			 (incf noc))
		(return (reverse lst)))))
    (setf (entries-num-loaded entries) noc)
    (when (eq (slot-value (entries-entries_flags entries) 'loadmode)
	      *loadmode-all*)
      (setf (entries-num-entries entries) noc)
      (setf (slot-value (entries-entries_flags entries) 'totlen-known) t)
      (close-file (entries-file-info entries))
      (setf (entries-file-info entries) nil))
    
    ;; Randomize entry order if wanted. 
    (when (slot-value (entries-entries_flags entries) 'random-order)
      (setf (entries-entries entries)
	(randomize-entry-order (entries-entries entries))))))

;; rewind-entries - go to the first entry in entries list. Returns pointer
;; to first data_entry. Loads data from file if it hasn't been loaded yet and
;; rewinds file if we are using buffered reading.
(defun rewind-entries (entries)
  (let ((data-entries (entries-entries entries)))
    (when (and (= (length data-entries) 0)
	       (not (slot-value (entries-entries_flags entries) 'totlen-known)))
      ;; not buffered
      (read-entries entries))
    (setf (entries-current entries) 0)
    (incf (entries-lap entries))))


;; randomize-entry-order - arrange a list of data-entries to random order. 
(defun randomize-entry-order (data-entries)
  (declare (optimize (speed 3)))
  (when data-entries
    (let ((noc (length data-entries))
	  (lst))
      (loop for i fixnum from noc downto 1
	  do (let* ((orand (orand))
		    (index (rem (the (integer 0 #.most-positive-fixnum) orand) i))
		    (data-entry-obj (nth index data-entries)))
	       (push data-entry-obj lst)
	       (setq data-entries
		 (remove data-entry-obj data-entries :test #'equal))))
      (setq lst (reverse lst))
      lst)))

		     
      

;; set-teach-params - sets values in teaching parameter structure based
;; on values given in codebook and data files 
(defun set-teach-params (params codes data)
  (setf (teach-params-topol params) (entries-topol codes))
  (setf (teach-params-mapdist params) nil)
  (setf (teach-params-neigh params) (entries-neigh codes))
  (setf (teach-params-neigh-adapt params) nil)

  ;; these two might change when using a different variation, for
  ;; example, dot product 

  (setf (teach-params-winner params) (function find-winner-euc))
  (setf (teach-params-dist params) (function vector-dist-euc))
  (setf (teach-params-vector-adapt params) (function adapt-vector))

  (when codes
    (setf (teach-params-codes params) codes))
  (when data
    (setf (teach-params-data params) data))
  (setf (teach-params-snapshot params) nil)
  )

(defun topol-str (i)
  (handler-case (get-type-by-id *topol-list* i)
    (error () "")))

(defun neigh-str (i)
  (handler-case (get-type-by-id *neigh-list* i)
    (error () "")))

   
;; write_header - writes header information to datafile 
(defun write-header (file-info codes)
  (let ((fp (file-info-fp file-info)))
    (when codes
      (format fp "~d" (entries-dimension codes))
      (when (> (entries-topol codes) *topol-data*)
	(format fp " ~a" (topol-str (entries-topol codes)))
	(when (> (entries-topol codes) *topol-lvq*)
	  (format fp " ~d" (entries-xdim codes))
	  (format fp " ~d" (entries-ydim codes))
	  (format fp " ~a" (neigh-str (entries-neigh codes)))))
      (format fp "~%")))
  t)

;; write_entry - writes one data entry to file. 
(defun write-entry (file-info entries data-entry)
  (let ((fp (file-info-fp file-info)))
    ;; write vector 
    (loop for i from 0 to (1- (entries-dimension entries))
	do (if (and (data-entry-mask data-entry)
		    (/= (aref (data-entry-mask data-entry) i) 0))
	       (format fp "~a " *masked-value*)
	     (format fp "~f " (aref (data-entry-points data-entry) i))))
    ;; Write labels. The last label is empty 
    (loop for i from 0
	do (let ((label (get-entry-labels data-entry i)))
	     (if (/= label *label-empty*)
		 (format fp "~a " (find-conv-to-lab label (entries-parent-gdata entries)))
	       (return nil))))
    (format fp "~%")
    t))

;; save_entries_wcomments - saves data to a file with optional comment
;; or header lines. Returns a non-zero value on error. 

(defun save-entries-wcomment (codes out-code-file &optional (comments))
  (let ((file-info (open-file out-code-file "w")))
    (handler-case
	(progn
	  ;; write header 
	  (write-header file-info codes)
	  ;; write comments if there are any 
	  (when comments 
	    (format (file-info-fp file-info) "~a" comments))
	  ;; write entries 
	  (rewind-entries codes)
	  (loop for data-entry in (entries-entries codes)
	      do (write-entry file-info codes data-entry)
		 ))
      (error ()))
    (close-file file-info)))

;;; -*- lisp -*-
;;; $Id: fileio.cl,v 1.1.2.2 2006/10/02 02:11:59 tada Exp $

(in-package :som)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (proclaim '(optimize (speed 3))))
   
(defclass file-info ()
  ((name :accessor file-info-name :initarg :name :initform nil)
   (fp :accessor file-info-fp :initarg :fp :initform nil) ; file stream
   (eof :accessor file-info-eof :initarg :eof :initform nil
	:documentation "has end of line been reached")
   (lineno :accessor file-info-lineno :initarg :lineno :initform 0
	   :documentation "line number we are on")))


;; som-getline - get a line from file. Returns the string of line 
(defmethod som-getline (file-info)
  (handler-case 
      (let ((line (read-line (file-info-fp file-info))))
	(incf (file-info-lineno file-info))
	line)
    (end-of-file () 
      (setf (file-info-eof file-info) t)
      nil)))

(defun open-file (filename fmode)
  (let ((file-info (make-instance 'file-info)))
    (when filename
      (cond ((string-equal fmode "r")
	     (setf (file-info-fp file-info)
	       (open filename :direction :input)))
	    ((string-equal fmode "w")
	     (setf (file-info-fp file-info)
	       (open filename 
		     :direction :output
		     :if-does-not-exist :create
		     :if-exists :supersede)))
	    (t
	     (error "open-file :: unknown mode ~a" fmode)))
      (setf (file-info-name file-info) filename))
    file-info))
		   

(defun close-file (file-info)
  (when (and file-info (streamp (file-info-fp file-info))
	     (open-stream-p (file-info-fp file-info)))
    (close (file-info-fp file-info))))


    

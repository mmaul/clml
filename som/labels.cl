;;; -*- lisp -*-
;;; $Id: labels.cl,v 1.1.2.4 2007/01/12 11:25:37 tada Exp $

(in-package :som)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (proclaim '(optimize (speed 3))))


;; ********** Routines for manipulating labels ******************** 

;; labels are stored in the following way: If there is only one label,
;; store it in the labels pointer.  If there are multiple labels, the
;; labels entry is a pointer to an array of labels. The num_labs field
;; in the entry tells the number of labels in that entry 

;; set_entry_label - sets the label on one data entry. All previous 
;;  labels are discarded.

(defun set-entry-label (data-entry label)
  ;; remove previous labels if any 
  (clear-entry-labels data-entry)
  (when (/= label *label-empty*)
    ;; empty label == no label 
    (setf (slot-value (data-entry-lab data-entry) 'label)
      label)
    (setf (data-entry-num-labs data-entry) 1)))

(defun copy-entry-labels (data-entry-dest data-entry-source)
  (setf (slot-value (data-entry-lab data-entry-dest) 'label)
    (slot-value (data-entry-lab data-entry-source) 'label))
  (setf (slot-value (data-entry-lab data-entry-dest) 'label-array)
    (copy-seq (slot-value (data-entry-lab data-entry-source) 'label-array)))
  (setf (data-entry-num-labs data-entry-dest)
    (data-entry-num-labs data-entry-source)))
  
;; enlarge-array - enlarges (or allocates) the array used to store 
;; the labels. 
(defun enlarge-array (gdata)
  (incf (gdata-lab-array-size gdata) *label-array-size*)
  (if (arrayp (gdata-labels-array gdata))
      (adjust-array (gdata-labels-array gdata) (gdata-lab-array-size gdata))
    (setf (gdata-labels-array gdata)
      (make-array (gdata-lab-array-size gdata) :adjustable t))))
  

;; get-entry-labels - get i:th label from entry (i starts from 0). 
(defun get-entry-labels (data-entry i)
  (cond ((and (<= (data-entry-num-labs data-entry) 1)
	      (= i 0))
	 (slot-value (data-entry-lab data-entry) 'label))
	((>= i (data-entry-num-labs data-entry))
	 *label-empty*)
	(t
	 (elt (slot-value (data-entry-lab data-entry) 'label-array) i))))
	

(defmacro get-entry-label (e)
  `(get-entry-labels ,e 0))

;; clear-entry-label - remove all labels from entry. 
(defun clear-entry-labels (data-entry)
  (setf (slot-value (data-entry-lab data-entry) 'label-array) nil)
  (setf (data-entry-num-labs data-entry) 0)
  (setf (slot-value (data-entry-lab data-entry) 'label)
    *label-empty*))

(defun add-entry-label (data-entry label)
  ;; adding an empty label does nothing 
  (unless (= label *label-empty*)
    (if (= (data-entry-num-labs data-entry) 0)
	;; add first label to entry 
	(progn
	  (setf (data-entry-num-labs data-entry) 1)
	  (setf (slot-value (data-entry-lab data-entry) 'label) label))
      ;; if there is already one label, we need to allocate a table for the labels
      (if (= (data-entry-num-labs data-entry) 1)
	  (let ((atable (make-array *atable-increment* :adjustable t)))
	    ;;  move old entry to table and add the new label 
	    (incf (data-entry-num-labs data-entry))
	    (setf (elt atable 0) (slot-value (data-entry-lab data-entry) 'label))
	    (setf (elt atable 1) label)
	    (setf (slot-value (data-entry-lab data-entry) 'label-array) atable))
	(progn
	  ;;  enlarge label array if needed 
	  (when (= (rem (data-entry-num-labs data-entry) *atable-increment*) 0)
	    (setf (slot-value (data-entry-lab data-entry) 'label-array)
	      (adjust-array (slot-value (data-entry-lab data-entry) 'label-array)
			    (+ (data-entry-num-labs data-entry) *atable-increment*))))
	  (setf (elt (slot-value (data-entry-lab data-entry) 'label-array)
		     (data-entry-num-labs data-entry))
	    label)
	  (incf (data-entry-num-labs data-entry)))
	))))


;; find-conv-to-ind - Give the corresponding index. If the label is
;; not yet there, add it to table. Empty label is always 0 
(defun find-conv-to-ind (lab gdata)
  (if (stringp lab)
      (if (string= lab "") ; empty string -> empty label 
	  *label-empty* 
	(let ((label (loop for item across (gdata-labels-array gdata)
			 for i from 1
			 when (and (stringp item) (string= lab item))
			 do (return i))))
	  (when (not (numberp label))
	    (setf label (gdata-num-labs gdata))
	    ;; label not found in array. Add it. 
	    (when (>= (gdata-num-labs gdata) (gdata-lab-array-size gdata))
	      (enlarge-array gdata))
	    (setf (elt (gdata-labels-array gdata) (gdata-num-labs gdata)) lab)
	    (incf (gdata-num-labs gdata)))
	  (1+ label)))
    *label-empty*			; no string -> empty label
    ))
      


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; hitlists - hitlist is a list of label,frequency pairs that is kept sorted 
;; so that the label with the highest frequency is always the first in the
;; list. 

(defclass hitlist ()
  ((lst :accessor hitlist-lst :initform nil
	:documentation "list of hit-entries")
   (entries :accessor hitlist-entries :initform 0
	    :documentation "number of entries")
   ))

(defclass hit-entry ()
  ((label :accessor hit-entry-label :initarg :label)
   (freq :accessor hit-entry-freq :initarg :freq
	 :documentation "frequency of this label")))

;; initialize a new hitlist 
(defun new-hitlist ()
  (make-instance 'hitlist))

;; find_hit - find a hit-entry corresponding to a certain label from
;; list. Returns nil if there is no entry for the label 
(defun find-hit (hl label)
  (let ((hit-entry (loop for he in (hitlist-lst hl)
		       do  (when (= label (hit-entry-label he))
			     (return he)))))
    hit-entry))

(defmethod sort-hit-entries ((hitlist hitlist))
  ;; sort by freq of hit-entry in hitlist
  (let ((lst (hitlist-lst hitlist)))
    (setf (hitlist-lst hitlist)
      (sort lst #'>
	    :key #'hit-entry-freq))))
  
;; add_hit - add a hit in the list for a label 
(defun add-hit (hl label)
  (let ((he (find-hit hl label)))
    (if he
	(progn 
	  (incf (hit-entry-freq he))
	  (sort-hit-entries hl))
      (progn
	(setq he 
	  (make-instance 'hit-entry
	    :label label
	    :freq 1))
	;; add to the end of list
	(setf (hitlist-lst hl)
	  (append (hitlist-lst hl) (list he)))
	(incf (hitlist-entries hl))))
    (hit-entry-freq he)))

	
;; find_conv_to_lab - Give the corresponding label; if the index is
;; not yet there, return NULL 
(defun find-conv-to-lab (ind gdata)
  (cond ((= ind *label-empty*)
	 nil)
	((or (> ind (gdata-num-labs gdata))
	     (< ind 0))
	 nil)
	(t
	 (elt (gdata-labels-array gdata) (1- ind)))))

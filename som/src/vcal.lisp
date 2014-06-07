;;; -*- lisp -*-
;;; $Id: vcal.cl,v 1.1.2.4 2006/10/02 05:48:03 tada Exp $

(in-package :som)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (proclaim '(optimize (speed 3))))

(defun find-labels (teach-params numlabs &optional (showmeter nil))
  (let ((winner (teach-params-winner teach-params))
	(data (teach-params-data teach-params))
	(codes (teach-params-codes teach-params))
	(win-info (make-instance 'winner-info))
	(nol)
	(ind)
	(index)
	(labs)
	(flag nil))
    
    (when (< numlabs 0)
      (setq numlabs 0))
    
    (rewind-entries codes)
    
    (let* ((noc (entries-num-entries codes))
	   (hits (loop for i from 0 to (1- noc)
		     collect (new-hitlist))))
      (rewind-entries data)
      
      ;; show progress meter only if number of data vectors is known (when
      ;; not using buffered input) 
      (when showmeter
	(setq flag (slot-value (entries-entries_flags data) 'totlen-known))
	(if flag
	    (setq nol (entries-num-entries data))
	  (setq nol 0)))
      (setq ind 0)
      
      (loop for data-entry in (entries-entries data)
	  do (let ((datalabel (get-entry-label data-entry)))
	       (when (funcall winner codes data-entry win-info 1)
		 ;; winner found
		 ;; if winner not found -> assume that all components
		 ;; of sample vector were masked off -> skip this
		 ;; sample 
		 (setq index (winner-info-index win-info))
		 (when (/= datalabel *label-empty*)
		   (add-hit (elt hits index) datalabel)))
	       ;; Take the next data entry 
	       (incf ind)
	       (when (and showmeter flag)
		 (format t "~d~%" (decf nol)))))
      
      ;; Set the label of codebook entries according the
      ;; selections. Numlabs tells how many labels at maximum to assign to
      ;; a certain codebook vector. 0 means all 
      (rewind-entries codes)
      (setq index 0)

      (loop for data-entry in (entries-entries codes)
	  do (if (= numlabs 0)
		 (setq labs (hitlist-entries (elt hits index)))
	       (setq labs 
		 (min (hitlist-entries (elt hits index))
		      numlabs)))
	     ;; remove previous labels from codebook vector 
	     (clear-entry-labels data-entry)
	     (let ((hit-entry-list (hitlist-lst (elt hits index))))
	       (loop for i from 0 to (1- labs)
		   do (add-entry-label data-entry (hit-entry-label 
						   (elt hit-entry-list i)))))
	     (setf (elt hits index) nil)
	     (incf index))
      
      codes)))
      
  
(defun vcal (g-data numlabels)
  (when (< numlabels 0)
    (setq numlabels 1))
  
  (let ((data (gdata-data g-data)))
    (label-not-needed 1)
    (let ((codes (gdata-codes g-data)))
      (when (< (entries-topol codes) *topol-hexa*)
	(error "codes data is broken.~%"))
      (when (/= (entries-dimension data) (entries-dimension codes))
	(error "dimension data is broken.~%"))
      (let ((params (make-instance 'teach-params)))
	(set-teach-params params codes data)
	(set-som-params params)
	(find-labels params numlabels)
	))))

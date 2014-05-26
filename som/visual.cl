;;; -*- lisp -*-
;;; $Id: visual.cl,v 1.1.2.7 2007/01/18 11:18:24 tada Exp $

(in-package :som)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (proclaim '(optimize (speed 3))))

(defun compute-visual-data (gdata teach-params out-file-name)
  (let ((data (teach-params-data teach-params))
	(codes (teach-params-codes teach-params))
	(winner (teach-params-winner teach-params))
	(emptylab *label-empty*)
	(win-info (make-instance 'winner-info))
	(file-info)
	(nod))
    (declare (ignore nod))
    
    (setq emptylab 
      (find-conv-to-ind "EMPTY_LINE" gdata))

    ;; initialize fake entries table 
    (let ((fake-data (alloc-entries gdata)))
      (setf (entries-dimension fake-data) 3)
      (setf (entries-topol fake-data) (entries-topol codes))
      (setf (entries-neigh fake-data) (entries-neigh codes))
      (setf (entries-xdim fake-data) (entries-xdim codes))
      (setf (entries-ydim fake-data) (entries-ydim codes))
      (let ((fake-entry (alloc-entry fake-data)))
	(setf (entries-entries fake-data) (list fake-entry))
	(setq file-info (open-file out-file-name "w"))
	(setf (entries-file-info fake-data) file-info)
	;; write header of output file 
	(write-header file-info fake-data)
	
	(rewind-entries data)
	#+ignore 
	(if (slot-value (entries-entries_flags data) 'totlen-known)
	    (setq nod (entries-num-entries data))
	  (setq nod 0))
	
	(loop for data-entry in (entries-entries data)
	    do (if (not (funcall winner codes data-entry win-info 1))
		   ;; empty sample
		   ;; Save the classification and coordinates 
		   (progn
		     (set-entry-label fake-entry emptylab)
		     (setf (aref (data-entry-points fake-entry) 0) -1.0d0)
		     (setf (aref (data-entry-points fake-entry) 1) -1.0d0)
		     ;; And the quantization error 
		     (setf (aref (data-entry-points fake-entry) 2) -1.0d0))
		 (let ((bpos (winner-info-index win-info)))
		       ;(index (get-entry-label (winner-info-winner win-info))))
		   ;; Save the classification and coordinates 
		   (copy-entry-labels fake-entry (winner-info-winner win-info))
		   (setf (aref (data-entry-points fake-entry) 0)
		     (coerce (rem bpos (entries-xdim codes)) 'double-float))
		   (setf (aref (data-entry-points fake-entry) 1)
		     (coerce (floor (/ bpos (entries-xdim codes))) 'double-float))
		   ;; And the quantization error 
		   (setf (aref (data-entry-points fake-entry) 2)
		     (sqrt (the (double-float 0d0 *) (winner-info-diff win-info))))))
	       ;; write new entry 
	       (write-entry file-info fake-data fake-entry)
	       ))
      (close-file file-info)
      fake-data)))
	       
		 
		     
		 
      
    
	 
(defun visual (g-data &key (debug nil) directory)
  (label-not-needed 1)
  
  (let ((data (gdata-data g-data))
	(codes (gdata-codes g-data))
	(params (make-instance 'teach-params))
	(noskip))
    
    (when (< (entries-topol codes) *topol-hexa*)
      (error "codes->topol is breken~%"))
    (when (/= (entries-dimension data) (entries-dimension codes))
      (error "dimension data is broken~%"))
    
    (set-teach-params params codes data)
    (set-som-params params)
    
    (when noskip
      (setf (slot-value (entries-entries_flags data) 'skip-empty) nil))
    
    (let ((pathname (make-pathname :name "out"
                                   :directory directory)))
      (compute-visual-data g-data params pathname)
      pathname)))

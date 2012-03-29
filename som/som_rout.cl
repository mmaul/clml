;;; -*- lisp -*-
;;; $Id: som_rout.cl,v 1.1.2.11 2007/01/18 11:18:08 tada Exp $

(in-package :som)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (proclaim '(optimize (speed 3))))

(defun randinit-codes (data topol neigh xdim ydim)
  (declare (type (integer 0 #.most-positive-fixnum) xdim ydim)
	   (type fixnum topol neigh)
	   (optimize (speed 3)))
  (let ((noc (* xdim ydim))
	(codes (alloc-entries (entries-parent-gdata data)))
	(dim (entries-dimension data)))
    (declare (type (integer 0 #.most-positive-fixnum) noc dim))
    
    (setf (entries-dimension codes) dim)
    (setf (slot-value (entries-entries_flags codes) 'loadmode) *loadmode-all*)
    (setf (entries-xdim codes) xdim)
    (setf (entries-ydim codes) ydim)
    (setf (entries-topol codes) topol)
    (setf (entries-neigh codes) neigh)
    (setf (entries-parent-gdata codes) (entries-parent-gdata data))
    
    (setf (entries-entries codes)
      (loop for i fixnum from 1 to noc
	  collect (alloc-entry codes)))
    (setf (entries-num-entries codes) noc)
    
    (let* ((compcnt (make-array dim :element-type '(signed-byte 32) :initial-element 0))
	   (maval (alloc-entry data))	; maval is data-entry
	   (mival (alloc-entry data))	; mival is data-entry
	   (data-entry-points-maval (data-entry-points maval))
	   (data-entry-points-mival (data-entry-points mival))
	  )
      (declare (type (simple-array double-float (*)) data-entry-points-maval data-entry-points-mival))
      (loop for i fixnum from 0 to (1- (the fixnum (entries-dimension data)))
	  do (setf (aref data-entry-points-maval i) *flt-min*)
	     (setf (aref data-entry-points-mival i) *flt-max*))
      (rewind-entries data)
      (loop for i fixnum from 0 to (1- (the fixnum (entries-num-entries data)))
	  do (let* ((data-entry (elt (entries-entries data) i))
		    (data-entry-points-data-entry (data-entry-points data-entry)))
	       (declare (type (simple-array double-float (*)) data-entry-points-data-entry))
	       (loop for j fixnum from 0 to (1- dim)
		   do (when (not (and (data-entry-mask data-entry)
				      (/= (the (signed-byte 32) (aref (data-entry-mask data-entry) j)) 0)))
			(incf (aref compcnt j))
			(when (< (aref data-entry-points-maval j)
				 (aref data-entry-points-data-entry j))
			  (setf (aref data-entry-points-maval j)
			    (aref data-entry-points-data-entry j)))
			(when (> (aref data-entry-points-mival j)
				 (aref data-entry-points-data-entry j))
			  (setf (aref data-entry-points-mival j)
			    (aref data-entry-points-data-entry j)))))))

      (loop for i fixnum from 0 to (1- dim)
	  do (when (= (aref compcnt i) 0)
	       (format t "randinit-codes: warning! component ~d has no data, using 0.0~%" (the fixnum (1+ i)))))
      
      ;; Randomize the vector values 
      (rewind-entries codes)
      
      (loop for dentry in (entries-entries codes)
	  do (loop for i fixnum from 0 to (1- dim)
		 do (let ((data-entry-points-dentry (data-entry-points dentry)))
		      (declare (type (simple-array double-float (*)) data-entry-points-dentry))
		      (if (> (aref compcnt i) 0)
			  (let ((orand (orand)))
			    (declare (type (integer 0 #.most-positive-fixnum) orand))
			    (setf (aref data-entry-points-dentry i) 
			      (+ (aref data-entry-points-mival i)
				 (* (- (aref data-entry-points-maval i)
				       (aref data-entry-points-mival i))
				    (/ (coerce orand 'double-float) 32768.0d0)))))
			(setf (aref data-entry-points-dentry i) 0.0d0)))
		    (clear-entry-labels dentry)))
      codes)))
      

(defun get-mapdistf (topol)
  (declare (optimize (speed 3)))
  (cond ((= topol *topol-rect*)
	 (function rect-dist))
	((= topol *topol-hexa*)
	 (function hexa-dist))
	(t nil)))

(defun get-nadaptf (neigh)
  (declare (optimize (speed 3)))
  (cond ((= neigh *neigh-gaussian*)
	 (function gaussian-adapt))
	((= neigh *neigh-bubble*)
	 (function bubble-adapt))
	(t nil)))


;; set-som-params - set functions needed by the SOM algorithm in the
;; teach-params structure 
(defun set-som-params (params)
  (unless (teach-params-mapdist params)
    (setf (teach-params-mapdist params)
      (get-mapdistf (teach-params-topol params))))
  (unless (teach-params-neigh-adapt params)
    (setf (teach-params-neigh-adapt params)
      (get-nadaptf (teach-params-neigh params)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rect-dist (bx by tx ty arr)
  (declare (optimize (speed 3)))
  (declare (type (integer 0 #.most-positive-fixnum) bx by tx ty))
  (declare (type (simple-array double-float (1)) arr))
  (let ((ret 0.0d0)
	(diff 0.0d0))
    (declare (type double-float diff)
	     (type (double-float 0.0d0 *) ret))
    ;(setq diff (float (- bx tx)))
    (setq diff (coerce (- bx tx) 'double-float))
    (setq ret (* diff diff))
    ;(setq diff (float (- by ty)))
    (setq diff (coerce (- by ty) 'double-float))
    (setq ret (+ ret (* diff diff)))
    (setf (aref arr 0) (sqrt ret))
    nil))

(defun hexa-dist (bx by tx ty arr)
  (declare (optimize (speed 3)))
  (declare (type (integer 0 #.most-positive-fixnum) bx by tx ty))
  (declare (type (simple-array double-float (1)) arr))
  (declare (:explain :boxing))
  (let ((ret 0.0d0)
	(diff 0.0d0)
	(by-ty 0))
    (declare (type double-float diff)
	     (type (double-float 0.0d0 *) ret)
	     (type (integer 0 #.most-positive-fixnum) by-ty))
    ;(setq diff (float (- bx tx)))
    (setq diff (coerce (- bx tx) 'double-float))
    (if (> by ty)
	(setq by-ty (- by ty))
      (setq by-ty (- ty by)))
    (if (/= (rem by-ty 2) 0)
	(if (= (rem by 2) 0)
	    (setq diff (- diff 0.5))
	  (setq diff (+ diff 0.5))))
    (setq ret (* diff diff))
    ;(setq diff (float (- by ty)))
    (setq diff (coerce (- by ty) 'double-float))
    (setq ret (+ ret (* 0.75d0 diff diff)))
    (setf (aref arr 0) (sqrt ret))
    nil
    ))


;; Adaptation function for bubble-neighborhood 
(defun bubble-adapt (teach-params sample bx by radius alpha)
  (declare (optimize (speed 3))
	   (type fixnum bx by)
	   (type (simple-array double-float (1)) radius alpha)
	   (type data-entry sample)
	   (type teach-params teach-params))
  (let* ((codes (teach-params-codes teach-params))
	 (dist (teach-params-mapdist teach-params))
	 ;(adapt (teach-params-vector-adapt teach-params))
	 (xdim (entries-xdim codes))
	 (dim (entries-dimension codes))
	 (alp (aref alpha 0)))
    (declare (type entries codes)
	     (type (integer 0 #.most-positive-fixnum) xdim ydim dim)
	     (type double-float alp))
    (rewind-entries codes)
    (let ((index 0)
	  (tx 0)
	  (ty 0)
	  (dd (make-array 1 :element-type 'double-float :initial-element 0.0d0)))
      (declare (type fixnum tx ty index)
	       (type (simple-array double-float (1)) dd))
      (loop for codes-data-entry in (entries-entries codes)
	  do (setq tx (rem index xdim)
		   ty (floor index xdim))
	     (funcall dist bx by tx ty dd)
	     (when (<= (aref dd 0) (aref radius 0))
;	       (funcall adapt codes-data-entry sample
;			(entries-dimension codes) alp))
	       ;; adapt-vector
	       (let ((current 0.0d0)
		     (sample-data-entry-mask (data-entry-mask sample))
		     (sample-data-entry-points (data-entry-points sample))
		     (codes-data-entry-points (data-entry-points codes-data-entry)))
		 (declare (type double-float new current)
			  (type (simple-array double-float (*)) sample-data-entry-points
				codes-data-entry-points)
			  (type (simple-array (signed-byte 32) (*)) sample-data-entry-mask))
		 (if sample-data-entry-mask
		     (dotimes (i dim)
		       (declare (type fixnum i))
		       (unless (/= (the (signed-byte 32) (aref sample-data-entry-mask i)) 0)
			 ;; ignore vector components that have 1 in mask 
			 (setq current (aref codes-data-entry-points i))
			 (setf (aref codes-data-entry-points i)
			   (+ current
			      (* alp
				 (- (aref sample-data-entry-points i)
				    current))))))
		   (dotimes (i dim)
		     (declare (type fixnum i))
		     (setq current (aref codes-data-entry-points i))
		     (setf (aref codes-data-entry-points i)
		       (+ current
			  (* alp
			     (- (aref sample-data-entry-points i)
				current)))))))
	       )
	     (incf index)))))



;; Adaptation function for gaussian neighbourhood 
(defun gaussian-adapt (teach-params sample bx by radius alpha)
  (declare (optimize (speed 3))
	   (type fixnum bx by)
	   (type (simple-array double-float (1)) radius alpha)
	   (type data-entry sample)
	   (type teach-params teach-params))
  (let* ((codes (teach-params-codes teach-params))
	 (dist (teach-params-mapdist teach-params))
	 ;(adapt (teach-params-vector-adapt teach-params))
	 (xdim (entries-xdim codes))
	 (dim (entries-dimension codes)))


    (declare (type entries codes)
	     (type (integer 0 #.most-positive-fixnum) xdim ydim dim))

    (rewind-entries codes)
    (let ((index 0)
	  (tx 0)
	  (ty 0)
	  (alp 0.0d0)
	  (w 0.0d0)
	  (dd (make-array 1 :element-type 'double-float :initial-element 0.0d0)))

      (declare (type double-float w alp)
	       (type (integer 0 #.most-positive-fixnum) tx ty index)
	       (type (simple-array double-float (1)) dd))
      
      ;; dd array is for reducing boxing.
      (dolist (codes-data-entry (entries-entries codes))
	(declare (type data-entry codes-data-entry))
	(setq ty (floor index xdim))
	(setq tx (rem index xdim))
	
	(funcall dist bx by tx ty dd)
	(setq w (/ (aref dd 0) (aref radius 0)))
	(setq alp (* (aref alpha 0)
		     (handler-case
			 (exp (the double-float
				(* -0.5d0 w w)))
		       (floating-point-underflow () 0.0d0))))
	
;;;	(funcall adapt codes-data-entry sample 
;;;		 dim
;;;		 alp)
	;; adapt-vector
	(let ((current 0.0d0)
	      (sample-data-entry-mask (data-entry-mask sample))
	      (sample-data-entry-points (data-entry-points sample))
	      (codes-data-entry-points (data-entry-points codes-data-entry)))
	  (declare (type double-float new current)
		   (type (simple-array double-float (*)) sample-data-entry-points
			 codes-data-entry-points))
	  (if sample-data-entry-mask
              (let ((sample-data-entry-mask sample-data-entry-mask))
                                        ; redundant definition for type declaration
                (declare (type (simple-array (signed-byte 32) (*)) sample-data-entry-mask))
                (dotimes (i dim)
                  (declare (type fixnum i))
                  (unless (/= (the (signed-byte 32) (aref sample-data-entry-mask i)) 0)
                    ;; ignore vector components that have 1 in mask 
                    (setq current (aref codes-data-entry-points i))
                    (setf (aref codes-data-entry-points i)
                      (+ current
                         (* alp
                            (- (aref sample-data-entry-points i)
                               current)))))))
	    (dotimes (i dim)
	      (declare (type fixnum i))
	      (setq current (aref codes-data-entry-points i))
	      (setf (aref codes-data-entry-points i)
		(+ current
		   (* alp
		      (- (aref sample-data-entry-points i)
			 current))))
              ))
	  )


	(incf index)
	))))

	       
					    
	       
;; som-training - train a SOM. Radius of the neighborhood decreases 
;; linearly from the initial value to one and the learning parameter 
;; decreases linearly from its initial value to zero. 
(defun som-training (teach-params)
  (declare (optimize (speed 3))
	   (type teach-params teach-params))
  (set-som-params teach-params)

  (let ((adapt (teach-params-neigh-adapt teach-params))
	(data (teach-params-data teach-params))
	(codes (teach-params-codes teach-params))
	(length (teach-params-length teach-params))
	(alpha (teach-params-alpha teach-params))
	(radius (teach-params-radius teach-params))
	(get-alpha (teach-params-alpha-func teach-params))
	(find-winner (teach-params-winner teach-params))
	(bxind 0)
	(byind 0)
	(win-info (make-instance 'winner-info)))
    (declare (type double-float alpha radius))
    (declare (type fixnum bxind byind length alpha-type))
    (declare (type winner-info win-info))
    (rewind-entries data)
    (let ((dim (entries-dimension codes))
	  (xdim (entries-xdim codes)))
      (declare (type fixnum dim xdim))
      (when (/= dim (the fixnum (entries-dimension data)))
	(error "code dimension (~d) != data dimension (~d)~%"
	       dim (entries-dimension data)))

      (setf (teach-params-start-time teach-params)
	(get-universal-time))
      (let* ((index 0)
	     (sample-list (entries-entries data))
	     (sample-len (length sample-list))
	     (alpha-arr (make-array 1 :element-type 'double-float))
	     (new-alpha-arr (make-array 1 :element-type 'double-float))
	     (trad-arr (make-array 1 :element-type 'double-float)))
	(declare (type cons sample-list)
		 (type fixnum index sample-len)
		 (type (simple-array double-float (1)) alpha-arr new-alpha-arr trad-arr))
	;; alpha-arr, new-alpha-arr, trad-arr are for reducing boxing.
	(setf (aref alpha-arr 0) alpha)

	(dotimes (le length)
	  (declare (type fixnum le))
;	  (when (= (mod le 1000) 0)
;	    (format t "training ~d times~%" le))
	  (when (>= index sample-len)
	    (rewind-entries data)
	    (setq index 0))
	  (let ((sample (nth index sample-list)))
	    (declare (type data-entry sample))
	    (let ((weight (data-entry-weight sample)))
	      (declare (type double-float weight))
	      
	      ;; Radius decreases linearly to one 
	      (setf (aref trad-arr 0)
		(+ 1.0d0
		   (* (- radius 1.0d0) 
		      (/ (coerce (- length le) 'double-float)
			 (coerce length 'double-float)))))
	      
	      
	      (funcall get-alpha le length alpha-arr new-alpha-arr)
	      

              
	      ;; If the sample is weighted, we
	      ;; modify the training rate so that we achieve the same effect as
	      ;; repeating the sample 'weighxt' times 
	      (when (and (> weight 0.0d0)
			 (use-weights -1))
		(setf (aref new-alpha-arr 0)
		  (the double-float
		    (- 1.0d0 (the double-float (expt (the double-float (- 1.0d0 (aref new-alpha-arr 0)))
                                                     weight)))))

		)

	      ;; Find the best match 
	      ;; If fixed point and is allowed then use that value 
	      (if (and (data-entry-fixed sample)
		       (use-fixed -1))
		  (progn
		    (setq bxind 
		      (slot-value (data-entry-fixed sample) 'xfix))
		    (setq byind 
		      (slot-value (data-entry-fixed sample) 'yfix)))
		(when (funcall find-winner codes sample win-info 1)
		  (setf byind (floor (winner-info-index win-info) xdim))
		  (setf bxind (rem (winner-info-index win-info) xdim))
		  ))
	      
	      ;; Adapt the units 
	      (funcall adapt teach-params sample bxind byind trad-arr new-alpha-arr)
	      ;; todo
	      ;; save snapshot when needed
		   
	      (incf index)
	      ))))
      (setf (teach-params-end-time teach-params)
	(get-universal-time))
      codes)))


#+ignore
(defun foo ()
  (declare (:explain :boxing))
  (let ((x 2.0d0))
    (declare (type (double-float 0.0d0 *) x))
    ;(setq x (sqrt x))
    (setq x (exp x))
    nil))

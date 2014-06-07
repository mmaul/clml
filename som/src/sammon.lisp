;;; -*- lisp -*-
;;; $Id: sammon.cl,v 1.1.2.13 2006/11/28 06:09:46 tada Exp $

(in-package :som)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (proclaim '(optimize (speed 3))))

(defparameter *magic* 0.2d0)

;; ps-string-filter: escape ps special characters in string. Returns string
;; containing the escaped string. 
(defun ps-string-filter (text)
  (if text
      (with-output-to-string (out)
	(with-input-from-string (in text)
	  (let ((char))
	    (handler-case 
		(loop 
		    do (setq char (read-char in))
		       (cond ((or (char= char #\()
				  (char= char #\))
				  (char= char #\\))
			      (format out "\\~a" char))
			     (t
			      (format out "~a" char))))
	      (end-of-file () char)))))
    ""))


;; remove duplicates in (entries-entries codes)
;; return the number of removed element 
(defun remove-identicals (codes)
  (let ((distance-func (function vector-dist-euc))
	(dim (entries-dimension codes))
	(before-len (length (entries-entries codes)))
	(after-len))
    ;; Compute the mutual distances between entries 
    ;; Remove the identical entries from the list 
    (setf (entries-entries codes)
      (remove-duplicates (entries-entries codes)
			 :test #'(lambda (x y)
				   (let ((arr (make-array 1 :element-type 'double-float)))
				     (declare (type (SIMPLE-ARRAY DOUBLE-FLOAT (1)) arr))
				     (funcall distance-func x y dim arr 0)
				     ;(= (funcall distance-func x y dim) 0.0)
				     (= (aref arr 0) 0.0d0)
				     ))
			 :from-end t))
    (setq after-len
      (length (entries-entries codes)))
    (setf (entries-num-entries codes) after-len)
    ;; return the number of removed element 
    (- before-len after-len)))



(defun sammon-iterate (g-data codes length &key (debug nil))
  (declare (optimize (speed 3))
	   (type entries codes)
	   (type fixnum length))
  (let ((distance-func (function vector-dist-euc))
	(dim (entries-dimension codes))
	(noc (entries-num-entries codes)))
    (declare (type (integer 0 #.most-positive-fixnum) dim noc))
    (format t "~d entries in codebook~%" noc)
    
    (let ((x (make-array noc :element-type 'double-float :initial-element 0.0d0))
	  (y (make-array noc :element-type 'double-float :initial-element 0.0d0))
	  (xu (make-array noc :element-type 'double-float :initial-element 0.0d0))
	  (yu (make-array noc :element-type 'double-float :initial-element 0.0d0))
	  (dd (make-array (floor (the fixnum
				   (* noc 
				      (the fixnum (1- noc)))) 2) :element-type 'double-float :initial-element 0.0d0))
	  (mutual 0))
      (declare (type fixnum mutual)
	       (type (simple-array double-float (*)) x y xu yu dd))

      
      ;; Initialize the tables
      (dotimes (i noc)
	(let ((orand (orand)))
	  (declare (type (integer 0 #.most-positive-fixnum) orand))
	  (setf (aref x i)
	    (coerce (/ (rem orand noc) noc) 'double-float))
	  (setf (aref y i)
	    (coerce (/ (the fixnum i) noc) 'double-float))))
      

      ;; Compute the mutual distances between entries 
      (rewind-entries codes)

      (let ((codes-entries-list (entries-entries codes)))
	(dotimes (i noc)
	  (dotimes (j i)
;;;	    (setf (aref dd mutual)
;;;	      (funcall distance-func
;;;		       (nth i codes-entries-list)
;;;		       (nth j codes-entries-list)
;;;		       dim))
	    (funcall distance-func
		     (nth i codes-entries-list)
		     (nth j codes-entries-list)
		     dim dd mutual)
	    
	    (when (= (aref dd mutual) 0.0d0)
	      (error "Identical entries in codebook"))
	    (incf mutual))))
      

      #+ignore
      (when debug ; for debug
	(with-open-file (stream "sammon-x-y.dat" :direction :output
			 :if-exists :supersede)
	  (loop for valx double-float across x 
	      for valy double-float across y
	      for i fixnum from 0
	    do (format stream "x[~d]=~f y[~d]=~f~%" i valx i valy)))
	(with-open-file (stream "sammon-dd.dat" :direction :output
			 :if-exists :supersede)
	  (loop for val double-float across dd
	      for i fixnum from 0
	      do (format stream "dd[~d]=~f~%" i val))))
      
      ;; Iterate
      (let ((e1x 0.0d0)
	    (e1y 0.0d0)
	    (e2x 0.0d0)
	    (e2y 0.0d0)
	    (xd 0.0d0)
	    (yd 0.0d0)
	    (dpj 0.0d0)
	    (dt 0.0d0)
	    (dq 0.0d0)
	    (dr 0.0d0)
	    (xx 0.0d0)
	    (yy 0.0d0)
	    (xj 0.0d0)
	    (yj 0.0d0)
	    (xd2 0.0d0)
	    (yd2 0.0d0)
	    (w 0.0d0)
	    (newent)
	    (magic *magic*))
	(declare (type double-float e1x e1y e2x e2y xd yd dt dq dpj dr xx yy xj yj w xd2 yd2 magic))
	
	(dotimes (i length)
	  (dotimes (j noc)
	    (declare (type fixnum i j))
	    (setq e1x 0.0d0
		  e1y 0.0d0
		  e2x 0.0d0
		  e2y 0.0d0
		  w 0.0d0
		  xj (aref x j)
		  yj (aref y j))
	    (dotimes (k noc)
	      (declare (type fixnum k))
	      (unless (= j k)
		(setq xd (- xj (aref x k))
		      yd (- yj (aref y k)))
		(setq xd2 (* xd xd)
		      yd2 (* yd yd))
		(setq dpj
		  (sqrt (the (double-float 0.0d0 *) (+ xd2 yd2))))
		

		;; Calculate derivatives 
		(if (> k j)
		    (setq dt
		      (aref dd (the (integer 0 #.most-positive-fixnum)
				 (+ (floor (the fixnum
					     (* k 
						(the fixnum (1- k)))) 2)
				    j))))
		  (setq dt
		    (aref dd (the (integer 0 #.most-positive-fixnum)
			       (+ (floor (the fixnum
					   (* j 
					      (the fixnum (1- j)))) 2)
				  k)))))
		
		  
		(setq dq (- dt dpj)
		      dr (* dt dpj))


;;;		(setq e1x (+ e1x (/ (* xd dq) dr)))
;;;		(setq e1y (+ e1y (/ (* yd dq) dr)))
;;;		(setq e2x (+ e2x
;;;			     (/ (- dq (/ (* xd xd (+ 1.0 (/ dq dpj))) dpj)) dr)))
;;;		(setq e2y (+ e2y
;;;			     (/ (- dq (/ (* yd yd (+ 1.0 (/ dq dpj))) dpj)) dr)))
		(setq w (+ 1.0d0 (/ dq dpj)))

		(setq e1x (+ e1x (/ (* xd dq) dr))
		      e1y (+ e1y (/ (* yd dq) dr))
		      e2x (+ e2x
			     (/ (- dq (/ (* xd2 w) dpj)) dr))
		      e2y (+ e2y
			     (/ (- dq (/ (* yd2 w) dpj)) dr)))
		))

	    ;; Correction
	    (setf (aref xu j)
	      (+ xj
		 (/ (the double-float (* magic e1x))
		    (the double-float (fabs e2x)))))

	    (setf (aref yu j)
	      (+ yj
		 (/ (the double-float (* magic e1y))
		    (the double-float (fabs e2y)))))

	    )
	  ;; Move the center of mass to the center of picture 
	  (setq xx 0.0d0
		yy 0.0d0)
	  (dotimes (j noc)
	    (setq xx (+ xx (aref xu j))
		  yy (+ yy (aref yu j))))
	  (setq xx (/ xx noc)
		yy (/ yy noc))
	  #+ignore
	  (when debug                   ; for debug
	    (with-open-file (stream "sammon-xxyy.dat" :direction :output :if-exists :append :if-does-not-exist :create)
	      (format stream "~d xx=~f yy=~f~%" i xx yy)))
	  
	  (dotimes (j noc)
	    (setf (aref x j) (- (aref xu j) xx))
	    (setf (aref y j) (- (aref yu j) yy)))
          
          
          
	  ;; omitting error for speed.
	  )

	(setq newent (alloc-entries g-data))
	(setf (entries-dimension newent) 2)
	(setf (entries-xdim newent) (entries-xdim codes))
	(setf (entries-ydim newent) (entries-ydim codes))
	(setf (entries-topol newent) (entries-topol codes))
	(setf (entries-neigh newent) (entries-neigh codes))
	
	;; Copy the data to return variable 
	(rewind-entries codes)


	(setf (entries-entries newent)
	  (loop for i fixnum from 0 to (1- noc)
	      collect (let* ((entr1 (init-entry newent nil))
			     (data-entry-points (data-entry-points entr1)))
			(declare (type (SIMPLE-ARRAY DOUBLE-FLOAT (*)) data-entry-points))
			(setf (aref data-entry-points 0)
			  (aref x i))
			(setf (aref data-entry-points 1)
			  (aref y i))
			(copy-entry-labels entr1 
					   (nth i (entries-entries codes)))
			entr1)))
	(setf (entries-num-entries newent) noc)
	
	newent))))



;; ps - t(ps) nil (eps)
;; rem - the number of removed data-entry
;; return sammon file path(ps) and gif label positions
(defun save-entries-in-eps (gdata spics filename ps rem &key (gif-link-width 60) (gif-link-height 20))
  (let ((*read-default-float-format* 'double-float))
    (let ((xmi *flt-max*)
          (xma *flt-min*)
          (ymi *flt-max*)
          (yma *flt-min*)
          (str)
          (frac)
          (label)
          (gif-label-pos-list))
      
      (if ps
          (setq str (format nil "~a.ps" filename))
        (setq str (format nil "~a.eps" filename)))
      (with-open-file (stream str :direction :output :if-exists :supersede)
        (rewind-entries spics)
        (loop for data-entry in (entries-entries spics)
            do (when (> xmi (aref (data-entry-points data-entry) 0))
                 (setq xmi (aref (data-entry-points data-entry) 0)))
               (when (< xma (aref (data-entry-points data-entry) 0))
                 (setq xma (aref (data-entry-points data-entry) 0)))
               (when (> ymi (aref (data-entry-points data-entry) 1))
                 (setq ymi (aref (data-entry-points data-entry) 1)))
               (when (< yma (aref (data-entry-points data-entry) 1))
                 (setq yma (aref (data-entry-points data-entry) 1))))
        (format t "xma-xmi ~f yma-ymi ~f~%"
                (- xma xmi)
                (- yma ymi))
        (if (> (* 1.45 (- xma xmi)) (- yma ymi))
                                        ;(setq frac (/ 510.0 (- xma xmi)))
            (setq frac (/ 510.0d0 (- xma xmi)))
                                        ;(setq frac (/ 760.0 (- yma ymi)))
          (setq frac (/ 740.0d0 (- yma ymi)))
          )
        (rewind-entries spics)
        (loop for data-entry in (entries-entries spics)
            do (setf (aref (data-entry-points data-entry) 0)
                 (- (aref (data-entry-points data-entry) 0) xmi))
               (setf (aref (data-entry-points data-entry) 1)
                 (- (aref (data-entry-points data-entry) 1) ymi)))
        (if ps
            ;; print ps header 
            (progn
              (format stream "%!PS-Adobe-2.0 EPSF-2.0~%")
              (format stream "%%Title: ~a~%%%Creator: sammon~%" "undefined")
              (format stream "%%Pages: 1~%%%EndComments~%")
              (format stream "40 40 translate~%")
              (format stream "/gscale ~f def~%" frac)
              (format stream "gscale dup scale~%"))
          ;; print eps header 
          (progn
            (format stream "%!PS-Adobe-2.0 EPSF-2.0~%")
            (format stream "%%Title: ~a~%%%Creator: sammon~%" "undefined")
            (format stream "%%BoundingBox: 0 0 ~f ~f~%" 
                    (- xma xmi)
                    (- yma ymi))
            (format stream "%%Pages: 0~%%%EndComments~%")
            (format stream "/gscale ~f def~%" frac)))
      
      
        (format stream "/Helvetica findfont 12 gscale div scalefont setfont~%")
        (format stream "/radius ~f def~%" (/ 2.0 frac))
        (format stream "/LN~%")
        (format stream "{newpath~%")
        (format stream "radius 0 360 arc fill~%")
        (format stream "} def~%")
        (format stream "/LP~%")
        (format stream "{dup stringwidth pop~%")
        (format stream "-2 div 0 rmoveto show} def~%")
        (format stream "~f setlinewidth~%" (/ 0.2 frac))
        (format stream "0 setgray~%")
      
        
        (rewind-entries spics)
        (loop for data-entry in (entries-entries spics)
            do (format stream "~f ~f LN~%"
                       (aref (data-entry-points data-entry) 0)
                       (aref (data-entry-points data-entry) 1))
               (when (/= (setq label (get-entry-label data-entry))
                         *label-empty*)
                 (format stream "~f ~f moveto~%"
                         (aref (data-entry-points data-entry) 0)
                         (aref (data-entry-points data-entry) 1))
                 
		
                 ;; multi label output 
                 (format stream "(")
                 (loop for i from 0 to (1- (data-entry-num-labs data-entry))
                     do (setq label (get-entry-labels data-entry i))
                        (format stream "~a " (ps-string-filter 
                                              (find-conv-to-lab label gdata)))
                        ;; calculate gif label position
                        #+ignore
                        (let* ((gif-center-x (floor (+ (* frac (aref (data-entry-points data-entry) 0)) 40)))
                               (gif-center-y (- *gif-page-height* (floor (+ (* frac (aref (data-entry-points data-entry) 1)) 40))))
                               (link-button-left (max 0 (- gif-center-x (floor gif-link-width 2))))
                               (link-button-right (min (+ link-button-left gif-link-width)
                                                       *gif-page-width*))
                               (link-button-top (max 0 (- gif-center-y (floor gif-link-height 2))))
                               (link-button-buttom (min (+ link-button-top gif-link-height)
                                                        *gif-page-height*)))
                          
                          (push 
                           (list 
                            (format nil "~d,~d,~d,~d"
                                    link-button-left
                                    link-button-top
                                    link-button-right
                                    link-button-buttom)
                            (find-conv-to-lab label gdata))
                           gif-label-pos-list))
                        )
                 (format stream ") LP~%")
                 ))
        (let ((xc 0))
          (when (= rem 0)
            (progn
              (rewind-entries spics)
              (loop for data-entry in (entries-entries spics)
                  do (if (= xc 0)
                         (progn
                           (format stream "newpath~%")
                           (format stream "~f ~f moveto~%"
                                   (aref (data-entry-points data-entry) 0)
                                   (aref (data-entry-points data-entry) 1)))
                       (progn
                         (format stream "~f ~f lineto~%"
                                 (aref (data-entry-points data-entry) 0)
                                 (aref (data-entry-points data-entry) 1))
                         (when (= xc (1- (entries-xdim spics)))
                           (format stream "stroke~%"))))
                     (incf xc)
                     (when (= xc (entries-xdim spics))
                       (setq xc 0))))))
        (let ((xc)
              (ec))
          (loop for yc from 0 to (1- (entries-xdim spics))
              do (setq xc 0
                       ec 0)
                 (rewind-entries spics)
                 (loop for data-entry in (entries-entries spics)
                     do (cond ((and (= ec 0)
                                    (= xc yc))
                               (format stream "newpath~%")
                               (format stream "~f ~f moveto~%"
                                       (aref (data-entry-points data-entry) 0)
                                       (aref (data-entry-points data-entry) 1)))
                              ((= xc yc)
                               (format stream "~f ~f lineto~%"
                                       (aref (data-entry-points data-entry) 0)
                                       (aref (data-entry-points data-entry) 1))
                               (when (= ec (1- (entries-ydim spics)))
                                 (format stream "stroke~%"))))
                        (incf xc)
                        (when (= xc (entries-xdim spics))
                          (setq xc 0)
                          (incf ec))
                        )
                 )
          (when ps
            (format stream "showpage~%"))
	
          #+ignore
          (setq gif-label-pos-list (reverse gif-label-pos-list))
          ;; return sammon file path(ps) and gif label positions((pos-list . label) ...)
          (values (pathname stream) gif-label-pos-list))))))


;; return gif pathname and gif-label-pos-list
;; gif-label-pos-list (("gif-left-upper-x,gif-left-upper-y,gif-right-lower-x,gif-right-lower-y" label) ...)
;; som-target-hash (key id, value som-target instance)
(defun sammon (g-data length randomize directory &key (debug nil))
  (declare (ignore directory))
  (let ((eps)
	(ps t)
	(out-code-file #+allegro (sys:make-temp-file-name)
                       #+sbcl (swank-backend::temp-file-name)
                       #+lispworks (hcl:make-temp-file))
	(removed)
	(spics)
	(sammon-file-path)
	(gif-label-pos-list))
    (label-not-needed 1)
    (let ((codes (gdata-codes g-data)))
      (setf (slot-value (entries-entries_flags codes) 'totlen-known) t)
      (init-random randomize)
      ;; Remove identical entries from the codebook 
      (setq removed
	(remove-identicals codes))

      (when debug
	(save-entries-wcomment (gdata-codes g-data) "sammon-before.dat"))
      
      (setq spics
	(sammon-iterate g-data codes length :debug debug))
      
      (when debug
	(save-entries-wcomment (gdata-codes g-data) "sammon-after.dat"))
      
      ;; Don't draw lines when the file is not a map file 
      (when (and (/= (the fixnum (entries-topol codes))
		     (the fixnum *topol-rect*))
		 (/= (the fixnum (entries-topol codes))
		     (the fixnum *topol-hexa*)))
	(setq removed 1))
      
      (when debug 
	(save-entries-wcomment spics "spics.dat"))
      
      (when (or ps eps)
	(multiple-value-setq (sammon-file-path gif-label-pos-list)
	  (save-entries-in-eps g-data spics out-code-file ps removed)))
      (close-entries spics)
      
      ;(let ((gif-pathname (convert-ps-to-gif sammon-file-path)))
      ;(values gif-pathname gif-label-pos-list))

      (values sammon-file-path gif-label-pos-list))))


;; publish som gif.
;; return published path
#+ignore
(defun som-gif-publish (gif-pathname gif-label-pos-list in-data-file)
  (let* ((publish-path (format nil "~a/~a.html" 
			       *sammon-map-prefix*
			       (pathname-name gif-pathname)))
	 (gif-publish-path (format nil "~a/gif/~a.~a" 
				   *sammon-map-prefix*
				   (pathname-name gif-pathname)
				   (pathname-type gif-pathname)))
	 (url-hash (make-hash-table :test #'eql)))
    (with-open-file (stream in-data-file)
      (loop 
	  do (let ((line (read-line stream)))
	       (if (eql (elt line 0) #\#)
		   (let ((id-url-list (cdr (delimited-string-to-list line #\Space))))
		     (setf (gethash (parse-integer (first id-url-list)) url-hash)
		       (second id-url-list)))
		 (return)))))
  
    (publish-file :path gif-publish-path :file gif-pathname
		  :content-type "image/gif")
    
    (publish :path publish-path
	     :content-type "text/html"
	     :function #'(lambda (req ent)
			   (with-http-response (req ent)
			     (with-http-body (req ent)
			       (html 
				(:html 
				 (:head (:title))
				 (:body ((:img :src (namestring gif-publish-path)
					       
					       :alt (namestring gif-pathname)
					       :usemap "#sommap"
					       :border "0"
					       :width (write-to-string *gif-page-width*)
					       :height (write-to-string *gif-page-height*)
					       ))
					((:map :name "sommap")
					 (loop for item in gif-label-pos-list
					     do 
					       (let* ((coords (car item))
						      (label (second item))
						      (id (get-som-target-id label))
						      (url (gethash id url-hash)))
						 (html 
						  ((:area :shape "rect"
							  :coords coords
							  :href url
							  :alt url
							  ))))))
					)))))))
    
    publish-path
    ))


#+ignore
(defun som-gif-publish (gif-pathname gif-label-pos-list som-target-hash)
  (let* ((publish-path (format nil "~a/~a.html" 
			       *sammon-map-prefix*
			       (pathname-name gif-pathname)))
	 (gif-publish-path (format nil "~a/gif/~a.~a" 
				   *sammon-map-prefix*
				   (pathname-name gif-pathname)
				   (pathname-type gif-pathname))))
    
    (publish-file :path gif-publish-path :file gif-pathname
		  :content-type "image/gif")
    (publish :path publish-path
	     :content-type "text/html"
	     :function #'(lambda (req ent)
			   (with-http-response (req ent)
			     (with-http-body (req ent)
			       (html 
				(:html 
				 (:head (:title))
				 (:body ((:img :src (namestring gif-publish-path)
					       
					       :alt (namestring gif-pathname)
					       :usemap "#sommap"
					       :border "0"
					       :width (write-to-string *gif-page-width*)
					       :height (write-to-string *gif-page-height*)
					       ))
					((:map :name "sommap")
					 (loop for item in gif-label-pos-list
					     do 
					       (let* ((coords (car item))
						      (label (second item))
						      (id (get-som-target-id label))
						      (url (som-target-url-from-id som-target-hash id)))
						 (html 
						  ((:area :shape "rect"
							  :coords coords
							  :href url
							  :alt url
							  ))))))
					)))))))
    
    publish-path
    ))


(defun convert-ps-to-gif (ps-namestring)
  (let* ((ps-path (parse-namestring ps-namestring))
	 (gif-path (make-pathname 
		    :host (pathname-host ps-path)
		    :directory (pathname-directory ps-path)
		    :name (pathname-name ps-path)
		    :type "gif")))
    (run-shell-command (format nil "~a -page ~dx~d ~a ~a"
			       *convert-command*
			       *gif-page-width*
			       *gif-page-height*
			       ps-path gif-path)
		       :wait t)
    gif-path))




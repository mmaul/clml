;;; -*- lisp -*-
;;; $Id: lvq_pak.cl,v 1.1.2.12 2007/01/12 11:25:48 tada Exp $

(in-package :som)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (proclaim '(optimize (speed 3))))

(defclass fixpoint ()
  ((xfix :accessor fixpoint-xfix :initform nil)
   (yfix :accessor fixpoint-yfix :initform nil)))

;; index to label data base 
(defclass data-entry_lab ()
  ((label :initform nil)
   (label-array :initform nil)))
  

;; every entry (either input data or code vector) is stored
;; in linked lists consisting of following objects 

(defclass data-entry ()
  ((points :accessor data-entry-points :initform nil)
   (mask :accessor data-entry-mask :initform nil
	 :documentation "if mask is present, ignore vector components marked 
		      with nonzero")
   (lab :accessor data-entry-lab :initform (make-instance 'data-entry_lab)
	:documentation "index to label data base")
   (num-labs :accessor data-entry-num-labs :initform 0)
   (weight :accessor data-entry-weight :type double-float :initform 0d0)
   (fixed :accessor data-entry-fixed :initform nil)))
   
(defclass entries_flags ()
  ((loadmode :initarg :loadmode :initform t
	     :documentation "read whole file into memory or read entries from file when needed")
   (totlen-known :initarg :totlen-known :initform nil
		 :documentation "true when total length of file is known")
   (random-order :initarg :random-order :initform nil
		 :documentation "use data vectors in random order. only available in *loadmode-all*")
   (skip-empty :initarg :skip-empty :initform t
	       :documentation "Ignore vectors that have all components masked off (default)")
   (labels-needed :initarg :labels-needed :initform t
		  :documentation "Set if labels are required")
   ))
    
(defclass entries ()
  ((dimension :accessor entries-dimension :initarg :dimension :initform 0)
   (topol :accessor entries-topol :initarg :topol :initform *topol-unknown*)
   (neigh :accessor entries-neigh :initarg :neigh :initform *neigh-unknown*)
   (xdim :accessor entries-xdim :initarg :xdim :initform 0)
   (ydim :accessor entries-ydim :initarg :ydim :initform 0)
   (current :accessor entries-current :initarg :current :initform 0
	    :documentation "index of current data-entry inside data-entries")
   (entries :accessor entries-entries :initarg :entries :initform nil
	    :type cons
	    :documentation "list of data-entries")
   (num-loaded :accessor entries-num-loaded :initarg :num-loaded :initform nil
	       :documentation "number of lines loaded in entries list")
   (num-entries :accessor entries-num-entries :initarg :num-entries :initform nil
		:documentation "number of entries in the data set if known")
   (flags :accessor entries-entries_flags :initarg :entries_flags 
	  :initform (make-instance 'entries_flags
		      :loadmode *loadmode-all*
		      :totlen-known nil
		      :random-order nil
		      :skip-empty t
		      :labels-needed t))
   (lap :accessor entries-lap :initarg :lap :initform 0
	:documentation "how many times have all samples been used")
   (file-info :accessor entries-file-info :initarg :file-info :initform nil)
   (buffer :accessor entries-buffer :initarg :buffer :initform 0
	   :documentation "how many lines to read from file at one time")
   (parent-gdata :accessor entries-parent-gdata :initarg :parent-gdata :initform nil)
   ))

;; get_type_by_id - search typelist for id 
(defun get-type-by-id (typelist id)
  (second (find id typelist :key #'car)))

;; get_type_by_str - search typelist for string 
(defun get-type-by-str (typelist str)
  (first (find str typelist :test #'string= :key #'second)))

(defparameter *rnd-max* 32767)
(defvar *next* 1)

;; May define my own random generator 
(defun osrand (i)
  (setq *next* i))

;; init_random - initialize own random number generator with seed. 
;;   If seed is 0, uses default
(defun init-random (seed)
  (if (> seed 0)
      (osrand seed)
    (osrand (get-universal-time))))



(defclass teach-params ()
    ((topol :accessor teach-params-topol :initarg :topol)
     (neigh :accessor teach-params-neigh :initarg :neigh)
     (alpha-type :accessor teach-params-alpha-type :initarg :alpha-type)
     (mapdist :accessor teach-params-mapdist :initarg :mapdist
	      :documentation "calculates distance between two units")
     (dist :accessor teach-params-dist :initarg :dist
	   :documentation "calculates distance between two vectors")
     (neigh-adapt :accessor teach-params-neigh-adapt :initarg :neigh-adapt
		  :documentation "adapts weights")
     (vector-adapt :accessor teach-params-vector-adapt :initarg :vector-adapt
		   :documentation "adapt one vector")
     (winner :accessor teach-params-winner :initarg :winner
	     :documentation "function to find winner")
     (alpha-func :accessor teach-params-alpha-func :initarg :alpha-func)
     (radius :accessor teach-params-radius :initarg :radius
	     :documentation "initial radius (for SOM)")
     (alpha :accessor teach-params-alpha :initarg :alpha
	    :documentation "initial alpha value")
     (length :accessor teach-params-length :initarg :length
	     :documentation "length of training")
     (knn :accessor teach-params-knn :initarg :knn
	  :documentation "nearest neighbours")
     (codes :accessor teach-params-codes :initarg :codes)
     (data :accessor teach-params-data :initarg :data)
     (snapshot :accessor teach-params-snapshot :initarg :snapshot)
     (start-time :accessor teach-params-start-time :initarg :start-time)
     (end-time :accessor teach-params-end-time :initarg :end-time)))

(defun use-fixed (level)
  (let ((fixed-level 0))
    (when (>= level 0)
      (setq fixed-level level))
    fixed-level))

(defun use-weights (level)
  (let ((weights-level 0))
    (when (>= level 0)
      (setq weights-level level))
    weights-level))

(defclass winner-info ()
  ((index :accessor winner-info-index :initarg :index)
   (winner :accessor winner-info-winner :initarg :winner)
   (diff :accessor winner-info-diff :initarg :diff :type double-float)))

;; find-winner-euc - finds the winning entry (1 nearest neighbour) in
;; codebook using euclidean distance. Information about the winning
;; entry is saved in the winner_info structure. Return 1 (the number
;; of neighbours) when successful and nil when winner could not be found
;; (for example, all components of data vector have been masked off) 
;; sample - sample data entry
;; win - winner-info instance
(defun find-winner-euc (codes sample win knn)
  (declare (ignore knn)
	   (optimize (speed 3))
	   (type entries codes)
	   (type data-entry sample)
	   (type winner-info win))
  (let ((dim (entries-dimension codes))
	(diffsf *flt-max*))
    (declare (type double-float diffsf)
	     (type fixnum dim))
    (setf (winner-info-index win) -1
	  (winner-info-winner win) nil
	  (winner-info-diff win) -1.0d0)
    
    ;; Go through all code vectors 
    (rewind-entries codes)
    
    (let ((index -1)
	  (sample-data-entry-points (data-entry-points sample))
	  (sample-data-entry-mask (data-entry-mask sample)))
      (declare (type fixnum index)
	       (type (simple-array double-float (*)) sample-data-entry-points))
      (dolist (codes-data-entry (entries-entries codes))
	(incf index)
        (let ((difference 0.0d0)
	      (masked 0)
	      (diff 0.0d0)
	      (codes-data-entry-points (data-entry-points codes-data-entry)))
	  (declare (type double-float difference diff))
	  (declare (type (signed-byte 32) masked))
	  (declare (type (simple-array double-float (*))
			 codes-data-entry-points))
	  (if sample-data-entry-mask
              (let ((sample-data-entry-mask sample-data-entry-mask))
                                        ; rebound for type declaration
                (declare (type (simple-array (signed-byte 32) (*)) sample-data-entry-mask))
                (dotimes (i dim)
                  (if (/= (the (signed-byte 32) (aref sample-data-entry-mask i)) 0)
                      (incf masked) ;; ignore vector components that have 1 in mask 
                    (progn
                      (setq diff 
                        (- (aref codes-data-entry-points i)
                           (aref sample-data-entry-points i)))
                      (setq difference
                        (+ difference (* diff diff)))
                      (when (> difference diffsf)
                        (return nil))))
                  (when (= dim masked)
                    ;; can't calculate winner, empty data vector 
                    (return-from find-winner-euc nil))))
	    (dotimes (i dim)
	      (setq diff 
		(- (aref codes-data-entry-points i)
		   (aref sample-data-entry-points i)))
	      (setq difference
		(+ difference (* diff diff)))
	      (when (> difference diffsf)
		(return nil))))
	  (when (< difference diffsf)
	    (setf (winner-info-winner win) codes-data-entry)
	    (setf (winner-info-index win) index)
	    (setf (winner-info-diff win) difference)
	    (setq diffsf difference)
	    )
	  )))
    
    (when (< (the fixnum (winner-info-index win)) 0)
      (error "find-winner-euc: can't find winner~%"))
    
    ;; number of neighbours
    1))

;; vector-dist-euc - compute distance between two vectors is euclidean
;; metric. 
;; arr and index are for not-boxing.
(defun vector-dist-euc (data-entry-1 data-entry-2 dim arr index)
  (declare (optimize (speed 3))
	   (type data-entry data-entry-1 data-entry-2)
	   (type fixnum dim index)
	   (type (simple-array double-float (*)) arr))
  (let ((masked 0)
	(difference 0.0d0)
	(diff 0.0d0))
    (declare (type double-float diff)
	     (type (double-float 0.0d0 *) difference)
	     (type (signed-byte 32) masked))
    (let ((data-entry-1-mask (data-entry-mask data-entry-1))
	  (data-entry-2-mask (data-entry-mask data-entry-2))
	  (data-entry-1-points (data-entry-points data-entry-1))
	  (data-entry-2-points (data-entry-points data-entry-2)))
      (declare (type (simple-array double-float (*)) data-entry-1-points data-entry-2-points))
      (if (not (or data-entry-1-mask data-entry-2-mask))
	  (dotimes (i dim)
	    (setq diff 
	      (- (aref data-entry-1-points i)
		 (aref data-entry-2-points i)))
	    (incf difference (* diff diff)))
	(dotimes (i dim)
	  (if (or (and data-entry-1-mask
                       (let ((data-entry-1-mask data-entry-1-mask))
                                        ; redundant definition for type declaration
                         (declare (type (simple-array (signed-byte 32) (*)) data-entry-1-mask))
                         (/= (aref data-entry-1-mask i) 0)))
		  (and data-entry-2-mask
                       (let ((data-entry-2-mask data-entry-2-mask))
                                        ; redundant definition for type declaration
                         (declare (type (simple-array (signed-byte 32) (*)) data-entry-2-mask))

                         (/= (aref data-entry-2-mask i) 0))))
	      ;; ignore vector components that have 1 in mask 
	      (incf masked)
	    (progn
	      (setq diff 
		(- (aref data-entry-1-points i)
		   (aref data-entry-2-points i)))
	      (incf difference (* diff diff))))))
      (unless (= dim masked)
	(setf (aref arr index) (sqrt difference)))
      nil)))

;; adapt-vector - move a codebook vector towards another vector 
;; codes-data-entry and sample is data-entry object.
(defun adapt-vector (codes-data-entry sample dim alpha)
  (declare (optimize (speed 3))
	   (type data-entry codes-data-entry sample)
	   (type fixnum dim)
	   (type double-float alpha))
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
		 (* alpha (- (aref sample-data-entry-points i)
			     current))))))
      (dotimes (i dim)
	(declare (type fixnum i))
	(setq current (aref codes-data-entry-points i))
	(setf (aref codes-data-entry-points i)
	  (+ current
	     (* alpha (- (aref sample-data-entry-points i)
			 current))))))
    ))


;; linearly decreasing alpha 
(defun linear-alpha (iter length alpha new-alpha)
  (declare (optimize (speed 3))
	   (type fixnum iter length)
	   (type (simple-array double-float (1)) alpha new-alpha))
  (setf (aref new-alpha 0)
    (* (aref alpha 0)
       (/ (coerce (- length iter) 'double-float)
	  (coerce length 'double-float))))
  nil)

(defparameter *inv-alpha-constant* 100.0d0)

(defun inverse-t-alpha (iter length alpha new-alpha)
  (declare (optimize (speed 3))
	   (type fixnum iter length)
	   (type (simple-array double-float (1)) alpha new-alpha))
  (let ((c (/ (coerce length 'double-float) (the double-float *inv-alpha-constant*))))
    (setf (aref new-alpha 0) (/ (* (aref alpha 0) c) (+ c iter))))
  nil)


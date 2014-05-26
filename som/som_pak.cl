;;; -*- lisp -*-
;;; $Id: som_pak.cl,v 1.1.2.8 2006/11/01 04:06:26 tada Exp $

(in-package :som)

(defclass gdata ()
  ((data :accessor gdata-data :initarg :data)
   (codes :accessor gdata-codes :initarg :codes)
   (labels-array :accessor gdata-labels-array
                 :initform (make-array 0 :adjustable t))
   (num-labs :accessor gdata-num-labs :initform 0)
   (lab-array-size :accessor gdata-lab-array-size :initform 0)))


(defun close-gdata (g-data)
  (when (gdata-data g-data)
    (close-entries (gdata-data g-data)))
  (when (gdata-codes g-data)
    (close-entries (gdata-codes g-data)))
  (setf (gdata-data g-data) nil)
  (setf (gdata-codes g-data) nil)
  (setf (gdata-labels-array g-data) (make-array 0 :adjustable t))
  (setf (gdata-num-labs g-data) 0)
  (setf (gdata-lab-array-size g-data) 0))


;; return sammon file path
(defun do-som-by-filename (
			   in-data-file ;; input data
			   s-topol ;; topology type (hexa/rect)
			   s-neigh ;; neighborhood type (bubble/gaussian)
			   xdim ;; x-dimension of output map
			   ydim ;; y-dimension of output map
			   randomize ;; random seed for initialization
			   ;; training parameters 
			   length ;; how many times train for path1
			   ialpha ;; learning rate for path1 x100
			   iradius ;; learning radius for path1 x100
			   ;; visualization parameters
			   num-labels ;; number of labels on same map point 
			   ;; output ps directory
			   directory
			   &key (debug nil)
			   )
  
  
  (let ((alpha (/ ialpha 100.0d0))
	(radius (/ iradius 100.0d0))
	(data)
	(g-data)
        (out-pathname)
        (sammon-pathname)
	(gif-pathname)
	(gif-label-pos-list))
    (format t "in-data-file [~a]~%" in-data-file)
    (format t "s-topol[~a] s-neigh[~a] xdim[~d] ydim[~d] nrand[~d]~%"
	    s-topol s-neigh xdim ydim randomize)
    (format t "num-label[~d]~%" num-labels)
    
    (format t "step 1 : initialization ~%")
    
    (init-random randomize)
    (setq g-data (make-instance 'gdata))
    (setq data (open-entries g-data in-data-file))
    (setf (gdata-data g-data) data)

    (randinit g-data s-topol s-neigh xdim ydim randomize)
    
    (when debug
      (save-entries-wcomment (gdata-data g-data) "data.dat")
      (save-entries-wcomment (gdata-codes g-data) "vsom-before.dat"))
    
    (format t "step 2 : learning ~%")
    (vsom g-data length alpha radius "linear" randomize)
    
    (when debug
      (save-entries-wcomment (gdata-codes g-data) "vsom-after.dat"))

    (format t "step 3 : calibration ~%")
    (vcal g-data num-labels)
    
    (format t "step 4 : labeling ~%")
    (setq out-pathname
      (visual g-data :debug debug :directory directory))

    (format t "step 5 : making sammon map~%")

    (multiple-value-setq (sammon-pathname gif-label-pos-list)
      (sammon g-data *sammon-length* randomize directory :debug debug))
    
    (close-gdata g-data)
    
    (values out-pathname sammon-pathname)))





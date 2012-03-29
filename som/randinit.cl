;;; -*- lisp -*-
;;; $Id: randinit.cl,v 1.1.2.5 2006/10/04 10:32:56 tada Exp $

(in-package :som)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (proclaim '(optimize (speed 3))))


(defun randinit (g-data s-topol s-neigh xdim ydim randomize)
  (let (topol
	neigh
	number-of-codes
	data
	codes)
    
    ;; the topology type of the map 
    (setq topol (topol-type s-topol))
    (when (unknown-topol-p topol)
      (error "Unknown topology type ~a" s-topol))
    
    ;; the neighbourhood type
    (setq neigh (neigh-type s-neigh))
    (when (unknown-neigh-p neigh)
      (error "Unknown neighborhood type ~a" s-neigh))
    (label-not-needed 1)
    
    (setq number-of-codes (* xdim ydim))
    (when (<= number-of-codes 0)
      (error "Dimensions of map (~d ~d) are incorrect" xdim ydim))
    (when (< xdim 0)
      (error "Dimensions of map (~d ~d) are incorrect" xdim ydim))

    (setq data (gdata-data g-data))
    
    (setf (slot-value (entries-entries_flags data) 'loadmode)
      *loadmode-all*)
    
    (init-random randomize)
    
    ;; do initialization 
    (setq codes
      (randinit-codes data topol neigh xdim ydim))
    
    (setf (gdata-data g-data) data)
    (setf (entries-parent-gdata data) g-data)
    (setf (gdata-codes g-data) codes)
    (setf (entries-parent-gdata codes) g-data)
    
    ))
	

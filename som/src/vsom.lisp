;;; -*- lisp -*-
;;; $Id: vsom.cl,v 1.1.2.5 2006/11/28 06:10:06 tada Exp $

(in-package :som)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (proclaim '(optimize (speed 3))))

(defun get-alpha-function (id)
  (eval (third (find id *alpha-list* :key #'car))))
  
(defun vsom (g-data length alpha radius alpha-type randomize)
  (declare (optimize (speed 3)))
  (let ((params (make-instance 'teach-params))
	(fixed 0)
	(weights 0)
	(data)
	(codes))
    
    (setf (teach-params-length params) length)
    (setf (teach-params-alpha params) alpha)
    (setf (teach-params-radius params) radius)
    
    (use-fixed fixed)
    (use-weights weights)
    (label-not-needed 1)
    
    (setq data (gdata-data g-data))
    (unless data
      (error "vsom: wrong input data"))
    (setf (slot-value (entries-entries_flags data) 'totlen-known) t)
    (when randomize
      (setf (slot-value (entries-entries_flags data) 'random-order) t))
    
    (setq codes (gdata-codes g-data))
    (unless codes
      (error "vsom: wrong input codes"))
    (setf (slot-value (entries-entries_flags codes) 'totlen-known) t)
    (setf (entries-lap codes) 0)
    (setf (entries-num-loaded codes)
      (entries-num-entries codes))
    
    (when (< (the fixnum (entries-topol codes)) (the fixnum *topol-hexa*))
      (error "vsom: topol is wrong. ~d~%" (entries-topol codes)))
    
    (when (/= (the fixnum (entries-dimension data)) (the fixnum (entries-dimension codes)))
      (error "vsom: dimension is wrong. data->dimension [~d] codes->dimension [~d]~%" (entries-dimension data) (entries-dimension codes)))
    
    (set-teach-params params codes data)
    (set-som-params params)
    (setf (teach-params-snapshot params) nil)
    
    (init-random randomize)
    (when randomize
      (setf (entries-entries data)
	(randomize-entry-order (entries-entries data)))
      (setf (slot-value (entries-entries_flags data) 'random-order) t))
    
    (unless alpha-type
      (setq alpha-type *alpha-linear*))
    
    (let ((type-id (get-type-by-str *alpha-list* alpha-type)))
      (declare (type fixnum type-id))
      (unless (or type-id (= type-id (the fixnum *alpha-unknown*)))
	(error "Unknown alpha type~a~%" alpha-type))
      (setf (teach-params-alpha-type params) type-id)
      (setf (teach-params-alpha-func params)
	(get-alpha-function type-id)))

    (som-training params)
    
    ))
    
    
      
      

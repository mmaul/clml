;;; -*- lisp -*-
;;; $Id: som_utils.cl,v 1.1.2.1 2007/01/12 11:26:42 tada Exp $

(in-package :som)

(defmacro orand ()
  `(let ((v *next*)
	 (p *rnd-max*))
     (declare (type (integer 0 *) v p))
     (setq v (* v 23))
     (rem 
      (setq *next* 
	(rem v
	     100000001))
      p
      )))

(defmacro fabs (x)
  `(if (> ,x 0.0)
       ,x
     (* -1.0 ,x)))

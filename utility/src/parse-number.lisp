;;;; -*- Mode: Lisp -*-
;;;; Defines functions to parse any number type, without using the reader
;;;; Version: 1.0
;;;; Author: Matthew Danish -- mrd.debian.org
;;;;
;;;; Copyright 2002 Matthew Danish.
;;;; All rights reserved.
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions
;;;; are met:
;;;; 1. Redistributions of source code must retain the above copyright
;;;;    notice, this list of conditions and the following disclaimer.
;;;; 2. Redistributions in binary form must reproduce the above copyright
;;;;    notice, this list of conditions and the following disclaimer in the
;;;;    documentation and/or other materials provided with the distribution.
;;;; 3. Neither the name of the author nor the names of its contributors
;;;;    may be used to endorse or promote products derived from this software
;;;;    without specific prior written permission.
;;;;
;;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
;;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
;;;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;;;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;;; SUCH DAMAGE.

(defpackage #:org.mapcar.parse-number
  (:use #:common-lisp)
  (:nicknames #:parse-number)
  (:export #:parse-number
	   #:parse-real-number
	   #:parse-positive-real-number))

(in-package #:org.mapcar.parse-number)

(define-condition invalid-number (simple-error)
  ((value :reader value
	  :initarg :value
	  :initform nil)
   (reason :reader reason
	   :initarg :reason
	   :initform "Not specified"))
  (:report (lambda (c s)
	     (format s "Invalid number: ~S [Reason: ~A]"
		     (value c) (reason c)))))

(declaim (inline parse-integer-and-places))
(defun parse-integer-and-places (string start end &key (radix 10))
  #+parse-number-optimizations
  (declare (optimize (speed 3) (safety 1))
	   (type simple-base-string string)
	   (type fixnum start end radix))
  (multiple-value-bind (integer end-pos)
      (if (= start end)
	  (values 0 0)
	  (parse-integer string
			 :start start
			 :end end
			 :radix radix))
    (cons integer (- end-pos start))))
		 
(defun parse-integers (string start end splitting-points &key (radix 10))
  #+parse-number-optimizations
  (declare (optimize (speed 3) (safety 1))
	   (type simple-base-string string)
	   (type fixnum start end radix))
  (values-list (loop for left = start then (1+ right)
		     for point in splitting-points
		     for right = point
		     collect (parse-integer-and-places string
						       left
						       right
						       :radix radix)
		     into integers
		     finally (return
			       (nconc integers
				      (list
				       (parse-integer-and-places string
								 left
								 end
								 :radix radix
								 )))))))

(declaim (inline number-value places))
(defun number-value (x) (car x))
(defun places (x) (cdr x))

(declaim (type cons *white-space-characters*))
(defparameter *white-space-characters*
  '(#\Space #\Tab #\Return #\Linefeed))

(declaim (inline white-space-p))
(defun white-space-p (x)
  #+parse-number-optimizations
  (declare (optimize (speed 3) (safety 0))
	   (type character x))
  (and (find x *white-space-characters*) t))

;; Numbers which could've been parsed, but intentionally crippled not to:
;; #xFF.AA
;; #o12e3

;; Numbers which CL doesn't parse, but this does:
;; #10r3.2
;; #2r  11

(defun parse-number (string &key (start 0) (end nil) (radix 10))
  "Given a string, and start, end, and radix parameters, produce a number according to the syntax definitions in the Common Lisp Hyperspec."
  (flet ((invalid-number (reason)
	   (error 'invalid-number
		  :value (subseq string start end)
		  :reason reason)))
    (let ((end (or end (length string))))
      (if (and (eql (char string start) #\#)
	       (member (char string (1+ start)) '(#\C #\c)))
	  (let ((\(-pos (position #\( string :start start :end end))
		(\)-pos (position #\) string :start start :end end)))
	    (when (or (not \(-pos)
		      (not \)-pos)
		      (position #\( string :start (1+ \(-pos) :end end)
		      (position #\) string :start (1+ \)-pos) :end end))
	      (invalid-number "Mismatched/missing parenthesis"))
	    (let ((real-pos (position-if-not #'white-space-p string
					     :start (1+ \(-pos) :end \)-pos)))
	      (unless real-pos
		(invalid-number "Missing real part"))
	      (let ((delimiting-space (position-if #'white-space-p string
						   :start (1+ real-pos)
						   :end \)-pos)))
		(unless delimiting-space
		  (invalid-number "Missing imaginary part"))
		(let ((img-pos (position-if-not #'white-space-p string
						:start (1+ delimiting-space)
						:end \)-pos)))
		  (unless img-pos
		    (invalid-number "Missing imaginary part"))
		  (let ((img-end-pos (position-if #'white-space-p string
						  :start (1+ img-pos)
						  :end \)-pos)))
		    (complex (parse-real-number string
						:start real-pos
						:end delimiting-space
						:radix radix)
			     (parse-real-number string
						:start img-pos
						:end (or img-end-pos
							 \)-pos)
						:radix radix)))))))
	  (parse-real-number string :start start :end end :radix radix)))))

(defun parse-real-number (string &key (start 0) (end nil) (radix 10))
  "Given a string, and start, end, and radix parameters, produce a number according to the syntax definitions in the Common Lisp Hyperspec -- except for complex numbers."
  (let ((end (or end (length string))))
    (case (char string start)
      ((#\-)
       (* -1 (parse-positive-real-number string
					 :start (1+ start)
					 :end end
					 :radix radix)))
      ((#\#)
       (case (char string (1+ start))
	 ((#\x #\X)
	  (parse-real-number string
			     :start (+ start 2)
			     :end end
			     :radix 16))
	 ((#\b #\B)
	  (parse-real-number string
			     :start (+ start 2)
			     :end end
			     :radix 2))
	 ((#\o #\O)
	  (parse-real-number string
			     :start (+ start 2)
			     :end end
			     :radix 8))
	 (t (if (digit-char-p (char string (1+ start)))
		(let ((r-pos (position #\r string
				       :start (1+ start)
				       :end end
				       :key #'char-downcase)))
		  (unless r-pos
		    (error 'invalid-number
			   :value (subseq string start end)
			   :reason "Missing R in #radixR"))
		  (parse-real-number string
				     :start (1+ r-pos)
				     :end end
				     :radix (parse-integer string
							   :start (1+ start)
							   :end r-pos)))))))
      (t (parse-positive-real-number string
				     :start start
				     :end end
				     :radix radix)))))

(defun parse-positive-real-number (string &key (start 0) (end nil) (radix 10))
  "Given a string, and start, end, and radix parameters, produce a number according to the syntax definitions in the Common Lisp Hyperspec -- except for complex numbers and negative numbers."
  (let ((end (or end (length string)))
	(first-char (char string start)))
    (flet ((invalid-number (reason)
	     (error 'invalid-number
		    :value (subseq string start end)
		    :reason reason))
	   (base-for-exponent-marker (char)
	     (case char
	       ((#\d #\D)
		10.0d0)
	       ((#\e #\E)
		10)
	       ((#\s #\S)
		10.0s0)
	       ((#\l #\L)
		10.0l0)
	       ((#\f #\F)
		10.0f0))))
      (case first-char
	((#\-)
	 (invalid-number "Invalid usage of -"))
	((#\/)
	 (invalid-number "/ at beginning of number"))
	((#\d #\D #\e #\E #\l #\L #\f #\F #\s #\S)
	 (when (= radix 10)
	   (invalid-number "Exponent-marker at beginning of number"))))
      (let (/-pos .-pos exp-pos exp-marker)
	(loop for index from start below end
	      for char = (char string index)
	      do (case char
		   ((#\/)
		    (if /-pos
			(invalid-number "Multiple /'s in number")
			(setf /-pos index)))
		   ((#\.)
		    (if .-pos
			(invalid-number "Multiple .'s in number")
			(setf .-pos index)))
		   ((#\e #\E #\f #\F #\s #\S #\l #\L #\d #\D)
		    (when (= radix 10)
		      (when exp-pos
			(invalid-number
			 "Multiple exponent-markers in number"))
		      (setf exp-pos index)
		      (setf exp-marker (char-downcase char)))))
	      when (eql index (1- end))
	      do (case char
		   ((#\/)
		    (invalid-number "/ at end of number"))
		   ((#\d #\D #\e #\E #\s #\S #\l #\L #\f #\F)
		    (when (= radix 10)
		      (invalid-number "Exponent-marker at end of number")))))
	(cond ((and /-pos .-pos)
	       (invalid-number "Both . and / cannot be present simultaneously"))
	      ((and /-pos exp-pos)
	       (invalid-number "Both an exponent-marker and / cannot be present simultaneously"))
	      ((and .-pos exp-pos)
	       (if (< exp-pos .-pos)
		   (invalid-number "Exponent-markers must occur after . in number")
		   (if (/= radix 10)
		       (invalid-number "Only decimal numbers can contain exponent-markers or decimal points")
		       (multiple-value-bind (whole-place frac-place exp-place)
			   (parse-integers string start end
					   (list .-pos exp-pos)
					   :radix radix)
			 (* (+ (number-value whole-place)
			       (/ (number-value frac-place)
				  (expt radix
					(places frac-place))))
			    (expt (base-for-exponent-marker exp-marker)
				  (number-value exp-place)))))))
	      (exp-pos
	       (if (/= radix 10)
		   (invalid-number "Only decimals can contain exponent-markers")
		   (multiple-value-bind (whole-place exp-place)
		       (parse-integers string start end
				       (list exp-pos)
				       :radix radix)
		     (* (number-value whole-place)
			(expt (base-for-exponent-marker exp-marker)
			      (number-value exp-place))))))
	      (/-pos
	       (multiple-value-bind (numerator denominator)
		   (parse-integers string start end
				   (list /-pos)
				   :radix radix)
		 (if (>= (number-value denominator) 0)
		     (/ (number-value numerator)
			(number-value denominator))
		     (invalid-number "Misplaced - sign"))))
	      (.-pos
	       (if (/= radix 10)
		   (invalid-number "Only decimal numbers can contain decimal points")
		   (multiple-value-bind (whole-part frac-part)
		       (parse-integers string start end
				       (list .-pos)
				       :radix radix)
		     (if (>= (number-value frac-part) 0)
			 (+ (number-value whole-part)
			    (/ (number-value frac-part)
			       (expt 10.0 (places frac-part))))
			 (invalid-number "Misplaced - sign")))))
	      (t
	       (values (parse-integer string
				      :start start
				      :end end
				      :radix radix))))))))
		 
(defparameter *test-values*
  '("1" "-1" "1034" "-364" "80/335" "3.5333" "2.4E4" "6.8d3" "#xFF" "#b-1000" "#o-101/75" "13.09s3" "35.66l5" "21.4f2" "#C(1 2)" "#c ( #xF #o-1 ) " "#c(1d1 2s1)" "#16rFF" "#9r10" "#C(#9r44/61 4f4)"))

(defun run-tests ()
  (format t "~&~16@A (~16@A) = ~16A~%~%"
	  "String value" "READ value" "Parsed value")
  (dolist (value *test-values*)
    (format t "~&~16@A (~16@A) = ~16A~%"
	    value
	    (read-from-string value)
	    (parse-number value))))


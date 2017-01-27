;;; -*- lisp -*-
;;; $Id: package.cl,v 1.1.2.6 2006/11/28 23:08:41 tada Exp $

(in-package :cl-user)

(defpackage :clml.som

  (:use
   :common-lisp)
  #+allegro
  (:use
   :excl)
  #-allegro
  (:use split-sequence)
  (:export
   "INIT-NOISEWORD-HASH"
   "EXTRACT-NOUN-WORDS-FOR-SOM"
   "MAKE-SOM-DATAFILE"
   "DO-SOM-BY-FILENAME"
   "*SOM-DIR-LOGICAL-PATH*")
  (:documentation "Self-Organizing-Map
package for self-organizing map

*** sample usage
#+INCLUDE: \"../sample/som.org\" example lisp
")
  )



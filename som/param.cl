;;; -*- lisp -*-
;;; $Id: param.cl,v 1.1.2.4 2006/11/13 05:40:16 tada Exp $

(in-package :som)

(defconstant *loadmode-all* t)
(defconstant *loadmode-buffer* nil)

;; topology types
(defconstant *topol-unknown* 0)
(defconstant *topol-data* 1)
(defconstant *topol-lvq* 2)
(defconstant *topol-hexa* 3)
(defconstant *topol-rect* 4)

(defparameter *topol-list*
    (list `(,*topol-data* "data" nil)
	  `(,*topol-lvq* "lvq" nil)
	  `(,*topol-hexa* "hexa" nil)
	  `(,*topol-rect* "rect" nil)
	  `(,*topol-unknown* "" nil)))

;; neighborhood types
(defconstant *neigh-unknown* 0)
(defconstant *neigh-bubble* 1)
(defconstant *neigh-gaussian* 2)

(defparameter *neigh-list*
    (list `(,*neigh-bubble* "bubble" nil)
	  `(,*neigh-gaussian* "gaussian" nil)
	  `(,*neigh-unknown* "" nil)))

;; alpha function types
(defconstant *alpha-unknown* 0)
(defconstant *alpha-linear* 1)
(defconstant *alpha-inverse-t* 2)

;; alpha functions
(defparameter *alpha-list*
    (list `(,*alpha-linear* "linear" (function linear-alpha))
	  `(,*alpha-inverse-t* "inverse_t" (function inverse-t-alpha))
	  `(,*alpha-unknown* "" nil)))

;; initialize
(defparameter *it-unknown* 0)
(defparameter *it-lin* 1)
(defparameter *it-rand* 2)

;;
(defconstant *label-empty* 0)

(defparameter *flt-min* most-negative-double-float)
(defparameter *flt-max* most-positive-double-float)

;;
(defparameter *masked-value* "x")

;;
(defparameter *atable-increment* 8)
(defparameter *label-array-size* 100)

;;
(defparameter *sammon-length* 100)

;; convert command (you must install ImageMagick package)
(defparameter *convert-command* "/usr/local/bin/convert")
(defparameter *gif-page-width* 612)
(defparameter *gif-page-height* 792)

(defparameter *sammon-map-prefix* "/relation/map")

;; web path
(defparameter *som-gif-base-url* "/som/gif")


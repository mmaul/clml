(in-package :statistics)

(defun histogram (values n)
  (let ((min (reduce #'min values))
	(max (reduce #'max values))
	(len (length values)))
    (loop
       with delta = (/ (- max min) n)
       for lst = values then (set-difference lst current :test #'=)
       for i from 1 to n
       for x = (linear-combination min (/ i n) max)
       for current = (remove-if-not (lambda (y) (< y x)) lst)
       collect (list x (/ (length current) len delta)))))

(defun discrete-histogram (values)
  (let ((min (reduce #'min values))
	(max (reduce #'max values))
	(len (length values)))
    (loop
       for x from min to max
       collect (list x (/ (count x values) len)))))

(defun plot-function (fn min max n)
  (loop
     for i from 0 below n
     for x = (linear-combination min (/ i (1- n)) max)
     collect (list x (funcall fn x))))

(defun plot-discrete-function (fn min max)
  (loop
     for x from min to max
     collect (list x (funcall fn x))))

;;; Usage example:
#+nil
(with-open-file (s "/tmp/random-standard-normal.data"
		   :direction :output :if-exists :supersede)
  (let ((values (rand-n (standard-normal-distribution) 10000)))
    (format s "~{~{~f ~f~}~%~}" (histogram values 50))))

#+nil
(with-open-file (s "/tmp/real-standard-normal.data"
		   :direction :output :if-exists :supersede)
  (format s "~{~{~f ~f~}~%~}"
	  (let ((d (standard-normal-distribution)))
	    (plot-function (lambda (x) (density d x)) -4 4 50))))

;;; .. then in GNUPlot:

;; plot [-4:4] "/tmp/random-standard-normal.data" \
;;      title "Random values (100 ranges from 10^5 data)", \
;;      "/tmp/real-standard-normal.data" with lines title "Exact distribution"

;;; Discrete case:
#+nil
(with-open-file (s "/tmp/random-pascal.data"
		   :direction :output :if-exists :supersede)
	(let ((values (rand-n (pascal-distribution 10 0.6d0) 10000)))
    (format s "~{~{~f ~f~}~%~}" (discrete-histogram values))))

#+nil
(with-open-file (s "/tmp/real-pascal.data"
		   :direction :output :if-exists :supersede)
  (format s "~{~{~f ~f~}~%~}"
	  (let ((d (pascal-distribution 10 0.6d0)))
	    (plot-discrete-function (lambda (x) (mass d x)) 0 20))))

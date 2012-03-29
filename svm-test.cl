;;; -*- mode: lisp; syntax: common-lisp -*-

(in-package :svm)

(defun test (kernel positive-data negative-data filename
             &key (border 5) (scale 10))
  "Requires two-dimensional integer data. Outputs a PPM file."
  (let* ((decision-fn (svm kernel positive-data negative-data)) ; :iterations 1))
         (minx (min (reduce #'min positive-data :key #'first)
                    (reduce #'min negative-data :key #'first)))
         (miny (min (reduce #'min positive-data :key #'second)
                    (reduce #'min negative-data :key #'second)))
         (maxx (max (reduce #'max positive-data :key #'first)
                    (reduce #'max negative-data :key #'first)))
         (maxy (max (reduce #'max positive-data :key #'second)
                    (reduce #'max negative-data :key #'second)))
         (width (* (+ (- maxx minx) (* 2 border)) scale))
         (height (* (+ (- maxy miny) (* 2 border)) scale)))
    (with-open-file (s filename :direction :output :if-exists :supersede)
      (format s "P3~%~d ~d~%255~%" width height)
      (flet ((closep (p q)
               (let ((d (v- p q)))
                 (< (scalar-product d d) 0.5d0))))
        (dotimes (y height)
          (dotimes (x width)
            (let ((pos (list (- (+ minx (/ x scale)) border)
                             (- (+ miny (/ y scale)) border))))
              (cond ((member pos positive-data :test #'closep)
                     (format s "0 255 0~%"))
                    ((member pos negative-data :test #'closep)
                     (format s "255 0 0~%"))
                    ((funcall decision-fn pos)
                     (format s "255 255 255~%"))
                    (t (format s "0 0 0~%"))))))))))

;;; Some sample tests
(defparameter *positive-set*
  '((8 8) (8 20) (8 44) (8 56) (12 32) (16 16) (16 48)
    (24 20) (24 32) (24 44) (28 8) (32 52) (36 16)))
(defparameter *negative-set*
  '((36 24) (36 36) (44 8) (44 44) (44 56)
    (48 16) (48 28) (56 8) (56 44) (56 52)))

#+nil
(test +linear-kernel+ *positive-set* *negative-set* "linear-test.ppm")

#+nil
(test (polynomial-kernel 3 nil) *positive-set* *negative-set* "polynomial-test.ppm")

#+nil
(test (gaussian-kernel 2.5d0) *positive-set* *negative-set* "gaussian-test.ppm")


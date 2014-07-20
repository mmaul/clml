(in-package :clml.pca.examples.pca-bench)

(defvar eyes (pick-and-specialize-data
              (read-data-from-file (clml.utility.data:fetch "https://mmaul.github.io/clml.data/sample/eyes200.sexp"))
              :except '(0 1)
              :data-types (make-list 1680 :initial-element :numeric)))
(setf (dataset-numeric-points eyes)
      (coerce (loop repeat 1680 collect (aref (dataset-points eyes) (random 200))) 'vector))

(time (princomp eyes :method :covariance))
(time (princomp eyes :method :correlation))



(defvar faces (pick-and-specialize-data
               (read-data-from-file (clml.utility.data:fetch "https://mmaul.github.io/clml.data/sample/faces200.sexp"))
               :except '(0 1)
               :data-types (make-list 19600 :initial-element :numeric)))
(setf data
      (coerce
       (loop repeat 5000
             collect (subseq (aref (dataset-points faces) (random 200)) 0 5000))
       'vector))
(setf (dataset-numeric-points faces) data)
(setf (dataset-dimensions faces) (subseq (dataset-dimensions faces) 0 5000))


(time (princomp faces :method :covariance))
(time (princomp faces :method :correlation))



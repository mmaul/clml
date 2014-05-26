
(in-package :som)

(defun test ()
  (declare (optimize (speed 3)))
  (do-som-by-filename 
      "animal.dat"
    "hexa" 
    "gaussian"
    24  ;; x size of map
    16  ;; y size of map
    123 ;; random seed
    10000 ;; total length of study
    5 ;; alpha x 100
    2400 ;;  radius x 100
    10
    "test/"))

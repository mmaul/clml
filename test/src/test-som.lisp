
(in-package :clml.test)

(define-test test-sample-som
  #+ (or lispworks allegro)
  (assert-prints
     (concatenate 'string
       (format nil "in-data-file [~s]~%" (clml.utility.data:fetch "https://mmaul.github.io/clml.data/sample/som/animal.dat"))
       (format nil "s-topol[hexa] s-neigh[gaussian] xdim[24] ydim[16] nrand[123]~%")
       (format nil "num-label[10]~%")
       (format nil "step 1 : initialization ~%")
       (format nil "step 2 : learning ~%")
       (format nil "step 3 : calibration ~%")
       (format nil "step 4 : labeling ~%")
       (format nil "step 5 : making sammon map~%")
       (format nil "384 entries in codebook~%")
       (format nil "xma-xmi 3.074987831736982 yma-ymi 2.129596273225805~%"))
     (do-som-by-filename (clml.utility.data:fetch "https://mmaul.github.io/clml.data/sample/som/animal.dat") "hexa" "gaussian"
                         24 16 123 10000 5 2400 10
       '(:absolute #+unix "tmp" #+mswindows "temp")))
  #+ (or sbcl ccl)
  (assert-true (string= (concatenate 'string
                           (format nil "~%in-data-file [~s]~%" (clml.utility.data:fetch "https://mmaul.github.io/clml.data/sample/som/animal.dat"))
                           (format nil "s-topol[hexa] s-neigh[gaussian] xdim[24] ydim[16] nrand[123]~%")
                           (format nil "num-label[10]~%")
                           (format nil "step 1 : initialization ~%")
                           (format nil "step 2 : learning ~%")
                           (format nil "step 3 : calibration ~%")
                           (format nil "step 4 : labeling ~%")
                           (format nil "step 5 : making sammon map~%")
                           (format nil "384 entries in codebook~%")
                           (format nil "xma-xmi 3.0798202668990733 yma-ymi 2.1309072126802384~%"))

                        (with-output-to-string (*standard-output*)  (do-som-by-filename (clml.utility.data:fetch "https://mmaul.github.io/clml.data/sample/som/animal.dat") "hexa" "gaussian"
                                                   24 16 123 10000 5 2400 10
                                                   '(:absolute #+unix "tmp" #+mswindows "temp")))))
  
  (multiple-value-bind (out-pathname ps-pathname)
        (do-som-by-filename (clml.utility.data:fetch "https://mmaul.github.io/clml.data/sample/som/animal.dat") "hexa" "gaussian"
                            24 16 123 10000 5 2400 10
                            '(:absolute #+unix "tmp" #+mswindows "temp"))
      (assert-true (probe-file out-pathname))
      (assert-true (probe-file ps-pathname))
      (with-open-file (stream out-pathname)
        (assert-eql (read stream) 3)
        (read-line stream)
        (assert-eql (read stream) (coerce  0.0 'single-float))
        (assert-eql (read stream) (coerce 15.0 'single-float))
        (read-line stream)
        (assert-eql (read stream) (coerce 0.0 'single-float))
        (assert-eql (read stream) (coerce 0.0 'single-float))
        (read-line stream)
        (assert-eql (read stream) (coerce  23.0 'single-float))
        (assert-eql (read stream) (coerce 1.0 'single-float))
        (read-line stream)
        (assert-eql (read stream) (coerce  21.0 'single-float))
        (assert-eql (read stream) (coerce  0.0 'single-float))
        (read-line stream)
        (assert-eql (read stream) (coerce 9.0 'single-float))
        (assert-eql (read stream) (coerce 15.0 'single-float))
        (read-line stream)
        (assert-eql (read stream) (coerce 0.0 'single-float))
        (assert-eql (read stream) (coerce 10.0 'single-float))
        (read-line stream)
        (assert-eql (read stream) (coerce 23.0 'single-float))
        (assert-eql (read stream) (coerce 15.0 'single-float))
        (read-line stream)
        (assert-eql (read stream) (coerce 17.0 'single-float))
        (assert-eql (read stream) (coerce 15.0 'single-float))
        (read-line stream)
        (assert-eql (read stream) (coerce 12.0 'single-float))
        (assert-eql (read stream) (coerce 0.0 'single-float)))))

(in-package :clml.pca)
(defun foo (in-pgm out-pgm eye-pgm &key width height (h-upper-ratio 0.35) (w-left-ratio 0.5))
  (let* ((width-left (floor (* width w-left-ratio)))
         (width-right (- width width-left))
         (height-upper (floor (* height h-upper-ratio)))
         (height-lower (- height height-upper))
         (center (get-face-center eye-pgm))
         (x-range (cons (- (car center) width-left)
                        (1- (+ (car center) width-right))))
         (y-range (cons (- (cdr center) height-upper)
                        (1- (+ (cdr center) height-lower)))))
    (when (>= 0 (car x-range))
      (setq x-range (cons 1 (- (cdr x-range) (1- (car x-range))))))
    (when (>= (cdr x-range) 384)
      (setq x-range (cons (- (car x-range) (- (cdr x-range) 383)) 383)))
    (when (>= 0 (car y-range))
      (setq y-range (cons 1 (- (cdr y-range) (1- (car y-range))))))
    (when (>= (cdr y-range) 286)
      (setq y-range (cons (- (car y-range) (- (cdr y-range) 285)) 285)))

    (with-open-file (in in-pgm :element-type '(unsigned-byte 8))
      (with-open-file (out out-pgm :direction :output
                       :if-exists :supersede :if-does-not-exist :create)
        (format out "P2~%~D ~D~%255~%"
                (if x-range (1+ (abs (- (car x-range) (cdr x-range)))) 384)
                (if y-range (1+ (abs (- (car y-range) (cdr y-range)))) 286))
        (loop repeat 4 do (read-byte in))
        (loop for i from 0
            for num = (read-byte in nil nil)
            as (x y) = (multiple-value-bind (yy xx) (floor i 384)
                         `(,(1+ xx) ,(1+ yy)))
            while num
            do (when (and (or (null x-range)
                              (<= (car x-range) x (cdr x-range)))
                          (or (null y-range)
                              (<= (car y-range) y (cdr y-range))))
                 (format out "~D~%" num)))))))

(loop with c = 0
    with files = (directory (excl:pathname-as-directory (clml.utility.data:fetch "https://mmaul.github.io/clml.data/sample/faces-org")))
    for file = (second files)           ; pgm file
    while (< c 200)
    do (when (string-equal "pgm" (pathname-type file))
         (incf c)
         (let ((eye-pgm (make-pathname
                         :directory (pathname-directory file)
                         :name (pathname-name file)
                         :type "eye"
                         :device (pathname-device file))))
           (setq files (remove file files :test #'excl::pathname-equalp)
                 files (remove eye-pgm files :test #'excl::pathname-equalp))
           #+ignore(foo file (clml.utility.data:fetch (format nil "https://mmaul.github.io/clml.data/sample/faces/~A.pgm" c)) eye-pgm
                        :width 140 :height 140)
           (foo file (clml.utility.data:fetch
                                                    (format nil
                                                            "https://mmaul.github.io/clml.data/sample/faces/deye-~A.pgm" c)) eye-pgm
                :width 115 :height 26 :h-upper-ratio 0.5 :w-left-ratio 0.5))))

(defun bar (pgm-file)
  (with-open-file (in pgm-file)
    (read-line in)
    (let* ((w-h (excl:delimited-string-to-list (read-line in nil nil) " "))
           (width (read-from-string (first w-h)))
           (height (read-from-string (second w-h)))
           (v (make-dvec (* width height))))
      (read-line in)
      (loop for i below (* width height)
          for num = (read-from-string (read-line in nil nil))
          while num
          do (setf (aref v i) (dfloat num))
          finally (return v)))))

(defun make-face-dataset (&key fname)
  (let* ((n 200)
         (dat (make-array n))
         cols)
    (loop for c below n
        as file = (if fname (asdf:system-relative-pathname
                             'clml (format nil  "https://mmaul.github.io/clml.data/sample/faces/~A-~A.pgm" fname (1+ c)))
                      (clml.utility.data:fetch (format nil
                                                                   "https://mmaul.github.io/clml.data/sample/faces/~A.pgm" (1+ c))))
        do (setf (aref dat c) (bar file))
           (push (pathname-name file) cols))
    (setq cols (reverse cols))
    (with-open-file (out (if fname (asdf:system-relative-pathname
                                    'clml (format nil
                                                  "https://mmaul.github.io/clml.data/sample/~As~A.sexp" fname n))
                           (asdf:system-relative-pathname
                            'clml (format nil
                                          "https://mmaul.github.io/clml.data/sample/faces~A.sexp" n)))
                     :direction :output :if-exists :supersede)
      (write
       (append `(("id" "personID" ,@(loop for i from 1 to (length (aref dat 0))
                                        collect (format nil "p~A" i))))
               (loop for vec across dat
                   for col in cols
                   for pid in '("kevin" "kevin" "kevin" "kevin" "kevin" "kevin" "kevin" "kevin"
                                "kevin" "kevin" "arthur" "kevin" "kevin" "kevin" "kevin" "kevin"
                                "igor" "igor" "kevin" "max" "max" "max" "max" "max" "max" "max" "max"
                                "max" "max" "max" "max" "max" "max" "max" "max" "max" "max" "max"
                                "max" "max" "max" "max" "max" "max" "max" "max" "kevin" "max" "kevin"
                                "kevin" "maria" "maria" "maria" "maria" "maria" "maria" "maria"
                                "maria" "maria" "maria" "maria" "maria" "maria" "maria" "maria"
                                "maria" "maria" "maria" "maria" "maria" "maria" "maria" "maria"
                                "maria" "maria" "maria" "mary" "mary" "mary" "mary" "mary" "mary"
                                "mary" "mary" "mary" "mary" "mary" "mary" "mary" "mary" "scott"
                                "scott" "scott" "scott" "scott" "scott" "scott" "scott" "arthur"
                                "arthur" "arthur" "arthur" "arthur" "arthur" "arthur" "arthur"
                                "arthur" "arthur" "arthur" "arthur" "arthur" "arthur" "arthur"
                                "arthur" "arthur" "david" "david" "david" "david" "david" "david"
                                "david" "david" "david" "david" "david" "david" "david" "david"
                                "david" "david" "david" "david" "david" "david" "david" "david"
                                "david" "david" "david" "david" "david" "david" "david" "david"
                                "david" "kevin" "scott" "scott" "scott" "scott" "scott" "scott"
                                "scott" "scott" "scott" "scott" "scott" "scott" "scott" "scott"
                                "scott" "scott" "scott" "scott" "scott" "scott" "scott" "scott"
                                "scott" "kevin" "kevin" "kevin" "kevin" "kevin" "kevin" "kevin"
                                "kevin" "kevin" "kevin" "sam" "sam" "sam" "sam" "sam" "sam" "sam"
                                "sam" "sam" "sam" "sam" "sam" "sam" "sam" "sam" "sam" "sam" "sam"
                                "sam" "sam")
                   collect `(,col ,pid ,@(coerce vec 'list))))
       :stream out))))

;; test eigen-face
(let ((eyes
       (normalize-faces
        (pick-and-specialize-data
         (read-data-from-file (clml.utility.data:fetch "https://mmaul.github.io/clml.data/sample/deyes200.sexp"))
         :data-types (append (make-list 2 :initial-element :category)
                             (make-list 2990 :initial-element :numeric))))))
  (loop for dim-thld in '(5 10 15 20 25 30 35)
      collect
        (cons dim-thld
              (loop repeat 10
                  collect
                    (multiple-value-bind (learn-eyes estimate-eyes)
                        (progn (print "divide data")
                               (divide-dataset eyes :divide-ratio '(1 1) :random t))
                      (multiple-value-bind (pca-result pca-model)
                          (progn (print "princomp")
                                 (time (sub-princomp (divide-dataset learn-eyes :except '(0 1))
                                                     :method :covariance
                                                     :dimension-thld dim-thld)))
                        (let* ((estimator
                                (progn (print "make estimator")
                                       (make-face-estimator
                                        learn-eyes :pca-result pca-result :pca-model pca-model
                                        :method :eigenface :bagging 20)))
                               (ested
                                (progn (print "estimation")
                                       (face-estimate (divide-dataset
                                                       estimate-eyes :except '(0 1))
                                                      estimator))))
                          (face-hitting-ratio ested estimate-eyes)))))) into hitting-ratios-list
      finally (loop for (dim-thld . hitting-ratios) in hitting-ratios-list
                  do (format t "~&次元数: ~A, 正解率(min max mean): ~,2F ~,2F ~,2F~%"
                             dim-thld (apply #'min hitting-ratios) (apply #'max hitting-ratios)
                             (/ (apply #'+ hitting-ratios) (length hitting-ratios))))))

;;test subspace
(let ((eyes (pick-and-specialize-data
             (read-data-from-file (clml.utility.data:fetch "https://mmaul.github.io/clml.data/sample/eyes200.sexp") :external-format :shiftjis)
             :data-types (append (make-list 2 :initial-element :category)
                                 (make-list 1680 :initial-element :numeric)))))
  (loop for dim-thld in '(5 10 15 20)
      collect
        (cons dim-thld
              (loop repeat 10
                  collect
                    (multiple-value-bind (learn-eyes estimate-eyes)
                        (progn (print "divide data")
                               (divide-dataset eyes :divide-ratio '(1 1) :random t))
                      (let* ((estimator
                              (progn (print "make estimator")
                                     (make-face-estimator learn-eyes :dimension-thld dim-thld
                                                          :pca-method :covariance
                                                          ; :correlation
                                                          :method :subspace)))
                             (ested
                              (progn (print "estimation")
                                     (face-estimate (divide-dataset
                                                     estimate-eyes :except '(0 1))
                                                    estimator))))
                        (face-hitting-ratio ested estimate-eyes))))) into hitting-ratios-list
      finally (loop for (dim-thld . hitting-ratios) in hitting-ratios-list
                  do (format t "~&次元数: ~A, 正解率(min max mean): ~,3F ~,3F ~,3F~%"
                             dim-thld (apply #'min hitting-ratios) (apply #'max hitting-ratios)
                             (/ (apply #'+ hitting-ratios) (length hitting-ratios))))))

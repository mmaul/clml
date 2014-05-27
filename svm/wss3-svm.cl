;;;Support Vector Machine Package using SMO-type algorithm
;;;Abe Yusuke,Jianshi Huang. 2010 June
;;;Reference: Working Set Selection Using Second Order Information for Training SVM.
;;;Chih-Jen Lin. Department of Computer Science. National Taiwan University. 
;;;Joint work with Rong-En Fan and Pai-Hsuen Chen.


(defpackage :svm.wss3
  (:use :cl
	:hjs.util.meta
	:hjs.util.vector
	:hjs.learn.read-data
        :hjs.util.matrix)
  (:import-from :decision-tree
		#:sum-up)
  (:export #:make-svm-learner
	   #:load-svm-learner
	   #:make-linear-kernel
	   #:make-rbf-kernel
	   #:make-polynomial-kernel
	   #:make-one-class-svm-kernel
	   #:svm-validation
	   ))

(in-package svm.wss3)

;; (declaim (optimize speed (safety 0) (debug 1)))

(defparameter *eps* 1d-3)
(defparameter *tau* 1d-12)
(defparameter *training-size* 0)
(defparameter *label-index* 0)
(defparameter *alpha-array* (make-array 0 :element-type 'double-float))
(defparameter *gradient-array* (make-array 0 :element-type 'double-float))
(defparameter *kernel-function-result* (make-array 1 :element-type 'double-float :initial-element 0d0))
(defparameter *kernel-cache* nil)
(defparameter *kernel-vec-d* (make-dvec 0))
(defparameter *iteration* 0)

(declaim (type double-float *eps* *tau*)
         (type fixnum *training-size* *label-index* *iteration*)
         (type dvec *alpha-array* *gradient-array* *kernel-vec-d*)
         (type (simple-array double-float (1)) *kernel-function-result*)
         ;; (type (or null cache) *kernel-cache*) ; cache is not declared yet
         )

(declaim (inline eta eta-cached sign update-gradient select-i select-j)
         (notinline get-cached-values))

;;;;
(defstruct kernel-function
  name
  scalar
  vectorized)

(defmacro call-kernel-function-uncached (kernel-function point1 point2)
  `(progn
     (funcall (the function (kernel-function-scalar ,kernel-function)) ,point1 ,point2)
     (the double-float
       (aref *kernel-function-result* 0))))

(defmacro call-kernel-function-vectorized-uncached (kernel-function point1 point2s result &optional start end)
  `(progn
     (funcall (the function (kernel-function-vectorized ,kernel-function)) ,point1 ,point2s ,result ,start ,end)))

(defmacro call-kernel-function (kernel-function point1 point2)
  `(call-kernel-function-uncached ,kernel-function ,point1 ,point2))

(defmacro call-kernel-function-vectorized (kernel-function point1 point2s result &optional start end)
  `(call-kernel-function-vectorized-uncached ,kernel-function ,point1 ,point2s ,result ,start ,end))


(defmacro define-kernel-function ((point1-var point2-var &optional (name :unknown)) &body body)
  (check-type point1-var symbol)
  (check-type point2-var symbol)
  (let ((point2-vec-var (intern (concatenate 'string (string point2-var) "-ARRAY"))))
    (with-unique-names (result i start end)
      `(make-kernel-function
        :name ,name
        :scalar
        (lambda (,point1-var ,point2-var)
          (declare (type dvec ,point1-var ,point2-var)
                   (optimize speed (safety 0)))
          (let ((,result (locally ,@body)))
            (declare (type double-float ,result))
            (setf (aref *kernel-function-result* 0) ,result)
            nil))
        :vectorized
        (lambda (,point1-var ,point2-vec-var ,result &optional ,start ,end)
          (declare (type dvec ,point1-var ,result)
                   (type (simple-array dvec (*)) ,point2-vec-var)
                   (optimize speed (safety 0))
                   (type (or null array-index) ,start ,end))
          (assert (<= (length ,point2-vec-var) (length ,result)))
          (loop for ,i of-type array-index from (or ,start 0) below (or ,end (length ,point2-vec-var))
                for ,point2-var of-type dvec = (aref ,point2-vec-var ,i)
                do
             (setf (aref ,result ,i) (locally ,@body))
                finally
             (return ,result)))))))

#| e.g.

(define-kernel-function (z-i z-j :linear)
  (loop
    for k of-type array-index below (1- (length z-i))
    sum (* (aref z-i k) (aref z-j k))
      into result of-type double-float
    finally (return result)))

(defun make-rbf-kernel (&key gamma)
  (declare (type double-float gamma))
  (assert (> gamma 0.0d0))
  (define-kernel-function (z-i z-j :rbf)
    (loop
      for k of-type array-index below (1- (length z-i))
      sum (expt (- (aref z-i k) (aref z-j k)) 2)
        into result of-type double-float
      finally (return (d-exp (* (- gamma) result))))))

|#

;;;; a circular list
(defconstant +double-float-in-bytes+ 8)

(defstruct head
  prev
  next
  data                          ; data[0, len) is cached in this entry
  (len 0 :type fixnum))

(defstruct (cache (:constructor %make-cache (total size heads lru-head)))
  (total 0 :type fixnum)
  (size #.(* 100 1024 1024) :type fixnum) ; size of free space (bytes)
  (heads #() :type (simple-array head (*)))
  (lru-head (make-head) :type head))

(defun make-cache (total size)
  (let* ((heads (coerce (loop repeat total collect (make-head)) 'vector))
         (lru-head (make-head))
         (size (max (/ size +double-float-in-bytes+) (* 2 total))))
    (setf (head-next lru-head) lru-head)
    (setf (head-prev lru-head) lru-head)
    (%make-cache total size heads lru-head)))

;;
(defmacro swap (a b)
  ;; `(psetf ,a ,b ,b ,a)
  (with-unique-names (va vb)
    `(let ((,va ,a)
           (,vb ,b))
       (setf ,a ,vb)
       (setf ,b ,va))))

;;
(declaim (type (function (cache head) cache) lru-delete lru-insert)
         (type (function (cache simple-vector array-index array-index) dvec) get-cached-values)
         (inline lru-delete lru-insert))

(locally (declare (optimize speed (safety 0)))
  (defun lru-delete (cache head)
    (declare (ignorable cache)
             (type cache cache)
             (type head head))
    (let ((next (head-next head))
          (prev (head-prev head)))
      (setf (head-next prev) next
            (head-prev next) prev))
    cache)

  (defun lru-insert (cache head)
    (declare (type cache cache)
             (type head head))
    (with-slots (lru-head) cache
      (let ((old-last (head-prev lru-head)))
        (setf (head-next head) lru-head
              (head-prev head) old-last
              (head-next old-last) head
              (head-prev lru-head) head)))
    cache)

  (defun get-cached-values (cache training-vector index len kernel-function)
    (declare (type cache cache)
             (type array-index index)
             (type fixnum len)
             (type simple-vector training-vector)
             (type kernel-function kernel-function))
    (with-slots (heads lru-head size) cache
      (declare (type (simple-array head (*)) heads)
               (type head lru-head)
               (type fixnum size))
      (let* ((h (aref heads index))
             (h-len (head-len (the head h)))
             (more (- len (the fixnum h-len))))
        (declare (type head h)
                 (type fixnum h-len more))
        (when (not (zerop h-len))
          (lru-delete cache h))
        (when (> more 0)
          ;; free old space
          (let (biggest-data-vec
                (biggest-data-vec-length 0))
            (loop while (< size more)
                  do
               (let* ((old (head-next lru-head)))
                 (lru-delete cache old)
                 (incf size (head-len old))
                 ;; reuse released data vec
                 (let ((data-size (length (the dvec (head-data old)))))
                   (when (> data-size biggest-data-vec-length)
                     (setf biggest-data-vec (head-data old))
                     (setf biggest-data-vec-length data-size)))
                 (setf (head-data old) nil)
                 (setf (head-len old) 0)))
            ;; allocate new space
            (let ((new-data (if (and biggest-data-vec (>= biggest-data-vec-length len))
                                biggest-data-vec
                                (make-dvec len)))
                  (h-data (head-data h)))
              (when h-data
                (locally
                    (declare (type dvec new-data h-data))
                  (replace new-data h-data)))
              (setf (head-data h) new-data)))
          (decf size more)
          (setf (head-len h) len)
          #+nil
          (loop with data of-type dvec = (head-data h)
                for j of-type array-index from h-len below len
                do
             (setf (aref data j)
                   (call-kernel-function kernel-function (aref training-vector index) (aref training-vector j))))
          (call-kernel-function-vectorized kernel-function (aref training-vector index)
                                           training-vector (head-data h) h-len len))
        (lru-insert cache h)
        (head-data h))))

  ;; (0) h includes neither i nor j: do nothing.
  ;; (1) h includes both i and j: the corresponding data would be swapped.
  ;; (2) h contains i but not j (recall that i < j): the column would be thrown away.
  (defun swap-index (cache i j)
    (declare (type cache cache)
             (type array-index i j))
    ;; 
    (when (= i j)
      (return-from swap-index))
    ;; 
    (with-slots (heads lru-head size) cache
      (declare (type (simple-array head (*)) heads)
               (type head lru-head)
               (type fixnum size))
      (let* ((head-i (aref heads i))
             (head-j (aref heads j)))
        (declare (type head head-i head-j))
        (unless (zerop (head-len head-i))
          (lru-delete cache head-i))
        (unless (zerop (head-len head-j))
          (lru-delete cache head-j))
        (swap (head-data head-i) (head-data head-j))
        (swap (head-len head-i) (head-len head-j))
        (unless (zerop (head-len head-i))
          (lru-insert cache head-i))
        (unless (zerop (head-len head-j))
          (lru-insert cache head-j))
        ;;
        (when (> i j)
          (swap i j))
        ;;
        (loop for h of-type head = lru-head then (head-next h)
              until (eq h lru-head)
              when (> (head-len h) i)
                do
             (let ((h-data (head-data h)))
               (declare (type dvec h-data))
               (if (> (head-len h) j)
                   (swap (aref h-data i) (aref h-data j))
                   (progn
                     (lru-delete cache h)
                     (incf size (head-len h))
                     (setf (head-data h) nil)
                     (setf (head-len h) 0))))))))
  )


;;;;
(locally (declare (optimize speed (safety 0)))

  (declaim (ftype (function (simple-vector kernel-function fixnum fixnum) double-float) eta)
           (ftype (function (dvec dvec fixnum fixnum) double-float) eta-cached))

  #+allegro
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (setf (get 'eta 'sys::immed-args-call)
          '((:lisp :lisp :lisp :lisp) double-float)))
  (defun eta (training-vector kernel-function i j)
    (declare (type simple-vector training-vector)
             (type kernel-function kernel-function)
             (type array-index i j)
             (ignorable kernel-function training-vector))
    
    (let ((point-i (svref training-vector i))
          (point-j (svref training-vector j)))
      
      (declare (type dvec point-i point-j))
      
      (+ (call-kernel-function kernel-function point-i point-i)
         (call-kernel-function kernel-function point-j point-j)
         (* -2.0d0 (call-kernel-function kernel-function point-i point-j)))))

  #+allegro
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (setf (get 'eta-cached 'sys::immed-args-call)
          '((:lisp :lisp :lisp :lisp) double-float)))
  (defun eta-cached (kernel-vec-i kernel-vec-d i j)
    (declare (type dvec kernel-vec-i kernel-vec-d)
             (type fixnum i j))
    (the double-float
      (+ (aref kernel-vec-d i)
         (aref kernel-vec-d j)
         (* -2.0d0 (aref kernel-vec-i j)))))


  (defun update-gradient (training-vector kernel-vec-i kernel-vec-j i j old-a-i old-a-j)
    (declare (type simple-vector training-vector) 
             (type double-float old-a-i old-a-j))
    
    (let* ((alpha-array *alpha-array*)
           (gradient-array *gradient-array*)
           (label-index *label-index*)
           (training-size *training-size*))
      (declare (type fixnum i j training-size label-index)
               (type dvec alpha-array gradient-array kernel-vec-i kernel-vec-j))
      (let ((delta-a-i (- (aref alpha-array i) old-a-i))
            (delta-a-j (- (aref alpha-array j) old-a-j)))
        (declare (type double-float delta-a-i delta-a-j))
        (loop
          for k of-type array-index below training-size
          with point-i of-type dvec = (svref training-vector i)
          with point-j of-type dvec = (svref training-vector j)
          with y-i of-type double-float = (aref point-i label-index)
          with y-j of-type double-float = (aref point-j label-index)
          as point-k of-type dvec = (svref training-vector k)
          as y-k of-type double-float = (aref point-k label-index)
          as s-i of-type double-float = (* y-k y-i)
          as s-j of-type double-float = (* y-k y-j)
          do
       (progn
         (incf (aref gradient-array k)
               (+ (* s-i (aref kernel-vec-i k) delta-a-i)
                  (* s-j (aref kernel-vec-j k) delta-a-j))))))
      nil))

  (defun qp-solver (training-vector kernel-function c weight cache-size-in-bytes)
    
    (declare (type simple-vector training-vector)
             (type kernel-function kernel-function)
             (type double-float c weight))

    (setf *iteration* 0)
    (setf *training-size* (length training-vector))
    (setf *label-index* (1- (length (the simple-array (aref training-vector 0)))))
    (setf *alpha-array* (make-array *training-size* :element-type 'double-float :initial-element 0.0d0))
    (setf *gradient-array* (make-array *training-size* :element-type 'double-float :initial-element -1.0d0))
    (setf *kernel-vec-d* (make-dvec *training-size*))
    (setf *kernel-cache* (make-cache *training-size* (or cache-size-in-bytes (* 100 1024 1024))))
    
    (let ((tau *tau*)
          (training-size *training-size*)
          (label-index *label-index*)
          (alpha-array *alpha-array*)
          (gradient-array *gradient-array*)
          (kernel-vec-d *kernel-vec-d*)
          (kernel-cache *kernel-cache*))
      
      (declare (type double-float tau)
               (type fixnum training-size)
               (type array-index label-index)
               (type dvec alpha-array gradient-array kernel-vec-d)
               (type cache kernel-cache))

      (loop for k of-type array-index below training-size
            for point-k = (aref training-vector k)
            do
         (setf (aref kernel-vec-d k) (call-kernel-function kernel-function point-k point-k)))
      
      (loop
        while t
        do (multiple-value-bind (i j) 
               (working-set-selection3 training-vector kernel-function c weight)
             (declare (type fixnum i j))
             
             (incf *iteration*)
             
             (when (= -1 j)
               ;; release memory
               (setf *kernel-cache* nil)
               (return-from qp-solver *alpha-array*))
             
             (let ((y-i (aref (the dvec (svref training-vector i)) label-index))
                   (y-j (aref (the dvec (svref training-vector j)) label-index))
                   (kernel-vec-i (get-cached-values kernel-cache training-vector i training-size kernel-function)))

               (declare (type double-float y-i y-j)
                        (type dvec kernel-vec-i))
               
               (let ((a (eta-cached kernel-vec-i kernel-vec-d i j))
                     (b (- (* y-j (aref gradient-array j))
                           (* y-i (aref gradient-array i)))))
                 
                 (declare (type double-float a b))

                 (when (<= a 0.0d0)
                   (setf a tau))
                 
                 ;;update alpha
                 (let ((old-a-i (aref alpha-array i))
                       (old-a-j (aref alpha-array j)))
                   
                   (declare (type double-float old-a-i old-a-j))
                   
                   (incf (aref alpha-array i) (/ (* y-i b) a))
                   (decf (aref alpha-array j) (/ (* y-j b) a))
                   
                   ;;clipping
                   (let ((diff (- old-a-i old-a-j))
                         (sum (+ old-a-i old-a-j))
                         (new-a-i (aref alpha-array i))
                         (new-a-j (aref alpha-array j))
                         (c-i (if (plusp y-i)
                                  c
                                  (* weight c)))
                         (c-j (if (plusp y-j)
                                  c
                                  (* weight c))))
                     
                     (declare (type double-float diff sum new-a-i new-a-j c-i c-j))
                     
                     (if (/= y-i y-j)
                         (progn
                           (if (> diff 0.0d0)
                               (when (< new-a-j 0.0d0)
                                 (setf (aref alpha-array j) 0.0d0)
                                 (setf (aref alpha-array i) diff))
                               (when (< new-a-i 0.0d0)
                                 (setf (aref alpha-array i) 0.0d0)
                                 (setf (aref alpha-array j) (- diff))))
                           
                           (if (> diff (- c-i c-j))
                               (when (> new-a-i c-i)
                                 (setf (aref alpha-array i) c-i)
                                 (setf (aref alpha-array j) (- c-i diff)))
                               (when (> new-a-j c-j)
                                 (setf (aref alpha-array j) c-j)
                                 (setf (aref alpha-array i) (+ c-j diff)))))
                         (progn
                           (if (> sum c-i)
                               (when (> new-a-i c-i)
                                 (setf (aref alpha-array i) c-i)
                                 (setf (aref alpha-array j) (- sum c-i)))
                               (when (< new-a-j 0.0d0)
                                 (setf (aref alpha-array j) 0.0d0)
                                 (setf (aref alpha-array i) sum)))
                           
                           (if (> sum c-j)
                               (when (> new-a-j c-j)
                                 (setf (aref alpha-array j) c-j)
                                 (setf (aref alpha-array i) (- sum c-j)))
                               (when (< new-a-i 0.0d0)
                                 (setf (aref alpha-array i) 0.0d0)
                                 (setf (aref alpha-array j) sum)))))
                     
                     ;;update gradient
                     (let ((kernel-vec-i (get-cached-values kernel-cache training-vector i training-size kernel-function))
                           (kernel-vec-j (get-cached-values kernel-cache training-vector j training-size kernel-function)))
                       (declare (type dvec kernel-vec-i kernel-vec-j))

                       #+nil
                       (let ((delta-a-i (- (aref alpha-array i) old-a-i))
                             (delta-a-j (- (aref alpha-array j) old-a-j)))
                         (declare (type double-float delta-a-i delta-a-j))
                         (loop
                           for k of-type array-index below training-size
                           as point-k of-type dvec = (svref training-vector k)
                           as y-k of-type double-float = (aref point-k label-index)
                           as s-i of-type double-float = (* y-k y-i)
                           as s-j of-type double-float = (* y-k y-j)
                           ;; branch is slower
                           ;; if (and (/= 0.0d0 delta-a-i) (/= 0.0d0 delta-a-j))
                           do (incf (aref gradient-array k)
                                    (+ (* s-i (aref kernel-vec-i k) delta-a-i)
                                       (* s-j (aref kernel-vec-j k) delta-a-j)))))
                       (update-gradient training-vector kernel-vec-i kernel-vec-j i j old-a-i old-a-j)
                       )))))))))

  (defun select-i (training-vector c)
    (declare (type simple-vector training-vector)
             (type double-float c))
    
    (let ((training-size *training-size*)
          (label-index *label-index*)
          (alpha-array *alpha-array*)
          (gradient-array *gradient-array*)
          (i -1)
          (g-max most-negative-double-float))
      
      (declare (type fixnum i training-size label-index)
               (type dvec alpha-array gradient-array)
               (type double-float g-max))
      (loop
        for k of-type array-index below training-size
        as y-k of-type double-float = (aref (the dvec (svref training-vector k)) label-index)
        as a-k of-type double-float = (aref alpha-array k)
        as g-k of-type double-float = (aref gradient-array k)
        as g-temp of-type double-float = (- (* y-k g-k))
        if (and (>= g-temp g-max)
                (or (and (= y-k 1.0d0) (< a-k c))
                    (and (= y-k -1.0d0) (> a-k 0d0))))
          do (progn
               (setf g-max g-temp)
               (setf i k)))
      (values i g-max)))

  (defun select-j (training-vector kernel-function c weight i g-max)
    (declare (type simple-vector training-vector)
             (type kernel-function kernel-function)
             (type double-float c weight g-max)
             (type array-index i)
             (ignorable kernel-function))
    
    (let* ((training-size *training-size*)
           (label-index *label-index*)
           (alpha-array *alpha-array*)
           (gradient-array *gradient-array*)
           (tau *tau*)
           (j -1)
           (g-min most-positive-double-float)
           (obj-min most-positive-double-float)
           (kernel-cache *kernel-cache*)
           (kernel-vec-d *kernel-vec-d*)
           (kernel-vec-i (get-cached-values kernel-cache training-vector i training-size kernel-function)))
      
      (declare (type fixnum i j training-size label-index)
               (type dvec alpha-array gradient-array kernel-vec-i kernel-vec-d)
               (type double-float tau g-max g-min obj-min)
               (dynamic-extent g-min obj-min))
      
      (loop
        for k of-type array-index below training-size
        as y-k of-type double-float = (aref (the dvec (svref training-vector k)) label-index)
        as a-k of-type double-float = (aref alpha-array k)
        as g-k of-type double-float = (aref gradient-array k)
        as g-temp of-type double-float = (- (* y-k g-k))
        with a of-type double-float = 0.0d0	      
        with b of-type double-float  = 0.0d0
        if (or (and (= y-k 1.0d0) (> a-k 0.0d0))
               (and (= y-k -1.0d0) (< a-k (* weight c))))
          do (setf b (- g-max g-temp))
             (when (> b 0.0d0)
               (setf a (eta-cached kernel-vec-i kernel-vec-d i k))
               (when (<= a 0.0d0)
                 (setf a tau))
               (let ((temp (/ (- (* b b)) a)))
                 (declare (type double-float temp))
                 (when (<= temp obj-min)
                   (setf obj-min temp)
                   (setf j k))))
             (when (<= g-temp g-min)
               (setf g-min g-temp)))
      (values j g-min)))
  
  (defun working-set-selection3 (training-vector kernel-function c weight)
    (declare (type simple-vector training-vector)
             (type kernel-function kernel-function)
             (type double-float c weight))
    (let ((i -1)
          (j -1)
          (eps *eps*)
          (tau *tau*)
          (training-size *training-size*)
          (label-index *label-index*)
          (alpha-array *alpha-array*)
          (gradient-array *gradient-array*))
      
      (declare (type fixnum i j)
               (type double-float eps tau)
               (type fixnum training-size)
               (type array-index label-index)
               (type dvec alpha-array gradient-array))
      
      (let ((g-max most-negative-double-float)
            (g-min most-positive-double-float))
        (declare (type double-float g-max g-min))
        
        ;;select i
        ;; (multiple-value-setq (i g-max) (select-i training-vector c))
        (loop
          for k of-type array-index below training-size
          as y-k of-type double-float = (aref (the dvec (svref training-vector k)) label-index)
          as a-k of-type double-float = (aref alpha-array k)
          as g-k of-type double-float = (aref gradient-array k)
          as g-temp of-type double-float = (- (* y-k g-k))
          if (and (>= g-temp g-max)
                  (or (and (= y-k 1.0d0) (< a-k c))
                      (and (= y-k -1.0d0) (> a-k 0.0d0))))
            do (progn 
                 (setf g-max g-temp)
                 (setf i k)))

        ;;select j
        ;; (multiple-value-setq (j g-min) (select-j training-vector kernel-function c weight i g-max))
        (let ((obj-min most-positive-double-float))
          (declare (type double-float obj-min))

          (let* ((kernel-cache *kernel-cache*)
                 (kernel-vec-d *kernel-vec-d*)
                 (kernel-vec-i (get-cached-values kernel-cache training-vector i training-size kernel-function)))
            (declare (type dvec kernel-vec-i kernel-vec-d))
            (loop
              for k of-type array-index below training-size
              as y-k of-type double-float = (aref (the dvec (svref training-vector k)) label-index)
              as a-k of-type double-float = (aref alpha-array k)
              as g-k of-type double-float = (aref gradient-array k)
              as g-temp of-type double-float = (- (* y-k g-k))
              with a of-type double-float = 0.0d0	      
              with b of-type double-float  = 0.0d0
              if (or (and (= y-k 1.0d0) (> a-k 0.0d0))
                     (and (= y-k -1.0d0) (< a-k (* weight c))))
                do (setf b (- g-max g-temp))
                   (when (> b 0.0d0)
                     (setf a (eta-cached kernel-vec-i kernel-vec-d i k))
                     (when (<= a 0.0d0)
                       (setf a tau))
                     (let ((temp (/ (- (* b b)) a)))
                       (declare (type double-float temp))
                       (when (<= temp obj-min)
                         (setf obj-min temp)
                         (setf j k))))
                   (when (<= g-temp g-min)
                     (setf g-min g-temp)))))
        
        (when (< (- g-max g-min) eps)
          (return-from working-set-selection3 (values -1 -1)))
        
        (values i j))))
  )



(defun compute-b (training-vector kernel-function c weight alpha-array)
  (declare (type simple-vector training-vector)
           (type dvec alpha-array)
	   (type kernel-function kernel-function)
	   (type double-float c weight)
           (ignorable kernel-function))
  
  (let ((label-index (1- (length (aref training-vector 0))))
	(n (length alpha-array)))
    
    (declare (type fixnum label-index n))
    
    (let ((result 0.0d0))
      (declare (type double-float result))
      (loop
        for i of-type fixnum below n
        as alpha-i of-type double-float = (aref alpha-array i)
        as y-i of-type double-float  = (aref (the dvec (svref training-vector i)) label-index)
        as c-i of-type double-float = (if (plusp y-i) c (* weight c))	 
        with count = 0
        if (< 0.0d0 alpha-i c-i)
	  do (incf count 1)
	     (incf result
		   (- y-i 
		      (let ((result2 0.0d0))
			(declare (type double-float result2))
			(loop 
                          for j of-type fixnum below n
                          as alpha-j of-type double-float = (aref alpha-array j)  
                          as y-j of-type double-float = (aref (the dvec (svref training-vector j)) label-index) 
                          unless (= 0.0d0 alpha-j)
			    do (incf result2 
				     (* alpha-j y-j
                                                (call-kernel-function kernel-function
                                                                      (svref training-vector i)
                                                                      (svref training-vector j))))
                          finally (return result2)))))
        finally (return (/ result count))))))


;;for check
(defun print-b (training-vector kernel-function c weight alpha-array)
  (declare (ignorable kernel-function))
  (let ((label-index (1- (length (aref training-vector 0)))))
    (loop
      for i below (length training-vector)
      as a-i = (aref alpha-array i)
      as point-i = (svref training-vector i)
      as y-i = (aref point-i label-index)
      as c-i of-type double-float = (if (plusp y-i) c (* weight c))
      if (< 0.0d0 a-i c-i)
	do (print (- y-i 
		     (loop
                       for j below (length training-vector)
                       as a-j = (aref alpha-array j)
                       as y-j = (aref (aref training-vector j) label-index)
                       unless (= 0.0d0 a-j)
			 sum (* a-j y-j
                                    (call-kernel-function kernel-function
                                                          (svref training-vector i)
                                                          (svref training-vector j)))))))))


(defun make-linear-kernel ()
  (define-kernel-function (z-i z-j :linear) 
    (loop
      for k of-type array-index below (1- (length z-i))
      sum (* (aref z-i k) (aref z-j k))
        into result of-type double-float
      finally (return result))))

(defun make-rbf-kernel (&key gamma)
  (let ((gamma (coerce gamma 'double-float)))
    (declare (type double-float gamma))
    (assert (> gamma 0.0d0))
    (define-kernel-function (z-i z-j :rbf)
      (loop
        for k of-type array-index below (1- (length z-i))
        sum (expt (- (aref z-i k) (aref z-j k)) 2)
          into result of-type double-float
        finally (return (d-exp (* (- gamma) result)))))))

(defun make-polynomial-kernel (&key gamma r d)
  (assert (> gamma 0.0d0))
  (assert (and (integerp d) (> d 0)))
  (let ((gamma (coerce gamma 'double-float))
        (r (coerce r 'double-float))
        (d (coerce d 'double-float)))
    (declare (type double-float gamma r d))
    (let ((linear-kernel (make-linear-kernel)))
      (define-kernel-function (z-i z-j :polynomial) 
        (d-expt (the (double-float 0d0)
                  (+ (* gamma (call-kernel-function-uncached linear-kernel z-i z-j)) r)) d)))))


;;for comparison
(declaim (inline sign))
#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (get 'sign 'sys::immed-args-call)
        '((double-float) double-float)))
(defun sign (x)
  (declare (type double-float x))
  (if (>= x 0.0d0)
      1.0d0
      -1.0d0))


(defun make-discriminant-function0 (training-vector kernel-function alpha-array b)
  (declare (ignorable kernel-function))
  (let ((label-index (1- (length (svref training-vector 0)))))
    (lambda (point)
      (sign (+ (loop 
                 for i below (length alpha-array)
                 as a-i = (aref alpha-array i)
                 unless (= 0.0d0 a-i)
                   sum (* a-i
                          (aref (svref training-vector i) label-index)
                          (call-kernel-function-uncached kernel-function (svref training-vector i) point)))
               b)))))


(defun make-discriminant-function (training-vector kernel-function alpha-array b)
  (declare (type simple-vector training-vector)
	   (type kernel-function kernel-function)
	   (type dvec alpha-array)
	   (type double-float b)
           (ignorable kernel-function))
  
  (let ((label-index (1- (length (svref training-vector 0)))))
    
    (declare (type fixnum label-index))
    
    (lambda (point)
      (sign (+ (let ((result 0.0d0))
                 (declare (type double-float result))  
                 (loop 
                   for i of-type fixnum below (length alpha-array)
                   as a-i of-type double-float = (aref alpha-array i)
                   unless (= 0.0d0 a-i)
                     do (incf result
                              (* a-i
                                 (aref (the dvec (svref training-vector i)) label-index)
                                 (call-kernel-function-uncached kernel-function (svref training-vector i) point))))
                 result)
               b)))))


(defun make-svm-learner (training-vector kernel-function &key c (weight 1.0d0) file-name external-format cache-size-in-MB)
  (assert (plusp c))
  (assert (plusp weight))
  (let* ((c (coerce c 'double-float))
	 (weight (coerce weight 'double-float))
	 (alpha-array (qp-solver training-vector kernel-function c weight (* (or cache-size-in-MB 100) 1024 1024)))
	 (b (compute-b training-vector kernel-function c weight alpha-array)))
    (when (and file-name external-format)
      (with-open-file (out file-name
                           :external-format external-format
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
	(write (list training-vector alpha-array b) :stream out)))
    (make-discriminant-function training-vector kernel-function alpha-array b)))


(defun load-svm-learner (file-name kernel-function &key external-format)
  (let* ((material-list 
	  (with-open-file (in file-name :external-format external-format :direction :input)
	    (read in)))
	 (training-vector (first material-list))
	 (alpha-array (specialize-vec (second material-list)))
	 (b (third material-list)))
    
    (loop 
      for i of-type fixnum below (length training-vector)
      do (setf (aref training-vector i) (specialize-vec (aref training-vector i))))
    
    (make-discriminant-function training-vector kernel-function alpha-array b)))


(defun svm-validation (svm-learner test-vector)
  (let* ((n (length test-vector))
	 (label-index (1- (length (svref test-vector 0))))
	 (sum-up-list
	  (sum-up (loop for i of-type fixnum below n
                        collect (cons (funcall svm-learner (svref test-vector i))
                                      (aref (the dvec (svref test-vector i)) label-index))))))
    (values sum-up-list (accuracy sum-up-list))))


(defun accuracy (sum-up-list)
  (loop for obj in sum-up-list
        as type = (first obj)
        sum (cdr obj) into m
        if (= (car type) (cdr type))
          sum (cdr obj) into n
        finally (return (* 100.0d0 (/ n m)))))

;;for test
(defun sample-vector (n)
  (let ((x (make-array n :initial-element 0.0d0 :element-type 'double-float)))
    (loop for i below n 
          do (setf (aref x i) (coerce (random 10) 'double-float))
          finally (return x)))) 


(defun make-one-class-svm-kernel (&key gamma)
  (declare (type double-float gamma))
  (assert (> gamma 0.0d0))
  (define-kernel-function (z-i z-j :rbf)
    (loop
      for k of-type array-index below (length z-i)
      sum (expt (- (aref z-i k) (aref z-j k)) 2)
        into result of-type double-float
      finally (return (d-exp (* (- gamma) result))))))


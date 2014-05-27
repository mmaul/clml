;;;2011/11/2 Abe Yusuke cf.PRML chapter4

(defpackage :logistic-regression
  (:use :cl
	:svm.wss3;比較実験用
	:hjs.learn.read-data
	:hjs.util.vector
        :hjs.util.matrix))

(in-package logistic-regression)

;;計画行列の作成。
(defun phi-mat (training-vector)
  (let* ((n (length training-vector))
	 (m (length (aref training-vector 0)));切片分を増やすためにこのサイズ
	 (phi (make-array (list n m) :element-type 'double-float :initial-element 0.0d0)))
    (loop
	for i below n
	do (loop
	       for j below m
	       do (if (= j 0)
		      (setf (aref phi i j) 1.0d0);切片
		    (setf (aref phi i j) (aref (aref training-vector i) (1- j)))))
	finally (return phi))))

;;ここでオーバーフロー対策が必要。
(defun logistic-sigmoid (x)
  (cond ((< 50.0d0 x) 1.0d0)
	((< x -50.0d0) 0.0d0)
	(t (/ 1.0d0 (+ 1.0d0 (exp (- x)))))))

;;切片分を追加しているために、引数はtraining-vectorではなく計画行列phi。
(defun y-vector (w-old phi)
  (let* ((n (nrow phi))
	 (m (ncol phi))
	 (y (make-dvec n 0.0d0)))
    (loop
	for i below n
	as x-n = (loop
		     with phi-n = (make-dvec m 0.0d0)
		     for j below m
		     do (setf (aref phi-n j) (aref phi i j))
		     finally (return phi-n))
	as a = (inner-product w-old x-n)
	do (setf (aref y i) (logistic-sigmoid a))
	finally (return y))))

;;svmと違い、ラベルは1.0d0と0.0d0であることに注意。
(defun t-vector (training-vector)
  (let* ((n (length training-vector))
	 (label-pos (1- (length (aref training-vector 0))))
	 (t-vec (make-dvec n 0.0d0)))
    (loop
	for i below n
	as label = (aref (aref training-vector i) label-pos)
	do (setf (aref t-vec i) (if (= -1.0d0 label)
				    0.0d0
				  1.0d0))
	finally (return t-vec))))

;;対角成分以外もきちんと0.0d0で埋める必要あり。
(defun r-mat (y-vec)
  (let* ((n (length y-vec))
	 (r (make-array (list n n) :element-type 'double-float :initial-element 0.0d0)))
    (loop
	for i below n
	as y-n = (aref y-vec i)
	do (setf (aref r i i) (* y-n (- 1.0d0 y-n)))
	finally (return r))))

;;Newton-Raphson法でロジスティック回帰のためのM個(ただし切片も含む)のパラメータを学習用データから求める。
;;0除算を避けるためにわざと展開しない更新式で。
;;wの初期値は0.0d0でないと精度がとても悪い。
(defun newton-raphson (training-vector)
  (let* ((n (length training-vector))
	 (phi (phi-mat training-vector))
	 (phi^t (transpose phi))
	 (m (ncol phi))
	 (w-init (make-dvec (ncol phi) 0.0d0))
	 (y-vec (y-vector w-init phi))
	 (t-vec (t-vector training-vector))
	 (r (r-mat y-vec)))
    (loop
	for i from 1 to 25;反復回数は最大で25(Rのglmに倣った)
	as w-new = (v- w-init
		       (m*v (m*m (m^-1 (m*m (m*m phi^t r) phi)) phi^t)
			    (v- y-vec t-vec (make-dvec n 0.0d0)))
		       (make-dvec m 0.0d0))
	as y-new = (y-vector w-new phi)
	as r-new = (r-mat y-new)
	as v = (v- w-new w-init (make-dvec m 0.0d0))
	while (< 0.001d0 (sqrt (inner-product v v))) ;収束判定
	do (if (loop for i below n thereis (= 0.0d0 (aref r-new i i))) ;Rの対角要素は0.0d0より大きい
	       (progn (pprint "The maximum likelihood estimate may not exist.")
		      (return-from newton-raphson w-init))
	     (setf w-init w-new
		   y-vec y-new
		   r r-new))
	finally (return w-init))))

;;ロジスティック回帰により判別を行う学習器のclosureを返す。
;;切片の扱いに対する配慮が少しだけ必要。
;;p1はクラスラベルが1.0d0であるクラスに属する確率で、それを多値で返す。
(defun make-logistic-regression-learner (training-vector)
  (let ((w (newton-raphson training-vector)))
    (lambda (x) (loop
		    for i from 1 below (length w)
		    as w-i = (aref w i)
		    for x-i = (aref x (1- i))
		    sum (* w-i x-i) into a
		    finally (return (let ((p1 (logistic-sigmoid (+ a (aref w 0)))));ラベル1.0d0のクラスに属する確率p1、および切片
				      (if (<= 0.5 p1)
					  (values 1.0d0 p1)
					(values -1.0d0 p1))))))))

;;Newton-Raphson法で最小化する目的関数値(Rとの比較確認用)。
(defun cross-entropy-error (training-vector w)
  (let* ((phi (phi-mat training-vector))
	 (y-vec (y-vector w phi))
	 (t-vec (t-vector training-vector)))
    (loop
	for y-n across y-vec
	for t-n across t-vec
	as z-n = (- (* (- 1.0d0 t-n) (log (- 1.0d0 y-n)))
		    (* t-n (log y-n)))
	sum z-n)))
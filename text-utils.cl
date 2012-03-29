(defpackage :text-utilities
  (:nicknames :text-utils)
  (:use :common-lisp :hjs.learn.read-data)
  (:export :calculate-string-similarity
           :equivalence-clustering
           ))

(in-package :text-utilities)

;;; 文字列の類似度を算出する
(defun calculate-string-similarity (str1 str2 &key (type :lev)) ;; :lev | :lcs
  (ecase type
    (:lev (calculate-levenshtein-similarity str1 str2))
    (:lcs (calculate-lcs-similarity str1 str2))))

;;;; levenshtein距離(編集距離)を計算する。
;;; levenshtein距離(編集距離)は二つの文字列の類似度を数値化したもの。
;;; 文字の挿入/削除/置換で一方を他方に変形するための最小手順回数を数えたもの
;;; ある文字列str1のi番目までの部分文字列とある文字列str2の文字列2のj番目までの部分文字列の間のlevenshtein距離をLD(i,j)とすると以下の漸化式が成り立つ。
;;; LD(i,j) = LD(i-1, j) + 1 (挿入)
;;; LD(i,j) = LD(i, j-1) + 1 (削除)
;;; LD(i,j) = LD(i-1, j-1) + c (cは次の文字が同じなら0, 違えば1。置換)
;;; LD(0,0) = 0, LD(i,0) = i, LD(0,j) = j (基点) 
;;; 大文字小文字は区別しない
;;; 引数 str1(文字列1) str2(文字列2)
(defun calculate-levenshtein-distance (str1 str2)
  (declare (optimize (speed 3))
           (type (simple-array character (*)) str1 str2))
  (let ((strlen1 (length str1))
        (strlen2 (length str2)))
    (declare (type (integer 0 #.most-positive-fixnum) strlen1 strlen2))
    (let ((len1 (1+ strlen1))
          (len2 (1+ strlen2)))
      (declare (type (integer 0 #.most-positive-fixnum) len1 len2))
      (let ((d (make-array (list len1 len2) :element-type '(integer 0 #.most-positive-fixnum) :initial-element 0)))
        (dotimes (i len1)
          (setf (aref d i 0) i))        ; 基点
        (dotimes (j len2)
          (setf (aref d 0 j) j))        ; 基点
        (dotimes (i strlen1)
          (dotimes (j strlen2)
            (declare (type (integer 0 #.most-positive-fixnum) i j))
            (let ((c (if (char-equal (char str1 i)
                                     (char str2 j))
                         0
                       1))
                  (ni (1+ i))
                  (nj (1+ j)))
              (declare (type (integer 0 1) c)
                       (type (integer 0 #.most-positive-fixnum) ni nj))
              (setf (aref d ni nj)
                (min (the (integer 0 #.most-positive-fixnum) (1+ (aref d i nj)))
                     (the (integer 0 #.most-positive-fixnum) (1+ (aref d ni j)))
                     (the (integer 0 #.most-positive-fixnum) (+ (aref d i j) c)))))))
      
        #+ignore
        (loop for i from 1 to (1- len1)
            do (loop for j from 1 to (1- len2)
                   do (let ((c (if (char-equal (char str1 (1- i))
                                               (char str2 (1- j)))
                                   0
                                 1)))
                        (setf (aref d i j)
                          (min (1+ (aref d (1- i) j))
                               (1+ (aref d i (1- j)))
                               (+ (aref d (1- i) (1- j)) c))))))
      
        (aref d strlen1 strlen2)
        ))))

;; 最長共通部分系列長（LCS長, longest common subsequence）を求める。
;; 文字列str1の部分列(連続している必要はないが、順序は変更できない)と文字列str2の部分列の中で両方に共通に含まれるものが共通部分列。
;; 共通部分列の中でもっとも長いものを最長共通部分列という。
;; LCSの長さが(もとの部分列の長さと比較して)長ければ類似した文字列となる。
;; 文字列str1のうちi番目までの文字列Xiと文字列str2のうちj番目までの文字列YiのLCS長をLCS(i,j)とする。
;; (a) XiとYiの最後の文字が同じである場合
;;     LCS(i,j) = LCS(i-1, j-1) + 1
;; (b) XiとYiの最後の文字が異っていた場合
;;     LCS(i,j) = max(LCS(i,j-1), LCS(i-1,j))
;; i=0またはj=0のときLCS(i,j)=0
;; 大文字小文字は区別しない
(defun calculate-lcs-distance (str1 str2)
  (declare (optimize (speed 3))
           (type (simple-array character (*)) str1 str2))
  (let ((strlen1 (length str1))
        (strlen2 (length str2)))
    (declare (type (integer 0 #.most-positive-fixnum) strlen1 strlen2))
    (let ((len1 (1+ strlen1))
          (len2 (1+ strlen2)))
      (declare (type (integer 0 #.most-positive-fixnum) len1 len2))
      (let ((d (make-array (list len1 len2) :element-type '(integer 0 #.most-positive-fixnum) :initial-element 0)))
        (dotimes (i strlen1)
          (dotimes (j strlen2)
            (declare (type (integer 0 #.most-positive-fixnum) i j))
            (let ((ni (1+ i))
                  (nj (1+ j)))
              (if (char-equal (char str1 i)
                              (char str2 j))
                  (setf (aref d ni nj)
                    (1+ (aref d i j)))
                (setf (aref d ni nj)
                  (max (aref d i nj)
                       (aref d ni j)))))))
        (aref d strlen1 strlen2)))))

;; レーベンシュタイン／LCS距離は、値が文字列の長さに引っ張られてしまうため、
;; 類似度として扱うには正規化してやる必要がある。
#+ignore
(defun calculate-levenshtein-similarity (str1 str2)
  (declare (optimize speed) (type (simple-array character (*)) str1 str2))
  (- 1.0 (/ (* 2.0 (the integer (calculate-levenshtein-distance str1 str2)))
            (+ (the fixnum (length str1)) (the fixnum (length str2))))))
(defun calculate-levenshtein-similarity (str1 str2)
  (declare (optimize speed) (type (simple-array character (*)) str1 str2))
  (- 1.0 (/ (the integer (calculate-levenshtein-distance str1 str2))
            (max (the fixnum (length str1)) (the fixnum (length str2))))))

(defun calculate-lcs-similarity (str1 str2)
  (declare (optimize speed) (type (simple-array character (*)) str1 str2))
  (/ (* 2.0 (the integer (calculate-lcs-distance str1 str2)))
     (+ (the fixnum (length str1)) (the fixnum (length str2)))))

;;文字列で名指された対象間に与えられた同値関係から、同値類を形成しリストで返す。
(defun equivalence-clustering (data-vector)
  "Based on Knuth's equivalence clustering algorithm"
  (when (= 0 (length data-vector))
    (return-from equivalence-clustering nil))
  ;;(assert (<= 1 (length data-vector)))
  (let* ((string-index-table 
	  (loop 
	      for i of-type fixnum below (length data-vector)
	      with table = (make-hash-table :test #'equal)
	      as row = (svref data-vector i)
	      as string0 = (svref row 0)
	      as string1 = (svref row 1)
	      do (setf (gethash string0 table) 0)
		 (setf (gethash string1 table) 0)
	      finally (return table)))
	 (n (hash-table-count string-index-table))
	 (string-array (coerce (loop for key being the hash-keys in string-index-table collect key) 'vector))
	 (class-id-array (coerce (loop for i of-type fixnum below n collect i) 'vector))
	 (label-index (1- (length (svref data-vector 0))))
	 (a-array)
	 (b-array))
    
    (loop 
	for i of-type fixnum below n
	do (setf (gethash (svref string-array i) string-index-table) i))
    
    (multiple-value-setq (a-array b-array)
      (loop
	  for row across data-vector
	  as label = (svref row label-index)
	  if (= 1.0 label)
	  collect (gethash (svref row 0) string-index-table) into a-list
	  if (= 1.0 label)
	  collect (gethash (svref row 1) string-index-table) into b-list
	  finally (return (values (coerce a-list 'vector)
				  (coerce b-list 'vector)))))
    
    (loop 
	for i of-type fixnum below (length a-array)
	as j = (svref a-array i)
	as k = (svref b-array i)
	do (loop
	       while (/= (svref class-id-array j) j)
	       do (setf j (svref class-id-array j)))
	   (loop 
	       while
		 (/= (svref class-id-array k) k)
	       do (setf k (svref class-id-array k)))
	   (when (/= j k)
	     (setf (svref class-id-array j) k)))

    ;;compute class-id-array
    (loop
	for j of-type fixnum below n
	do (loop
	       while (/= (svref class-id-array j) (svref class-id-array (svref class-id-array j)))
	       do (setf (svref class-id-array j) (svref class-id-array (svref class-id-array j)))))

    ;;class-id-array->clustering-result
    (loop
	with class-id-table = (loop
				  with class-id-table = (make-hash-table)
				  for class-id across class-id-array
				  do (setf (gethash class-id class-id-table) 0)
				  finally (return class-id-table))
	for class-id of-type fixnum being the hash-keys in class-id-table
	collect (loop
    		    for i of-type fixnum below n
    		    as string = (svref string-array i)
    		    if (= class-id (svref class-id-array i))
    		    collect string) into clustering-result
    	finally (return clustering-result))))

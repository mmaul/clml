
(in-package "TEST")

(defun rule-equal (expected-rule rule)
  (loop for r1 across expected-rule
      for r2 across rule
      for i from 0
      always (cond ((and (or (= i 0) (= i 1))
                         (eql (length r1) (length r2)))
                    (set-equal r1 r2 :test #'string=))
                   ((or (= i 0) (= i 1)) nil)
                   (t (epsilon> r1 r2)))))

(defun assert-assoc-equal (expected-assoc assoc)
  (assert-true 
   (set-equal (coerce (assoc-result-header expected-assoc) 'list)
              (coerce (assoc-result-header assoc) 'list) :test #'string=))
  (let ((expected-rules (assoc-result-rules expected-assoc))
        (rules (assoc-result-rules assoc)))
    (when (assert-eql (length expected-rules) (length rules))
      (assert-false 
       (set-difference expected-rules rules :test #'rule-equal)))))

(define-test test-sample-assoc
    (let (dataset result result1
          (expected-header #("premise" "conclusion" "support" "confidence" "lift" "conviction"))
          (expected-result
           '(#(("商品名=野菜") ("商品名=菓子")
               16.886543535620053d0 31.44963144963145d0 1.1053549f0 1.0437279f0)
             #(("商品名=菓子") ("商品名=野菜")
               16.886543535620053d0 59.35085007727975d0 1.1053549f0 1.1391644f0)
             #(("商品名=野菜") ("商品名=菓子" "商品名=パン")
               8.970976253298153d0 16.707616707616708d0 1.2706729f0 1.0427288f0)
             #(("商品名=菓子") ("商品名=野菜" "商品名=パン")
               8.970976253298153d0 31.530139103554866d0 1.3351868f0 1.1156036f0)
             #(("商品名=パン") ("商品名=野菜" "商品名=菓子")
               8.970976253298153d0 23.80396732788798d0 1.4096411f0 1.0907845f0)
             #(("商品名=野菜" "商品名=菓子") ("商品名=パン")
               8.970976253298153d0 53.125d0 1.4096411f0 1.3293462f0)
             #(("商品名=野菜" "商品名=パン") ("商品名=菓子")
               8.970976253298153d0 37.988826815642454d0 1.3351868f0 1.153791f0)
             #(("商品名=菓子" "商品名=パン") ("商品名=野菜")
               8.970976253298153d0 68.22742474916387d0 1.2706729f0 1.4574226f0)
             #(("商品名=野菜") ("商品名=菓子" "商品名=乳製品")
               7.695690413368514d0 14.332514332514332d0 1.2439747f0 1.0328125f0)
             #(("商品名=菓子") ("商品名=野菜" "商品名=乳製品")
               7.695690413368514d0 27.04791344667697d0 1.28407f0 1.0820224f0))))
      (assert-true
       (setf result (association-analyze "sample/pos.sexp" "sample/result.sexp"
                                         '("商品名") "ID番号" 3 
                                         :support 2 :external-format #+allegro :932 #-allegro :sjis)))
      (loop for rule1 in expected-result
          for rule2 in (assoc-result-rules result)
          do (assert-true (rule-equal rule1 rule2)))
      (loop with rules = (assoc-result-rules result)
          for rule in 
            (with-open-file (in "sample/result.sexp" :external-format #+allegro :932 #-allegro :sjis) (read in))
          for i from 0
          do (if (= i 0) 
                 (assert-true 
                  (set-equal (coerce (assoc-result-header result) 'list)
                             (coerce rule 'list) :test #'string=))
               (assert-true (rule-equal rule (nth (1- i) rules)))))
      (assert-true 
       (setf dataset (read-data-from-file "sample/pos.sexp" :external-format #+allegro :932 #-allegro :sjis)))
      
      ;; verify rule indexes
      (let* ((key-pos (dimension-index (find "ID番号" (dataset-dimensions dataset)
                                             :test #'string= :key #'dimension-name)))
             (val-pos (dimension-index (find "商品名" (dataset-dimensions dataset)
                                             :test #'string= :key #'dimension-name)))
             (transaction-hash (make-hash-table :test #'equal))
             total)
        (loop for pts across (dataset-points dataset)
            as key = (aref pts key-pos) as val = (aref pts val-pos)
            do (pushnew val (gethash key transaction-hash) :test #'string=))
        (setq total (hash-table-count transaction-hash))
        (labels
            ((after= (str) (declare (type string str))
                     (subseq str (1+ (position #\= str :test #'char-equal))))
             (support (rule) (/ rule total))
             (confident (rule pre) (/ rule pre))
             (lift (rule pre conq) (/ (confident rule pre) (support conq)))
             (conviction (rule pre conq) (/ (- 1 (support conq)) (- 1 (confident rule pre))))
             (verify-rule-indexes (rule)
               (let ((pre-item (mapcar #'after= (aref rule 0)))
                     (conq-item (mapcar #'after= (aref rule 1)))
                     (rule 0) (pre 0) (conq 0))
                 (maphash (lambda (k vals) (declare (ignore k))
                                  (when (subsetp pre-item vals :test #'string=)
                                    (incf pre) (incf rule))
                                  (if (subsetp conq-item vals :test #'string=)
                                      (incf conq) (decf rule)))
                          transaction-hash)
                 (assert-true
                  (set-equal () () :test #'epsilon>)))))
          (loop repeat 100 ;; for more accurate test, remove 'repeat'
              for rule in (assoc-result-rules result)
              do (verify-rule-indexes rule))))
      
      (assert-true 
       (setf result1 (%association-analyze-apriori dataset '("商品名") "ID番号" 3 :support 2)))
      (assert-assoc-equal result result1)
      
      (assert-true 
       (setf result1 (%association-analyze-da-ap-genrule dataset '("商品名") "ID番号" 3 :support 2)))
      (assert-assoc-equal result result1)
      
      (assert-true 
       (setf result1 (%association-analyze-fp-growth dataset '("商品名") "ID番号" 3 :support 2)))
      (assert-assoc-equal result result1)
      
      (assert-true 
       (setf result1 (%association-analyze-eclat dataset '("商品名") "ID番号" 3 :support 2)))
      (assert-assoc-equal result result1)
      
      (assert-true 
       (setf result1 (%association-analyze-lcm dataset '("商品名") "ID番号" 3 :support 2)))
      (assert-assoc-equal result result1)))
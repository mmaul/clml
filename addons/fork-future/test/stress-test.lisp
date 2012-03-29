(in-package :fork-future-test)

(in-suite root-suite)

(defsuite* stress-test)

(deftest 100p-100times ()
  (loop repeat 100
        do
     (progn
       (assert-no-futures) 
       (let ((futures (loop repeat 100
                            collect
                         (future (+ 1 1)))))
         (is (= 100 (futures-count)))
         (is (= 200 (reduce '+ futures :key 'touch)))
         (is (> 0 (fork-future::waitpid 0))))
       (assert-no-futures))))

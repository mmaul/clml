(in-package :fork-future-test)

(defun running-futures-count ()
  (hash-table-count fork-future::*running-futures*))

(defun pending-futures-count ()
  (fork-future::queue-length fork-future::*pending-futures*))

(defun futures-count ()
  (+ (running-futures-count) (pending-futures-count)))

(defun assert-no-running-futures ()
  (is (= 0 (running-futures-count))))

(defun assert-no-pending-futures ()
  (is (= 0 (pending-futures-count))))

(defun assert-no-futures ()
  (assert-no-running-futures)
  (assert-no-pending-futures))


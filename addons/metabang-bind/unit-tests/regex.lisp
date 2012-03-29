(in-package #:metabang-bind-test)

#+(or)
(run-tests :suite 'test-regex)


(deftestsuite test-regex (metabang-bind-test)
  ()
  (:equality-test #'equalp))

(addtest (test-regex)
  simple-bind
  (ensure-same
   (bind (((:re "(\\w+)\\s+(\\w+)\\s+(\\d{1,2})\\.(\\d{1,2})\\.(\\d{4})"
		fname lname date month year) "Frank Zappa 21.12.1940"))
     (list fname lname date month year))
   (list "Frank" "Zappa" "21" "12" "1940")))

(addtest (test-regex)
  nils-are-ignored-1
  (ensure-same
   (bind (((:re "(\\w+)\\s+(\\w+)\\s+(\\d{1,2})\\.(\\d{1,2})\\.(\\d{4})"
		fname lname nil month year) "Frank Zappa 21.12.1940"))
     (list fname lname month year))
   (list "Frank" "Zappa" "12" "1940")))

(addtest (test-regex)
  nils-are-ignored-2
  (ensure-same
   (bind (((:re "(\\w+)\\s+(\\w+)\\s+(\\d{1,2})\\.(\\d{1,2})\\.(\\d{4})"
		nil lname nil month year) "Frank Zappa 21.12.1940"))
     (list lname month year))
   (list "Zappa" "12" "1940")))

#+(or)
(addtest (test-regex)
  nils-are-ignored-1
  (let ((result 
	 (lambda ()
	   (bind (((:re "(\\w+)\\s+(\\w+)\\s+(\\d{1,2})\\.(\\d{1,2})\\.(\\d{4})"
			fname lname nil month year) "Frank Zappa 21.12.1940"))
	     (list lname month year)))))
    (ensure-same (funcall result)
		 (list "Zappa" "12" "1940"))
    (ensure-warning
      (compile 
       nil
       (lambda ()
	 (bind (((:re "(\\w+)\\s+(\\w+)\\s+(\\d{1,2})\\.(\\d{1,2})\\.(\\d{4})"
		      fname lname nil month year) "Frank Zappa 21.12.1940"))
	   (list lname month year)))))))

(addtest (test-regex)
  executes-when-no-bindings
  (ensure-same
   (bind (((:re "(a|b)+" first) "cccc"))
     (list "still seen" first))
   (list "still seen" nil)))

#+(or)
(addtest (test-regex)
  you-can-use-doit
  (ensure-same
   (bind (((:re "(\\w+)\\s+(\\w+)\\s+(\\d{1,2})\\.(\\d{1,2})\\.(\\d{4})"
			fname lname nil month year) 
	   "Frank Zappa 21.12.1940"))
     (flet ((doit (&rest vars)
	      (reverse vars)))
       (doit fname lname month year)))
   (reverse (list "Frank" "Zappa" "12" "1940"))))

#+(or)
(defun xxx ()
  (let ((result 
	 '(lambda ()
	   (bind (((:re "(\\w+)\\s+(\\w+)\\s+(\\d{1,2})\\.(\\d{1,2})\\.(\\d{4})"
			nil lname nil month year) "Frank Zappa 21.12.1940"))
	     (list lname month year)))))
  (compile nil result)))

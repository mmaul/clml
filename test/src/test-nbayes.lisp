(in-package :clml.test)

(define-test test-nbayes
  (let (
        (bow-train (dataset-points (read-data-from-file (clml.utility.data:fetch "https://mmaul.github.io/clml.data/sample/bow-train.csv")
                                                             :type :csv :csv-type-spec `(,@(loop repeat 928 collect 'double-float) string) :external-format :utf-8)))

        (bow-test (dataset-points (read-data-from-file (clml.utility.data:fetch "https://mmaul.github.io/clml.data/sample/bow-test.csv")
                                                            :type :csv :csv-type-spec `(,@(loop repeat 928 collect 'double-float) string) :external-format :utf-8 ))))

    (assert-eql 84 (length bow-train))

    (assert-eql 70 (length bow-test))

    (let ((mbnb-learner (apply #'make-mbnb-learner (mbnb-learn bow-train))))

      (assert-equalp "cobol" (funcall mbnb-learner (svref bow-test 0)))

      (assert-equalp "cobol" (funcall mbnb-learner (svref bow-test 1)))

      (assert-equalp "cobol" (funcall mbnb-learner (svref bow-test 2)))

      (assert-equalp "lisp" (funcall mbnb-learner (svref bow-test 40)))

      (assert-equalp "lisp" (funcall mbnb-learner (svref bow-test 41)))

      (assert-equalp "lisp" (funcall mbnb-learner (svref bow-test 42))))
    (let ((mnb-learner (apply #'make-mnb-learner (mnb-learn bow-train))))

        (assert-equalp "cobol" (funcall mnb-learner (svref bow-test 0)))

        (assert-equalp "cobol" (funcall mnb-learner (svref bow-test 1)))

        (assert-equalp "cobol" (funcall mnb-learner (svref bow-test 2)))

        (assert-equalp "lisp" (funcall mnb-learner (svref bow-test 40)))

        (assert-equalp "lisp" (funcall mnb-learner (svref bow-test 41)))

        (assert-equalp "lisp" (funcall mnb-learner (svref bow-test 42)))))
  )

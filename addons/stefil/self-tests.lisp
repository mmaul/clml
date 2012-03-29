;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :stefil)

(defpackage :stefil-test
  (:use
   :common-lisp
   :metabang-bind
   :alexandria
   :iterate
   :stefil
   )
  (:shadow #:deftest)
  (:export #:test))

(eval-always
  (import
   '(enable-sharp-boolean-syntax *suite* count-tests
     rebind parent-of name-of *tests* eval-always
     extract-assert-expression-and-message record-failure record-failure*
     assertion-count-of run-tests-of failure-descriptions-of
     *global-context* *context* debug-on-unexpected-error-p
     debug-on-assertion-failure-p print-test-run-progress-p
     file-header rem-test lambda-list-to-variable-name-list
     lambda-list-to-value-list-expression lambda-list-to-funcall-expression
     illegal-lambda-list
     )
   (find-package :stefil-test)))

(in-package :stefil-test)

#.(file-header)

(defparameter *stefil-temp-suite* (defsuite (stefil-temp-suite :documentation "Suite active when the Stefil self-tests are being run")))

(defsuite* (test :in root-suite :documentation "Stefil self tests"))

;; hide deftest with a local version that rebinds and sets *suite* when executing the body
(defmacro deftest (name args &body body)
  `(stefil:deftest ,name ,args
    (let ((*suite* *stefil-temp-suite*))
      ,@body)))

(deftest lifecycle (&key (test-name (gensym "TEMP-TEST")) (suite-name (gensym "TEMP-SUITE")))
  (bind ((original-test-count (count-tests *suite*))
         (original-current-suite *suite*)
         (transient-test-name (gensym "TRANSIENT-TEST")))
    (unwind-protect
         (progn
           (eval `(deftest ,test-name ()))
           (is (= (count-tests *suite*) (1+ original-test-count))))
      (rem-test test-name :otherwise nil))
    (is (= (count-tests *suite*) original-test-count))
    (unwind-protect
         (bind ((temp-suite (eval `(defsuite (,suite-name :in ,*suite*)))))
           (is (= (count-tests *suite*) (1+ original-test-count)))
           (is (eq (parent-of temp-suite) *suite*))
           (is (eq (find-test (name-of temp-suite)) temp-suite))
           (eval `(in-suite ,(name-of temp-suite)))
           (is (eq *suite* (find-test suite-name)))
           (eval `(deftest ,transient-test-name ())))
      (rem-test suite-name))
    (signals error (find-test transient-test-name))
    (signals error (find-test suite-name))
    (is (= (count-tests *suite*) original-test-count))
    (is (eq original-current-suite *suite*))))

(defparameter *global-counter-for-lexical-test* 0)

(let ((counter 0))
  (setf *global-counter-for-lexical-test* 0)
  (deftest (counter-in-lexical-environment :compile-before-run #f) ()
    (incf counter)
    (incf *global-counter-for-lexical-test*)
    (is (= counter *global-counter-for-lexical-test*))))

(defmacro false-macro ()
  #f)

(defmacro true-macro ()
  #t)

(deftest (assertions :compile-before-run #t :in test) (&key (test-name (gensym "TEMP-TEST")))
  (unwind-protect
       (eval `(deftest ,test-name ()
               (is (= 42 42))
               (is (= 1 42))
               (is (not (= 42 42)))
               (is (true-macro))
               (is (not (false-macro)))))
    (progn
      ;; this uglyness here is due to testing the test framework which is inherently
      ;; not nestable, so we need to backup and restore some state
      (bind ((context *global-context*)
             (old-assertion-count (assertion-count-of context))
             (old-failure-description-count (length (failure-descriptions-of context)))
             (old-debug-on-unexpected-error (debug-on-unexpected-error-p context))
             (old-debug-on-assertion-failure (debug-on-assertion-failure-p context))
             (old-print-test-run-progress-p (print-test-run-progress-p context)))
        (unwind-protect
             (progn
               (setf (debug-on-unexpected-error-p context) #f)
               (setf (debug-on-assertion-failure-p context) #f)
               (setf (print-test-run-progress-p context) #f)
               (funcall test-name))
          (setf (debug-on-unexpected-error-p context) old-debug-on-unexpected-error)
          (setf (debug-on-assertion-failure-p context) old-debug-on-assertion-failure)
          (setf (print-test-run-progress-p context) old-print-test-run-progress-p))
        (is (= (assertion-count-of context)
               (+ old-assertion-count 6))) ; also includes the current assertion
        (is (= (length (failure-descriptions-of context))
               (+ old-failure-description-count 2)))
        (dotimes (i (- (length (failure-descriptions-of context))
                       old-failure-description-count))
          ;; drop failures registered by the test-test
          (vector-pop (failure-descriptions-of context))))
      (rem-test test-name :otherwise nil)))
  (values))

(defsuite* (fixtures :in test))

(defparameter *fixture-test-global* '())

(defixture test-fixture
  (:setup (push '42 *fixture-test-global*))
  (:teardown (setf *fixture-test-global* (remove '42 *fixture-test-global*))))

(defparameter *fixture-test-counter* 0)

(defixture simple-test-fixture
  (incf *fixture-test-counter*))

(deftest fixtures ()
  (with-fixture simple-test-fixture
    (is (not (zerop *fixture-test-counter*)))
    (with-fixture test-fixture
      (is (equal *fixture-test-global* '(42)))
      (nested-fixtures1)
      (is (equal *fixture-test-global* '(42)))
      (nested-fixtures2)
      (is (equal *fixture-test-global* '(42))))
    (is (equal *fixture-test-global* '()))))

(defun nested-fixtures1 ()
  (with-fixture test-fixture
    (is (equal *fixture-test-global* '(42)))))

(deftest (nested-fixtures2 :auto-call #f) ()
  (with-fixture test-fixture
    (is (equal *fixture-test-global* '(42)))))

(defsuite* (lambda-lists :in test))

(deftest lambda-list-processing ()
  (is (equal (lambda-list-to-value-list-expression '(p1 p2 &optional o1 (o2 "o2") &key k1 (k2 "k2") &allow-other-keys))
             '(list (cons 'p1 p1) (cons 'p2 p2) (cons 'o1 o1) (cons 'o2 o2) (cons 'k1 k1)
               (cons 'k2 k2))))
  (is (equal (lambda-list-to-funcall-expression 'foo '(p1 p2 &optional o1 (o2 "o2") &key k1 (k2 "k2") &allow-other-keys))
             '(FUNCALL FOO P1 P2 O1 O2 :K1 K1 :K2 K2)))
  (is (equal (lambda-list-to-funcall-expression 'foo '(&optional &key &allow-other-keys))
             '(FUNCALL FOO)))
  (is (equal (lambda-list-to-funcall-expression 'foo '(&optional &rest args &key &allow-other-keys))
             '(APPLY FOO args)))
  (is (equal (lambda-list-to-funcall-expression 'foo '(p1 p2 &optional o1 (o2 "o2") &rest args &key k1 (k2 "k2") &allow-other-keys))
             '(APPLY FOO P1 P2 O1 O2 :K1 K1 :K2 K2 ARGS)))
  (is (equal (lambda-list-to-variable-name-list '(&whole whole p1 p2 &optional o1 (o2 "o2") &body body)
                                                :macro t :include-specials t)
             '(WHOLE P1 P2 O1 O2 BODY)))
  (is (equal (multiple-value-list
              (lambda-list-to-variable-name-list '(&whole whole &environment env p1 p2 &optional o1 (o2 "o2") &body body)
                                                 :macro t :include-specials nil))
             '((P1 P2 O1 O2)
               BODY
               WHOLE
               ENV)))
  (dolist (entry '((p1 &whole)
                   (&allow-other-keys)
                   (&key k1 &optional o1)
                   (&aux x1 &key k1)))
    (signals illegal-lambda-list
      (lambda-list-to-variable-name-list entry)))
  (dolist (entry '((p1 &whole)
                   (&allow-other-keys)
                   (&key k1 &optional o1)
                   (&aux x1 &key k1)
                   (a &rest rest &body body)
                   (&aux a &body body)))
    (signals illegal-lambda-list
      (lambda-list-to-variable-name-list entry :macro t))))


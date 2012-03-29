;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :stefil)

#.(file-header)

;; Warning: setf-ing these variables in not a smart idea because other systems may rely on their default value.
;; It's smarter to rebind them in an :around method from your .asd or shadow stefil:deftest with your own that sets
;; their keyword counterparts.
(defvar *suite*)
(defvar *root-suite*)
(defvar *print-test-run-progress* #t)
(defvar *compile-tests-before-run* #f)
(defvar *compile-tests-with-debug-level* nil)
(defvar *test-progress-print-right-margin* 80)
(defvar *debug-on-unexpected-error* #t)
(defvar *debug-on-assertion-failure* #t)
(defvar *test-result-history* '())
(defvar *last-test-result* nil)
(defvar *failures-and-errors-are-expected* #f)

(defvar *tests* (make-hash-table :test 'eql)) ; this is not thread-safe, but...

(defmacro without-debugging (&body body)
  `(bind ((*debug-on-unexpected-error* #f)
          (*debug-on-assertion-failure* #f))
    ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; conditions

(define-condition test-related-condition ()
  ((test :initform nil :accessor test-of :initarg :test)))

(define-condition test-style-warning (style-warning test-related-condition simple-warning)
  ())

(define-condition assertion-failed (test-related-condition serious-condition)
  ((failure-description :accessor failure-description-of :initarg :failure-description))
  (:report (lambda (c stream)
             (format stream "Test assertion failed:~%~%")
             (describe (failure-description-of c) stream))))

(define-condition error-in-teardown (error)
  ((condition :accessor condition-of :initarg :condition)
   (fixture :accessor fixture-of :initarg :fixture))
  (:report (lambda (c stream)
             (format stream "Error while running teardown of fixture ~A:~%~%~A" (fixture-of c) (condition-of c)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; some classes

#+nil
(defclass-star:defclass* testable ()
  ((name :type symbol)
   (parent nil :initarg nil :type (or null testable))
   (children (make-hash-table) :documentation "A mapping from testable names to testables")
   (auto-call #t :type boolean :documentation "Controls whether to automatically call this test when its parent suite is invoked. Enabled by default.")))

(defclass testable ()
  ((name :accessor name-of :initarg :name :type symbol)
   (parent :initform nil :accessor parent-of :type (or null testable))
   (children :initform (make-hash-table) :accessor children-of :initarg :children :documentation "A mapping from testable names to testables")
   (auto-call :initform t :accessor auto-call-p :initarg :auto-call :type boolean :documentation "Controls whether to automatically call this test when its parent suite is invoked. Enabled by default.")))

(defprint-object (self testable :identity #f :type #f)
  (format t "test ~S" (name-of self))
  (bind ((children (count-tests self)))
    (unless (zerop children)
      (format t " :tests ~S" children))))

(defmethod shared-initialize :after ((self testable) slot-names
                                     &key (in (or (parent-of self)
                                                  (and (boundp '*suite*)
                                                       *suite*))))
  (assert (name-of self))
  (setf (find-test (name-of self)) self)
  ;; make sure the specialized writer below is triggered
  (setf (parent-of self) in))

(defmethod (setf parent-of) :around (new-parent (self testable))
  (assert (typep new-parent '(or null testable)))
  (when (and new-parent
             (symbol-package (name-of self)) ; leave alone tests named by uninterned symbols
             (not (eq new-parent *root-suite*))
             (not (eq (symbol-package (name-of new-parent))
                      (symbol-package (name-of self)))))
    (warn 'test-style-warning :test self
          :format-control "Adding test under parent ~S which is in a different package (parent: ~A, child: ~A). Maybe a missing (in-root-suite)?"
          :format-arguments (list new-parent (symbol-package (name-of new-parent)) (symbol-package (name-of self)))))
  (bind ((old-parent (parent-of self)))
    (when old-parent
      (remhash (name-of self) (children-of old-parent)))
    (prog1
        (call-next-method)
      (when new-parent
        (setf (gethash (name-of self) (children-of new-parent)) self)))))

(defgeneric count-tests (testable)
  (:method ((self testable))
           (+ (hash-table-count (children-of self))
              (iter (for (nil child) :in-hashtable (children-of self))
                    (summing (count-tests child))))))

#+nil(defclass-star:defclass* test (testable)
  ((package nil)
   (lambda-list nil)
   (compile-before-run #t :type boolean)
   (declarations nil)
   (documentation nil)
   (body nil)))

(defclass test (testable)
  ((package :initform nil :accessor package-of :initarg :package)
   (lambda-list :initform nil :accessor lambda-list-of :initarg :lambda-list)
   (compile-before-run :initform t :accessor compile-before-run-p :initarg :compile-before-run :type boolean)
   (declarations :initform nil :accessor declarations-of :initarg :declarations)
   (documentation :initform nil :accessor documentation-of :initarg :documentation)
   (body :initform nil :accessor body-of :initarg :body)))

(defun make-test (name &rest args &key &allow-other-keys)
  (apply #'make-instance 'test :name name args))

(defun make-suite (name &rest args &key &allow-other-keys)
  (apply #'make-instance 'test :name name args))

#+nil
(defclass-star:defclass* failure-description ()
  ((test-context-backtrace)
   (progress-char #\X :allocation :class)
   (expected *failures-and-errors-are-expected* :type boolean)))

(defclass failure-description ()
  ((test-context-backtrace :accessor test-context-backtrace-of :initarg :test-context-backtrace)
   (progress-char :initform #\X :accessor progress-char-of :initarg :progress-char :allocation :class)
   (expected :initform *failures-and-errors-are-expected* :accessor expected-p :initarg :expected :type boolean)))

#+nil
(defclass-star:defclass* failed-assertion (failure-description)
  ((form)
   (format-control)
   (format-arguments)))

(defclass failed-assertion (failure-description)
  ((form :accessor form-of :initarg :form)
   (format-control :accessor format-control-of :initarg :format-control)
   (format-arguments :accessor format-arguments-of :initarg :format-arguments)))

(defmethod describe-object ((self failed-assertion) stream)
  (let ((*print-circle* nil))
    (apply #'format stream (format-control-of self) (format-arguments-of self))))

(defprint-object (self failed-assertion :identity #f :type #f)
  (format t "failure ~S backtrace: ~{~A~^,~}"
          (form-of self)
          (mapcar (compose #'name-of #'test-of)
                  (test-context-backtrace-of self))))

#+nil
(defclass-star:defclass* missing-condition (failure-description)
  ((form)
   (condition)))

(defclass missing-condition (failure-description)
  ((form :accessor form-of :initarg :form)
   (condition :accessor condition-of :initarg :condition)))

(defmethod describe-object ((self missing-condition) stream)
  (let ((*print-circle* nil))
    (format stream "~S failed to signal condition ~S" (form-of self) (condition-of self))))

#+nil
(defclass-star:defclass* unexpected-error (failure-description)
  ((condition)
   (progress-char #\E :allocation :class)))

(defclass unexpected-error (failure-description)
  ((condition :accessor condition-of :initarg :condition)
   (progress-char :initform #\E :accessor progress-char-of :initarg :progress-char :allocation :class)))

(defprint-object (self unexpected-error :identity #f :type #f)
  (format t "error ~{~A~^,~}: ~S"
          (mapcar (compose #'name-of #'test-of)
                  (reverse (test-context-backtrace-of self)))
          (condition-of self)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; test repository

(defun find-test (name &key (otherwise :error))
  (bind (((:values test found-p) (if (typep name 'testable)
                                    (values name t)
                                    (gethash name *tests*))))
    (when (and (not found-p)
               otherwise)
      (etypecase otherwise
        (symbol (ecase otherwise
                  (:error (error "Testable called ~A was not found" name))))
        (function (funcall otherwise))
        (t (setf test otherwise))))
    (values test found-p)))

(defun (setf find-test) (new-value key)
  (if new-value
      (progn
        (when (gethash key *tests*)
          (warn 'test-style-warning
                :format-control "redefining test ~A"
                :format-arguments (list (let ((*package* #.(find-package "KEYWORD")))
                                          (format nil "~S" key)))))
        (setf (gethash key *tests*) new-value))
      (rem-test key)))

(defun rem-test (name &rest args)
  (bind ((test (apply #'find-test name args))
         (parent (when test
                   (parent-of test))))
    (when test
      (assert (or (not (eq *suite* test))
                  (parent-of test))
              () "You can not remove a test which is the current suite and has no parent")
      (remhash name *tests*)
      (setf (parent-of test) nil)
      (fmakunbound (name-of test))
      (iter (for (nil subtest) :in-hashtable (children-of test))
            (rem-test (name-of subtest)))
      (when (eq *suite* test)
        (setf *suite* parent)))
    test))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; the real thing

#+nil
(define-dynamic-context* global-context
  ((failure-descriptions (make-array 8 :adjustable #t :fill-pointer 0))
   (assertion-count 0)
   (progress-char-count 0)
   (print-test-run-progress-p *print-test-run-progress* :type boolean)
   (debug-on-unexpected-error-p *debug-on-unexpected-error* :type boolean)
   (debug-on-assertion-failure-p *debug-on-assertion-failure* :type boolean)
   (toplevel-context nil)
   (current-test nil)
   (run-tests (make-hash-table) :documentation "test -> context mapping")
   (run-fixtures (make-hash-table))
   (test-lambdas (make-hash-table) :documentation "test -> compiled test lambda mapping for this test run")))

(define-dynamic-context global-context
  ((failure-descriptions :initform (make-array 8 :adjustable t :fill-pointer 0) :accessor failure-descriptions-of :initarg :failure-descriptions)
   (assertion-count :initform 0 :accessor assertion-count-of :initarg :assertion-count)
   (progress-char-count :initform 0 :accessor progress-char-count-of :initarg :progress-char-count)
   (print-test-run-progress-p :initform *print-test-run-progress* :accessor print-test-run-progress-p :initarg :print-test-run-progress-p :type boolean)
   (debug-on-unexpected-error-p :initform *debug-on-unexpected-error* :accessor debug-on-unexpected-error-p :initarg :debug-on-unexpected-error-p :type boolean)
   (debug-on-assertion-failure-p :initform *debug-on-assertion-failure* :accessor debug-on-assertion-failure-p :initarg :debug-on-assertion-failure-p :type boolean)
   (toplevel-context :initform nil :accessor toplevel-context-of :initarg :toplevel-context)
   (current-test :initform nil :accessor current-test-of :initarg :current-test)
   (run-tests :initform (make-hash-table) :accessor run-tests-of :initarg :run-tests :documentation "test -> context mapping")
   (run-fixtures :initform (make-hash-table) :accessor run-fixtures-of :initarg :run-fixtures)
   (test-lambdas :initform (make-hash-table) :accessor test-lambdas-of :initarg :test-lambdas :documentation "test -> compiled test lambda mapping for this test run")))

(defprint-object (self global-context :identity #f :type #f)
  (let* ((failure-descriptions (failure-descriptions-of self))
         (total-failure-count (length failure-descriptions))
         (failed-assertion-count (count-if (rcurry #'typep 'failed-assertion) failure-descriptions))
         (unexpected-error-count (count-if (rcurry #'typep 'unexpected-error) failure-descriptions))
         (expected-count (count-if 'expected-p (failure-descriptions-of self))))
    (format t "test-run: ~A tests, ~A assertions, ~A failures in ~A sec~[~:; (~A failed assertions, ~A errors, ~A expected)~]"
            (hash-table-count (run-tests-of self))
            (assertion-count-of self)
            total-failure-count
            (bind ((toplevel-context (toplevel-context-of self))
                   (real-time-spent-in-seconds
                    (when toplevel-context
                      (real-time-spent-in-seconds toplevel-context))))
              (if (and toplevel-context
                       real-time-spent-in-seconds)
                  real-time-spent-in-seconds
                  "?"))
            total-failure-count ; index in the ~[] conditional
            failed-assertion-count
            unexpected-error-count
            (cond ((= expected-count total-failure-count)
                   "all")
                  ((zerop expected-count)
                   "none")
                  (t expected-count)))))

(defmacro without-test-progress-printing (&body body)
  (with-unique-names (old-state)
    `(let ((,old-state (print-test-run-progress-p *global-context*)))
      (unwind-protect
           (progn
             (setf (print-test-run-progress-p *global-context*) #f)
             ,@body)
        (setf (print-test-run-progress-p *global-context*) ,old-state)))))

(defmacro with-toplevel-restarts (&body body)
  `(block restart-wrapper
     (restart-bind
         ((continue-without-debugging
           (lambda ()
             (setf (debug-on-unexpected-error-p *global-context*) #f)
             (setf (debug-on-assertion-failure-p *global-context*) #f)
             (continue))
            :report-function (lambda (stream)
                               (format stream "~@<Turn off debugging for this test session and invoke the first CONTINUE restart~@:>")))
          (continue-without-debugging-errors
           (lambda ()
             (setf (debug-on-unexpected-error-p *global-context*) #f)
             (continue))
            :report-function (lambda (stream)
                               (format stream "~@<Do not stop at unexpected errors for the rest of this test session and continue by invoking the first CONTINUE restart~@:>")))
          (continue-without-debugging-assertions
           (lambda ()
             (setf (debug-on-assertion-failure-p *global-context*) #f)
             (continue))
            :report-function (lambda (stream)
                               (format stream "~@<Do not stop at failed assertions for the rest of this test session and continue by invoking the first CONTINUE restart~@:>")))
          (abort-testing
           (lambda ()
             (return-from restart-wrapper))
            :report-function (lambda (stream)
                               (format stream "~@<Abort the entire test session~@:>"))))
       (bind ((swank::*sldb-quit-restart* (find-restart 'abort-testing)))
         ,@body))))

(defun test-was-run-p (test)
  (declare (type testable test))
  (and (gethash test (run-tests-of *global-context*))
       (not (eq (current-test-of *global-context*) test))))

(defun register-test-being-run (test)
  (declare (type testable test))
  (setf (gethash test (run-tests-of *global-context*)) (current-context))
  (setf (current-test-of *global-context*) test))

(defgeneric get-test-lambda (test global-context)
  (:method ((test test) (context global-context))
    (bind (((:values test-lambda found-p) (gethash test (test-lambdas-of context))))
      (unless found-p
        (setf test-lambda (bind ((*package* (package-of test))
                                 (*readtable* (copy-readtable)))
                            (compile nil `(lambda ,(lambda-list-of test)
                                            ,@(body-of test)))))
        (setf (gethash test (test-lambdas-of context)) test-lambda))
      test-lambda)))

#+nil
(define-dynamic-context* context
  ((test)
   (internal-realtime-spent-with-test nil)
   (test-arguments)
   (number-of-added-failure-descriptions 0))
  :chain-parents #t)

(define-dynamic-context context
  ((test :accessor test-of :initarg :test)
   (internal-realtime-spent-with-test :initform nil :accessor internal-realtime-spent-with-test-of :initarg :internal-realtime-spent-with-test)
   (test-arguments :accessor test-arguments-of :initarg :test-arguments)
   (number-of-added-failure-descriptions :initform 0 :accessor number-of-added-failure-descriptions-of :initarg :number-of-added-failure-descriptions))
  :chain-parents #t)

(defprint-object (self context :identity #f :type #f)
  (format t "test-run ~@<(~S~{~^ ~S~})~@:>"
          (name-of (test-of self))
          (bind ((result (lambda-list-to-funcall-list (lambda-list-of (test-of self)))))
            (mapcar (lambda (arg-cell)
                      (setf result (substitute (cdr arg-cell) (car arg-cell) result :test #'eq)))
                    (test-arguments-of self))
            result)))

(defgeneric real-time-spent-in-seconds (context)
  (:method ((self context))
    (awhen (internal-realtime-spent-with-test-of self)
      (coerce (/ it
                 internal-time-units-per-second)
              'float))))

(defmacro run-failed-tests (&optional (test-result-place '*last-test-result*))
  `(with-new-global-context ()
     (if (> (length (failure-descriptions-of ,test-result-place)) 0)
         (progn
           (%run-failed-tests ,test-result-place)
           (push *global-context* *test-result-history*)
           (setf *last-test-result* *global-context*)
           (setf ,test-result-place *global-context*))
         (progn
           (warn "There are no failed tests in ~S" ',test-result-place)
           (values)))))

(defun %run-failed-tests (global-context-to-be-processed)
  (warn "Re-running failed tests without considering their dynamic environment, which may affect their behaviour!")
  (with-toplevel-restarts
    (iter (for failure :in-sequence (failure-descriptions-of global-context-to-be-processed))
          (for context = (elt (test-context-backtrace-of failure) 0))
          (apply (name-of (test-of context)) (mapcar #'cdr (test-arguments-of context))))
    (when (print-test-run-progress-p *global-context*)
      (terpri *debug-io*))))

(defun run-test-body-in-handlers (test function)
  (declare (type test test)
           (type function function))
  (register-test-being-run test)
  (labels ((prune-failure-descriptions ()
             ;; drop failures recorded by the previous run of this test
             (loop repeat (number-of-added-failure-descriptions-of *context*)
                do (vector-pop (failure-descriptions-of *global-context*)))
             (setf (number-of-added-failure-descriptions-of *context*) 0))
           (run-test-body ()
             (handler-bind
                 ((assertion-failed (lambda (c)
                                      (declare (ignore c))
                                      (unless (debug-on-assertion-failure-p *global-context*)
                                        (continue))))
                  (error (lambda (c)
                           (unless (typep c 'assertion-failed)
                             (record-failure* 'unexpected-error
                                              :description-initargs (list :condition c)
                                              :signal-assertion-failed #f)
                             (when (debug-on-unexpected-error-p *global-context*)
                               (invoke-debugger c))
                             (return-from run-test-body)))))
               (restart-case
                   (bind ((*package* (package-of test))
                          (*readtable* (copy-readtable))
                          (start-time (get-internal-run-time)))
                     (multiple-value-prog1
                         (funcall function)
                       (setf (internal-realtime-spent-with-test-of *context*)
                             (- (get-internal-run-time) start-time))))
                 (continue ()
                   :report (lambda (stream)
                             (format stream "~@<Skip the rest of the test ~S and continue by returning (values)~@:>" (name-of test)))
                   (values))
                 (retest ()
                   :report (lambda (stream)
                             (format stream "~@<Rerun the test ~S~@:>" (name-of test)))
                   ;; TODO: this will only prune the failures that were recorded in the current context.
                   ;; in case of nesting it will leave alone the failures recorded in deeper levels.
                   (prune-failure-descriptions)
                   (return-from run-test-body (run-test-body)))))))
    (run-test-body)))

(defun run-test-body (test function arguments toplevel-p)
  (declare (type test test))
  (bind ((result-values '()))
    (flet ((body ()
             (with-new-context (:test test :test-arguments arguments)
               (when toplevel-p
                 (setf (toplevel-context-of *global-context*) (current-context)))
               (setf result-values (multiple-value-list (run-test-body-in-handlers test function))))))
      (if toplevel-p
          (with-toplevel-restarts
            (body))
          (body))
      (if toplevel-p
          (progn
            (when (print-test-run-progress-p *global-context*)
              (terpri *debug-io*))
            (if result-values
                (values-list (append result-values (list *global-context*)))
                *global-context*))
          (values-list result-values)))))

(defmacro deftest (&whole whole name args &body body)
  (bind (((:values remaining-forms declarations documentation) (parse-body body :documentation #t :whole whole))
         ((name &rest test-args &key (compile-before-run *compile-tests-before-run*) in &allow-other-keys) (ensure-list name))
         (in-p (get-properties test-args '(:in))))
    (remove-from-plistf test-args :in)
    (unless (or (not (symbol-package name))
                (eq (symbol-package name) *package*))
      (warn 'test-style-warning :test name
            :format-control "Defining test on symbol ~S whose home package is not *package* which is ~A"
            :format-arguments (list name *package*)))
    (with-unique-names (test test-lambda global-context toplevel-p body)
      `(progn
        (eval-when (:load-toplevel :execute)
          (make-test ',name
           :package ,*package*
           :lambda-list ',args
           :declarations ',declarations
           :documentation ',documentation
           :body ',remaining-forms
           ,@(when in-p
                   (if in
                       `(:in (find-test ',in))
                       '(:in nil)))
           ,@test-args))
        (defun ,name ,args
          ,@(when documentation (list documentation))
          ,@declarations
          ,@(awhen *compile-tests-with-debug-level*
              `((declare (optimize (debug ,it)))))
          (bind ((,test (find-test ',name))
                 (,toplevel-p (not (has-global-context)))
                 (,global-context (unless ,toplevel-p
                                    (current-global-context))))
            ;; for convenience we define a function in a LABELS with the test name, so the debugger shows it in the backtrace
            (labels (,@(unless compile-before-run
                               `((,name ()
                                  ,@remaining-forms)))
                       (,body ()
                         ,(if compile-before-run
                              `(bind ((,test-lambda (get-test-lambda ,test ,global-context)))
                                (run-test-body ,test
                                 (lambda ()
                                   ;; TODO install a labels entry with the test name? to avoid compile at each recursion...
                                   ,(lambda-list-to-funcall-expression test-lambda args))
                                 ,(lambda-list-to-value-list-expression args)
                                 ,toplevel-p))
                              `(run-test-body ,test
                                #',name
                                ,(lambda-list-to-value-list-expression args)
                                ,toplevel-p))))
              (declare (dynamic-extent ,@(unless compile-before-run `(#',name))
                                       #',body))
              (if ,toplevel-p
                  (with-new-global-context ()
                    (setf ,global-context (current-global-context))
                    (push ,global-context *test-result-history*)
                    (setf *last-test-result* ,global-context)
                    (,body))
                  (,body)))))))))


(defmacro defixture (name &body body)
  "Fixtures are defun's that only execute the :setup part of their body once per test session if there is any at the time of calling."
  (with-unique-names (global-context nesting-count phase)
    (bind (setup-body
           teardown-body)
      (iter (for entry :in body)
            (if (and (consp body)
                     (member (first entry) '(:setup :teardown)))
                (ecase (first entry)
                  (:setup
                   (assert (not setup-body) () "Multiple :setup's for fixture ~S" name)
                   (setf setup-body (rest entry)))
                  (:teardown
                   (assert (not teardown-body) () "Multiple :teardown's for fixture ~S" name)
                   (setf teardown-body (rest entry))))
                (progn
                  (assert (and (not setup-body)
                               (not teardown-body))
                          () "Error parsing body of fixture ~A" name)
                  (setf setup-body body)
                  (leave))))
      `(defun ,name (&optional (,phase :setup))
        (declare (optimize (debug 3)))
        (bind ((,global-context (and (has-global-context)
                                     (current-global-context)))
               (,nesting-count (or (and ,global-context
                                        (gethash ',name (run-fixtures-of ,global-context)))
                                   0)))
          (assert (>= ,nesting-count 0))
          (ecase ,phase
            (:setup
             (incf ,nesting-count)
             (prog1
                 (if (and ,global-context
                          (> ,nesting-count 1))
                     #f
                     (progn
                       ,@setup-body
                       #t))
               (when ,global-context
                 (setf (gethash ',name (run-fixtures-of ,global-context)) ,nesting-count))))
            (:teardown
             (decf ,nesting-count)
             (prog1
                 (if (and ,global-context
                          (> ,nesting-count 0))
                     #f
                     (progn
                       (setf ,nesting-count 0)
                       ,@teardown-body
                       #t))
               (when ,global-context
                 (setf (gethash ',name (run-fixtures-of ,global-context))
                       ,nesting-count))))))))))

(defmacro with-fixture (name &body body)
  (with-unique-names (whole-fixture-body)
    `(flet ((,whole-fixture-body ()
             (,name :setup)
             (unwind-protect (progn ,@body)
               (block teardown-block
                 (handler-bind
                     ((serious-condition
                       (lambda (c)
                         (with-simple-restart
                             (continue ,(let ((*package* (find-package :common-lisp)))
                                          (format nil "Skip teardown of ~S and continue" name)))
                           (error 'error-in-teardown :condition c :fixture ',name))
                         (return-from teardown-block))))
                   (,name :teardown))))))
      (declare (dynamic-extent #',whole-fixture-body))
      (if (has-global-context)
          (,whole-fixture-body)
          (with-new-global-context ()
            (,whole-fixture-body))))))

(defmacro with-fixtures (fixtures &body body)
  (if fixtures
      `(with-fixture ,(first fixtures)
        (with-fixtures ,(rest fixtures)
          ,@body))
      `(progn ,@body)))

(defun record-failure (description-type &rest args)
  (record-failure* description-type :description-initargs args))

(defun record-failure* (type &key (signal-assertion-failed #t) description-initargs)
  (bind ((description (apply #'make-instance type
                             :test-context-backtrace (when (has-context)
                                                       (iter (for context :first (current-context) :then (parent-context-of context))
                                                             (while context)
                                                             (collect context)))
                             description-initargs)))
    (if (and (has-global-context)
             (has-context))
        (progn
          (vector-push-extend description (failure-descriptions-of *global-context*))
          (incf (number-of-added-failure-descriptions-of *context*))
          (write-progress-char (progress-char-of description))
          (when signal-assertion-failed
            (restart-case (error 'assertion-failed
                                 :test (test-of *context*)
                                 :failure-description description)
              (continue ()
                :report (lambda (stream)
                          (format stream "~@<Roger, go on testing...~@:>"))))))
        (progn
          (describe description *debug-io*)
          (when *debug-on-assertion-failure* ; we have no *global-context*
            (restart-case (error 'assertion-failed
                                 :failure-description description)
              (continue ()
                :report (lambda (stream)
                          (format stream "~@<Ignore the failure and continue~@:>")))))))))

(defun extract-assert-expression-and-message (input-form)
  (bind ((negatedp #f)
         (predicate)
         (arguments '()))
    (labels ((process (form)
               (if (consp form)
                   (case (first form)
                     ((not)
                      (assert (= (length form) 2))
                      (setf negatedp (not negatedp))
                      (process (second form)))
                     (t (setf predicate (first form))
                        (setf arguments (rest form))))
                   (setf predicate form))))
      (process input-form)
      (cond ((ignore-errors
               (macro-function predicate))
             (values '() input-form "Macro expression ~A evaluated to false." (list `(quote ,input-form))))
            ((ignore-errors
               (fdefinition predicate))
             (cond ((= (length arguments) 0)
                    (values '()
                            input-form
                            "Expression ~A evaluated to false."
                            (list `(quote ,input-form))))
                   ((= (length arguments) 2)
                    (with-unique-names (x y)
                      (values `((,x ,(first arguments))
                                (,y ,(second arguments)))
                              (if negatedp
                                  `(not (,predicate ,x ,y))
                                  `(,predicate ,x ,y))
                              "Binary predicate ~A failed.~%~
                               x: ~S => ~S~%~
                               y: ~S => ~S"
                              (list (if negatedp
                                        `(quote (not (,predicate x y)))
                                        `(quote (,predicate x y)))
                                    `(quote ,(first arguments)) x
                                    `(quote ,(second arguments)) y))))
                   (t (bind ((arg-values (mapcar (lambda (el)
                                                   (unless (keywordp el)
                                                     (gensym)))
                                                 arguments))
                             (bindings (iter (for arg :in arguments)
                                             (for arg-value :in arg-values)
                                             (when arg-value
                                               (collect `(,arg-value ,arg)))))
                             (expression-values (mapcar (lambda (arg-value argument)
                                                          (or arg-value argument))
                                                        arg-values
                                                        arguments))
                             (expression (if negatedp
                                             `(not (,predicate ,@expression-values))
                                             `(,predicate ,@expression-values)))
                             ((:values message message-args) (iter (with message = "Expression ~A evaluated to ~A")
                                                                  (for arg :in arguments)
                                                                  (for idx :upfrom 0)
                                                                  (for arg-value :in arg-values)
                                                                  (when arg-value
                                                                    (setf message (concatenate 'string message "~%~D: ~A => ~S"))
                                                                    (appending `(,idx (quote ,arg) ,arg-value) :into message-args))
                                                                  (finally (return (values message message-args))))))
                        (values bindings
                                expression
                                message
                                (nconc (list `(quote (,predicate ,@arguments)) (if negatedp "true" "false")) message-args))))))
            (t
             (values '() input-form "Expression ~A evaluated to false." (list `(quote ,input-form))))))))

(defun write-progress-char (char)
  (bind ((global-context (when (boundp '*global-context*)
                           *global-context*)))
    (when (and global-context
               (print-test-run-progress-p global-context))
      (when (and (not (zerop (progress-char-count-of global-context)))
                 (zerop (mod (progress-char-count-of global-context)
                             *test-progress-print-right-margin*)))
        (terpri *debug-io*))
      (incf (progress-char-count-of global-context)))
    (when (or (and global-context
                   (print-test-run-progress-p global-context))
              (and (not global-context)
                   *print-test-run-progress*))
      (write-char char *debug-io*))))

(defun register-assertion-was-successful ()
  (write-progress-char #\.))

(defun register-assertion ()
  (when (boundp '*global-context*)
    (incf (assertion-count-of *global-context*))))

(defmacro is (&whole whole form &optional (message nil message-p) &rest message-args)
  (bind (((:values bindings expression message message-args)
          (if message-p
              (values nil form message message-args)
              (extract-assert-expression-and-message form))))
    (with-unique-names (result)
      `(progn
        (register-assertion)
        (bind ,bindings
          (bind ((,result (multiple-value-list ,expression)))
            (if (first ,result)
                (register-assertion-was-successful)
                (record-failure 'failed-assertion :form ',whole
                                :format-control ,message :format-arguments (list ,@message-args)))
            (values-list ,result)))))))

(defmacro signals (what &body body)
  (bind ((condition-type what))
    (unless (symbolp condition-type)
      (error "SIGNALS expects a symbol as condition-type! (Is there a superfulous quote at ~S?)" condition-type))
    `(progn
      (register-assertion)
      (block test-block
        (handler-bind ((,condition-type
                        (lambda (c)
                          (register-assertion-was-successful)
                          (return-from test-block c))))
          ,@body)
        (record-failure 'missing-condition
                        :form (list* 'progn ',body)
                        :condition ',condition-type)
        (values)))))

(defmacro finishes (&body body)
  `(progn
    (register-assertion)
    (multiple-value-prog1
        (progn
          ,@body)
      (register-assertion-was-successful))))

(defmacro runs-without-failure? (&body body)
  (with-unique-names (old-failure-count)
    `(bind ((,old-failure-count (length (failure-descriptions-of *global-context*))))
       ,@body
       (= ,old-failure-count (length (failure-descriptions-of *global-context*))))))

(defmacro with-expected-failures (&body body)
  "Any failure inside the dynamic extent of this block is registered as an expected failure."
  `(bind ((*failures-and-errors-are-expected* #t))
     ,@body))



;;;;;;;;;;;;;;;;;;;;;;;;
;;; some utils

(define-condition illegal-lambda-list (error)
  ((lambda-list :accessor lambda-list-of :initarg :lambda-list)))

(defun illegal-lambda-list (lambda-list)
  (error 'illegal-lambda-list :lambda-list lambda-list))

(defun parse-lambda-list (lambda-list visitor &key macro)
  ;; TODO finish macro lambda list parsing
  (declare (optimize (speed 3))
           (type list lambda-list)
           (type (or symbol function) visitor))
  (let ((args lambda-list))
    (labels
        ((fail ()
           (illegal-lambda-list lambda-list))
         (ensure-list (list)
           (if (listp list)
               list
               (list list)))
         (process-&whole ()
           (assert (eq (first args) '&whole))
           (pop args)
           (unless macro
             (fail))
           (let ((whole (pop args)))
             (unless whole
               (fail))
             (funcall visitor '&whole whole whole))
           (case (first args)
             (&key          (entering-&key))
             (&rest         (process-&rest))
             (&optional     (entering-&optional))
             (&body         (process-&body))
             (&environment  (process-&environment))
             ((&whole &aux &allow-other-keys) (fail))
             (t             (process-required))))
         (process-&body ()
           (assert (eq (first args) '&body))
           (pop args)
           (unless macro
             (fail))
           (let ((body (pop args)))
             (unless (null args)
               (fail))
             (unless body
               (fail))
             (funcall visitor '&body body body)))
         (process-&environment ()
           (assert (eq (first args) '&environment))
           (pop args)
           (unless macro
             (fail))
           (let ((env (pop args)))
             (unless env
               (fail))
             (funcall visitor '&environment env env))
           (case (first args)
             (&key          (entering-&key))
             (&rest         (process-&rest))
             (&optional     (entering-&optional))
             (&body         (process-&body))
             (&aux          (process-&aux))
             ((&whole &environment &allow-other-keys) (fail))
             (t             (process-required))))
         (process-required ()
           (unless args
             (done))
           (case (first args)
             (&key          (entering-&key))
             (&rest         (process-&rest))
             (&optional     (entering-&optional))
             (&body         (process-&body))
             (&environment  (process-&environment))
             ((&whole &allow-other-keys) (fail))
             (&aux          (entering-&aux))
             (t
              (let ((arg (pop args)))
                (funcall visitor nil arg arg))
              (process-required))))
         (process-&rest ()
           (assert (eq (first args) '&rest))
           (pop args)
           (let ((rest (pop args)))
             (unless rest
               (fail))
             (funcall visitor '&rest rest rest))
           (unless args
             (done))
           (case (first args)
             (&key               (entering-&key))
             (&environment       (process-&environment))
             ((&whole &optional &rest &body &allow-other-keys) (fail))
             (&aux               (entering-&aux))
             (t                  (fail))))
         (entering-&optional ()
           (assert (eq (first args) '&optional))
           (pop args)
           (process-&optional))
         (process-&optional ()
           (unless args
             (done))
           (case (first args)
             (&key               (entering-&key))
             (&rest              (process-&rest))
             (&body              (process-&body))
             ((&whole &optional &environment &allow-other-keys) (fail))
             (&aux               (entering-&aux))
             (t
              (let* ((arg (ensure-list (pop args)))
                     (name (first arg))
                     (default (second arg)))
                (funcall visitor '&optional name arg nil default))
              (process-&optional))))
         (entering-&key ()
           (assert (eq (first args) '&key))
           (pop args)
           (process-&key))
         (process-&key ()
           (unless args
             (done))
           (case (first args)
             (&allow-other-keys       (funcall visitor '&allow-other-keys nil nil))
             ((&key &optional &whole &environment &body) (fail))
             (&aux                    (entering-&aux))
             (t
              (let* ((arg (ensure-list (pop args)))
                     (name-part (first arg))
                     (default (second arg))
                     (external-name (if (consp name-part)
                                        (progn
                                          (unless (= (length name-part) 2)
                                            (illegal-lambda-list lambda-list))
                                          (first name-part))
                                        (intern (symbol-name name-part) #.(find-package "KEYWORD"))))
                     (local-name (if (consp name-part)
                                     (second name-part)
                                     name-part)))
                (funcall visitor '&key local-name arg external-name default))
              (process-&key))))
         (entering-&aux ()
           (assert (eq (first args) '&aux))
           (pop args)
           (process-&aux))
         (process-&aux ()
           (unless args
             (done))
           (case (first args)
             ((&whole &optional &key &environment &allow-other-keys &aux &body) (fail))
             (t
              (let ((arg (ensure-list (pop args))))
                (funcall visitor '&aux (first arg) arg))
              (process-&aux))))
         (done ()
           (return-from parse-lambda-list (values))))
      (when args
        (case (first args)
          (&whole (process-&whole))
          (t      (process-required)))))))

(defun lambda-list-to-funcall-list (args)
  (let ((result (list))
        (rest-variable-name nil))
    (parse-lambda-list args
                       (lambda (kind name entry &optional external-name default)
                         (declare (ignore entry default))
                         (case kind
                           (&key
                            (push external-name result)
                            (push name result))
                           (&allow-other-keys)
                           (&rest (setf rest-variable-name name))
                           (t (push name result)))))
    (values (nreverse result)
            rest-variable-name)))

(defun lambda-list-to-lambda-list-with-quoted-defaults (args)
  (let ((primaries (list))
        (keywords (list))
        (optionals (list))
        (rest-variable-name nil)
        (allow-other-keys? nil))
    (parse-lambda-list args
                       (lambda (kind name entry &optional external-name default)
                         (declare (ignore entry))
                         (ecase kind
                           (&key
                            (push `((,external-name ,name) (quote ,default)) keywords))
                           (&optional
                            (push `(,name ,default) optionals))
                           (&allow-other-keys (setf allow-other-keys? t))
                           (&rest (setf rest-variable-name name))
                           ((nil) (push name primaries)))))
    (values `(,@(nreverse primaries)
              ,@(when optionals (cons '&optional (nreverse optionals)))
              ,@(when keywords (cons '&key (nreverse keywords)))
              ,@(when allow-other-keys? (list '&allow-other-keys)))
            rest-variable-name)))

(defun lambda-list-to-funcall-expression (function args)
  (multiple-value-bind (arg-list rest-variable)
      (lambda-list-to-funcall-list args)
    (if rest-variable
        `(apply ,function ,@arg-list ,rest-variable)
        `(funcall ,function ,@arg-list))))

(defun lambda-list-to-value-list-expression (args)
  `(list ,@(let ((result (list)))
             (parse-lambda-list args
                                (lambda (kind name entry &optional external-name default)
                                  (declare (ignore entry external-name default))
                                  (case kind
                                    (&allow-other-keys)
                                    (t (push `(cons ',name ,name) result)))))
             (nreverse result))))

(defun lambda-list-to-variable-name-list (args &key macro include-specials)
  (let ((result (list))
        (rest-variable-name nil)
        (whole-variable-name nil)
        (env-variable-name nil))
    (parse-lambda-list args
                       (lambda (kind name entry &optional external-name default)
                         (declare (ignore entry external-name default))
                         (case kind
                           (&allow-other-keys )
                           (&environment      (setf env-variable-name name)
                                              (when include-specials
                                                (push name result)))
                           (&whole            (setf whole-variable-name name)
                                              (when include-specials
                                                (push name result)))
                           ((&rest &body)     (setf rest-variable-name name)
                                              (when include-specials
                                                (push name result)))
                           (t                 (push name result))))
                       :macro macro)
    (values (nreverse result)
            rest-variable-name
            whole-variable-name
            env-variable-name)))

(defun funcall-test-with-feedback-message (test-function &rest args)
  "Run the given test non-interactively and print the results to *standard-output*.
This function is ideal for ASDF:TEST-OP's."
  (aprog1
      (without-debugging (apply test-function args))
    (let ((*package* (find-package :common-lisp)))
      (format *standard-output*
"The result of ~S is:

  ~A

For more details run it from the REPL and use the customized Slime inspector
to inspect the results (ASDF eats up the return values). Some inspector
features may only be available when using the Slime branch at
darcs get --lazy http://common-lisp.net/project/cl-dwim/darcs/slime
but the official Slime should also work fine.~%"
              test-function it))))

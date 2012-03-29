;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :stefil)

#.(file-header)

(unless (assoc "STEFIL" *readtable-alist* :test #'string=)
  (bind ((*readtable* (copy-readtable)))
    (setup-readtable)
    (push (cons "STEFIL" *readtable*) *readtable-alist*)
    (push (cons "STEFIL-TEST" *readtable*) *readtable-alist*)))

#+slime-dwim-branch
(progn

(defun stefil-inspector-lookup-hook (form)
  (when (symbolp form)
    (let ((test (find-test form :otherwise nil)))
      (when test
        (values test t)))))

(when (boundp 'swank::*inspector-dwim-lookup-hooks*)
  (pushnew 'stefil-inspector-lookup-hook swank::*inspector-dwim-lookup-hooks*))

(defvar *display-all-slots-in-inspector* #f)

(defun rerun-test-action-for-inspector (context inspector)
  (lambda ()
    (apply (name-of (test-of context))
           (mapcar #'cdr (test-arguments-of context)))
    (swank::inspect-object *last-test-result* inspector)))

(defun run-test-action-for-inspector (test inspector)
  (lambda ()
    (funcall (name-of test))
    (swank::inspect-object *last-test-result* inspector)))

(defun present-test-for-emacs (test inspector &key name-only undefine-action actions-first)
  (when test
    (bind ((actions `(,@(if (lambda-list-of test)
                            (when actions-first
                              `((:label "[run]")))
                            `((:action "[run]" ,(run-test-action-for-inspector test inspector))))
                      ,@(when undefine-action
                          (if (find-test (name-of test) :otherwise nil)
                              (when actions-first
                                `((:label "[undefine]")))
                              `((:action "[undefine]" ,(lambda () (rem-test (name-of test)))))))))
           (value `((:value ,(if name-only (name-of test) test)))))
      (if actions-first
          (append actions '(" ") value)
          (append value '(" ") actions)))))

(defun present-context-for-emacs (context inspector)
  `((:value ,context) " " (:action "[rerun]" ,(rerun-test-action-for-inspector context inspector))))

(defun present-test-backtrace-for-emacs (description inspector)
  (iter (for context :in (test-context-backtrace-of description))
        (for idx :upfrom 0)
        (when (first-time-p)
          (appending `((:newline) (:label "Test backtrace:") (:newline))))
        (collect (format nil "  ~D: " idx))
        (appending (present-context-for-emacs context inspector))
        (collect `(:newline))))

(defun present-all-slots-for-emacs (object inspector)
  (if *display-all-slots-in-inspector*
      (append `((:newline)
                (:action "[hide slots]" ,(lambda () (setf *display-all-slots-in-inspector* #f)))
                (:newline))
              (swank::all-slots-for-inspector object inspector))
      `((:newline)
        (:action "[show all slots]" ,(lambda () (setf *display-all-slots-in-inspector* #t))))))

#-slime-dwim-branch
(defun drop-labels (content)
  (iter (for el :in content)
        (if (and (consp el)
                 (eq :label (first el)))
            (collect (second el))
            (collect el))))

(defmacro inspector-result (title content)
  #+slime-dwim-branch
  `(list :title ,title :type nil :content ,content)
  #-slime-dwim-branch
  `(values ,title (drop-labels ,content)))

(defmethod inspect-for-emacs ((global-context global-context) inspector)
  (inspector-result
   "Stefil test results"
   `((:label "Executed tests: ") (:value ,(hash-table-values (run-tests-of global-context))
                                  ,(princ-to-string (hash-table-count (run-tests-of global-context)))) (:newline)
     (:label "Executed assertions: ") ,(princ-to-string (assertion-count-of global-context))
     (:newline) (:newline)
     ,@(unless (emptyp (failure-descriptions-of global-context))
               `((:label ,(format nil "List of failures (~A): " (length (failure-descriptions-of global-context))))
                 (:action "[rerun all failed tests]"
                  ,(lambda () (swank::inspect-object (run-failed-tests global-context) inspector)))
                 (:newline)))
     ;; intentionally reverse the order by push'ing
     ,@(iter (for description :in-vector (failure-descriptions-of global-context))
             (for context = (first (test-context-backtrace-of description)))
             (collect "  ")
             (collect `(:action "[rerun]" ,(rerun-test-action-for-inspector context inspector)))
             (collect " ")
             (collect `(:value ,description))
             (collect `(:newline)))
     ,@(present-all-slots-for-emacs global-context inspector))))

(defmethod inspect-for-emacs ((context context) inspector)
  (inspector-result
   "Stefil test context"
   `((:label "Test:                    ") (:value ,(test-of context)) " " (:action "[rerun]" ,(rerun-test-action-for-inspector context inspector)) (:newline)
     (:label "Test arguments:          ") ,@(awhen (test-arguments-of context) `((:value ,it))) (:newline)
     (:label "Real time spent in body: ") ,(princ-to-string (real-time-spent-in-seconds context)) (:label " sec")
     (:newline) (:newline)
     ,@(iter (for parent-context :first (parent-context-of context) :then (parent-context-of parent-context))
             (while parent-context)
             (when (first-time-p)
               (collect `(:label "Parent test frames:"))
               (collect `(:newline)))
             (collect "  ")
             (appending (reverse (present-context-for-emacs parent-context inspector)))
             (collect `(:newline)))
     ,@(present-all-slots-for-emacs context inspector))))

(defmethod inspect-for-emacs ((failure failed-assertion) inspector)
  (inspector-result
   "Failed Stefil assertion"
   `((:label "Form: ") (:value ,(form-of failure)) (:newline)
     ,@(present-test-backtrace-for-emacs failure inspector)
     ,@(present-all-slots-for-emacs failure inspector))))

(defmethod inspect-for-emacs ((description unexpected-error) inspector)
  (inspector-result
   "Unexpected error in a Stefil test"
   `("Condition: " (:value ,(condition-of description)) (:newline)
     ,@(present-test-backtrace-for-emacs description inspector)
     ,@(present-all-slots-for-emacs description inspector))))

(defmethod inspect-for-emacs ((test test) inspector)
  (inspector-result
   "Stefil test"
   `((:label "Name:                    ") ,@(present-test-for-emacs test inspector :undefine-action #t :name-only #t) (:newline)
     (:label "Package:                 ") (:value ,(package-of test)) (:newline)
     (:label "Compile before run?:     ") ,(if (compile-before-run-p test) "yes" "no") (:newline)
     (:label "Auto call by its suite?: ") ,(if (auto-call-p test) "yes" "no") (:newline)
     (:label "Documentation:           ") ,@(when (documentation-of test) `((:value ,(documentation-of test)))) (:newline)
     (:label "Parent:                  ") ,@(present-test-for-emacs (parent-of test) inspector) (:newline)
     ,@(iter (for (nil child) :in-hashtable (children-of test))
             (when (first-time-p)
               (appending `((:newline) (:label "Children:") (:newline))))
             (collect "  ")
             (appending (present-test-for-emacs child inspector :actions-first #t))
             (collect `(:newline)))
     ,@(present-all-slots-for-emacs test inspector))))

) ; slime head changed too much, turn off the customizations on it

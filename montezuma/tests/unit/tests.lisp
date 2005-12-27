(in-package #:montezuma)

;; Some simple unit test utilities

(defparameter *passed-tests* '())
(defparameter *failed-tests* '())

(defmacro test (name expr expected-value &optional (comparator '(function equal))
		failure-code)
  `(unless (test-aux ',name ',expr ,expr ,expected-value ,comparator)
    ,failure-code))

(defmacro atest (prefix expr expected-value &optional (comparator '(function equal))
		 failure-code)
  `(unless (test-aux (gensym (string ',prefix)) ',expr ,expr ,expected-value ,comparator)
     ,failure-code))

(defmacro condition-test (name expr expected-condition &optional (comparator '(function typep))
			  failure-code)
  (let ((completed-var (gensym "COMPLETED"))
	(condition-var (gensym "CONDITION"))
	(value-var (gensym "VALUE")))
    `(let ((,completed-var NIL))
       (multiple-value-bind (,value-var ,condition-var)
	   (ignore-errors
	     ,expr
	     (setf ,completed-var T))
	 (unless (condition-test-aux ',name ',expr ,value-var (not ,completed-var)
				     ,condition-var ,expected-condition ,comparator)
	   ,failure-code)))))

(defun condition-test-aux (name expr value error-p error expected-error comparator)
  (if error-p
      (let ((got-expected-p (funcall comparator error expected-error)))
	(if got-expected-p
	    (test-success name expr error expected-error)
	    (test-failure name expr error expected-error))
	got-expected-p)
      (test-failure name expr value expected-error)))

(defun test-aux (name expr value expected-value comparator)
  (let ((got-expected-p (funcall comparator value expected-value)))
    (if got-expected-p
	(test-success name expr value expected-value)
	(test-failure name expr value expected-value))
    got-expected-p))

(defun test-failure (name expr value expected-value)
  (assert (not (assoc name *failed-tests*)) nil "There is already a test named ~S." name)
  (assert (not (assoc name *passed-tests*)) nil "There is already a test named ~S." name)
  (push (cons name (list expr value expected-value)) *failed-tests*)
  (warn "FAILURE: Test ~S: ~S evaluated to ~S instead of ~S."
	name expr value expected-value)
  (format T "F")
  nil)

(defun test-success (name expr value expected-value)
  (assert (not (assoc name *failed-tests*)) nil "There is already a test named ~S." name)
  (assert (not (assoc name *passed-tests*)) nil "There is already a test named ~S." name)
  (push (cons name (list expr value expected-value)) *passed-tests*)
;;  (format T "~&Test ~S passed.~%" name)
  (format T ".")
)

(defun begin-tests ()
  (setf *passed-tests* '())
  (setf *failed-tests* '()))

(defun end-tests ()
  (let ((num-failed (length *failed-tests*))
	(num-passed (length *passed-tests*)))
    (format T "~&-----~&Testing complete, ~S of ~S tests failed (~,2F)"
	    num-failed
	    (+ num-failed num-passed)
	    (/ num-failed (+ num-failed num-passed)))
    (= num-failed 0)))


(defvar *test-functions* '())

(defun add-test-function (name function)
  (let ((pair (assoc name *test-functions*)))
    (if pair
	(setf (cdr pair) function)
	(setf *test-functions* (append *test-functions* (list (cons name function))))))
  *test-functions*)

(defun clear-test-functions ()
  (setf *test-functions* '()))

(defmacro deftestfun (name &body body)
  `(progn (defun ,name ()
	      ,@body)
	  (add-test-function ',name
		      (function ,name))))


(defun run-tests ()
  (begin-tests)
  (dolist (pair *test-functions*)
    (destructuring-bind (name . function) pair
      (format T "~&;; ~S" name)
      (funcall function)))
  (end-tests))

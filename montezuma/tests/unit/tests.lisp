(in-package #:montezuma) ; FIXME: should probably move this to another package.

;; Some simple unit test utilities

(defvar *trap-errors* T)
(defvar *break-on-failure* T)


;; --------------------
;; Simple Tests
;; --------------------

;; The test macro is used for tests that are only ever run once.
;; atest is used for tests that may run multiple times in a test suite
;; (e.g., I might have a test function that runs tests on several
;; subclasses of some superclass.  Dumb.

(defmacro test (name expr expected-value &optional (comparator '(function equal)) failure-code)
  `(flet ((test-thunk () ,expr)
	  (failure-thunk () ,failure-code))
     (execute-test-thunk ',name ',expr #'test-thunk ,expected-value ,comparator #'failure-thunk)))

(defmacro atest (prefix expr expected-value &optional (comparator '(function equal)) failure-code)
  `(flet ((test-thunk () ,expr)
	  (failure-thunk () ,failure-code))
     (execute-test-thunk (gensym (format nil "~A-" ',prefix)) ',expr #'test-thunk ,expected-value ,comparator #'failure-thunk)))


;; ----------

(define-condition test-failure (error)
  ())

(defun fail-test (condition)
  (invoke-restart 'fail-test condition))

(defun maybe-fail-test (condition)
  (when (and *trap-errors* (not (typep condition 'test-failure)))
    (fail-test condition)))

(defun execute-test-thunk (name expr test-thunk expected-value comparator failure-thunk)
  (flet ((handle-test-success (value)
	   (test-success name expr value expected-value))
	 (handle-test-failure (value condition)
	   (test-failure name expr value expected-value condition)
	   (when failure-thunk (funcall failure-thunk))))
    (restart-case 
	(handler-bind ((error #'maybe-fail-test))
	  (let ((value (funcall test-thunk)))
	    (if (funcall comparator value expected-value)
		(handle-test-success value)
		(handle-test-failure value nil))))
      (fail-test (condition)
	(handle-test-failure nil condition)))))

#||
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
||#


(defparameter *passed-tests* '())
(defparameter *failed-tests* '())

(defun test-failure (name expr value expected-value condition)
  (assert (not (assoc name *failed-tests*)) nil "There is already a test named ~S." name)
  (assert (not (assoc name *passed-tests*)) nil "There is already a test named ~S." name)
  (push (cons name (list expr value expected-value)) *failed-tests*)
  (if (not condition)
      (warn "FAILURE: Test ~S: ~S evaluated to ~S instead of ~S."
	    name expr value expected-value)
      (let ((condition-report-string (format nil "~A" condition)))
	(warn "FAILURE: Test ~S: ~S signalled ~S (~S) instead of returning ~S" name expr condition condition-report-string expected-value)))
  (format T "F")
  (when *break-on-failure* (error 'test-failure))
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



;; --------------------
;; Test Functions
;; --------------------

;; deftestfun defines a test function that will be called when
;; (run-tests) is executed (and can be run individually by
;; run-test-named).

(defmacro deftestfun (name &body body)
  `(progn (defun ,name ()
	    (format T "~&;; ~S " ',name)
	    ,@body)
	  (add-test-function ',name
			     (function ,name))))

(defun run-tests ()
  (begin-tests)
  (dolist (pair *test-functions*)
      (funcall (cdr pair)))
  (end-tests))

(defun run-test-named (name)
  (begin-tests)
  (let ((test (assoc name *test-functions*)))
    (if test
	(funcall (cdr test))
	(error "There is no test named ~S." name))))


;; ----------

(defvar *test-functions* '())

(defun add-test-function (name function)
  (let ((pair (assoc name *test-functions*)))
    (if pair
	(setf (cdr pair) function)
	(setf *test-functions* (append *test-functions* (list (cons name function))))))
  *test-functions*)

(defun clear-test-functions ()
  (setf *test-functions* '()))


;; --------------------
;; Test Fixtures
;; --------------------

;; Fixtures have state (fixture vars), an optional setup function, an
;; optional teardown function, and a set of test functions.

(defmacro deftestfixture (name &rest clauses)
  (let ((vars (collect-fixture-clauses :vars clauses))
	(setups (collect-fixture-clauses :setup clauses))
	(tests (collect-fixture-clauses :testfun clauses))
	(teardowns (collect-fixture-clauses :teardown clauses)))
    (assert (<= (length vars) 1))
    (assert (<= (length setups) 1))
    (assert (<= (length teardowns) 1))
    (deftestfixture-expr name
	(cdr (first vars))
      (cdr (first setups))
      (cdr (first teardowns))
      tests)))


;; ----------

(defstruct test-fixture
  name
  (vars (make-hash-table))
  setup
  teardown
  test-functions)

(defun run-fixture (fixture)
  (let ((setup-fn (test-fixture-setup fixture)))
    (when setup-fn
      (funcall setup-fn fixture)))
  (dolist (test (test-fixture-test-functions fixture))
    (funcall test fixture))
  (let ((teardown-fn (test-fixture-teardown fixture)))
    (when teardown-fn
      (funcall teardown-fn fixture))))


(defun collect-fixture-clauses (name clauses)
  (remove-if-not #'(lambda (clause)
		     (eq (car clause) name))
		 clauses))

(defun make-fixture-method (expr)
  `(function (lambda (fixture)
     (flet ((fixture-var (name)
	      (gethash name (test-fixture-vars fixture)))
	    ((setf fixture-var) (value name)
	      (setf (gethash name (test-fixture-vars fixture)) value)))
       (declare (ignorable (function fixture-var) (function (setf fixture-var))))
       ,expr))))

(defun make-fixture-test-function-defn (name expr)
  (let ((fixture-var (gensym "FIXTURE")))
    `(defun ,name (,fixture-var)
       (format T "~&;; ~S " ',name)
       (funcall ,(make-fixture-method expr) ,fixture-var))))
	   
    
(defun deftestfixture-expr (name vars setup-exprs teardown-exprs test-case-functions)
  (declare (ignore vars))
  (let ((fixture-var (gensym "FIXTURE")))
    `(progn
       ,@(mapcar #'(lambda (test-case-function)
		     (make-fixture-test-function-defn (second test-case-function)
					      `(progn ,@(cdr (cdr test-case-function)))))
		 test-case-functions)
       (let ((,fixture-var (make-test-fixture
			    :name ',name
			    ,@(if setup-exprs
				  `(:setup ,(make-fixture-method `(progn ,@setup-exprs)))
				  '())
			    ,@(if teardown-exprs
				  `(:teardown ,(make-fixture-method `(progn ,@setup-exprs)))
				  '())
			    :test-functions (list ,@(mapcar #'(lambda (test-case-function)
							      `(function ,(second test-case-function)))
							  test-case-functions)))))
	 (add-test-function (intern (format nil "~A" ',name))
			    #'(lambda () (run-fixture ,fixture-var)))))))

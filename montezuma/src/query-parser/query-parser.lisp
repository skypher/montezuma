(in-package #:montezuma)

;; This is just a dumb stub implementation for now so I can write unit
;; tests for index.lisp.


(defclass query-parser ()
  ((field :initform nil)
   (default-field :initarg :default-field)
   (analyzer :initarg :analyzer)
   (wild-lower :initarg :wild-lower)
   (occur-default :initarg :occur-default)
   (default-slop :initarg :default-slop)
   (fields :accessor fields)
   (handle-parse-errors :initarg :handle-parse-errors))
  (:default-initargs
   :default-field "*"
    :analyzer (make-instance 'analyzer)
    :wild-lower T
    :occur-default :should-occur
    :default-slop 0
    :handle-parse-errors NIL))

(defmethod initialize-instance :after ((self query-parser) &key)
  (with-slots (field default-field) self
    (when (null field)
      (setf field default-field))))

(defmethod add-and-clause ((parser query-parser) clauses clause)
  (setf clauses (if (listp clauses) clauses (list clauses)))
  (setf clauses (remove nil clauses))
  (when (= (length clauses) 1)
    (let ((last-clause (first clauses)))
      (when (not (prohibited? last-clause))
	(setf (occur last-clause) :must-occur))))
  (if clause
      (progn
	(unless (prohibited? clause)
	  (setf (occur clause) :must-occur))
	(cons clause clauses))
      clauses))

(defmethod add-or-clause ((parser query-parser) clauses clause)
  (cons clause (if (listp clauses) clauses (list clauses))))

(defmethod add-default-clause ((parser query-parser) clauses clause)
  (if (eq (slot-value parser 'occur-default) :must-occur)
      (add-and-clause parser clauses clause)
      (add-or-clause parser clauses clause)))

(defmethod get-term-query ((parser query-parser) word)
  (make-instance 'term-query
		 :term (make-term (use-active-field parser)
				  word)))

(defmethod get-boolean-clause ((parser query-parser) query occur)
  (make-instance 'boolean-clause
		 :query query
		 :occur occur))

(defmethod get-boolean-query ((parser query-parser) clauses)
  (let ((q (make-instance 'boolean-query)))
    (dolist (clause (if (listp clauses) clauses (list clauses)))
      (add-clause q clause))
    q))

(defmethod add-word-to-phrase ((parser query-parser) phrase word)
  (append (if (listp phrase) phrase (list phrase)) (list word)))

(defmethod get-phrase-query ((parser query-parser) words)
  (let ((q (make-instance 'phrase-query))
	(field (use-active-field parser)))
    (dolist (word (if (listp words) words (list words)))
      (add-term-to-query q (make-term field word)))
    q))

(defmethod get-wild-query ((parser query-parser) word)
  (make-instance 'wildcard-query
		 :term (make-term (use-active-field parser)
				  word)))

(defmethod set-query-field ((parser query-parser) field)
  (setf (slot-value parser 'field) field))

(defmethod use-active-field ((parser query-parser))
  (if (slot-value parser 'field)
      (prog1 (slot-value parser 'field)
	(setf (slot-value parser 'field) nil))
      (slot-value parser 'default-field)))



;; Note that the following grammar rules must be used in a parser
;; created with a parselet form, with a lexical context that includes
;; a "parser" binding.

(defprod req-op () "+")
(defprod not-op () "!")

(defprod top-query ()
  (^ bool-query
     (get-boolean-query parser bool-query)))
     

(defprod bool-query ()
  ((^ bool-clause)
   (* (^ ((* white-space) bool-clause)
	 (add-default-clause parser bool-query bool-clause)))))

(defprod bool-clause ()
  (/ (^ (req-op query)
	(get-boolean-clause parser query :must-occur))
     (^ (not-op query)
	(get-boolean-clause parser query :must-not-occur))
     (^ query 
	(get-boolean-clause parser query :should-occur))))

(defprod query ()
  (/ (^ wild-query)
     (^ field-query)
     (^ term-query)
     (^ phrase-query
	(get-phrase-query parser phrase-query))))

(defprod term-query ()
  (^ word (get-term-query parser word)))

(defprod phrase-query ()
  ("\""
   (^ word)
   (* (^ ((* white-space) word)
	 (add-word-to-phrase parser phrase-query word)))
   "\""))

(defprod field-query ()
  (^ ((@ (word ":") (set-query-field parser word)) query)
     query))

(defprod wild-query ()
  (^ wild-word (get-wild-query parser wild-word)))

(defprod white-space () (/ #\space #\tab #\page))

(defprod word () (non-wild-letter (* non-wild-letter)))
(defprod any-word () (any-letter (* any-letter)))
(defprod wild-word () (+ (/ (wild-letter (? any-word))
			    (word wild-letter (? any-word)))))


(defchartype any-letter '(satisfies alphanumericp))
(defchartype non-wild-letter '(or (and (satisfies alphanumericp) (satisfies not-wildcard-char-p)) (member #\-)))
(defchartype wild-letter '(satisfies wildcard-char-p))

(defun wildcard-char-p (char) (member char '(#\* #\?)))
(defun not-wildcard-char-p (char) (not (member char '(#\* #\?))))


(defun default-field ()
  "contents")

#||
(defun get-term-query (word)
  (make-instance 'term-query
		 :term (make-term (default-field) word)))

(defun get-boolean-clause (query occur)
  (make-instance 'boolean-clause
		 :query query
		 :occur occur))

(defun get-boolean-query (clauses)
  (let ((q (make-instance 'boolean-query)))
    (if (listp clauses)
	(dolist (clause clauses)
	  (add-clause q clause))
	(add-clause q clauses))
    q))

(defun get-wild-query (&rest args)
  (list :wild-query args))
||#

(defmethod parse ((parser query-parser) query)
  (parselet ((query-parser (^ top-query)))
      (multiple-value-bind (success-p result)
	  (query-parser query)
	(if success-p
	    result
	    nil))))




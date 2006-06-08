(in-package #:montezuma)

;; This is just a dumb stub implementation for now so I can write unit
;; tests for index.lisp.


(defclass query-parser ()
  ((field)
   (default-search-field :initarg :default-search-field)
   (analyzer)
   (fields :accessor fields)
   (options :initarg :options))
  (:default-initargs
   :default-search-field "*" :options '()))

(defmethod initialize-instance :after ((self query-parser) &key)
  (with-slots (options) self
    (check-type options index-options-list)))


(defprod req-op () "+")
(defprod not-op () "!")

(defprod top-query ()
  (^ bool-query
     (get-boolean-query bool-query)))
     

(defprod bool-query ()
  ((^ bool-clause)
   (* (^ ((* white-space) bool-clause)
	 (add-default-clause bool-query bool-clause)))))

(defprod bool-clause ()
  (/ (^ (req-op query)
	(get-boolean-clause query :must-occur))
     (^ (not-op query)
	(get-boolean-clause query :must-not-occur))
     (^ query 
	(get-boolean-clause query :should-occur))))

(defprod query ()
  (/ (^ wild-query)
     (^ field-query)
     (^ term-query)
     (^ phrase-query
	(get-phrase-query phrase-query))
     ))

(defprod term-query ()
  (^ word (get-term-query word)))

(defprod phrase-query ()
  ("\""
   (^ word)
   (* (^ ((* white-space) word)
	 (add-word-to-phrase phrase-query word)))
   "\""))

(defprod field-query ()
  (^ (word ":" query)
     (list :field word query)))

(defprod wild-query ()
  (^ wild-word (get-wild-query wild-word)))

(defprod white-space () (/ #\space #\tab #\page))

(defprod word () (non-wild-letter (* non-wild-letter)))
(defprod any-word () (any-letter (* any-letter)))
(defprod wild-word () (/ (wild-letter (? any-word))
			 (word wild-letter (? any-word))))


(defchartype any-letter '(satisfies alphanumericp))
(defchartype non-wild-letter '(and (satisfies alphanumericp) (satisfies not-wildcard-char-p)))
(defchartype wild-letter '(and (satisfies wildcard-char-p)))

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

(defun add-and-clause (clauses clause)
  (cons clause (if (listp (car clauses)) clauses (list clauses))))

(defun add-default-clause (clauses clause)
  (cons clause (if (listp (car clauses)) clauses (list clauses))))

(defun get-term-query (word)
  (list :term-query word))
(defun get-boolean-clause (query occur)
  (list :boolean-clause occur query))
(defun get-boolean-query (clauses)
  (list :boolean-query (if (listp clauses) clauses (list clauses))))
(defun add-word-to-phrase (phrase word)
  (append (if (listp phrase) phrase (list phrase)) (list word)))
(defun get-phrase-query (words)
  (cons :phrase-query words))
(defun get-wild-query (word)
  (list :wild-query word))

(defmethod parse ((self query-parser) query)
  (parselet ((parser (^ top-query)))
    (multiple-value-bind (success-p result)
	(parser query)
      (if success-p
	  result
	  nil))))

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
  (^ (word ":" query)
     (set-query-field parser query word)))

(defprod wild-query ()
  (^ wild-word (get-wild-query parser wild-word)))

(defprod white-space () (/ #\space #\tab #\page))

(defprod word () (non-wild-letter (* non-wild-letter)))
(defprod any-word () (any-letter (* any-letter)))
(defprod wild-word () (+ (/ (wild-letter (? any-word))
			    (word wild-letter (? any-word)))))


(defchartype any-letter '(satisfies alphanumericp))
(defchartype non-wild-letter '(and (satisfies alphanumericp) (satisfies not-wildcard-char-p)))
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




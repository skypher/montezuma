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
(defprod and-op () (/ "AND" "&&"))
(defprod or-op () (/ "OR" "||"))

(defprod top-query ()
  (^ bool-query
     (get-boolean-query bool-query)))

(defprod bool-query ()
  (/ (^ ((^ bool-clause) (* white-space) and-op (* white-space) (^ bool-query))
	(add-and-clause bool-query bool-clause))
     (^ (bool-clause (* white-space) or-op (* white-space) bool-query)
	(add-or-clause bool-query bool-clause))
     (^ (bool-clause (* white-space) bool-query)
	(add-default-clause bool-query bool-clause))
     (^ bool-clause
	(list bool-clause))))

(defprod bool-clause ()
  (^ (/ (^ (req-op query)
	   (get-boolean-clause query :must-occur))
	(^ (not-op query)
	   (get-boolean-clause query :must-not-occur))
	(^ boosted-query
	   (get-boolean-clause boosted-query :should-occur)))))

(defprod boosted-query ()
  (^ (/ (^ (query "^" word)
	   (progn (setf (boost query) (parse-integer word))
		  query))
	(^ query))))

(defprod query ()
  (^ (/ (^ ("(" (* white-space) bool-query (* white-space) ")")
	   (get-boolean-query bool-query))
	(^ wild-query)
	(^ term-query))))
     

(defprod term-query ()
  (^ (/ (^ word
	   (get-term-query word)))))

(defprod wild-query ()
  (^ wild-word
     (get-wild-query wild-word)))

(defprod white-space () (/ #\space #\tab #\page))

(defprod word () (non-wild-letter (* non-wild-letter)))
(defprod any-word () (any-letter (* any-letter)))
(defprod wild-word () (/ (wild-letter (? any-word))
			 (word wild-letter (* any-letter))))


(defchartype any-letter '(satisfies alphanumericp))
(defchartype non-wild-letter '(and (satisfies alphanumericp) (satisfies not-wildcard-char-p)))
(defchartype wild-letter '(and (satisfies wildcard-char-p)))

(defun wildcard-char-p (char) (member char '(#\* #\?)))
(defun not-wildcard-char-p (char) (not (member char '(#\* #\?))))


(defun default-field ()
  "contents")

(defun get-term-query (word)
  (make-instance 'term-query
		 :term (make-term (default-field) word)))

(defun get-boolean-clause (query occur)
  (make-instance 'boolean-clause
		 :query query
		 :occur occur))

(defun get-boolean-query (clauses)
  (let ((q (make-instance 'boolean-query)))
    (dolist (clause clauses)
      (add-clause q clause))
    q))

(defun add-and-clause (clauses clause)
  (cons clause clauses))

(defun add-default-clause (clauses clause)
  (cons clause clauses))

(defun get-wild-query (&rest args)
  (list :wild-query args))

#|
(defchartype double-quote '(member #\"))

(defprod query () (^ (/ implicit-boolean-query
			term-query
			phrase-query)))

(defprod term-query () (^ word
			  (if (some #'wildcard-char-p word)
			      (make-instance 'wildcard-query
					     :term (make-term "contents" word))
			      (make-instance 'term-query
					     :term (make-term "contents" word)))))

(defprod words () (+ (^ (word (? white-space))
			 (reverse (cons word words)))))
 
(defprod phrase-query () (^ (double-quote (? white-space) words (? white-space) double-quote)
			    (let ((q (make-instance 'phrase-query)))
			      (dolist (word words)
				(add-term-to-query q (make-term "contents" word)))
			      q)))

(defprod implicit-boolean-query ()
  (+ (^ (term-query (? white-space))
	(if (not implicit-boolean-query)
	    (let ((q (make-instance 'boolean-query)))
	      (add-query q term-query :must-occur)
	      q)
	    (progn
	      (add-query implicit-boolean-query term-query :must-occur)
	      implicit-boolean-query)))))


(defprod word () (letter-or-wildcard (* letter-or-wildcard)))

(defchartype letter '(satisfies alpha-char-p))
(defchartype letter-or-wildcard '(or letter (satisfies wildcard-char-p)))

(defparser query-parser (^ query))
|#
 

(defmethod parse ((self query-parser) query)
  (parselet ((parser (^ top-query)))
    (multiple-value-bind (success-p result)
	(parser query)
      (if success-p
	  result
	  nil))))

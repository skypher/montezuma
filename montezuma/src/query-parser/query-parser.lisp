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
  (^ (/ and-bool-query
	implied-and-bool-query
	single-bool-clause)))
	

#||
   ((^ bool-clause)
      (* (^ ((* white-space) and-op (* white-space) bool-clause)
	    (add-and-clause bool-query bool-clause))))
     ((^ bool-clause)
      (* (^ ((* white-space) bool-clause)
	    (add-default-clause bool-query bool-clause))))
     (^ bool-clause
	(list bool-clause))))
||#


(defprod and-bool-query ()
  ((^ bool-clause)
   (+ (^ ((* white-space) and-op (* white-space) bool-clause)
	 (add-and-clause and-bool-query bool-clause)))))

(defprod implied-and-bool-query ()
  ((^ bool-clause)
   (+ (^ ((* white-space) bool-clause)
	 (add-and-clause implied-and-bool-query bool-clause)))))

(defprod single-bool-clause ()
  (^ bool-clause
     (list bool-clause)))

#||
  ((^ bool-clause) (* white-space) and-op (* white-space) (^ bool-query
							     (add-and-clause bool-query bool-clause))))
     (^ (bool-clause (* white-space) or-op (* white-space) bool-query)
	(add-or-clause bool-query bool-clause))
     (^ (bool-clause (* white-space) bool-query)
	(add-default-clause bool-query bool-clause))
     (^ bool-clause
	(list bool-clause))))
||#

(defprod bool-clause ()
  (^ (/ (^ (req-op query)
	   (get-boolean-clause query :must-occur))
	(^ (not-op query)
	   (get-boolean-clause query :must-not-occur))
	(^ boosted-query
	   (get-boolean-clause boosted-query :should-occur)))))

(defprod boosted-query ()
  (^ (/ (^ (query "^" word)
	   (set-query-boost query (parse-integer word)))
	query)))

(defprod query ()
  (^ (/ ("(" (* white-space) (^ bool-query (get-boolean-query bool-query)) (* white-space) ")")
	wild-query
	term-query)))
     

(defprod term-query ()
  (^ word
     (get-term-query word)))

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
(defun set-query-boost (query boost)
  (list :boost boost query))
      

(defmethod parse ((self query-parser) query)
  (parselet ((parser (^ top-query)))
    (multiple-value-bind (success-p result)
	(parser query)
      (if success-p
	  result
	  nil))))

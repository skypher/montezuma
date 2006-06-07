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

(defmethod parse ((self query-parser) query)
  (make-instance 'term-query
		 :term (make-term (slot-value self 'default-search-field)
				  "one")))



(defchartype double-quote '(member #\"))

(defprod white-space () (/ #\space #\tab #\page))

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

(defun wildcard-char-p (char) (member char '(#\* #\?)))


(defparser query-parser (^ query))
 
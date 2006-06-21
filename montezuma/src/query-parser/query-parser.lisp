(in-package #:montezuma)

;; This is just a dumb stub implementation for now so I can write unit
;; tests for index.lisp.


(defclass query-parser ()
  ((field :initform nil)
   (default-field :initarg :default-field)
   (analyzer :initarg :analyzer :reader analyzer)
   (wild-lower :initarg :wild-lower)
   (default-occur :initarg :default-occur)
   (default-slop :initarg :default-slop)
   (fields :initform '() :accessor fields :initarg :fields)
   (handle-parse-errors :initarg :handle-parse-errors))
  (:default-initargs
   :default-field "*"
    :analyzer (make-instance 'standard-analyzer)
    :wild-lower T
    :default-occur :should-occur
    :default-slop 0
    :handle-parse-errors NIL))


(defgeneric $add-and-clause (parser clauses clause))
(defgeneric $add-or-clause (parser clauses clause))
(defgeneric $add-default-clause (parser clauses clause))
(defgeneric $get-term-query (parser word))
(defgeneric $get-boolean-clause (parser query occur))
(defgeneric $get-boolean-query (parser clauses))
(defgeneric $add-word-to-phrase (parser phrase word))
(defgeneric $get-phrase-query (parser words))
(defgeneric get-normal-phrase-query (parser words))
(defgeneric $get-wild-query (parser word))
(defgeneric $set-query-field (parser field))
(defgeneric use-active-field (parser))
(defgeneric parse (parser query-string))


(defmacro do-multiple-fields ((field-var parser field-spec) &body body)
  `(combine-multiple-fields (map 'list
			     #'(lambda (,field-var)
				 ,@body)
			     (compute-multiple-fields ,parser ,field-spec))))



(defmethod initialize-instance :after ((self query-parser) &key)
  (with-slots (field default-field) self
    (when (null field)
      (setf field default-field))))

(defmethod $add-and-clause ((parser query-parser) clauses clause)
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

(defmethod $add-or-clause ((parser query-parser) clauses clause)
  (cons clause (if (listp clauses) clauses (list clauses))))

(defmethod $add-default-clause ((parser query-parser) clauses clause)
  (if (eq (slot-value parser 'default-occur) :must-occur)
      ($add-and-clause parser clauses clause)
      ($add-or-clause parser clauses clause)))

(defmethod $get-term-query ((parser query-parser) word)
  (do-multiple-fields (field parser (use-active-field parser))
  (let ((tokens (all-tokens (analyzer parser) field word)))
    (cond ((= (length tokens) 0)
	   (make-instance 'term-query
			  :term (make-term field "")))
	  ((= (length tokens) 1)
	   (make-instance 'term-query
			  :term (make-term field (term-text (car tokens)))))
	  (T
	   (let ((pq (make-instance 'phrase-query)))
	     (dolist (token tokens)
	       (add-term-to-query pq (make-term field (term-text token)) nil (token-increment token)))
	     pq))))))

(defmethod $get-boolean-clause ((parser query-parser) query occur)
  (make-instance 'boolean-clause
		 :query query
		 :occur occur))

(defmethod $get-boolean-query ((parser query-parser) clauses)
  (let ((q (make-instance 'boolean-query)))
    (dolist (clause (reverse (if (listp clauses) clauses (list clauses))))
      (add-clause q clause))
    q))

(defmethod $add-word-to-phrase ((parser query-parser) phrase word)
  (append (if (listp phrase) phrase (list phrase)) (list word)))

(defmethod $get-phrase-query ((parser query-parser) words)
  (setf words (if (listp words) words (list words)))
  (if (= (length words) 1)
      ($get-term-query parser (car words))
      (get-normal-phrase-query parser words)))

(defmethod get-normal-phrase-query ((parser query-parser) words)
  (do-multiple-fields (field parser (use-active-field parser))
    (let ((pq (make-instance 'phrase-query)))
      (setf (slop pq) (slot-value parser 'default-slop))
      (let ((pos-inc 0))
	(dolist (word words)
	  (if (null word)
	      (incf pos-inc)
	      (let ((tokens (all-tokens (slot-value parser 'analyzer) field word)))
		(dolist (token tokens)
		  (add-term-to-query pq (make-term field (term-text token))
				     nil (+ pos-inc (token-increment token)))
		  (setf pos-inc 0))))))
      pq)))

(defmethod $get-wild-query ((parser query-parser) word)
  (do-multiple-fields (field parser (use-active-field parser))
    (make-instance 'wildcard-query
		   :term (make-term field word))))

(defmethod $set-query-field ((parser query-parser) field)
  (setf (slot-value parser 'field) field))

(defmethod use-active-field ((parser query-parser))
  (if (slot-value parser 'field)
      (prog1 (slot-value parser 'field)
	(setf (slot-value parser 'field) nil))
      (slot-value parser 'default-field)))

(defun combine-multiple-fields (queries)
  (if (= (length queries) 1)
      (first queries)
      (let ((bq (make-instance 'boolean-query)))
	(dolist (q queries)
	  (add-clause bq (make-instance 'boolean-clause
					:query q
					:occur :should-occur)))
	bq)))

(defgeneric compute-multiple-fields (parser field-spec))

(defmethod compute-multiple-fields ((parser query-parser) field-spec)
  (if (string= field-spec "*")
      (fields parser)
      (list field-spec)))

;; Note that the following grammar rules must be used in a parser
;; created with a parselet form, with a lexical context that includes
;; a "parser" binding.

(defprod req-op () "+")
(defprod not-op () "!")

(defprod top-query ()
  (^ bool-query
     ($get-boolean-query parser bool-query)))
     

(defprod bool-query ()
  ((^ bool-clause)
   (* (^ ((* white-space) bool-clause)
	 ($add-default-clause parser bool-query bool-clause)))))

(defprod bool-clause ()
  (/ (^ (req-op query)
	($get-boolean-clause parser query :must-occur))
     (^ (not-op query)
	($get-boolean-clause parser query :must-not-occur))
     (^ query 
	($get-boolean-clause parser query :should-occur))))

(defprod query ()
  (/ (^ boosted-query)
     (^ unboosted-query)))

(defprod unboosted-query ()
 (/ (^ phrase-query
       ($get-phrase-query parser phrase-query))
    (^ wild-query)
    (^ field-query)
    (^ term-query)))

(defprod boosted-query ()
  (^ (unboosted-query "^" word)
     (progn (setf (boost unboosted-query) (parse-integer word))
	    unboosted-query)))
 
(defprod term-query ()
  (^ word ($get-term-query parser word)))

(defprod phrase-query ()
  ("\""
   (^ word)
   (* (^ ((* white-space) word)
	 ($add-word-to-phrase parser phrase-query word)))
   "\""))

(defprod field-query ()
  (^ ((@ (word ":") ($set-query-field parser word)) unboosted-query)
     unboosted-query))


(defprod field-name ()
  (/ wild-word word))

(defprod wild-query ()
  (^ wild-word ($get-wild-query parser wild-word)))

(defchartype white-space () '(satisfies white-space-p))

(defprod word () (non-wild-letter (* non-wild-letter)))
(defprod any-word () (any-letter (* any-letter)))
(defprod wild-word () (+ (/ (wild-letter (? any-word))
			    (word wild-letter (? any-word)))))


(defchartype any-letter '(and (satisfies graphic-char-p) (not (satisfies white-space-p)) (not (satisfies disallowed-punctuation-p))))
(defchartype non-wild-letter '(and (satisfies graphic-char-p) (not (satisfies white-space-p)) (not (satisfies disallowed-punctuation-p)) (satisfies not-wildcard-char-p)))
(defchartype wild-letter '(satisfies wildcard-char-p))

(defun wildcard-char-p (char) (member char '(#\* #\?)))
(defun not-wildcard-char-p (char) (not (member char '(#\* #\?))))
(defun white-space-p (char) (member char '(#\space #\tab #\page #\newline)))

(defun disallowed-punctuation-p (char) (member char '(#\" #\* #\? #\: #\^)))

(defmethod parse ((parser query-parser) query-string)
  (parselet ((query-parser (^ top-query)))
      (multiple-value-bind (success-p result)
	  (query-parser query-string)
	(if success-p
	    result
	    (get-bad-parse parser query-string)))))



(defgeneric get-bad-parse (parser query-string))

(defmethod get-bad-parse ((parser query-parser) query-string)
  (do-multiple-fields (field parser (slot-value parser 'default-field))
    (let ((tokens (all-tokens (analyzer parser) field query-string)))
      (cond ((= (length tokens) 0)
	     (make-instance 'term-query
			    :term (make-term field "")))
	    ((= (length tokens) 1)
	     (make-instance 'term-query
			    :term (make-term field (term-text (elt tokens 0)))))
	    (T
	     (let ((bq (make-instance 'boolean-query)))
	       (dosequence (token tokens)
		 (add-clause bq
			     (make-instance 'boolean-clause
					    :query (make-instance 'term-query
								  :term (make-term field (term-text token))))))
	       bq))))))

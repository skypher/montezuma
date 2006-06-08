(in-package #:montezuma)


(defclass test-query-parser (query-parser)
  ())


(defmethod add-and-clause ((parser test-query-parser) clauses clause)
  (cons clause (if (listp (car clauses)) clauses (list clauses))))

(defmethod add-default-clause ((parser test-query-parser) clauses clause)
  (cons clause (if (listp (car clauses)) clauses (list clauses))))

(defmethod get-term-query ((parser test-query-parser) word)
  (list :term-query (get-active-field parser) word))

(defmethod get-boolean-clause ((parser test-query-parser) query occur)
  (list :boolean-clause occur query))

(defmethod get-boolean-query ((parser test-query-parser) clauses)
  (list :boolean-query (if (listp clauses) clauses (list clauses))))

(defmethod add-word-to-phrase ((parser test-query-parser) phrase word)
  (append (if (listp phrase) phrase (list phrase)) (list word)))

(defmethod get-phrase-query ((parser test-query-parser) words)
  (list :phrase-query (get-active-field parser) (if (listp words) words (list words))))

(defmethod get-wild-query ((parser test-query-parser) word)
  (list :wild-query (get-active-field parser) word))


(defun check-query-parse (query-string expected-parse-tree)
  (atest check-query-parse
	 (parse (make-instance 'test-query-parser)
		query-string)
	 expected-parse-tree
	 #'(lambda (a b) (tree-equal a b :test #'equal))
	 (format T "~&Query string was ~S" query-string)))

(deftestfun query-parser
    (let ((tests
	   '(("abc"
	      (:BOOLEAN-QUERY (:BOOLEAN-CLAUSE :SHOULD-OCCUR (:TERM-QUERY "*" "abc")))) 
	     ("abc def"
	      (:BOOLEAN-QUERY
	       ((:BOOLEAN-CLAUSE :SHOULD-OCCUR (:TERM-QUERY "*" "def"))
		(:BOOLEAN-CLAUSE :SHOULD-OCCUR (:TERM-QUERY "*" "abc"))))) 
	     ("+abc" (:BOOLEAN-QUERY (:BOOLEAN-CLAUSE :MUST-OCCUR (:TERM-QUERY "*" "abc")))) 
	     ("abc +def ghi"
	      (:BOOLEAN-QUERY
	       ((:BOOLEAN-CLAUSE :SHOULD-OCCUR (:TERM-QUERY "*" "ghi"))
		(:BOOLEAN-CLAUSE :MUST-OCCUR (:TERM-QUERY "*" "def"))
		(:BOOLEAN-CLAUSE :SHOULD-OCCUR (:TERM-QUERY "*" "abc"))))) 
	     ("foo*"
	      (:BOOLEAN-QUERY (:BOOLEAN-CLAUSE :SHOULD-OCCUR (:WILD-QUERY "*" "foo*")))) 
	     ("foo?bar"
	      (:BOOLEAN-QUERY (:BOOLEAN-CLAUSE :SHOULD-OCCUR (:WILD-QUERY "*" "foo?bar")))) 
	     ("*foo*"
	      (:BOOLEAN-QUERY (:BOOLEAN-CLAUSE :SHOULD-OCCUR (:WILD-QUERY "*" "*foo*")))) 
	     ("*foo"
	      (:BOOLEAN-QUERY (:BOOLEAN-CLAUSE :SHOULD-OCCUR (:WILD-QUERY "*" "*foo")))) 
	     ("*foo*bar"
	      (:BOOLEAN-QUERY (:BOOLEAN-CLAUSE :SHOULD-OCCUR (:WILD-QUERY "*" "*foo*bar")))) 
	     ("+*foo"
	      (:BOOLEAN-QUERY (:BOOLEAN-CLAUSE :MUST-OCCUR (:WILD-QUERY "*" "*foo")))) 
	     ("!abc"
	      (:BOOLEAN-QUERY (:BOOLEAN-CLAUSE :MUST-NOT-OCCUR (:TERM-QUERY "*" "abc")))) 
	     ("abc !def"
	      (:BOOLEAN-QUERY
	       ((:BOOLEAN-CLAUSE :MUST-NOT-OCCUR (:TERM-QUERY "*" "def"))
		(:BOOLEAN-CLAUSE :SHOULD-OCCUR (:TERM-QUERY "*" "abc"))))) 
	     ("\"abc\""
	      (:BOOLEAN-QUERY (:BOOLEAN-CLAUSE :SHOULD-OCCUR (:PHRASE-QUERY "*" ("abc")))))
	     ("\"abc def\""
	      (:BOOLEAN-QUERY
	       (:BOOLEAN-CLAUSE :SHOULD-OCCUR (:PHRASE-QUERY "*" ("abc" "def")))))
	     ("abc \"def hij\""
	      (:BOOLEAN-QUERY
	       ((:BOOLEAN-CLAUSE :SHOULD-OCCUR (:PHRASE-QUERY "*" ("def" "hij")))
		(:BOOLEAN-CLAUSE :SHOULD-OCCUR (:TERM-QUERY "*" "abc"))))) 
	     ("foo* \"bad dog\""
	      (:BOOLEAN-QUERY
	       ((:BOOLEAN-CLAUSE :SHOULD-OCCUR (:PHRASE-QUERY "*" ("bad" "dog")))
		(:BOOLEAN-CLAUSE :SHOULD-OCCUR (:WILD-QUERY "*" "foo*"))))) 
	     ("field:value"
	      (:BOOLEAN-QUERY
	       (:BOOLEAN-CLAUSE :SHOULD-OCCUR (:TERM-QUERY "field" "value"))))
	     ("field:foo*"
	      (:BOOLEAN-QUERY
	       (:BOOLEAN-CLAUSE :SHOULD-OCCUR (:WILD-QUERY "field" "foo*"))))
	     ("field:*foo"
	      (:BOOLEAN-QUERY
	       (:BOOLEAN-CLAUSE :SHOULD-OCCUR (:WILD-QUERY "field" "*foo"))))
	     ("+field:abc"
	      (:BOOLEAN-QUERY
	       (:BOOLEAN-CLAUSE :MUST-OCCUR (:TERM-QUERY "field" "abc"))))
	     ("field:\"1 2 3\""
	      (:BOOLEAN-QUERY
	       (:BOOLEAN-CLAUSE :SHOULD-OCCUR (:PHRASE-QUERY "field" ("1" "2" "3"))))))))
      (dolist (test tests)
	(check-query-parse (first test) (second test)))))

	    
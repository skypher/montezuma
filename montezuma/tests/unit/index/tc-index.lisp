(in-package #:montezuma)

(defun check-query-results (index query expected)
  (let ((count 0))
    (format T "~&Query: ~S" query)
    (search-each index query
		 #'(lambda (doc score)
		     (format T "~&doc: ~S  score: ~S" doc score)
		     (atest search-result-got-expected
			    (find doc expected)
			    T
			    #'bool=)
		     (incf count)))
    (atest search-result-count-correct
	   count
	   (length expected))))

(defun do-test-index-with-array (index)
  (let ((data '(#("one" "two")
		#("one" "three")
		#("two")
		#("one" "four")
		#("one two")
		#("two" "three" "four")
		#("one")
		#("two" "three" "four" "five"))))
    (dolist (doc data)
      (add-document-to-index index doc))
    (describe index)
    (atest index-with-array-1 (size index) 8)
    
    ;; FIXME: We don't have a query language parser to handle these
    ;; yet, so fake it.
    ;; (check-query-results index "one" '(0 1 3 4 6))
    ;; (check-query-results index "one AND two" '(0 4))
    ;; (check-query-results index "one OR five" '(0 1 3 4 6 7))
    (flet ((term-query (term)
	     (make-instance 'term-query
			    :term (make-term (slot-value index 'default-search-field)
					     term))))
      (check-query-results index (term-query "one") '(0 1 3 4 6))
      (let ((query (make-instance 'boolean-query)))
	(add-query query (term-query "one") :must-occur)
	(check-query-results index query '(0 1 3 4 6)))
      (let ((query (make-instance 'boolean-query)))
	(add-query query (term-query "one") :should-occur)
	(check-query-results index query '(0 1 3 4 6)))
      (let ((query (make-instance 'boolean-query)))
	(add-query query (term-query "one") :must-occur)
	(add-query query (term-query "two") :must-occur)
	(check-query-results index query '(0 4)))
      (let ((query (make-instance 'boolean-query)))
	(add-query query (term-query "one") :should-occur)
	(add-query query (term-query "five") :should-occur)
	(check-query-results index query '(0 1 3 4 6 7))))
    (atest index-with-array-2
	   (get-field (get-doc index 7) "def_field")
	   "two three four five"
	   #'string=)))

#|
(deftestfixture index-test
  (:testfun test-ram-index
    (let ((index (make-instance 'index
				:default-field "def_field")))
      (do-test-index-with-array index)
      (close index))))
|#
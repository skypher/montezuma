(in-package #:montezuma)

(defun set= (a b &key (test #'eql))
  (let ((al (coerce a 'list))
	(bl (coerce b 'list)))
    (and (null (set-difference al bl :test test))
	 (null (set-difference bl al :test test)))))

(defun check-query-results (index query expected)
  (let ((count 0))
    (let ((results '()))
      (search-each index query
		   #'(lambda (doc score)
		       (push doc results)))
      (atest search-results-correct
	     (reverse results)
	     expected
	     #'set=))))

(defun do-test-index-with-array (index)
  (let ((data '(#("one two")
		#("one" "three")
		#("two")
		#("one" "four")
		#("one two")
		#("two" "three" "four")
		#("one")
		#("two" "three" "four" "five"))))
    (dolist (doc data)
      (add-document-to-index index doc))
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
	(add-query query (term-query "two") :must-occur)
	(add-query query (term-query "three") :must-occur)
	(add-query query (term-query "four") :must-occur)
	(check-query-results index query '(5 7)))
      (let ((query (make-instance 'boolean-query)))
	(add-query query (term-query "one") :should-occur)
	(add-query query (term-query "five") :should-occur)
	(check-query-results index query '(0 1 3 4 6 7))))
    (atest index-with-array-2
	   (document-values (get-document index 7) "def_field")
	   "two three four five"
	   #'string=)))

(defun do-test-index-with-table (index)
  (let ((data '((("def_field" . "one two"))
		(("def_field" . "one") ("field2" . "three"))
		(("def_field" . "two"))
		(("def_field" . "one") ("field2" . "four"))
		(("def_field" . "one two"))
		(("def_field" . "two") ("field2" . "three") ("field3" . "four"))
		(("def_field" . "one"))
		(("def_field" . "two") ("field2" . "three") ("field3" . "five")))))
    (dolist (doc data)
      (add-document-to-index index doc))
    (flet ((term-query (term &optional field)
	     (make-instance 'term-query
			    :term (make-term (or field
						 (slot-value index 'default-search-field))
					     term))))
      (check-query-results index (term-query "one") '(0 1 3 4 6))
      (let ((query (make-instance 'boolean-query)))
	(add-query query (term-query "one") :must-occur)
	(add-query query (term-query "two") :must-occur)
	(check-query-results index query '(0 4)))
      (let ((query (make-instance 'boolean-query)))
	(add-query query (term-query "one") :should-occur)
	(add-query query (term-query "five") :should-occur)
	(check-query-results index query '(0 1 3 4 6)))
      (let ((query (make-instance 'boolean-query)))
	(add-query query (term-query "one") :should-occur)
	(add-query query (term-query "five" "field3") :should-occur)
	(check-query-results index query '(0 1 3 4 6 7)))
      (let ((query (make-instance 'wildcard-query
				  :term (make-term "field3" "f*"))))
	(check-query-results index query '(5 7)))
      (let ((query (make-instance 'boolean-query)))
	(add-query query (term-query "two") :must-occur)
	(add-query query
		   (make-instance 'wildcard-query
				  :term (make-term "field3" "f*"))
		   :must-occur)
	(check-query-results index query '(5 7)))
      (atest index-with-table-1
	     (document-values (get-document index 7) "field3")
	     "five"
	     #'string=)
      (atest index-with-table-2
	     (document-values (get-document index 7) "def_field")
	     "two"
	     #'string=)
      (atest index-with-table-3 (has-deletions-p index) NIL)
      (atest index-with-table-4 (deleted-p index 5) NIL)
      (atest index-with-table-5 (size index) 8)
      (delete index 5)
      (atest index-with-table-6 (has-deletions-p index) T #'bool=)
      (atest index-with-table-7 (deleted-p index 5) T #'bool=)
      (atest index-with-table-8 (size index) 7)
      (let ((query (make-instance 'wildcard-query
				  :term (make-term "field3" "f*"))))
	(check-query-results index query '(7)))

)))

(deftestfixture index-test
  (:testfun test-ram-index
    (let ((index (make-instance 'index
				:default-field "def_field")))
      (do-test-index-with-array index)
      (close index))
    (let ((index (make-instance 'index
				:default-field "def_field")))
      (do-test-index-with-table index)
      (close index)))
  (:testfun test-fs-index
    (let ((path *test-directory-path*))
      (flet ((delete-test-index ()
	       (dolist (file (cl-fad:delete-directory-and-files path :if-does-not-exist :ignore)))))
	(delete-test-index)
	(condition-test
	 fs-index-1
	 (make-instance 'index
			:path path
			:create-if-missing-p NIL
			:default-field "def_field")
	 'error)
	(delete-test-index)
	(let ((index (make-instance 'index
				    :path path
				    :create-p T
				    :default-field "def_field")))
	  (do-test-index-with-array index)
	  (close index))
	(delete-test-index)
	(let ((index (make-instance 'index
				    :path path
				    :create-p T
				    :default-field "def_field")))
	  (do-test-index-with-table index)
	  (close index))))))



	    

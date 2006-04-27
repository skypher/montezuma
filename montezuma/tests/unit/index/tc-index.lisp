(in-package #:montezuma)

(defun check-query-results (index query expected)
  (let ((count 0))
    (search-each index query
		 #'(lambda (doc score)
		     (atest search-result-got-expected
			    (index expected doc)
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
    (atest index-with-array-1 (size index) 8)
    (check-query-results index "one" '(0 1 3 4 6))
    (check-query-results index "one AND two" '(0 4))
    (check-query-results index "one OR five" '(0 1 3 4 6 7))
    (atest index-with-array-2
	   (get-field (get-doc index 7) "def_field")
	   "two three four five"
	   #'string=)))


(deftestfixture index-test
  (:testfun test-ram-index
    (let ((index (make-instance 'index
				:default-field "def_field")))
      (do-test-index-with-array index)
      (close index))))

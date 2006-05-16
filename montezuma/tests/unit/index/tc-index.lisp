(in-package #:montezuma)

(defun set= (a b &key (test #'eql))
  (let ((al (coerce a 'list))
	(bl (coerce b 'list)))
    (and (null (set-difference al bl :test test))
	 (null (set-difference bl al :test test)))))

(defun check-query-results (index query expected)
  (let ((results '()))
    (search-each index query
		 #'(lambda (doc score)
		     (declare (ignorable score))
		     (push doc results)))
    (atest search-results-correct
	   (reverse results)
	   expected
	   #'set=)))

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

(defun print-docs (index)
  (dotimes (i 15)
    (let ((doc (ignore-errors (get-document index i))))
      (format T "~&~3S ~S" i doc))))

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
	(check-query-results index query '(7))
	(add-document-to-index index
			       '(("field3" . "fool")))
	(check-query-results index query '(6 7))
	(atest index-with-table-9 (size index) 8)
	(atest index-with-table-10
	       (document-values (get-document index 7)
				"field3")
	       "fool"
	       #'string=)
	(optimize index)
	(check-query-results index query '(6 7)))
      (let ((te (make-term "field2" "three"))) 
	(delete index te))
      (atest index-with-table-11 (deleted-p index 1) T #'bool=)
      (atest index-with-table-12 (deleted-p index 3) NIL #'bool=)
      (atest index-with-table-13 (deleted-p index 6) T #'bool=)
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
	       (cl-fad:delete-directory-and-files path :if-does-not-exist :ignore)))
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
	  (close index)))))
  (:testfun test-fs-index-is-persistent
    (let ((path *test-directory-path*))
      (flet ((delete-test-index ()
	       (dolist (file (cl-fad:delete-directory-and-files path :if-does-not-exist :ignore)))))
	(delete-test-index)
	(let ((data '((("def_field" . "one two") (:id . "me"))
		      (("def_field" . "one") (:field2 . "three"))
		      (("def_field" . "two"))
		      (("def_field" . "one") (:field2 . "four"))
		      (("def_field" . "one two"))
		      (("def_field" . "two") (:field2 . "three") ("field3" . "four"))
		      (("def_field" . "one"))
		      (("def_field" . "two") (:field2 . "three") ("field3" . "five"))))
	      (index (make-instance 'index
				    :path path
				    :default-field "def_field")))
	  (dolist (doc data)
	    (add-document-to-index index doc))
	  (test fs-index-is-persistent-1 (size index) 8)
	  (close index))
	(let ((index (make-instance 'index
				    :path path
				    :create-if-missing-p NIL)))
	  (atest fs-index-is-persistent-2 (size index) 8)
	  (atest fs-index-is-persistent-3
		 (document-values (get-document index 5) "field3")
		 "four"
		 #'string=)
	  (close index)))))
  (:testfun test-merging-indices
    (let ((index1 (make-instance 'index
				 :default-field "f")))
      (let ((data '((("f" . "zero"))
		    (("f" . "one"))
		    (("f" . "two")))))
	(dolist (doc data)
	  (add-document-to-index index1 doc)))
      (let ((index2 (make-instance 'index
				   :default-field "f")))
	(let ((data '((("f" . "three"))
		      (("f" . "four"))
		      (("f" . "five")))))
	  (dolist (doc data)
	    (add-document-to-index index2 doc)))
	(let ((index3 (make-instance 'index
				     :default-field "f")))
	  (let ((data '((("f" . "six"))
			(("f" . "seven"))
			(("f" . "eight")))))
	    (dolist (doc data)
	      (add-document-to-index index3 doc)))
	  (let ((index (make-instance 'index
				      :default-field "f")))
	    (add-indexes index index1)
	    (test merging-indices-1 (size index) 3)
;;	    (print (get-doc index 0))
	    (test merging-indices-2
		  (document-values (get-document index 0) "f")
		  "zero"
		  #'string=)
	    (add-indexes index index2 index3)
	    (test merging-indices-3 (size index) 9)
	    (test merging-indices-4
		  (document-values (get-document index 0) "f")
		  "zero"
		  #'string=)
	    (test merging-indices-5
		  (document-values (get-document index 8) "f")
		  "eight"
		  #'string=)
	    (close index1)
	    (close index2)
	    (close index3)
	    (test merging-indices-6
		  (document-values (get-document index 7) "f")
		  "seven"
		  #'string=)
	    (let ((data '((("f" . "alpha"))
			  (("f" . "beta"))
			  (("f" . "charlie")))))
	      (let* ((dir1 (make-instance 'ram-directory))
		     (index1 (make-instance 'index
					    :directory dir1
					    :default-field "f")))
		(dolist (doc data)
		  (add-document-to-index index1 doc))
		(flush index1)
	    (let ((data '((("f" . "delta"))
			  (("f" . "echo"))
			  (("f" . "foxtrot")))))
	      (let* ((dir2 (make-instance 'ram-directory))
		     (index2 (make-instance 'index
					    :directory dir2
					    :default-field "f")))
		(dolist (doc data)
		  (add-document-to-index index2 doc))
		(flush index2)
	    (let ((data '((("f" . "golf"))
			  (("f" . "india"))
			  (("f" . "juliet")))))
	      (let* ((dir3 (make-instance 'ram-directory))
		     (index3 (make-instance 'index
					    :directory dir3
					    :default-field "f")))
		(dolist (doc data)
		  (add-document-to-index index3 doc))
		(flush index3)
		(add-indexes index dir1)
		(test merging-indices-7 (size index) 12)
		(test merging-indices-8 (document-values (get-document index 9) "f")
		      "alpha" #'string=)
		(add-indexes index dir2 dir3)
		(test merging-indices-9 (size index) 18)
		(test merging-indices-10 (document-values (get-document index 17) "f")
		      "juliet" #'string=)
		(close index1)
		(close dir1)
		(close index2)
		(close dir2)
		(close index3)
		(close dir3)
		(test merging-indices-11 (document-values (get-document index 15) "f")
		      "golf" #'string=)
		(close index))))))))))))
  (:testfun test-persist-index
    (cl-fad:delete-directory-and-files *test-directory-path* :if-does-not-exist :ignore)
    (let ((index (make-instance 'index
				:default-field "f")))
      (let ((data '((("f" . "zero"))
		    (("f" . "one"))
		    (("f" . "two")))))
	(dolist (doc data)
	  (add-document-to-index index doc)))
      (persist index *test-directory-path* :create-p T)
      (test persist-index-1 (size index) 3)
      (test persist-index-2 (document-values (get-document index 0) "f")
	    "zero" #'string=)
      (close index))
    (let ((index (make-instance 'index
				:path *test-directory-path*)))
      (test persist-index-3 (size index) 3)
      (test persist-index-4 (document-values (get-document index 0) "f")
	    "zero" #'string=)
      (close index))
    (let ((index (make-instance 'index
				:default-field "f")))
      (let ((data '((("f" . "romeo"))
		    (("f" . "sierra"))
		    (("f" . "tango")))))
	(dolist (doc data)
	  (add-document-to-index index doc)))
      (test persist-index-5 (size index) 3)
      (test persist-index-6 (document-values (get-document index 0) "f")
	    "romeo" #'string=)
      (let ((dir (make-fs-directory *test-directory-path* :create-p NIL)))
	(persist index dir))
      (test persist-index-7 (size index) 6)
      (test persist-index-8 (document-values (get-document index 0) "f")
	    "zero" #'string=)
      (test persist-index-9 (document-values (get-document index 3) "f")
	    "romeo" #'string=)
      (close index))
    (let ((index (make-instance 'index
				:path *test-directory-path*)))
      (test persist-index-10 (size index) 6)
      (test persist-index-11 (document-values (get-document index 0) "f")
	    "zero" #'string=)
      (test persist-index-12 (document-values (get-document index 3) "f")
	    "romeo" #'string=)
      (close index)))
  (:testfun test-index-auto-update-when-externally-modified
    (let ((index (make-instance 'index
				:path *test-directory-path*
				:default-field "f"
				:create-p T)))
      (add-document-to-index index "document 1")
      (test index-auto-update-when-externally-modified-1
	    (size index)
	    1)
      (let ((index2 (make-instance 'index
				   :path *test-directory-path*
				   :default-field "f")))
	(test index-auto-update-when-externally-modified-2
	      (size index2)
	      1)
	(add-document-to-index index2 "document 2")
	(test index-auto-update-when-externally-modified-3
	      (size index2)
	      2)
	(test index-auto-update-when-externally-modified-4
	      (size index)
	      2)
	(let ((top-docs (search index (make-instance 'term-query
						     :term (make-term "f" "content3")))))
	  (test index-auto-update-when-externally-modified-5
		(size top-docs)
		0)
	  (let ((iw (make-instance 'index-writer
				   :directory *test-directory-path*
				   :analyzer (make-instance 'whitespace-analyzer)))
		(doc (make-instance 'document)))
	    (add-field doc (make-field "f" "content3" :stored T :index :tokenized))
	    (add-document-to-index-writer iw doc)
	    (close iw)
	    (let ((top-docs (search index (make-instance 'term-query
							 :term (make-term "f" "content3")))))
	      (test index-auto-update-when-externally-modified-6
		    (size top-docs)
		    1)))))))
  (:testfun test-index-delete
    (flet ((q (field value) (make-instance 'term-query
					   :term (make-term (string field) value)))
	   (qw (field value) (make-instance 'wildcard-query
					    :term (make-term (string field) value))))
      (let ((data '(((:|id| . 0) (:|cat| . "/cat1/subcat1"))
		    ((:|id| . 1) (:|cat| . "/cat1/subcat2"))
		    ((:|id| . 2) (:|cat| . "/cat1/subcat2"))
		    ((:|id| . 3) (:|cat| . "/cat1/subcat3"))
		    ((:|id| . 4) (:|cat| . "/cat1/subcat4"))
		    ((:|id| . 5) (:|cat| . "/cat2/subcat1"))
		    ((:|id| . 6) (:|cat| . "/cat2/subcat2"))
		    ((:|id| . 7) (:|cat| . "/cat2/subcat3"))
		    ((:|id| . 8) (:|cat| . "/cat2/subcat4"))
		    ((:|id| . 9) (:|cat| . "/cat2/subcat5"))))
	    (index (make-instance 'index
				  :analyzer (make-instance 'whitespace-analyzer))))
	(dolist (doc data)
	  (add-document-to-index index doc))
	(test index-delete-1 (size index) 10)
	;; FIXME: use query strings in addition to query objects once
	;; we have a query parser.
	(test index-delete-2 (size (search index (q :|id| "9"))) 1)
	(delete index 9)
	(test index-delete-3 (size (search index (q :|id| "9"))) 0)
	(test index-delete-4 (size (search index (q :|id| "8"))) 1)
	(delete index "8")
	(test index-delete-5 (size index) 8)
	(test index-delete-6 (size (search index (q :|id| "8"))) 0)
	(test index-delete-7 (size (search index (qw :|cat| "/cat1*"))) 5)
	(query-delete index (qw :|cat| "/cat1*"))
	(test index-delete-8 (size index) 3)
	(test index-delete-9 (size (search index (qw :|cat| "/cat1*"))) 0)
	(close index))))
  (:testfun test-index-update
    (flet ((q (field value) (make-instance 'term-query
					   :term (make-term (string field) value)))
	   (qw (field value) (make-instance 'wildcard-query
					    :term (make-term (string field) value))))
      (let ((data '(((:|id| . 0) (:|cat| . "/cat1/subcat1") (:|content| . "content0"))
		    ((:|id| . 1) (:|cat| . "/cat1/subcat2") (:|content| . "content1"))
		    ((:|id| . 2) (:|cat| . "/cat1/subcat2") (:|content| . "content2"))
		    ((:|id| . 3) (:|cat| . "/cat1/subcat3") (:|content| . "content3"))
		    ((:|id| . 4) (:|cat| . "/cat1/subcat4") (:|content| . "content4"))
		    ((:|id| . 5) (:|cat| . "/cat2/subcat1") (:|content| . "content5"))
		    ((:|id| . 6) (:|cat| . "/cat2/subcat2") (:|content| . "content6"))
		    ((:|id| . 7) (:|cat| . "/cat2/subcat3") (:|content| . "content7"))
		    ((:|id| . 8) (:|cat| . "/cat2/subcat4") (:|content| . "content8"))
		    ((:|id| . 9) (:|cat| . "/cat2/subcat5") (:|content| . "content9"))))
	    (index (make-instance 'index
				  :analyzer (make-instance 'whitespace-analyzer)
				  :default-field :|content|)))
	(dolist (doc data)
	  (add-document-to-index index doc))
	(test index-update-1 (size index) 10)
	(test index-update-2
	      (document-values (get-document index "5") :|content|)
	      "content5"
	      #'string=)
	(update index 5 "content five")
	(test index-update-3
	      (document-values (get-document index "5") :|content|)
	      "content five"
	      #'string=)
	(test index-update-3.5
	      (document-values (get-document index "5") :|extra-content|)
	      nil)
	;; FIXME: Also check just passing a string as the second
	;; argument to update.
	;;	(update index "5"
	;;		'((:|cat| . "/cat1/subcat6")
	;;		  (:|content| . "high five")
	;;		  (:|extra-content| . "hello")))
	(update index (make-term "id" "5")
		'((:|cat| . "/cat1/subcat6")
		  (:|content| . "high five")
		  (:|extra-content| . "hello")))
	(test index-update-4
	      (document-values (get-document index "5") :|extra-content|)
	      "hello"
	      #'string=)
	(test index-update-5
	      (document-values (get-document index "5") :|content|)
	      "high five"
	      #'string=)
	(test index-update-6
	      (document-values (get-document index "5") :|cat|)
	      "/cat1/subcat6"
	      #'string=)
	(test index-update-7
	      (document-values (get-document index "9") :|content|)
	      "content9"
	      #'string=)
	(update index (make-term "content" "content9")
		'((:|content| . "content nine")))
	(test index-update-8
	      (document-values (get-document index "9") :|content|)
	      "content nine"
	      #'string=))))
)

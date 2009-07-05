(in-package #:montezuma)

(deftestfixture boolean-subscorer-test
  (:setup
   (setf (fixture-var 'dir) (make-instance 'ram-directory))
   (let ((iw (make-instance 'index-writer
			    :directory (fixture-var 'dir)
			    :analyzer (make-instance 'whitespace-analyzer)
			    :create-p T)))
     (setf (fixture-var 'documents) (index-test-helper-prepare-search-docs))
     (dosequence (doc (fixture-var 'documents))
       (add-document-to-index-writer iw doc))
     (close iw)
     (setf (fixture-var 'is) (make-instance 'index-searcher
					    :directory (fixture-var 'dir)))))

  (:teardown
   (close (fixture-var 'is))
   (close (fixture-var 'dir)))
  (:testfun test-boolean-query-with-two-must-not
   (let ((sub-query-1 (make-instance 'boolean-query))
	 (sub-query-2 (make-instance 'boolean-query))
	 (query (make-instance 'boolean-query)))
     (add-query query (make-instance 'term-query
				   :term (make-term "field" "word1"))
		:should-occur)
     (add-query query (make-instance 'term-query
					   :term (make-term "field" "word1"))
		:must-not-occur)
     (add-query query (make-instance 'term-query
					   :term (make-term "field" "word1"))
		:must-not-occur)
     (check-hits (fixture-var 'is) query '()))))
(in-package #:montezuma)

(deftestfixture boolean-subscorer-test
  (:setup
   (setf (fixture-var 'dir) (make-instance 'ram-directory))
   (let ((iw (make-instance 'index-writer
			    :directory (fixture-var 'dir)
			    :analyzer (make-instance 'whitespace-analyzer)
			    :create-p T)))
     (setf (fixture-var 'documents) (index-test-helper-prepare-search-docs))
     (dosequence (doc (fixture-var 'documents))
       (add-document-to-index-writer iw doc))
     (close iw)
     (setf (fixture-var 'is) (make-instance 'index-searcher
					    :directory (fixture-var 'dir)))))

  (:teardown
   (close (fixture-var 'is))
   (close (fixture-var 'dir)))
  (:testfun test-boolean-query-with-two-must-not
   (let ((sub-query-1 (make-instance 'boolean-query))
	 (sub-query-2 (make-instance 'boolean-query))
	 (query (make-instance 'boolean-query)))
     (add-query query (make-instance 'term-query
				   :term (make-term "field" "word1"))
		:should-occur)
     (add-query query (make-instance 'term-query
					   :term (make-term "field" "word1"))
		:must-not-occur)
     (add-query query (make-instance 'term-query
					   :term (make-term "field" "word1"))
		:must-not-occur)
     (check-hits (fixture-var 'is) query '()))))
(in-package #:montezuma)

(deftestfixture boolean-subscorer-test
  (:setup
   (setf (fixture-var 'dir) (make-instance 'ram-directory))
   (let ((iw (make-instance 'index-writer
			    :directory (fixture-var 'dir)
			    :analyzer (make-instance 'whitespace-analyzer)
			    :create-p T)))
     (setf (fixture-var 'documents) (index-test-helper-prepare-search-docs))
     (dosequence (doc (fixture-var 'documents))
       (add-document-to-index-writer iw doc))
     (close iw)
     (setf (fixture-var 'is) (make-instance 'index-searcher
					    :directory (fixture-var 'dir)))))

  (:teardown
   (close (fixture-var 'is))
   (close (fixture-var 'dir)))
  (:testfun test-boolean-query-with-two-must-not
   (let ((sub-query-1 (make-instance 'boolean-query))
	 (sub-query-2 (make-instance 'boolean-query))
	 (query (make-instance 'boolean-query)))
     (add-query query (make-instance 'term-query
				   :term (make-term "field" "word1"))
		:should-occur)
     (add-query query (make-instance 'term-query
					   :term (make-term "field" "word1"))
		:must-not-occur)
     (add-query query (make-instance 'term-query
					   :term (make-term "field" "word1"))
		:must-not-occur)
     (check-hits (fixture-var 'is) query '()))))

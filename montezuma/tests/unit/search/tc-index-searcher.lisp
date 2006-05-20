(in-package #:montezuma)


(defun get-docs (score-docs)
  (mapcar #'doc score-docs))

(defun check-hits (is query expected &optional top total-hits)
;;  (format T "~&query: ~S expected: ~S~%" query expected)
  (let ((top-docs (search is query)))
;;    (format T "~&got: ~S~%" (score-docs top-docs))
    (atest check-hits-0 (length (score-docs top-docs)) (length expected))
    (when top
      (atest check-hits-1 top (doc (elt (score-docs top-docs) 0))))
    (if total-hits
	(atest check-hits-2 (total-hits top-docs) total-hits)
	(atest check-hits-3 (total-hits top-docs) (length expected)))
    (dosequence (score-doc (score-docs top-docs))
      (atest check-hits-4 (member (doc score-doc) expected) T #'bool=)
      ;; FIXME: Once we implement explain, add back the explain-based
      ;; tests.
      ;;      (atest check-hits-5 (value (explain-score is query (doc score-doc)))
      ;;	     (score score-doc))
)))

(defun check-docs (is query expected &rest options)
  (let* ((top-docs (apply #'search is query options))
	 (docs (score-docs top-docs)))
    (atest check-docs-1 (length docs) (length expected))
    (dotimes (i (length docs))
      (atest check-docs-2 (doc (elt docs i)) (elt expected i)))))

(deftestfixture index-searcher-test
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
  (:testfun test-index-searcher-get-doc
   (let ((is (fixture-var 'is)))
     (test index-searcher-get-doc-1 (max-doc is) 18)
     (test index-searcher-get-doc-2
	   (document-values (get-document is 0) :|date|)
	   "20050930"
	   #'string=)
     (test index-searcher-get-doc-3
	   (document-values (get-document is 4) :|cat|)
	   "cat1/sub2/subsub2"
	   #'string=)))
  (:testfun test-term-query
   (flet ((term-query (field value) (make-instance 'term-query
					   :term (make-term field value)))
	  (check-hits (query expected)
	    (check-hits (fixture-var 'is) query expected)))
     (let ((tq (term-query "field" "word2")))
       (setf (boost tq) 100)
       (check-hits tq '(1 4 8)))
     (check-hits (term-query "field" "2342") '())
     (check-hits (term-query "field" "") '())
     (let ((tq (term-query "field" "word1")))
       (let ((top-docs (search (fixture-var 'is) tq)))
	 (test term-query-1 (total-hits top-docs) (length (fixture-var 'documents)))
	 (test term-query-2 (length (score-docs top-docs)) 10))
       (let ((top-docs (search (fixture-var 'is) tq :num-docs 20)))
	 (test term-query-3 (length (score-docs top-docs)) (length (fixture-var 'documents)))))))
  (:testfun test-first-doc
   (let ((tq (make-instance 'term-query
			    :term (make-term "field" "word1")))
	 (is (fixture-var 'is)))
     (setf (boost tq) 100)
     (let* ((top-docs (search is tq :num-docs 100))
	    (expected (mapcar #' doc (score-docs top-docs))))
       (condition-test first-doc-1
		       (search is tq :first-doc -1)
		       'error)
       (condition-test first-doc-2
		       (search is tq :num-docs 0)
		       'error)
       (condition-test first-doc-3
		       (search is tq :num-docs -1)
		       'error)
       (check-docs is tq (subseq expected 0 8) :num-docs 8 :first-doc 0)
       (check-docs is tq (subseq expected 1 4) :num-docs 3 :first-doc 1)
       (check-docs is tq (subseq expected 2 8) :num-docs 6 :first-doc 2)
       (check-docs is tq '() :num-docs 2 :first-doc (length expected))
       (check-docs is tq '() :num-docs 2 :first-doc (+ (length expected) 100)))))
  (:testfun test-boolean-query
   (let ((bq (make-instance 'boolean-query))
	 (tq1 (make-instance 'term-query
			     :term (make-term "field" "word1")))
	 (tq2 (make-instance 'term-query
			     :term (make-term "field" "word3"))))
     (add-query bq tq1 :must-occur)
     (add-query bq tq2 :must-occur)
     (check-hits (fixture-var 'is) bq '(2 3 6 8 11 14) 14)
     (let ((tq3 (make-instance 'term-query
			       :term (make-term "field" "word2"))))
       (add-query bq tq3 :should-occur)
       (check-hits (fixture-var 'is) bq '(2 3 6 8 11 14) 8)
       (let ((bq (make-instance 'boolean-query)))
	 (add-query bq tq2 :must-occur)
	 (add-query bq tq3 :must-not-occur)
	 (check-hits (fixture-var 'is) bq '(2 3 6 11 14)))
       (let ((bq (make-instance 'boolean-query)))
	 (add-query bq tq2 :must-not-occur)
	 (check-hits (fixture-var 'is) bq '()))
       (let ((bq (make-instance 'boolean-query)))
	 (add-query bq tq2 :should-occur)
	 (add-query bq tq3 :should-occur)
	 (check-hits (fixture-var 'is) bq '(1 2 3 4 6 8 11 14))))))
  (:testfun test-phrase-query
   (let ((pq (make-instance 'phrase-query))
	 (t1 (make-term "field" "quick"))
	 (t2 (make-term "field" "brown"))
	 (t3 (make-term "field" "fox")))
     (add-term-to-query pq t1)
     (check-hits (fixture-var 'is) pq '(1 11 14 16 17))
     (add-term-to-query pq t2)
     (check-hits (fixture-var 'is) pq '(1))
     (add-term-to-query pq t3)
     (check-hits (fixture-var 'is) pq '(1))
     (setf pq (make-instance 'phrase-query))
     (add-term-to-query pq t1)
     (add-term-to-query pq t3 2)
     (check-hits (fixture-var 'is) pq '(1 11 14))
     (setf (slop pq) 1)
     (check-hits (fixture-var 'is) pq '(1 11 14 16))
     (setf (slop pq) 4)
     (check-hits (fixture-var 'is) pq '(1 11 14 16 17))))
)

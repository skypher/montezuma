(in-package montezuma-lift)

(deftestsuite test-term-search ()
  (searcher)
  (:setup (let* ((index-directory "user-home:temporary;montezuma-test-1")
                 (dir (make-fs-directory index-directory :create-p t)))
            (add-test-documents lift::test dir)
            (setf searcher (create-searcher-for-test lift::test dir))))
  (:teardown (close searcher)))

(defmethod get-analyzer ((test test-term-search))
  (make-instance 'standard-analyzer))

(defmethod use-compound-index-p ((test test-term-search))
  (values t))

(defmethod create-searcher-for-test ((test test-term-search) directory)
  (make-instance 'index-searcher :directory directory))

(defmethod add-test-documents ((test test-term-search) directory)
  (let ((writer (make-instance 'index-writer
                  :directory directory
                  :create-p t
                  :analyzer (get-analyzer test)
                  :use-compound-file-p (use-compound-index-p test))))
    (loop for (id title text) in
          `(("1" "Big Sleep, the" "Rock a bye baby")
            ("2" "Black Sheep" "Baa Baa Black Sheep, have you any wool")
            ("3" "Dance all Day" "Rock yourself to sleep")
            ("4" "Unconquerable World, the" "Power to the people. Black and white"))
          do
          (let ((document (make-instance 'document)))
            (add-field 
             document (make-keyword-field "id" id))
            (add-field 
             document (make-unindexed-field "title" title))
            (add-field 
             document (make-text-field "text" text))
            (add-document-to-index-writer writer document)))
    ;;(optimize writer)
    (close writer))) 

(addtest (test-term-search)
  test-1
  (ensure-same (max-doc searcher) 4))

(addtest (test-term-search)
  test-2
  (let* ((term (make-term "text" "sleep"))
         (query (make-instance 'term-query :term term)))
    ;(inspect (weight query searcher))
    ;(inspect (search searcher query))
    (inspect (scorer (weight query searcher) (reader searcher)))
    ))
    

#|
(let ((test (make-instance 'test-term-search)))
  (lift::setup-test test)
  )
|#

#|

;;; ---------------------------------------------------------------------------
;;; search it
;;; ---------------------------------------------------------------------------

(addtest (test-term-search)
  test-search-1
  (let* ((index-directory "user-home:temporary;montezuma-test")
         (dir (make-fs-directory index-directory :create-p t))
         (searcher (make-instance 'index-searcher :directory dir))
         (term (make-term "text" "sleep"))
         (query (make-instance 'term-query :term term))
         #+Ignore 
         (hits (search-index query)))
    ))

(let* ((index-directory "user-home:temporary;montezuma-test")
       (directory (make-fs-directory index-directory :create-p t))
       (writer (make-instance 'index-writer
                 :directory directory
                 :create-p t
                 :analyzer (make-instance 'standard-analyzer)
                 :use-compound-file-p t)))
    (loop for (id title text) in
          `(("1" "Big Sleep, the" "Rock a bye baby")
            ("2" "Black Sheep" "Baa Baa Black Sheep, have you any wool")
            ("3" "Dance all Day" "Rock yourself to sleep")
            ("4" "Unconquerable World, the" "Power to the people. Black and white")
            )
          do
          (let ((document (make-instance 'document)))
            (add-field 
             document (make-keyword-field "id" id))
            (add-field 
             document (make-unindexed-field "title" title))
            (add-field 
             document (make-text-field "text" text))
            (add-document-to-index-writer writer document)
            ))
    (optimize writer)
    (close writer))


(let* ((index-directory "user-home:temporary;montezuma-test")
       (directory (make-fs-directory index-directory :create-p nil))
       (searcher (make-instance 'index-searcher :directory directory))
       (term (make-term "text" "sleep"))
       (query (make-instance 'term-query :term term))
       (hits (search searcher query)))
  (close searcher))
|#
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
          `(("1" "RFID in depth" "Radio frequencies tie up my brain")
            ("3" "Big Sleep, the" "Rock a bye baby")
            ("5" "Black Sheep" "Baa Baa Black Sheep, have you any wool")
            ("7" "Dance all Day" "Rock yourself to sleep")
            ("9" "Unconquerable World, the" "Power to the people. I am Black and white"))
          do
          (let ((document (make-instance 'document)))
            (add-field 
             document (make-keyword-field "id" id))
            (add-field 
             document (make-unindexed-field "title" title))
            (add-field 
             document (make-text-field "text" text))
            (add-document-to-index-writer writer document)))
    (optimize writer)
    (close writer))) 

(addtest (test-term-search)
  test-max-doc
  (ensure-same (max-doc searcher) 5))

#+Notyet
(addtest (test-term-search)
  test-sleep
  (let* ((term (make-term "text" "sleep"))
         (query (make-instance 'term-query :term term)) 
         (hits (search searcher query)))
    (ensure-same (size hits) 1)
    (let ((docs
           ;;?? ought to be a function for this already somewhere...
           (let ((result nil))
             (each hits (lambda (hit) 
                          (push (list (get-document searcher (doc hit))
                                      (score hit)) result)))
             (nreverse result))))
      (ensure-same (first (first docs)) 4))))

#+test
(addtest (test-term-search)
  test-Black
  (let* ((term (make-term "text" "black"))
         (query (make-instance 'term-query :term term)) 
         (hits (search searcher query)))
    (inspect hits)
    (break)))

(addtest (test-term-search)
  test-Black
  (let* ((term (make-term "text" "black"))
         (query (make-instance 'term-query :term term)) 
         (hits (search searcher query)))
    (ensure-same (size hits) 2)
    (let ((score-docs
           ;;?? ought to be a function for this already somewhere...
           (let ((result nil))
             (each hits (lambda (hit) (push hit result)))
             (nreverse result))))
      (ensure-same (doc (first score-docs)) 2)
      (ensure-same (doc (second score-docs)) 4))))
    

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
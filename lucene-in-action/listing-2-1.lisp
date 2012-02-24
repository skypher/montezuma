(in-package common-lisp-user)

(defpackage #:montezuma-lift
  (:shadow #:directory #:read-byte #:write-byte #:write-string 
	   #:close #:delete-file #:rename-file #:count #:search
	   #:merge #:file-length #:read #:write #:delete #:optimize)
  (:use #:common-lisp #:montezuma #:lift))
(in-package montezuma-lift)

#|
> Error: Unbound-slot POSITION in #<SEGMENT-TERM-ENUM #x3FFF4A6>.
> While executing: #<CCL::STANDARD-KERNEL-METHOD SLOT-UNBOUND (T T T)>
> Type Command-. to abort.
See the RestartsÉ menu item for further choices.
2 > 

|#

(let ((index (make-instance 'index
               :path "user-home:temporary;montezuma-test")))
  (add-document-to-index index '(("title" . "Programming Ruby")
				   ("content" . "blah blah blah")))
  (optimize (writer index))
  (close (writer index)))

(deftestsuite test-indexing ()
  ()
  (:setup (let* ((index-directory "user-home:temporary;montezuma-test")
                 (dir (make-fs-directory index-directory :create-p t)))
            (add-test-documents lift::test dir))))
       
(defmethod documents-keywords ((test test-indexing))
  (list "1" "2"))

(defmethod documents-unindexed ((test test-indexing))
  (list "Netherlands" "Italy"))

(defmethod documents-unstored ((test test-indexing))
  (list "Amsterdam has lots of bridges"
        "Venice has lots of canals"))

(defmethod documents-text ((test test-indexing))
  (list "Amsterdam" "Venice"))

(defmethod get-analyzer ((test test-indexing))
  (make-instance 'standard-analyzer))

(defmethod use-compound-index-p ((test test-indexing))
  (values t))

(defmethod add-test-documents ((test test-indexing) directory)
  (let ((writer (make-instance 'index-writer
                  :directory directory
                  :create-p t
                  :analyzer (get-analyzer test)
                  :use-compound-file-p (use-compound-index-p test))))
    (loop for keyword in (documents-keywords test)
          for unindexed in (documents-unindexed test)
          for unstored in (documents-unstored test)
          for text in (documents-text test) do
          (let ((document (make-instance 'document)))
            (add-field 
             document (make-keyword-field "id" keyword))
            (add-field 
             document (make-unindexed-field "country" unindexed))
            (add-field 
             document (make-unstored-field "contents" unstored))
            (add-field 
             document (make-text-field "city" text))
            (add-document-to-index-writer writer document)))
    (optimize writer)
    (close writer))) 

(addtest (test-indexing)
  noop
  (values))

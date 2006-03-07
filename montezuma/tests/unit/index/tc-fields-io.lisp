(in-package #:montezuma)

(deftestfixture fields-writer
    (:vars dir)
  (:setup
   (setf (fixture-var 'dir) (make-instance 'ram-directory)))
  (:teardown
   (close (fixture-var 'dir)))
  (:testfun test-fields-writer
   (let ((doc (make-instance 'document)))
     (add-field doc (make-field "name" "daily news" :stored T :index :tokenized))
     (add-field doc (make-field "content" "Nothing happened today." :stored T))
     (let ((infos (make-instance 'field-infos)))
       (add-doc-fields infos doc)
       (let ((writer (make-instance 'fields-writer
				    :directory (fixture-var 'dir)
				    :segment "fieldswritertest"
				    :field-infos infos)))
	 (add-document writer doc)
	 (close writer))
       (let ((fstream (open-input (fixture-var 'dir)
				  "fieldswritertest.fdt")))
	 (let ((stored (read-vint fstream))
	       (field-num1 (read-vint fstream))
	       (byte1 (read-byte fstream))
	       (data1 (read-string fstream)))
	   (test field-writer-1 stored 2)
	   (test field-writer-2 (and (logbitp +field-is-tokenized-bit+ byte1) T) T)
	   (test field-writer-3 data1 "daily news" #'string=))
	 (let ((field-num2 (read-vint fstream))
	       (byte2 (read-byte fstream))
	       (data2 (read-string fstream)))
	   (test field-writer-4 (logbitp byte2 +field-is-tokenized-bit+) NIL)
	   (test field-writer-5 data2 "Nothing happened today." #'string=)))))))

(deftestfixture fields-reader
  (:vars dir)
  (:setup
   (setf (fixture-var 'dir) (make-instance 'ram-directory)))
  (:teardown
   (close (fixture-var 'dir)))
  (:testfun test-fields-doc
   (let ((dir (fixture-var 'dir))
	 (doc (make-instance 'document)))
     (add-field doc (make-field "name" "daily news"))
     (add-field doc (make-field "content" "Nothing happened today."))
     (let ((infos (make-instance 'field-infos)))
       (add-doc-fields infos doc)
       (let ((fstream (create-output dir "fieldsreadertest.fdt"))
	     (istream (create-output dir "fieldsreadertest.fdx")))
	 (write-long istream 0)
	 (close istream)
	 (write-vint fstream 2)
	 (write-vint fstream 0)
	 (write-byte fstream 0)
	 (write-string fstream "daily news")
	 (write-vint fstream 1)
	 (write-byte fstream 0)
	 (write-string fstream "Nothing happened today.")
	 (close fstream))
       (let ((reader (make-instance 'fields-reader
				    :directory dir
				    :segment "fieldsreadertest"
				    :field-infos infos)))
	 (let ((docres (get-doc reader 0)))
	   (test field-reader-1 (field-data (document-field docres "name")) "daily news" #'equal)
	   (test field-reader-2 (field-data (document-field docres "content")) "Nothing happened today." #'equal)))))))

	 
(deftestfixture fields-io
  (:vars dir docres)
  (:setup
   (let ((dir (make-instance 'ram-directory)))
     (setf (fixture-var 'dir) dir)
     (let ((doc (index-test-helper-prepare-document))
	   (infos (make-instance 'field-infos)))
       (add-doc-fields infos doc)
       (let ((writer (make-instance 'fields-writer
				    :directory dir
				    :segment "field_types"
				    :field-infos infos)))
	 (add-document writer doc)
	 (close writer))
       (let ((reader (make-instance 'fields-reader
				    :directory dir
				    :segment "field_types"
				    :field-infos infos)))
	 (setf (fixture-var 'docres) (get-doc reader 0))))))
  (:teardown
   (close (fixture-var 'dir)))
  (:testfun test-text-field-no-term-vector
   (let ((field (document-field (fixture-var 'docres) "text_field1")))
     (test text-field-no-term-vector-1
	   field
	   '("field one text" T T T NIL NIL)
	   #'field-values-equal)))
  (:testfun test-text-field-term-vector
   (let ((field (document-field (fixture-var 'docres) "text_field2")))
     (test text-field-term-vector-1
	   field
	   '("field field field two text" T T T T NIL)
	   #'field-values-equal)))
  (:testfun test-key-field
   (let ((field (document-field (fixture-var 'docres) "key_field")))
     (test key-field-1
	   field
	   '("keyword" T T NIL NIL NIL)
	   #'field-values-equal)))
  (:testfun test-unindexed-field
   (let ((field (document-field (fixture-var 'docres) "unindexed_field")))
     (test unindexed-field-1
	   field
	   '("unindexed field text" T NIL NIL NIL NIL)
	   #'field-values-equal)))
  (:testfun test-unstored-field-no-term-vector
   (let ((field (document-field (fixture-var 'docres) "unstored_field1")))
     (test unstored-field-no-term-vector-1
	   field
	   nil)))
  (:testfun test-compressed-field
   (let ((field (document-field (fixture-var 'docres) "compressed_field")))
     (test comrpessed-field-1
	   field
	   '("compressed text" T T T T NIL)
	   #'field-values-equal)))
  (:testfun test-binary-field
   (let ((field (document-field (fixture-var 'docres) "binary_field")))
     (test binary-field-1
	   field
	   `(,*index-test-helper-binary-data* T NIL NIL NIL T)
	   #'field-values-equal)))
  (:testfun test-compressed-binary-field
   (let ((field (document-field (fixture-var 'docres) "compressed_binary_field")))
     (test compressed-binary-field-1
	   field
	   `(,*index-test-helper-compressed-binary-data* T NIL NIL NIL T)
	   #'field-values-equal))))

  


	 

(defun field-values-equal (field values)
  (flet ((bool= (a b) (or (and a b) (and (not a) (not b)))))
    (destructuring-bind (value stored indexed tokenized term-vector binary)
	values
      (and (equalp (field-data field) value)
	   (bool= (field-stored-p field) stored)
	   (bool= (field-indexed-p field) indexed)
	   (bool= (field-tokenized-p field) tokenized)
	   (bool= (field-store-term-vector-p field) term-vector)
	   (bool= (field-binary-p field) binary)))))

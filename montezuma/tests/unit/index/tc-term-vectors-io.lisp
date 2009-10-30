(in-package #:montezuma)

(flet ((t (a b)
	 (make-instance 'term-vector-offset-info :start-offset a :end-offset b)))
  (deftestfixture term-vectors-io
      (:vars dir fis)
    (:setup
     (setf (fixture-var 'dir) (make-instance 'ram-directory))
     (let ((fis (make-instance 'field-infos)))
       (setf (fixture-var 'fis) fis)
       (add-field-info fis "field1" :indexed-p T :store-term-vector T :store-position T :store-offset T)
       (add-field-info fis "field2" :indexed-p T :store-term-vector T)))
    (:teardown
     (close (fixture-var 'dir))
     (setf (fixture-var 'dir) nil))
    (:testfun test-term-vector-io-add-fields
	      (let ((tv-w (make-instance 'term-vectors-writer
					 :directory (fixture-var 'dir)
					 :segment "_test"
					 :field-infos (fixture-var 'fis))))
		(open-document tv-w)
		(test term-vector-io-add-fields-1 (and (document-open-p tv-w) T) T)
		(open-field tv-w "field1")
		(add-term-to-term-vectors-writer tv-w "text1" 1 :positions #(1) :offsets (vector (t 0 4)))
		(add-term-to-term-vectors-writer tv-w "text2" 2 :positions #(3 4) :offsets (vector (t 5 10) (t 11 16)))
		(close-field tv-w)
		(close-document tv-w)
		(close tv-w))
	      (let ((tv-r (make-instance 'term-vectors-reader
					 :directory (fixture-var 'dir)
					 :segment "_test"
					 :field-infos (fixture-var 'fis))))
		(test term-vector-io-add-fields-2 (size tv-r) 1)
		(let ((tv (get-field-term-vector tv-r 0 "field1")))
		  (test term-vector-io-add-fields-3 (size tv) 2)
		  (test term-vector-io-add-fields-4 (aref (terms tv) 0) "text1" #'string=)
		  (test term-vector-io-add-fields-5 (aref (term-frequencies tv) 0) 1)
		  (test term-vector-io-add-fields-6 (aref (aref (positions tv) 1) 0) 3)
		  (test term-vector-io-add-fields-7 (aref (aref (offsets tv) 1) 0) (t 5 10) #'term-vector-offset-info=)
		  (test term-vector-io-add-fields-8 (aref (aref (positions tv) 1) 1) 4)
		  (test term-vector-io-add-fields-9 (aref (aref (offsets tv) 1) 1) (t 11 16) #'term-vector-offset-info=)
		  (close tv-r))))
    (:testfun test-term-vector-io-add-fields-bmp
	      (let ((text1 "תורה1")
		    (text2 "תורה2"))
		(let ((tv-w (make-instance 'term-vectors-writer
					   :directory (fixture-var 'dir)
					   :segment "_test"
					   :field-infos (fixture-var 'fis))))
		  (open-document tv-w)
		  (test term-vector-io-add-fields-bmp-1 (and (document-open-p tv-w) T) T)
		  (open-field tv-w "field1")
		  (add-term-to-term-vectors-writer tv-w text1 1 :positions #(1) :offsets (vector (t 0 4)))
		  (add-term-to-term-vectors-writer tv-w text2 2 :positions #(3 4) :offsets (vector (t 5 10) (t 11 16)))
		  (close-field tv-w)
		  (close-document tv-w)
		  (close tv-w))
		(let ((tv-r (make-instance 'term-vectors-reader
					   :directory (fixture-var 'dir)
					   :segment "_test"
					   :field-infos (fixture-var 'fis))))
		  (test term-vector-io-add-fields-bmp-2 (size tv-r) 1)
		  (let ((tv (get-field-term-vector tv-r 0 "field1")))
		    (test term-vector-io-add-fields-bmp-3 (size tv) 2)
		    (test term-vector-io-add-fields-bmp-4 (aref (terms tv) 0) text1 #'string=)
		    (test term-vector-io-add-fields-bmp-5 (aref (term-frequencies tv) 0) 1)
		    (test term-vector-io-add-fields-bmp-6 (aref (aref (positions tv) 1) 0) 3)
		    (test term-vector-io-add-fields-bmp-7 (aref (aref (offsets tv) 1) 0) (t 5 10) #'term-vector-offset-info=)
		    (test term-vector-io-add-fields-bmp-8 (aref (aref (positions tv) 1) 1) 4)
		    (test term-vector-io-add-fields-bmp-9 (aref (aref (offsets tv) 1) 1) (t 11 16) #'term-vector-offset-info=)
		    (close tv-r)))))
    (:testfun test-term-vector-io-add-fields-overflow-buffer
	      (let ((text1 "Pseudo")
		    (text2 "Pseudopseudohypoparathyroidism"))
		(let ((tv-w (make-instance 'term-vectors-writer
					   :directory (fixture-var 'dir)
					   :segment "_test"
					   :field-infos (fixture-var 'fis))))
		  (open-document tv-w)
		  (test test-term-vector-io-add-fields-overflow-buffer-1 (and (document-open-p tv-w) T) T)
		  (open-field tv-w "field1")
		  (add-term-to-term-vectors-writer tv-w text1 1 :positions #(1) :offsets (vector (t 0 4)))
		  (add-term-to-term-vectors-writer tv-w text2 2 :positions #(3 4) :offsets (vector (t 5 10) (t 11 16)))
		  (close-field tv-w)
		  (close-document tv-w)
		  (close tv-w))
		(let ((tv-r (make-instance 'term-vectors-reader
					   :directory (fixture-var 'dir)
					   :segment "_test"
					   :field-infos (fixture-var 'fis))))
		  (test test-term-vector-io-add-fields-overflow-buffer-2 (size tv-r) 1)
		  (let ((tv (get-field-term-vector tv-r 0 "field1")))
		    (test test-term-vector-io-add-fields-overflow-buffer-3 (size tv) 2)
		    (test test-term-vector-io-add-fields-overflow-buffer-4 (aref (terms tv) 0) text1 #'string=)
		    (test test-term-vector-io-add-fields-overflow-buffer-5 (aref (term-frequencies tv) 0) 1)
		    (test test-term-vector-io-add-fields-overflow-buffer-6 (aref (aref (positions tv) 1) 0) 3)
		    (test test-term-vector-io-add-fields-overflow-buffer-7 (aref (aref (offsets tv) 1) 0) (t 5 10) #'term-vector-offset-info=)
		    (test test-term-vector-io-add-fields-overflow-buffer-8 (aref (aref (positions tv) 1) 1) 4)
		    (test test-term-vector-io-add-fields-overflow-buffer-9 (aref (aref (offsets tv) 1) 1) (t 11 16) #'term-vector-offset-info=)
		    (test test-term-vector-io-add-fields-overflow-buffer-10 (aref (terms tv) 1) text2 #'string=)
		    (close tv-r)))))
    (:testfun test-term-vector-io-add-documents
	      (let ((tvs1
		     (list
		      (make-instance 'segment-term-vector
				     :field "field1"
				     :terms (vector "word1" "woord2")
				     :term-frequencies (vector 3 2)
				     :positions (vector (vector 1 5 8)
							(vector 2 9))
				     :offsets (vector (vector (t 0 5) (t 34 39) (t 45 50))
						      (vector (t 6 11) (t 51 56))))
		      (make-instance 'segment-term-vector
				     :field "field2"
				     :terms (vector "word3" "woord4")
				     :term-frequencies (vector 1 5)
				     :positions (vector (vector 8)
							(vector 2 9 11 34 56))
				     :offsets (vector (vector (t 45 50))
						      (vector (t 6 10) (t 51 56)
							      (t 64 69) (t 103 108)
							      (t 183 188))))))
		    (tvs2
		     (list
		      (make-instance 'segment-term-vector
				     :field "field1"
				     :terms (vector "word1" "woord2")
				     :term-frequencies (vector 3 2)
				     :positions (vector (vector 1 5 8)
							(vector 2 9))
				     :offsets (vector (vector (t 0 5) (t 34 39) (t 45 50))
						      (vector (t 6 11) (t 51 56)))))))
		(let ((tv-w (make-instance 'term-vectors-writer
					   :directory (fixture-var 'dir)
					   :segment "_test"
					   :field-infos (fixture-var 'fis))))
		  (add-all-doc-vectors tv-w tvs1)
		  (add-all-doc-vectors tv-w tvs2)
		  (close tv-w))
		(let ((tv-r (make-instance 'term-vectors-reader
					   :directory (fixture-var 'dir)
					   :segment "_test"
					   :field-infos (fixture-var 'fis))))
		  (let ((tv (get-field-term-vector tv-r 0 "field1")))
		    (test term-vector-io-add-documents-1 (size tv) 2)
		    (test term-vector-io-add-documents-2 (aref (terms tv) 0) "word1" #'string=)
		    (test term-vector-io-add-documents-3 (aref (term-frequencies tv) 0) 3)
		    (test term-vector-io-add-documents-4 (aref (aref (positions tv) 0) 0) 1)
		    (test term-vector-io-add-documents-5 (aref (aref (positions tv) 0) 1) 5)
		    (test term-vector-io-add-documents-6 (aref (aref (positions tv) 0) 2) 8)
		    (test term-vector-io-add-documents-7 (aref (aref (offsets tv) 0) 0) (t 0 5) #'term-vector-offset-info=)
		    (test term-vector-io-add-documents-8 (aref (aref (offsets tv) 0) 1) (t 34 39) #'term-vector-offset-info=)
		    (test term-vector-io-add-documents-9 (aref (aref (offsets tv) 0) 2) (t 45 50) #'term-vector-offset-info=)
		    (test term-vector-io-add-documents-10 (aref (terms tv) 1) "woord2" #'string=)
		    (test term-vector-io-add-documents-11 (aref (term-frequencies tv) 1) 2)
		    (test term-vector-io-add-documents-12 (aref (aref (positions tv) 1) 0) 2)
		    (test term-vector-io-add-documents-13 (aref (aref (positions tv) 1) 1) 9)
		    (test term-vector-io-add-documents-14 (aref (aref (offsets tv) 1) 0) (t 6 11) #'term-vector-offset-info=)
		    (test term-vector-io-add-documents-15 (aref (aref (offsets tv) 1) 1) (t 51 56) #'term-vector-offset-info=)
		    (test term-vector-io-add-documents-16 (aref (aref (offsets tv) 0) 2) (t 45 50) #'term-vector-offset-info=))
		  (let ((tv (get-field-term-vector tv-r 0 "field2")))
		    (test term-vector-io-add-documents-17 (size tv) 2)
		    (test term-vector-io-add-documents-18 (aref (terms tv) 0) "word3" #'string=))
		  (let ((tv (get-field-term-vector tv-r 1 "field1")))
		    (test term-vector-io-add-documents-19 (size tv) 2)
		    (test term-vector-io-add-documents-20 (aref (terms tv) 0) "word1" #'string=)))))
    (:testfun test-term-vector-io-close-rw
	      (test term-vector-io-close-nil-reader
		    (close (make-instance 'term-vectors-reader
					  :directory (make-instance 'ram-directory)
					  :segment "doesnotexist")) nil))))



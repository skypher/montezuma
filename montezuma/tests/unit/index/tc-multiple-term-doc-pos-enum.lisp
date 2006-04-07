(in-package #:montezuma)

(deftestfixture multiple-term-doc-pos-enum
  (:vars ir dir)
  (:setup
   (let* ((dir (make-instance 'ram-directory))
	  (iw (make-instance 'index-writer
			     :directory dir
			     :analyzer (make-instance 'whitespace-analyzer)
			     :create-if-missing-p T))
	  (documents (index-test-helper-prepare-search-docs)))
     (setf (fixture-var 'dir) dir
	   (fixture-var 'documents) documents)
     (dosequence (doc documents)
       (add-document-to-index-writer iw doc))
     (close iw)
     (setf (fixture-var 'ir) (open-index-reader dir :close-directory-p T))))
  (:teardown
   (close (fixture-var 'ir)))
  (:testfun test-mtdpe
   (let ((t1 (make-term "field" "red"))
	 (t2 (make-term "field" "brown"))
	 (t3 (make-term "field" "hairy")))
     (let ((mtdpe (make-instance 'multiple-term-doc-pos-enum
				 :reader (fixture-var 'ir)
				 :terms (list t1 t2 t3))))
       (test mtdpe-1 (next mtdpe) T #'bool=)
       (test mtdpe-2 (doc mtdpe) 1)
       (test mtdpe-3 (freq mtdpe) 1)
       (test mtdpe-4 (next-position mtdpe) 4)

       (test mtdpe-5 (next mtdpe) T #'bool=)
       (test mtdpe-6 (doc mtdpe) 8)
       (test mtdpe-7 (freq mtdpe) 1)
       (test mtdpe-8 (next-position mtdpe) 5)

       (test mtdpe-9 (next mtdpe) T #'bool=)
       (test mtdpe-10 (doc mtdpe) 11)
       (test mtdpe-11 (freq mtdpe) 1)
       (test mtdpe-12 (next-position mtdpe) 4)

       (test mtdpe-13 (next mtdpe) T #'bool=)
       (test mtdpe-14 (doc mtdpe) 14)
       (test mtdpe-15 (freq mtdpe) 1)
       (test mtdpe-16 (next-position mtdpe) 4)

       (test mtdpe-17 (next mtdpe) T #'bool=)
       (test mtdpe-18 (doc mtdpe) 16)
       (test mtdpe-19 (freq mtdpe) 3)
       (test mtdpe-20 (next-position mtdpe) 5)
       (test mtdpe-21 (next-position mtdpe) 7)
       (test mtdpe-22 (next-position mtdpe) 11)

       (test mtdpe-23 (next mtdpe) T #'bool=)
       (test mtdpe-24 (doc mtdpe) 17)
       (test mtdpe-25 (freq mtdpe) 2)
       (test mtdpe-26 (next-position mtdpe) 2)
       (test mtdpe-27 (next-position mtdpe) 7)

       (test mtdpe-28 (next mtdpe) NIL #'bool=)
       (close mtdpe))))
)

       
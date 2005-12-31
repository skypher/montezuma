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
     (close (fixture-var 'dir)))
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
		(close tv-w)))))

   
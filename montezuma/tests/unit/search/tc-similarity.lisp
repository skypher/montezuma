(in-package #:montezuma)

(deftestfun test-similarity-byte-float-conversion
  (dotimes (i 256) 
    (atest test-inverse 
           (similarity-float-to-byte (similarity-byte-to-float i))
           i #'=)
    (atest test-norm-table-values 
           (similarity-byte-to-float i) (aref *norm-table* i) #'=)
    (atest test-encoding-inverse 
           (similarity-encode-norm (similarity-decode-norm i)) i #'=)))

(defclass test-similarity-searcher ()
  ())

(defmethod term-doc-freq ((self test-similarity-searcher) term)
  (declare (ignore term))
  9)

(defmethod max-doc ((self test-similarity-searcher))
  10)

(deftestfun test-similarity-default-similarity
  (let ((dsim (make-instance 'default-similarity)))
    (test test-1 (/ 4.0) (length-norm dsim "field" 16))
    (test test-2 (/ 4.0) (query-norm dsim 16))
    (test test-3 3.0 (tf dsim 9))
    (test test-4 (/ 10.0) (sloppy-freq dsim 9))
    (test test-5 1.0 (idf dsim 9 10))
    (test test-6 4.0 (coord dsim 12 3))
    
    (let ((searcher (make-instance 'test-similarity-searcher))
          (terms (list (make-term "field1" "text1")
                       (make-term "field1" "text2")
                       (make-term "field2" "text3")
                       (make-term "field2" "text4"))))
      (test test-7 1.0 (idf-term dsim (make-term "field" "text") searcher))
      (test test-8 4.0 (idf-phrase dsim terms searcher)))))
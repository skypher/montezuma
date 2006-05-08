(in-package #:montezuma)

(defclass wildcard-query (multi-term-query)
  ())

(defmethod get-term-enum ((self wildcard-query) reader)
  (make-instance 'wildcard-term-enum
		 :reader reader
		 :search-term (slot-value self 'term)))


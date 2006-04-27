(in-package #:montezuma)

;; This is just a dumb stub implementation for now so I can write unit
;; tests for index.lisp.


(defclass query-parser ()
  ((field)
   (default-search-field :initarg :default-search-field)
   (analyzer)
   (fields :accessor fields)
   (options :initarg :options))
  (:default-initargs
   :default-search-field "*" :options '()))

(defmethod initialize-instance :after ((self query-parser) &key)
  (with-slots (options) self
    (check-type options index-options-list)))

(defmethod parse ((self query-parser) query)
  (make-instance 'term-query
		 :term (make-term (slot-value self 'default-search-field)
				  "one")))

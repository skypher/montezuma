(in-package montezuma)

(defclass filter ()
  ()
  (:documentation 
   "Abstract base class providing a mechanism to restrict searches to a subset of an index."))

(defgeneric bits (filter reader)
  (:documentation "Returns a BitSet with true for documents which should be permitted in search results, and false for those that should not."))
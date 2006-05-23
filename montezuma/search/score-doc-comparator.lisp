(in-package montezuma)

(defclass score-doc-comparator ()
  ())

(defgeneric sort-value (score-doc document-index)
  (:documentation ""))

#|
Returns the value used to sort the given document.  The object returned must implement the java.io.Serializable interface.  This is used by multisearchers to determine how to collate results from their searchers.

See FieldDoc

i:: Document
returns:: Serializable object
|#


(defgeneric sort-type (score-doc)
  (:documentation "Returns the type of sort.  Should return +SortField.SCORE+, +SortField.DOC+, +SortField.STRING+, +SortField.INTEGER+, +SortField.FLOAT+ or +SortField.CUSTOM+.  It is not valid to return +SortField.AUTO+.

This is used by multisearchers to determine how to collate results from their searchers.  returns:: One of the constants in SortField.
See SortField."))

(defclass simple-field-comparator (score-doc-comparator)
  ((index :initarg :index :reader index)
   (sort-type :initarg :sort-type :accessor sort-type)))

(defmethod compare ((a simple-field-comparator) (b simple-field-comparator))
  (values (<=> (index (doc  a)) (index (doc b)))))

#||
(defmethod sort-value ((self simple-field-comparator) document-index)
  (values (index (doc ))))
||#

;;; ---------------------------------------------------------------------------
;;; special-field-comparator
;;; ---------------------------------------------------------------------------

(defclass special-field-comparator (simple-field-comparator)
  ((comparator :initarg :comparator :reader comparator)))

(defmethod compare ((a special-field-comparator) (b special-field-comparator))
  )

;;; ---------------------------------------------------------------------------
;;; string-field-comparator
;;; ---------------------------------------------------------------------------

(defclass string-field-comparator (score-doc-comparator)
  ())

(defmethod compare ((a string-field-comparator) (b string-field-comparator))
  (values (<=> (index (doc  a)) (index (doc b)))))

#||
(defmethod sort-value ((self string-field-comparator) document-index)
  (values (index (doc ))))
||#

(defmethod sort-type ((self string-field-comparator))
  )

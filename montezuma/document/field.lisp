(in-package #:montezuma)

;; A field is a section of a Document.  Each field has two parts, a
;; name and a value.  Values may be free text, provided as a String or
;; as a Reader, or they may be atomic keywords, which are not further
;; processed.  Such keywords may be used to represent dates, urls,
;; etc.  Fields are optionally stored in the index, so that they may
;; be returned with hits on the document.

(defclass field ()
  ((name :initarg :name :reader field-name)
   (data :initarg :data :accessor field-data)
   (boost :initarg :boost :accessor boost)

   (stored-p :initarg :stored-p :accessor field-stored-p)
   (indexed-p :initarg :indexed-p :accessor field-indexed-p)
   (tokenized-p :initarg :tokenized-p :accessor field-tokenized-p)
   (binary-p :initarg :binary-p :accessor field-binary-p)
   (compressed-p :initarg :compressed-p :accessor field-compressed-p)
   (store-term-vector-p :initarg :store-term-vector-p :accessor field-store-term-vector-p)
   (store-positions-p :initarg :store-positions-p :accessor field-store-positions-p)
   (store-offsets-p :initarg :store-offsets-p :accessor field-store-offsets-p)
   (omit-norms-p :initarg :omit-norms-p :accessor field-omit-norms-p)))

(defmethod print-object ((self field) stream)
  (print-unreadable-object (self stream :type T :identity T)
    (with-slots (name data stored-p compressed-p  indexed-p tokenized-p store-term-vector-p
		      store-offsets-p store-positions-p omit-norms-p binary-p) self
      (when stored-p
	(format stream "stored")
	(if compressed-p
	    (format stream "/compressed,")
	    (format stream "/uncompressed,")))
      (when indexed-p (format stream "indexed,"))
      (when tokenized-p (format stream "tokenized,"))
      (when store-term-vector-p (format stream "store_term_vector,"))
      (when store-offsets-p (format stream "tv_offset,"))
      (when store-positions-p (format stream "tv_position,"))
      (when omit-norms-p (format stream "omit_norms,"))
      (when binary-p (format stream "binary,"))
      (format stream " ~S:~S" name data))))
	


(defmethod (setf field-stored) (stored (self field))
  (check-type stored (member NIL T :compress))
  (setf (field-stored-p self) (and stored T))
  (setf (field-compressed-p self) (eq stored :compress))
  stored)

(defmethod (setf field-index) (index (self field))
  (check-type index (member NIL :tokenized :untokenized :no-norms))
  (setf (field-indexed-p self) (and index T))
  (setf (field-tokenized-p self) (eq index :tokenized))
  (setf (field-omit-norms-p self) (eq index :no-norms))
  index)

(defmethod (setf field-store-term-vector) (store-term-vector (self field))
  (check-type store-term-vector (member NIL T :with-positions :with-offsets :with-positions-offsets))
  (setf (field-store-term-vector-p self) (and store-term-vector T))
  (setf (field-store-positions-p self) (and (member store-term-vector '(:with-positions :with-positions-offsets)) T))
  (setf (field-store-offsets-p self) (and (member store-term-vector '(:with-offsets :with-positions-offsets)) T))
  store-term-vector)

(defun make-field (name value &key (stored T) (index :untokenized) (store-term-vector NIL) (binary-p NIL) (boost 1.0))
  (check-type index (member NIL :tokenized :untokenized :no-norms))
  (check-type stored (member NIL T :compress))
  (check-type store-term-vector (member NIL T :with-positions :with-offsets :with-positions-offsets))
  (when (and (eq index NIL) (eq stored NIL))
    (error "It doesn't make sense to have a field that is neither indexed nor stored."))
  (when (and (eq index NIL) (not (eq store-term-vector NIL)))
    (error "Cannot store term vector information for a field that is not indexed."))
  (let ((f (make-instance 'field
			  :name name
			  :data value
			  :binary-p binary-p
			  :boost boost)))
    (setf (field-stored f) stored)
    (setf (field-index f) index)
    (setf (field-store-term-vector f) store-term-vector)
    f))

(defun make-binary-field (name value stored)
  (when (eq stored NIL)
    (error "Binary values can't be unstored"))
  (make-field name value :stored stored :index NIL :store-term-vector NIL :binary-p T))



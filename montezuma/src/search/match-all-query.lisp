(in-package #:montezuma)

(defclass match-all-query (query)
  ())


(defclass match-all-scorer (scorer)
  ((reader :initarg :reader)
   (count :initform -1)
   (max-doc)))

(defmethod initialize-instance :after ((self match-all-scorer) &key)
  (with-slots (max-doc reader) self
    (setf max-doc (max-doc reader))))

(defmethod document ((self match-all-scorer))
  (slot-value self 'count))

(defmethod explain ((self match-all-scorer))
  (make-instance 'explanation
		 :thing 1.0
		 :thing2 "match-all-query"))

(defmethod next? ((self match-all-scorer))
  (with-slots (count max-doc reader) self
    (loop while (< count (- max-doc 1))
	 do
	 (incf count)
	 (when (not (deleted-p reader count))
	   (return-from next? T)))
    NIL))

(defmethod score ((self match-all-scorer))
  1.0)

(defmethod skip-to ((self match-all-scorer) target)
  (setf (slot-value self 'count) (- target 1))
  (next? self))


(defclass match-all-weight (weight)
  ((query :initarg :query :reader query)
   (searcher :initarg :searcher)))

(defmethod value ((self match-all-weight))
  1.0)

(defmethod sum-of-squared-weights ((self match-all-weight))
  1.0)

(defmethod normalize-weight ((self match-all-weight) query-norm)
  )

(defmethod scorer ((self match-all-weight) reader)
  (with-slots (query searcher) self
    (make-instance 'match-all-scorer
		   :reader reader
		   :similarity (similarity-implementation query searcher))))

(defmethod create-weight ((self match-all-query) searcher)
  (make-instance 'match-all-weight
		 :query self
		 :searcher searcher))

							   

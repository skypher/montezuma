(in-package montezuma)

(defclass term-query (query)
  ((term :reader term :initarg t)))

(defclass term-weight (weight)
  ((value :reader value :initform 0)
   (query :reader query :initarg :query)
   (query-weight :accessor query-weight)
   (similarity :accessor similarity)
   (term-idf :accessor term-idf)))

(defmethod initialize-instance :after ((self term-weight) &key query searcher)
  (setf (similarity self) (similarity-implementation query searcher)
        (term-idf self) (idf (similarity self)
                             (term-doc-freq searcher (term query))
                             (max-doc searcher))))

(defmethod sum-of-squared-weights ((self term-weight))
  (setf (query-weight self) (* (term-idf self) (boost (query self))))
  (values (* (query-weight self) (query-weight self))))

#|
(defmethod normalize-weight (weight)
  )

(defmethod scorer ()
  )

(defmethod explain ()
            )
|#
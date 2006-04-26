(in-package montezuma)

(defclass term-query (query)
  ((term :reader term :initarg :term)))

(defclass term-weight (weight)
  ((value :accessor value :initform 0)
   (query :reader query :initarg :query)
   (query-weight :accessor query-weight)
   (similarity :accessor similarity)
   (term-idf :accessor term-idf)
   (term-query-norm :accessor term-query-norm)))

(defmethod initialize-instance :after ((self term-weight) &key query searcher)
  (setf (similarity self) (similarity-implementation query searcher)
        (term-idf self) (idf (similarity self)
                             (term-doc-freq searcher (term query))
                             (max-doc searcher))))

(defmethod sum-of-squared-weights ((self term-weight))
  (setf (query-weight self) (* (term-idf self) (boost (query self))))
  (values (* (query-weight self) (query-weight self))))

(defmethod normalize-weight ((self term-weight) query-norm)
  (setf (term-query-norm self) query-norm
        (query-weight self) (* (query-weight self) query-norm)
        (value self) (* (query-weight self) (term-idf self))))

(defmethod scorer ((self term-weight) reader)
  (let ((term-docs (term-docs-for reader (term (query self)))))
    (if term-docs
	(make-instance 'term-scorer
		       :weight self
		       :term-docs term-docs
		       :similarity (similarity self)
		       :norms (get-norms reader (term-field (term (query self)))))
	nil)))

(defmethod explain-score ((self term-weight) reader doc)
  (let ((explanation (make-instance 'explanation 
                       :description (format nil "Weight(~A in ~A), product of:" 
                                            (query self) (doc self))))
        (idf-explanation (make-instance 'explanation
                           :idf (idf self)
                           :description (format nil "idf(doc-freq=~A)" 
                                                (doc-freq reader)
                                                (term (query self)))))
        (query-explanation (make-instance 'explanation
                             :description (format nil "query-weight(~A), product of:" 
                                                (query self))))
        (boost-explanation (make-instance 'explanation
                             (boost (query self))
                             :description "boost")))
    (unless (= (boost (query self)) 1.0)
      (<< query-explanation boost-explanation))
    (<< query-explanation idf-explanation)
    
    
    (values explanation)))

(defmethod create-weight ((self term-query) searcher)
  (make-instance 'term-weight
    :searcher searcher
    :query self))

(defmethod extract-terms ((self term-query) terms)
  (<< terms (term self)))

;;?? to-s
;;?? eql?
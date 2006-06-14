(in-package montezuma)

;;?? <<
;;?? create-weight
;;?? to-s
;;?? eql?

(defparameter +default-max-clause-count+ (expt 2 20))

(defclass boolean-query (query)
  ((clauses :initform (make-array 10 :adjustable T :fill-pointer 0) :accessor clauses)
   (coord-disabled :initform T :initarg :coord-disabled
                   :reader coord-disabled?)
   (max-clause-count :initarg :max-clause-count
                     :accessor max-clause-count
                     :allocation :class))
  (:default-initargs 
    :max-clause-count +default-max-clause-count+))


(defmethod print-object ((self boolean-query) stream)
  (print-unreadable-object (self stream :type T)
    (format stream "~S clauses: ~{~A~^ ~}" (length (clauses self)) (coerce (clauses self) 'list))))

(define-condition too-many-clauses-error (error)
                  ()
  (:documentation "Thrown when an attempt is made to add more than #max_clause_count() clauses. This typically happens if a PrefixQuery, FuzzyQuery, WildcardQuery, or RangeQuery is expanded to many terms during search."))


#|
Constructs an empty boolean query.

Similarity#coord(int,int) may be disabled in scoring, as appropriate. For example, this score factor does not make sense for most automatically generated queries, like WildcardQuery and FuzzyQuery. 

coord_disabled:: disables Similarity#coord(int,int) in scoring.
|#

(defun coord-disabled-function (similarity overlap max-overlap)
  (declare (ignore similarity overlap max-overlap))
  (values 1.0))

(defmethod similarity-implementation ((self boolean-query) searcher)
  (declare (ignore searcher))
  ;;?? should this be a copy of sim?
  (let ((sim (call-next-method)))
    (when (coord-disabled? self)
      (setf (slot-value sim 'coord-function) 'coord-disabled-function))
    sim))

(defgeneric add-query (boolean-query query occur)
  (:documentation ""))

(defmethod add-query ((self boolean-query) query occur)
  (add-clause self (make-instance 'boolean-clause :query query :occur occur)))

(defgeneric add-clause  (boolean-query clause))

(defmethod add-clause  ((self boolean-query) clause)
  (when (> (length (clauses self)) (max-clause-count self))
    (error 'too-many-clauses-error))
  
  (vector-push-extend clause (clauses self)))

(defmethod create-weight ((self boolean-query) searcher)
  (make-instance 'boolean-weight :query self :searcher searcher))

(defmethod rewrite ((self boolean-query) reader)
  (when (= (length (clauses self)) 1)
    (let ((clause (aref (clauses self) 0)))
      (unless (prohibited? clause)
        (let ((query (rewrite (query clause) reader)))
          (when (/= (boost self) 1.0)
            ;; if rewrite was a no-op, make a clone
            (when (eq query (query clause))
              (setf query (clone query)))
            (setf (boost query) (* (boost self) (boost query))))
          
          ;; early returns
          (return-from rewrite query)))))
  
  (let ((clone nil))
    (dosequence (clause (clauses self) :index i)
      (let ((query (rewrite (query clause) reader)))
	(when (not (eq query (query clause)))
	  (setf clone (or clone (clone self)))
	  (setf (aref (clauses clone) i)
		(make-instance 'boolean-clause
			       :query query
			       :occur (occur clause))))))
    (if (not (null clone))
      ;; we did some re-writing
      (values clone)
      ;; everything is the same
      (values self))))

(defmethod extract-terms ((self boolean-query) terms)
  (dosequence (clause (clauses self))
    (extract-terms (query clause) terms)))


(defgeneric combine (boolean-query queries))

(defmethod initialize-copy :after ((copy boolean-query) original)
  (setf (clauses copy) (clone (clauses original))))




;;; ---------------------------------------------------------------------------
;;; boolean-weight
;;; ---------------------------------------------------------------------------

(defclass boolean-weight (weight)
  ((similarity :accessor similarity)
   (weights :accessor weights :initform '())
   (query :initarg :query :reader query)
   (searcher :initarg :searcher :reader searcher)))

(defmethod initialize-instance :after ((self boolean-weight) &key)
  (setf (similarity self) (similarity-implementation (query self) (searcher self)))
  (let ((weights '())
	(searcher (searcher self)))
    (dosequence (clause (clauses (query self)))
      (push (create-weight (query clause) searcher)
	    weights))
    (setf (weights self) (reverse weights))))


#|
        query.clauses.each do |clause|

          @weights << clause.query.create_weight(searcher)

        end


|#

(defmethod value ((self boolean-weight))
  (values (boost (query self))))

(defmethod sum-of-squared-weights ((self boolean-weight))
  (let ((sum 0)
	(query (query self)))
    (dosequence (weight (weights self) :index i)
      (let ((clause (elt (clauses query) i)))
	(unless (prohibited? clause)
	  (incf sum (sum-of-squared-weights weight)))))
    (setf sum (* sum (boost query) (boost query)))))

(defmethod normalize-weight ((self boolean-weight) norm)
  (let ((query (query self)))
    (setf norm (* norm (boost query)))
    (dosequence (weight (weights self) :index i)
      (let ((clause (elt (clauses query) i)))
	(unless (prohibited? clause)
	  (normalize-weight weight norm))))))

  #|
        norm *= @query.boost()

        @weights.each_with_index do |weight, i|

          clause = @query.clauses[i]

          if not clause.prohibited?

            weight.normalize(norm)

          end

        end


|#


(defmethod scorer ((self boolean-weight) reader)
  (let ((result (make-instance 'boolean-scorer
			       :similarity (similarity self)))
	(query (query self)))
    (dosequence (weight (weights self) :index i)
      (let* ((clause (elt (clauses query) i))
	     (sub-scorer (scorer weight reader)))
	(if sub-scorer
	    (add-scorer result sub-scorer (occur clause))
	    (when (required? clause)
	      (return-from scorer nil)))))
    result))
	

#||
(defmethod explain-score ((self boolean-weight) reader doc)
  )
||#


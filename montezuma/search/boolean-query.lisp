(in-package montezuma)

;;?? <<
;;?? create-weight
;;?? to-s
;;?? eql?

(defparameter +default-max-clause-count+ 1024)

(defclass boolean-query (query)
  ((clauses :initform nil :accessor clauses)
   (coord-disabled :initform nil :initarg :coord-disabled
                   :reader coord-disabled?)
   (max-clause-count :initarg :max-clause-count
                     :accessor max-clause-count
                     :allocation :class))
  (:default-initargs 
    :max-clause-count +default-max-clause-count+))

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

(defmethod query-similarity ((self boolean-query) searcher)
  ;;?? should this be a copy of sim?
  (let ((sim (when (next-method-p) (call-next-method))))
    (when (coord-disabled? self)
      (setf (slot-value sim 'coord-function) 'coord-disabled-function))
    sim))

(defgeneric add-query (boolean-query query occur)
  (:documentation ""))

(defmethod add-query ((self boolean-query) query occur)
  (add-clause self (make-instance 'boolean-clause :query query :occur occur)))

(defmethod add-clause  ((self boolean-query) clause)
  (when (> (length (clauses self)) (max-clause-count self))
    (error 'too-many-clauses-error))
  
  (setf (clauses self) (nreverse (clauses self)))
  (push clause (clauses self))
  (setf (clauses self) (nreverse (clauses self))))

(defmethod create-weight ((self boolean-query) searcher)
  (make-instance 'boolean-weight :query self :searcher searcher))

(defmethod rewrite ((self boolean-query) reader)
  (when (length-1-list-p (clauses self))
    (let ((clause (first (clauses self))))
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
    ;;?? iterate
    (if (not (null clone))
      ;; we did some re-writing
      (values clone)
      ;; everything is the same
      (values self))))

(defmethod extract-terms ((self boolean-query) terms)
#|
      @clauses.each do |clause|
        clause.query.extract_terms(terms)
      end
|#  )

(defmethod combine ((self boolean-query) queries)
  )

(defmethod initialize-copy :after ((copy boolean-query) original)
  (setf (clauses copy) (copy-list (clauses original))))




;;; ---------------------------------------------------------------------------
;;; boolean-weight
;;; ---------------------------------------------------------------------------

(defclass boolean-weight (weight)
  ((similarity :accessor similarity)
   (weights :accessor weights :initform nil)
   (query :reader query :initform :query)
   (searcher :reader searcher :initform :searcher)))

(defmethod initialize-instance :after ((self boolean-weight) &key)
  (setf (similarity self) (query-similarity query searcher))


#|
        query.clauses.each do |clause|

          @weights << clause.query.create_weight(searcher)

        end


|#  )

(defmethod value ((self boolean-weight))
  (values (boost (query self))))

(defmethod sum-of-squared-weights ((self boolean-weight))
  (let ((sum 0))
    #|
        @weights.each_with_index do |weight, i|

          clause = @query.clauses[i]

          if not clause.prohibited?

            sum += weight.sum_of_squared_weights()         # sum sub weights

          end

        end



        sum *= @query.boost() * @query.boost()             # boost each sub-weight


|#
    (values sum)))

(defmethod normalize ((self boolean-weight) norm)
  #|
        norm *= @query.boost()

        @weights.each_with_index do |weight, i|

          clause = @query.clauses[i]

          if not clause.prohibited?

            weight.normalize(norm)

          end

        end


|#
  )

(defmethod scorer ((self boolean-weight) reader)
  )

(defmethod explain ((self boolean-weight) reader doc)
  )


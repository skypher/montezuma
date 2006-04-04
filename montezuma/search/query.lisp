(in-package montezuma)

;;?? compare
;;?? merge-boolean-queries

(defclass query ()
  ((boost :accessor boost :initform 1.0)))

(defmethod weight ((self query) searcher)
  (let ((query (rewrite searcher self))
        (weight (create-weight query searcher))
        (sum (sum-of-squared-weights weight))
        (norm (query-norm (similarity searcher) sum)))
    (values (normalize weight norm))))

(defmethod compare ((self query) queries)
#|
      queries.each do |query|
        if self != query
          raise ArgumentError
        end
      end
      return self

|#
  )

(defmethod merge-boolean-queries ((self query) queries)
#|
      all_clauses = Set.new
      queries.each do |query|
        query.clauses.each do |clause|
          all_clauses << clause
        end
      end

      coord_disabled = queries.size==0 ? false : queries[0].coord_disabled?
      result = BooleanQuery.new(coord_disabled)
      all_clauses.each do |clause|
        result << clause
      end
      return result
|#  
  )





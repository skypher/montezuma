(in-package montezuma)


(defclass weight ()
  ())

(defgeneric query (weight)
  (:documentation "The query that this concerns. "))

(defgeneric value (weight)
  (:documentation "The weight for this query."))

(defgeneric sum-of-squared-weights (weight)
  (:documentation "The sum of squared weights of contained query clauses. "))

(defgeneric normalize-weight (weight norm)
  (:documentation "Assigns the query normalization factor to this."))

(defgeneric scorer (weight reader)
  (:documentation "Constructs a scorer for this. "))

(defgeneric explain-score (weight reader document-index)
  (:documentation "An explanation of the score computation for the named document. "))



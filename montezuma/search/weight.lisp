(in-package montezuma)

;;?? wacko? 

(defclass weight ()
  ())

(defgeneric query (weight)
  (:documentation ""))

(defgeneric value (weight)
  (:documentation ""))

(defgeneric sum-of-squared-weights (weight)
  (:documentation ""))

(defgeneric normalize-weight (weight)
  (:documentation ""))

(defgeneric scorer (weight)
  (:documentation ""))

(defgeneric explain (weight reader document-index)
  (:documentation ""))



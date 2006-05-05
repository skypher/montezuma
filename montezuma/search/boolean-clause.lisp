(in-package montezuma)

;;?? equality testing in ruby (cf. eql?)
;;?? alias :== eql?

(defclass boolean-clause ()
  ((query :initform nil :accessor query :initarg :query
          :documentation "The query whose matching documents are combined by the boolean query.")
   (required :initform nil :reader required? :writer required
             :documentation "If true, documents documents which _do not_ match this sub-query will _not_ match the boolean query.")
   (prohibited :initform nil :reader prohibited? :writer prohibited
               :documentation "If true, documents documents which _do_ match this sub-query will _not_ match the boolean query.")
   (occur :initform :should :accessor occur :initarg :occur
          :documentation "See BooleanQuery::Occur for values for this attribute.")))

(defmethod initialize-instance :after ((self boolean-clause) &key)
  (set-fields self (occur self)))

(defmethod print-object ((self boolean-clause) stream)
  (print-unreadable-object (self stream :type T)
    (format stream "~A: ~A" (occur self) (query self))))

(defmethod (setf occur) :after (value (self boolean-clause))
  (set-fields self value))

(defmethod eql? ((self boolean-clause) (other t))
  (and (typep other 'boolean-clause)
       (and (equal (query self) (query other))
            (equal (required? self) (required? other))
            (equal (prohibited? self) (prohibited? other)))))

(defmethod set-fields ((self boolean-clause) occur)
  (ecase (occur self)
    (:must-occur
     (required t self)
     (prohibited nil self))
    (:must-not-occur
     (required nil self) 
     (prohibited t self))
    (:should-occur
     (required nil self)
     (prohibited nil self))))


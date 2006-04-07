(in-package montezuma)

(defclass top-docs ()
  ((score-docs :accessor score-docs :initarg :score-docs)
    (total-hits :accessor total-hits :initarg :total-hits)
    (fields :accessor fields :initarg :fields :initform "SortField::FIELD_SCORE")))

(defmethod size ((self top-docs))
  (total-hits self))

(defmethod each ((self top-docs))
  ;;??
  )


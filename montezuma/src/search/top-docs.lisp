(in-package montezuma)

(defclass top-docs ()
  ((score-docs :accessor score-docs :initarg :score-docs
               :initform nil)
   (total-hits :accessor total-hits :initarg :total-hits
               :initform 0)
   (fields :accessor fields :initarg :fields
           :initform "?? SortField::FIELD_SCORE")))

(defmethod print-object ((self top-docs) stream)
  (print-unreadable-object (self stream :type T)
    (format stream "~S hits, ~S" (total-hits self) (score-docs self))))

(defmethod size ((self top-docs))
  (total-hits self))

(defmethod each ((self top-docs) block)
  (each (score-docs self) block))


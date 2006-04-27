(in-package montezuma)

(defclass sort-field ()
  ((name :initform nil :initarg :name)
   (sort-type :reader sort-type :initarg :sort-type
              :initform :auto)
   (comparator :reader comparator :initarg :comparator)
   (reverse? :reader reverse? :initarg :reverse?
             :initform nil)))

(defmethod print-object ((self sort-field) stream)
  (print-unreadable-object (self stream :identity t :type t)
    (format stream "")))
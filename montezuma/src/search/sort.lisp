(in-package #:montezuma)

(defclass sort ()
  ((fields :accessor fields :initarg :fields)
   (reverse :initarg :reverse-p))
  (:default-initargs
   :fields (list *field-score* *field-doc*)
    :reverse-p NIL))

(defmethod initialize-instance :after ((self sort) &key)
  (with-slots (fields) self
    (let ((reverse-p (slot-value self 'reverse-p)))
      (setf fields map 'vector #'(lambda (field)
				   (if (typep field 'sort-field)
				       field
				       (make-instance 'sort-field
						      :name (string field)
						      :sort-type *auto-sorter*
						      :reverse-p reverse-p)))))
    (when (= (length fields) 1)
      (setf fields (concatenate 'vector fields (vector *field-doc*))))))

(defparameter *relevance* (make-instance 'sort))

(defparameter *index-order* (make-instance 'sort
					   :fields (vector *field-doc*)))


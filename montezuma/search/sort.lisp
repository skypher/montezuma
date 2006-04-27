(in-package montezuma)

(defclass sorter ()
  ((fields :accessor fields)))

(defmethod initialize-instance :after ((self sorter) &key reverse fields)
  (setf (fields self) (coerce fields 'array))
  )


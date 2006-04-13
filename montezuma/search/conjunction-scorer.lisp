(in-package montezuma)

;;?? <<
;;?? next?, do-next, score, init, sort-scorers


(defclass conjunction-scorer (scorer)
  ((scorers :initform (make-scorers-array) :reader scorers)
   (first-time-p :initform t :reader first-time-p)
   (more-p :initform t :reader more-p)))

(defun make-scorers-array ()
  (make-array 10 :adjustable t :fill-pointer 0))

(defmethod add ((self conjunction-scorer) scorer)
  (vector-push-extend scorer (scorers self))
  (scorers self))

(defmethod first ((self conjunction-scorer))
  (aref (scorers self) 0))

(defmethod last ((self conjunction-scorer))
  (aref (scorers self) (1- (length (scorers self)))))

(defmethod doc ((self conjunction-scorer))
  (document (first self)))

(defmethod next? ((self conjunction-scorer))
  )

(defmethod do-next ((self conjunction-scorer))
  )

(defmethod score ((self conjunction-scorer))
  )

(defmethod init ((self conjunction-scorer) init-scorers)
  )

(defmethod sort-scorers ((self conjunction-scorer))
  )

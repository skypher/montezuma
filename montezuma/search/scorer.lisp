(in-package montezuma)

(defparameter +max-docs+ #x7FFFFFF)

(defclass scorer ()
  ((similarity :initarg :similarity :reader similarity)
   (pipe)))

(defmethod initialize-instance :after ((object scorer) &key)
  )

(defmethod setup-pipe ((self scorer))
  (setf (slot-value self 'pipe)
        (make-pipe nil nil)))          
         
(defmethod each-hit ((self scorer))
  #+Ignore
  (make-pipe first rest)
  empty-pipe)

(defmethod each-hit-up-to ((self scorer) (max-docs integer))
  (values (< (document self) max-docs)))

(in-package montezuma)

;;?? implementation of yield
(defparameter +max-docs+ #x7FFFFFF)

(defclass scorer ()
  ((similiarity :initarg :similiarity :reader similarity)))

(defmethod initialize-instance :after ((object scorer) &key)
  )

(defmethod each-hit ((self scorer) block)
  (loop while (next? self) do
        (funcall block (document self) (score self))))

(defmethod each-hit-up-to ((self scorer) (max-docs integer) block)
  (loop while (and (next? self)
                   (< (document self) max-docs)) do
        (funcall block (document self) (score self)))
  (values (< (document self) max-docs)))

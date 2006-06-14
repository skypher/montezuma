(in-package montezuma)

(defparameter +max-docs+ #x7FFFFFF)

(defclass scorer ()
  ((similarity :initarg :similarity :reader similarity)))

          
(defmethod each-hit ((self scorer) fn)
  (loop while (next? self) do (funcall fn (document self) (score self))))

#||
(defmethod each-hit-up-to ((self scorer) (max-docs integer) fn)
  (values (< (document self) max-docs)))
||#

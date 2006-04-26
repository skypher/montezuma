(in-package montezuma)

;;?? implementation of yield
(defparameter +max-docs+ #x7FFFFFF)

(defclass scorer ()
  ((similarity :initarg :similarity :reader similarity)))

(defmethod initialize-instance :after ((object scorer) &key)
  )

(defmethod hits ((self scorer))
  ;;(loop while (next? self) do
  ;;(funcall block (document self) (score self)))
  )

(defmethod hits-up-to ((self scorer) (max-docs integer))
  (loop while (and (next? self)
                   (< (document self) max-docs)) do
        (funcall block (document self) (score self)))
  (values (< (document self) max-docs)))

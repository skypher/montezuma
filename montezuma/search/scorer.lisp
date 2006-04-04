(in-package montezuma)

;;?? implementation of yield
(defparameter +max-docs+ #x7FFFFFF)

(defclass scorer ()
  ((similiarity :initarg :similiarity :reader similarity)))

(defmethod initialize-instance :after ((object scorer) &key)
  )

(defmethod each-hit ((self similarity))
  (loop while (next? self) do
        (yield (doc self) (score self))))

(defmethod each-hit-up-to ((self similarity) (max-docs integer))
  (loop while (and (next? self)
                   (< (doc self) max-docs)) do
        (yield (doc self) (score self)))
  (values (< (doc self) max-docs)))

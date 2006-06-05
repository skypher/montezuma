(in-package montezuma)

;;?? what does the % do here?
;;       "#{@doc} -> %0.2f" % @score

(defclass score-doc ()
  ((score :accessor score :initarg :score)
   (doc :accessor doc :initarg :doc)))

(defmethod print-object ((self score-doc) stream)
  (print-unreadable-object (self stream :identity t :type t)
    (format stream "#(~D) -> ~,2F" (doc self) (score self))))

(defmethod <=> ((self score-doc) (other score-doc))
  (let ((result (<=> (score self) (score other))))
    (if (zerop result)
      (values (<=> (doc other) (doc self)))
      (values result))))
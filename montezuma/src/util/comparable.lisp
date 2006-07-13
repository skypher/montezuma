(in-package #:montezuma)

(defgeneric <=> (a b)
  (:documentation "Ruby three way comparison operator. Returns -1, 0, or 1."))

(defmethod <=> ((a number) (b number))
  (cond ((= a b) 0)
        ((< a b) -1)
        (t 1)))
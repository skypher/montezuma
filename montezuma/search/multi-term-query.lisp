(in-package #:montezuma)

(defclass multi-term-query (query)
  ((term :initarg :term :reader term)))

(defmethod rewrite ((self multi-term-query) reader)
  (let ((enumerator (get-term-enum self reader))
	(bq (make-instance 'boolean-query)))
    (unwind-protect
	 (loop do
	      (let ((term (term enumerator)))
		(when term
		  (let ((tq (make-instance 'term-query
					   :term term)))
		    (setf (boost tq) (* (boost self) (difference enumerator)))
		    (add-query bq tq :should-occur))))
	    while (next? enumerator))
      (close enumerator))
    bq))


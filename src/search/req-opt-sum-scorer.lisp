(in-package #:montezuma)

(defclass req-opt-sum-scorer (scorer)
  ((req-scorer :initarg :req-scorer)
   (opt-scorer :initarg :opt-scorer)
   (first-time-opt-scorer-p :initform T)))


(defmethod next? ((self req-opt-sum-scorer))
  (next? (slot-value self 'req-scorer)))

(defmethod skip-to ((self req-opt-sum-scorer) target)
  (skip-to (slot-value self 'req-scorer) target))

(defmethod document ((self req-opt-sum-scorer))
  (document (slot-value self 'req-scorer)))

(defmethod score ((self req-opt-sum-scorer))
  (with-slots (req-scorer opt-scorer first-time-opt-scorer-p) self
    (let ((cur-doc (document req-scorer))
	  (req-score (score req-scorer)))
      (cond (first-time-opt-scorer-p
	     (setf first-time-opt-scorer-p NIL)
	     (when (not (skip-to (slot-value self 'opt-scorer) cur-doc))
	       (setf opt-scorer nil)
	       (return-from score req-score)))
	    ((null opt-scorer)
	     (return-from score req-score))
	    ((and (< (document opt-scorer) cur-doc)
		  (not (skip-to opt-scorer cur-doc)))
	     (setf opt-scorer nil)
	     (return-from score req-score)))
      (if (= (document opt-scorer) cur-doc)
	  (+ req-score (score opt-scorer))
	  req-score))))

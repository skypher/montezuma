(in-package montezuma)

;;?? FIXME: to-s

(defclass phrase-positions ()
  ((document :reader document :initform -1)
   (phrase-position :reader phrase-position :initform -1)
   (next :accessor next :initform nil)
   (tp-enum :initarg :tp-enum :reader tp-enum)
   (offset :initarg :offset)
   (position)
   (phrase-count :initform -1 :reader phrase-count)))

(defmethod print-object ((self phrase-positions) stream)
  (print-unreadable-object (self stream :type T)
    (format stream "document: ~S position: ~S"
	    (document self)
	    (if (slot-boundp self 'position)
		(slot-value self 'position)
		"[unbound]"))))

(defmethod next? ((self phrase-positions))
  (unless (next? (tp-enum self))
    (close (tp-enum self))
    (setf (slot-value self 'document) +max-docs+)
    (return-from next? NIL))
  (setf (slot-value self 'document) (doc (tp-enum self))
        (slot-value self 'position) 0)
  (slot-value self 'document))

(defmethod skip-to ((self phrase-positions) target)
  (unless (skip-to (tp-enum self) target)
    (close (tp-enum self))
    (setf (slot-value self 'document) +max-docs+)
    (return-from skip-to nil))
  (setf (slot-value self 'document) (doc (tp-enum self))
        (slot-value self 'position) 0)
  (slot-value self 'document))

(defmethod first-position ((self phrase-positions))
  (setf (slot-value self 'phrase-count) (freq (tp-enum self)))
  (next-position self))

(defmethod next-position ((self phrase-positions))
  (decf (slot-value self 'phrase-count))
  (if (>= (phrase-count self) 0)
      (progn
	(setf (slot-value self 'phrase-position)
	      (- (next-position (tp-enum self)) (slot-value self 'offset)))
	T)
      NIL))

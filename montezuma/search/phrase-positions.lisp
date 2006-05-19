(in-package montezuma)

;;?? FIXME: to-s

(defclass phrase-positions ()
  ((doc :reader doc :initform -1)
   (phrase-position :reader phrase-position :initform -1)
   (next :accessor next :initform nil)
   (tp-enum :initarg :tp-enum :reader tp-enum)
   (offset :initarg :offset)
   (position)
   (phrase-count :initform -1 :reader phrase-count)))

(defmethod next? ((self phrase-positions))
  (unless (next? (tp-enum self))
    (close (tp-enum self))
    (setf (slot-value self 'doc) +max-docs+)
    (return-from next? nil))
  (setf (slot-value self 'doc) (doc (tp-enum self))
        (slot-value self 'position) 0)
  (values t))

(defmethod skip-to ((self phrase-positions) target)
  (unless (skip-to (tp-enum self) target)
    (close (tp-enum self))
    (setf (slot-value self 'doc) +max-docs+)
    (return-from skip-to nil))
  (setf (slot-value self 'doc) (doc (tp-enum self))
        (slot-value self 'position) 0)
  (values t))

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

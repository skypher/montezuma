(in-package #:montezuma)


(defclass filtered-term-enum (term-enum)
  ((term :initform nil :reader term)
   (enum :initform nil)
   (reader :initform nil)))

(defmethod enum= ((self filtered-term-enum) enum)
  (setf (slot-value self 'enum) enum)
  (let ((term (term enum)))
    (if (and term (term-compare self term))
	(setf (slot-value self 'term) term)
	(next? self))))

(defmethod doc-freq ((self filtered-term-enum))
  (with-slots (enum) self
    (if (null enum)
	-1
	(doc-freq enum))))

(defmethod next? ((self filtered-term-enum))
  (with-slots (enum) self
    (if (null enum)
	NIL
	(progn
	  (setf (slot-value self 'term) nil)
	  (loop while (null (slot-value self 'term))
	       do
	       (when (or (end-enum self) (not (next? enum)))
		 (return-from next? NIL))
	       (let ((term (term enum)))
		 (when (term-compare self term)
		   (setf (slot-value self 'term) term)
		   (return-from next? T))))
	  (setf (slot-value self 'term) nil)
	  NIL))))

(defmethod close ((self filtered-term-enum))
  (with-slots (enum term) self
    (close enum)
    (setf term nil)
    (setf enum nil)))





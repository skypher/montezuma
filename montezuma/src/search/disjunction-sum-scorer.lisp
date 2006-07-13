(in-package #:montezuma)

(defclass disjunction-sum-scorer (scorer)
  ((num-scorers)
   (current-doc :initform -1)
   (current-score :initform nil)
   (num-matchers :initform -1)
   (minimum-num-matchers :initarg :minimum-num-matchers)
   (sub-scorers :initarg :sub-scorers)
   (scorer-queue :initform nil))
  (:default-initargs
   :similarity nil
   :minimum-num-matchers 1))

(defmethod initialize-instance :after ((self disjunction-sum-scorer) &key)
  (setf (slot-value self 'num-scorers) (length (slot-value self 'sub-scorers)))
  (when (<= (slot-value self 'minimum-num-matchers) 0)
    (error ":minimum-num-matchers must be greater than zero."))
  (when (<= (slot-value self 'num-scorers) 1)
    (error "There must be at least two sub-scorers.")))

(defgeneric init-scorer-queue (disjunction-sum-scorer))

(defmethod init-scorer-queue ((self disjunction-sum-scorer))
  (with-slots (scorer-queue sub-scorers num-scorers) self
    (setf scorer-queue (make-instance 'scorer-queue
				      :max-size num-scorers))
    (dosequence (sub-scorer sub-scorers)
      (when (next? sub-scorer)
	(queue-insert scorer-queue sub-scorer)))))


(defclass scorer-queue (priority-queue)
  ())

(defmethod less-than ((self scorer-queue) scorer1 scorer2)
  (< (document scorer1) (document scorer2)))


(defmethod next? ((self disjunction-sum-scorer))
  (with-slots (scorer-queue minimum-num-matchers) self
    (when (null scorer-queue)
      (init-scorer-queue self))
    (if (< (size scorer-queue) minimum-num-matchers)
	NIL
	(advance-after-current self))))


(defgeneric advance-after-current (disjunction-sum-scorer))

(defmethod advance-after-current ((self disjunction-sum-scorer))
  (with-slots (scorer-queue current-doc current-score minimum-num-matchers num-matchers num-scorers) self
    (block outer-loop
      (loop
	 do
	   (let ((top (queue-top scorer-queue)))
	     (setf current-doc (document top)
		   current-score (score top)
		   num-matchers 1)
	     (block inner-loop
	       (loop
		  do
		    (if (next? top)
			(adjust-top scorer-queue)
			(progn
			  (queue-pop scorer-queue)
			  (when (< (size scorer-queue)
				   (- minimum-num-matchers num-matchers))
			    (return-from advance-after-current NIL))
			  (when (= (size scorer-queue) 0)
			    (return-from inner-loop))))
		    (setf top (queue-top scorer-queue))
		    (if (not (eql (document top) current-doc))
			(return-from inner-loop)
			(progn
			  (incf current-score (score top))
			  (incf num-matchers)))))
	     (if (>= num-matchers minimum-num-matchers)
		 (return-from advance-after-current T)
		 (when (< (size scorer-queue) minimum-num-matchers)
		   (return-from advance-after-current NIL))))))))


(defmethod score ((self disjunction-sum-scorer))
  (slot-value self 'current-score))

(defmethod document ((self disjunction-sum-scorer))
  (slot-value self 'current-doc))

(defmethod skip-to ((self disjunction-sum-scorer) target)
  (with-slots (scorer-queue minimum-num-matchers current-doc) self
    (when (null scorer-queue)
      (init-scorer-queue self))
    (if (< (size scorer-queue) minimum-num-matchers)
	NIL
	(progn
	  (when (<= target current-doc)
	    (setf target (+ current-doc 1)))
	  (loop do
	       (let ((top (queue-top scorer-queue)))
		 (cond ((>= (document top) target)
			(return-from skip-to (advance-after-current self)))
		       ((skip-to top target)
			(adjust-top scorer-queue))
		       (T
			(queue-pop scorer-queue)
			(when (< (size scorer-queue) minimum-num-matchers)
			  (return-from skip-to NIL))))))))))

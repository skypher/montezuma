(in-package #:montezuma)

(defclass coordinator ()
  ((max-coord :initform 0)
   (coord-factors :initform nil)
   (similarity :initarg :similarity)))

(defmethod init ((self coordinator))
  (let* ((max-coord (slot-value self 'max-coord))
	 (coord-factors (make-array (+ max-coord 1)))
	 (similarity (slot-value self 'similarity)))
    (dotimes (i (+ max-coord 1))
      (setf (aref coord-factors i) (coord similarity i max-coord)))
    (setf (slot-value self 'coord-factors) coord-factors)))


(defclass boolean-scorer (scorer)
  ((counting-sum-scorer :initform nil)
   (required-scorers :initform '())
   (optional-scorers :initform '())
   (prohibited-scorers :initform '())
   (coordinator)))


(defmethod initialize-instance :after ((self boolean-scorer) &key)
  (with-slots (coordinator similarity) self
    (setf coordinator (make-instance 'coordinator :similarity similarity))))

(defmethod add-scorer ((self boolean-scorer) scorer occur)
  (unless (eq occur :must-not-occur)
    ;(incf (max-coord (coordinator self)))
    )
  (ecase occur
    (:must-occur)
    (:should-occur)
    (:must-not-occur)))

(defmethod next? ((self boolean-scorer))
  (with-slots (counting-sum-scorer) self
    (when (null counting-sum-scorer)
      (setf counting-sum-scorer (init-counting-sum-scorer self)))
    (next? counting-sum-scorer)))

(defmethod init-counting-sum-scorer ((self boolean-scorer))
  (with-slots (coordinator counting-sum-scorer) self
    (init coordinator)
    (setf counting-sum-scorer (make-counting-sum-scorer self))))

(defmethod make-counting-sum-scorer ((self boolean-scorer))
  (with-slots (required-scorers optional-scorers) self
    (cond ((= (size required-scorers) 0)
	   (cond ((= (size optional-scorers) 0)
		  (make-instance 'non-matching-scorer))
		 ((= (size optional-scorers) 1)
		  (make-counting-sum-scorer2 (make-instance 'single-match-scorer
							    :self self
							    :scorers (aref optional-scorers 0))
					     '()))
		 ((> (size optional-scorers) 1)
		  (make-counting-sum-scorer2 (counting-disjunction-sum-scorer optional-scorers)
					     '()))))
	  ((= (size required-scorers) 1)
	   (make-counting-sum-scorer2 (make-instance 'single-match-scorer
						     :self self
						     :scorers (aref required-scorers 0))
				      optional-scorers))
	  (T
	   (make-counting-sum-scorer2 (counting-disjunction-sum-scorer required-scorers)
				      optional-scorers)))))

	    
	      

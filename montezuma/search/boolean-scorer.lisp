(in-package #:montezuma)

(defclass coordinator ()
  ((max-coord :initform 0 :accessor max-coord)
   (coord-factors :initform nil)
   (num-matchers :accessor num-matchers)
   (similarity :initarg :similarity)))

(defmethod init ((self coordinator))
  (let* ((max-coord (slot-value self 'max-coord))
	 (coord-factors (make-array (+ max-coord 1)))
	 (similarity (slot-value self 'similarity)))
    (dotimes (i (+ max-coord 1))
      (setf (aref coord-factors i) (coord similarity i max-coord)))
    (setf (slot-value self 'coord-factors) coord-factors)))

(defmethod init-doc ((self coordinator))
  (with-slots (num-matchers) self
    (setf num-matchers 0)))

(defmethod coord-factor ((self coordinator))
  (with-slots (coord-factors num-matchers) self
    (aref coord-factors num-matchers)))


(defclass boolean-scorer (scorer)
  ((counting-sum-scorer :initform nil)
   (required-scorers :initform (make-scorers-array) :reader required-scorers)
   (optional-scorers :initform (make-scorers-array))
   (prohibited-scorers :initform (make-scorers-array))
   (coordinator :reader coordinator)))


(defmethod initialize-instance :after ((self boolean-scorer) &key)
  (with-slots (coordinator similarity) self
    (setf coordinator (make-instance 'coordinator :similarity similarity))))

(defmethod add-scorer ((self boolean-scorer) scorer occur)
  (unless (eq occur :must-not-occur)
    (incf (slot-value (coordinator self) 'max-coord)))
  (ecase occur
    ((:must-occur)
     (vector-push-extend scorer (slot-value self 'required-scorers)))
    ((:should-occur)
     (vector-push-extend scorer (slot-value self 'optional-scorers)))
    ((:must-not-occur)
     (vector-push-extend scorer (slot-value self 'prohibited-scorers)))))

(defmethod init-counting-sum-scorer ((self boolean-scorer))
  (with-slots (coordinator counting-sum-scorer) self
    (init coordinator)
    (setf counting-sum-scorer (make-counting-sum-scorer self))))

(defmethod next? ((self boolean-scorer))
  (with-slots (counting-sum-scorer) self
    (when (null counting-sum-scorer)
      (init-counting-sum-scorer self))
    (next? counting-sum-scorer)))

(defmethod each-hit ((self boolean-scorer) fn)
  (with-slots (counting-sum-scorer) self
    (when (null counting-sum-scorer)
      (init-counting-sum-scorer self))
    (loop while (next? counting-sum-scorer)
	 do (funcall fn (document counting-sum-scorer) (score self)))))


(defmethod make-counting-sum-scorer ((self boolean-scorer))
  (with-slots (required-scorers optional-scorers) self
    (cond ((= (length required-scorers) 0)
	   (cond ((= (length optional-scorers) 0)
		  (make-instance 'non-matching-scorer))
		 ((= (length optional-scorers) 1)
		  (make-counting-sum-scorer2 self
					     (make-instance 'single-match-scorer
							    :self self
							    :scorers (aref optional-scorers 0))
					     '()))
		 ((> (length optional-scorers) 1)
		  (make-counting-sum-scorer2 self
					     (counting-disjunction-sum-scorer self optional-scorers)
					     '()))))
	  ((= (length required-scorers) 1)
	   (make-counting-sum-scorer2 self
				      (make-instance 'single-match-scorer
						     :parent-scorer self
						     :scorers (aref required-scorers 0))
				      optional-scorers))
	  (T
	   (make-counting-sum-scorer2 self
				      (counting-conjunction-sum-scorer self required-scorers)
				      optional-scorers)))))


(defmethod counting-disjunction-sum-scorer ((self boolean-scorer) scorers)
  (make-instance 'counting-disjunction-sum-scorer
		 :parent-scorer self
		 :sub-scorers scorers))



(defclass counting-disjunction-sum-scorer (disjunction-sum-scorer)
  ((parent-scorer :initarg :parent-scorer)))

(defmethod score :around ((self counting-disjunction-sum-scorer))
  (incf (slot-value (slot-value (slot-value self 'parent-scorer) 'coordinator)
		    'num-matchers)
	(slot-value self 'num-matchers))
  (call-next-method))
  
  

(defmethod make-counting-sum-scorer2 ((self boolean-scorer) required-counting-sum-scorer optional-scorers)
  (with-slots (prohibited-scorers) self
    (cond ((= (length optional-scorers) 0)
	   (cond ((= (length prohibited-scorers) 0)
		  required-counting-sum-scorer)
		 ((= (length prohibited-scorers) 1)
		  (error "FOO"))
		 (T
		  (error "FOO!"))))
	  ((= (length optional-scorers) 1)
	   (make-counting-sum-scorer3 self
				      required-counting-sum-scorer
				      (make-instance 'single-match-scorer
						     :parent self
						     :thing (aref optional-scorers 0))))
	  (T
	   (error "OFO")))))


(defmethod document ((self boolean-scorer))
  (document (slot-value self 'counting-sum-scorer)))


(defmethod score ((self boolean-scorer))
  (with-slots (coordinator counting-sum-scorer) self
    (init-doc coordinator)
    (let ((sum (score counting-sum-scorer)))
      (* sum (coord-factor coordinator)))))


(defmethod counting-conjunction-sum-scorer ((self boolean-scorer) required-scorers)
  (let ((required-num-matchers (length required-scorers))
	(ccs (make-instance 'counting-conjunction-scorer
			    :parent-scorer self
			    :similarity (make-instance 'default-similarity))))
    (dosequence (scorer required-scorers)
      (add ccs scorer))
    ccs))

(defmethod skip-to ((self boolean-scorer) target)
  (with-slots (counting-sum-scorer) self
    (when (null counting-sum-scorer)
      (init-counting-sum-scorer self))
    (skip-to counting-sum-scorer target)))


(defclass counting-conjunction-scorer (conjunction-scorer)
  ((parent-scorer :initarg :parent-scorer)
   (required-num-matchers)
   (last-scored-doc :initform -1)))

(defmethod initialize-instance :after ((self counting-conjunction-scorer) &key)
  (setf (slot-value self 'required-num-matchers)
	(length (required-scorers (slot-value self 'parent-scorer)))))

(defmethod score :around ((self counting-conjunction-scorer))
  (with-slots (parent-scorer last-scored-doc required-num-matchers) self
    (when (> (document parent-scorer) last-scored-doc)
      (setf last-scored-doc (document parent-scorer))
      (incf (num-matchers (coordinator parent-scorer))
	    required-num-matchers))
    (call-next-method)))


(in-package #:montezuma)

;; -- BOOLEAN-SCORER

(defclass boolean-scorer (scorer)
  ((counting-sum-scorer :initform nil)
   (required-scorers :initform (make-scorers-array) :reader required-scorers)
   (optional-scorers :initform (make-scorers-array))
   (prohibited-scorers :initform (make-scorers-array))
   (coordinator :reader coordinator)))


;; -- COORDINATOR

(defclass coordinator ()
  ((max-coord :initform 0 :accessor max-coord)
   (coord-factors :initform nil)
   (num-matchers :accessor num-matchers)
   (similarity :initarg :similarity)))

(defgeneric init (coordinator))

(defmethod init ((self coordinator))
  (let* ((max-coord (slot-value self 'max-coord))
	 (coord-factors (make-array (+ max-coord 1)))
	 (similarity (slot-value self 'similarity)))
    (dotimes (i (+ max-coord 1))
      (setf (aref coord-factors i) (coord similarity i max-coord)))
    (setf (slot-value self 'coord-factors) coord-factors)))

(defgeneric init-doc (coordinator))

(defmethod init-doc ((self coordinator))
  (with-slots (num-matchers) self
    (setf num-matchers 0)))

(defgeneric coord-factor (coordinator))

(defmethod coord-factor ((self coordinator))
  (with-slots (coord-factors num-matchers) self
    (aref coord-factors num-matchers)))


;; -- BOOLEAN-SCORER

(defmethod initialize-instance :after ((self boolean-scorer) &key)
  (with-slots (coordinator similarity) self
    (setf coordinator (make-instance 'coordinator :similarity similarity))))

(defgeneric add-scorer (boolean-scorer scorer occur))

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

(defgeneric init-counting-sum-scorer (boolean-scorer))

(defmethod init-counting-sum-scorer ((self boolean-scorer))
  (with-slots (coordinator counting-sum-scorer) self
    (init coordinator)
    (setf counting-sum-scorer (make-counting-sum-scorer self))))


;; -- SINGLE-MATCH-SCORER

(defclass single-match-scorer (scorer)
  ((parent-scorer :initarg :parent-scorer)
   (scorer :initarg :scorer)))

(defmethod initialize-instance :after ((self single-match-scorer) &key)
  (setf (slot-value self 'similarity) (similarity (slot-value self 'scorer))))

(defmethod score ((self single-match-scorer))
  (with-slots (parent-scorer scorer) self
      (incf (num-matchers (coordinator parent-scorer)))
    (score scorer)))

(defmethod document ((self single-match-scorer))
  (document (slot-value self 'scorer)))

(defmethod next? ((self single-match-scorer))
  (next? (slot-value self 'scorer)))

(defmethod skip-to ((self single-match-scorer) doc-num)
  (skip-to (slot-value self 'scorer) doc-num))

#|
(defmethod explain-score ((self single-match-scorer) doc-num)
  (explain-score (slot-value self 'scorer) doc-num))
|#


;; -- COUNTING-DISJUNCTION-SUM-SCORER

(defclass counting-disjunction-sum-scorer (disjunction-sum-scorer)
  ((parent-scorer :initarg :parent-scorer)))

(defmethod score :around ((self counting-disjunction-sum-scorer))
  (incf (slot-value (slot-value (slot-value self 'parent-scorer) 'coordinator)
		    'num-matchers)
	(slot-value self 'num-matchers))
  (call-next-method))


;; -- BOOLEAN-SCORER  
  
(defgeneric counting-disjunction-sum-scorer (boolean-scorer scorers))

(defmethod counting-disjunction-sum-scorer ((self boolean-scorer) scorers)
  (make-instance 'counting-disjunction-sum-scorer
		 :parent-scorer self
		 :sub-scorers scorers))


;; -- COUNTING-CONJUNCTION-SCORER

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


;; -- BOOLEAN-SCORER

(defgeneric counting-conjunction-sum-scorer (boolean-scorer required-scorers))

(defmethod counting-conjunction-sum-scorer ((self boolean-scorer) required-scorers)
  (let ((ccs (make-instance 'counting-conjunction-scorer
			    :parent-scorer self
			    :similarity (make-instance 'default-similarity))))
    (dosequence (scorer required-scorers)
      (add ccs scorer))
    ccs))


(defgeneric make-counting-sum-scorer (boolean-scorer))

(defmethod make-counting-sum-scorer ((self boolean-scorer))
  (with-slots (required-scorers optional-scorers) self
    (cond ((= (length required-scorers) 0)
	   (cond ((= (length optional-scorers) 0)
		  (make-instance 'non-matching-scorer))
		 ((= (length optional-scorers) 1)
		  (make-counting-sum-scorer2 self
					     (make-instance 'single-match-scorer
							    :parent-scorer self
							    :scorer (aref optional-scorers 0))
					     '()))
		 (T
		  (make-counting-sum-scorer2 self
					     (counting-disjunction-sum-scorer self optional-scorers)
					     '()))))
	  ((= (length required-scorers) 1)
	   (make-counting-sum-scorer2 self
				      (make-instance 'single-match-scorer
						     :parent-scorer self
						     :scorer (aref required-scorers 0))
				      optional-scorers))
	  (T
	   (make-counting-sum-scorer2 self
				      (counting-conjunction-sum-scorer self required-scorers)
				      optional-scorers)))))


(defgeneric make-counting-sum-scorer2 (boolean-scorer required-counting-sum-scorer optional-scorers))

(defmethod make-counting-sum-scorer2 ((self boolean-scorer) required-counting-sum-scorer optional-scorers)
  (with-slots (prohibited-scorers) self
    (cond ((= (length optional-scorers) 0)
	   (cond ((= (length prohibited-scorers) 0)
		  required-counting-sum-scorer)
		 ((= (length prohibited-scorers) 1)
		  (make-instance 'req-excl-scorer
				 :req-scorer required-counting-sum-scorer
				 :excl-scorer (aref prohibited-scorers 0)))
		 (T
		  (make-instance 'req-excl-scorer
				 :req-scorer required-counting-sum-scorer
				 :excl-scorer (make-instance 'disjunction-sum-scorer
							     :scorer prohibited-scorers)))))
	  ((= (length optional-scorers) 1)
	   (make-counting-sum-scorer3 self
				      required-counting-sum-scorer
				      (make-instance 'single-match-scorer
						     :parent-scorer self
						     :scorer (aref optional-scorers 0))))
	  (T
	   (make-counting-sum-scorer3 self
				      required-counting-sum-scorer
				      (counting-disjunction-sum-scorer self optional-scorers))))))

(defgeneric make-counting-sum-scorer3 (boolean-scorer required-counting-sum-scorer optional-counting-sum-scorer))

(defmethod make-counting-sum-scorer3 ((self boolean-scorer) required-counting-sum-scorer optional-counting-sum-scorer)
  (with-slots (prohibited-scorers) self
    (cond ((= (length prohibited-scorers) 0)
	  (make-instance 'req-opt-sum-scorer
			 :req-scorer required-counting-sum-scorer
			 :opt-scorer optional-counting-sum-scorer))
	  ((= (length prohibited-scorers) 1)
	   (make-instance 'req-opt-sum-scorer
			  :req-scorer (make-instance 'req-excl-scorer
						     :req-scorer required-counting-sum-scorer
						     :excl-scorer (aref prohibited-scorers 0))
			  :opt-scorer optional-counting-sum-scorer))
	  (T
	   (make-instance 'req-opt-sum-scorer
			  :req-scorer (make-instance 'req-excl-scorer
						     :req-scorer required-counting-sum-scorer
						     :excl-scorer (make-instance 'disjunction-sum-scorer
											:sub-scorers prohibited-scorers))
			  :opt-scorer optional-counting-sum-scorer)))))


;; -- BOOLEAN-SCORER

(defmethod each-hit ((self boolean-scorer) fn)
  (with-slots (counting-sum-scorer) self
    (when (null counting-sum-scorer)
      (init-counting-sum-scorer self))
    (loop while (next? counting-sum-scorer)
	 do (funcall fn (document counting-sum-scorer) (score self)))))


(defmethod each-hit-up-to ((self boolean-scorer) max fn)
  (with-slots (counting-sum-scorer) self
    (let ((doc-num (document counting-sum-scorer)))
      (loop while (< doc-num max)
	   do
	   (funcall fn doc-num (score self))
	   (when (not (next? counting-sum-scorer))
	     (return-from each-hit-up-to NIL))
	   (setf doc-num (document counting-sum-scorer)))
      T)))


(defmethod next? ((self boolean-scorer))
  (with-slots (counting-sum-scorer) self
    (when (null counting-sum-scorer)
      (init-counting-sum-scorer self))
    (next? counting-sum-scorer)))

(defmethod score ((self boolean-scorer))
  (with-slots (coordinator counting-sum-scorer) self
    (init-doc coordinator)
    (let ((sum (score counting-sum-scorer)))
      (* sum (coord-factor coordinator)))))


(defmethod skip-to ((self boolean-scorer) target)
  (with-slots (counting-sum-scorer) self
    (when (null counting-sum-scorer)
      (init-counting-sum-scorer self))
    (skip-to counting-sum-scorer target)))


(defmethod document ((self boolean-scorer))
  (document (slot-value self 'counting-sum-scorer)))

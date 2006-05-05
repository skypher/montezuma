(in-package montezuma)

;;?? <<
;;?? next?, do-next, score, init, sort-scorers


(defclass conjunction-scorer (scorer)
  ((scorers :initform (make-scorers-array) :reader scorers)
   (first-time-p :initform t :reader first-time-p)
   (more-p :initform t :reader more-p)
   (coord)))

(defun make-scorers-array ()
  (make-array 10 :adjustable t :fill-pointer 0))

(defmethod add ((self conjunction-scorer) scorer)
  (vector-push-extend scorer (scorers self))
  (scorers self))

(defmethod first-scorer ((self conjunction-scorer))
  (aref (scorers self) 0))

(defmethod last-scorer ((self conjunction-scorer))
  (aref (scorers self) (1- (length (scorers self)))))

(defmethod document ((self conjunction-scorer))
  (document (first-scorer self)))

(defmethod next? ((self conjunction-scorer))
  (with-slots (first-time-p more-p) self
    (cond (first-time-p
	   (do-init self T))
	  (more-p
	   (setf more-p (next? (last-scorer self)))))
    (do-next self)))

(defmethod do-next ((self conjunction-scorer))
  (with-slots (more-p scorers) self
    (loop while (and more-p (< (document (first-scorer self))
			       (document (last-scorer self))))
	 do
	 (setf more-p (skip-to (first-scorer self) (document (last-scorer self))))
	 (rotatef (aref scorers 0)
		  (aref scorers (- (length scorers) 1))))
    more-p))


(defmethod score ((self conjunction-scorer))
  (let ((score (reduce #'+ (slot-value self 'scorers) :key #'score)))
    (* score (slot-value self 'coord))))


(defmethod do-init ((self conjunction-scorer) init-scorers-p)
  (with-slots (coord more-p scorers first-time-p) self
    (setf coord (coord (similarity self) (length scorers) (length scorers)))
    (setf more-p (> (length scorers) 0))
    (when init-scorers-p
      (dosequence (scorer scorers)
	(when (not more-p)
	  (return))
	(setf more-p (next? scorer)))
      (when more-p
	(sort-scorers self)))
    (setf first-time-p NIL)))

(defmethod sort-scorers ((self conjunction-scorer))
  )

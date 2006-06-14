(in-package montezuma)

;;?? <<
;;?? next?, do-next, score, init, sort-scorers


(defclass conjunction-scorer (scorer)
  ((scorers      :initform (make-scorers-array) :reader scorers)
   (first-time-p :initform T                    :reader first-time-p)
   (more-p       :initform T                    :reader more-p)
   (coord)))

(defun make-scorers-array ()
  (make-array 10 :adjustable T :fill-pointer 0))

(defgeneric add (conjunction-scorer scorer))

(defmethod add ((self conjunction-scorer) scorer)
  (vector-push-extend scorer (scorers self))
  (scorers self))

(defgeneric first-scorer (conjunction-scorer))

(defmethod first-scorer ((self conjunction-scorer))
  (aref (scorers self) 0))

(defgeneric last-scorer (conjunction-scorer))

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

(defgeneric do-next (conjunction-scorer))

(defmethod do-next ((self conjunction-scorer))
  (with-slots (more-p scorers) self
    (loop while (and more-p (< (document (first-scorer self))
			       (document (last-scorer self))))
	 do
	 (setf more-p (skip-to (first-scorer self) (document (last-scorer self))))
	 ;; FIXME: in Ruby: @scorers << @scorers.shift()
;	 (let ((new-scorers (coerce scorers 'list)))
;	   (let ((first-scorer (pop new-scorers)))
;	     (setf scorers (coerce (append new-scorers (list first-scorer)) 'vector))))
	 (let ((first (aref scorers 0)))
	   (replace scorers scorers
		    :end1 (- (length scorers) 1)
		    :start2 1)
	   (setf (aref scorers (1- (length scorers))) first)))
    more-p))


(defmethod skip-to ((self conjunction-scorer) target)
  (with-slots (first-time-p more-p scorers) self
    (when first-time-p
      (do-init self NIL))
    (dosequence (scorer scorers)
      (when (not more-p)
	(return))
      (setf more-p (skip-to scorer target)))
    (when more-p
      (sort-scorers self))
    (do-next self)))

(defmethod score ((self conjunction-scorer))
  (let ((score (reduce #'+ (slot-value self 'scorers) :key #'score)))
    (* score (slot-value self 'coord))))


(defgeneric do-init (conjunction-scorer init-scorers-p))

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

(defgeneric sort-scorers (conjunction-scorer))

(defmethod sort-scorers ((self conjunction-scorer))
  (with-slots (scorers) self
    (setf scorers (sort scorers
			#'(lambda (a b)
			    (< (document a) (document b)))))))

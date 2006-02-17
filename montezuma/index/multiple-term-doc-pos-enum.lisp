(in-package #:montezuma)

(defclass term-positions-queue (priority-queue)
  ())

(defmethod initialize-instance :around ((self term-positions-queue) &key term-positions)
  (call-next-method :size (length term-positions))
  (dolist (tp term-positions)
    (when (next tp)
      (queue-push self tp))))

(defmethod less-than ((self term-positions-queue) tp1 tp2)
  (document< tp1 tp2))


(defclass multiple-term-doc-enum (term-doc-enum)
  ((tps-queue)
   (pos-list :initform '())))

(defmethod initialize-instance :after ((self multiple-term-doc-enum) &key reader terms)
  (with-slots (tps-queue pos-list) self
    (let ((term-positions (loop for term in terms
			       collecting (term-positions-for reader term))))

      (setf tps-queue (make-instance 'term-positions-queue
				     :term-positions term-positions)))))

(defmethod next ((self multiple-term-doc-enum))
  (with-slots (tps-queue pos-list doc freq) self
    (if (= (size tps-queue) 0)
	NIL
	(progn
	  (loop
	     do (let ((tps (queue-top tps-queue)))
		  (dotimes (i (freq tps))
		    (setf pos-list (append pos-list (list (next-position tps)))))
		  (if (next tps)
		      (adjust-top tps-queue)
		      (progn
			(queue-pop tps-queue)
			(close tps))))
	     while (and (> (size tps-queue) 0)
			(= (doc (queue-top tps-queue)) doc)))
	  (setf pos-list (sort pos-list #'<))
	  (setf freq (length pos-list))))))

(defmethod next-position ((self multiple-term-doc-enum))
  (with-slots (pos-list) self
    (pop (slot-value self 'pos-list))))

(defmethod skip-to ((self multiple-term-doc-enum) target)
  (with-slots (tps-queue) self
    (while (and (queue-top tps-queue)
		(> target (doc (queue-top tps-queue))))
      (let ((tps (queue-pop tps-queue)))
	(if (skip-to tps target)
	    (queue-push tps-queue tps)
	    (close tps))))
    (next self)))

(defmethod close ((self multiple-term-doc-enum))
  (with-slots (tps-queue) self
    (do ((tps (queue-pop tps-queue) (queue-pop tps-queue)))
	((null tps))
      (close tps))))

(defmethod seek ((self multiple-term-doc-enum) term)
  (declare (ignore term))
  (error "Not implemented"))

(defmethod read-docs ((self multiple-term-doc-enum) docs freqs)
  (declare (ignore docs freqs))
  (error "Not implemented."))

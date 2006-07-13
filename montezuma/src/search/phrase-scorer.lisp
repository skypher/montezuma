(in-package montezuma)

;;?? FIXME: explain, to-s

(defgeneric phrase-freq (phrase-scorer))


(defclass phrase-queue (priority-queue)
  ())

(defmethod less-than ((queue phrase-queue) a b)
  (if (= (document a) (document b))
    (< (phrase-position a) (phrase-position b))
    (< (document a) (document b))))

;;?? renamed first and last to first-index and last-index

(defclass phrase-scorer (scorer)
  ((first :reader first-index)
   (last :initform nil :reader last-index)
   (norms :initarg :norms)
   (weight :initarg :weight)
   (value :accessor value)
   (first-time-p :accessor first-time-p :initform T)
   (more-p :accessor more-p :initform T)
   (pq)
   (freq :accessor freq)))

(defmethod print-object ((self phrase-scorer) stream)
  (print-unreadable-object (self stream :identity T :type T)
    (format stream "weight: ~S" (slot-value self 'weight))))

(defmethod initialize-instance :after ((self phrase-scorer) &key term-positions positions)
  (with-slots (value first last pq weight) self
    (setf value (value weight))
    (dotimes (i (length term-positions))
      (let ((pp (make-instance 'phrase-positions
			       :tp-enum (elt term-positions i)
			       :offset (elt positions i))))
	(if last
	    (setf (next last) pp)
	    (setf first pp))
	(setf last pp)))
    (setf pq (make-instance 'phrase-queue
			    :max-size (length term-positions)))))

(defmethod document ((self phrase-scorer))
  (values (document (first-index self))))

(defmethod next? ((self phrase-scorer))
  (cond ((first-time-p self)
         (init self)
         (setf (first-time-p self) NIL))
        ((more-p self)
         (setf (more-p self) (next? (last-index self)))))
  (do-next self))

(defmethod do-next ((self phrase-scorer))
  (while (more-p self)
    ;; find doc with all the terms
    (while (and (more-p self)
                (< (document (first-index self)) (document (last-index self)))) 
            
      (setf (more-p self) (skip-to (first-index self) (document (last-index self))))
      (first-to-last self))
    (when (more-p self)
      (setf (freq self) (phrase-freq self))
      (if (= (freq self) 0.0)
	  ;; no match so trigger further scanning
	  (setf (more-p self) (next? (last-index self)))
	  ;; found a match
	  (return-from do-next T))))
  ;; no more matches
  NIL)

(defmethod each ((self phrase-scorer) fn)
  (let ((pp (slot-value self 'first)))
    (loop while pp do
	 (funcall fn pp)
	 (setf pp (next pp)))))

(defmethod score ((self phrase-scorer))
  (let ((raw (* (tf (similarity self) (freq self)) (value self))))
    ;; normalize
    (* raw (similarity-decode-norm 
	    (aref (slot-value self 'norms) (document (first-index self)))))))

(defmethod skip-to ((self phrase-scorer) target)
  (block each
    (each self (lambda (pp)
                 (unless (setf (more-p self) (skip-to pp target))
                   (return-from each)))))
  (when (more-p self) 
    ;;?? fixme: better name
    (do-sort self))
  (do-next self))

(defmethod init ((self phrase-scorer))
  (block nil (each self (lambda (pp) (unless (and (and (more-p self) T) (next? pp)) (return)))))
  (when (more-p self)
    (do-sort self)))

(defgeneric do-sort (phrase-scorer))

(defmethod do-sort ((self phrase-scorer))
  (with-slots (pq) self
    (queue-clear pq)
    (each self (lambda (pp) (queue-push pq pp)))
    (pq->list self)))

(defgeneric pq->list (phrase-scorer))

(defmethod pq->list ((self phrase-scorer))
  (with-slots (first last pq) self
    (setf last nil
	  first nil)
    (while (queue-top pq)
      (let ((pp (queue-pop pq)))
	(if last
	    ;; add next to end of list
	    (setf (next last) pp)
	    (setf first pp))
	(setf last pp)
	(setf (next pp) nil)))))

(defgeneric first-to-last (phrase-scorer))

(defmethod first-to-last ((self phrase-scorer))
  (with-slots (last first) self
    (setf (next last) first)
    (setf last first)
    (setf first (next first))
    (setf (next last) nil)))


#|

    def explain(doc)

      tf_explanation = Explanation.new()



      while (next? and doc() < doc)

      end



      phrase_freq = (doc() == doc) ? @freq : 0.0

      tf_explanation.value = @similarity.tf(phrase_freq)

      tf_explanation.description = "tf(phrase_freq=#{phrase_freq})"



      return tf_explanation

    end



    def to_s() return "phrase_scorer(#{@weight})" end

|#








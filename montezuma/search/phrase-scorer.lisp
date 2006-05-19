(in-package montezuma)

;;?? FIXME: explain, to-s

(defgeneric phrase-freq (phrase-scorer))


(defclass phrase-queue (priority-queue)
  ())

(defmethod less-than ((queue phrase-queue) a b)
  (if (= (doc a) (doc b))
    (< (phrase-position a) (phrase-position b))
    (< (doc a) (doc b))))

;;?? renamed first and last to first-index and last-index

(defclass phrase-scorer (scorer)
  ((first :reader first-index)
   (last :reader last-index)
   (norms :initarg :norms)
   (weight :initarg :weight)
   (value :accessor value)
   (first-time-p :accessor first-time-p :initform t)
   (more-p :accessor more-p :initform t)
   (freq :accessor freq)))

#|
(defmethod initialize-instance :after ((self phrase-scorer) &key tps)
  ;; convert tps to a list
  (dotimes (i (length (times tps)))
    (phrase-positions 
    tps.length.times do |i|

        pp = PhrasePositions.new(tps[i], positions[i])

        if (@last != nil) # add next to end of list

          @last.next = pp

        else

          @first = pp

        end

        @last = pp

      end



      @pq = PhraseQueue .new(tps.length)  # construct empty pq
  )


|#
(defmethod document ((self phrase-scorer))
  (values (document (first-index self))))

(defmethod next? ((self phrase-scorer))
  (cond ((first-time-p self)
         (init self)
         (setf (first-time-p self) nil))
        ((more-p self)
         (setf (more-p self) (next? (last-index self)))))
  (values (do-next self)))

(defmethod do-next ((self phrase-scorer))
  (while (more-p self)
    ;; find doc with all the terms
    (while (and (more-p self)
                (< (doc (first-index self)) (doc (last-index self)))) 
            
      (setf (more-p self) (skip-to self (first-index self) (doc (last-index self))))
      (first-to-last self))

    (more-p self)
    (setf (freq self) (phrase-freq self))
    (if (= (freq self) 0.0)
      ;; no match so trigger further scanning
      (setf (more-p self) (next? (last-index self)))
      ;; found a match
      (return-from do-next t)))
  
  ;; no more matches
  (values nil))

(defmethod each ((self phrase-scorer) fn)
  (each (first-index self) fn))

(defmethod score ((self phrase-scorer))
  (let ((raw (* (tf (similarity self) (freq self)) (value self))))
    ;; normalize
    (values (* raw (similarity-decode-norm 
                    (aref (norms self) (doc (first-index self))))))))

(defmethod skip-to ((self phrase-scorer) target)
  (block each
    (each self (lambda (pp)
                 (unless (= (more-p self) (skip-to pp target))
                   (return-from each)))))
  (when (more-p self) 
    ;;?? fixme: better name
    (do-sort self))
  
  (values (do-next self)))

(defmethod init ((self phrase-scorer))
  (block nil (each self (lambda (pp) (unless (= (more-p self) (next? pp)) (return)))))
  (when (more-p self)
    (do-sort self)))

(defmethod do-sort ((self phrase-scorer))
  (queue-clear (pq self))
  (each self (lambda (pp) (queue-push (pq self) pp)))
  (pq-to-list self))

(defmethod pq->list ((self phrase-scorer))
  (setf (last-index self) nil
        (first-index self) nil)
  (while (queue-top (pq self))
    (let* ((pp (cons (queue-pop (pq self)) nil)))
      (if (last-index self)
        ;; add next to end of list
        (setf (cdr (last-index self)) pp)
        (setf (first-index self) pp))
      (setf (last-index self) pp))))

(defmethod first-to-last ((self phrase-scorer))
  (setf (cdr (last-index self)) (first-index self)
        (index-index self) (first-index self)
        (first-index self) (cdr (first-index self))
        (cdr (last-index self)) nil))


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








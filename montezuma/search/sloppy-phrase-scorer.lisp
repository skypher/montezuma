(in-package #:montezuma)

(defclass sloppy-phrase-scorer (phrase-scorer)
  ((slop :initarg :slop)))

(defmethod phrase-freq ((self sloppy-phrase-scorer))
  (with-slots (pq slop similarity) self
    (queue-clear pq)
    (let ((last-pos 0))
      (each self #'(lambda (pp)
		     (first-position pp)
		     (when (> (phrase-position pp) last-pos)
		       (setf last-pos (phrase-position pp)))
		     (queue-push pq pp)))
      (let ((freq 0.0)
	    (done-p NIL))
	(loop do
	     (let* ((pp (queue-pop pq))
		    (pos (phrase-position pp))
		    (start pos)
		    (next-pos (phrase-position (queue-top pq))))
	       (loop while (and (<= pos next-pos) (not done-p)) do
		    (setf start pos)
		    (if (not (next-position pp))
			(setf done-p T)
			(setf pos (phrase-position pp))))
	       (let ((match-length (- last-pos start)))
		 (when (<= match-length slop)
		   (incf freq (sloppy-freq similarity match-length))))
	       (when (> (phrase-position pp) last-pos)
		 (setf last-pos (phrase-position pp)))
	       (queue-push pq pp))
	     while (not done-p))
	freq))))

		 

	       



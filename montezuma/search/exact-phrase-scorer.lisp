(in-package #:montezuma)

(defclass exact-phrase-scorer (phrase-scorer)
  ())

(defmethod phrase-freq ((self exact-phrase-scorer))
  (with-slots (pq first last) self
    (each self #'(lambda (pp)
		   (first-position pp)
		   (queue-push pq pp)))
    (pq->list self)
    (let ((freq 0))
      (loop do
	   (loop while (< (phrase-position first) (phrase-position last)) do
		(loop do
		     (when (not (next-position first))
		       (return-from phrase-freq freq))
		     while (< (phrase-position first) (phrase-position last)))
		(first-to-last self))
	   (incf freq)
	   while (next-position last))
      freq)))


(in-package #:montezuma)

(defclass segment-merge-info ()
  ((base :initarg :base :reader base)
   (term-enum :initarg :term-enum :reader term-enum)
   (reader :initarg :reader)
   (postings :initform '())
   (term-buffer :reader term-buffer)
   (doc-map :initform nil)))

(defmethod initialize-instance :after ((self segment-merge-info) &key)
  (with-slots (term-enum term-buffer) self
    (setf term-buffer (term-buffer term-enum))))

(defmethod positions ((self segment-merge-info))
  ;; FIXME: what does "@postings ||= @reader.term_positions()" mean?
  (with-slots (postings reader) self
    (if postings
	postings
	(setf postings (term-positions reader)))))

(defmethod doc-map ((self segment-merge-info))
  (with-slots (doc-map reader) self
    (when (null doc-map)
      (when (has-deletions-p reader)
	(let ((max-doc (max-doc reader))
	      (j 0))
	  (setf doc-map (make-array max-doc))
	  (dotimes (i max-doc)
	    (if (deleted-p reader i)
		(setf (aref doc-map i) -1)
		(progn
		  (setf (aref doc-map i) j)
		  (incf j)))))))
    doc-map))

(defmethod next ((self segment-merge-info))
  (with-slots (term-enum) self
    (next term-enum)))

(defmethod close ((self segment-merge-info))
  (with-slots (term-enum postings reader) self
    (close term-enum)
    (when postings (close postings))
    (setf reader nil)))

(in-package #:montezuma)

;; A phrase-query is a query that matches documents containing a
;; particular sequence of terms.

(defclass phrase-query (query)
  ((slop :initform 0 :accessor slop)
   (terms :initform (make-array 3 :adjustable T :fill-pointer 0) :reader terms)
   (positions :initform (make-array 3 :adjustable T :fill-pointer 0) :reader positions)
   (field :initform nil :reader field)))

(defmethod print-object ((self phrase-query) stream)
  (print-unreadable-object (self stream :type T)
    (format stream "field:~S terms: " (field self))
    (dosequence (term (terms self) :index i)
      (let ((position (aref (positions self) i)))
	(format stream "~S:~S " (term-text term) position)))))


(defmethod add-term-to-query ((self phrase-query) term &optional position (pos-inc 1))
  (with-slots (positions terms field) self
    (when (null position)
      (setf position (if (> (length positions) 0)
			 (+ (aref positions (- (length positions) 1)) pos-inc)
			 0)))
    (if (= (length terms) 0)
	(setf field (term-field term))
	(when (not (string= (term-field term) field))
	  (error "All phrase terms must be in the same field: ~S" term)))
    (vector-push-extend term terms)
    (vector-push-extend position positions)
    self))


(defclass phrase-weight (weight)
  ((query :initarg :query :reader query)
   (similarity)
   (idf)
   (query-weight)
   (query-norm)
   (value :initform nil :reader value)))

(defmethod initialize-instance :after ((self phrase-weight) &key searcher)
  (with-slots (similarity idf query) self
    (setf similarity (similarity-implementation query searcher))
    (setf idf (idf-phrase similarity (terms query) searcher))))

(defmethod sum-of-squared-weights ((self phrase-weight))
  (with-slots (query-weight idf query) self
    (let ((w (* idf (boost query))))
      (setf query-weight w)
      (* w w))))

(defmethod normalize-weight ((self phrase-weight) query-norm)
  (with-slots (query-weight value idf) self
    (setf (slot-value self 'query-norm) query-norm)
    (setf query-weight (* query-weight query-norm))
    (setf value (* query-weight idf))))

(defmethod scorer ((self phrase-weight) reader)
  (with-slots (query similarity) self
    (if (= (length (terms query)) 0)
	nil
	(let ((tps (loop for term across (terms query)
			collecting
			(let ((tp (term-positions-for reader term)))
			  (if (null tp)
			      (return-from scorer nil)
			      tp)))))
	  (if (= (slop query) 0)
	      (make-instance 'exact-phrase-scorer
			     :weight self
			     :term-positions tps
			     :positions (positions query)
			     :similarity similarity
			     :norms (get-norms reader (field query)))
	      (make-instance 'sloppy-phrase-scorer
			     :weight self
			     :term-positions tps
			     :positions (positions query)
			     :similarity similarity
			     :slop (slop query)
			     :norms (get-norms reader (field query))))))))


(defmethod create-weight ((self phrase-query) searcher)
  (with-slots (terms) self
    (if (= (length terms) 1)
	(let* ((term (aref terms 0))
	       (tq (make-instance 'term-query
				  :term term)))
	  (setf (boost tq) (boost self))
	  (create-weight tq searcher))
	(make-instance 'phrase-weight
		       :query self
		       :searcher searcher))))

(defmethod extract-terms ((self phrase-query) query-terms)
  (add-all query-terms (slot-value self 'terms)))


(in-package #:montezuma)

(defclass range-query (query)
  ((field :initarg :field :reader field)
   (lower-term :initform nil :initarg :lower-term :reader lower-term)
   (upper-term :initform nil :initarg :upper-term :reader upper-term)
   (include-upper-p :initarg :include-upper-p :reader include-upper-p)
   (include-lower-p :initarg :include-lower-p :reader include-lower-p)))

(defmethod initialize-instance :after ((self range-query) &key)
  (with-slots (lower-term upper-term include-lower-p include-upper-p) self
    (cond ((and (null lower-term) (null upper-term))
	   (error "At least one of :upper-term and :lower-term must be specified and non-null."))
	  ((and include-lower-p (null lower-term))
	   (error "The lower term bound must be non-null to be inclusive."))
	  ((and include-upper-p (null upper-term))
	   (error "The upper term bound must be non-null to be inclusive."))
	  ((and upper-term lower-term (string< upper-term lower-term))
	   (error "The lower term bound must be less than the upper term bound.")))))

(defmethod rewrite ((self range-query) reader)
  (with-slots (field lower-term upper-term include-lower-p include-upper-p) self
    (let ((bq (make-instance 'boolean-query))
	  (term-enum (terms-from reader (make-term field (or lower-term "")))))
      (unwind-protect
	   (let ((check-lower-p (not include-lower-p))
		 (test-field (field self)))
	     (loop do
		  (let ((term (term term-enum)))
		    (when (or (null term) (not (string= (term-field term) field)))
		      (return))
		    (when (or (not check-lower-p)
			      (null lower-term)
			      (string> (term-text term) lower-term))
		      (setf check-lower-p NIL)
		      (when upper-term
			(let ((compare (string-compare upper-term (term-text term))))
			  (when (or (< compare 0) (and (not include-upper-p) (= compare 0)))
			    (return))))
		      (let ((tq (make-instance 'term-query
					       :term term
					       :boost (boost self))))
			(add-query bq tq :should-occur))))
		  while (next? term-enum)))
	(close term-enum))
      bq)))

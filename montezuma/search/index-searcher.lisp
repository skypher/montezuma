(in-package montezuma)

(defclass index-searcher ()
  ((similarity :accessor similarity :initarg :similarity)
   (reader :accessor reader)
   (directory :accessor directory))
  (:default-initargs
    :similarity (make-default-similarity)))

(defmethod initialize-instance :after ((self index-searcher) &key reader)
  (setf (slot-value self 'reader) (initialize-reader self reader)))

(defmethod initialize-reader ((self index-searcher) (reader index-reader))
  (values reader))

(defmethod initialize-reader ((self index-searcher) (reader directory))
  (open-index-reader reader :close-directory-p NIL))

(defmethod initialize-reader ((self index-searcher) (reader string))
  (setf (directory self) (make-fs-directory reader))
  (open-index-reader (directory self) :close-directory-p t))

(defmethod close ((self index-searcher))
  ;; delegate
  (close (reader self)))

(defmethod term-doc-freq ((self index-searcher) (term term))
  ;; delegate
  (term-doc-freq (reader self) term))

(defmethod term-doc-freqs ((self index-searcher) (terms sequence))
  (let ((result (make-array (length terms))))
    (dosequence (i terms)
      (setf (aref i result) 
            (term-doc-freq self (aref terms i))))
    (values result)))

(defmethod get-document ((self index-searcher) index)
  ;; delegate
  (get-document (reader self) index))

(defmethod max-doc ((self index-searcher))
  ;; delegate
  (max-doc (reader self)))

(defmethod create-weight ((self index-searcher) (query query))
  (weight query self))

#|
(defmethod search ((self index-searcher) (query query) 
                   &optional options)
  (destructuring-bind (&key (filter nil) (first-document 0) (num-documents 10)
                            (max-size (+ first-document num-documents))
                            (sort nil)) options
    
    (assert (plusp num-documents))
    (assert (not (minusp first-document)))
    (let ((scorer (scorer (weight query self) (reader self))))
      (when (null scorer)
        (return-from search (make-instance 'top-docs)))
      
      ;;?? ignore filter
      ;;?? ignore sort
      (let ((hq (make-instance 'hit-queue))
            (total-hits 0)
            (minimum-score 0.0))
	(map-pipe #'(lambda (doc score)
		      (when (and (plusp score)
				 ;; bits
				 )
			(incf total-hits)
			(when (or (< (size hq) max-size)
				  (>= score minimum-score))
			  (queue-push hq (make-instance 'score-doc :doc doc :score score))
			  (setf minimum-score (score (queue-top hq))))))
		  (each-hit scorer))
        
        (let ((score-docs (make-array 10 :fill-pointer 0 :adjustable t)))
          (when (> (size hq) first-document)
            (when (< (- (size hq) first-document) num-documents)
              (setf num-documents (- (size hq) first-document)))
            (dotimes (i num-documents)
              (vector-push-extend (queue-pop hq) score-docs)))
          (queue-clear hq)
        
          (values (make-instance 'top-docs :total-hits total-hits
                                 :score-docs score-docs)))))))
|#

(defmethod search ((self index-searcher) query &optional options)
  (let* ((filter (getf options :filter))
	 (first-doc (or (getf options :first-doc) 0))
	 (num-docs (or (getf options :num-docs) 10))
	 (max-size (+ first-doc num-docs))
	 (sort (getf options :sort)))
    (when (and sort (not (typep sort 'sort)))
      (setf sort (make-instance 'sort :sort sort)))
    (when (<= num-docs 0)
      (error ":num-docs must be greater then zero to run a search."))
    (when (< first-doc 0)
      (error "first-doc must be greater than or equal to zero to run a search."))
    (let ((scorer (scorer (weight query self) (reader self))))
      (if (null scorer)
	  (make-instance 'top-docs
			 :thing 0
			 :other-thing '())
	  (let ((bits (if (null filter)
			  nil
			  (bits filter (reader self))))
		(hq (if sort
			(let ((fields (if (typep sort 'sort)
					  (fields sort)
					  sort)))
			  (make-instance 'field-sorted-hit-queue
					 :reader (reader self)
					 :fields fields
					 :max-size max-size))
			(make-instance 'hit-queue
				       :max-size max-size))))
	    (let ((total-hits 0)
		  (min-score 0.0))
	      (dolist (hit (hits scorer))
		(destructuring-bind (doc score) hit
		  (when (and (> score 0.0)
			     (or (null bits)
				 (bit-set-p bits doc)))
		    (incf total-hits)
		    (when (or (< (size hq) max-size)
			      (>= score min-score))
		      (queue-insert hq (make-instance 'score-doc
						      :doc doc
						      :score score))
		      (setf min-score (score (queue-top hq)))))))
	      (let ((score-docs '()))
		(when (> (size hq) first-doc)
		  (when (< (- (size hq) first-doc) num-docs)
		    (setf num-docs (- (size hq) first-doc)))
		  (dotimes (i num-docs)
		    (declare (ignorable i))
		    (queue-push (queue-pop hq) score-docs)))
		(queue-clear hq)
		(make-instance 'top-docs
			       :total-hits total-hits
			       :score-docs score-docs))))))))


(defmethod search-each ((self index-searcher) (query query) &optional (options nil))
  (let ((scorer (scorer (weight query self) (reader self))))
    (when (null scorer)
      (return-from search-each nil))
    
    ;;?? bits
    (map-pipe #'(lambda (doc score)
		  (if (and (plusp score)
			   ;;?? bits
			   )
		      (yield doc score)))
	      (hits scorer))))
    
(defmethod rewrite ((self index-searcher) original)
  (let* ((query original)
         (rewritten-query (rewrite query (reader self))))
    (while (not (equal query rewritten-query))
      (setf query rewritten-query
            rewritten-query (rewrite query (reader self))))
    (values query)))

(defmethod explain-score ((self index-searcher) (query query) index)
  ;; not implemented
  (error "not yet implemented"))